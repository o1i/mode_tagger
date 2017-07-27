library(shiny)
library(leaflet)
library(dplyr)
library(stringr)
library(RColorBrewer)
library(Rcpp)
library(tidyr)
library(sp)
options(digits = 12)
options(digits.secs = 3)

source("utils.R")
Sys.setenv(TZ='GMT')

# --- Initialisation of values -------------------------------------------------

# Different modes
modes <- list("Unknown" = "unk",
              "Indoors" = "indo",
              "Walking" = "walk",
              "Bike" = "bike",
              "Car" = "car",
              "Bus" = "bus",
              "Tram" = "tram",
              "Train" = "train",
              "Motorbike" = "moto",
              "Ship" = "ship",
              "Transfer" = "trans",
              "Other" = "other",
              "Remove" = "rm")

# There are too many colours for a single palette
mode_col_palette <- c("#777777", brewer.pal(8, "Dark2"),
                      "#f03b20", "#7a0177", "#08519c", "#FF0000")
mode_cols <- setNames(as.list(mode_col_palette), as.character(unlist(modes)))

# --- UI side of the shiny app -------------------------------------------------
ui <- fluidPage(

   # Application title
   titlePanel("Movement data Annotation"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
      fileInput('gpsfile', 'Choose GPS File',
                accept=c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv')),
      fileInput('imufile', 'Choose IMU File',
                accept=c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv')),
      fileInput('flagfile', 'Choose Flags File',
                accept=c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv')),
      fileInput('labelfile', 'Choose Labelled File (optional)',
                accept=c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv')),
      hr(),
      actionButton("shift_b", "Move Back"),
      actionButton("shift_f", "Move Ahead"),
      br(),
      actionButton("zoom_in", "Zoom In"),
      actionButton("center", "Center"),
      actionButton("zoom_out", "Zoom Out"),
      br(),
      actionButton("remove", "Remove rest"),
      hr(),
      selectInput(
        'mode_select', 'Select Mode', choices = modes,
        selectize = FALSE
      ),
      hr(),
      downloadButton('download', 'Download Labelled data')
      ),  # End of sidebar panel
      # Show a plot of the generated distribution
      mainPanel(
        # textOutput("str"),
        plotOutput("overview", width = "800px", height = "150px",
                   click = "overview_click"),
        leafletOutput("map"),
        plotOutput("speed_overview", width = "800px", height = "150px",
                   click = "speed_overview_click"),
        plotOutput("acc_overview", width = "800px", height = "150px",
                   click = "acc_overview_click")
      )
   )
)

# --- Server side of the Shiny app ---------------------------------------------
server <- function(input, output) {

  # Having (small) dummy datasets allows also Users with no data to see
  #   how the thing works. Also eliminates ugly error messages.
  dummy_gps <- read_gps("dummy_gps.csv")
  dummy_imu <- read_imu("dummy_imu.csv")

  # All the (reactive) values are stored here
  values <- reactiveValues(
    gps_data = dummy_gps,
    imu_data = dummy_imu,
    time_focus = min(dummy_imu$ts, na.rm = TRUE),
    time_window = 60*5,
    click_id_gps = min(as.numeric(dummy_gps$id)),
    click_id_imu = min(as.numeric(dummy_imu$id))
  )

  # --- Reading Data Events ----------------------------------------------------
  observeEvent(input$gpsfile, { # GPS Data
    if(!is.null(input$gpsfile)){
      print("Loading GPS Data")
      values$gps_data <- read_gps(input$gpsfile$datapath)
      values$time_focus <- min(values$gps_data$ts, na.rm = TRUE)
      values$time_window <- 60*5
      values$click_id_gps <- min(as.numeric(values$gps_data$id))
      print("GPS Data Loaded")
    }
  })

  observeEvent(input$imufile, { # IMU Data
    if(!is.null(input$imufile)){
      print("Loading IMU Data")
      values$imu_data <- read_imu(input$imufile$datapath)
      values$time_focus <- min(values$imu_data$ts, na.rm = TRUE)
      values$time_window <- 60*5
      values$click_id_imu <- min(as.numeric(values$imu_data$id))
      print("IMU Data Loaded")
    }
  })

  observeEvent(input$flagfile, { # Flagging Data
    if(!is.null(input$flagfile)){
      print("Loading Flagging Data")
      values$flags_data <- read.csv(input$flagfile$datapath, header = T, sep = ",",
                                  dec = ".", strip.white = TRUE,
                                  stringsAsFactors = FALSE) %>%
        mutate(Time = gsub("^([0-9]*:[0-9]*:[0-9]*):([0-9]*).*$", "\\1.\\2", paste0(Time, ":000")),
               ts = as.POSIXct(paste(Date, Time), format = "%d/%m/%Y %H:%M:%OS")) %>%
        arrange(ts) %>%
        select(-c(Date, Time)) %>%
        filter(Action == "Device event : User Event")
      rownames(values$flags_data) <- values$flags_data$id
      # Remove automatically generatet flags 10 seconds after user-flags
      auto_flags <- c(FALSE, diff(values$flags_data$ts, units = "secs") < 22)
      values$flags_data <- values$flags_data[!auto_flags, ]
      print("Flagging Data Loaded")
    }
  })

  observeEvent(input$labelfile, { # Labelled data
    if(!is.null(input$labelfile)){
      print("Loading Labelled Data")
      values$labelled_data <- read.csv(input$labelfile$datapath, header = T, sep = ",",
                                  dec = ".", strip.white = TRUE,
                                  stringsAsFactors = FALSE) %>%
        arrange(ts) %>%
        select(ts, flag_mode_imu) %>%
        mutate(ts = as.POSIXct(ts, format = "%Y-%m-%d %H:%M:%OS"))
      print("Labelled Data Loaded")
    }
  })

  observeEvent({ # Out dataset (plotted on map) (subset of GPS data)
    print("Make out dataset")
    values$plot_range
    values$gps_data
  }, {
    inds <- tryCatch(values$plot_range %>%
      findInterval(values$gps_data$ts) %>%
      `+`(c(-2, 2)) %>%
      pmax(1) %>%
      pmin(nrow(values$gps_data)) %>%
      as.list() %>%
      do.call(what = `:`), error = function(e) 1:10)
    values$out_data <- values$gps_data[inds, ]

  })

  observeEvent(values$out_data, { # Adjust GPS clicked time point if necessary
    if(!values$click_id_gps %in% values$out_data){
      ids <- as.numeric(values$out_data$id)
      values$click_id_gps <- as.numeric(values$click_id_gps) %>%
        pmin(max(ids)) %>%
        pmax(min(ids)) %>%
        as.character()
    }
  })

  # --- Producing output -------------------------------------------------------
  output$overview <- renderPlot({ # Overall overview
     par(mar = c(2.5, 8.1, 1, 2.1))
     inds <- seq(1, nrow(values$imu_data), l = 1000)
     if(sum(values$imu_data$flag_mode_imu == "unk") < 20000){
       inds <- c(inds, which(values$imu_data$flag_mode_imu == "unk"))
     }
     plot(NULL, xlim = range(values$imu_data$ts) + c(-0.01, 0.01), ylim = c(1, length(modes)),
          ylab = "", bty = "n", xaxt = 'n', yaxt = 'n')

     # Red GPS-gap rectangles
     if(length(values$gps_gap_starts) > 1){
       rect(values$gps_gap_starts, -1,
            values$gps_gap_ends, length(modes) + 2,
            col = rgb(1, 0, 0, 0.3), border = NA)
     }
     xlabels <- seq(min(values$gps_data$ts), max(values$gps_data$ts), l = 6)
     # Time is always UTC, just the labels are CE(S)T
     axis(1, at = xlabels, labels = strftime(xlabels, format = "%H:%M",
                                             tz = "CET"))
     axis(2, at = 1:length(modes), labels = names(modes), las = 1, tick = FALSE)

     # Grey focus window
     rect(values$time_focus - values$time_window, -1,
          values$time_focus + values$time_window, 100,
          col = "#AAAAAA", border = NA)
     abline(v = range(values$out_data$ts))

     # Points with the transport modes
     points(x = values$imu_data$ts[inds],
            y = as.numeric(factor(values$imu_data$flag_mode_imu[inds],
                                  levels = as.character(unlist(modes)))),
            col = mode_col_palette[as.numeric(factor(
              values$imu_data$flag_mode_imu[inds],
              levels = as.character(unlist(modes))))])
     abline(v = values$overview_hover_x)

     # Add flag-points if there are any
     tryCatch(abline(v = values$flags_data$ts, col = rgb(1, 0, 0, 0.7)),
              error=function(e){})
   })

  output$map <- renderLeaflet({ # Map
    print("Printing map")
    print(paste0("Map contains ", nrow(values$out_data), " Points."))
    point_cols <- get_point_colour(values$out_data)
    line_cols <- get_line_colour(values$out_data$flag_mode_gps, mode_cols)
    if(abs(as.numeric(values$out_data[values$click_id_gps, "ts"]) -
           as.numeric(values$imu_data[values$click_id_imu, "ts"])) < 3 ){
      highlight_ind <- which(values$out_data$id == values$click_id_gps)
    }else{
      highlight_ind <- findInterval(values$imu_data[values$click_id_imu, "ts"],
                                    values$out_data$ts)
      print(paste("GPS map-interval:", highlight_ind))
    }
    leaflet(options=leafletOptions(minZoom=13, maxZoom=18)) %>% addTiles() %>%
      addCircleMarkers(lng = values$out_data$Longitude,
                       lat = values$out_data$Latitude,
                       layerId = values$out_data$id,
                       color = point_cols$c,
                       opacity = point_cols$a) %>%
      mcpl(lon = values$out_data$Longitude, lat = values$out_data$Latitude,
           col = line_cols) %>%
      addCircleMarkers(lng = values$out_data$Longitude[highlight_ind],
                       lat = values$out_data$Latitude[highlight_ind],
                       color = "#CC0000")
  })

   output$acc_overview <- renderPlot({ # Acceleration
     par(mar = c(2.5, 8.1, 2.5, 2.1))
     # the used IMU data is only needed here, so not stored to values
     imu_used <- subset(values$imu_data,
                        abs(as.numeric(values$imu_data$ts -
                                         values$time_focus,
                                       units = "secs")) < values$time_window)
     imu_used <- imu_used[unique(floor(seq(1, nrow(imu_used), l = 2000))), ]

     plot(NULL, xlim = values$plot_range  + c(-0.01, 0.01), ylim = c(200, 1800),
          ylab = "", bty = "n", xaxt = 'n', yaxt = 'n', main = "Acceleration")
     xlabels <- seq(min(values$plot_range), max(values$plot_range), l = 4)
     axis(1, at = xlabels, labels = strftime(xlabels, format = "%H:%M:%S",
                                             tz = "CET"))
     # lines in black to see the connections, point in colour to see the modes
     lines(x = imu_used$ts,
           y = imu_used$acc_tot)
     points(x = imu_used$ts,
            y = imu_used$acc_tot,
            pch = 16, cex = 0.6, col = get_line_colour(imu_used$flag_mode_imu,
                                                       mode_cols))
     # 1000 is gravity
     abline(h = 1000)

     # marking the highlighted point
     abline(v = values$imu_data[values$click_id_imu, "ts"])
     tryCatch(abline(v = values$flags_data$ts, col = rgb(1, 0, 0)),
              error=function(e){})
   })

   output$speed_overview <- renderPlot({ # Speed
     par(mar = c(2.5, 8.1, 2.5, 2.1))
     plot(NULL, xlim = values$plot_range + c(-0.01, 0.01), ylim = range(0, 150),
          ylab = "", bty = "n", xaxt = 'n', yaxt = 'n', main = "Speed")
     xlabels <- seq(min(values$plot_range), max(values$plot_range), l = 4)
     axis(1, at = xlabels, labels = strftime(xlabels, format = "%H:%M:%S",
                                             tz = "CET"))
     axis(2, at = c(4.5, 30, 50, 80, 100), tick = F)
     abline(h = c(4.5, 30, 50, 80, 100), col = "#AAAAAA")

     # Lines to see order, colour to see modes
     lines(x = pmax(0, values$out_data$ts),
           y = values$out_data$Speed)
     points(x = pmax(0, values$out_data$ts),
           y = values$out_data$Speed,
           pch = 16, cex = 0.6,
           col = unlist(mode_cols[values$out_data$flag_mode_gps]))

     # Marking the highlighted point
     abline(v = values$imu_data[values$click_id_imu, "ts"])

     # Adding custom flag if present
     tryCatch(abline(v = values$flags_data$ts, col = rgb(1, 0, 0)),
              error=function(e){})
   })


   # --- Events ----------------------------------------------------------------
   observeEvent({ # Determine the gaps in GPS (interruptions of >5 Sec)
     values$gps_data
     values$imu_data
   }, {
     print("Potentially listening to GPS gap events")
     print(values$imu_data$ts[1])
     print(values$gps_data$ts[1])
     print(abs(as.numeric(values$imu_data$ts[1] - values$gps_data$ts[1],
                          units = "secs")))
     if(!is.null(values$gps_data) & !is.null(values$imu_data)){
       print("Really determining GPS gaps")
       if(abs(as.numeric(values$imu_data$ts[1] - values$gps_data$ts[1],
                         units = "secs")) > 5){
         starts <- values$imu_data$ts[1]
         ends <- values$gps_data$ts[1]
       }else{
         starts <- numeric(0)
         ends <- numeric(0)
       }
       midgaps <- which(diff(as.numeric(values$gps_data$ts,
                                        units = "secs")) > 5)
       if(length(midgaps) > 0){
         starts <- c(starts, values$gps_data$ts[midgaps])
         ends <- c(ends, values$gps_data$ts[midgaps + 1])
       }
       if(abs(as.numeric(values$imu_data$ts[nrow(values$imu_data)] -
                         values$gps_data$ts[nrow(values$gps_data)])) > 5){
         starts <- c(starts, values$gps_data$ts[nrow(values$gps_data)])
         ends <- c(ends, values$imu_data$ts[nrow(values$imu_data)])
       }
      values$gps_gap_starts <- starts
      values$gps_gap_ends <- ends
     }
   })

   observeEvent({  # Auto-Label data
     # As soon as there are labelled data, this gets executed
     values$labelled_data
     values$imu_data
     values$gps_data
     }, {
     if(!is.null(values$labelled_data) & !is.null(values$imu_data) &
        !is.null(values$gps_data)){
       # First, we need to find starts, ends and labels of triplegs
       n <- nrow(values$labelled_data)
       breaks <- which(values$labelled_data$flag_mode_imu[-1] !=
                         values$labelled_data$flag_mode_imu[-n] |
         as.numeric(values$labelled_data$ts[-1] -
                      values$labelled_data$ts[-n], units = "secs") > 10)
       starts <- values$labelled_data$ts[c(1, breaks + 1)]
       ends <- values$labelled_data$ts[c(breaks, n)]
       labels <- values$labelled_data$flag_mode_imu[c(1, breaks + 1)]

       # Then we apply them
       sapply(1:length(labels), function(i){
         new_inds <- values$imu_data$ts >= starts[i] &
           values$imu_data$ts < ends[i]
         values$imu_data$flag_mode_imu[new_inds] <- labels[i]
       })
     }
   })


   observeEvent({values$time_focus  # Set window for plots
                values$time_window
                }, {
     values$plot_range <- values$time_focus + c(-1, 1) * values$time_window
  })

   observeEvent(input$map_marker_click, {  # Observer to show Popups on click
     click <- input$map_marker_click
     if (!is.null(click)) {
       time_pressed <- values$out_data[click$id, "ts"]
       values$click_id_imu <- values$imu_data[
         findInterval(time_pressed, values$imu_data$ts)[1], "id"]
       values$click_id_gps <- click$id
       print(paste("Click id:", values$click_id_imu))
     }
   })

   observeEvent(input$shift_f, { # Move ahead button
     values$time_focus <- values$time_focus + floor(values$time_window)
   })

   observeEvent(input$shift_b, { # Move back button
     values$time_focus <- values$time_focus - floor(values$time_window)
   })

   observeEvent(input$zoom_in, { # Zoom In button
     values$time_window <- values$time_window / 2
   })

   observeEvent(input$zoom_out, { # Zoom Out button
     values$time_window <- values$time_window * 2
   })

   observeEvent(input$center, { # Center button
     values$time_focus <- values$imu_data[values$click_id_imu, "ts"]
   })

   observeEvent(input$mode_select, { # Setting Labels
     if(!is.null(values$gps_data) & !is.null(values$imu_data)){
       print("Before changes (GPS):")
       print(values$gps_data$flag_mode_gps[as.numeric(values$click_id_gps) +
                                             -1:1])
       values$gps_data$flag_mode_gps <- insert_value(
         values$gps_data$flag_mode_gps,
         which(values$gps_data$id == values$click_id_gps),
         input$mode_select)
       values$imu_data$flag_mode_imu <- insert_value(
         values$imu_data$flag_mode_imu,
         which(values$imu_data$id == values$click_id_imu),
         input$mode_select)
       print("After changes (GPS):")
       print(values$gps_data$flag_mode_gps[as.numeric(values$click_id_gps) +
                                             -1:1])
     }
   })

   observeEvent(input$remove, { # Remove rest button
     values$imu_data$flag_mode_imu[values$imu_data$flag_mode_imu == "unk"] <-
       "rm"
     values$gps_data$flag_mode_gps[values$gps_data$flag_mode_gps == "unk"] <-
       "rm"
   })

   observeEvent(input$overview_click, { # Click in the overview plot: set focus
     values$time_focus <- values$imu_data$ts[
       which.min((input$overview_click$x - as.numeric(values$imu_data$ts))^2)]
   })

   observeEvent(input$acc_overview_click, { # Click in the acc overview plot: set highlight
     values$click_id_gps <- values$out_data$id[
       which.min((input$acc_overview_click$x -
                    as.numeric(values$out_data$ts))^2)]
     values$click_id_imu <- values$imu_data[
       findInterval(input$acc_overview_click$x, values$imu_data$ts)[1], "id"]
     print(paste("Click ids IMU/GPS:", values$click_id_imu,
                 values$click_id_gps))
     print(paste("IMU/GPS times: ", values$imu_data$ts[values$click_id_imu],
                 values$gps_data$ts[values$click_id_gps]))
   })

   observeEvent(input$speed_overview_click, { # Click in the speed overview plot: set highlight
     values$click_id_gps <- values$out_data$id[
       which.min((input$speed_overview_click$x -
                    as.numeric(values$out_data$ts))^2)]
     values$click_id_imu <- values$imu_data[
       findInterval(input$speed_overview_click$x, values$imu_data$ts)[1], "id"]
     print(paste("Click ids IMU/GPS:", values$click_id_imu,
                 values$click_id_gps))
     print(paste("IMU/GPS times: ", values$imu_data[values$click_id_imu, "ts"],
                 values$gps_data[values$click_id_gps, "ts"]))
   })

   output$download <- downloadHandler( # Download button
     filename = function() {
       paste0('dummy.csv')
     },
     content = function(file) {
       stopifnot(all(values$imu_data$flag_mode_imu != "unk"))
       write.csv(merge_sources(gps = values$gps_data, imu = values$imu_data),
                 file, row.names = FALSE)
     }
   )

}

# Run the application
shinyApp(ui = ui, server = server)

