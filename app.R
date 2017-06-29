#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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

mode_col_palette <- c("#777777", brewer.pal(8, "Dark2"), "#f03b20", "#7a0177", "#08519c", "#FF0000")
mode_cols <- setNames(as.list(mode_col_palette), as.character(unlist(modes)))

# Define UI for application that draws a histogram
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
      hr(),
      actionButton("shift_f", "Move Ahead"),
      actionButton("shift_b", "Move Back"),
      actionButton("zoom_in", "Zoom In"),
      actionButton("zoom_out", "Zoom Out"),
      actionButton("center", "Center"),
      actionButton("remove", "Remove rest"),
      hr(),
      selectInput(
        'mode_select', 'Select Mode', choices = modes,
        selectize = FALSE
      ),
      actionButton("set_label", "Apply label"),
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

# Define server logic required to draw a histogram
server <- function(input, output) {

  values <- reactiveValues()

  # --- Reading Data Events ----------------------------------------------------
  observeEvent(input$gpsfile, { # GPS Data
    if(!is.null(input$gpsfile)){
      print("Loading GPS Data")
      values$gps_data <- read.csv(input$gpsfile$datapath, header = T, sep = ",",
                           dec = ".", strip.white = TRUE,
                           stringsAsFactors = FALSE) %>%
        filter(Longitude != "Charging") %>%
        mutate(Latitude = as.numeric(Latitude),
               Longitude = as.numeric(Longitude),
               flag_interpol = is.na(Longitude) | is.na(Latitude),
               Latitude = approx(x = seq_along(Latitude),
                                 y = Latitude,
                                 xout = seq_along(Latitude),
                                 rule = 2)$y,
               Longitude = approx(x = seq_along(Longitude),
                                  y = Longitude,
                                  xout = seq_along(Longitude),
                                  rule = 2)$y,
               Time = gsub("^([0-9]*:[0-9]*:[0-9]*):([0-9]*).*$", "\\1.\\2", paste0(Time, ":000")),
               ts = as.POSIXct(paste(Date, Time), format = "%d/%m/%Y %H:%M:%OS"),
               id = as.character(row_number()),
               flag_mode_gps = "unk") %>%
        arrange(ts) %>%
        select(-c(Date, Time))
      rownames(values$gps_data) <- values$gps_data$id
      values$focus_time <- min(values$gps_data$ts, na.rm = TRUE)
      values$time_window <- 60*5
      values$click_id_imu <- 1
      values$click_id_gps <- 1
      print("GPS Data Loaded")
    }
  })

  observeEvent(input$imufile, { # IMU Data
    if(!is.null(input$imufile)){
      print("Loading IMU Data")
      values$imu_data <- read.csv(input$imufile$datapath, header = T, sep = ",",
                                  dec = ".", strip.white = TRUE,
                                  stringsAsFactors = FALSE) %>%
        mutate(Time = gsub("^([0-9]*:[0-9]*:[0-9]*):([0-9]*).*$", "\\1.\\2", paste0(Time, ":000")),
               ts = as.POSIXct(paste(Date, Time), format = "%d/%m/%Y %H:%M:%OS"),
               id = as.character(row_number()),
               acc_tot = sqrt(Acc_X.mg.^2 + Acc_Y.mg.^2 + Acc_Z.mg.^2),
               flag_mode_imu = "unk") %>%
        arrange(ts) %>%
        select(-c(Date, Time))
      rownames(values$imu_data) <- values$imu_data$id
      dt <- diff(values$imu_data$ts, units = "secs")
      ind_isolated <- c(TRUE, dt > 5) & c(dt > 5, TRUE)
      values$imu_data <- values$imu_data[!ind_isolated, ]
      print("IMU Data Loaded")
    }
  })

  observeEvent({ # Out dataset (plotted on map)
    values$time_window
    values$focus_time
  }, {
    values$out_data <- values$gps_data[
      (values$focus_time + values$time_window * c(-1, 1)) %>%
        findInterval(values$gps_data$ts) %>%
        `+`(c(-1, 1)) %>%
        pmax(1) %>%
        pmin(nrow(values$gps_data)) %>%
        as.list() %>%
        do.call(what = `:`), ]
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
     inds <- seq(1, nrow(values$imu_data), by = 1000)
     if(sum(values$imu_data$flag_mode_imu == "unk") < 20000){
       inds <- c(inds, which(values$imu_data$flag_mode_imu == "unk"))
     }
     plot(NULL, xlim = range(values$imu_data$ts), ylim = c(1, length(modes)),
          ylab = "", bty = "n", xaxt = 'n', yaxt = 'n')
     if(length(values$gps_gap_starts) > 1){
       rect(values$gps_gap_starts, -1,
            values$gps_gap_ends, length(modes) + 2,
            col = rgb(1, 0, 0, 0.3), border = NA)
     }
     xlabels <- seq(min(values$gps_data$ts), max(values$gps_data$ts), l = 6)
     axis(1, at = xlabels, labels = strftime(xlabels, format = "%H:%M", tz = "CET"))
     axis(2, at = 1:length(modes), labels = names(modes), las = 1, tick = FALSE)

     rect(values$focus_time - values$time_window, -1,
          values$focus_time + values$time_window, 100,
          col = "#AAAAAA", border = NA)
     abline(v = range(values$out_data$ts))

     points(x = values$imu_data$ts[inds],
            y = as.numeric(factor(values$imu_data$flag_mode_imu[inds],
                                  levels = as.character(unlist(modes)))),
            col = mode_col_palette[as.numeric(factor(values$imu_data$flag_mode_imu[inds],
                                              levels = as.character(unlist(modes))))])
     abline(v = values$overview_hover_x)
   })

  output$map <- renderLeaflet({ # Map
    print("Printing map")
    print(paste0("Map contains ", nrow(values$out_data), " Points."))
    point_cols <- get_point_colour(values$out_data)
    line_cols <- get_line_colour(values$out_data$flag_mode_gps, mode_cols)
    print(paste("IMU time:", values$imu_data[values$click_id_imu, "ts"]))
    print(paste("GPS time:", values$out_data[values$click_id_gps, "ts"]))
    print(paste("diff:", abs(as.numeric(values$out_data[values$click_id_gps, "ts"]) -
                               as.numeric(values$imu_data[values$click_id_imu, "ts"]))))
    print(paste("Condition: ", (abs(as.numeric(values$out_data[values$click_id_gps, "ts"]) -
                                      as.numeric(values$imu_data[values$click_id_imu, "ts"])) < 3 )))
    if(abs(as.numeric(values$out_data[values$click_id_gps, "ts"]) -
           as.numeric(values$imu_data[values$click_id_imu, "ts"])) < 3 ){
      highlight_ind <- which(values$out_data$id == values$click_id_gps)
    }else{
      highlight_ind <- findInterval(values$imu_data[values$click_id_imu, "ts"],
                                    values$out_data$ts)
      print(paste("GPS map-interval:", highlight_ind))
    }
    print("---------- Length comparison:")
    print(length(values$out_data$flag_mode_gps))
    print(length(line_cols))
    print(length(values$out_data$Longitude))
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
     imu_used <- subset(values$imu_data,
                        abs(as.numeric(values$imu_data$ts -
                                         values$focus_time,
                                       units = "secs")) < values$time_window)
     imu_used <- imu_used[unique(floor(seq(1, nrow(imu_used), l = 2000))), ]

     plot(NULL, xlim = range(imu_used$ts), ylim = c(200, 1800),
          ylab = "", bty = "n", xaxt = 'n', yaxt = 'n', main = "Acceleration")
     xlabels <- seq(min(imu_used$ts), max(imu_used$ts), l = 4)
     axis(1, at = xlabels, labels = strftime(xlabels, format = "%H:%M:%S", tz = "CET"))
     lines(x = imu_used$ts,
           y = imu_used$acc_tot)
     points(x = imu_used$ts,
            y = imu_used$acc_tot,
            pch = 16, cex = 0.6, col = get_line_colour(imu_used$flag_mode_imu, mode_cols))
     abline(h = 1000)
     abline(v = values$imu_data[values$click_id_imu, "ts"])
   })

   output$speed_overview <- renderPlot({ # Speed
     par(mar = c(2.5, 8.1, 2.5, 2.1))
     plot(NULL, xlim = range(values$out_data$ts), ylim = range(0, 150),
          ylab = "", bty = "n", xaxt = 'n', yaxt = 'n', main = "Speed")
     xlabels <- seq(min(values$out_data$ts), max(values$out_data$ts), l = 4)
     axis(1, at = xlabels, labels = strftime(xlabels, format = "%H:%M:%S", tz = "CET"))
     axis(2, at = c(4.5, 30, 50, 80, 100), tick = F)
     abline(h = c(4.5, 30, 50, 80, 100), col = "#AAAAAA")
     lines(x = pmax(0, values$out_data$ts),
           y = values$out_data$Speed)
     points(x = pmax(0, values$out_data$ts),
           y = values$out_data$Speed,
           pch = 16, cex = 0.6, col = unlist(mode_cols[values$out_data$flag_mode_gps]))
     abline(v = values$imu_data[values$click_id_imu, "ts"])
     print(head(values$out_data, 3))
   })


   # --- Events ----------------------------------------------------------------
   observeEvent({ # Determine the gaps
     values$gps_data
     values$imu_data
   }, {
     print("Potentially listening to GPS gap events")
     print(values$imu_data$ts[1])
     print(values$gps_data$ts[1])
     print(abs(as.numeric(values$imu_data$ts[1] - values$gps_data$ts[1], units = "secs")))
     if(!is.null(values$gps_data) & !is.null(values$imu_data)){
       print("Really determining GPS gaps")
       if(abs(as.numeric(values$imu_data$ts[1] - values$gps_data$ts[1], units = "secs")) > 5){
         starts <- values$imu_data$ts[1]
         ends <- values$gps_data$ts[1]
       }else{
         starts <- numeric(0)
         ends <- numeric(0)
       }
       midgaps <- which(diff(as.numeric(values$gps_data$ts, units = "secs")) > 5)
       if(length(midgaps) > 0){
         starts <- c(starts, values$gps_data$ts[midgaps])
         ends <- c(ends, values$gps_data$ts[midgaps + 1])
       }
       if(abs(as.numeric(values$imu_data$ts[nrow(values$imu_data)] - values$gps_data$ts[nrow(values$gps_data)])) > 5){
         starts <- c(starts, values$gps_data$ts[nrow(values$gps_data)])
         ends <- c(ends, values$imu_data$ts[nrow(values$imu_data)])
       }
      values$gps_gap_starts <- starts
      values$gps_gap_ends <- ends
     }
   })


   observeEvent(input$map_marker_click, {#Observer to show Popups on click
     click <- input$map_marker_click
     if (!is.null(click)) {
       time_pressed <- values$out_data[click$id, "ts"]
       values$click_id_imu <- values$imu_data[findInterval(time_pressed, values$imu_data$ts)[1], "id"]
       values$click_id_gps <- click$id
       print(paste("Click id:", values$click_id_imu))
     }
   })

   observeEvent(input$shift_f, { # Move ahead button
     values$focus_time <- values$focus_time + floor(values$time_window)
   })

   observeEvent(input$shift_b, { # Move back button
     values$focus_time <- values$focus_time - floor(values$time_window)
   })

   observeEvent(input$zoom_in, { # Zoom In button
     values$time_window <- values$time_window / 2
   })

   observeEvent(input$zoom_out, { # Zoom Out button
     values$time_window <- values$time_window * 2
   })

   observeEvent(input$center, { # Center button
     values$focus_time <- values$imu_data[values$click_id_imu, "ts"]
   })

   observeEvent(input$set_label, { # Button setting labels
     values$gps_data$flag_mode_gps <- insert_value(values$gps_data$flag_mode_gps,
                                               as.numeric(values$click_id_gps),
                                               input$mode_select)
     values$imu_data$flag_mode_imu <- insert_value(values$imu_data$flag_mode_imu,
                                                  as.numeric(values$click_id_imu),
                                                  input$mode_select)
   })

   observeEvent(input$remove, { # Remove rest button
     values$imu_data$flag_mode_imu[values$imu_data$flag_mode_imu == "unk"] <- "rm"
     values$gps_data$flag_mode_gps[values$gps_data$flag_mode_gps == "unk"] <- "rm"
   })

   observeEvent(input$overview_click, { # Click in the overview plot: set focus
     values$focus_time <- values$imu_data$ts[which.min((input$overview_click$x - as.numeric(values$imu_data$ts))^2)]
   })

   observeEvent(input$acc_overview_click, { # Click in the acc overview plot: set highlight
     values$click_id_gps <- values$out_data$id[which.min((input$acc_overview_click$x - as.numeric(values$out_data$ts))^2)]
     values$click_id_imu <- values$imu_data[findInterval(input$acc_overview_click$x, values$imu_data$ts)[1], "id"]
     print(paste("Click ids IMU/GPS:", values$click_id_imu, values$click_id_gps))
   })

   observeEvent(input$speed_overview_click, { # Click in the speed overview plot: set highlight
     values$click_id_gps <- values$out_data$id[which.min((input$speed_overview_click$x - as.numeric(values$out_data$ts))^2)]
     values$click_id_imu <- values$imu_data[findInterval(input$speed_overview_click$x, values$imu_data$ts)[1], "id"]
     print(paste("Click ids IMU/GPS:", values$click_id_imu, values$click_id_gps))
   })

   output$download <- downloadHandler( # Download button
     filename = function() {
       paste0('dummy.csv')
     },
     content = function(file) {
       stopifnot(all(values$imu_data$flag_mode_imu != "unk"))
       write.csv(merge_sources(values$gps_data, values$imu_data), file, row.names = FALSE)
     }
   )

}

# Run the application
shinyApp(ui = ui, server = server)

