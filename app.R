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

source("multicolored_polyline.R")
Sys.setenv(TZ='GMT')

get_point_colour <- function(df){
  # Gets the colors of the GPS points to be displayed
  c <- c("#3333DD", "#999999")
  ind <- as.numeric(df$flag_interpol) * 2 + as.numeric(!df$flag_interpol)

  a <- as.numeric(!df$flag_interpol) / 0.9 + 0.2
  return(list(c = c[ind], a = a))
}

get_line_colour <- function(v, cols){
  # gets colours based on modes of transport.
  # Colors and modes are hard coded here
  return(as.character(unlist(cols[v])))
}

options(shiny.maxRequestSize=1000*1024^2) #  Max size of input files

insert_value <- function(v, ind, val){
  # Inserts a new value into a vector v at a position and before
  #
  # Inputs:
  #   v   The vector to be altered
  #   ind The last position to be changed (ony same values)
  #   val The new value to insert
  #
  # Output: the corrected vector v
  first <- v[1:ind] %>%
    rev() %>%
    `!=`(v[ind]) %>%
    c(TRUE) %>%         # For the case if we are in the first segment
    which.max()
  v_out <- v
  v_out[(ind - first + 2):ind] <- val
  return(v_out)
}

# Different modes
modes <- list("Unknown" = "unk",
              "Active movement" = "mv_a",
              "Passive movement" = "mv_p",
              "Active stationary" = "st_a",
              "Passive stationary" = "st_p",
              "Remove" = "rm")

mode_col_palette <- c("#777777", brewer.pal(4, "Paired"), "#FF0000")
mode_cols <- list(unk = "#999999",
             mv_a = mode_col_palette[1],
             mv_p = mode_col_palette[2],
             st_a = mode_col_palette[3],
             st_p = mode_col_palette[4],
             rm = mode_col_palette[length(mode_col_palette)])


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
      hr(),
      selectInput(
        'mode_select', 'Select Mode', choices = modes,
        selectize = FALSE
      ),
      actionButton("set_label", "Apply label"),
      hr(),
      textInput("text", label = ("Filename of output"), value = "flagged_data"),
      downloadButton('download', 'Download')
      ),  # End of sidebar panel
      # Show a plot of the generated distribution
      mainPanel(
        # textOutput("str"),
        plotOutput("overview", width = "800px", height = "150px",
                   click = "overview_click", hover = "overview_hover"),
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

  observeEvent(input$gpsfile, {
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

  observeEvent(input$imufile, {
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
      print("IMU Data Loaded")
    }
  })

  # values$out_data <- reactive(
  #   gps_data[pmin(nrow(values$gps_data)) %>%
  #   pmin(1) %>%
  #   as.list() %>%
  #   do.call(what = `:`), ])

   output$map <- renderLeaflet({
     print("Printing map")
     inds <- pmax(1, pmin(nrow(values$gps_data),
                          findInterval(as.numeric(values$focus_time) +
                                         c(-values$time_window,
                                           values$time_window),
                                       values$gps_data$ts) + c(-1, 1)))
     values$out_data <- values$gps_data[inds[1]:inds[2], ]
     print(paste0("Map contains ", nrow(values$out_data), " Points."))
     point_cols <- get_point_colour(values$out_data)
     line_cols <- get_line_colour(values$out_data$flag_mode, mode_cols)
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

     leaflet() %>% addTiles() %>%
       addCircleMarkers(lng = values$out_data$Longitude,
                        lat = values$out_data$Latitude,
                        layerId = values$out_data$id,
                        color = point_cols$c, opacity = point_cols$a) %>%
       mcpl(lon = values$out_data$Longitude, lat = values$out_data$Latitude,
            col = line_cols) %>%
       addCircleMarkers(lng = values$out_data$Longitude[highlight_ind],
                        lat = values$out_data$Latitude[highlight_ind],
                        color = "#CC0000")

   })

   output$overview <- renderPlot({
     par(mar = c(2.5, 8.1, 1, 2.1))
     inds <- seq(1, nrow(values$gps_data), by = 15)
     if(sum(values$gps_data$flag_mode == "unk") < 3600){
       inds <- c(inds, which(values$gps_data$flag_mode == "unk"))
     }
     plot(NULL, xlim = range(values$gps_data$ts), ylim = c(1, length(modes)),
          ylab = "", bty = "n", xaxt = 'n', yaxt = 'n')
     xlabels <- seq(min(values$gps_data$ts), max(values$gps_data$ts), l = 4)
     axis(1, at = xlabels, labels = strftime(xlabels, format = "%H:%M:%S", tz = "CET"))
     axis(2, at = 1:length(modes), labels = names(modes), las = 1, tick = FALSE)
     rect(min(values$out_data$ts), -1, max(values$out_data$ts), 100, col = "#AAAAAA")
     points(x = values$gps_data$ts[inds],
            y = as.numeric(factor(values$gps_data$flag_mode[inds],
                                  levels = as.character(unlist(modes)))),
            col = mode_col_palette[as.numeric(factor(values$gps_data$flag_mode[inds],
                                              levels = as.character(unlist(modes))))])
     abline(v = values$overview_hover_x)
   })

   output$acc_overview <- renderPlot({
     par(mar = c(2.5, 8.1, 2.5, 2.1))
     imu_used <- subset(values$imu_data, values$imu_data$ts > min(values$out_data$ts) &
                          values$imu_data$ts < max(values$out_data$ts))
     imu_used <- imu_used[unique(floor(seq(1, nrow(imu_used), l = 2000))), ]

     plot(NULL, xlim = range(imu_used$ts), ylim = c(200, 1800),
          ylab = "", bty = "n", xaxt = 'n', yaxt = 'n', main = "Acceleration")
     xlabels <- seq(min(imu_used$ts), max(imu_used$ts), l = 4)
     axis(1, at = xlabels, labels = strftime(xlabels, format = "%H:%M:%S", tz = "CET"))
     lines(x = imu_used$ts,
            y = imu_used$acc_tot)
     abline(h = 1000)
     abline(v = values$imu_data[values$click_id_imu, "ts"])
   })

   output$speed_overview <- renderPlot({
     par(mar = c(2.5, 8.1, 2.5, 2.1))
     plot(NULL, xlim = range(values$out_data$ts), ylim = range(0, 150),
          ylab = "", bty = "n", xaxt = 'n', yaxt = 'n', main = "Speed")
     xlabels <- seq(min(values$out_data$ts), max(values$out_data$ts), l = 4)
     axis(1, at = xlabels, labels = strftime(xlabels, format = "%H:%M:%S", tz = "CET"))
     abline(h = c(4.5, 30, 50, 80, 100), col = "#AAAAAA")
     lines(x = pmax(0, values$out_data$ts),
           y = values$out_data$Speed)
     abline(v = values$imu_data[values$click_id_imu, "ts"])
     print(head(values$out_data, 3))
   })

   observeEvent(input$map_marker_click, {#Observer to show Popups on click
     click <- input$map_marker_click
     if (!is.null(click)) {
       time_pressed <- values$out_data[click$id, ts]
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

   observeEvent(input$overview_hover, { # Hover over the overview plot
     values$overview_hover_x <- values$gps_data$ts[which.min((input$overview_hover$x - as.numeric(values$gps_data$ts))^2)]
   })

   observeEvent(input$overview_click, { # Click in the overview plot: set focus
     values$focus_time <- values$gps_data$ts[which.min((input$overview_click$x - as.numeric(values$gps_data$ts))^2)]
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

   output$download <- downloadHandler(
     filename = function() {
       paste0(input$output_name, '.csv')
     },
     content = function(file) {
       write.csv(values$gps_data, file, rownames = FALSE)
     }
   )

}

# Run the application
shinyApp(ui = ui, server = server)

