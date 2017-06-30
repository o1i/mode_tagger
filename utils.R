makelines <- function(mat, id){
  # This functions takes a matrix of coordinates and makes it a Lines object
  # Inputs:
  # mat  a nx2 matrix with the coordinates
  # id   a name for the Polyline (must be unique if you make multiple of these)
  #
  # Outputs:
  # A Lines object
  Line(mat) %>%
    list() %>%
    Lines(ID = id)
}

mcpl <- function(map, lon, lat, col){
  # Makes a Multi-Colored-Polyline with each segment having its own colour
  #
  # Inputs:
  # map  A leaflet map
  # lon  A vector with longitudes
  # lat  A vector with latitudes
  # col  A vector with colours (same length, last item will be ignored)
  #
  # Outputs:
  # The input map with an added Polyline layer.
  stopifnot(is.numeric(lon),
            is.numeric(lat),
            length(lon) == length(lat),
            length(lon) == length(col))

  # Assuming most points will be the same colours, there is one Polyline until
  #   the colour changes
  inds <- c(0, as.factor(col) %>%
    as.numeric() %>%
    diff() %>%
    `!=`(0) %>%
    cumsum()) %>%
    split(x = 1:(length(col)))
  lines <- inds %>%
    lapply(function(v) makelines(cbind(lon[v], lat[v]), id = min(v))) %>%
    unname() %>%
    SpatialLines(proj4string = CRS("+init=epsg:4326"))
  map %>% addPolylines(data = lines, color = col[as.numeric(sapply(inds, min))],
                       opacity = 0.9)
}

get_point_colour <- function(df){
  # Gets the colors of the GPS points to be displayed
  # Not really nice, as it assumes that the input dataframe contains a column
  #   called flag_interpol
  # Paints the points Blue and the interpolated values gray

  # Returns a list with two vectors: c, the colours, a the alphas
  c <- c("#3333DD", "#999999")
  ind <- as.numeric(df$flag_interpol) * 2 + as.numeric(!df$flag_interpol)

  a <- as.numeric(!df$flag_interpol) / 0.9 + 0.2
  return(list(c = c[ind], a = a))
}

get_line_colour <- function(v, cols){
  # gets colours based on modes of transport.
  # Inputs:
  # v      a vector with the name of the transport modes
  # cols   a list whose name correspond to the transport modes and whose content
  #        are the colours
  #
  # Outputts:
  # The colours of the modes of transport in v
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

# As timestamps are very large vectors but are sorted, finding the "closest"
#   matches (L2) of one in the other is best not done in R but in RC++.
# Inputs:
# t1, t2: numeric vectors
#
# Outputs:
# The indices (1-based, as in R) for every value in t2 of the closest point in
#   t1.
cppFunction('
            NumericVector match_inds(NumericVector t1, NumericVector t2){
              /*Find closest elements to elements of t2 in t1. Assume both are sorted ascendingly*/
              int n1 = t1.size();
              int n2 = t2.size();
              int j = 0;
              NumericVector out(n2);

              for(int i = 0; i<n2; ++i) {
                if(t1[j] >= t2[i]){
                out[i] = j + 1;
              }else{
                if(pow(t1[j] - t2[i], 2) <= pow(t1[j + 1] - t2[i], 2)){
                  out[i] = j + 1;
                } else{
                  j += 1;
                  i -=1;
                  if(j >= (n1 - 1)){
                    i +=1;
                    out[i] = n1;
                  }
              }
              }
              }
              return out;
            }
            ')

merge_sources <- function(gps, imu){
  # Merges the relevant info of GPS and IMU files into one for output (download)
  #
  # Inputs:
  # The gps and imu files (stored in values$***_data)
  #
  # Output:
  # The IMU file with added info from GPS. Values are filled down to avoid NAs.
  print("Merging sources")
  print(paste("GPS has", nrow(gps), "rows"))
  print(paste("IMU has", nrow(imu), "rows"))
  print(head(imu, 2))
  imu <- imu %>%
    filter(flag_mode_imu != "rm") %>%
    select(-id) %>%
    mutate(flag_gps = FALSE,
           Longitude = NA,
           Latitude = NA,
           Satellites = NA,
           Altitude = NA,
           VDOP = NA,
           HDOP = NA,
           Speed = NA,
           flag_interpol = NA)
  print(str(as.numeric(imu$ts)))
  print(str(as.numeric(gps$ts)))
  save(imu, file = "imu.rda")
  save(gps, file = "gps.rda")
  print("Getting Matches")
  matches <- match_inds(as.numeric(imu$ts), as.numeric(gps$ts))
  print("Got Matches")
  print(head(matches))
  print(str(matches))

  # Matches further than a second from any IMU signal should be discarded
  keep <- abs(as.numeric(imu$ts[matches] - gps$ts, units = "secs")) < 1
  gps <- gps[keep, ]
  matches <- matches[keep]

  print("Filling GPS info to IMU")
  imu$flag_gps[matches] <- TRUE
  header <- c("Longitude", "Latitude", "Satellites", "Altitude", "VDOP", "HDOP", "Speed", "flag_interpol")
  imu[matches, header] <- gps[, header]
  imu <- fill(imu, Longitude, Latitude, Satellites, Altitude, VDOP, HDOP, Speed, flag_interpol)
  imu[is.na(imu)] <- 0
  return(imu)
}

read_gps <- function(filename){
  # Reading in a GPS file
  #
  # Input:
  # filename  A string pointing to the location of the file to be read
  #
  # Output:
  # A data-frame. Timestaps are added, Longitudes and Latitudes are interpolated
  #   if missing (with a flag)
  out <- read.csv(filename, header = T, sep = ",",
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
  rownames(out) <- out$id
  return(out)
}

read_imu <- function(filename){
  # Reading in an IMU file
  #
  # Input:
  # filename  A string pointing to the location of the file to be read
  #
  # Output:
  # A data-frame. Timestaps are added. Isolated short sequences (of less than
  #   10 minutes) are removed from the file.
  out <- read.csv(filename, header = T, sep = ",",
                  dec = ".", strip.white = TRUE,
                  stringsAsFactors = FALSE) %>%
    mutate(Time = gsub("^([0-9]*:[0-9]*:[0-9]*):([0-9]*).*$", "\\1.\\2", paste0(Time, ":000")),
           ts = as.POSIXct(paste(Date, Time), format = "%d/%m/%Y %H:%M:%OS"),
           id = as.character(row_number()),
           acc_tot = sqrt(Acc_X.mg.^2 + Acc_Y.mg.^2 + Acc_Z.mg.^2),
           flag_mode_imu = "unk") %>%
    arrange(ts) %>%
    select(-c(Date, Time))
  rownames(out) <- out$id

  print("Starting isolation stuff")
  breaks <- which(diff(out$ts, units = "mins") > 10)
  block_starts <- c(1, breaks + 1)
  block_ends <- c(breaks, length(out$ts))
  short_block_inds <- as.numeric(out$ts[block_ends] -
                                   out$ts[block_starts],
                                 units = "mins") < 10

  ind_isolated <- cbind(block_starts[short_block_inds], block_ends[short_block_inds]) %>%
    apply(1, function(v) do.call(as.list(v), what = `:`)) %>%
    unlist()
  print("End isolation stuff")
  out <- out[-ind_isolated, ]
  return(out)
}




