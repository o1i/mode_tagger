add_one <- function(v){
  # Not currently used
  return(c(v, v[length(v)] + 1))
}

makelines <- function(mat, id){
  Line(mat) %>%
    list() %>%
    Lines(ID = id)
}

mcpl <- function(map, lon, lat, col){
  print(str(lon))
  print(str(lat))
  print(is.numeric(lat))
  stopifnot(is.numeric(lon),
            is.numeric(lat),
            length(lon) == length(lat),
            length(lon) == length(col))

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

cppFunction('
            NumericVector match_inds(NumericVector t1, NumericVector t2){
            /*Find closest elements to elements of t2 in t1. Assume both are sorted ascendingly*/
            int n1 = t1.size();
            int n2 = t2.size();
            int j = 0;
            NumericVector out(n2);

            for(int i = 0; i<n2; ++i) {
            if(t1[j] > t2[i]){
            out[i] = j + 1;
            }else{
            if(pow(t1[j] - t2[i], 2) < pow(t1[j + 1] - t2[i], 2)){
            out[i] = j + 1;
            } else{
            j += 1;
            j = std::min(j, n1 - 1);
            i -= 1;
            }
            }
            }
            return out;
            }
            ')

merge_sources <- function(gps, imu){
  print("Merging sources")
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
  print("Getting Matches")
  matches <- match_inds(as.numeric(imu$ts), as.numeric(gps$ts))

  # Matches further than a second from any IMU signal should be discarded
  keep <- abs(as.numeric(imu$ts - gps$ts, units = "secs")) < 1
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
