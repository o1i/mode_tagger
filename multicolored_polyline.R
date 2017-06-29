library(leaflet)
library(sp)

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
