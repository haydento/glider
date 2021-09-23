# functions used to extract science and mission data from glider and create leaflet map

#' @title utility function to convert lat/lon in sci and mission data to decimal degrees
#' @param x vector of lat or lons, either science or mission data with lat and lon
#' @description function called within "clean" function

to_dd <- function(x){

  x <- stringr::str_pad(x, width = 10, pad = "0", side = "right")
  hemisphere <- substr(x, 1,1)
  hemisphere <- ifelse(hemisphere == "-", -1, 1)
  degrees <- ifelse(hemisphere == -1, as.numeric(substr(x, 2, 3)), as.numeric(substr(x, 1, 2)))
  minutes <- ifelse(hemisphere == -1, as.numeric(substr(x, 4,(nchar(x)))), as.numeric(substr(x, 3,(nchar(x)))))
  dd <- hemisphere * (degrees + (minutes/60))
  return(dd)
}


#' @title cleans up and formats science and mission data files
#' @description removes 2nd line of header, sets time column to POSIX, and convert lat/lon to decimal degrees
#' @param pth file path to mission or science data offload
#' @param lat column in file containing latitude
#' @param lon column in file containing longitude values
#' @return function returns data table of cleaned table
#' @examples
#' #single file
#' clean("data/glider_decimated/science/ascii_depth_lat_lon_alt_temp_1632243087.txt", 
#'       lat = m_gps_lat, lon = m_gps_lon)
#'
#' #multiple files
#' pth_files <- list.files("data/glider_decimated/mission", full.names = TRUE)
#' clean(pth_files[1:2], lat = m_gps_lat, lon = m_gps_lon)

clean <- function(pth, lat = m_gps_lat, lon = m_gps_lon){
  header <- data.table::fread(pth[1], nrows = 0)
  dta_list <- lapply(pth, data.table::fread, skip = 2, na.strings = "NaN")
  dta <- data.table::rbindlist(dta_list)
  data.table::setnames(dta, names(dta), names(header))
  data.table::set(dta, j="time", 
                  value = as.POSIXct(dta$time, 
                                     origin = "1970-01-01 00:00:00", 
                                     tz = "UTC"))
  dta[, lat_dd := to_dd(m_gps_lat)]
  dta[, lon_dd := to_dd(m_gps_lon)]
  return(dta)

}

#' @title combines both science and mission data into single object.
#' @description function combines mission and science data into single data.table.  Also removes any records without both latitude and longitude
#' @param x science data
#' @param y mission data

combine <- function(x,y){  
  #x <- x[!(is.na(m_gps_lon) | is.na(m_gps_lat)),]
  #y <- y[!(is.na(m_gps_lon) | is.na(m_gps_lat)),]
  out <- rbind(x, y, fill = TRUE)
  setkey(out, time)
}

#' @title creates interactive map of glider track
#' @description writes out html widget of glider track
#' @param glider_track combined data.table containing surface points of glider
#' @param pth output file path (character)
#' @examples
#' tar_load(glider_trk) 
#' leaflet_map(glider_track = glider_trk, pth = "output/test.html")

leaflet_map <- function(glider_track = f3, 
                        dtc = clean_vem_detections, 
                        pth){
  recs <- fst::read_fst("data/receiver_coords.fst")

  m <- leaflet()
  m <- setView(m, zoom = 15, lat = 45.537 , lng = -83.999)
  m <- addTiles(m)
  #m <- leafem::addGeoRaster(m, bath, opacity = 1, colorOptions = leafem::colorOptions(palette = viridis::viridis(256), breaks = brks_bath$brks), group = "bathy (ft)")
  m <- addTiles(m, urlTemplate = "http://tileservice.charts.noaa.gov/tiles/50000_1/{z}/{x}/{y}.png", group = "nav chart")
  m <- addProviderTiles(m, providers$Esri.WorldImagery, group = "satellite")
  m <- addProviderTiles(m, providers$Esri.NatGeoWorldMap, group = "alt")
  #drop missing lat/lon
  glider_track <- glider_track[!(is.na(lon_dd) | is.na(lat_dd)),]
  m <- addPolylines(map = m, data = glider_track, lng = ~lon_dd, lat = ~lat_dd, color = "green")
  
  #  m <- addMarkers(m, lng = -83.58845, lat = 44.08570, label = "release")
  m <- addCircleMarkers(m, data = glider_track, lng = ~lon_dd, lat = ~lat_dd, color = "red", radius = 5, stroke = FALSE, fillOpacity = 1)
  m <- addCircleMarkers(m, data = recs, lng = ~lon, lat = ~lat, color = "blue", radius = 5, stroke = FALSE, fillOpacity = 1)

  #add detection locations
  m <- addCircleMarkers(map = m, data = dtc, lng = ~lon_dd, lat = ~lat_dd, color = "yellow", radius = 3, stroke = FALSE, fillOpacity = 1)
  
  m <- leafem::addMouseCoordinates(m)
  m <- addMeasure(m, primaryLengthUnit = "meters", secondaryLengthUnit = "kilometers")  
  m <- addLayersControl(m, baseGroups = c("satellite", "nav chart", "alt"), position = "bottomright", options = layersControlOptions(collapsed = FALSE))

  htmlwidgets::saveWidget(m, pth)
  return(pth)
}




  
 



  
  
