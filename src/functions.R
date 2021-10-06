# functions used to extract science and mission data from glider and create leaflet map

#' @title extract vrls
#' @description function opens and creates csv files from each vrl file
#' @param in_pth path to directory that contains raw vrl files
#' @param out_dir  path to directory where extracted vrl files will be written as .csv files
#' @param vdat_pth  pth to directory that contains vdat command line program
#' @examples
#' tar_load("vrl_data")
#' extract_vrl(in_pth = "~/Desktop/lost_AR_data/vrl", out_dir = "~/Desktop/vrl_to_csv", vdat_pth = "/home/todd/tools")
#' in_pth = "~/Desktop/lost_AR_data/vrl"
#' out_dir = "~/Desktop/vrl_to_csv"
#' vdat_pth = "/home/todd/tools"

extract_vrl <- function(in_pth, out_dir, vdat_pth){
  tdir <- tempdir()
  tdir_arg <- sprintf("--output=%s", tdir)
  
  fls <- list.files(path = in_pth, pattern="*.vrl", full.names = TRUE)
  fls_arg <- paste(shQuote(fls), collapse= " ")
   
  vdat_pth <- file.path(path.expand(vdat_pth), "vdat")

  out_dir <- path.expand(out_dir)
  out_names <- sub(".vrl$", ".csv", x = basename(fls))
  temp_out <- file.path(tdir, out_names)
  final_files <- file.path(out_dir, out_names)

  system2(vdat_pth, c("convert", "--format=csv.fathom", "--timec=default", tdir_arg, fls_arg))

  file.copy(temp_out, out_dir, overwrite = TRUE)
  file.remove(temp_out)
  
  return(final_files)
}


#' @title compile directory of vdat vrl files into single object
#' @description compiles detection data from vdat files extracted from vrl files.  Vdat command line program is used to create vdat files for each vrl.  Function compiles data from each vrl and changes column names to something sane.
#' @param pth path to directory that contains vrl files

#' @return pth path to directory that contains .csv files extracted from .vrl files with vdat software
#' 
#' @examples
#' fls <- list.files("~/Documents/glider_range_test_results_2021/data/vdat_csv", full.names = TRUE)
#' compile_dtc(fls)


compile_dtc <- function(fls){
  name_col <- names(fread(cmd = paste("grep -iw DET_DESC", fls[1])))
  int_trans <- paste("grep -iw", "DET", fls)
  trans <- lapply(int_trans, function(x) fread(cmd = x))
  trans <- rbindlist(trans)
  setnames(trans, paste0("V", 1:length(name_col)), name_col)
  trans[, frequency := as.numeric(gsub("(.*)-", "", Model))]
  set(trans, j = "datetime", value = fasttime::fastPOSIXct(trans$Time, tz = "UTC"))
  setnames(trans, c("Serial Number", "ID", "Sensor Value", "Signal Strength (dB)", "Noise (dB)"), c("serial_no", "transmitter_id", "sensor_value_adc", "signal_level_db", "noise_level_db"))
  set(trans, j = "serial_no", value = as.character(trans$serial_no))
  return(trans)
}

#' @title adds geographic coordinates to detections
#' @description joins instrument data with detections
#' @param vrl compiled detections from multiple vrl files
#' @param hst_l gear deployment log
#' @examples
#' tar_load(instr_deploy_data)
#' tar_load(dtc)
#' vrl <- dtc
#' hst_l <- instr_deploy_data
#' stationary_recs_geo(vrl = vrl, hst_l = hst_l)

stationary_recs_geo <- function(vrl, hst_l){
  
  hst_file <- hst_l[[1]]
  if(length(hst_l) > 1) stop("Can only load one hst file. Need to expand.")
  hst_l <- data.table::fread(hst_file)

  #set all missing timestamps to now for convenience
  if(!inherits(hst_l$timestamp_end_utc, "POSIXct")) {
    hst_l[ , timestamp_end_utc := as.POSIXct(timestamp_end_utc)]
    hst_l[is.na(timestamp_end_utc), timestamp_end_utc := Sys.time()]
    attributes(hst_l$timestamp_end_utc)$tzone <- "UTC"
  }

  ## deep copy
  dtc <- data.table::as.data.table(vrl)
  hst <- data.table::as.data.table(hst_l)
  
  dtc <- dtc[hst, ':='(latitude = latitude, longitude = longitude), on = .(Time >= timestamp_start_utc, Time <= timestamp_end_utc,  serial_no == instr_id)]

  # todo... rename columns to make this file look like .vem data, append this data to .vem and then run through "big joins" functions.
  
  return(dtc[])
}




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


clean <- function(pth, lat = m_pth_lat, lon = m_gps_lon){
  pth_files <- list.files(pth, full.names = TRUE)
  out <- .clean(pth_files, lat = lat, lon = lon)
return(out)
}

.clean <- function(pth, lat = m_gps_lat, lon = m_gps_lon){

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
  out <- rbind(x, y, fill = TRUE)
  out <- unique(out)
  setkey(out, time)
}

#' @title creates interactive map of glider track
#' @description writes out html widget of glider track
#' @param glider_track combined data.table containing surface points of glider
#' @param pth output file path (character)
#' @examples
#' tar_load(glider_trk) 
#' leaflet_map(glider_track = glider_trk, pth = "output/test.html", recs = "data/receiver_coords.fst")

leaflet_map <- function(glider_track = glider_trk, 
                        dtc = clean_vem_detections_geo,
                        recs = "data/receiver_coords.fst",
                        pth, v_pth = vps){
  
  #v_pth <- "data/vps/synthetic.positions/all.csv"
  vps <- data.table::fread(v_pth)
  set(vps, j = "Time", value = fasttime::fastPOSIXct(vps$Time))
  vps <- vps[FullId %in% c("A69-1604-32403"),]

  color_pal <- colorNumeric(palette = "magma", domain = vps$HPEs, reverse = TRUE)
  
  recs <- fst::read_fst(recs)
  MBU1_180 <- dtc[receiver_site == "mary_lou" & receiver_freq == 180 & transmitter_site == "MBU-001",]
  MBU1_180_dtc_ct <- nrow(MBU1_180)

  MBU1_180[, receiver_label := sprintf("ML depth (m): %.1f; tag-ML dist (m): %.0f; dtc date: %s", glider_m_depth, rt_distance_meters, format(datetime, "%Y-%m-%d %H:%M"))]
  MBU1_180[, tag_label := sprintf("180kHz, water depth (m): %.1f; tag depth (m): %.1f; tot dtc count: %.0f", transmitter_water_depth, transmitter_instr_depth_from_top, MBU1_180_dtc_ct)]

  MBU2_180 <- dtc[receiver_site == "mary_lou" & receiver_freq == 180 & transmitter_site == "MBU-002",]
  MBU2_180_dtc_ct <- nrow(MBU2_180)
  
  MBU2_180[, receiver_label := sprintf("ML depth (m): %.1f; tag-ML dist (m): %.0f; dtc date: %s", glider_m_depth, rt_distance_meters, format(datetime, "%Y-%m-%d %H:%M"))]
  MBU2_180[, tag_label := sprintf("180kHz, water depth (m): %.1f; tag depth (m): %.1f; tot dtc count: %.0f", transmitter_water_depth, transmitter_instr_depth_from_top, MBU2_180_dtc_ct)]

  ##################
  MBU1_69 <- dtc[receiver_site == "mary_lou" & (transmitter_instr_model %in% c("V13-1x-H", "V13-1x-L")) & transmitter_site == "MBU-001" ,]
  MBU1_69_dtc_ct <- nrow(MBU1_69)

  MBU1_69[, receiver_label := sprintf("ML depth (m): %.1f; tag-ML dist (m): %.0f; dtc date: %s", glider_m_depth, rt_distance_meters, format(datetime, "%Y-%m-%d %H:%M"))]
  MBU1_69[, tag_label := sprintf("69kHz, water depth (m): %.1f; tag depth (m): %.1f; tot dtc count: %.0f", transmitter_water_depth, transmitter_instr_depth_from_top, MBU1_69_dtc_ct)]

  MBU2_69 <- dtc[receiver_site == "mary_lou" & (transmitter_instr_model %in% c("V13-1x-H", "V13-1x-L")) & receiver_freq == 69 & transmitter_site == "MBU-002",]
  MBU2_69_dtc_ct <- nrow(MBU2_69)
  
  MBU2_69[, receiver_label := sprintf("ML depth (m): %.1f; tag-ML dist (m): %.0f; dtc date: %s", glider_m_depth, rt_distance_meters, format(datetime, "%Y-%m-%d %H:%M"))]
  MBU2_69[, tag_label := sprintf("69kHz, water depth (m): %.1f; tag depth (m): %.1f; tot dtc count: %.0f", transmitter_water_depth, transmitter_instr_depth_from_top, MBU2_69_dtc_ct)]

  ################
  self_dtc_180 <- dtc[receiver_site == "mary_lou" & transmitter_site == "mary_lou" & receiver_freq == 180,]
  self_dtc_180[, label := sprintf("180kHz, ML depth (m): %.1f; dtc date: %s", glider_m_depth, format(datetime, "%Y-%m-%d %H:%M"))]
  self_dtc_69 <- dtc[receiver_site == "mary_lou" & transmitter_site == "mary_lou" & receiver_freq == 69,]
  self_dtc_69[, label := sprintf("69kHz, ML depth (m): %.1f; dtc date: %s", glider_m_depth, format(datetime, "%Y-%m-%d %H:%M"))]
  

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

  #vps
  m <- addPolylines(map = m, data = vps, lng = ~Longitude, lat = ~Latitude, color = "purple", group = "vps")
  m <- addCircleMarkers(m, data = vps, lng = ~Longitude, lat = ~Latitude, color = ~color_pal(HPEs), radius = 10, stroke = FALSE, fillOpacity = 1, group = "vps")
  
  #  m <- addMarkers(m, lng = -83.58845, lat = 44.08570, label = "release")
  m <- addCircleMarkers(m, data = glider_track, lng = ~lon_dd, lat = ~lat_dd, color = "green", radius = 5, stroke = FALSE, fillOpacity = 1)
  m <- addCircleMarkers(m, data = recs, lng = ~lon, lat = ~lat, color = "blue", radius = 8, stroke = FALSE, fillOpacity = 1)

  #add MBU-001-180 receiver and detections
  m <- addCircleMarkers(map = m, data = MBU1_180, lng = ~receiver_longitude, lat = ~receiver_latitude, color = "yellow", radius = 9, stroke = FALSE, fillOpacity = 1, group = "Tag-180deep", label = ~receiver_label)
  
  m <- addCircleMarkers(map = m, data = MBU1_180, lng = ~transmitter_longitude, lat = ~transmitter_latitude, color = "red", radius = 9, stroke = FALSE, fillOpacity = 1, group = "Tag-180deep", label = ~tag_label)
  #add MBU-002-180 receiver and detections
  m <- addCircleMarkers(map = m, data = MBU2_180, lng = ~receiver_longitude, lat = ~receiver_latitude, color = "yellow", radius = 9, stroke = FALSE, fillOpacity = 1, group = "Tag-180shallow", label = ~receiver_label)
  m <- addCircleMarkers(map = m, data = MBU2_180, lng = ~transmitter_longitude, lat = ~transmitter_latitude, color = "red", radius = 9, stroke = FALSE, fillOpacity = 1, group = "Tag-180shallow", label = ~tag_label)

  # add MBU-001-69 detections
    m <- addCircleMarkers(map = m, data = MBU1_69, lng = ~receiver_longitude, lat = ~receiver_latitude, color = "yellow", radius = 9, stroke = FALSE, fillOpacity = 1, group = "Tag-69deep", label = ~receiver_label)
  
  m <- addCircleMarkers(map = m, data = MBU1_69, lng = ~transmitter_longitude, lat = ~transmitter_latitude, color = "red", radius = 9, stroke = FALSE, fillOpacity = 1, group = "Tag-69deep", label = ~tag_label)
  #add MBU-002-180 receiver and detections
  m <- addCircleMarkers(map = m, data = MBU2_69, lng = ~receiver_longitude, lat = ~receiver_latitude, color = "yellow", radius = 9, stroke = FALSE, fillOpacity = 1, group = "Tag-69shallow", label = ~receiver_label)
  m <- addCircleMarkers(map = m, data = MBU2_69, lng = ~transmitter_longitude, lat = ~transmitter_latitude, color = "red", radius = 9, stroke = FALSE, fillOpacity = 1, group = "Tag-69shallow", label = ~tag_label)

  # self dtc-180
  m <- addCircleMarkers(map = m, data = self_dtc_180, lng = ~transmitter_longitude, lat = ~transmitter_latitude, colo = "orange", radius = 9, stroke = FALSE, fillOpacity = 1, group = "self-dtc,180", label = ~label)
  m <- addCircleMarkers(map = m, data = self_dtc_69, lng = ~transmitter_longitude, lat = ~transmitter_latitude, colo = "orange", radius = 9, stroke = FALSE, fillOpacity = 1, group = "self-dtc,69", label = ~label)
  
  m <- leafem::addMouseCoordinates(m)
  m <- addMeasure(m, primaryLengthUnit = "meters", secondaryLengthUnit = "kilometers")  
  m <- addLayersControl(m, baseGroups = c("satellite", "nav chart", "alt"), overlayGroups = c("Tag-180deep", "Tag-180shallow", "Tag-69deep", "Tag-69shallow", "self-dtc,180", "self-dtc,69", "vps"),position = "bottomright", options = layersControlOptions(collapsed = FALSE))
  #m <- addLegend( map = m, pal = color_pal, values = ~HPEs, title = "HPE", opacity=1)

  
  htmlwidgets::saveWidget(m, pth)
  return(pth)
}




  
 



  
  
