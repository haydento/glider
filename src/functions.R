
#' @title load and clean up full glider data
#' @description Function reads in file, cleans it up for further use
#' @param in_pth file path to full data file.  must be *.csv
#' @examples
#' tar_load(full_glider_raw)
#' .load_glider(in_pth = full_glider_raw)

.load_glider <- function(in_pth){
  dta <- fread(in_pth, na.strings = "NaN")


old_names = c("time (UTC)", "latitude (degrees_north)", "longitude (degrees_east)", "depth (m)", "backscatter (m-1 sr-1)", "cdom (ppb)", "chlorophyll (ug/l)", "conductivity (S m-1)", "density (kg m-3)", "dissolved_oxygen (%)", "instrument_ctd (1)", "m_depth (meters)", "m_water_depth (m)", "platform_meta (1)", "precise_lat (degree_north)", "precise_lon (degree_east)", "precise_time (UTC)", "pressure (bar)", "rinkoii_temperature (Celsius)", "salinity (1)", "sci_flur_units (ppb)", "temperature (Celsius)", "u (m s-1)", "v (m s-1)")


new_names = c("time_UTC", "latitude", "longitude", "depth_m", "backscatter_m-1_sr-1", "cdom_ppb", "chlorophyll_ug_l", "conductivity_S_m-1", "density_kg_m-3", "dissolved_oxygen_%", "instrument_ctd_1", "m_depth_m", "m_water_depth_m", "platform_meta_1", "precise_lat", "precise_lon", "precise_time_utc", "pressure_bar", "rinkoii_temperature_C", "salinity_1", "sci_flur_units_ppb", "temperature_C", "u_m_s-1", "v_m_s-1")  
  
  setnames(dta, old_names, new_names)

  return(dta)
}

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
   
#  vdat_pth <- file.path(path.expand(vdat_pth), "vdat")
  vdat_pth <- file.path(normalizePath(vdat_pth), "vdat")

  #out_dir <- path.expand(out_dir)
  out_dir <- normalizePath(out_dir)
  
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
  set(trans, j = "transmitter_code_space", value = gsub("^([^-]*-[^-]*)-.*$", "\\1", trans$`Full ID`))
  setnames(trans, c("Serial Number", "ID", "Sensor Value", "Signal Strength (dB)", "Noise (dB)"), c("serial_no", "transmitter_id", "sensor_value_adc", "signal_level_db", "noise_level_db"))
  set(trans, j = "serial_no", value = as.character(trans$serial_no))
  return(trans)
}


#' @title adds geographic coordinates to detections
#' @description joins instrument data with detections
#' @param vrl compiled detections from multiple vrl files
#' @param hst_l gear deployment log
#' @examples
#' tar_load(hst)
#' tar_load(dtc)
#' vrl <- dtc
#' hst_l <- hst
#' stationary_recs_geo(vrl = vrl, hst_l = hst)

stationary_recs_geo <- function(vrl, hst_l){
  
  ## deep copy
  dtc <- data.table::as.data.table(vrl)
  hst <- data.table::as.data.table(hst_l)

  # add latitude and longitude
  dtc <- dtc[hst, ':='(lat_dd = latitude, lon_dd = longitude), on = .(Time >= timestamp_start_utc, Time <= timestamp_end_utc,  serial_no == instr_id)]

  # extract out code space
#  dtc[, transmitter_code_space := gsub("^([^-]*-[^-]*)-.*$", "\\1", `Full ID`)]
  dtc <- dtc[, c("datetime", "Model", "serial_no", "transmitter_id", "signal_level_db", "noise_level_db", "sensor_value_adc", "Sensor Unit", "frequency", "lat_dd", "lon_dd", "transmitter_code_space")]
  
  
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


#' @title get range test trial data
#' @description range test was conducted over multiple time periods and locations.  This document contains start and end dates and identifiers (start and end datetime are local)
#' @param pth
#' @examples
#' pth <- "data/range_trial/range_trial_dates.csv"
#' trial_parser(pth)

trial_parser <- function(pth = glider_trial){

  x <- fread(pth)
  
  x[ ,timestamp_end_utc := as.POSIXct(timestamp_end_utc)]
  x[ ,timestamp_start_utc := as.POSIXct(timestamp_start_utc)]
  
  x[is.na(timestamp_end_utc), timestamp_end_utc := Sys.time()]
  attributes(x$timestamp_end_utc)$tzone <- "UTC"
  attributes(x$timestamp_start_utc)$tzone <- "UTC"
  return(x)
}


#' @title cleans up and formats science and mission data files
#' @description removes 2nd line of header, sets time column to POSIX, and convert lat/lon to decimal degrees, adds file start and end timestamp columns
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
#' tar_load("sci_data")
#' pth = sci_data
#' tar_load("hst")
#' trial = hst
#' 
#' clean(pth = sci_data, lat = m_gps_lat, lon = m_gps_lon)


clean <- function(pth, lat = m_pth_lat, lon = m_gps_lon, trial = hst){
  pth_files <- list.files(pth, full.names = TRUE)
  out <- .clean(pth = pth_files, lat = lat, lon = lon)
  return(out)
}

.clean <- function(pth = pth_files, lat = m_gps_lat, lon = m_gps_lon){

  # bring in all files and drop the second row
  dta <- lapply(pth, function(x){data.table::fread(x, na.strings = "NaN")[-1]})
  dta <- data.table::rbindlist(dta, fill = TRUE, idcol = "file_num")
  
  # convert all columns from character to numeric
  dta[, names(dta) := lapply(.SD, as.numeric)]
  data.table::set(dta, j="time", 
                  value = as.POSIXct(dta$time, 
                                     origin = "1970-01-01 00:00:00", 
                                     tz = "UTC"))
  dta[, lat_dd := to_dd(m_gps_lat)]
  dta[, lon_dd := to_dd(m_gps_lon)]

  dta[, start_time := min(time), by = .(file_num)]
  dta[, end_time := max(time), by = .(file_num)]
  
  
  return(dta[])

}

#' @title combines both science and mission data into single object.
#' @description function combines mission and science data into single data.table.  Also removes any records without both latitude and longitude
#' @param x science data
#' @param y mission data
#' @examples
#' tar_load(clean_mission)
#' tar_load(clean_sci)
#' tar_load(trial_id)
#' x <- clean_mission
#' y <- clean_sci
#' id <- trial_id

combine <- function(x,y,id){  
  out <- rbind(x, y, fill = TRUE)
  out <- unique(out)
  setkey(out, time)

  out[id, ':=' (run_id = run_id, run = run), on = .(time >= timestamp_start_utc, time <= timestamp_end_utc)]
  #out[id, ':=' (run_id = run_id), on = .(time >= timestamp_start_utc, time <= timestamp_end_utc)]
  
  
  return(out)
}

#' @title creates interactive map of glider track
#' @description writes out html widget of glider track
#' @param glider_track combined data.table containing surface points of glider
#' @param pth output file path (character)
#' @examples
#' tar_load("glider_trk")
#' tar_load("vrl_vem_combined_dtc")
#' tar_load("vps")
#' tar_load("hst")
#' glider_track = glider_trk
#' dtc = vrl_vem_combined_dtc
#' log = hst
#' pth = "docs/index.html"
#' v_pth = vps
#'
#' leaflet_map(glider_track = glider_trk, pth = "output/test.html", recs = "data/receiver_coords.fst")

leaflet_map <- function(glider_track = glider_trk, 
                        dtc = vrl_vem_combined_dtc,
                        pth = "docx/index.html", log = hst){

  #v_pth <- "data/vps/synthetic.positions/all.csv"
#  vps <- data.table::fread(v_pth)
#  set(vps, j = "Time", value = fasttime::fastPOSIXct(vps$Time))
#  vps <- vps[FullId %in% c("A69-1604-32403"),]
#  color_pal <- colorNumeric(palette = "magma", domain = vps$HPEs, reverse = TRUE)

  # HB receivers
  recs_HB <- log[instr == "receiver" & mooring_type == "stationary" & run == 1]
  recs_HB <- unique(recs_HB, by = c("latitude", "longitude"))
  # SB receivers
  recs_SB <- log[instr == "receiver" & mooring_type == "stationary" & run == 2]
  recs_SB <- unique(recs_SB, by = c("latitude", "longitude"))

  # HB glider
  glider_HB <- glider_track[!(is.na(lon_dd) | is.na(lat_dd)) & run_id == 1]
  # SB glider
  glider_SB <- glider_track[!(is.na(lon_dd) | is.na(lat_dd)) & run_id == 2]

  # detections of MBU_1 tags on glider (180kHz)
  dtc_180 <- dtc[(receiver_site == "cormorant" | receiver_site == "mary_lou") & receiver_freq == 180 & (transmitter_site == "MBU-001" | transmitter_site == "MBU-002"),]
  dtc_180[transmitter_site == "MBU-001", color := "yellow"][transmitter_site == "MBU-002", color := "red"]
  
  dtc_69 <- dtc[(receiver_site == "cormorant" | receiver_site == "mary_lou") & receiver_freq == 69 & (transmitter_site == "MBU-001" | transmitter_site == "MBU-002"),]
  dtc_69[transmitter_site == "MBU-001", color := "yellow"][transmitter_site == "MBU-002", color := "red"]

  MBU1_recs <- log[instr == "receiver" & mooring_type == "stationary" & site == "MBU-001",][, ':=' (color = "yellow", radius = 16) ]
  MBU2_recs <- log[instr == "receiver" & mooring_type == "stationary" & site == "MBU-002",][, ':=' (color = "red", radius = 16)] 


  
  #MBU1_180_dtc_ct <- nrow(MBU1_180)
  
 #MBU1_180[, receiver_label := sprintf("ML depth (m): %.1f; tag-ML dist (m): %.0f; dtc date: %s", glider_m_depth, rt_distance_meters, format(datetime, "%Y-%m-%d %H:%M"))]
 #MBU1_180[, tag_label := sprintf("180kHz, water depth (m): %.1f; tag depth (m): %.1f; tot dtc count: %.0f", transmitter_water_depth, transmitter_instr_depth_from_top, MBU1_180_dtc_ct)]
  
  # detections of MBU_2 tags on glider (180kHz)
  ## MBU2_180 <- dtc[(receiver_site == "cormorant" | receiver_site == "mary_lou") & receiver_freq == 180 & transmitter_site == "MBU-002",]
  ## MBU2_180_dtc_ct <- nrow(MBU2_180)
  
  ## MBU2_180[, receiver_label := sprintf("ML depth (m): %.1f; tag-ML dist (m): %.0f; dtc date: %s", glider_m_depth, rt_distance_meters, format(datetime, "%Y-%m-%d %H:%M"))]
  ## MBU2_180[, tag_label := sprintf("180kHz, water depth (m): %.1f; tag depth (m): %.1f; tot dtc count: %.0f", transmitter_water_depth, transmitter_instr_depth_from_top, MBU2_180_dtc_ct)]

  # detections of MBU1 tags on glider (69kHz)
  ## MBU1_69 <- dtc[(receiver_site == "mary_lou" | receiver_site == "cormorant") & (transmitter_instr_model %in% c("V13-1x-H", "V13-1x-L")) & transmitter_site == "MBU-001" ,]
  ## MBU1_69_dtc_ct <- nrow(MBU1_69)

  ## MBU1_69[, receiver_label := sprintf("ML depth (m): %.1f; tag-ML dist (m): %.0f; dtc date: %s", glider_m_depth, rt_distance_meters, format(datetime, "%Y-%m-%d %H:%M"))]
  ## MBU1_69[, tag_label := sprintf("69kHz, water depth (m): %.1f; tag depth (m): %.1f; tot dtc count: %.0f", transmitter_water_depth, transmitter_instr_depth_from_top, MBU1_69_dtc_ct)]

  ## MBU2_69 <- dtc[(receiver_site == "mary_lou" | receiver_site == "cormorant") & (transmitter_instr_model %in% c("V13-1x-H", "V13-1x-L")) & receiver_freq == 69 & transmitter_site == "MBU-002",]
  ## MBU2_69_dtc_ct <- nrow(MBU2_69)
  
  ## MBU2_69[, receiver_label := sprintf("ML depth (m): %.1f; tag-ML dist (m): %.0f; dtc date: %s", glider_m_depth, rt_distance_meters, format(datetime, "%Y-%m-%d %H:%M"))]
  ## MBU2_69[, tag_label := sprintf("69kHz, water depth (m): %.1f; tag depth (m): %.1f; tot dtc count: %.0f", transmitter_water_depth, transmitter_instr_depth_from_top, MBU2_69_dtc_ct)]

  # self-detection of mary-lou and cormorant
  self_dtc_180 <- dtc[receiver_mooring_type == "mobile" & transmitter_mooring_type == "mobile" & receiver_freq == 180,]
  self_dtc_69 <- dtc[receiver_mooring_type == "mobile" & transmitter_mooring_type == "mobile" & receiver_freq == 69,]

  # now build leaflet map
  m <- leaflet()
  m <- setView(m, zoom = 15, lat = 45.537 , lng = -83.999)
  m <- addTiles(m)
  #m <- leafem::addGeoRaster(m, bath, opacity = 1, colorOptions = leafem::colorOptions(palette = viridis::viridis(256), breaks = brks_bath$brks), group = "bathy (ft)")
  m <- addTiles(m, urlTemplate = "http://tileservice.charts.noaa.gov/tiles/50000_1/{z}/{x}/{y}.png", group = "nav chart")
  m <- addProviderTiles(m, providers$Esri.WorldImagery, group = "satellite")
  m <- addProviderTiles(m, providers$Esri.NatGeoWorldMap, group = "alt")

  #vps
  ## m <- addPolylines(map = m, data = vps, lng = ~Longitude, lat = ~Latitude, color = "purple", group = "vps")
  ## m <- addCircleMarkers(m, data = vps, lng = ~Longitude, lat = ~Latitude, color = ~color_pal(HPEs), radius = 10, stroke = FALSE, fillOpacity = 1, group = "vps")
  
  #  m <- addMarkers(m, lng = -83.58845, lat = 44.08570, label = "release")
  ## m <- addCircleMarkers(m, data = glider_track, lng = ~lon_dd, lat = ~lat_dd, color = "green", radius = 5, stroke = FALSE, fillOpacity = 1)

  # receivers
  m <- addCircleMarkers(m, data = recs_HB, lng = ~longitude, lat = ~latitude, color = "blue", radius = 8, stroke = FALSE, fillOpacity = 1)
  m <- addCircleMarkers(m, data = recs_SB, lng = ~longitude, lat = ~latitude, color = "blue", radius = 8, stroke = FALSE, fillOpacity = 1)

  # glider track
  m <- addPolylines(map = m, data = glider_HB, lng = ~lon_dd, lat = ~lat_dd, color = "green")
  m <- addPolylines(map = m, data = glider_SB, lng = ~lon_dd, lat = ~lat_dd, color = "green")

  m <- addCircleMarkers(map = m, data = dtc_180, lng = ~receiver_longitude, lat = ~receiver_latitude, color = ~color, radius = 9, stroke = FALSE, fillOpacity = 1, group = "180 kHz")
  m <- addCircleMarkers(map = m, data = MBU1_recs, lng = ~longitude, lat = ~latitude, color = ~color, radius = ~radius, stroke = FALSE, fillOpacity = 1, group = "180 kHz")
  m <- addCircleMarkers(map = m, data = MBU2_recs, lng = ~longitude, lat = ~latitude, color = ~color, radius = ~radius, stroke = FALSE, fillOpacity = 1, group = "180 kHz")


  m <- addCircleMarkers(map = m, data = dtc_69, lng = ~receiver_longitude, lat = ~receiver_latitude, color = ~color, radius = 9, stroke = FALSE, fillOpacity = 1, group = "69 kHz")
  m <- addCircleMarkers(map = m, data = MBU1_recs, lng = ~longitude, lat = ~latitude, color = ~color, radius = ~radius, stroke = FALSE, fillOpacity = 1, group = "69 kHz", label = "MBU-001 transmitter")
  m <- addCircleMarkers(map = m, data = MBU2_recs, lng = ~longitude, lat = ~latitude, color = ~color, radius = ~radius, stroke = FALSE, fillOpacity = 1, group = "69 kHz", label = "MBU-002 transmitter")
  


  

  
  #add MBU-001-180 receiver and detections
##   m <- addCircleMarkers(map = m, data = MBU1_180, lng = ~receiver_longitude, lat = ~receiver_latitude, color = "yellow", radius = 9, stroke = FALSE, fillOpacity = 1, group = "Tag-180-MBU1", label = ~receiver_label)

## m <- addCircleMarkers(map = m, data = MBU1_180, lng = ~transmitter_longitude, lat = ~transmitter_latitude, color = "red", radius = 9, stroke = FALSE, fillOpacity = 1, group = "Tag-180-MBU1", label = ~tag_label)
##   #add MBU-002-180 receiver and detections
##   m <- addCircleMarkers(map = m, data = MBU2_180, lng = ~receiver_longitude, lat = ~receiver_latitude, color = "yellow", radius = 9, stroke = FALSE, fillOpacity = 1, group = "Tag-180-MBU2", label = ~receiver_label)
##   m <- addCircleMarkers(map = m, data = MBU2_180, lng = ~transmitter_longitude, lat = ~transmitter_latitude, color = "red", radius = 9, stroke = FALSE, fillOpacity = 1, group = "Tag-180-MBU2", label = ~tag_label)

##   # add MBU-001-69 detections
##     m <- addCircleMarkers(map = m, data = MBU1_69, lng = ~receiver_longitude, lat = ~receiver_latitude, color = "yellow", radius = 9, stroke = FALSE, fillOpacity = 1, group = "Tag-69-MBU1", label = ~receiver_label)
  
##   m <- addCircleMarkers(map = m, data = MBU1_69, lng = ~transmitter_longitude, lat = ~transmitter_latitude, color = "red", radius = 9, stroke = FALSE, fillOpacity = 1, group = "Tag-69-MBU1", label = ~tag_label)
##   #add MBU-002-180 receiver and detections
##   m <- addCircleMarkers(map = m, data = MBU2_69, lng = ~receiver_longitude, lat = ~receiver_latitude, color = "yellow", radius = 9, stroke = FALSE, fillOpacity = 1, group = "Tag-69-MBU2", label = ~receiver_label)
##   m <- addCircleMarkers(map = m, data = MBU2_69, lng = ~transmitter_longitude, lat = ~transmitter_latitude, color = "red", radius = 9, stroke = FALSE, fillOpacity = 1, group = "Tag-69-MBU2", label = ~tag_label)

  # self dtc-180
  m <- addCircleMarkers(map = m, data = self_dtc_180, lng = ~transmitter_longitude, lat = ~transmitter_latitude, col = "orange", radius = 9, stroke = FALSE, fillOpacity = 1, group = "self-dtc 180kHz")
  m <- addCircleMarkers(map = m, data = self_dtc_69, lng = ~transmitter_longitude, lat = ~transmitter_latitude, colo = "orange", radius = 9, stroke = FALSE, fillOpacity = 1, group = "self-dtc 69kHz")
  
  m <- leafem::addMouseCoordinates(m)
  m <- addMeasure(m, primaryLengthUnit = "meters", secondaryLengthUnit = "kilometers")  
  #  m <- addLayersControl(m, baseGroups = c("satellite", "nav chart", "alt"), overlayGroups = c("Tag-180-MBU1", "Tag-180-MBU2", "Tag-69-MBU1", "Tag-69-MBU2", "self-dtc,180", "self-dtc,69", "vps"),position = "bottomright", options = layersControlOptions(collapsed = FALSE))
  m <- addLayersControl(m, baseGroups = c("satellite", "nav chart", "alt"), overlayGroups = c("self-dtc 180kHz", "self-dtc 69kHz", "180 kHz", "69 kHz"),position = "bottomright", options = layersControlOptions(collapsed = FALSE))

  #m <- addLegend( map = m, pal = color_pal, values = ~HPEs, title = "HPE", opacity=1)

  
  htmlwidgets::saveWidget(m, pth)
  return(pth)
}


###########################
#' spatial events function
#' tar_load("glider_trk")
#' tar_load("hst")
#' # pull out stationary receivers (69 khz at MBU-001)
#'  hst_lat <- hst[site == "MBU-001"][[ c("latitude")]][[1]]
#'  hst_lon <- hst[site == "MBU-001"][["longitude"]][[1]]
#' spatial_events(track = glider_trk, rec_lat = hst_lat, rec_lon = hst_lon, dist_thresh = 1000)

spatial_events <- function(track = glider_trk, rec_lat = hst_lat, rec_lon = hst_lon, dist_thresh = 1000){ 

  trk <- data.table::copy(track)
  
  trk <- trk[!(is.na(lon_dd) | is.na(lat_dd)),]
  setkey(trk, time)

  trk[, GT_dist := geosphere::distGeo(c(rec_lon, rec_lat), cbind(lon_dd, lat_dd))]

  trk[, lag_dist := shift(GT_dist, type = "lag")]
  trk[, lead_dist := shift(GT_dist, type = "lead")]
  trk[, arrive := 0]
  trk[, depart := 0]
  trk[GT_dist < dist_thresh & lag_dist > dist_thresh, arrive := 1]
  trk[GT_dist < dist_thresh & lead_dist > dist_thresh, depart := 1]
  trk[GT_dist < dist_thresh & is.na(lag_dist), arrive := 1]
  trk[GT_dist < dist_thresh & is.na(lead_dist), depart := 1]
  trk[, event := cumsum(arrive), ]

  return(trk)
}


#' @examples
#' tar_load("instr_deploy_data")
#' hst_l <- instr_deploy_data
#' hst_clean(hst_l = hst)

hst_clean <- function(hst_l){

  hst_file <- hst_l[[1]]
  if(length(hst_l) > 1) stop("Can only load one hst file. Need to expand.")
  hst_l <- data.table::fread(hst_file)

  
  #set all missing timestamps to now for convenience
  hst_l[ , timestamp_end_utc := as.POSIXct(timestamp_end_utc)]
  hst_l[51:60, timestamp_start_utc := as.POSIXct(timestamp_start_utc)]
  
  hst_l[is.na(timestamp_end_utc), timestamp_end_utc := Sys.time()]
  attributes(hst_l$timestamp_end_utc)$tzone <- "UTC"
  attributes(hst_l$timestamp_start_utc)$tzone <- "UTC"

  # for development...
  #hst_l <- hst_l[run_id == 2,]

  return(hst_l)
}

#' @title utility function for formatting data reported out in receiver points table
#' @description formats columns and names for output in receiver points table created by rec_points_2021.rmd.  This function changes adds id, site, depth_ft, lat, lon and changes order of columns
#' @param y data table input from sentinal_depth and rec_depth targets
#'
#' @examples
#' tar_load("dtc_summary_clean")
#' y <- dtc_summary_clean
#' formatter(y)

formatter <- function(y){
  data.table::setDT(y)
  x <- data.table::copy(y)
  data.table::setorder(x, receiver_run, receiver_freq, transmitter_site)
#  set(x, j = "row_num", value = 1:nrow(x))
  new_names <- c("trial", "receiver model", "receiver freq", "tag model", "tag id", "receiver site", "tag site", "first dtc", "last dtc", "num dtc")
  setnames(x, names(x), new_names)
  x <- x[, c("trial", "receiver model", "tag model", "tag id", "receiver freq", "receiver site", "tag site", "first dtc", "last dtc", "num dtc")] 
  return(x)
}
  
##########################

#' @title utility function that creates basic table of coordinates using flextable
#' @description creates simple flextable for use within rmarkdown to output list of coordinates
#' @param out_tbl data.table of information to output in report

coords_table <- function(out_tbl){#, path = "output/juv_coords.html"){
  flex <- flextable::flextable(out_tbl)
  flex <- flextable::fontsize(flex, part = "all", size = 12)
  flex <- flextable::bold(flex, part = "header")
  flex <- flextable::theme_zebra(flex)
  flex <- flextable::autofit(flex)
  flex <- flextable::align(flex, align = "center", part = "all")
#  flex <- flextable::add_header_row(flex, values = "", colwidths = 6)
#  flex <- flextable::add_header_row(flex, values = paste("as of:", as.Date(Sys.time()), sep = " "),
#                                    colwidths = 6)
#  flex <- flextable::add_header_row(flex, values = "Juvenile cisco gear deployment", colwidths = 6)
#  flex <- flextable::fontsize(flex, i = 1, part = "header", size = 24)
#  flex <- flextable::bold(flex, part = "header", i = 1)
#  flex <- flextable::fontsize(flex, i = 2, part = "header", size = 12)
#  flex <- flextable::align(flex, align = "center", part = "header", i = 2)
  #save_as_html(flex, path = path, title = "Juv cisco") 
  #return(path)
  return(flex)
}


#' @title extract dtc summary info
#' @description create detection data summary table for report
#' @param tbl data.table of tag detection summary info
#' tar_load(vrl_vem_combined_dtc)
#' raw_data = vrl_vem_combined_dtc



.dtc_summary <- function(raw_data){

  foo <- raw_data[transmitter_mooring_type == "stationary" & receiver_mooring_type == "mobile", .(min = .SD[, min(datetime)], max = .SD[, max(datetime)], count = .N), by = .(receiver_run, receiver_instr_model, receiver_freq, transmitter_instr_model, transmitter_instr_id, receiver_site, transmitter_site), .SDcols = "datetime"]

  return(foo)

}



#########################################3  
#' @title impute missing detections
#' @param dtc detections for a tag
#' @param t_thresh threshold of time between successive detections that trigger imputation of tag transmissions
#' 
#' @examples
#' tar_load("vrl_vem_combined_dtc")
#' dtc <- vrl_vem_combined_dtc
#'# for Devi
#' 
#' test <- dtc[receiver_run == 2 & receiver_freq == 69 & (transmitter_instr_id == "A69-1604-32404"), c("datetime", "transmitter_instr_model", "transmitter_freq", "transmitter_instr_id")]
#' dtc <- test
#' t_thresh = 240

#' impute_dtc(dtc = test, t_thresh = 240)

impute_dtc <- function(dtc, t_thresh = 240){
  dtcc <- data.table::copy(dtc)
  setkey(dtcc, datetime)
  input_names <- names(dtc)
    dtcc[, lag_time := data.table::shift(datetime, type = "lead")]
  dtcc[, time_diff := as.numeric(lag_time - datetime, units = "secs")]
  dtcc[, num_missing := 0]
  dtcc[time_diff > t_thresh, num_missing := time_diff%/%t_thresh]
  dtcc <- dtcc[rep(dtcc[, .I], num_missing+1),]

  dtcc[, imputed := as.numeric(duplicated(dtcc, by = "datetime"))]
  dtcc[imputed == 1, datetime := NA]
  dtcc[, datetime := as.POSIXct(na_interpolation(as.numeric(dtcc$datetime)), origin = "1970-01-01 00:00:00", tz = "UTC")]
  out <- c(input_names, "imputed")
  bar <- dtcc[, ..out]
  return(bar)
}


#' @title interpolate time series
#' @description Interpolate NAs in time series vector.  Function borrowed from "imputeTS::na_interpolation" package
#' @param x numeric vector or time series objetct, NAs will be replaced
#' @param option interpolation algorithm to be used.  Can be "linear", "spline", "stine"
#' @param maxgap max number of successive NAs that will be imputed.  Default is to replace all NAs.  If maxgap is provided, then NAs will be left on values greater than maxgap.
#' @details missing values (NAs) will get replaced by values from approx, spline, or stinterp interpolation


na_interpolation <- function (x, option = "linear", maxgap = Inf, ...) 
{
    data <- x
    if (!is.null(dim(data)[2]) && dim(data)[2] > 1) {
        for (i in 1:dim(data)[2]) {
            if (!anyNA(data[, i])) {
                next
            }
            tryCatch(data[, i] <- na_interpolation(data[, i], 
                option, maxgap), error = function(cond) {
                warning(paste("imputeTS: No imputation performed for column", 
                  i, "because of this", cond), call. = FALSE)
            })
        }
        return(data)
    }
    else {
        missindx <- is.na(data)
        if (!anyNA(data)) {
            return(data)
        }
        if (any(class(data) == "tbl")) {
            data <- as.vector(as.data.frame(data)[, 1])
        }
        if (sum(!missindx) < 2) {
            stop("Input data needs at least 2 non-NA data point for applying na_interpolation")
        }
        if (!is.null(dim(data)[2]) && !dim(data)[2] == 1) {
            stop("Wrong input type for parameter x")
        }
        if (!is.null(dim(data)[2])) {
            data <- data[, 1]
        }
        if (!is.numeric(data)) {
            stop("Input x is not numeric")
        }
        n <- length(data)
        allindx <- 1:n
        indx <- allindx[!missindx]
        data_vec <- as.vector(data)
        if (option == "linear") {
            interp <- stats::approx(indx, data_vec[indx], 1:n, 
                rule = 2, ...)$y
        }
        else if (option == "spline") {
            interp <- stats::spline(indx, data_vec[indx], n = n, 
                ...)$y
        }
        else if (option == "stine") {
            interp <- stinepack::stinterp(indx, data_vec[indx], 
                1:n, ...)$y
            if (any(is.na(interp))) {
                interp <- na_locf(interp, na_remaining = "rev")
            }
        }
        else {
            stop("Wrong parameter 'option' given. Value must be either 'linear', 'spline' or 'stine'.")
        }
        data[missindx] <- interp[missindx]
        if (is.finite(maxgap) && maxgap >= 0) {
            rlencoding <- rle(is.na(x))
            rlencoding$values[rlencoding$lengths <= maxgap] <- FALSE
            en <- inverse.rle(rlencoding)
            data[en == TRUE] <- NA
        }
        if (!is.null(dim(x)[2])) {
            x[, 1] <- data
            return(x)
        }
        return(data)
    }
}


#' @title impute tag transmissions from colocated tag and receiver
#' @param dtc combined detection/vem dataset
#' @param hst cleaned gear deployment info
#' @param mooring_type either "stationary" or "mobile".  
#' 
#' @examples
#' tar_load("vrl_vem_combined_dtc")
#' tar_load("hst")
#'  dtc <- vrl_vem_combined_dtc
#' mrng_type = "mobile"
#' impute_missing_transmissions(dtc, hst, mrng_type = "stationary")

impute_missing_transmissions <- function(dtc, hst, mrng_type = "stationary"){

  # find co-located receiver for each stationary tag
  tags <- hst[instr == "tag" & mooring_type == mrng_type]
  recs <- hst[instr == "receiver" & mooring_type == mrng_type]
  tags[recs, `:=` (colocated_rec = i.instr_id), on = .(run_id, run, site, freq)] 

  #
  beeps <- dtc[tags, .(datetime, trial = receiver_run, transmitter_instr_id, transmitter_instr_model, transmitter_freq, transmitter_longitude, transmitter_latitude, receiver_site, receiver_instr_model, colocated_rec, tag_min_delay, tag_max_delay), on = .(receiver_run_id = run_id, receiver_serial_no = colocated_rec, transmitter_instr_id = instr_id), nomatch = NULL]
  beeps[, thresh := tag_min_delay + tag_max_delay]

  setkey(beeps, transmitter_instr_id, datetime)
  tags <-  unique(beeps$transmitter_instr_id)

  #x = "A180-1702-61650"
  #y = beeps
  #imputed_trams(x, y = beeps)
    
  imputed_trans <- function(x, y = beeps){
    beeps.i <- y[transmitter_instr_id == x,]
    out <- impute_dtc(dtc = beeps.i, t_thresh = beeps.i$thresh[1])
    return(out)
  }
  
  out <- lapply(tags, imputed_trans, y = beeps)
  out <- rbindlist(out)
  return(out)
}


#' @title identify periods of time when data were collected as indicated by reporting of self-detection transmissions of glider tag on glider receiver.
#' @description Although glider was actively listening during the entire mission, real-time data was not sent back for entire time period.  This function identifies when data is present by evaluating when tag on glider was detected under the assumption that detection probability on the glider was very close to 1.  Distance between tag and receiver on glider was approximately 0.5 m.
#'
#' @examples
#' tar_load("vrl_vem_combined_dtc")
#' dtc <- vrl_vem_combined_dtc
#' tar_load("hst")
#' .data_present(dtc = dtc, hst = hst, thresh_sec = 86400)


.data_present <- function(dtc, hst, thresh_sec = 86400){
  # find co-located receiver for mobile tag
  
  beeps1 <- dtc[receiver_mooring_type == "mobile" & transmitter_mooring_type == "mobile" & transmitter_instr_model != "V5D-1x"]
  setkey(beeps1, receiver_run, datetime)
  

  beeps1[, `:=`(arrive_diff = data.table::shift(datetime, fill = NA, type = "lag"))]
  beeps1[, `:=`(depart_diff = data.table::shift(datetime, fill = NA, type = "lead"))]

  beeps1[, `:=`(arrive_diff = as.numeric(datetime - arrive_diff), depart_diff = as.numeric(depart_diff - datetime))]
  set(beeps1, j = "arrive", value = 0)
  set(beeps1, j = "depart", value = 0)
  beeps1[is.na(arrive_diff) | arrive_diff > thresh_sec, arrive := 1]
  beeps1[is.na(depart_diff) | depart_diff > thresh_sec, depart := 1]
  beeps1[, event := cumsum(arrive)]
  out <- beeps1[, .(start = min(datetime), end = max(datetime)), by = .(event)]

  return(out)
}



#' @title calculates distance between glider and tag for all transmissions, identifies detections of transmissions
#' @examples
#' tar_load("vrl_vem_combined_dtc")
#' dtc <- vrl_vem_combined_dtc
#' tar_load("imputed_transmissions")
#' tag_beeps = imputed_transmissions
#' tar_load(glider_trk)
#' glider_geo = glider_trk
#' receiver_site = c("cormorant", "mary_lou")
#' 

glider_dtc <- function(dtc, receiver_site = c("cormorant", "mary_lou"), tag_beeps, glider_geo){ 

  glider_dtc <- dtc[receiver_mooring_type == "mobile" & receiver_site %in% receiver_site, c("datetime", "transmitter_instr_id", "receiver_site", "glider_lat_dd", "glider_lon_dd")]

  set(tag_beeps, j = "tran_dtc", value =  0)
  tag_beeps[glider_dtc, ':=' (tran_dtc = 1, glider_lat = glider_lat_dd, glider_lon = glider_lon_dd, glider_dtc_time = datetime), on = .(transmitter_instr_id = transmitter_instr_id, datetime = datetime), roll = "nearest", rollends = c(FALSE, FALSE)]

  tag_beeps[, `:=`(glider_lon = approx(x = glider_geo$time[!is.na(glider_geo$lon_dd)],
                                       y = glider_geo$lon_dd[!is.na(glider_geo$lon_dd)],
                                       xout = datetime,
                                       ties = "ordered")$y,
                   glider_lat = approx(x = glider_geo$time[!is.na(glider_geo$lat_dd)],
                                       y = glider_geo$lat_dd[!is.na(glider_geo$lon_dd)],
                                       xout = datetime,
                                       ties = "ordered")$y)]
  
  tag_beeps[, rt_distance_m := geosphere::distVincentyEllipsoid(p1 = cbind(glider_lon, glider_lat), p2 = cbind(transmitter_longitude, transmitter_latitude))]

  return(tag_beeps)
}


#' @title filter tag transmissions for periods of time when we have real-time data
#' @examples
#' tar_load("glider_dtc_transmissions")
#' dtc = glider_dtc_transmissions
#' tar_load(data_present)
#' inter = data_present

glider_dtc_transmissions_time_filtered <- function(dtc, inter){
  foo <- inter[dtc, on = .(start <= datetime, end >= datetime)]
  dtc[inter, `:=`(event = event), on = .(datetime >= start, datetime <= end)]
  dtc <- dtc[!is.na(event)]
  return(dtc)
}

#' @title fit varying coefficient (on transmitter frequency) to stationary tag-glider detection data
#' @examples
#' tar_load("glider_dtc_range")
#' dtc = glider_dtc_range
#' limit_dist_m = 3000
#' trial_run = 2
#' .GAMit(dtc = dtc, trial_run = 1, limit_dist_m = limit_dist_m)

.GAMit <- function(dtc = glider_dtc_range, trial_run = 1, limit_dist_m = 3000){

  dtc.i <- dtc[transmitter_instr_id %in% c("A69-1604-32401", "A69-1604-32402", "A69-1604-32405", "A69-1604-32406", "A180-1702-61650", "A180-1702-61651") & trial == trial_run & rt_distance_m <= limit_dist_m]

  mod <- gam(tran_dtc ~ s(rt_distance_m, bs = "cs", by = as.factor(transmitter_instr_model)), data = dtc.i, family = "binomial")

  return(mod)
}


#' @title fit varying coefficient (on transmitter frequency) to stationary tag-glider detection data
#' @examples
#' tar_load("glider_dtc_range")
#' dtc = glider_dtc_range
#' limit_dist_m = 3000
#' trial_run = 2
#' .GAMit_tensor(dtc = dtc, trial_run = 1, limit_dist_m = limit_dist_m)

.GAMit_tensor <- function(dtc = glider_dtc_range, trial_run = 1, limit_dist_m = 3000){

  dtc.i <- dtc[transmitter_instr_id %in% c("A69-1604-32401", "A69-1604-32402", "A69-1604-32405", "A69-1604-32406", "A180-1702-61650", "A180-1702-61651") & trial == trial_run & rt_distance_m <= limit_dist_m]

mod <- gam(tran_dtc ~ te(as.numeric(datetime), rt_distance_m, by = as.factor(transmitter_instr_model)), data = dtc.i, family = "binomial")

  return(mod)
}








#' tar_load("GAMit_tensor_HB")
#' mod <- GAMit_tensor_HB
#' trial_run = 1
#'
#' tar_load("GAMit_tensor_SB")
#' mod <- GAMit_tensor_SB
#' trial_run = 2
#'
#' 
#' tar_load("glider_dtc_range")
#' dtc <- glider_dtc_range
#' 
#' limit_dist_m = 2500
#' tar_load(data_bounds)

# plots detection probability as a function of distance at 10 selected times

.dtc_prob_dist <- function(mod = GAMit_tensor, dtc = glider_dtc_range, out_pth = "output/dtc_prob_rt_dist.pdf", limit_dist_m = 2500, trial_run = 1, bounds=data_bounds) {

  if(trial_run == 1) {
    title = "detection probability by distance for 10 time periods at Hammond Bay"
  } else {
    title = "detection probability by distance for 10 time periods at Saginaw Bay"
  }

  # subset out stationary transmitters for detection by glider

  dtc.i <- dtc[transmitter_instr_id %in% c("A69-1604-32401", "A69-1604-32402", "A69-1604-32405", "A69-1604-32406", "A180-1702-61650", "A180-1702-61651") & trial == trial_run & rt_distance_m <= limit_dist_m]
  
  rt_distance_m = seq(min(dtc.i$rt_distance_m, na.rm = TRUE), limit_dist_m, length.out = 500)
  pred_time <- as.numeric(seq(min(dtc.i$datetime), max(dtc.i$datetime), length.out = 10))
  
  predicted_data <- CJ(transmitter_instr_model = unique(dtc.i$transmitter_instr_model), rt_distance_m = rt_distance_m, datetime = pred_time)
  predicted_data[bounds, event := event, on = .(datetime >= start, datetime <= end)]
  predicted_data <- predicted_data[!is.na(event),]

  ilink <- family(mod)$linkinv
  fit <-  predict(mod, newdata = predicted_data, se.fit = TRUE, type = "link")
  
  predicted_data[, `:=`(fit_link = fit$fit, se_link = fit$se.fit)] 
  predicted_data[, `:=`(fit_resp = ilink(fit_link), fit_upr = ilink(fit_link + (2 * se_link)), fit_lwr = ilink(fit_link - (2 * se_link)))]
  predicted_data[, `:=`(datetime = as.POSIXct(datetime, origin = '1970-01-01 00:00:00', tz = "UTC"))]

  setkey(predicted_data, transmitter_instr_model, datetime)
  predicted_data[, datetime_f := as.factor(datetime)]

  
  pl <- ggplot(predicted_data, aes(x = rt_distance_m,y = fit_resp, group = datetime_f)) +
    geom_line(aes(color = datetime_f), size = 2) +
    facet_wrap(vars(transmitter_instr_model)) +
    scale_color_viridis_d()

  pdf(out_pth)
  print(pl)
  dev.off()

  return(out_pth)
}

  
## # function below plots detection range as a function of time at fixed distance


.dtc_prob_time <- function(mod = GAMit_tensor, dtc = glider_dtc_range, out_pth = "output/predicted_dtc_prob.pdf", limit_dist_m = 2500, trial_run = 1, bounds = data_bounds) {

  dtc.i <- dtc[transmitter_instr_id %in% c("A69-1604-32401", "A69-1604-32402", "A69-1604-32405", "A69-1604-32406", "A180-1702-61650", "A180-1702-61651") & trial == trial_run & rt_distance_m <= limit_dist_m]

  dtc.i[tran_dtc == 0, obs_col := "black"][tran_dtc == 1, obs_col := "red"]
  rt_distance_m = seq(min(dtc.i$rt_distance_m, na.rm = TRUE), limit_dist_m, length.out = 500)
  pred_time <- as.numeric(seq(min(dtc.i$datetime), max(dtc.i$datetime), length.out = 500))
  fix_dist <- 1500
  
  predicted_data <- CJ(transmitter_instr_model = unique(dtc.i$transmitter_instr_model), rt_distance_m = fix_dist, datetime = pred_time)
  predicted_data[bounds, event := event, on = .(datetime >= start, datetime <= end)]
  predicted_data <- predicted_data[!is.na(event),]


  ilink <- family(mod)$linkinv
  fit <-  predict(mod, newdata = predicted_data, se.fit = TRUE, type = "link")
  
  predicted_data[, `:=`(fit_link = fit$fit, se_link = fit$se.fit)] 
  predicted_data[, `:=`(fit_resp = ilink(fit_link), fit_upr = ilink(fit_link + (2 * se_link)), fit_lwr = ilink(fit_link - (2 * se_link)))]
  predicted_data[, `:=`(datetime = as.POSIXct(datetime, origin = '1970-01-01 00:00:00', tz = "UTC"))]

  setkey(predicted_data, transmitter_instr_model, datetime)

  
  pdf(out_pth)
  par(mfrow = c(2,1), oma = c(0,0,2,0), mar = c(4,4,0,0))

  # plot 69 kHz
  plot(fit_resp ~ datetime, data = predicted_data[transmitter_instr_model %in% c("V13-1x-H")], type = "l", las = 1, ylim = c(0,1), ylab = "dtc prob (+-2SE)", col = "blue")
  lines(fit_resp ~ datetime, data = predicted_data[transmitter_instr_model %in% c("V13-1x-L")], col = "red")
                                                                                                    
  # V13-H error
  polygon(c(predicted_data[transmitter_instr_model %in% c("V13-1x-H")][["datetime"]], rev(predicted_data[transmitter_instr_model %in% c("V13-1x-H")][["datetime"]])),
        c(predicted_data[transmitter_instr_model %in% c("V13-1x-H")][["fit_upr"]], rev(predicted_data[transmitter_instr_model %in% c("V13-1x-H")][["fit_lwr"]])), col = scales::alpha("blue", 0.1), border = "blue", lwd = 1.5 )

  #V13-L error
  polygon(c(predicted_data[transmitter_instr_model %in% c("V13-1x-L")][["datetime"]], rev(predicted_data[transmitter_instr_model %in% c("V13-1x-L")][["datetime"]])),
        c(predicted_data[transmitter_instr_model %in% c("V13-1x-L")][["fit_upr"]], rev(predicted_data[transmitter_instr_model %in% c("V13-1x-L")][["fit_lwr"]])), col = scales::alpha("red", 0.1), border = "red", lwd = 1.5 )


 legend("topright", lty = c(1,1), col = c("blue", "red"), legend = c("V13-1x-H", "V13-1x-L"))


  plot(fit_resp ~ datetime, data = predicted_data[transmitter_instr_model %in% c("V9-2x-180K")], col = "black", type = "l", las = 1, ylim = c(0,1), ylab = "dtc_prob (+-2SE)")
 
  #180kHz error
  polygon(c(predicted_data[transmitter_instr_model %in% c("V9-2x-180K")][["datetime"]], rev(predicted_data[transmitter_instr_model %in% c("V9-2x-180K")][["datetime"]])),
        c(predicted_data[transmitter_instr_model %in% c("V9-2x-180K")][["fit_upr"]], rev(predicted_data[transmitter_instr_model %in% c("V9-2x-180K")][["fit_lwr"]])), col = scales::alpha("black", 0.1), border = "black", lwd = 1.5 )


  legend("topright", lty = c(1,1), col = c("black"), legend = c("V9-2x-180K"))

 # mtext(..., side = 3, outer = TRUE, line = 0)

  dev.off()

  return(out_pth)
  
}

  
  

#' @title plot of predicted and observed over distance
#' @examples
#' tar_load("GAMit_SB")
#' mod  = GAMit_SB
#' tar_load("glider_dtc_range")
#' dtc <- glider_dtc_range

#' limit_dist_m = 3000
 
.mod_output <- function(mod = GAMit, dtc = glider_dtc_range, out_pth = "output/predicted_dtc_prob.pdf", limit_dist_m = 2500, trial_run = 1, ...){

  dtc.i <- dtc[transmitter_instr_id %in% c("A69-1604-32401", "A69-1604-32402", "A69-1604-32405", "A69-1604-32406", "A180-1702-61650", "A180-1702-61651") & trial == trial_run & rt_distance_m <= limit_dist_m]

  dtc.i[tran_dtc == 0, obs_col := "black"][tran_dtc == 1, obs_col := "red"]
  rt_distance_m = seq(min(dtc.i$rt_distance_m, na.rm = TRUE), limit_dist_m, length.out = 500)
  
  predicted_data <- CJ(transmitter_instr_model = unique(dtc.i$transmitter_instr_model), rt_distance_m = rt_distance_m)

  ilink <- family(mod)$linkinv
  fit <-  predict(mod, newdata = predicted_data, se.fit = TRUE, type = "link")

  predicted_data[, `:=`(fit_link = fit$fit, se_link = fit$se.fit)] 
  predicted_data[, `:=`(fit_resp = ilink(fit_link), fit_upr = ilink(fit_link + (2 * se_link)), fit_lwr = ilink(fit_link - (2 * se_link)))]

  setkey(predicted_data, transmitter_instr_model, rt_distance_m)

  pdf(out_pth)
  par(mfrow = c(2,1), oma = c(0,0,2,0), mar = c(4,4,0,0))

  # plot 69 kHz
  plot(fit_resp ~ rt_distance_m, data = predicted_data[transmitter_instr_model %in% c("V13-1x-H")], type = "l", las = 1, ylim = c(0,1), ylab = "dtc prob (+-2SE)", xlim = c(0, 2500), col = "blue")
  lines(fit_resp ~ rt_distance_m, data = predicted_data[transmitter_instr_model %in% c("V13-1x-L")], col = "red")

  # V13-H error
  polygon(c(predicted_data[transmitter_instr_model %in% c("V13-1x-H")][["rt_distance_m"]], rev(predicted_data[transmitter_instr_model %in% c("V13-1x-H")][["rt_distance_m"]])),
        c(predicted_data[transmitter_instr_model %in% c("V13-1x-H")][["fit_upr"]], rev(predicted_data[transmitter_instr_model %in% c("V13-1x-H")][["fit_lwr"]])), col = scales::alpha("blue", 0.1), border = "blue", lwd = 1.5 )

  #V13-L error
  polygon(c(predicted_data[transmitter_instr_model %in% c("V13-1x-L")][["rt_distance_m"]], rev(predicted_data[transmitter_instr_model %in% c("V13-1x-L")][["rt_distance_m"]])),
        c(predicted_data[transmitter_instr_model %in% c("V13-1x-L")][["fit_upr"]], rev(predicted_data[transmitter_instr_model %in% c("V13-1x-L")][["fit_lwr"]])), col = scales::alpha("blue", 0.1), border = "red", lwd = 1.5 )

  legend("topright", lty = c(1,1), col = c("blue", "red"), legend = c("V13-1x-H", "V13-1x-L"))

  # plot 180 kHz
  plot(fit_resp ~ rt_distance_m, data = predicted_data[transmitter_instr_model %in% c("V9-2x-180K")], type = "l", las = 1, ylim = c(0,1), ylab = "dtc prob (+-2SE)", xlab = "glider-tag distance", xlim = c(0, 2500), col = "blue")
  
  polygon(c(predicted_data[transmitter_instr_model %in% c("V9-2x-180K")][["rt_distance_m"]], rev(predicted_data[transmitter_instr_model %in% c("V9-2x-180K")][["rt_distance_m"]])),
        c(predicted_data[transmitter_instr_model %in% c("V9-2x-180K")][["fit_upr"]], rev(predicted_data[transmitter_instr_model %in% c("V9-2x-180K")][["fit_lwr"]])), col = scales::alpha("blue", 0.1), border = "blue", lwd = 1.5 )


    rug(dtc.i[transmitter_instr_model %in% c("V9-2x-180K") & tran_dtc == 0,][["rt_distance_m"]], side = 1, col = dtc.i[transmitter_instr_model %in% c("V9-2x-180K") & tran_dtc == 0,][["obs_col"]], ticksize = 0.02, lwd = 0.5)
  rug(dtc.i[transmitter_instr_model %in% c("V9-2x-180K") & tran_dtc == 1,][["rt_distance_m"]], side = 1, col = dtc.i[transmitter_instr_model %in% c("V9-2x-180K") & tran_dtc ==1,][["obs_col"]], ticksize = -0.02, lwd = 0.5)

  legend("topright", lty = c(1,1), col = c("blue"), legend = c("V9-2x-180K"))

  mtext(..., side = 3, outer = TRUE, line = 0)
  
  dev.off()
  return(out_pth)
  
}
  

######################
#' tar_load(glider_trk)
#' glider_geo <- glider_trk  
#' tar_load(hst)
#' gear_log = hst
#' tar_load(vrl_vem_combined_dtc)
#' dta = vrl_vem_combined_dtc
#' bsize = 3600
#' tar_load(data_bounds)
#' bounds = data_bounds


.discrete_dtc_prob <- function(gear_log = hst, dta = vrl_vem_combined, bsize = 3600, glider_geo, bounds ){

  # copy hst
  hst1 <- copy(gear_log)
  set(hst1, j = "freq", value = as.character(hst1$freq))

  # copy dtc
  dtc <- copy(dta)

  # range(vrl_vem_combined_dtc$datetime)
  tseq <- seq(from = lubridate::floor_date(min(dta$datetime), unit = "day"), to = lubridate::ceiling_date(max(dta$datetime), unit = "day"), by = bsize)

  # bin detections
  dtc[, tbin := tseq[findInterval(datetime, tseq)]]

  # extract all tag, receiver, and trial ids
  tags <- hst1[ instr == "tag", "instr_id"]
  recs <- hst1[instr == "receiver", "instr_id"]
  trial = unique(hst1$run)

  # create all combinations of tag, receiver, and trials. Not all of these combinations are possible but will sort  out later.
  tag_rec_comb <- CJ(transmitter_instr_id = unique(tags$instr_id), receiver_serial_no = unique(recs$instr_id), receiver_run = unique(trial), tbin = tseq, tran_dtc = 0, unique = TRUE)

  # calculate locations for all tags and receivers in full combination dataset
  # how this is done depends on whether tag/receiver is moving or not...
  # do static first
  # recs separately from transmitters
  stat_deps_recs <- hst1[mooring_type == "stationary",]
  tag_rec_comb[stat_deps_recs, `:=` (receiver_mooring = mooring_type, receiver_latitude = latitude, receiver_longitude = longitude), on = .(receiver_serial_no = instr_id, tbin >= timestamp_start_utc, tbin <= timestamp_end_utc)]

  # now transmitters
  tag_rec_comb[stat_deps_recs, `:=` (transmitter_mooring = mooring_type, transmitter_latitude = latitude, transmitter_longitude = longitude), on = .(transmitter_instr_id = instr_id, tbin >= timestamp_start_utc, tbin <= timestamp_end_utc)]

 # add mobile info
  stat_deps_recs <- hst1[mooring_type == "mobile",]
  tag_rec_comb[stat_deps_recs, receiver_mooring := mooring_type, on = .(receiver_serial_no = instr_id, tbin >= timestamp_start_utc, tbin <= timestamp_end_utc)]

  tag_rec_comb[stat_deps_recs, transmitter_mooring := mooring_type, on = .(transmitter_instr_id = instr_id, tbin >= timestamp_start_utc, tbin <= timestamp_end_utc)]

  
  # calculate average position during time bin for mobile transmitters and receivers
  tag_rec_comb[receiver_mooring == "mobile" | transmitter_mooring == "mobile", `:=`(glider_lon_recs = approx(x = glider_geo$time[!is.na(glider_geo$lon_dd)],
                                      y = glider_geo$lon_dd[!is.na(glider_geo$lon_dd)],
                                      xout = tbin,
                                      ties = "ordered")$y,
                  glider_lat_recs = approx(x = glider_geo$time[!is.na(glider_geo$lat_dd)],
                                      y = glider_geo$lat_dd[!is.na(glider_geo$lon_dd)],
                                      xout = tbin,
                                      ties = "ordered")$y)]
  
  tag_rec_comb[is.na(receiver_latitude) & receiver_mooring == "mobile", `:=` (receiver_latitude = glider_lat_recs, receiver_longitude = glider_lon_recs),]

  tag_rec_comb[is.na(transmitter_latitude) & transmitter_mooring == "mobile",  `:=` (transmitter_latitude = glider_lat_recs, transmitter_longitude = glider_lon_recs),]

  # remove invalid combinations
  tag_rec_comb <- tag_rec_comb[!is.na(receiver_latitude) & !is.na(receiver_longitude) & !is.na(transmitter_longitude) & !is.na(transmitter_latitude),]

  # remove glider lat/lon- unneeded
  tag_rec_comb[, glider_lon_recs := NULL][, glider_lat_recs := NULL]


  dtc_obs <- dtc[, .(num_dtc = .N ), by = .(receiver_run, transmitter_instr_id, tbin, receiver_serial_no, receiver_frequency )]

  # join with all tag-receiver combinations
  combined <- dtc_obs[tag_rec_comb, on = .(receiver_run, transmitter_instr_id, tbin, receiver_serial_no) ]

  tag_rec_comb[dtc_obs, tran_dtc := num_dtc, on = .(transmitter_instr_id, tbin, receiver_serial_no) ]

  setkey(tag_rec_comb, transmitter_instr_id, receiver_serial_no, tbin)

 
  # add transmitter delay info
  tag_rec_comb[hst1, tag_min_delay := tag_min_delay, on = .(transmitter_instr_id = instr_id)]
  tag_rec_comb[hst1, tag_max_delay := tag_max_delay, on = .(transmitter_instr_id = instr_id)]

  # add transmitter frequency
  tag_rec_comb[hst1, receiver_frequency := freq, on = .(transmitter_instr_id = instr_id)]

  # calculate nominal delay
  tag_rec_comb[, nom_delay := ((tag_max_delay - tag_min_delay)/2) + tag_min_delay]

  # calculate expected transmissions
  tag_rec_comb[, exp_tran := bsize/nom_delay]

  # calculate detection prob
  tag_rec_comb[, dtc_prob := tran_dtc/exp_tran]

  # calculate tag-receiver distance
  tag_rec_comb[, rt_distance_m := geosphere::distVincentyEllipsoid(p1 = cbind(receiver_longitude, receiver_latitude), p2 = cbind(transmitter_longitude, transmitter_latitude))]

  tag_rec_comb[bounds, event := event, on = .(tbin >= start, tbin <= end)]
  tag_rec_comb <- tag_rec_comb[!is.na(event),]

  setkey(tag_rec_comb, receiver_run, transmitter_instr_id, tbin)

  tag_rec_comb[, num_success := tran_dtc][is.na(num_success), num_success := 0]
  tag_rec_comb[num_success > exp_tran, exp_tran := num_success]
  tag_rec_comb[, num_failure := exp_tran - num_success]
 
  return(tag_rec_comb)
}


#' @title discrete detection probability plot for stationary tags to glider (69kHz). These are hard-coded!
#' tar_load(discrete_dtc_prob)
#' dtc = discrete_dtc_prob
#' trial = 2 # saginaw Bay
#' trans = c("A180-1702-61652")
#' rec = c("300813", "300815")
#' out_pth = "output/discrete_SB.pdf"


.discrete_gam <- function(dtc = discrete_dtc_prob, trial, trans = c("A180-1702-61650"), rec = 458000){
  foo <- dtc[transmitter_instr_id %in% trans & receiver_serial_no %in% rec & receiver_run %in% trial,]

  mod <- gam(cbind(foo$num_success, foo$num_failure) ~ te(as.numeric(tbin), rt_distance_m), data = foo, family = "binomial", control = )

  return(mod)
}

  
#' @title creates predicted detection range curves for specified tag and receiver combination.  uses model predictions from GAM.
#' @param dtc discrete detection data
#' @param mod gam model output
#' @param trial trial (either 1 or two, corresponding to Hammond Bay (1) or Saginaw Bay (2) glider trial
#' @param out_pth output figure path
#' @param trans Transmitter that will be used in prediction. This must match model inputs
#' @param rec receiver that will be used in prediction.  This must match model inputs
#' @param limit_dist_m maximum distance plotted on x-axis
#' @param bounds defines periods of time in which we have data available
#'
#' @examples
#' tar_load(discrete_dtc_prob)
#' dtc <- discrete_dtc_prob
#' tar_load(discrete_gam_SB)
#' mod <- discrete_gam_SB
#' trial = 2
#' out_pth = "output/tst.png"
#' trans = "A180-1702-61650"
#' rec = "458000"
#' limit_dist_m = 1000
#' tar_load(data_bounds)
#' bounds = data_bounds



.discrete_rng_crv <- function(dtc = discrete_dtc_prob, mod = mod, trial, out_pth, trans = "A180-1702-61650", rec = 458000, limit_dist_m, bounds){

  foo <- dtc[transmitter_instr_id %in% trans & receiver_serial_no %in% as.character(rec) & receiver_run %in% trial,]

  setkey(foo, receiver_run, rt_distance_m)

#  plot(dtc_prob ~ rt_distance_m, data = foo, type = "p", xlim = c(0,500), ylim = c(0,1), pch = 16, las = 1)
 
  rt_distance_m = seq(min(foo$rt_distance_m, na.rm = TRUE), limit_dist_m, length.out = 500)

  # next line chooses a specified number of equally spaced time bins to predict detection range curves for that time
  #pred_time <- as.numeric(seq(min(foo$tbin), max(foo$tbin), length.out = 200)
  #pred_time <- sample(unique(foo$tbin), length(unique(foo$tbin)) , replace = FALSE)

  #pred_time <- sample(unique(foo$tbin), 10, replace = FALSE)
  
  # to predict for all time bins...This takes forever and does a ton of overplotting!
   pred_time <- unique(foo$tbin)

  # make predicted data
  predicted_data <- CJ(transmitter_instr_model = unique(foo$transmitter_instr_id), rt_distance_m = rt_distance_m, tbin = pred_time)
  predicted_data[bounds, event := event, on = .(tbin >= start, tbin <= end)]
  predicted_data <- predicted_data[!is.na(event),]

  # actual data- by tbin
  real <- foo[tbin %in% pred_time,]
  real[, datetime_f := as.factor(tbin)]

  # define inverse link function
  ilink <- family(mod)$linkinv

  # make prediction (on non-back-transformed data)
  fit <-  predict(mod, newdata = predicted_data, se.fit = TRUE, type = "link")

  # calculate +- 2 SE
  predicted_data[, `:=`(fit_link = fit$fit, se_link = fit$se.fit)] 
  predicted_data[, `:=`(fit_resp = ilink(fit_link), fit_upr = ilink(fit_link + (2 * se_link)), fit_lwr = ilink(fit_link - (2 * se_link)))]
  predicted_data[, `:=`(tbin = as.POSIXct(tbin, origin = '1970-01-01 00:00:00', tz = "UTC"))]
  setkey(predicted_data, transmitter_instr_model, tbin)
  predicted_data[, datetime_f := as.factor(tbin)]

  #make plot (doesn't include error bars)
  pl <- ggplot(predicted_data, aes(x = rt_distance_m,y = fit_resp, group = datetime_f)) +
    geom_line(aes(color = datetime_f), size = 1, show.legend = FALSE) +
#    facet_wrap(vars(transmitter_instr_model)) +
  labs(title = paste0("tag: ", trans, ", rec: ", rec), x = "receiver-tag distance", y = "detection prob") +
  scale_x_continuous(limits = c(0,1000)) +
  scale_y_continuous(limits = c(0,1)) +
  geom_point(data = real, aes(x = rt_distance_m, y = dtc_prob, group = datetime_f), color = "black")
  #scale_color_viridis_d()

  pdf(out_pth)
  print(pl)
  dev.off()

  return(out_pth)
  
  ## low <- loess(dtc_prob ~ rt_distance_m, data = foo, span = 0.2)
  ## foo[, loe_pred := predict(low, foo)]
  ## lines(loe_pred ~ rt_distance_m, data = foo, col = "red")

  ## dev.off()
  
  ## return(out_pth)
}


##########################
#' tar_load(vrl_vem_combined_dtc)
#' dtc <- vrl_vem_combined_dtc
#' tar_load(hst)
#' trial = 2
#' out_pth = "output/rec_abacus_SB.pdf"
#' tar_load(data_present)
#' bounds = data_present
#' receiver_abacus(dtc = dtc, hst = hst, trial = 1, main = "Hammond Bay receiver detections", out_pth = out_pth, bounds = data_present)

receiver_abacus <- function(dtc, hst, trial, out_pth, bounds = data_present, ...){

  dtc <- dtc[receiver_run == trial,]
  hst <- hst[run == trial,]
  set(dtc, j = "receiver_serial_no_fac", value = as.factor(dtc$receiver_serial_no))

  trial_start <- max(hst[][["timestamp_start_utc"]])
  trial_end <- min(hst[][["timestamp_end_utc"]])

  tseries <- seq(lubridate::floor_date(trial_start, "day"), lubridate::ceiling_date(trial_end, "day"), by = "day")
  dtc[, label := as.factor(paste(receiver_instr_model, receiver_serial_no, receiver_freq, sep = ","))]

  pdf(out_pth)
  par(mar = c(4,10,3,2), oma = c(0,1,0,0))
  plot(as.numeric(label) ~ datetime, data = dtc, axes = FALSE, col = "red", pch = 16, xlab = "time", ylab = NA, xlim = c(trial_start, trial_end), ...)
  abline(v = bounds$start, col = "red", lwd = 2, lty = "dashed")
  abline(v = bounds$end, col = "blue", lwd = 2, lty = "dashed")
  axis.POSIXct(1, x = tseries, format = "%Y-%m-%d")
  axis(2, at = unique(dtc$receiver_serial_no_fac), labels = levels(dtc$label), las = 1)
  box()
  mtext("receiver model, serial no, frequency", outer = TRUE, side = 2, line = -1)
  dev.off()
  
  return(out_pth)
}



#######################
#' file abacus plot
#' tar_load(clean_mission)
#' dtc = clean_mission

file_abacus <- function(dtc = clean_mission, out_pth){
  file_ints <- unique(dtc, by = c("file_num", "start_time", "end_time"))

  start <- min(file_ints$start_time)
  end <- max(file_ints$end_time)
  file_ints[, label := as.factor(file_num)]

  pdf(out_pth)
  par(las = 1)
  plot(as.numeric(label) ~ start_time, data = file_ints, col = "green", pch = 16, xlab = "time", ylab = NA, xlim = c(start, end))
  points(as.numeric(label) ~ end_time, data = file_ints, col = "red", pch = 16)
  segments(x0 = file_ints$start_time, y0 = file_ints$file_num, x1 = file_ints$end_time, y1 = file_ints$file_num, col = "black")

  dev.off()
  return(out_pth)
}


#' tar_load(clean_mission)
#' dtc = clean_mission


.data_bounds <- function(dtc = clean_mission){
  out <- unique(dtc[, c("file_num", "start_time", "end_time")])
  out[, `:=`(start_time = as.numeric(start_time), end_time = as.numeric(end_time))]
  
  int <- as.data.table(intervals::interval_union(intervals::Intervals(as.matrix(out[,c("start_time", "end_time")])), check_valid = TRUE))
  int[, group := 1:.N]

  setkey(out, start_time, end_time)
  setkey(int, V1, V2)

  out <- foverlaps(int, out)
  out <- out[, c("group", "V1", "V2")]

  out[, start := as.POSIXct(V1, tz = "UTC", origin = "1970-01-01 00:00:00")]
  out[, end := as.POSIXct(V2, tz = "UTC", origin = "1970-01-01 00:00:00")]
  out <- unique(out[, !c("V1", "V2")])
  setnames(out, "group", "event")
  return(out)
}


