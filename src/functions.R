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
#' tar_load("sci_data")
#' pth_files = sci_data
#' tar_load("hst")
#' trial = hst
#' 
#' tst <- clean(pth = mission_data, lat = m_gps_lat, lon = m_gps_lon)


clean <- function(pth, lat = m_pth_lat, lon = m_gps_lon, trial = hst){
  pth_files <- list.files(pth, full.names = TRUE)
  out <- .clean(pth = pth_files, lat = lat, lon = lon)
  return(out)
}

.clean <- function(pth = pth_files, lat = m_gps_lat, lon = m_gps_lon){

  # bring in all files and drop the second row
  dta <- lapply(pth, function(x){data.table::fread(x, na.strings = "NaN")[-1]})
  dta <- data.table::rbindlist(dta, fill = TRUE)

  # convert all columns from character to numeric
  dta[, names(dta) := lapply(.SD, as.numeric)]
  data.table::set(dta, j="time", 
                  value = as.POSIXct(dta$time, 
                                     origin = "1970-01-01 00:00:00", 
                                     tz = "UTC"))
  dta[, lat_dd := to_dd(m_gps_lat)]
  dta[, lon_dd := to_dd(m_gps_lon)]
  
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
#' 
#' @examples
#' tar_load("vrl_vem_combined_dtc")
#' tar_load("hst")
#'  dtc <- vrl_vem_combined_dtc

impute_missing_transmissions <- function(dtc, hst){

  # find co-located receiver for each stationary tag
  tags <- hst[instr == "tag" & mooring_type == "stationary"]
  recs <- hst[instr == "receiver" & mooring_type == "stationary"]
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
  tags <- hst[instr == "tag" & mooring_type == "mobile" & instr_model != "V5D-1x"]
  recs <- hst[instr == "receiver" & mooring_type == "mobile", ]
  tags[recs, `:=` (colocated_rec = i.instr_id), on = .(run_id, run, site, freq)] 

  beeps <- dtc[tags, .(datetime, trial = receiver_run, transmitter_instr_id, transmitter_instr_model, transmitter_freq, transmitter_longitude, transmitter_latitude, receiver_site, receiver_instr_model, colocated_rec, tag_min_delay, tag_max_delay), on = .(receiver_run_id = run_id, receiver_serial_no = colocated_rec, transmitter_instr_id = instr_id), nomatch = NULL]
setkey(beeps, datetime)
  
  beeps[, `:=`(arrive_diff = data.table::shift(datetime, fill = NA, type = "lag"))]
  beeps[, `:=`(depart_diff = data.table::shift(datetime, fill = NA, type = "lead"))]

  beeps[, `:=`(arrive_diff = as.numeric(datetime - arrive_diff), depart_diff = as.numeric(depart_diff - datetime))]
  set(beeps, j = "arrive", value = 0)
  set(beeps, j = "depart", value = 0)
  beeps[is.na(arrive_diff) | arrive_diff > thresh_sec, arrive := 1]
  beeps[is.na(depart_diff) | depart_diff > thresh_sec, depart := 1]
  beeps[, event := cumsum(arrive)]
  out <- beeps[, .(start = min(datetime), end = max(datetime)), by = .(event)]

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


################

## tar_load("glider_dtc_range")
## dtc <- glider_dtc_range[transmitter_instr_id %in% c("A69-1604-32401", "A69-1604-32402", "A69-1604-32405", "A69-1604-32406", "A180-1702-61650", "A180-1702-61651")]

## mod <- gam(tran_dtc ~ s(rt_distance_m, bs = "cs", k = 40, by = as.factor(transmitter_freq)), data = dtc, family = "binomial")

## appraise(mod)
## draw(mod)
## dtc[, mod_fit := predict(mod, type = "response")]
## gam.check(mod)

## setkey(dtc, transmitter_freq, rt_distance_m)
## plot(tran_dtc ~ rt_distance_m , data = dtc[transmitter_freq == 69], pch = 16, col = "black", main = "69kHz", las = 1)
## lines(mod_fit ~ rt_distance_m, data = dtc[transmitter_freq == 69], col = "red")






## https://fromthebottomoftheheap.net/2018/12/10/confidence-intervals-for-glms/
## dtc[tran_dtc == 0, obs_col := "black"][tran_dtc == 1, obs_col := "red"]
## new_data <- data.table(rt_distance_m = c(seq(from = min(dtc[transmitter_freq == 180, rt_distance_m]), max(dtc[transmitter_freq == 180, rt_distance_m]), length = 50), seq(min(dtc[transmitter_freq == 69, rt_distance_m]), max(dtc[transmitter_freq == 69, rt_distance_m]), length = 50)), transmitter_freq = rep(c(180, 69), each = 50))

## new_data[, resp := predict(mod, newdata = new_data, type = "response")]

setkey(new_data, transmitter_freq, rt_distance_m)


## library(ggplot2)

## ggplot(new_data, aes(x = rt_distance_m, y = resp, group = transmitter_freq, color = as.factor(transmitter_freq))) +
##   geom_line() +
##   geom_rug(data = dtc, aes(y = tran_dtc, color = obs_col))











## mod1 <- gam(tran_dtc ~ s(as.numeric(datetime), bs = "cs", by = as.factor(transmitter_freq)), data = dtc, family = "binomial")
## draw(mod1)
## dtc[, mod_fit_tm := predict(mod, type = "response")]

## setkey(dtc, transmitter_freq, datetime)
## plot(tran_dtc ~ as.numeric(datetime), data = dtc[transmitter_freq == 69], pch = 16, col = "black", main = "69kHz", las = 1)
## lines(mod_fit_tm ~ as.numeric(datetime), data = dtc[transmitter_freq == 69], col = "red")



## dtc[, event_freq := paste(event, transmitter_freq, sep="_")]

## mod2 <- gam(tran_dtc ~ s(as.numeric(datetime), bs = "cs", by = as.factor(event_freq), k = 15), data = dtc, family = "binomial")
## draw(mod2)
## gam.check(mod2)


## mod3 <- gam(tran_dtc ~ t2(rt_distance_m, as.numeric(datetime), bs = c("cs", "cs"), k = c(40, 12), by = as.factor(transmitter_freq)), data = dtc, family = "binomial")




## plot(tran_dtc ~ as.factor(event), data = dtc[dtc$transmitter_freq == ""], ylim = c(0,0.2))

## dtc[, .(.N), by = .(event, tran_dtc, transmitter_freq)] 
## dtc[, .(.N), by = .(tran_dtc, transmitter_freq)]








## setkey(dtc, transmitter_freq, rt_distance_m)


## pdf("output/initial_range_curves.pdf")
## par(mfrow = c(2,1))
## plot(tran_dtc ~ rt_distance_m, data = dtc[transmitter_freq == 180], pch = 16, col = "black", main = "180 kHz", xlim = c(0,500), las = 1, ylab = "detection prob", xlab = "tag-receiver dist (m)")
## lines(mod_fit ~ rt_distance_m, data = dtc[transmitter_freq == 180], col = "red")

## plot(tran_dtc ~ rt_distance_m, data = dtc[transmitter_freq == 69], pch = 16, col = "black", main = "69 kHz", xlim = c(0,3000), las = 1, ylab = "detection prob", xlab = "tag-receiver dist (m)")
## lines(mod_fit ~ rt_distance_m, data = dtc[transmitter_freq == 69], col = "red")
## dev.off()


## library(gratia)
## draw(mod)
## appraise(mod)




## ####
## pdf("output/initial_range_curves.pdf")
## par(mfrow = c(6,1))
## plot(tran_dtc ~ rt_distance_m, data = dtc[transmitter_instr_id == "A69-1604-32402"], pch = 16, col = "black", main = "180 kHz", xlim = c(0,2000), las = 1, ylab = "detection prob", xlab = "tag-receiver dist (m)")
## lines(mod_fit ~ rt_distance_m, data = dtc[transmitter_instr_id == "A69-1604-32402"], col = "red")

## plot(tran_dtc ~ rt_distance_m, data = dtc[transmitter_freq == 69], pch = 16, col = "black", main = "69 kHz", xlim = c(0,3000), las = 1, ylab = "detection prob", xlab = "tag-receiver dist (m)")
## lines(mod_fit ~ rt_distance_m, data = dtc[transmitter_freq == 69], col = "red")
## dev.off()





## gam.check(mod)
## plot.gam(mod)

## ## plot(tran_dtc ~ rt_distance_m, data = dtc[transmitter_freq == 69])



## load_mgcv()
## df <- data_sim("eg1", n = 1000, dist = "poisson", scale = 0.1, seed = 6)

## # A poisson example
## m <- gam(y ~ s(x0, bs = "cr") + s(x1, bs = "cr") + s(x2, bs = "cr") +
##          s(x3, bs = "cr"), family = poisson(), data = df, method = "REML")
## rootogram(m, plot = TRUE)
