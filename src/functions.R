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




# junk below...
#########################

## x <- data.table(dist = c(500,500,1000,500,250,2000,500,2000,500,5000,5000,500))
##   trk[, lag_dist := shift(dist, type = "lag")]
##   x[, lead_dist := shift(dist, type = "lead")]
##   x[, arrive := 0]
##   x[, depart := 0]
##   x[dist < 1000 & lag_dist > 1000, arrive := 1]
##   x[dist < 1000 & lead_dist >1000, depart := 1]
##   x[dist < 1000 & is.na(lag_dist), arrive := 1]
##   x[dist < 1000 & is.na(lead_dist), depart := 1]
##   x[, event := cumsum(arrive), ]

  

## tar_load("vrl_vem_combined_dtc")
## dtc <- vrl_vem_combined_dtc

## dtc[receiver_site == "MBU-001" & transmitter_instr_id %in% c("A69-1604-32405", "A69-1604-32406"),]
## dtc[transmitter_instr_id %in% c("A69-1604-32405", "A69-1604-32406"),]
## foo <- dtc[receiver_site == "mary_lou" & transmitter_instr_id %in% c("A69-1604-32405", "A69-1604-32406")]
## unique(foo$transmitter_instr_id)
## setkey(dtc, datetime)
## foo <- diff(dtc$datetime)
## hist(as.numeric(foo))
## range(foo)

#' tar_load("vrl_vem_combined_dtc")
#' dtc <- vrl_vem_combined_dtc
#' ref_tags = c("A69-1604-32405", "A69-1604-32406", "A180-1702-61650", "A180-1702-61651", "A69-1604-32401", "A69-1604-32402")
#' run = 1
#' ref_receivers = c("MBU-001", "MBU-002")
#'  thresh = list("A69-1604-32405" = 240, "A69-1604-32406" = 240, "A180-1702-61650" = 120, "A180-1702-61651" = 120, "A69-1604-32401" = 240, "A69-1604-32402" = 240)
#' tag_beeps = impute_missing_transmissions(dtc = dtc, ref_tags = ref_tags, run = run, ref_receivers = ref_receivers, thresh = thresh)

impute_missing_transmissions <- function(dtc, ref_tags = c("A69-1604-32405", "A69-1604-32406", "A180-1702-61650", "A180-1702-61651", "A69-1604-32401", "A69-1604-32402"), run = 1, ref_receivers = c("MBU-001", "MBU-002"), thresh = list("A69-1604-32405" = 240, "A69-1604-32406" = 240, "A180-1702-61650" = 120, "A180-1702-61651" = 120, "A69-1604-32401" = 240, "A69-1604-32402" = 240)){
  
  beeps <- dtc[receiver_run %in% run & transmitter_instr_id %in% ref_tags, c("datetime", "receiver_run", "transmitter_instr_id", "transmitter_instr_model", "transmitter_freq", "transmitter_longitude", "transmitter_latitude", "receiver_site", "receiver_instr_model")] 
  setnames(beeps, c("receiver_run"), c("trial"))
  setkey(beeps, datetime)
  
  imputed_trans <- function(x, y){
    beeps.i <- beeps[transmitter_instr_id == x,]
    beeps.i$thresh <- y[[beeps.i$transmitter_instr_id[1]]]
    out <- impute_dtc(dtc = beeps.i, t_thresh = beeps.i$thresh[1])

    return(out)
  }

  out <- lapply(ref_tags, imputed_trans, thresh)
  out <- rbindlist(out)
  return(out)
}





#' tar_load("vrl_vem_combined_dtc")
#' dtc <- vrl_vem_combined_dtc
#' ref_tags = c("A69-1604-32405", "A69-1604-32406", "A180-1702-61650", "A180-1702-61651", "A69-1604-32401", "A69-1604-32402")
#' run = 1
#' receiver_site = "mary_lou"
#' tar_load(glider_trk)
#' glider_geo = glider_trk
#' tar_load("imputed_transmissions")
#' tag_beeps = imputed_transmissions
#' tar_load(glider_trk)
#' glider_geo = glider_trk

glider_dtc <- function(dtc, ref_tags = c("A69-1604-32405", "A69-1604-32406", "A180-1702-61650", "A180-1702-61651", "A69-1604-32401", "A69-1604-32402"), receiver_site = "cormorant", tag_beeps, glider_geo){ 


glider_dtc <- dtc[transmitter_instr_id %in% ref_tags & receiver_site %in% "cormorant", c("datetime", "transmitter_instr_id", "receiver_site", "glider_lat_dd", "glider_lon_dd")]

  glider_dtc[, tran_dtc := 1]
  tag_beeps[, tran_dtc := 0]
  tag_beeps[glider_dtc, ':=' (tran_dtc = 1, glider_lat = glider_lat_dd, glider_lon = glider_lon_dd), on = .(transmitter_instr_id = transmitter_instr_id, datetime = datetime), roll = "nearest"]
  
  tag_beeps[, `:=`(glider_lon = approx(x = glider_geo$time[!is.na(glider_geo$lon_dd)],
                                       y = glider_geo$lon_dd[!is.na(glider_geo$lon_dd)],
                                       xout = datetime,
                                       ties = "ordered")$y,
                   glider_lat = approx(x = glider_geo$time[!is.na(glider_geo$lat_dd)],
                                       y = glider_geo$lat_dd[!is.na(glider_geo$lon_dd)],
                                       xout = datetime,
                                       ties = "ordered")$y,
                   rt_distance_m = geosphere::distVincentyEllipsoid(p1 = cbind(glider_lon, glider_lat), p2 = cbind(transmitter_longitude, transmitter_latitude)))]



  return(tag_beeps)
}


  
  


  
## tar_load("glider_dtc_transmissions")
## dtc <- glider_dtc_transmissions


## plot(tran_dtc ~ rt_distance_m, data = tag_beeps[transmitter_instr_id == "A69-1604-32405"], pch = 16, xlim = c(0,1000))
