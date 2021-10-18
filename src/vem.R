#' Parse data from Innovasea cabled acoustic telemetry receiver (vem file)
#' 
#' @param a character vector containing paths to vem files
#' 
#' @return A list with two elements: one with status data and one with 
#'  detection data.
#' 
#' @examples 
#' 
#' vem_files <- list.files("./data/vem", 
#'                         pattern = "\\.vem$|\\.VEM$", 
#'                         full.names = TRUE)  
#'                         
#' vem <- read_vem(vem_files)
#' 
#' @export
read_vem <- function(vem_files){
   vem_files <- list.files(vem_files, full.names = TRUE)
  #read data from each vem file and combine
  vem <- list(
    status = data.table::rbindlist(lapply(vem_files, 
                                          function(x) .read_vem(x)$status)),
    detections = data.table::rbindlist(lapply(vem_files, 
                                          function(x) .read_vem(x)$detections)))
  
  #sort by date time
  data.table::setkey(vem$status, datetime)
  data.table::setkey(vem$detections, datetime)
  
  return(vem)
}


#' Parse a vem file

.read_vem <- function(vem_file){
    #see section 2.8 in Innovasea Rx-LIVE Receiver User Manual
  # https://go.innovasea.com/rx_live_receiver_manual.pdf
  
  #schema for Rx-LIVE output ascii
  #serial_no = receiver serial number
  #sequence = 3 digit line counter (000-999)
  #datetime = event time as 24 hour UTC time as YYYY-MM-DD HH:MM:SS.mmm
  #info = either Rx-LIVE status information or tag detection information
  
  #identify column names
  col_names <- c("serial_no", "sequence", "datetime", "info") 
                 
  col_names_sts <- c("DC", "PC", "LV", "T", "DU", "RU", "XYZ", "N", "NP", "HH")
  
  col_names_tags <- c("transmitter_code_space", "transmitter_id", 
                      "sensor_value_adc", "S", "N", "C", "HH")
  
  #read data into list
  vem <- data.table::fread(vem_file, sep = ",", header = FALSE, fill = TRUE,
                           tz = "")
  
  names(vem)[seq_along(col_names)] <- col_names
  
  #add file name
  vem[ , source_file := basename(vem_file)]

  #coerce timestamps (here instead of above for error checking)
  vem[ , datetime := lubridate::fast_strptime(datetime, 
                                              format = "%Y-%m-%d %H:%M:%OS",
                                              lt = FALSE)]
  
  #check timestamp and omit with warning
  bad_timestamp <- which(is.na(vem$datetime))
  if(length(bad_timestamp) > 0) {
    vem <- vem[-bad_timestamp] 
    warning(length(bad_timestamp), " record was ignored from `", 
            basename(vem_file), "' (line ", bad_timestamp, ")",
            " because of invalid 'datetime'.")
  }
  
    
  #subset scheduled status information records
  vem_sts <- vem[info == "STS"]
  
  #subset records from transmitters with sensors
  vem_sensor_tag <- vem[info != "STS" & grepl("^S=", V7)]

  #subset records from pinger transmitters (without sensors)
  vem_pinger_tag <- vem[info != "STS" & grepl("^S=", V6)]  
  
  
  
  #parse STS identifier:value pairs
  names(vem_sts)[5:14] <- col_names_sts
  vem_sts[ , `:=`(detection_count = data.table::tstrsplit(DC, "=", keep = 2)[[1]],
          ping_count = data.table::tstrsplit(PC, "=", keep = 2)[[1]],
          line_voltage_volts = data.table::tstrsplit(LV, "=", keep = 2)[[1]],
          internal_temperature_celcius = data.table::tstrsplit(T, "=", 
                                                               keep = 2)[[1]],
          detection_memory_used = data.table::tstrsplit(DU, "=", keep = 2)[[1]],
          raw_memory_used_percent = data.table::tstrsplit(RU, "=", keep = 2)[[1]],
          tilt_information_in_g = data.table::tstrsplit(XYZ, "=", keep = 2)[[1]],
          output_noise = data.table::tstrsplit(N, "=", keep = 2)[[1]],
          output_ppm_noise = data.table::tstrsplit(NP, "=", keep = 2)[[1]])]
  
  #parse STS tilt information
  vem_sts[ , c("tilt_x", "tilt_y", "tilt_z") := 
             data.table::tstrsplit(tilt_information_in_g, ":")]
  
  #parse sensor tag records
  names(vem_sensor_tag)[4:10] <- col_names_tags
  
  #drop empty
  vem_sensor_tag[, paste0("V", 11:14) := NULL]
  
  #parse sensor tag records
  names(vem_pinger_tag)[4:9] <- setdiff(col_names_tags, "sensor_value_adc")
  
  #drop empty
  vem_pinger_tag[, paste0("V", 10:14) := NULL]
  
  #combine sensor and nonsensor tag
  vem_tag <- rbind(vem_pinger_tag, vem_sensor_tag, fill = TRUE)
  
  #parse tag identifier:value pairs
  if(nrow(vem_tag) > 0){
    vem_tag[ , `:=`(
            signal_level_db = data.table::tstrsplit(S, "=", keep = 2)[[1]],
            noise_level_db = data.table::tstrsplit(N, "=", keep = 2)[[1]],
            channel = data.table::tstrsplit(C, "=", keep = 2)[[1]],
            frequency = gsub("^[a-zA-Z]", "", 
                              data.table::tstrsplit(transmitter_code_space, 
                                                     "-", keep = 1)[[1]]))]
  } else {
    vem_tag[ , `:=`(signal_level_db = NA_real_,
                    noise_level_db = NA_real_,
                    channel = NA_real_,
                    frequency = NA_character_)][0,]
  }
  
  #coerce numeric columns and order by time
  vem_sts[ , `:=`(serial_no = as.character(serial_no),
                  detection_count = as.numeric(detection_count),
                  ping_count = as.numeric(ping_count),
                  line_voltage_volts = as.numeric(line_voltage_volts),
                  internal_temperature_celcius = as.numeric(internal_temperature_celcius),
                  detection_memory_used = as.numeric(detection_memory_used),
                  raw_memory_used_percent = as.numeric(raw_memory_used_percent),
                  output_noise = as.numeric(output_noise),
                  output_ppm_noise = as.numeric(output_ppm_noise),
                  tilt_x = as.numeric(tilt_x),
                  tilt_y = as.numeric(tilt_y),
                  tilt_z = as.numeric(tilt_z))]  
  
  data.table::setkey(vem_sts, datetime)
  
  vem_tag[ , `:=`(serial_no = as.character(serial_no),
                  sensor_value_adc = as.numeric(sensor_value_adc),
                  signal_level_db = as.numeric(signal_level_db),
                  noise_level_db = as.numeric(noise_level_db))]
  
  data.table::setkey(vem_tag, datetime)
  
  
  vem <- list(status = vem_sts, detections = vem_tag)
  
}


#' Add locations of each detection from glider
#' 
#' @param cleaned detection locations
#' 
#' @examples
#' targets::tar_load("clean_vem_detections")
#' dtc <- clean_vem_detections
#' targets::tar_load("glider_trk")
#' pos <- glider_trk

infer_detection_locations <- function(dtc, pos){
  
  dtc[ , `:=`(lon_dd = approx(x = pos$time[!is.na(pos$lon_dd)],
                              y = pos$lon_dd[!is.na(pos$lon_dd)],
                              xout = datetime,
                              ties = "ordered")$y,
              lat_dd = approx(x = pos$time[!is.na(pos$lat_dd)],
                              y = pos$lat_dd[!is.na(pos$lat_dd)],
                              xout = datetime,
                              ties = "ordered")$y,
              m_depth = approx(x = pos$time[!is.na(pos$m_depth)],
                              y = pos$m_depth[!is.na(pos$m_depth)],
                              xout = datetime,
                              ties = "ordered")$y)]
}


#' Add info about tags and receivers from instr log to clean_vem_detections
#' 
#' @param dtc cleaned detection data from glider with locations added
#' 
#' @param hst instrument history log (tag and reciever metadata)
#' 
#' @examples
#' targets::tar_load("vrl_vem_combined")
#' dta <- vrl_vem_combined
#' 
#' targets::tar_load("hst")
#' hst_l <- hst

get_instr_data <- function(dta, hst_l){
    
  #deep copy
  dtc <- data.table::as.data.table(dta)
  hst <- data.table::as.data.table(hst_l)
  
  # Rename all dtc cols to transmitter_, receiver_, glider_ col names
  
  # recievers
  rec_col_names <- c("serial_no", "frequency", "noise_level_db",
                              "channel", "source_file")
  data.table::setnames(dtc, rec_col_names, paste0("receiver_", rec_col_names))
                     

  # transmitters
  tag_col_names <- c("sensor_value_adc", "signal_level_db")
  data.table::setnames(dtc, tag_col_names, paste0("transmitter_", 
                                                  tag_col_names))    
  
  # combine code space and id to match hst
  dtc[, transmitter_instr_id := paste0(transmitter_code_space, "-",
                                        transmitter_id),
       by = c("transmitter_code_space", "transmitter_id")]
  
  # glider
  gld_col_names <- c("lon_dd", "lat_dd", "m_depth")
  data.table::setnames(dtc, gld_col_names, paste0("glider_", gld_col_names))    
  
  
  # Split hst by instr type and add instr prefix to column names as above
  
  # receivers
  hst_rec <- hst[instr == "receiver"]
  
  #  rename instr_id to serial_no to match dtc
  data.table::setnames(hst_rec, "instr_id", "serial_no")
  
  #  add receiver_ prefix
  data.table::setnames(hst_rec, names(hst_rec), 
                       paste0("receiver_", names(hst_rec)))


  # transmitters
  hst_tag <- hst[instr == "tag"]

  #  parse instr_id to serial_no to match dtc
  data.table::setnames(hst_tag, names(hst_tag), 
                       paste0("transmitter_", names(hst_tag)))  

  
  #--------------------------------------
  # BIG JOINS
  
  # STEP 1. Non equi join hst_tag with dtc by transmitter id, datetime

  # A69-1601-63808 and A180-1702-61651 were not detected.  Added "nomatch = NULL" to not
  # include records of "hst_tag" that do not match records of dtc.
  # should be accomplish the same thing by swapping hst_tag and dtc position in join.
  # data.table does left join where i in x[i] keeps all rows.
  # in this case, we want all of the dtc records in the output but not any unmatched rows in hst_tag
  dtc2 <- dtc[hst_tag, on = list(datetime >= transmitter_timestamp_start_utc,
                                 datetime <= transmitter_timestamp_end_utc,
                                 transmitter_instr_id = transmitter_instr_id),
                  list(datetime = x.datetime, 
                       receiver_serial_no, 
                       receiver_channel,
                       receiver_frequency,
                       receiver_source_file,
                       receiver_noise_level_db,
                       transmitter_run_id,
                       transmitter_run,
                       transmitter_instr,
                       transmitter_mooring_type,
                       transmitter_instr_model,
                       transmitter_freq,
                       transmitter_instr_id,
                       transmitter_site,
                       transmitter_latitude,
                       transmitter_longitude,
                       transmitter_water_depth,
                       transmitter_instr_elevation_from_bottom,
                       transmitter_instr_depth_from_top,
                       transmitter_timestamp_start_utc,
                       transmitter_timestamp_end_utc,
                       transmitter_comment,
                       transmitter_sensor_value_adc,
                       transmitter_signal_level_db,
                       glider_lon_dd,
                       glider_lat_dd,
                       glider_m_depth
                       ), nomatch = NULL]
  

  # STEP 2. Non equi join hst_rec with dtc by transmitter id, datetime
  
  dtc2 <- dtc2[hst_rec, on = list(datetime >= receiver_timestamp_start_utc,
                                   datetime <= receiver_timestamp_end_utc,
                                   receiver_serial_no = receiver_serial_no),
                  list(datetime = x.datetime, 
                       receiver_run_id,
                       receiver_run,
                       receiver_site,
                       receiver_instr,
                       receiver_mooring_type,
                       receiver_instr_model,
                       receiver_serial_no, 
                       receiver_channel,
                       receiver_freq,
                       receiver_frequency,
                       receiver_latitude,
                       receiver_longitude,
                       receiver_water_depth,
                       receiver_instr_elevation_from_bottom,
                       receiver_instr_depth_from_top,
                       receiver_timestamp_start_utc,
                       receiver_timestamp_end_utc,
                       receiver_comment,
                       receiver_source_file,
                       receiver_noise_level_db,
                       transmitter_run_id,
                       transmitter_run,
                       transmitter_instr,
                       transmitter_mooring_type,
                       transmitter_instr_model,
                       transmitter_freq,
                       transmitter_instr_id,
                       transmitter_site,
                       transmitter_latitude,
                       transmitter_longitude,
                       transmitter_water_depth,
                       transmitter_instr_elevation_from_bottom,
                       transmitter_instr_depth_from_top,
                       transmitter_timestamp_start_utc,
                       transmitter_timestamp_end_utc,
                       transmitter_comment,
                       transmitter_sensor_value_adc,
                       transmitter_signal_level_db,
                       glider_lon_dd,
                       glider_lat_dd,
                       glider_m_depth
                       ), nomatch = NULL]  
  
  # STEP 3. Update receiver_ and transmitter_ lonlat with glider
  
  
  #Update receiver records with relevant from glider
  dtc2[receiver_site %in% c("mary_lou", "cormorant") & receiver_mooring_type == "mobile",
       `:=`(receiver_latitude = glider_lat_dd,
            receiver_longitude = glider_lon_dd,
            receiver_water_depth = NA, #!! calc from depth + altitude
            receiver_instr_elevation_from_bottom = NA, # !! missing from dtc
            receiver_instr_depth_from_top = glider_m_depth
       )]
 
  #Update transmitter records with relevant from glider
  dtc2[transmitter_site %in% c("mary_lou", "cormorant") & transmitter_mooring_type == "mobile",
       `:=`(transmitter_latitude = glider_lat_dd,
            transmitter_longitude = glider_lon_dd,
            transmitter_water_depth = NA, #!! calc from depth + altitude
            transmitter_instr_elevation_from_bottom = NA, # !! missing from dtc
            transmitter_instr_depth_from_top = glider_m_depth
       )] 
  
  
  # ----------------------
  
  # Calculate distance between tag and receiver at each transmission
  dtc2[ , `:=`(
            rt_distance_meters = geosphere::distVincentyEllipsoid(
                      p1 = cbind(receiver_longitude, receiver_latitude), 
                      p2 = cbind(transmitter_longitude, transmitter_latitude)))]

  # Calculate heading between glider and transmitter
  dtc2[(receiver_longitude != transmitter_longitude) |  
       (receiver_latitude != transmitter_latitude), 
       `:=`(
          rt_heading_degrees = geosphere::bearing(
            p1 = cbind(receiver_longitude, receiver_latitude), 
            p2 = cbind(transmitter_longitude, transmitter_latitude)))]
  
  
  
  return(dtc2)
}


########################
#' tar_load("clean_vem_detections_geo")
#' dtc <- clean_vem_detections_geo
#' dtc[receiver_freq == 180,]
## targets::tar_load("clean_vem_detections_geo")

## boxplot(rt_distance_meters ~ transmitter_freq,
##         data = clean_vem_detections_geo[rt_distance_meters > 0],
##         ylim = c(0, 2200), xlab = NA, las = 2)


## par(mar = c(10,5,4,1))
## boxplot(rt_distance_meters ~ transmitter_instr_id,
##         data = clean_vem_detections_geo[rt_distance_meters > 0],
##         ylim = c(0, 2200), xlab = NA, las = 2)
