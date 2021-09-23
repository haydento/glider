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
            channel = data.table::tstrsplit(C, "=", keep = 2)[[1]])] 
  } else {
    vem_tag[ , `:=`(signal_level_db = NA_real_,
                    noise_level_db = NA_real_,
                    channel = NA_real_)][0,]
  }
  
  data.table::setkey(vem_sts, datetime)
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

