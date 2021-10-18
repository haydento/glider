library(targets)
source("src/functions.R")
source("src/vem.R")

tar_option_set(packages = c("data.table", "leaflet", "leafem", "htmlwidgets", "leaflet.extras", "stringr", "geosphere", "leafem"))

list(

  tar_target(
    sci_data,
    #    list.files("data/glider_decimated/science", full.names = TRUE),
    "data/glider_decimated/science",
    format = "file"
  ),

  tar_target(
    mission_data,
    "data/glider_decimated/mission",
    #list.files("data/glider_decimated/mission", full.names = TRUE),
    format = "file"
  ),

  tar_target(
    clean_sci,
    clean(pth = sci_data, lat = m_gps_lat, lon = m_gps_lon),
    format = "fst_dt"
  ),

  tar_target(
    clean_mission,
    clean(pth = mission_data, lat = m_gps_lat, lon = m_gps_lon),
    format = "fst_dt"
  ),

  tar_target(
    glider_trk,
    combine(x = clean_mission, y = clean_sci, id = trial_id),
    format = "fst_dt"
  ),
  
  tar_target(
    vem_data,
    "data/vem",
    format = "file"
  ),
  
  tar_target(
    clean_vem_status, #operating info from glider receivers
    read_vem(vem_data)$status,
    format = "fst_dt"
  ),
  
  tar_target(
    clean_vem_detections, #all glider detections.  look here for fish detections
    infer_detection_locations(read_vem(vem_data)$detections, glider_trk),
    format = "fst_dt"
  ),

  tar_target(
    vrl_vem_combined,
    rbind(dtc_geo, clean_vem_detections, fill = TRUE),
    format = "fst_dt"
    ),
  
  tar_target(
    instr_deploy_data,
    "data/instr_deploy_log/gear_deployment_log.csv",
    format = "file"
  ),  

  # detections from vrl and vem combined
  # "big joins"- joins receiver and tag location information with detection
  tar_target( 
    vrl_vem_combined_dtc,
    get_instr_data(dta = vrl_vem_combined, hst_l = hst),
    format = "fst_dt"
  ),

  
  tar_target(
    glider_leaflet,
    leaflet_map(glider_track = glider_trk, dtc = vrl_vem_combined_dtc, 
                pth = "docs/index.html", log = hst),
    format = "file"
  ),

  tar_target(
    recs,
    "data/receiver_coords.fst",
    format = "file"
  ),

  tar_target(
    vrl_data,
    "data/vrl/VRL",
    format = "file"
  ),

  tar_target(
    vdat_csv,
    extract_vrl(in_pth = vrl_data, out_dir = "data/vdat_csv", vdat_pth = "/home/todd/tools"),
    format = "file"
  ),

  tar_target(
    dtc,
    compile_dtc(fls = vdat_csv),
    format = "fst_dt"
  ),

  tar_target(
    dtc_geo,
    stationary_recs_geo(vrl = dtc, hst_l = hst),
    format = "fst_dt"
  ),
  
  ## tar_target(
  ##   vps,
  ##   "data/vps/synthetic.positions/all.csv",
  ##   format = "file"
  ## ),

  tar_target(
    hst,
    hst_clean(hst_l = instr_deploy_data),
    format = "fst_dt"
  ),

  tar_target(
    glider_trial,
    "data/range_trial/range_trial_dates.csv",
    format = "file"
  ),

  tar_target(
    trial_id,
    trial_parser(pth = glider_trial),
    format = "fst_dt"
  )
  
  
  

  
)



# 180 tags
## tar_load("clean_vem_detections_geo")

## dtc <- clean_vem_detections_geo[transmitter_freq == "180" & transmitter_mooring_type == "stationary",]

## #MBU-001
## pdf("output/180_receivers_distance_detected.pdf")
## boxplot(rt_distance_meters ~ transmitter_site,
##         data = dtc,
##         ylim = c(0, 500), xlab = NA, las = 1, ylab = "tag-receiver distance", main = "180kHz distance detected")
## dev.off()

## pdf("output/180_receivers_glider_dpth_detection.pdf")
## boxplot(glider_m_depth ~ transmitter_site, data = dtc,
##         ylim = c(0,40), las = 1, ylab = "glider depth at detection", main = "glider depth at detection")

## dev.off()

## pdf("output/180_receivers_tag_receiver_dist.pdf")
## hist(dtc$rt_distance_meters, xlab = "tag receiver distance (m)", main = "tag-receiver distance")
## dev.off()

## dtc <- clean_vem_detections_geo[transmitter_freq == "69" & transmitter_mooring_type == "stationary" & transmitter_instr_model %in% c("V13-1x-L", "V13-1x-H"), ]
## dtc[, id := paste(transmitter_instr_model, transmitter_site, sep = ",")]


## boxplot(rt_distance_meters ~ id, data = dtc)
## boxplot(glider_m_depth ~ id, data = dtc)


## ##################
## tar_load("clean_vem_detections_geo")

## dtc <- clean_vem_detections_geo[transmitter_instr_id == "A180-1702-61651",]

## diff(dtc$datetime)
