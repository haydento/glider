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
    combine(x = clean_mission, y = clean_sci),
    format = "fst_dt"
  ),
  
  tar_target(
    vem_data,
    "data/vem",
#    list.files("data/vem", full.names = TRUE),
    format = "file"
  ),
  
  tar_target(
    clean_vem_status,
    read_vem(vem_data)$status,
    format = "fst_dt"
  ),
  
  tar_target(
    clean_vem_detections,
    infer_detection_locations(read_vem(vem_data)$detections, glider_trk),
    format = "fst_dt"
  ),

  tar_target(
    instr_deploy_data,
    list.files("data/instr_deploy_log", full.names = TRUE, pattern = "\\.csv$"),
    format = "file"
  ),  

  # just detections with receiver-tag distances calculated and tag/receiver info added
  tar_target( 
    clean_vem_detections_geo,
    get_instr_data(clean_vem_detections, instr_deploy_data),
    format = "fst_dt"
  ),

  
  tar_target(
    glider_leaflet,
    leaflet_map(glider_track = glider_trk, dtc = clean_vem_detections_geo, 
                pth = "docs/index.html", recs = recs),
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
    vps,
    "data/vps/synthetic.positions/all.csv",
    format = "file"
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
