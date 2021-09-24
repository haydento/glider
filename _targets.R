library(targets)
source("src/functions.R")
source("src/vem.R")

tar_option_set(packages = c("data.table", "leaflet", "leafem", "htmlwidgets", "leaflet.extras", "stringr"))

list(

  tar_target(
    sci_data,
    list.files("data/glider_decimated/science", full.names = TRUE),
    format = "file"
  ),

  tar_target(
    mission_data,
    list.files("data/glider_decimated/mission", full.names = TRUE),
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
    list.files("data/vem", full.names = TRUE),
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
    glider_leaflet,
    leaflet_map(glider_track = glider_trk, dtc = clean_vem_detections, 
                pth = "docs/index.html", recs = recs),
    format = "file"
  ),

  tar_target(
    recs,
    "data/receiver_coords.fst",
    format = "file"
    )
)

  
  
 
