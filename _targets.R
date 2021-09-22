library(targets)
source("src/functions.R")

tar_option_set(packages = c("data.table", "leaflet", "leafem", "htmlwidgets", "leaflet.extras", "stringr"))

list(
  tar_target(
    sci_data,
    "data/ascii_depth_lat_lon_alt_temp.txt",
    format = "file"
  ),

  tar_target(
    mission_data,
    "data/ascii_depth_lat_lon_pitch_roll.txt",
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
    glider_leaflet,
    leaflet_map(glider_track = glider_trk, pth = "docs/index.html"),
    format = "file"
  )
)

  
  
 
