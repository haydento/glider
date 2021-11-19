library(targets)
library(tarchetypes)
source("src/functions.R")
source("src/vem.R")

tar_option_set(packages = c("data.table", "leaflet", "leafem", "htmlwidgets", "leaflet.extras", "stringr", "geosphere", "leafem", "tarchetypes", "mgcv", "ggplot2"))

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

  #all glider detections with interpolated lat/lon.look here for fish detections
  tar_target(
    clean_vem_detections, 
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
  ),

  tar_render(dtc_summary, "src/dtc_table.rmd", output_dir = "output", output_file = "dtc_summary.html"),
  tar_target(
    dtc_summary_clean,
    .dtc_summary(raw_data = vrl_vem_combined_dtc),
    format = "fst_dt"
  ),

  # determine times when we have real-time data in hand
  tar_target(
    data_present,
    .data_present(dtc = vrl_vem_combined_dtc, hst = hst, thresh_sec = (3600*12)),
    format = "fst_dt"
    ),

  # impute missed detections on tags 
  tar_target(
    imputed_transmissions,
    impute_missing_transmissions(dtc = vrl_vem_combined_dtc, hst = hst, mooring_type = "stationary"),
    format = "fst_dt"
  ),

  
 tar_target(
   glider_dtc_transmissions,
   glider_dtc(dtc = vrl_vem_combined_dtc, receiver_site = c("cormorant", "mary_lou"), tag_beeps = imputed_transmissions, glider_geo = glider_trk),
   format = "fst_dt"
 ),

 tar_target(
   glider_dtc_range,
   glider_dtc_transmissions_time_filtered(dtc = glider_dtc_transmissions, inter = data_bounds),
   format = "fst_dt"
 ),

 tar_target( # GAM model of HB trial
   GAMit_HB,
   .GAMit(dtc = glider_dtc_range, trial_run = 1, limit_dist_m = 2500),
   format = "rds"
 ),

 tar_target( # GAM model of SB trial
   GAMit_SB,
   .GAMit(dtc = glider_dtc_range, trial_run = 2, limit_dist_m = 2500),
   format = "rds"
 ),
 
 tar_target( # model output for HB
   mod_output_HB,
   .mod_output(mod = GAMit_HB, dtc = glider_dtc_range, out_pth = "output/predicted_dtc_prob_HB.pdf", limit_dist_m = 2500, text = "Hammond Bay", trial_run = 1),
   format = "file"
 ),

 tar_target( # model output for SB
   mod_output_SB,
   .mod_output(mod = GAMit_SB, dtc = glider_dtc_range, out_pth = "output/predicted_dtc_prob_SB.pdf", limit_dist_m = 2500, text = "Saginaw Bay", trial_run = 2),
   format = "file"
 ),

 tar_target( # receiver abacus for HB
   receiver_abacus_HB,
   receiver_abacus(dtc = vrl_vem_combined_dtc, hst = hst, trial = 1, out_pth = "output/rec_abacus_HB.pdf", main = "Hammond Bay receiver detections", bounds = data_present),
   format = "file"
 ),

 tar_target( # receiver abacus for SB
   receiver_abacus_SB,
   receiver_abacus(dtc = vrl_vem_combined_dtc, hst = hst, trial = 2, out_pth = "output/rec_abacus_SB.pdf", main = "Saginaw Bay receiver detections", bounds = data_present),
   format = "file"
 ),

 tar_target(
   discrete_dtc_prob,
   .discrete_dtc_prob(gear_log = hst, dta = vrl_vem_combined_dtc, bsize = 3600, glider_geo = glider_trk, bounds = data_bounds),
   format = "fst_dt"
 ),

 tar_target(
   discrete_HB,
   discrete_rng_crv(dtc = discrete_dtc_prob, trial = 1, out_pth = "output/discrete_HB_69kHz.pdf", main = "Hammond Bay, 69kHz"),
   format = "file"
 ),

 tar_target(
   discrete_SB,
   discrete_rng_crv(dtc = discrete_dtc_prob, trial = 2, out_pth = "output/discrete_SB_69kHz.pdf", main = "Saginaw Bay, 69kHz"),
   format = "file"
 ),

 tar_target(
   mission_data_summary,
   file_abacus(dtc = clean_mission, out_pth = "output/mission_data_operating.pdf"),
   format = "file"
 ),

 tar_target(
   science_data_summary,
   file_abacus(dtc = clean_sci, out_pth = "output/science_data_operating.pdf"),
   format = "file"
 ),

 tar_target(
   data_bounds,
   .data_bounds(dtc = clean_mission),
   format = "fst_dt"
 ),

 tar_target(
   GAMit_tensor_SB,
   .GAMit_tensor(dtc = glider_dtc_range, trial_run = 2, limit_dist_m = 3000),
   format = "rds"
 ),

 tar_target(
   GAMit_tensor_HB,
   .GAMit_tensor(dtc = glider_dtc_range, trial_run = 1, limit_dist_m = 3000),
   format = "rds"
 ),
 

tar_target(
  dtc_prob_dist_HB,
  .dtc_prob_dist(mod = GAMit_tensor_HB, dtc = glider_dtc_range, out_pth = "output/dtc_prob_rt_dist_HB.pdf", limit_dist_m = 2500, trial_run=1, bounds = data_bounds),
  format = "file"
),


tar_target(
  dtc_prob_dist_SB,
  .dtc_prob_dist(mod = GAMit_tensor_SB, dtc = glider_dtc_range, out_pth = "output/dtc_prob_rt_dist_SB.pdf", limit_dist_m = 2500, trial_run=2, bounds = data_bounds),
  format = "file"
),

tar_target(
  dtc_prob_time_HB,
  .dtc_prob_time(mod = GAMit_tensor_HB, dtc = glider_dtc_range, out_pth = "output/dtc_prob_500m_HB.pdf", limit_dist_m = 2500, trial_run=1, bounds = data_bounds),
  format = "file"
), 

tar_target(
  dtc_prob_time_SB,
  .dtc_prob_time(mod = GAMit_tensor_SB, dtc = glider_dtc_range, out_pth = "output/dtc_prob_500m_SB.pdf", limit_dist_m = 2500, trial_run=2, bounds = data_bounds),
  format = "file"
)



)



