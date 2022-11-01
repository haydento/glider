library(targets)
library(tarchetypes)
library(data.table)
library(tibble)
source("src/functions.R")
source("src/vem.R")

tar_option_set(packages = c("data.table", "leaflet", "leafem", "htmlwidgets", "leaflet.extras", "stringr", "geosphere", "leafem", "tarchetypes", "mgcv", "ggplot2", "viridis"))

list(
#-----------------------------
 #### 2022-10-24 import full processed glider data- 2021 glider data for HB. Data downloaded from IOOS DAC on 2022-10-24 from url- gliders.ioos.us.  Data for HB
 tar_target(
   HB,
   .load_glider("data/full_glider/marylou-20210920T1530_HB.csv"),
   format = "fst_dt"
 ),

# 2021 glider data for SB.  Data downloaded from IOOS DAC on 2022-10-24 from url- gliders.ioos.us.  Data for SB.
 tar_target(
  SB,
  .load_glider("data/full_glider/cormorant-20211013T1655_SB.csv"),
  format = "fst_dt"
 ),

# combine both glider datasets into single object file.  This is environmental data from glider only.
 tar_target(
   glider_limno,
   rbind(HB, SB, idcol = "run", fill = TRUE),
   format = "fst_dt"
 ),
 
#------------------------------------
  # path to VEM data.  Confirmed on 2022-10-21, got vem files from cailin in email and all files are accounted for.  These are receiver detections from glider recs only.
  tar_target(
    vem_data,
    "data/vem",
    format = "file"
  ),

  # read VEM status files, parse, and clean-up
  # this object contains the receiver operating status info for the glider only, NO detections
  tar_target(
    clean_vem_status, #operating info from glider receivers
    read_vem(vem_data)$status,
    format = "fst_dt"
  ),


 # read in VEM detections.  Compiles detections from all VEM files.
 tar_target(
   clean_vem_detections,
   read_vem(vem_data)$detections,
   format = "fst_dt"
   ),
 
 # read in vem detections from glider, use linear interpolation to estimate limnological data and location from glider sensors for glider detections.
 # Output is vem detection file with additional columns for glider limno data.
  tar_target(
    vem_dtc,
    infer_detection_locations_multi(i_cols = c("precise_lat", "precise_lon", "pressure_bar", "rinkoii_temperature_C", "salinity_1", "sci_flur_units_ppb", "temperature_C", "u_m_s-1", "v_m_s-1", "water_depth (m)" ), in_time = "precise_time_utc", pos = glider_limno, dtc = clean_vem_detections, dtc_timestamp = "datetime"),
    format = "fst_dt"
  ),

   # combine stationary receiver detections from vrl files with detections that have interpolated positions from the glider VEM files (this is for the full processed glider data)
  tar_target(
    all_dtc,
    rbind(dtc_geo, vem_dtc, fill = TRUE),
    format = "fst_dt"
    ),
    
  # path to instrument deployment/recovery locations and operating specs
  tar_target(
    instr_deploy_data,
    "data/instr_deploy_log/gear_deployment_log.csv",
    format = "file"
  ),  

  # detections from vrl and vem combined (this is for the full glider dataset)
  # "big joins"- joins receiver and tag location information with detection data and creates a "to-from" type dataset where tag transmissions are linked to receivers and each tag and receiver mooring type is included.  This is for the full glider dataset in 2021.
  # this dataset is the one used for detection range analyses
 tar_target(
   proc_dtc,
   get_instr_data(dta = all_dtc, hst_l = hst),
   format = "fst_dt"
 ),
  
 # path to vrl data downloaded from static receivers
  tar_target(
    vrl_data,
    "data/vrl/VRL",
    format = "file"
  ),

  # list of paths to vrl files for both HB and SB trial
  tar_target(
    vdat_csv,
    extract_vrl(in_pth = vrl_data, out_dir = "data/vdat_csv", vdat_pth = "/home/todd/tools"),
    format = "file"
  ),

  # Detections from all stationary receivers
  tar_target(
    dtc,
    compile_dtc(fls = vdat_csv),
    format = "fst_dt"
  ),

  # Adds lat/lon to stationary receivers.  Information for stationary receivers is in "hst"
  tar_target(
    dtc_geo,
    stationary_recs_geo(vrl = dtc, hst_l = hst),
    format = "fst_dt"
  ),

  # cleaned-up instrument log
  tar_target(
    hst,
    hst_clean(hst_l = instr_deploy_data),
    format = "fst_dt"
  ),

  # path to glider trial start-end dates csv document
  tar_target(
    glider_trial,
    "data/range_trial/range_trial_dates.csv",
    format = "file"
  ),

  # Formats glider trial information
  tar_target(
    trial_id,
    trial_parser(pth = glider_trial),
    format = "fst_dt"
  ),

 # pulls together visualization of glider track, tag transmissions, and detections.  Output is in docs/index.html and is on github pages at https://haydento.github.io/glider/
 tar_target(
   glider_leaflet,
   leaflet_map(glider_track = glider_limno, dtc = proc_dtc, 
               pth = "docs/index.html", log = hst),
   format = "file"
 ),

  # renders summary document of detection. (output/dtc_summary.html)
  # shows number of detections from STATIONARY tags detected by MOBILE glider
  tar_render(dtc_summary, "src/dtc_table.rmd", output_dir = "output", output_file = "dtc_summary.html"),

  # prep detection summary document data
  tar_target(
    dtc_summary_clean,
    .dtc_summary(raw_data = proc_dtc),
    format = "fst_dt"
  ),

 ## # determine times when when data are present
 tar_target(
    data_present,
    .data_present(dtc = proc_dtc, hst = hst, thresh_sec = (3600*12)),
    format = "fst_dt"
 ),

 # determine periods when glider is underwater and receivers are active for detecting tags
 tar_target(
   active_glider,
   .active_glider(glider_limno, thresh = 500),
   format = "fst_dt"
   ),

 # self-detection by 69 kHz tags (mobile tag and mobile rec)
 tar_target(
   dp_69mt_mr,
   .discrete_dtc_prob(gear_log = hst,
                      dta = proc_dtc,
                      bsize = 120,
                      glider_geo = glider_limno,
                      bounds = active_glider,
                      tags = c("A69-1604-32403", "A69-1604-32404"), # 32403 is L power, 32404 is H power
                      recs = "457003",
                      trial = 2),
   format = "fst_dt"
 ),

# 2022-11-01
# create detection probability dataset for stationary tags and mobile receiver (glider) for 69 kHz tags
 tar_target(
   dp_69st_mr,
   .discrete_dtc_prob(gear_log = hst,
                      dta = proc_dtc,
                      bsize = 120,
                      glider_geo = glider_limno,
                      bounds = active_glider,
                      tags = c("A69-1604-32405", "A69-1604-32401", "A69-1604-32402", "A69-1604-32406"),
                      recs = "457003",
                      trial = 2),
   format = "fst_dt"
 ),

 # 2022-11-01
 ## V13-H/L- stationary tags detected by mobile receiver.
 #Modeled detection range as function of receiver-tag distance and time using gam "te" function
 tar_target(
   mod_69st_mr,
   .GAMit(dtc = dp_69st_mr, limit_dist_m = 3000, k = 12),
   format = "rds"
 ),

 # 2022-11-01
 ## calculate model predictions and uncertainity for 69 kHz stationary tags and mobile receivers
 tar_target(
   pred_glider_69st_mr,
   predicted_dta(gam_dta = dp_69st_mr,
                 gam_mod = mod_69st_mr,
                 start_end_ts = c("2021-10-14", "2021-10-22"),
                 start_end_rt_dist = c(0,2000),
                 t_int = 86400, rt_dist_int = 100,
                 site = c("A69-1604-32401", "A69-1604-32402", "A69-1604-32405", "A69-1604-32406")),
                 
   format = "fst_dt"
 ),

 # 2022-11-01
 # detection range curves for 69 kHz stationary tags and mobile receivers
 tar_target(
   rng_crv_fig_69st_mr,
   .rng_curve(dta = pred_glider_69st_mr,
              dp_data = dp_69st_mr,
              output = "output/rng_crv_69_st_mr.png"),
   format = "file"
 )
 
   # tar_target(
#   mod_glider_69mt_mr,
#   .GAMit(dtc = dp_69mt_mr, limit_dist_m = 3000, k = 2),
#   format = "rds"
# ),

 ## tar_target(
 ## pred_glider_69mt_mr,
 ## .discrete_rng_crv(dtc = dp_69mt_mr, mod = mod_glider_69mt_mr, out_pth = "output/mod_69mt_mr", limit_dist_m = 3000, tle = "predicted detection probability- moble tag, mobile receiver"),
 ## format = "file"
 
)



  # link to decimated science data
  ## tar_target(
  ##   sci_data,
  ##   "data/glider_decimated/science",
  ##   format = "file"
  ## ),

  ## # link to decimated mission data
  ## tar_target(
  ##   mission_data,
  ##   "data/glider_decimated/mission",
  ##   #list.files("data/glider_decimated/mission", full.names = TRUE),
  ##   format = "file"
  ## ),

  ## # formats science data
  ## tar_target(
  ##   clean_sci,
  ##   clean(pth = sci_data, lat = m_gps_lat, lon = m_gps_lon),
  ##   format = "fst_dt"
  ## ),

  ## # formats mission data
  ## tar_target(
  ##   clean_mission,
  ##   clean(pth = mission_data, lat = m_gps_lat, lon = m_gps_lon),
  ##   format = "fst_dt"
  ## ),

  ## # combine mission data and science data into single object.  This represents decimated data collected by glider and reported via satellite uplink.
  ## tar_target(
  ##   glider_trk,
  ##   combine(x = clean_mission, y = clean_sci, id = trial_id),
  ##   format = "fst_dt"
  ## ),


  ## # match detections to location of glider.  Uses linear interpolation to add coordinates
  ## # from glider data to detection file.  OUtput is VEM file with lat/lon coordinates of glider.
  ## tar_target(
  ##   clean_vem_detections, 
  ##   infer_detection_locations(read_vem(vem_data)$detections, glider_trk),
  ##   format = "fst_dt"
  ## ),

  
    
  ## # combine stationary receiver detections (vrl) with detections that have interpolated postions from glider receiver (vem) (this is for the real-time data, not the full processed data)
  ## tar_target(
  ##   vrl_vem_combined,
  ##   rbind(dtc_geo, clean_vem_detections, fill = TRUE),
  ##   format = "fst_dt"
  ##   ),


  # detections from vrl and vem combined
  # "big joins"- joins receiver and tag location information with detection data
  # Creates a "to-from" type dataset where tag transmissions are linked to receivers and each tag and receiver mooring type is included.
  # contains data for both HBBS and Sag Bay trials
  ## tar_target( 
  ##   vrl_vem_combined_dtc,
  ##   get_instr_data(dta = vrl_vem_combined, hst_l = hst),
  ##   format = "fst_dt"
  ## ),

  

  
  ## tar_target(
  ##   vps,
  ##   "data/vps/synthetic.positions/all.csv",
  ##   format = "file"
  ## ),

  # impute missed detections on tags 
  ## tar_target(
  ##   imputed_transmissions,
  ##   impute_missing_transmissions(dtc = vrl_vem_combined_dtc, hst = hst, mrng_type = "stationary"),
  ##   format = "fst_dt"
  ## ),

  
 ## tar_target(
 ##   glider_dtc_transmissions,
 ##   glider_dtc(dtc = vrl_vem_combined_dtc, receiver_site = c("cormorant", "mary_lou"), tag_beeps = imputed_transmissions, glider_geo = glider_trk),
 ##   format = "fst_dt"
 ## ),

 ## tar_target(
 ##   glider_dtc_range,
 ##   glider_dtc_transmissions_time_filtered(dtc = glider_dtc_transmissions, inter = data_bounds),
 ##   format = "fst_dt"
 ## ),

 ## tar_target( # GAM model of HB trial
 ##   GAMit_HB,
 ##   .GAMit(dtc = glider_dtc_range, trial_run = 1, limit_dist_m = 2500),
 ##   format = "rds"
 ## ),

 ## tar_target( # GAM model of SB trial
 ##   GAMit_SB,
 ##   .GAMit(dtc = glider_dtc_range, trial_run = 2, limit_dist_m = 2500),
 ##   format = "rds"
 ## ),
 
 ## tar_target( # model output for HB
 ##   mod_output_HB,
 ##   .mod_output(mod = GAMit_HB, dtc = glider_dtc_range, out_pth = "output/predicted_dtc_prob_HB.pdf", limit_dist_m = 2500, text = "Hammond Bay", trial_run = 1),
 ##   format = "file"
 ## ),

 ## tar_target( # model output for SB
 ##   mod_output_SB,
 ##   .mod_output(mod = GAMit_SB, dtc = glider_dtc_range, out_pth = "output/predicted_dtc_prob_SB.pdf", limit_dist_m = 2500, text = "Saginaw Bay", trial_run = 2),
 ##   format = "file"
 ## ),

  # this shows when we have detection data in hand, based on detection of tags on glider (self-detection)
 ## tar_target( # receiver abacus for HB
 ##   receiver_abacus_HB,
 ##   receiver_abacus(dtc = vrl_vem_combined_dtc, hst = hst, trial = 1, out_pth = "output/rec_abacus_HB.pdf", main = "Hammond Bay receiver detections", bounds = data_present),
 ##   format = "file"
 ## ),

 ## # receiver abacus for SB- shows when detections are missing.  Saginaw Bay appears to be complete but HB is NOT
 ## tar_target( 
 ##   receiver_abacus_SB,
 ##   receiver_abacus(dtc = vrl_vem_combined_dtc, hst = hst, trial = 2, out_pth = "output/rec_abacus_SB.pdf", main = "Saginaw Bay receiver detections", bounds = data_present),
 ##   format = "file"
 ## ),

 ## # mission data operation periods
 ## tar_target(
 ##   mission_data_summary,
 ##   file_abacus(dtc = clean_mission, out_pth = "output/mission_data_operating.pdf"),
 ##   format = "file"
 ## ),

 ## # science data operation periods
 ## tar_target(
 ##   science_data_summary,
 ##   file_abacus(dtc = clean_sci, out_pth = "output/science_data_operating.pdf"),
 ##   format = "file"
 ## ),

 ## # data boundaries
 ## tar_target(
 ##   data_bounds,
 ##   .data_bounds(dtc = clean_mission),
 ##   format = "fst_dt"
 ## ),

 ## tar_target(
 ##   GAMit_tensor_SB,
 ##   .GAMit_tensor(dtc = glider_dtc_range, trial_run = 2, limit_dist_m = 3000),
 ##   format = "rds"
 ## ),

 ## tar_target(
 ##   GAMit_tensor_HB,
 ##   .GAMit_tensor(dtc = glider_dtc_range, trial_run = 1, limit_dist_m = 3000),
 ##   format = "rds"
 ## ),
 
## tar_target(
##   dtc_prob_dist_HB,
##   .dtc_prob_dist(mod = GAMit_tensor_HB, dtc = glider_dtc_range, out_pth = "output/dtc_prob_rt_dist_HB.pdf", limit_dist_m = 2500, trial_run=1, bounds = data_bounds),
##   format = "file"
## ),

## tar_target(
##   dtc_prob_dist_SB,
##   .dtc_prob_dist(mod = GAMit_tensor_SB, dtc = glider_dtc_range, out_pth = "output/dtc_prob_rt_dist_SB.pdf", limit_dist_m = 2500, trial_run=2, bounds = data_bounds),
##   format = "file"
## ),

## tar_target(
##   dtc_prob_time_HB,
##   .dtc_prob_time(mod = GAMit_tensor_HB, dtc = glider_dtc_range, out_pth = "output/dtc_prob_500m_HB.pdf", limit_dist_m = 2500, trial_run=1, bounds = data_bounds),
##   format = "file"
## ), 

## tar_target(
##   dtc_prob_time_SB,
##   .dtc_prob_time(mod = GAMit_tensor_SB, dtc = glider_dtc_range, out_pth = "output/dtc_prob_500m_SB.pdf", limit_dist_m = 2500, trial_run=2, bounds = data_bounds),
##   format = "file"
## ),




 
# this is good below!!!!

## ####################################3############################

## # V13-H/L- stationary tags detected by mobile receiver on glider
## tar_target(
##   discrete_gam_V13_static_tag_mobile_glider,
##   .discrete_gam(dtc = discrete_dtc_prob_V13_static_tag_mobile_glider),
##   format = "rds"
## ),


## # V13-H/L- stationary tags detected by mobile receiver on glider
## tar_target(
##   discrete_model_predictions_V13_static_tag_mobile_glider,
##   .discrete_rng_crv_by(dtc = discrete_dtc_prob_V13_static_tag_mobile_glider,
##                     mod = discrete_gam_V13_static_tag_mobile_glider,
##                     limit_dist_m = 3000,
##                     bounds = data_bounds,
##                     out_pth = "output/static_tag_mobile_rec_69.png",
##                     tle = "mobile receiver, V13 stationary tags"
##                     ),
##   format = "file" ),
## ####################################################################3
## # V13-H/L- stationary recs detected mobile tag on glider
##  tar_target(
##    discrete_dtc_prob_V13_static_rec_mobile_tag,
##    .discrete_dtc_prob(gear_log = hst,
##                       dta = vrl_vem_combined_dtc,
##                       bsize = 120,
##                       glider_geo = glider_trk,
##                       bounds = data_bounds,
##                       tags = c("A69-1604-32403", "A69-1604-32404"),
##                       recs = c("480026", "480027", "483814", "483815", "483829", "483846", "483868", "483871", "483884", "483888"),
##                       trial = 2),
##    format = "fst_dt"
##  ),

## # V13-H/L- stationary recs detected mobile tag on glider
## tar_target(
##   discrete_gam_V13_static_rec_mobile_tag,
##   .discrete_gam(dtc = discrete_dtc_prob_V13_static_rec_mobile_tag),
##   format = "rds"
## ),


## # V13-H/L- stationary recs detected mobile tag on glider
## tar_target(
##   discrete_model_predictions_V13_static_rec_mobile_tag,
##   .discrete_rng_crv_by(dtc =  discrete_dtc_prob_V13_static_rec_mobile_tag,
##                     mod = discrete_gam_V13_static_rec_mobile_tag,
##                     limit_dist_m = 3000,
##                     bounds = data_bounds,
##                     tle = "stationary receiver, V13 mobile tag",
##                     out_pth = "output/static_rec_mobile_tag_69.png"
##                     ),
##   format = "file" ),
## ######################################################################
## # V9- stationary tags detected by mobile receiver on glider
##  tar_target(
##    discrete_dtc_prob_V9_static_tag_mobile_glider,
##    .discrete_dtc_prob(gear_log = hst,
##                       dta = vrl_vem_combined_dtc,
##                       bsize = 60,
##                       glider_geo = glider_trk,
##                       bounds = data_bounds,
##                       tags = c("A180-1702-61650", "A180-1702-61651"),
##                       recs = "458000",
##                       trial = 2),
##    format = "fst_dt"
##  ),

## # V9- stationary tags detected by mobile receiver on glider
## # did not converge? need to bump up iterations in gam process...
## tar_target(
##   discrete_gam_V9_static_tag_mobile_glider,
##   .discrete_gam(dtc = discrete_dtc_prob_V9_static_tag_mobile_glider),
##   format = "rds"
## ),


## # V9- stationary tags detected by mobile receiver on glider
## tar_target(
##   discrete_model_predictions_V9_static_tag_mobile_glider,
##   .discrete_rng_crv_by(dtc = discrete_dtc_prob_V9_static_tag_mobile_glider,
##                     mod = discrete_gam_V9_static_tag_mobile_glider,
##                     limit_dist_m = 800,
##                     bounds = data_bounds,
##                     out_pth = "output/static_tag_mobile_rec_180.png",
##                     tle = "mobile receiver, V9-180 stationary tags"
##                     ),
##   format = "file" ),
## ####################################################################3
## # V9- stationary recs detect mobile tag on glider
##  tar_target(
##    discrete_dtc_prob_V9_static_rec_mobile_tag,
##    .discrete_dtc_prob(gear_log = hst,
##                       dta = vrl_vem_combined_dtc,
##                       bsize = 60,
##                       glider_geo = glider_trk,
##                       bounds = data_bounds,
##                       tags = c("A180-1702-61652"),
##                       recs = c("300813", "300815"),
##                       trial = 2),
##    format = "fst_dt"
##  ),

## # V9- stationary recs detect mobile tag on glider
## tar_target(
##   discrete_gam_V9_static_rec_mobile_tag,
##   .discrete_gam(dtc =  discrete_dtc_prob_V9_static_rec_mobile_tag),
##   format = "rds"
## ),


## # V9- stationary recs detect mobile tag on glider
## tar_target(
##   discrete_model_predictions_V9_static_rec_mobile_tag,
##   .discrete_rng_crv_by(dtc =  discrete_dtc_prob_V9_static_rec_mobile_tag,
##                     mod = discrete_gam_V9_static_rec_mobile_tag,
##                     limit_dist_m = 800,
##                     bounds = data_bounds,
##                     out_pth = "output/static_rec_mobile_tag_180.png",
##                     tle = "stationary receiver, V9-180 mobile tag"
##                     ),
##   format = "file" ),

## #######################################################################
## # V9- 180 kHz self-detections
##  tar_target(
##    discrete_self_dtc_180,
##    .discrete_dtc_prob(gear_log = hst,
##                       dta = vrl_vem_combined_dtc,
##                       bsize = 300,
##                       glider_geo = glider_trk,
##                       bounds = data_bounds,
##                       tags = c("A180-1702-61652"),
##                       recs = c("458000"),
##                       trial = 2),
##    format = "fst_dt"
##  ),

## tar_target(
##   self_dtc_plot,
##   self_dtc(dta = discrete_self_dtc_180, out_pth = "output/self_dtc_180.pdf"),
##   format = "file")
## )



## ###########################3

