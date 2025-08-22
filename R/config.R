rm(list=ls())
source(here::here("R", "utilities.R"))
source(here::here("R", "model_funs.R"))
source(here::here("R", "rpm.R"))
# source(here::here("R", "util_claude.R"))
library(RTMB)
library(TMBhelper)
library(patchwork)
library(tidyverse)
# Read in last year's estimates for comparisons----------
library(ebswp)
pm<- read_rep(here::here("runs", "rtmb", "pm.rep")) # Read in the report file)
# pm$sam_bts

# Clean up some of the odd arrays from ADMB
pm$phat_ats <- pm$phat_ats[,2:16]
pm$phat_bts <- pm$phat_bts[,2:16]
pm$sel_ats <- pm$sel_ats[31:61,]
pm$sel_bts <- pm$sel_bts[19:61,]
pm$phat_fsh <- pm$phat_fsh[,2:16]
pm$bts_like <- pm$surv_like[1]
pm$ats_like <- pm$surv_like[2]
pm$ats_age1_like <- pm$surv_like[3]
pm$SSB <- pm$SSB[,2]
#--Get the parameters (from ADMB converged model!)------------
parms <- read_pars(here::here("runs", "rtmb", "pm.par"))
names(parms)
data <- Get_Data()
data$sam_bts <- floor(pm$sam_bts)
# data$sam_fsh <- as.integer(pm$sam_fsh)
# data$styr
#map_obj <- create_map_from_zeros(parms)
# "log_avgrec" "log_avginit"  "log_avg_F"  "log_q_ats"  "log_Rzero"  
# "steepness"  "log_q_cpue"  "log_q_avo"  "log_initdevs"        "log_rec_devs"       
# "log_F_devs"  "sel_devs_fsh"        "sel_devs_ats"        "sel_coffs_fsh"       "sel_coffs_ats"      
# "sel_slp_bts"  "sel_a50_bts"  "sel_age_one_bts"     "sel_slp_bts_dev"     "sel_a50_bts_dev"    
# "sel_age_one_bts_dev" "rec_dev_future"      "L1"  "L2"  "log_K"   "d_scale"  
map_obj <- c(create_map_from_par(parms,  parms,   
                                          exact_names = c(
                                             #"sel_devs_fsh", #Primary
                                             #"log_rec_devs",
                                             #"sel_a50_bts_dev", #Primary
                                             #"sel_slp_bts_dev", #Primary
                                             #"sel_age_one_bts_dev", #Primary
                                             #"log_F_devs", #Primary
                                             # "sel_coffs_fsh", #Primary
                                             # "log_initdevs",
                                              "log_Rzero",
                                              "steepness",
                                             # "log_q_ats",
                                             # "log_q_cpue",
                                             # "log_q_avo",
                                             # "sel_devs_ats",
                                             # "sel_coffs_ats",
                                             # -- these are out of play from here down
                                            "log_K","d_scale", "L1", "L2", #Secondary
                                            "coh_eff", "yr_eff", #Secondary
                                             "log_avginit",
                                             "log_avgrec",
                                             "log_avg_F", #Keep off
                                            "log_q_bts",
                                             "rec_dev_future",
                                            "natmort_phi",
                                            "larv_rec_devs",
                                            "sel_devs_bts",
                                            "sel_slp_bts",
                                            "sel_a50_bts",
                                            "sel_age_one_bts",
                                            "sel_trm2_fsh",
                                            "resid_temp_x1",
                                            "resid_temp_x2",
                                            "log_q_std_area", "bt_slope", "sigr",
                                            "sel_coffs_bts", "sel_dif1_fsh", "sel_a501_fsh",
                                            "sel_trm1_fsh",
                                            "sel_dif2_fsh",
                                            "sel_trm2_fsh",
                                            "sel_dif1_fsh_dev",
                                            "sel_a501_fsh_dev",
                                            "sel_trm2_fsh_dev",
                                            "M_dev",
                                            "log_a_II",
                                            "log_b_II",
                                            "log_a_II_vec",
                                            "log_b_II_vec",
                                            "log_rho",
                                            "log_resid_M",
                                            "log_alpha"
                                          ), exclude_patterns = "xxx")
)
# Convert any single-element lists to scalars
fix_structure <- function(x) {
  if(is.list(x) && length(x) == 1 && is.numeric(x[[1]])) {
    return(x[[1]])
  }
  if(is.list(x) && all(sapply(x, is.numeric))) {
    return(unlist(x))
  }
  return(x)
}
#parms <- fix_structure(parms)

obj <- MakeADFun(rpm, parms, map = map_obj)

#vars$data <- lapply(vars$data, fix_structure)
#vars$parameters <- lapply(vars$parameters, fix_structure)
#vars$data <- vars$data[sapply(vars$data, is.numeric)]
source(here::here("R/rpm.R"))
