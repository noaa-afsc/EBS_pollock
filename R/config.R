library(RTMB)
library(TMBhelper)
# Read in last year's estimates for comparisons----------
library(ebswp)
pm<- read_rep(here::here("runs", "rtmb", "pm.rep")) # Read in the report file)

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
source(here::here("R", "utilities.R"))
source(here::here("R", "Likelihoods.R"))
source(here::here("R", "model_funs.R"))
# source(here::here("R", "util_claude.R"))
#--Get the parameters (from ADMB converged model!)------------
parms <- read_pars(here::here("runs", "rtmb", "pm.par"))
#names(dat)
data <- Get_Data()
map_obj <- create_map_from_zeros(parms)
map_obj <- c(map_obj, create_map_from_par(parms,  parms,   
                                          patterns = c(
                                            "log_q_std_area", "bt_slope", "sigr",
                                            "sel_coffs_bts", "sel_dif1_fsh", "sel_a501_fsh",
                                            "sel_trm1_fsh",
                                            "sel_dif2_fsh",
                                            "sel_trm2_fsh",
                                            "sel_dif1_fsh_dev",
                                            "sel_a501_fsh_dev",
                                            "sel_trm2_fsh_dev",
                                            "log_K","d_scale", "L1", "L2", #Secondary
                                            "coh_eff", "yr_eff", #Secondary
                                            "log_F_devs", #Primary
                                             "sel_coffs_fsh", #Primary
                                            "sel_devs_fsh", #Primary
                                            "sel_slp_bts_dev", #Primary
                                            "sel_a50_bts_dev", #Primary
                                            "sel_age_one_bts_dev", #Primary
                                            "sel_a50_bts_dev", #Primary
                                            "M_dev",
                                            "log_a_II",
                                            "log_b_II",
                                            "log_a_II_vec",
                                            "log_b_II_vec",
                                            "log_rho",
                                            "log_resid_M",
                                            "log_alpha"
                                          ), exclude_patterns = "log_avg_F")
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

#vars$data <- lapply(vars$data, fix_structure)
#vars$parameters <- lapply(vars$parameters, fix_structure)
#vars$data <- vars$data[sapply(vars$data, is.numeric)]
source(here::here("R/rpm.R"))
