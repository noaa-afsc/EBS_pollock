library(RTMB)
# Read in last year's estimates for comparisons----------
library(ebswp)
pm<- read_rep(here::here("runs", "lastyr", "pm.rep")) # Read in the report file)

# Clean up some of the odd arrays from ADMB
pm$phat_ats <- pm$phat_ats[,2:16]
pm$phat_bts <- pm$phat_bts[,2:16]
pm$sel_ats <- pm$sel_ats[31:61,]
pm$sel_bts <- pm$sel_bts[19:61,]
pm$phat_fsh <- pm$phat_fsh[,2:16]
pm$SSB <- pm$SSB[,2]
source(here::here("R", "utilities.R"))
source(here::here("R", "Likelihoods.R"))
source(here::here("R", "model_funs.R"))
# source(here::here("R", "util_claude.R"))
#--Get the parameters (from ADMB converged model!)------------
parms <- read_pars(here::here("Rtmb", "pm.par"))
dat <- build_model_inputs(here::here("Rtmb", "input.dat"), 
                          fn = here::here("Rtmb", "rpm.dat"))
dat$spawnmo <- 4. # scalar, month of spawning
dat$yrfrac <- (dat$spawnmo - 1.) / 12 # scalar, fraction of year for spawning
dat$yrfrac <- (dat$spawnmo - 1.) / 12 # scalar, fraction of year for spawning
dat$inv_bts_cov <- solve(dat$cov_matrix)  # inverse covariance matrix
dat$obs_cpue_var <- dat$obs_cpue_std^2 # observation variance for CPUE
dat$obs_avo_var <- dat$ob_avo_std^2 # observation variance for AVO
dat$nyrs <- dat$endyr_r-dat$styr+1
dat$return_nll_only <- 1 # Flag to only return nll
#names(dat)
vars <- preliminary_calcs(data = dat, parameters = parms)
# prel_vars$data;prel_vars$parameters
cmb <- function(f, d) function(p) f(p, d)
# prel_vars$data$files
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

vars$data <- lapply(vars$data, fix_structure)
vars$parameters <- lapply(vars$parameters, fix_structure)
vars$data <- vars$data[sapply(vars$data, is.numeric)]
source(here::here("R/rpm.R"))
