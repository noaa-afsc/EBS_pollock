
# =============================================================================
# PARAMETERS (Global variables instead of parameter list)
# =============================================================================

log_avgrec <- 0
log_avginit <- 0
log_avg_F <- 0
natmort_phi <- 0
natmort <- rep(0.2, nages)
base_natmort <- rep(0.2, nages)
log_q_bts <- 0
log_q_std_area <- 0
bt_slope <- 0
log_q_ats <- 0
log_Rzero <- 0
steepness <- 1.0
log_q_cpue <- 0
log_q_avo <- 0
log_initdevs <- rep(0, nages - 1)
log_rec_devs <- rep(0, nyears)
larv_rec_devs <- matrix(0, 11, 11)
alpha <- 1
beta <- 1
Rzero <- 1e6
YC_2018 <- 0
q_all <- 0
log_F_devs <- rep(0, nyears)
sigr <- 1.0

# Selectivity parameters
sel_devs_fsh <- matrix(0, 10, 10)  # Dimensions will be updated
sel_devs_bts <- matrix(0, 10, 10)
sel_devs_ats <- matrix(0, 10, 10)
sel_coffs_fsh <- rep(0, 10)
sel_coffs_bts <- rep(0, 10)
sel_coffs_ats <- rep(0, 10)

# Weight and selectivity variables
wt_fut <- rep(1.0, nages)
sel_slp_bts <- 1.0
sel_a50_bts <- 4.0
sel_age_one_bts <- 1.0
sel_slp_bts_dev <- rep(0, nyears)
sel_a50_bts_dev <- rep(0, nyears)
sel_age_one_bts_dev <- rep(0, nyears)

sel_dif1_fsh <- 1.0
sel_a501_fsh <- 3.0
sel_trm2_fsh <- 0.5
sel_dif2_fsh <- 1.0
sel_dif1_fsh_dev <- rep(0, nyears)
sel_a501_fsh_dev <- rep(0, nyears)
sel_trm2_fsh_dev <- rep(0, nyears)

SPR_ABC <- 0.4
endyr_N <- rep(1e6, nages)
B_Bnofsh <- 1
Bzero <- 1
Percent_Bzero <- 1
Percent_B100 <- 1

# Additional parameters
L1 <- 30
L2 <- 60
log_alpha <- -0.5
log_K <- 0.1

# =============================================================================
# DERIVED QUANTITIES (Global arrays)
# =============================================================================

# Population arrays
natage <- matrix(0, nyears, nages)
meannatage <- matrix(0, nyears, nages)
catage <- matrix(0, nyears, nages)
F <- matrix(0, nyears, nages)
Fmort <- rep(0, nyears)
Z <- matrix(0, nyears, nages)
S <- matrix(0, nyears, nages)
SSB <- rep(0, nyears)
pred_rec <- rep(0, nyears)
rec_epsilons <- rep(0, nyears)

# Selectivity arrays
log_sel_fsh <- matrix(0, nyears, nages)
log_sel_bts <- matrix(0, nyears, nages)
log_sel_ats <- matrix(0, nyears, nages)
sel_fsh <- matrix(0, nyears, nages)
avgsel_fsh <- rep(0, nages)
avgsel_bts <- rep(0, nages)
avgsel_ats <- rep(0, nages)

# Weight arrays
wt_fsh <- matrix(1, nyears, nages)
wt_bts <- matrix(1, nyears, nages)
wt_ats <- matrix(1, nyears, nages)
wt_ssb <- matrix(1, nyears, nages)
wt_avo <- matrix(1, nyears, nages)
wt_mn <- rep(1, nages)
wt_sigma <- rep(0.1, nages)

# Maturity
p_mature <- rep(1, nages)
yrfrac <- 0.5

# Predictions
pred_catch <- rep(0, nyears)
pred_cpue <- rep(0, 10)  # Will be resized based on data
pred_avo <- rep(0, 10)
pred_cope <- rep(0, 10)

# Age compositions
eac_fsh <- matrix(0, 10, nages)
eac_bts <- matrix(0, 10, nages)
eac_ats <- matrix(0, 10, nages)
oac_fsh <- matrix(0, 10, nages)
oac_bts <- matrix(0, 10, nages)
oac_ats <- matrix(0, 10, nages)

# Survey quantities
eb_bts <- rep(0, 10)
et_bts <- rep(0, 10)
eb_ats <- rep(0, 10)
et_ats <- rep(0, 10)
ea1_ats <- rep(0, 10)

# Observations (will be filled from data)
obs_catch <- rep(0, nyears)
ob_bts <- rep(0, 10)
ot_bts <- rep(0, 10)
ob_ats <- rep(0, 10)
ot_ats <- rep(0, 10)
oa1_ats <- rep(0, 10)
obs_cpue <- rep(0, 10)
obs_avo <- rep(0, 10)
obs_cope <- rep(0, 10)

# Variance and sample size arrays
var_ob_bts <- rep(1, 10)
var_ob_ats <- rep(1, 10)
lvar_ats <- rep(1, 10)
lvarb_ats <- rep(1, 10)
lse_ats <- rep(1, 10)
lseb_ats <- rep(1, 10)
sam_fsh <- rep(100, 10)
sam_bts <- rep(100, 10)
sam_ats <- rep(100, 10)

# Year vectors
yrs_fsh_data <- 1970:2020
yrs_bts_data <- 1982:2020
yrs_ats_data <- 1996:2020
yrs_cpue <- 1978:2020
yrs_avo <- 2000:2020
yrs_cope <- 2001:2020

# Dimensions based on data
n_fsh_r <- length(yrs_fsh_data)
n_bts_r <- length(yrs_bts_data)
n_ats_r <- length(yrs_ats_data)
n_ats_ac_r <- n_ats_r
n_cpue <- length(yrs_cpue)
n_avo_r <- length(yrs_avo)
n_cope <- length(yrs_cope)

# Age range parameters
mina_bts <- 3
mina_ats <- 3
nlbins <- 10

# Error matrices (if using age error)
age_err <- list()  # Will be populated if use_age_err > 0

# Likelihood components
NLL <- rep(0, 20)
age_like <- rep(0, 3)
age_like_offset <- rep(0, 3)
len_like <- 0
len_like_offset <- 0
surv_like <- rep(0, 3)
sel_like <- rep(0, 10)
sel_like_dev <- rep(0, 10)
rec_like <- rep(0, nyears)

# Covariance matrices
inv_bts_cov <- diag(n_bts_r)
cov_matrix <- NULL

# Phases and flags
last_phase <- FALSE
current_phase <- 1
do_srrdevs <- TRUE
do_pred <- 0
do_check <- FALSE
ignore_last_ats_age1 <- FALSE

# =============================================================================
# STOCK-RECRUITMENT FUNCTIONS (Converted to direct calculations)
# =============================================================================

# Stock-recruitment calculation (scalar)
SRecruit <- function(Stmp) {
  if (SrType == 1) {
    return((Stmp / phizero) * exp(alpha * (1 - Stmp / Bzero)))
  } else if (SrType == 2) {
    return(Stmp / (alpha + beta * Stmp))
  } else if (SrType == 3) {
    return(exp(log_avgrec))
  } else if (SrType == 4) {
    return(Stmp * exp(alpha - beta * Stmp))
  } else {
    stop("Unsupported SrType")
  }
}

# =============================================================================
# COMPUTATION ROUTINES (Simplified without function wrappers)
# =============================================================================

# Calculate Bzero and related quantities
Rzero <<- exp(log_Rzero)
Ntmp <- numeric(nages)
survtmp <- exp(-natmort)

Ntmp[1] <- Rzero
for (j in 1:(nages - 1)) {
  Ntmp[j + 1] <- Ntmp[j] * survtmp[j]
}
Ntmp[nages] <- Ntmp[nages] / (1 - survtmp[nages])
Ntmp <- Ntmp * exp(yrfrac * log(survtmp))

phizero <- sum(wt_ssb[endyr_r, ] * p_mature * Ntmp) / Rzero

# Stock-recruit parameters
if (SrType == 1) {
  alpha <<- log(-4 * steepness / (steepness - 1))
} else if (SrType == 2) {
  alpha <<- Bzero * (1 - (steepness - 0.2) / (0.8 * steepness)) / Rzero
  beta <<- (5 * steepness - 1) / (4 * steepness * Rzero)
} else if (SrType == 4) {
  beta <<- log(5 * steepness) / (0.8 * Bzero)
  alpha <<- log(Rzero / Bzero) + beta * Bzero
}

# Initialize catch future scenarios
Cat_Fut[1] <- next_yrs_catch
for (i in 2:10) {
  Cat_Fut[i] <- Cat_Fut[i - 1] * 0.9
}

# Decision table catch scenarios  
dec_tab_catch <- c(
  10,
  0.25 * obs_catch[endyr_r],
  0.50 * obs_catch[endyr_r], 
  0.75 * obs_catch[endyr_r],
  1.00 * obs_catch[endyr_r],
  1.25 * obs_catch[endyr_r],
  1.50 * obs_catch[endyr_r],
  2.00 * obs_catch[endyr_r]
)

# =============================================================================
# SELECTIVITY SHAPE FUNCTIONS (Direct implementations)
# =============================================================================

# Logistic selectivity
logistic_sel <- function(age_vec, slp, a50) {
  return(-log(1 + exp(-slp * (age_vec - a50))))
}

# Double logistic selectivity  
double_logistic_sel <- function(age_vec, asc, inf1, dsc, inf2) {
  sel <- 1 / (1 + exp(-asc * (age_vec - inf1))) *
    (1 - 1 / (1 + exp(-dsc * (age_vec - inf2))))
  return(log(sel) - max(log(sel)))
}

# Coefficient-based selectivity
coeff_sel <- function(coffs, nsel, nages_vec) {
  log_sel <- rep(0, length(nages_vec))
  log_sel[1:nsel] <- coffs
  if (length(nages_vec) > nsel) {
    log_sel[(nsel + 1):length(nages_vec)] <- coffs[nsel]
  }
  return(log_sel - mean(log_sel))
}

# =============================================================================
# INITIALIZATION MESSAGE
# =============================================================================

cat("RTMB global variables initialized successfully\n")
cat("Key dimensions:\n")
cat("  Years:", styr, "to", endyr, "(", nyears, "years )\n") 
cat("  Ages: 1 to", nages, "\n")
cat("  Retrospective year:", retroYr, "\n")
cat("  Estimation end year:", endyr_r, "\n")

f <- function(parms) {
  getAll(ChickWeight, parms, warn=FALSE)
  ## Optional (enables extra RTMB features)
  #weight <- OBS(weight)
  ## Initialize joint negative log likelihood
  nll <- 0
  ## Random slopes
  nll <- nll - sum(dnorm(a, mean=mua, sd=sda, log=TRUE))
  ## Random intercepts
  nll <- nll - sum(dnorm(b, mean=mub, sd=sdb, log=TRUE))
  ## Data
  predWeight <- a[Chick] * Time + b[Chick]
  nll <- nll - sum(dnorm(weight, predWeight, sd=sdeps, log=TRUE))
  ## Get predicted weight uncertainties
  ADREPORT(predWeight)
  ## Return
  nll
}

parameters <- list(
  mua=0,          ## Mean slope
  sda=1,          ## Std of slopes
  mub=0,          ## Mean intercept
  sdb=1,          ## Std of intercepts
  sdeps=1,        ## Residual Std
  a=rep(0, 50),   ## Random slope by chick
  b=rep(0, 50)    ## Random intercept by chick
)
obj <- MakeADFun(f, parameters, random=c("a", "b"))
opt <- nlminb(obj$par, obj$fn, obj$gr)
sdr <- sdreport(obj)
sdr
as.list(sdr, "Est") ## parameter estimates
as.list(sdr, "Std") ## parameter uncertaintieso
