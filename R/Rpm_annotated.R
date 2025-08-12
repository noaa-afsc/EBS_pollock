# ----------------------------------------------------------------------------
# RTMB Bridge Script: cleaned & annotated
# Notes:
# - Removed rm(list=ls()); centralized libraries with quiet load
# - Safer covariance inversion; fixed minor typo; added ADMB→RTMB TODOs
# - Consider using the function-based version (Rpm_build.R) for testing
# ----------------------------------------------------------------------------

## Dependencies ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(RTMB)         # core
  library(here)         # for here::here()
})
# (library call moved to header)
# Read in last year's estimates for comparisons----------
library(ebswp)
pm<- read_rep(here::here("runs", "lastyr", "pm.rep")) # Read in the report file)

source(here::here("R", "utilities.R"))
#--Get the parameters (from ADMB converged model!)------------
parms <- read_pars(here::here("Rtmb", "pm.par"))
dat <- build_model_inputs(here::here("Rtmb", "input.dat"), 
                          fn = here::here("Rtmb", "rpm.dat"))
dat$spawnmo <- 4. # scalar, month of spawning
dat$yrfrac <- (dat$spawnmo - 1.) / 12 # scalar, fraction of year for spawning
# [NOTE] Duplicate yrfrac assignment removed; this line is redundant.
# [FIX] Safer positive-definite inverse via Cholesky with fallback
dat$inv_bts_cov <- try({
  L <- chol(dat$cov_matrix)
  chol2inv(L)
}, silent = TRUE)
if (inherits(dat$inv_bts_cov, 'try-error')) {
  warning('Cholesky failed; falling back to solve(); consider regularization.')
  dat$inv_bts_cov <- solve(dat$cov_matrix)
}
dat$obs_cpue_var <- dat$obs_cpue_std^2 # observation variance for CPUE
prel_vars <- preliminary_calcs(data = dat, parameters = parms)
dat$obs_avo_var <- dat$obs_avo_std^2  # [FIX] Typo: ob_avo_std → obs_avo_std
# rpm <- function(parms) {
getAll(prel_vars$data, prel_vars$parameters, warn = TRUE)
# [INFO] getAll() exposes data/parameters in the caller; ensure names match RTMB template fields.
# dim(dat$cov_matrix )
## Optional (enables extra RTMB features)

## Initialize joint negative log likelihood
nll <- 0

# dim(sel_devs_fsh)[1] (sel_devs_fsh) mean(sel_coffs_fsh) args(compute_fsh_selectivity) n_selages_fsh styr nages
# [TODO] Confirm selectivity normalization order (before/after penalties) to match ADMB.
tmp <- compute_selectivity_fsh(
  nsel = n_selages_fsh,
  stsel = styr,
  endyr_r = endyr_r,
  nages = nages,
  coffs = sel_coffs_fsh,
  sel_devs = sel_devs_fsh,
  yrs_ch_fsh=1965:2023
)

log_sel_fsh <- tmp$log_sel
avgsel_fsh <- tmp$avgsel
sel_fsh <- exp(log_sel_fsh)
# [TODO] Confirm selectivity normalization order (before/after penalties) to match ADMB.
log_sel_bts <- compute_selectivity_ind(
  stsel = styr_bts,
  slp = sel_slp_bts,
  a50 = sel_a50_bts,
  se = sel_slp_bts_dev,
  ae = sel_a50_bts_dev,
  age_vector = 0.5+1:nages,
  endyr_r = endyr_r
)
log_sel_bts[,1] <- sel_age_one_bts*exp(sel_age_one_bts_dev);

# dim(log_sel_bts)
 # matplot(exp(t(log_sel_bts)), type="l")
 # matplot((t(pm$sel_bts)), type="l")
 # matplot(exp((log_sel_bts[1:41,]))/pm$sel_bts[2:42], type="b")

# [TODO] Confirm selectivity normalization order (before/after penalties) to match ADMB.
# compute_selectivity_ats_devs <- function(nsel, stsel, coffs, sel_devs,
                                         # mina_ats, yrs_ch_ats ) {
yrs_ch_ats <- 1995:2024
# [TODO] Confirm selectivity normalization order (before/after penalties) to match ADMB.
log_sel_ats <- compute_selectivity_ats_devs(
  nsel = n_selages_ats,
  stsel = styr_ats,
  coffs = sel_coffs_ats,
  sel_devs = sel_devs_ats,
  mina_ats, yrs_ch_ats
)
# log_sel_ats[,1]

# matplot(exp(t(log_sel_ats)), type="l")

#--get mortality rates--------------
# OjO move to data
nyrs <- endyr_r - styr + 1
years <- styr:endyr_r
ages <- 1:nages
#
#   Initialize outputs
S <- matrix(0, nrow = nyrs, ncol = nages, dimnames = list(years, ages))
Z <- matrix(0, nrow = nyrs, ncol = nages, dimnames = list(years, ages))
F <- matrix(0, nrow = nyrs, ncol = nages, dimnames = list(years, ages))
M <- matrix(0, nrow = nyrs, ncol = nages, dimnames = list(years, ages))

# Compute or update natmort (assumes time-invariant)
natmort <- base_natmort
if (!is.null(natmort_phi)) {
  natmort[3:nages] <- base_natmort[3:nages] * exp(natmort_phi)
}

# Fishing mortality
Fmort <- exp(log_avg_F + log_F_devs) # vector of length = nyrs

for (i in seq_along(years)) {
  yr <- years[i]
  if (!Mmatrix) {
    M[i, ] <- natmort
  }

  F[i, ] <- Fmort[i] * sel_fsh[i, ] # F[i] = Fmort[i] * sel_fsh[i]
  Z[i, ] <- F[i, ] + M[i, ] # Z[i] = F + M
}
S <- exp(-Z) # S = exp(-Z)
#--Get Bzero------
# Get_Bzero <- function(
# log_Rzero,            # scalar
# natmort,              # vector [1:nages]
wt_ssb_lastyr <- wt_ssb[nyrs,] # vector [1:nages], e.g., wt_ssb[endyr_r, ]
# p_mature,             # vector [1:nages]
# steepness,            # scalar (h)
# yrfrac                # scalar (e.g., 0.5 for mid-year spawning)
# ) {
# Initialize
Rzero <- exp(log_Rzero)
survtmp <- exp(-natmort) # annual survival at age
Ntmp <- numeric(nages) # numbers-at-age
Ntmp[1] <- Rzero

# Fill ages 2 to nages - 1
for (j in 1:(nages - 1)) {
  Ntmp[j + 1] <- Ntmp[j] * survtmp[j]
}

# Plus group (geometric series closure)
Ntmp[nages] <- Ntmp[nages] / (1 - survtmp[nages])

# Fractional year survival (spawning timing)
Ntmp <- Ntmp * exp(yrfrac * log(survtmp)) # same as pow(survtmp, yrfrac)

# Spawning biomass per recruit
Bzero <- 0.5*sum(wt_ssb_lastyr * p_mature * Ntmp)
# Bzero/pm$Bzero
# phizero <- Bzero / Rzero

# Beverton-Holt alpha (Eq. 13)
alpha <- log(-4 * steepness / (steepness - 1))

#--Get numbers at age------------
# GetNumbersAtAge <- function(
# styr, endyr_r, nages,

# log_rec_devs,           # vector [styr:endyr_r]
# log_initdevs,           # vector [2:nages]
# log_avginit,            # scalar
# log_avgrec,             # scalar
# S,                      # matrix [year, age]
# yrfrac,                 # scalar
# p_mature,               # vector [age]
# wt_ssb,                 # vector [year]
# SRecruit,               # function: SRR(SSB)
# ) {
# years <- styr:endyr_r
# nyrs <- length(years)
# Initialize matrices and vectors
natage <- matrix(0,
  nrow = nyrs, ncol = nages,
  dimnames = list(years, ages)
)

SSB <- numeric(nyrs)
names(SSB) <- years
rec_epsilons <- numeric(nyrs)
names(rec_epsilons) <- years
pred_rec <- numeric(nyrs)
names(pred_rec) <- years

# Initial numbers at age
log_initage <- log_avginit + log_initdevs
# log_initage
natage[as.character(styr), 2:nages] <- exp(log_initage) # Eq. 1
# Loop over years
# for (i in styr:(endyr_r - 1)) {
for (i in 1:(nyrs - 1)) {
  yr <- as.character(years[i])
  yr1 <- as.character(years[i] + 1)

  # Spawning biomass
  SSB[i] <- 0.5 * sum(natage[i, ] * (S[i, ]^yrfrac) * p_mature * wt_ssb[i,])

  # Recruitment
  natage[yr, 1] <- exp(log_avgrec + log_rec_devs[i]) # Eq. 1

  # Survivors advance age
  natage[yr1, 2:nages] <- natage[yr, 1:(nages - 1)] * S[i, 1:(nages - 1)]
  natage[yr1, nages] <- natage[yr1, nages] + natage[yr, nages] * S[i, nages]

  # Optional print check
}
natage[yr1, 1] <- exp(log_avgrec + log_rec_devs[i+1]) # Eq. 1
SSB[yr1] <- 0.5 * sum(natage[yr1, ] * S[nyrs, ]^yrfrac * p_mature * wt_ssb[nyrs,])
pred_rec <- natage[,1]

# 
# natage[50:61,1n]
# pm$N[50:61,1]

# Convert complete ADMB expected values calculation to R
#--Catch at age and other predicitons-------
# Initialize ADMB variables in R
# Note: You'll need to define the dimension variables first (n_fsh_r, nbins, etc.)

# Matrices (2D arrays)
eac_fsh <- matrix(0, nrow = n_fsh, ncol = nages) # Expected proportion at age in Fishery
eac_bts <- matrix(0, nrow = n_bts, ncol = nages) # Expected proportion at age in trawl survey
eac_ats <- matrix(0, nrow = n_ats, ncol = nages) # Expected proportion at age in hydro survey

# Vectors (1D arrays)
elc_fsh <- numeric(nlbins) # Expected proportion at length in Fishery
ea1_ats <- numeric(n_ats) # Expected age 1 index from hydro survey
pred_cpue <- numeric(n_cpue) # Expected total numbers in Fishery
pred_avo <- numeric(n_avo) # Expected total numbers in Fishery
pred_ats <- numeric(n_ats) # Expected total numbers in Fishery
pred_bts <- numeric(n_bts) # Expected total numbers in Fishery
et_fsh <- numeric(n_fsh) # Expected total numbers in Fishery
et_bts <- numeric(n_bts) # Expected total numbers in Survey
avail_bts <- numeric(n_bts) # Availability estimates in BTS
avail_ats <- numeric(n_bts) # Availability estimates in HydroSurvey (note: uses n_bts_r)
eb_bts <- numeric(n_bts) # Expected total biomass in Survey
eb_ats <- numeric(n_ats) # Expected total biomass in Survey
ea1_ats <- numeric(n_ats) # Expected total biomass in Survey

et_ats <- numeric(n_ats) # Expected total numbers in HydroSurvey
lse_ats <- numeric(n_ats) # (no comment in original)
lvar_ats <- numeric(n_ats) # (no comment in original)
et_avo <- numeric(n_avo) # Expected total numbers in HydroSurvey
et_cpue <- numeric(n_cpue) # Expected total numbers in CPUE
Fmort <- numeric(endyr - styr + 1) # Fishing mortality vector

# Alternative initialization if you want to explicitly set names/indices
# (more similar to ADMB's 1-based indexing approach)

# For the Fmort vector with specific year range:
years <- styr:endyr_r
Fmort <- setNames(numeric(length(years)), years)

# If you want to maintain 1-based indexing conceptually, you could use:
# eac_fsh <- array(0, dim = c(n_fsh_r, nbins), dimnames = list(1:n_fsh_r, 1:nbins))
# But standard R 0-based indexing is more common and efficient
# Calculate catchability coefficients
q_cpue <- exp(log_q_cpue)
q_avo <- exp(log_q_avo)

# Define catch-at-age to get to survey time of year
catage <- natage * F * (1 - S) / Z

# Calculate predicted catch
# for (i in styr:endyr_r) {
pred_catch <- rowSums(catage * wt_fsh)
et_fsh <- rowSums(catage)
# for (i in 1:nyrs) {
#   pred_catch[i] <- sum(catage[i, ] * wt_fsh[i, ])
# }

# Fishery expected values
for (i in 1:n_fsh) {
  iyr <- yrs_fsh_data[i]
  et_fsh[i] <- sum(catage[i, ])

  if (use_age_err) {
    eac_fsh[i, ] <- (age_err[err_fsh[i], ] %*% catage[i, ]) / et_fsh[i]
  } else {
    eac_fsh[i, ] <- catage[i, ] / et_fsh[i]
  }
}
# Only do this for 3+ predicted fish
# elc_fsh <- (selages * catage[endyr, ]) / sum(catage[endyr, 3:nages]) * age_len

# CPUE predicted values
for (i in 1:n_cpue) {
  iyr <- as.character(yrs_cpue[i])
  pred_cpue[i] <- sum((wt_fsh[i, ] * natage[iyr, ]) * sel_fsh[iyr, ]) * q_cpue
}

# AVO predicted values
for (i in 1:n_avo) {
  iyr <- as.character(yrs_avo[i])
  # Note uses ats selectivity for predicted AVO
  pred_avo[i] <- sum((wt_avo[i, ] * natage[iyr, ]) * exp(log_sel_ats[iyr, ])) * q_avo
  # Alternative formulations (commented out):
  # pred_avo[i] <- sum(wt_fsh[iyr, ] * (natage[iyr, ] * sel_avo_in)) * q_avo
  # pred_avo[i] <- sum(wt_fsh[iyr, ] * natage[iyr, ]) * q_avo
}

q_bts <- exp(log_q_bts)
# Trawl survey (BTS) expected values
# length(yrs_bts_data)
# Problem that in ADMB the selectivity matrix is 43 rows, but here its the same dimension as the number of data points
i=n_bts-3
sel_bts <- exp(log_sel_bts)
for (i in 1:(n_bts)) {
  iyr  <- (yrs_bts_data[i]) - styr + 1 #
  # Added a selectivity index to account for the fact that the bts selectivity is defined from styr_bts to endyr_r
  isel <- (yrs_bts_data[i]) - styr_bts + 1 #
  ntmp <- (natage[iyr, ] * (S[iyr, ]^0.5))
   #ntmp <- (pm$N[iyr, ] * (pm$S[iyr, ]^0.5))

  if (use_age_err) {
    eac_bts[i, ] <- (age_err[err_bts[i], ] %*% (ntmp * sel_bts[isel, ])) 
  } else {
    eac_bts[i, ] <- ntmp * sel_bts[isel, ] 
  }

  #eb_bts[i] <- sum(wt_bts[i, mina_bts:nages] * eac_bts[i, mina_bts:nages])
  eb_bts[i] <- sum(wt_bts[i, ] * eac_bts[i, ])
  #et_bts[i] <- sum(eac_bts[i, mina_bts:nages])
  et_bts[i] <- sum(eac_bts[i, ])
  eac_bts[i, ] <- eac_bts[i, ] / (et_bts[i])
}

q_bts   <- mean(ob_bts)/mean(eb_bts)
eb_bts = eb_bts*q_bts

# Hydro survey (ATS) expected values
q_ats <- exp(log_q_ats)
# log_q_ats

# Loop for age composition data
for (i in 1:n_ats) {
  iyr <- yrs_ats_data[i] - styr +1
  isel <- (yrs_ats_data[i]) - styr_ats + 1 #
  ntmp <- (natage[iyr, ] * (S[iyr, ]^0.5))
  if (use_age_err) {
    # Eq. 15 - with age error
    eac_ats[i, ] <- (age_err[err_ats[i], ] %*% (ntmp * exp(log_sel_ats[isel, ]))) * q_ats
  } else {
    # Without age error
    eac_ats[i, ] <- ntmp * exp(log_sel_ats[isel, ]) * q_ats
  }
  # Age-1 expected values (independent of selectivity)
  ea1_ats[i] <- ntmp[1]

  # Biomass expected values
  eb_ats[i] <- sum(wt_ats[i, mina_ats:nages] * eac_ats[i, mina_ats:nages])
  eb_ats[i] <- sum(wt_ats[i,] * eac_ats[i, ])
  

  # Total expected numbers
  et_ats[i] <- sum(eac_ats[i, mina_ats:nages])

  # Normalize age composition (only for the relevant age range)
  eac_ats[i, mina_ats:nages] <- eac_ats[i, mina_ats:nages] / et_ats[i]
}
#dim(wt_ats)
  # summary(eb_ats/pm$eb_ats)

#--Likelihood components----------------
age_like <- multinomial_likelihood_age(
    oac = list(oac_fsh, oac_bts, oac_ats), 
    eac = list(eac_fsh, eac_bts, eac_ats), 
    sam = list(sam_fsh, sam_bts, sam_ats),
    MN_const = MN_const,
    age_like_offset = age_like_offset,
    mina_ats = mina_ats, 
    nages = nages
  )
age_like <- age_like + pm$age_like_offset
pm$age_like <- pm$age_like + pm$age_like_offset
age_like_offset   <- pm$age_like_offset
# dat$ob_avo
obs_avo_var <- ob_avo_std^2

Surv_Likelihood()

# ADMB: catch_like = norm2(log(obs_catch(styr,endyr_r)+1e-4)-log(pred_catch+1e-4));
yrs_ch_fsh      <- 1965:2023           # length 59
nch_fsh        <- length(yrs_ch_fsh)  # length 59
sel_ch_sig_fsh  <- rep(0.5, nch_fsh)  # length 59, all 0.5
sel_ch_sig_fsh[55:56]  <- 1.9  # length 59, all 0.5
#sel_ch_sig_fsh[nch_fsh]   <- 0.000001  # length 59, all 0.5
years <- styr:endyr_r
year_index <- setNames(seq_along(years), years)

pen_fsh <- selectivity_like_fsh(
  log_sel_fsh, styr, endyr_r, n_selages_fsh,
  yrs_ch_fsh, nch_fsh,
  sel_devs_fsh, sel_ch_sig_fsh, year_index
)

# bts
pen_bts <- selectivity_like_bts( 
  styr_bts = styr_bts,
  log_sel_bts = log_sel_bts,
  year_index = year_index,
  sel_age_one_bts_dev=sel_age_one_bts_dev )
# ats
yrs_ch_ats <- 1995:2024
sel_ch_sig_ats <- rep(0.138, length(yrs_ch_ats)) # length 30, all 0.5
year_index <- setNames(seq_along(styr_ats:endyr_r), styr_ats:endyr_r)
pen_ats <- selectivity_like_ats(
  log_sel_ats, styr, styr_ats, endyr_r,
  mina_ats, n_selages_ats,
  yrs_ch_ats, nch_ats=length(yrs_ch_ats),
  sel_ch_sig_ats,
  year_index
)
sel_like <- list(
  fsh = pen_fsh[1],
  bts = pen_bts[1],
  ats = pen_ats[1]
)
sel_like_dev <- list(
  fsh = pen_fsh$dev,
  bts = pen_bts$dev,
  ats = pen_ats$dev
)
#pen_ats
# Sum into your objective
total_penalty <- pen_fsh$total + pen_bts$total + pen_ats$total
pm$phat_ats <- pm$phat_ats[,2:16]
pm$phat_bts <- pm$phat_bts[,2:16]
pm$sel_ats <- pm$sel_ats[31:61,]
pm$sel_bts <- pm$sel_bts[19:61,]
pm$phat_fsh <- pm$phat_fsh[,2:16]
pm$SSB <- pm$SSB[,2]
styr_est = 1978
endyr_est = endyr - omitSR


# Define your SR function (e.g., Beverton–Holt with parameters a, b):
rec_like <- recruitment_likelihood( yrs_est = styr_est:endyr_est, 
    exclude_year = 1979,     # special-case exclusion
    eps = 1e-8               # to avoid log(0)
)

Priors <- numeric(4)
Priors[1] = -((srprior_a-1.)*log(steepness) + (srprior_b-1)*log(1.-steepness)); 
bts_like <- BTS_likelihood()
ats_like <- ATS_likelihood()
ats_age1_like <- ATS_age1_likelihood()
cpue_like <- CPUE_likelihood()
avo_like <- AVO_likelihood()
cat_like <- catBio*catch_like(obs_catch, pred_catch)
sel_like <- unlist(sel_like)
sel_like_dev <- unlist(sel_like_dev)
sel_like/pm$sel_like

tot_like <- sum( c(Priors, rec_like$rec_like,  age_like, bts_like, ats_like, ats_age1_like, 
                 cpue_like, avo_like, cat_like, sel_like, sel_like_dev - pm$age_like_offset ) )
rtmb <- list(
  N = natage,                               # from GetNumbersAtAge()
  Z = Z,                                    # from Get_Mortality_Rates()
  F = F,                                    # from Get_Mortality_Rates()
  M = M,                                    # from Get_Mortality_Rates()
  S = exp(-Z),                              # or from Get_Mortality_Rates()
  C = C,  #F / Z * (1 - exp(-Z)) * natage,       # Baranov catch
  pred_catch = pred_catch,
  obs_catch = obs_catch,               # if available
  SSB = SSB,                                # computed inside GetNumbersAtAge()
  phizero = phizero,                  # from Get_Bzero()
  Bzero = Bzero,                      # from Get_Bzero()
  steepness = steepness,              # input parameter
  pred_cpue=pred_cpue,
  pred_avo=pred_avo,
  eb_bts = eb_bts,                        # BTS biomass
  eb_ats = eb_ats,                        # BTS biomass
  sel_fsh = sel_fsh,                        # selectivity-at-age
  sel_bts = exp(log_sel_bts),
  sel_ats = exp(log_sel_ats),
  phat_fsh = eac_fsh,
  phat_bts = eac_bts,
  phat_ats = eac_ats,
  Priors   = Priors, 
  rec_like = rec_like$rec_like,
  age_like = age_like,
  age_like_offset = age_like_offset,
  sel_like = (sel_like),
  sel_like_dev = (sel_like_dev),
  bts_like = bts_like,
  ats_like = ats_like,
  ats_age1_like = ats_age1_like,
  cpue_like = cpue_like, 
  avo_like = avo_like, 
  cat_like = cat_like,
  tot_like = tot_like +pm$wt_like
)

# matplot(rtmb$phat_bts/pm$phat_bts, type="b")
# rowSums(rtmb$phat_bts[,1:15])
# rowSums(pm$phat_bts[,1:15])

