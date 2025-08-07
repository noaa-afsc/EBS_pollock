rm(list = ls())
library(RTMB)
# Read in last year's estimates for comparisons----------
library(ebswp)
setwd(here::here("runs", "lastyr"))
pm<- read_rep("pm.rep")

source(here::here("R", "utilities.R"))
#--Get the parameters (from ADMB converged model!)------------
parms <- read_pars(here::here("Rtmb", "pm.par"))
dat <- build_model_inputs(here::here("Rtmb", "input.dat"), 
                          fn = here::here("Rtmb", "rpm.dat"))
dat$spawnmo <- 4. # scalar, month of spawning
dat$yrfrac <- (dat$spawnmo - 1.) / 12 # scalar, fraction of year for spawning
dat$yrfrac <- (dat$spawnmo - 1.) / 12 # scalar, fraction of year for spawning
dat$inv_bts_cov <- solve(dat$cov_matrix)  # inverse covariance matrix
dat$obs_cpue_var <- dat$obs_cpue_std^2 # observation variance for CPUE
prel_vars <- preliminary_calcs(data = dat, parameters = parms)
dat$obs_avo_var <- dat$ob_avo_std^2 # observation variance for AVO
# rpm <- function(parms) {
getAll(prel_vars$data, prel_vars$parameters, warn = TRUE)
# dim(dat$cov_matrix )
## Optional (enables extra RTMB features)

## Initialize joint negative log likelihood
nll <- 0

# dim(sel_devs_fsh)[1] (sel_devs_fsh) mean(sel_coffs_fsh) args(compute_fsh_selectivity) n_selages_fsh styr nages
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
rowMeans(sel_fsh)
rowMeans(pm$sel_fsh)
matplot(sel_fsh-pm$sel_fsh, type="b")
matplot(sel_fsh, type="b")
matplot(pm$sel_fsh, type="b")
# compute_selectivity_ind <- function(stsel, slp, a50, se, ae, age_vector, endyr_r)
# sel_slp_bts_dev
# sel_a50_bts_dev
log_sel_bts <- compute_selectivity_ind(
  stsel = styr_bts,
  slp = sel_slp_bts,
  a50 = sel_a50_bts,
  se = sel_slp_bts_dev,
  ae = sel_a50_bts_dev,
  age_vector = 1:nages,
  endyr_r = endyr_r
)
# dim(log_sel_bts)
# matplot(exp(t(log_sel_bts)), type="l")

# compute_selectivity_ats_devs <- function(nsel, stsel, coffs, sel_devs,
                                         # mina_ats, yrs_ch_ats ) {
yrs_ch_ats <- 1995:2024
log_sel_ats <- compute_selectivity_ats_devs(
  nsel = n_selages_ats,
  stsel = styr_ats,
  coffs = sel_coffs_ats,
  sel_devs = sel_devs_ats,
  mina_ats, yrs_ch_ats
)
log_sel_ats[,1]

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
wt_ssb_lastyr <- wt_ssb[nyrs] # vector [1:nages], e.g., wt_ssb[endyr_r, ]
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
Bzero <- sum(wt_ssb_lastyr * p_mature * Ntmp)
phizero <- Bzero / Rzero

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
  SSB[yr] <- sum(natage[yr, ] * S[i, ]^yrfrac * p_mature * wt_ssb[yr])

  # Recruitment
  natage[yr, 1] <- exp(log_avgrec + log_rec_devs[i]) # Eq. 1

  # Survivors advance age
  natage[yr1, 2:nages] <- natage[yr, 1:(nages - 1)] * S[i, 1:(nages - 1)]
  natage[yr1, nages] <- natage[yr1, nages] + natage[yr, nages] * S[i, nages]

  # Optional print check
}
natage[yr1, 1] <- exp(log_avgrec + log_rec_devs[i+1]) # Eq. 1
# 
# natage[50:61,1]
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

# Trawl survey (BTS) expected values
# length(yrs_bts_data)
# i=n_bts
for (i in 1:n_bts) {
  iyr <- (yrs_bts_data[i]) - styr #
  ntmp <- natage[iyr, ] * (S[iyr, ]^0.5)

  if (use_age_err) {
    # Eq. 15 - with age error
    eac_bts[i, ] <- (age_err[err_bts[i], ] %*% (ntmp * exp(log_sel_bts[i, ]))) * exp(log_q_bts)
  } else {
    # Without age error
    eac_bts[i, ] <- ntmp * exp(log_sel_bts[i, ]) * exp(log_q_bts)
  }

  eb_bts[i] <- sum(wt_bts[i, mina_bts:nages] * eac_bts[i, mina_bts:nages])
  et_bts[i] <- sum(eac_bts[i, mina_bts:nages])
  eac_bts[i, ] <- eac_bts[i, ] / sum(eac_bts[i, ])
}
# dim(log_sel_bts)

# Hydro survey (ATS) expected values
q_ats <- exp(log_q_ats)
log_q_ats

# Loop for age composition data
i <- 1
for (i in 1:n_ats) {
  iyr <- yrs_ats_data[i] - styr +1
  ntmp <- (natage[iyr, ] * (S[iyr, ]^0.5))
  # ntmp
  # natage[iyr, ] / pm$N[iyr,]
  # S[iyr, ] / pm$S[iyr,] 
  # (natage[iyr, ] * (S[iyr, ]^0.5) )/ 
  #   (pm$N[iyr,]*(pm$S[iyr,]^0.5))
   # ntmp
  if (use_age_err) {
    # Eq. 15 - with age error
    eac_ats[i, ] <- (age_err[err_ats[i], ] %*% (ntmp * exp(log_sel_ats[iyr, ]))) * q_ats
  } else {
    # Without age error
    eac_ats[i, ] <- ntmp * exp(log_sel_ats[i, ]) * q_ats
  }
  #ntmp * exp(log_sel_ats[i, ]) * q_ats
  #(pm$N[iyr,]*(pm$S[iyr,]^0.5)) * pm$sel_ats[iyr, ] * q_ats

  # i; iyr
  # matplot(pm$sel_ats[31:61,2:15], type="b")
  # matplot(exp(log_sel_ats[,2:15]),type="b")
  # matplot(exp(log_sel_ats[,2:15])/pm$sel_ats[31:61,2:15], type="b")
  # rowMeans(exp(log_sel_ats[,1:15]))
  # rowMeans(pm$sel_ats[31:61,1:15])
  # (exp(log_sel_ats[,1]))
  # (pm$sel_ats[31:61,1])
  # 
  # Age-1 expected values (independent of selectivity)
  ea1_ats[i] <- ntmp[1]

  # Biomass expected values
  eb_ats[i] <- sum(wt_ats[i, mina_ats:nages] * eac_ats[i, mina_ats:nages])
  #pm$eb_ats

  # Total expected numbers
  et_ats[i] <- sum(eac_ats[i, mina_ats:nages])

  # Normalize age composition (only for the relevant age range)
  eac_ats[i, mina_ats:nages] <- eac_ats[i, mina_ats:nages] / et_ats[i]
}
#dim(wt_ats)

# Handle cases where et_ats is greater than age composition data
# # (for surveys with total abundance but no age composition)
# if (n_ats > n_ats_ac) {
#   for (i in (n_ats_ac + 1):n_ats) {
#     iyr <- yrs_ats_data[i]
#     ntmp <- natage[iyr, ] * (S[iyr, ]^0.5)
#
#     # Calculate total expected abundance for the age range
#     et_ats[i] <- sum((ntmp * exp(log_sel_ats[iyr, ]))[mina_ats:nages]) * q_ats
#   }
# }






# Final year recruitment and SSB
# ## Random slopes
# nll <- nll - sum(dnorm(a, mean = mua, sd = sda, log = TRUE))
# ## Random intercepts
# nll <- nll - sum(dnorm(b, mean = mub, sd = sdb, log = TRUE))
# ## Data
# predWeight <- a[Chick] * Time + b[Chick]
# nll <- nll - sum(dnorm(weight, predWeight, sd = sdeps, log = TRUE))
# ## Get predicted weight uncertainties
# ADREPORT(predWeight)
# ## Return
# nll
# # }

# dat$ob_avo
obs_avo_var <- ob_avo_std

Surv_Likelihood()


