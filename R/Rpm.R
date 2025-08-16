rpm <- function(parms) {
  "c" <- ADoverload("c")
  "[<-" <- ADoverload("[<-")
  
  getAll(parms, data, warn = TRUE)
  ## Initialize joint negative log likelihood
  nll <- 0
  # sel_devs_fsh <- sel_devs_fsh - mean(sel_devs_fsh) # Centering selectivity deviations
  tmp <- compute_selectivity_fsh(
    nsel = n_selages_fsh,
    stsel = styr,
    endyr_r = endyr_r,
    nages = nages,
    coffs = sel_coffs_fsh,
    sel_devs = sel_devs_fsh,
    yrs_ch_fsh = 1965:2023
  )
  #--
  log_sel_fsh <- tmp$log_sel
  avgsel_fsh <- tmp$avgsel
  sel_fsh <- exp(log_sel_fsh)
  sel_slp_bts_dev <- sel_slp_bts_dev - mean(sel_slp_bts_dev ) # Centering selectivity deviations
  sel_a50_bts_dev <- sel_a50_bts_dev - mean(sel_a50_bts_dev ) # Centering selectivity deviations
  log_sel_bts <- compute_selectivity_ind(
    stsel = styr_bts,
    slp = sel_slp_bts,
    a50 = sel_a50_bts,
    se = sel_slp_bts_dev,
    ae = sel_a50_bts_dev,
    age_vector = 0.5 + 1:nages,
    endyr_r = endyr_r
  )
  sel_age_one_bts_dev <- sel_age_one_bts_dev - mean(sel_age_one_bts_dev ) # Centering selectivity deviations
  log_sel_bts[, 1] <- sel_age_one_bts * exp(sel_age_one_bts_dev)

  yrs_ch_ats <- 1995:2024
  log_sel_ats <- compute_selectivity_ats_devs(
    nsel = n_selages_ats,
    nages = nages,
    stsel = styr_ats,
    endyr_r = endyr_r,
    coffs = sel_coffs_ats,
    sel_devs = sel_devs_ats,
    mina_ats, yrs_ch_ats
  )
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
  log_F_devs <- log_F_devs-mean(log_F_devs) 
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
  wt_ssb_lastyr <- wt_ssb[nyrs, ] # vector [1:nages], e.g., wt_ssb[endyr_r, ]
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
  Bzero <- 0.5 * sum(wt_ssb_lastyr * p_mature * Ntmp)
  # Bzero/pm$Bzero
  phizero <- Bzero / Rzero

  # Beverton-Holt alpha (Eq. 13)
  alpha <- log(-4 * steepness / (steepness - 1))

  #--Get numbers at age------------
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
    SSB[i] <- 0.5 * sum(natage[i, ] * (S[i, ]^yrfrac) * p_mature * wt_ssb[i, ])

    # Recruitment
    natage[yr, 1] <- exp(log_avgrec + log_rec_devs[i]) # Eq. 1

    # Survivors advance age
    natage[yr1, 2:nages] <- natage[yr, 1:(nages - 1)] * S[i, 1:(nages - 1)]
    natage[yr1, nages] <- natage[yr1, nages] + natage[yr, nages] * S[i, nages]

    # Optional print check
  }
  natage[yr1, 1] <- exp(log_avgrec + log_rec_devs[i + 1]) # Eq. 1
  SSB[yr1] <- 0.5 * sum(natage[yr1, ] * S[nyrs, ]^yrfrac * p_mature * wt_ssb[nyrs, ])
  pred_rec <- natage[, 1]

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
  sel_bts <- exp(log_sel_bts)
  for (i in 1:(n_bts)) {
    iyr <- (yrs_bts_data[i]) - styr + 1 #
    # Added a selectivity index to account for the fact that the bts selectivity is defined from styr_bts to endyr_r
    isel <- (yrs_bts_data[i]) - styr_bts + 1 #
    ntmp <- (natage[iyr, ] * (S[iyr, ]^0.5))
    # ntmp <- (pm$N[iyr, ] * (pm$S[iyr, ]^0.5))

    if (use_age_err) {
      eac_bts[i, ] <- (age_err[err_bts[i], ] %*% (ntmp * sel_bts[isel, ]))
    } else {
      eac_bts[i, ] <- ntmp * sel_bts[isel, ]
    }

    # eb_bts[i] <- sum(wt_bts[i, mina_bts:nages] * eac_bts[i, mina_bts:nages])
    eb_bts[i] <- sum(wt_bts[i, ] * eac_bts[i, ])
    # et_bts[i] <- sum(eac_bts[i, mina_bts:nages])
    et_bts[i] <- sum(eac_bts[i, ])
    eac_bts[i, ] <- eac_bts[i, ] / (et_bts[i])
  }
  # sel_bts[42,1:8 ]
  # (yrs_bts_data)
  # (wt_bts)

  q_bts   <- mean(ob_bts)/mean(eb_bts)
  eb_bts <- eb_bts * q_bts
  # Hydro survey (ATS) expected values
  q_ats <- exp(log_q_ats)

  # Loop for age composition data
  for (i in 1:n_ats) {
    iyr <- yrs_ats_data[i] - styr + 1
    isel <- (yrs_ats_data[i]) - styr_ats + 1 #
    ntmp <- (natage[iyr, ] * (S[iyr, ]^0.5))
    if (use_age_err) {
      # Eq. 15 - with age error
      eac_ats[i, ] <- (age_err[err_ats[i], ] %*% (ntmp * exp(log_sel_ats[isel, ]))) * q_ats
    } else {
      # Without age error
      eac_ats[i, ] <- ntmp * exp(log_sel_ats[isel, ]) * q_ats
    }
    ea1_ats[i] <- ntmp[1]

    # Biomass expected values
    eb_ats[i] <- sum(wt_ats[i, mina_ats:nages] * eac_ats[i, mina_ats:nages])
    eb_ats[i] <- sum(wt_ats[i, ] * eac_ats[i, ])

    # Total expected numbers
    et_ats[i] <- sum(eac_ats[i, mina_ats:nages])
    # Normalize age composition (only for the relevant age range)
    eac_ats[i, mina_ats:nages] <- eac_ats[i, mina_ats:nages] / et_ats[i]
  }

  age_like <- multinomial_likelihood_age(
    oac = list(oac_fsh, oac_bts, oac_ats),
    eac = list(eac_fsh, eac_bts, eac_ats),
    sam = list(sam_fsh, sam_bts, sam_ats),
    MN_const = MN_const, age_like_offset = age_like_offset,
    mina_ats = mina_ats, nages = nages )
  age_like <- age_like + pm$age_like_offset
  pm$age_like <- pm$age_like + pm$age_like_offset
  age_like_offset <- pm$age_like_offset
  obs_avo_var <- ob_avo_std^2

  # Surv_Likelihood()
  yrs_ch_fsh <- 1965:2023 # length 59
  nch_fsh <- length(yrs_ch_fsh) # length 59
  sel_ch_sig_fsh <- rep(0.5, nch_fsh) # length 59, all 0.5
  sel_ch_sig_fsh[55:56] <- 1.9 # length 59, all 0.5
  # sel_ch_sig_fsh[nch_fsh]   <- 0.000001  # length 59, all 0.5
  years <- styr:endyr_r
  year_index <- setNames(seq_along(years), years)

  nyrs <- length(years)
  pen_fsh <- selectivity_like_fsh(
    log_sel_fsh, styr, endyr_r, n_selages_fsh,
    yrs_ch_fsh, nch_fsh, nyrs = length(years),
    domFish, selTFsh, selCFsh, selCurv,
    sel_devs_fsh, sel_ch_sig_fsh, year_index )
  # bts
  pen_bts <- selectivity_like_bts(
    styr=styr, endyr_r=endyr_r,
    styr_bts = styr_bts,
    log_sel_bts = log_sel_bts,
    year_index = year_index,
    selCurv = selCurv,
    selVarbts = selVarbts,
    nages = nages,
    sel_age_one_bts_dev = sel_age_one_bts_dev )
  # pm# ats
  yrs_ch_ats <- 1995:2024
  sel_ch_sig_ats <- rep(0.138, length(yrs_ch_ats)) # length 30, all 0.5
  year_index <- setNames(seq_along(styr_ats:endyr_r), styr_ats:endyr_r)
  pen_ats <- selectivity_like_ats(
    log_sel_ats, styr, styr_ats, endyr_r,
    mina_ats, n_selages_ats, yrs_ch_ats,
    nch_ats = length(yrs_ch_ats),
    selATS, selTATS, sel_ch_sig_ats, year_index )
 sel_like <-  c(pen_fsh$shape, pen_bts$shape, pen_ats$shape)
 sel_like_dev <-  c(pen_fsh$dev,  pen_bts$dev,  pen_ats$dev)

  styr_est <- 1978
  endyr_est <- endyr - omitSR
  # Define your SR function (e.g., Beverton–Holt with parameters a, b):
  rec_like <- recruitment_likelihood(
    yrs_est = styr_est:endyr_est, SSB = SSB, pred_rec = pred_rec,
    log_rec_devs, sigr, log_initdevs, phizero, Bzero,
    alpha, omit78 = 1, exclude_year = 1979,  srrPrior, eps = 1e-8  )
  Fpen_like <- norm2(log_F_devs) # Fpenalty)
  Priors <- numeric(4)
  Priors[1] <- -((srprior_a - 1.) * log(steepness) + (srprior_b - 1) * log(1. - steepness))
  bts_like <- BTS_likelihood(
    ob_bts, ot_bts, eb_bts, et_bts, # observed and expected values
    inv_bts_cov,  var_ob_bts = ob_bts_std^2,  DoCovBTS = 1, do_bts_bio = TRUE  )
  ats_like <- ATS_likelihood(
    ob_ats, ot_ats, eb_ats, et_ats, # observed and expected values
    lvar_ats, lvarb_ats,  do_ats_bio = TRUE # flag for biology likelihood type) {
  )
  ats_age1_like <- ATS_age1_likelihood(
    oa1_ats, ea1_ats,  age1_sigma_ats,  use_age1_ats = TRUE, 
    ignore_last_ats_age1 = TRUE,  n_ats = length(oa1_ats)  )
# ats_age1_like/pm$ats_age1_like
  cpue_like <- CPUE_likelihood( obs_cpue, pred_cpue, obs_cpue_var )
  avo_like <- AVO_likelihood( ob_avo, pred_avo, obs_avo_var  )
  cat_like <- catBio * catch_like(obs_catch, pred_catch)

  #--Weight-age fixed effects-----------
  sigma_coh <- exp(log_sd_coh)
  sigma_yr <- exp(log_sd_yr)
  K <- exp(log_K)
  alphawt <- exp(log_alpha)
  yrs_wt <- styr_wt:endyr_wt
  yr_index <- setNames(seq_along(yrs_wt), yrs_wt)

  wt_like <- 0.0
  wt_nll <- numeric(4)
  mnwt <- numeric(age_end)
  wt_inc <- numeric(age_end - 1)
  wt_pre <- matrix(0, nrow = endyr_wt - styr_wt + 1, ncol = age_end)
  wt_hat <- array(0, dim = c(ndat_wt, max(nyrs_data), age_end))
  # ... other arrays as needed
  for (j in age_st:age_end) { mnwt[j] <- alphawt * (L1 + (L2 - L1) * (1 - K^(j - age_st)) / (1 - K^(nages - 1)))^3 }

  # Calculate weight increments
  wt_inc <- mnwt[(age_st + 1):age_end] - mnwt[age_st:(age_end - 1)]
  # Initialize first year
  wt_pre[1, ] <- mnwt
  # Subsequent years
  for (i in 2:(endyr_wt - styr_wt + 1)) {
    # Youngest age class # wt_pre(i,age_st) = mnwt(age_st)   * exp(square(sigma_coh)/2.+sigma_coh*coh_eff(i)); # print(c(i+styr_wt,coh_eff[i]))
    wt_pre[i, age_st] <- mnwt[age_st] * exp((sigma_coh^2) / 2 + sigma_coh * coh_eff[nages + i])
    # wt_pre(i)(age_st+1,age_end) = ++(wt_pre(i-1)(age_st,age_end-1) +
    wt_pre[i, (age_st + 1):age_end] <- wt_pre[i - 1, age_st:(age_end - 1)] +
      wt_inc * exp((sigma_yr^2) / 2 + sigma_yr * yr_eff[i])
    # wt_inc * exp(                sigma_yr * yr_eff[i]) #   wt_inc * exp(square(sigma_yr)/2. + sigma_yr*yr_eff(i)));
  }
  # wt_prek
  # h=1;i=1
  for (h in 1:ndat_wt) {
    for (i in 1:nyrs_data[h]) {
      iyr <- yr_index[as.character(yrs_data[h, i])]  # - styr_wt  # Adjust year index to start from 1
      if (h > 1) {
        wt_hat[h, i, ] <- c(0, 0, d_scale) * wt_pre[iyr, ]
      } else {
        # note 10 year diff between h=1 and h>1
        wt_hat[h, i, ] <- wt_pre[iyr, ]
      }
      for (j in age_st:age_end) {
        # Fit to global mean
        wt_nll[1] <- wt_nll[1] + (wt_obs[h, i, j - 2] - mnwt[j])^2 / (2 * sd_obs[h, i, j - 2]^2)
        # Fit to predicted values
        wt_nll[2] <- wt_nll[2] + (wt_obs[h, i, j - 2] - wt_hat[h, i, j])^2 /
          (2 * sd_obs[h, i, j - 2]^2)
        # print(wt_nll[2])
        # wt_nll(2) +=       square(wt_obs(h, i, j )  - wt_hat(h, i, j))   / (2.*square(sd_obs(h,i,j)));
      }
       #print(c(iyr+styr_wt,wt_nll[2],wt_hat[h,i,3:6]))
    }
  }
  # wt_pre[20:22,] (cbind(wt_hat[1,,3:7], wt_obs[1,,1:5]))
  wt_nll[3] <- wt_nll[3] + 0.5 * sum(coh_eff^2) # norm2 equivalent
  wt_nll[4] <- wt_nll[4] + 0.5 * sum(yr_eff^2) # norm2 equivalent
  #wt_nll
  # 1 5568.18 
  # 2 732.671 # 3 20.6849 # 4 23.3254 dim(wt_pre) dim(wt_obs) dim(wt_hat) dim(sd_obs) 
  # (wt_obs[1,42,]) dim(wt_pre_hat) (wt_pre_hat[2,2,])

  # Add penalty terms
  wt_like <- sum(wt_nll[1:4])

  # sel_like
  nll <- sum(c(
    Priors, rec_like$rec_like, 
    age_like, bts_like, ats_like, 
    ats_age1_like,
    cpue_like, avo_like, Fpen_like, 
     cat_like, 
    sel_like, 
    sel_like_dev, 
    wt_like-42105.12
  ))
  # REPORT(SSB)
  age_like <- age_like - age_like_offset
  REPORT(Priors)
  REPORT(rec_like$rec_like)
  REPORT(age_like)
  REPORT(bts_like)
  REPORT(ats_like)
  REPORT(ats_age1_like)
  REPORT(cpue_like)
  REPORT(avo_like)
  REPORT(Fpen_like)
  REPORT(cat_like)
  REPORT(sel_like)
  REPORT(sel_like_dev)
  REPORT(wt_like)
  REPORT(nll)
  if (return_nll_only)
    return( nll)
  else
    return(
    rtmb=list(
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
      cat_like = cat_like,
      bts_like = bts_like,
      ats_like = ats_like,
      ats_age1_like = ats_age1_like,
      cpue_like = cpue_like, 
      avo_like = avo_like, 
      wt_nll  = wt_nll , 
      wt_like = wt_like, 
      age_like = age_like - age_like_offset,
      age_like_offset = age_like_offset, 
      rec_like = rec_like$rec_like,
      Fpen_like = Fpen_like, # Fpenalty
      sel_like = (sel_like),
      sel_like_dev = (sel_like_dev),
      Priors   = Priors, 
      tot_like = nll #-42105.12
    )
  )
}