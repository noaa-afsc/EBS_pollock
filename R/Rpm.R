# This R file constructs a model input list mimicking ADMB's DATA_SECTION and LOCAL_CALCS
# It can be sourced directly in an RTMB project

read_model_inputs <- function(filepath) {
  lines <- readLines(filepath)
  if (length(lines) < 10) stop("Insufficient lines in the input file.")

  # Filenames read from file
  file_info <- list(
    model_name          = lines[1],
    datafile_name       = lines[2],
    selchng_filename    = lines[3],
    control_filename    = lines[4],
    Alt_MSY_File        = lines[5],
    Cov_Filename        = lines[6],
    Wtage_file          = lines[7],
    RawSurveyCPUE_file  = lines[8],
    Temp_Cons_Dist_file = lines[9],
    GenGamm_Filename    = lines[10]
  )
  constants <- list(
    DoCovBTS = 1,
    SrType = 1,
    Do_Combined = 0,
    use_age_err = 0,
    use_age1_ats = 1,
    age1_sigma_ats = 1,
    use_endyr_len = 0,
    use_popwts_ssb = 1,
    natmortprior = 0.3,
    cvnatmortprior = 0.1,
    natmort_in = c(0.9, 0.45, rep(0.3, 13)),
    q_all_prior = 0,
    q_all_sigma = 2,
    q_bts_prior = 0,
    q_bts_sigma = 2,
    sigrprior = 1,
    cvsigrprior = 0.2,
    phase_sigr = -6,
    steepnessprior = 0.6,
    cvsteepnessprior = 0.12,
    phase_steepness = 5,
    use_spr_msy_pen = 0,
    sigma_spr_msy = 0.20,
    use_last_ats_ac = 1,
    nyrs_sel_avg = 5,
    do_bts_bio = 1,
    do_ats_bio = 1,
    srprior_a = 14.93209877,
    srprior_b = 14.93209877,
    nyrs_future = 5,
    next_yrs_catch = 1350,
    nscen = 8,
    fixed_catch_fut2 = 1400,
    fixed_catch_fut3 = 1200,
    phase_F40 = 6,
    robust_phase = 1350,
    ats_robust_phase = 1350,
    ats_like_type = 0,
    phase_logist_fsh = -1,
    phase_logist_bts = 2,
    phase_seldevs_fsh = 4,
    phase_seldevs_bts = 5,
    phase_age1devs_bts = 3,
    phase_selcoffs_ats = 3,
    phase_sel_ats_dev = 5,
    phase_natmort = -6,
    phase_q_bts = 3,
    phase_q_std_area = -4,
    phase_q_ats = 4,
    phase_bt = -6,
    phase_rec_devs = 3,
    phase_larv = -3,
    phase_sr = 5,
    wt_fut_phase = 6,
    last_age_sel_fsh = 4,
    last_age_sel_bts = 8,
    last_age_sel_ats = 8,

    # Formerly ctrl_flag vector, now expanded:
    catBio = 200, # 1  Catch Biomass Emphasis
    btsEmph = 1, # 2  BTS Emphasis
    recDevs = 1, # 3  Recruitment Deviations
    fDevs = 1, # 4  F_devs
    atsEmph = 1, # 5  ATS Emphasis
    avoEmph = 1, # 6  AVO Emphasis
    ageComp = 1, # 7  Age Comp General
    ageFish = 1, # 8  Fishery AgeComp Emph
    ageBTS = 1, # 9  BTS AgeComp Emph
    selTFsh = 1, # 10 Fishery Selex Time Emph
    selCFsh = 1, # 11 Fishery Curv Emph
    cpueFsh = 1, # 12 Fishery CPUE Emph
    domFish = 12.5, # 13 Fishery Dome Emph
    domBTS = 1, # 14 BTS Dome Emph
    selATS = 1, # 15 ATS Selex Emph
    yrsFixF = 1, # 16 Fishery Sel Yrs Fixed
    yrsFixB = 1, # 17 BTS Sel Yrs Fixed
    resv18 = 1, # 18 Reserved
    selCurv = 3.125, # 19 Survey Sel Curvature
    btsVarT = 5, # 20 BTS Time Variability
    selTBTS = 1, # 21 BTS Selex Time Emph
    selTATS = 5, # 22 ATS Selex Time Emph
    larvDev = 1, # 23 Larval Rec-Devs
    rec78on = 1, # 24 Recruits 1978+
    omit78 = 2, # 25 Ignore 1978 in SRR
    resv26 = 0, # 26 Reserved
    sel3dif = 0, # 27 Fishery Selex 3rd Diff
    retroYr = 0, # 28 Retrospective Year
    omitSR = 1, # 29 Omit Recent SRR Yrs
    srrPrior = 1, # 30 SRR Prior Only
    mcmcmode = 0,
    Mmatrix = 0,
    count_mcmc = 0,
    count_mcsave = 0,
    q_amin = 3,
    q_amax = 15,
    do_EIT1 = 1,
    pflag = 0
  )

  # Vectors and arrays
  selages <- c(0.0, 0.0, rep(1.0, 13))
  sel_avo_in <- c(0.0, 1, 1, 0.85, 0.7, 0.55, 0.3, 0.15, 0.05, rep(0.01, 6))
  Cat_Fut <- rep(NA_real_, 10)

  # Placeholder values for control flags, catch, and flags
  obs_catch <- function(year) 1000 # mockup function for illustration

  # Local calculations
  lambda_spr_msy <- 0.5 / (0.1^2)
  phase_sr <- 1
  SrType <- 2

  phase_logic <- list(
    phase_nosr = if (phase_sr > 0) -1 else 1,
    phase_Rzero = if (phase_sr > 0) {
      if (SrType == 3) -2 else 3
    } else {
      -1
    },
    Steepness_UB = if (SrType == 4) 4.0 else 0.99
  )

  dat <- read_data(here::here("runs", "data", "rpm.dat"))
  # Assume these are values already defined in full model
  endyr_r <- endyr - retroYr
  endyr_est <- endyr_r - omitSR
  dec_tab_catch <- c(
    10,
    0.25 * obs_catch(endyr_r),
    0.50 * obs_catch(endyr_r),
    0.75 * obs_catch(endyr_r),
    1.00 * obs_catch(endyr_r),
    1.25 * obs_catch(endyr_r),
    1.50 * obs_catch(endyr_r),
    2.00 * obs_catch(endyr_r)
  )

  return(list(
    files = file_info,
    constants = constants,
    selages = selages,
    sel_avo_in = sel_avo_in,
    Cat_Fut = Cat_Fut,
    ctrl_flag = ctrl_flag,
    lambda_spr_msy = lambda_spr_msy,
    phase_logic = phase_logic,
    endyr_r = endyr_r,
    endyr_est = endyr_est,
    dec_tab_catch = dec_tab_catch
  ))
}

read_pars <- function(file) {
  lines <- readLines(file)
  result <- list()
  current_name <- NULL
  buffer <- c()

  flush_buffer <- function() {
    if (!is.null(current_name)) {
      values <- as.numeric(unlist(strsplit(buffer, "\\s+")))
      result[[current_name]] <<- if (length(values) == 1) values else values
    }
  }

  for (line in lines) {
    line <- trimws(line)
    if (line == "") next
    if (grepl("^#\\s+.+:", line)) {
      flush_buffer()
      current_name <- sub("^#\\s+", "", sub(":", "", line))
      buffer <- c()
    } else {
      buffer <- c(buffer, line)
    }
  }

  flush_buffer()
  return(result)
}
# This script builds the full input list for RTMB::MakeADFun based on the ADMB-style model inputs
# It uses the read_model_inputs() function from rtmb_input_setup.R

build_model_inputs <- function(filepath) {
  inputs <- read_model_inputs(filepath)

  # Utilities to read .dat-style content
  read_matrix <- function(file, ...) {
    as.matrix(read.table(file, header = FALSE, ...))
  }

  read_vector <- function(file) {
    scan(file, what = numeric(), quiet = TRUE)
  }

  read_ivector <- function(file) {
    scan(file, what = integer(), quiet = TRUE)
  }

  read_ragged_matrix <- function(file, nyrs_vec) {
    lines <- readLines(file)
    blocks <- split(lines, rep(1:length(nyrs_vec), nyrs_vec))
    lapply(blocks, function(x) as.integer(x))
  }

  # Load required data
  Cov_Filename <- inputs$files$Cov_Filename
  Wtage_file <- inputs$files$Wtage_file
  oac_fsh_file <- "oac_fsh_data.dat" # assumed filename
  yrs_avo_file <- "yrs_avo.dat" # assumed filename

  cov_matrix <- if (file.exists(Cov_Filename)) read_matrix(Cov_Filename) else NULL
  wt_fsh <- if (file.exists(Wtage_file)) read_matrix(Wtage_file) else matrix(runif(32 * 15, 0.5, 2), ncol = 15)

  styr <- 1964
  endyr <- styr + nrow(wt_fsh) - 1
  nages <- ncol(wt_fsh)
  nyears <- endyr - styr + 1
  rownames(wt_fsh) <- styr:endyr
  colnames(wt_fsh) <- paste0("Age", 1:nages)

  wt_tmp <- wt_fsh[as.character(styr:(endyr - 1)), , drop = FALSE]
  wt_mn <- apply(wt_tmp, 2, min)
  wt_sigma <- apply(wt_tmp, 2, function(x) sqrt(mean((x - mean(x))^2)))

  # Read optional age comp or ragged data files
  oac_fsh_data <- if (file.exists(oac_fsh_file)) read_matrix(oac_fsh_file) else matrix(NA, nrow = 10, ncol = nages)
  yrs_avo <- if (file.exists(yrs_avo_file)) read_ivector(yrs_avo_file) else 2000:2010

  # Placeholder: ragged matrix e.g. from yrs_data blocks
  ragged_file <- "yrs_data.dat"
  nyrs_vec <- rep(5, 3) # e.g., 3 blocks of 5 lines each
  ragged_blocks <- if (file.exists(ragged_file)) read_ragged_matrix(ragged_file, nyrs_vec) else NULL

  inputs$parameters <- list(
    wt_fsh = wt_fsh,
    wt_mn = wt_mn,
    wt_sigma = wt_sigma,
    lambda_spr_msy = inputs$lambda_spr_msy,
    selages = inputs$selages,
    sel_avo_in = inputs$sel_avo_in,
    Cat_Fut = inputs$Cat_Fut
  )





  names(inputs$ctrl_flag) <- c(
    "catBio", # 1  Catch Biomass Emphasis
    "btsEmph", # 2  BTS Emphasis
    "recDevs", # 3  Recruitment Deviations
    "fDevs", # 4  F_devs
    "atsEmph", # 5  ATS Emphasis
    "avoEmph", # 6  AVO Emphasis
    "ageComp", # 7  Age Comp General
    "ageFish", # 8  Fishery AgeComp Emph
    "ageBTS", # 9  BTS AgeComp Emph
    "selTFsh", # 10 Fishery Selex Time Emph
    "selCFsh", # 11 Fishery Curv Emph
    "cpueFsh", # 12 Fishery CPUE Emph
    "domFish", # 13 Fishery Dome Emph
    "domBTS", # 14 BTS Dome Emph
    "selATS", # 15 ATS Selex Emph
    "yrsFixF", # 16 Fishery Sel Yrs Fixed
    "yrsFixB", # 17 BTS Sel Yrs Fixed
    "resv18", # 18 Reserved
    "selCurv", # 19 Survey Sel Curvature
    "btsVarT", # 20 BTS Time Variability
    "selTBTS", # 21 BTS Selex Time Emph
    "selTATS", # 22 ATS Selex Time Emph
    "larvDev", # 23 Larval Rec-Devs
    "rec78on", # 24 Recruits 1978+
    "omit78", # 25 Ignore 1978 in SRR
    "resv26", # 26 Reserved
    "sel3dif", # 27 Fishery Selex 3rd Diff
    "retroYr", # 28 Retrospective Year
    "omitSR", # 29 Omit Recent SRR Yrs
    "srrPrior" # 30 SRR Prior Only
  )
  inputs$data <- list(
    styr = styr,
    endyr = endyr,
    nages = nages,
    nyears = nyears,
    ctrl_flag = inputs$ctrl_flag,
    cov_matrix = cov_matrix,
    oac_fsh_data = oac_fsh_data,
    yrs_avo = yrs_avo,
    ragged_blocks = ragged_blocks
  )
  return(inputs)
}

# This file defines RTMB-style parameter declarations based on the ADMB PARAMETER_SECTION
# It maps init_number, init_vector, init_matrix, sdreport_number, etc. into R lists
make_parameters <- function(data) {
  list(
    log_avgrec = 0,
    log_avginit = 0,
    log_avg_F = 0,
    natmort_phi = 0,
    natmort = rep(0.2, data$nages),
    base_natmort = rep(0.2, data$nages),
    log_q_bts = 0,
    log_q_std_area = 0,
    bt_slope = 0,
    log_q_ats = 0,
    log_Rzero = 0,
    steepness = 1.0,
    log_q_cpue = 0,
    log_q_avo = 0,
    log_initdevs = rep(0, data$nages - 1),
    log_rec_devs = rep(0, data$endyr_r - data$styr + 1),
    larv_rec_devs = matrix(0, 11, 11),
    alpha = 1,
    beta = 1,
    Rzero = 1e6,
    YC_2018 = 0,
    q_all = 0,
    log_F_devs = rep(0, data$endyr_r - data$styr + 1),
    sigr = 1.0,
    sel_devs_fsh = matrix(0, data$dim_sel_fsh, data$n_selages_fsh),
    sel_devs_bts = matrix(0, data$dim_sel_bts, data$n_selages_bts),
    sel_devs_ats = matrix(0, data$dim_sel_ats, data$n_selages_ats - data$mina_ats + 1),
    sel_coffs_fsh = rep(0, data$n_selages_fsh),
    sel_coffs_bts = rep(0, data$n_selages_bts),
    sel_coffs_ats = rep(0, data$n_selages_ats - data$mina_ats + 1),
    wt_fut = rep(1.0, data$nages),
    sel_slp_bts = 1.0,
    sel_a50_bts = 4.0,
    sel_age_one_bts = 1.0,
    sel_slp_bts_dev = rep(0, data$endyr_r - data$styr_bts + 1),
    sel_a50_bts_dev = rep(0, data$endyr_r - data$styr_bts + 1),
    sel_age_one_bts_dev = rep(0, data$endyr_r - data$styr_bts + 1),
    sel_dif1_fsh = 1.0,
    sel_a501_fsh = 3.0,
    sel_trm2_fsh = 0.5,
    sel_dif2_fsh = 1.0,
    sel_dif1_fsh_dev = rep(0, data$endyr_r - data$styr + 1),
    sel_a501_fsh_dev = rep(0, data$endyr_r - data$styr + 1),
    sel_trm2_fsh_dev = rep(0, data$endyr_r - data$styr + 1),
    SPR_ABC = 0.4,
    endyr_N = rep(1e6, data$nages),
    B_Bnofsh = 1,
    Bzero = 1,
    Percent_Bzero = 1,
    Percent_B100 = 1,

    # Example placeholders — more can be added as needed
    L1 = 30,
    L2 = 60,
    log_alpha = -0.5,
    log_K = 0.1,
    d_scale = array(0, dim = c(data$nscale_parm, data$age_st:data$age_end)),
    coh_eff = rep(0, data$endyr_wt - data$styr_wt + 1),
    yr_eff = rep(0, data$endyr_wt - data$styr_wt + 4)
  )
}

parameters<- read_pars(here::here("Rtmb","pm.par"))
# RTMB-style stock-recruitment functions based on ADMB's SRecruit

# Scalar version
SRecruit_scalar <- function(Stmp, data, parameters, derived) {
  SrType <- data$SrType
  alpha <- derived$alpha
  beta <- derived$beta
  phizero <- derived$phizero
  Bzero <- derived$Bzero
  log_avgrec <- parameters$log_avgrec

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

# Vectorized version
SRecruit_vector <- function(Stmp, data, parameters, derived) {
  SrType <- data$SrType
  alpha <- derived$alpha
  beta <- derived$beta
  phizero <- derived$phizero
  Bzero <- derived$Bzero
  log_avgrec <- parameters$log_avgrec

  if (SrType == 1) {
    return((Stmp / phizero) * exp(alpha * (1 - Stmp / Bzero)))
  } else if (SrType == 2) {
    return(Stmp / (alpha + beta * Stmp))
  } else if (SrType == 3) {
    return(rep(exp(log_avgrec), length(Stmp)))
  } else if (SrType == 4) {
    return(Stmp * exp(alpha - beta * Stmp))
  } else {
    stop("Unsupported SrType")
  }
}

# RTMB-style version of ADMB's Get_Catch_at_Age function
# Computes catage, pred_catch, CPUE, AVO, and survey age compositions

get_catch_predictions <- function(data, parameters, derived) {
  with(data, {
    nages <- data$nages
    nyears <- data$nyears
    q_cpue <- exp(parameters$log_q_cpue)
    q_avo <- exp(parameters$log_q_avo)
    q_ats <- exp(parameters$log_q_ats)

    catage <- array(0, dim = c(nyears, nages))
    pred_catch <- numeric(nyears)
    et_fsh <- numeric(n_fsh_r)
    eac_fsh <- matrix(0, n_fsh_r, nages)
    elc_fsh <- numeric(nlbins)
    pred_cpue <- numeric(n_cpue)
    pred_avo <- numeric(n_avo_r)
    pred_cope <- numeric(n_cope)
    eac_bts <- matrix(0, n_bts_r, nages)
    eb_bts <- numeric(n_bts_r)
    et_bts <- numeric(n_bts_r)
    eac_ats <- matrix(0, n_ats_ac_r, nages)
    ea1_ats <- numeric(n_ats_ac_r)
    eb_ats <- numeric(n_ats_ac_r)
    et_ats <- numeric(n_ats_r)

    for (i in 1:nyears) {
      catage[i, ] <- natage[i, ] * F[i, ] * (1 - S[i, ]) / Z[i, ]
      pred_catch[i] <- sum(catage[i, ] * wt_fsh[i, ])
    }

    for (i in 1:n_fsh_r) {
      iyr <- yrs_fsh_data[i] - styr + 1
      et_fsh[i] <- sum(catage[iyr, ])
      p <- if (use_age_err) {
        age_err[[err_fsh[i]]] %*% (catage[iyr, ] / et_fsh[i])
      } else {
        catage[iyr, ] / et_fsh[i]
      }
      eac_fsh[i, ] <- p
    }

    elc_fsh <- (selages * catage[nyears, ]) / sum(catage[nyears, 3:nages]) %*% age_len

    for (i in 1:n_cpue) {
      iyr <- yrs_cpue[i] - styr + 1
      pred_cpue[i] <- sum(wt_fsh[iyr, ] * natage[iyr, ] * sel_fsh[iyr, ]) * q_cpue
    }

    for (i in 1:n_avo_r) {
      iyr <- yrs_avo[i] - styr + 1
      pred_avo[i] <- sum(wt_avo[i, ] * natage[iyr, ] * exp(log_sel_ats[iyr, ])) * q_avo
    }

    for (i in 1:n_cope) {
      iyr <- yrs_cope[i] - styr + 1 + 3
      if (iyr <= nyears) pred_cope[i] <- natage[iyr, 3]
    }

    for (i in 1:n_bts_r) {
      iyr <- yrs_bts_data[i] - styr + 1
      ntmp <- natage[iyr, ] * sqrt(S[iyr, ])
      sel <- exp(log_sel_bts[iyr, ])
      eac <- if (use_age_err) {
        age_err[[err_bts[i]]] %*% (ntmp * sel) * exp(parameters$log_q_bts)
      } else {
        ntmp * sel * exp(parameters$log_q_bts)
      }
      eac_bts[i, ] <- eac / sum(eac)
      eb_bts[i] <- sum(wt_bts[i, ] * eac)
      et_bts[i] <- sum(eac[mina_bts:nages])
    }

    for (i in 1:n_ats_ac_r) {
      iyr <- yrs_ats_data[i] - styr + 1
      ntmp <- natage[iyr, ] * sqrt(S[iyr, ])
      sel <- exp(log_sel_ats[iyr, ])
      eac <- if (use_age_err) {
        age_err[[err_ats[i]]] %*% (ntmp * sel) * q_ats
      } else {
        ntmp * sel * q_ats
      }
      eac_ats[i, ] <- 0
      eac_ats[i, mina_ats:nages] <- eac[mina_ats:nages] / sum(eac[mina_ats:nages])
      ea1_ats[i] <- ntmp[1]
      eb_ats[i] <- sum(wt_ats[i, ] * eac)
    }

    for (i in n_ats_ac_r:n_ats_r) {
      iyr <- yrs_ats_data[i] - styr + 1
      ntmp <- natage[iyr, ] * sqrt(S[iyr, ])
      et_ats[i] <- sum(ntmp[mina_ats:nages] * exp(log_sel_ats[iyr, mina_ats:nages])) * q_ats
    }

    list(
      catage = catage,
      pred_catch = pred_catch,
      eac_fsh = eac_fsh,
      elc_fsh = elc_fsh,
      pred_cpue = pred_cpue,
      pred_avo = pred_avo,
      pred_cope = pred_cope,
      eac_bts = eac_bts,
      eb_bts = eb_bts,
      et_bts = et_bts,
      eac_ats = eac_ats,
      ea1_ats = ea1_ats,
      eb_ats = eb_ats,
      et_ats = et_ats
    )
  })
}


# RTMB-compatible robust-p likelihood functions
# Equivalent to ADMB robust_p() overloads

# Vector-vector version
robust_p_vector <- function(obs, pred, a, b) {
  v <- a + 2 * obs * (1 - obs)
  l <- exp(-b * ((pred - obs)^2 / v))
  logL <- -sum(log(l + 0.01)) + 0.5 * sum(log(v))
  return(logL)
}

# Matrix-matrix version (indexed by year and age bin)
robust_p_matrix <- function(obs, pred, a, b) {
  if (!all(dim(obs) == dim(pred))) stop("Dimension mismatch in robust_p_matrix")
  v <- a + 2 * obs * (1 - obs)
  l <- (pred - obs)^2 / v
  logL <- 0
  for (i in seq_len(nrow(obs))) {
    logL <- logL - sum(log(exp(-b[i] * l[i, ]) + 0.01))
  }
  logL <- logL + 0.5 * sum(log(v))
  return(logL)
}

# Matrix-subset version for (amin:amax) indices
robust_p_matrix_window <- function(obs, pred, a, b, amin, amax) {
  if (!all(dim(obs) == dim(pred))) stop("Dimension mismatch in robust_p_matrix_window")
  logL <- 0
  for (i in seq_len(nrow(obs))) {
    o <- obs[i, amin:amax]
    p <- pred[i, amin:amax]
    v <- a + 2 * o * (1 - o)
    l <- (p - o)^2 / v
    logL <- logL - sum(log(exp(-b[i] * l) + 0.01)) + 0.5 * sum(log(v))
  }
  return(logL)
}

# RTMB-style survey likelihood: BTS, ATS, CPUE, AVO, COPE

survey_likelihood <- function(data, parameters, derived, predictions) {
  with(data, {
    surv_like <- numeric(3)
    cpue_like <- 0
    avo_like <- 0
    cope_like <- 0

    eb_bts <- predictions$eb_bts * mean(ob_bts) / mean(predictions$eb_bts) # q_bts estimate
    srv_tmp <- if (do_bts_bio) log(ob_bts) - log(eb_bts) else ot_bts - predictions$et_bts

    if (DoCovBTS == 1) {
      surv_like[1] <- 0.5 * t(srv_tmp) %*% inv_bts_cov %*% srv_tmp
    } else if (DoCovBTS == 2) {
      for (i in 1:n_bts_r) {
        if (sd_GenGam[i] > 0) {
          # Placeholder for generalized gamma
          surv_like[1] <- surv_like[1] + (log(ob_bts[i]) - log(eb_bts[i]))^2 / (2 * var_ob_bts[i])
        } else {
          surv_like[1] <- surv_like[1] + (log(ob_bts[i]) - log(eb_bts[i]))^2 / (2 * var_ob_bts[i])
        }
      }
    } else {
      for (i in 1:n_bts_r) {
        surv_like[1] <- surv_like[1] + (srv_tmp[i])^2 / (2 * var_ob_bts[i])
      }
    }

    if (do_ats_bio) {
      for (i in 1:n_ats_r) {
        surv_like[2] <- surv_like[2] + (log(ob_ats[i] + 0.01) - log(predictions$eb_ats[i] + 0.01))^2 / (2 * lvarb_ats[i])
      }
    } else {
      for (i in 1:n_ats_r) {
        surv_like[2] <- surv_like[2] + (log(ot_ats[i] + 0.01) - log(predictions$et_ats[i] + 0.01))^2 / (2 * lvar_ats[i])
      }
    }

    if (use_age1_ats > 0) {
      qtmp <- exp(mean(log(oa1_ats) - log(predictions$ea1_ats)))
      if (ignore_last_ats_age1) {
        surv_like[3] <- sum((log(oa1_ats[1:(n_ats_r - 1)] + 0.01) - log(predictions$ea1_ats[1:(n_ats_r - 1)] * qtmp + 0.01))^2) / (2 * age1_sigma_ats^2)
      } else {
        surv_like[3] <- sum((log(oa1_ats + 0.01) - log(predictions$ea1_ats * qtmp + 0.01))^2) / (2 * age1_sigma_ats^2)
      }
    }

    for (i in 1:n_cpue) {
      cpue_like <- cpue_like + (obs_cpue[i] - predictions$pred_cpue[i])^2 / (2 * obs_cpue_var[i])
    }

    for (i in 1:n_avo_r) {
      avo_like <- avo_like + (obs_avo[i] - predictions$pred_avo[i])^2 / (2 * obs_avo_var[i])
    }

    if (last_phase && phase_cope > 0) {
      ntmp <- n_cope - (yrs_cope[n_cope] + 3 - endyr_r)
      qtmp <- exp(mean(log(obs_cope[1:ntmp]) - log(predictions$pred_cope[1:ntmp])))
      for (i in 1:n_cope) {
        cope_like <- cope_like + (log(obs_cope[i]) - log(predictions$pred_cope[i]))^2 / (2 * lvar_cope[i])
      }
    }

    list(
      surv_like = ctrl_flag[2] * surv_like,
      cpue_like = ctrl_flag[5] * cpue_like,
      avo_like = ctrl_flag[6] * avo_like,
      cope_like = cope_like
    )
  })
}

# RTMB-compatible objective function
# Aggregates all component likelihoods and penalties

evaluate_objective_fn <- function(data, parameters, derived, predictions, likelihoods, priors) {
  NLL <- numeric(20)

  # Core data likelihoods
  NLL[1] <- data$ctrl_flag[1] * sum((log(data$obs_catch + 1e-4) - log(predictions$pred_catch + 1e-4))^2)
  NLL[2:4] <- data$ctrl_flag[2] * likelihoods$surv_like
  NLL[5] <- data$ctrl_flag[5] * likelihoods$cpue_like
  NLL[6] <- data$ctrl_flag[6] * likelihoods$avo_like
  NLL[7] <- data$ctrl_flag[3] * sum(likelihoods$rec_like)
  NLL[8] <- if (data$phase_cope > 0 && data$current_phase >= data$phase_cope) likelihoods$cope_like else 0
  NLL[9] <- data$ctrl_flag[4] * sum(parameters$log_F_devs^2)

  # Age and length comps
  NLL[10:12] <- data$ctrl_flag[7:9] * likelihoods$age_like[1:3]
  if (data$use_endyr_len > 0) NLL[13] <- data$ctrl_flag[7] * likelihoods$len_like

  # Selectivity penalties
  NLL[14] <- sum(likelihoods$sel_like)
  NLL[15] <- sum(likelihoods$sel_like_dev)

  # Priors and optional penalty components
  NLL[16] <- sum(priors)

  # Regularization to guide F and initial comps
  if (data$current_phase < 3) {
    F_penalty <- 10 * (log(mean(parameters$Fmort)) - log(0.2))^2
    init_penalty <- 10 * (parameters$log_avginit - parameters$log_avgrec)^2
    return(sum(NLL) + F_penalty + init_penalty)
  }

  return(sum(NLL))
}



Get_Selectivity <- function() {
  avgsel_fsh[] <- 0
  avgsel_bts[] <- 0
  avgsel_ats[] <- 0

  # Fishery selectivity
  if (is_active(sel_devs_fsh)) {
    log_sel_fsh <- compute_fsh_selectivity(n_selages_fsh, styr, avgsel_fsh, sel_coffs_fsh, sel_devs_fsh, group_num_fsh)
  } else {
    log_sel_fsh <- compute_selectivity(n_selages_fsh, styr, avgsel_fsh, sel_coffs_fsh)
  }

  # Logistic fishery (optional and usually not used)
  if (phase_logist_fsh > 0) {
    if (is_active(sel_a501_fsh_dev)) {
      seldevs_tmp <- abind::abind(sel_dif1_fsh_dev, sel_a501_fsh_dev, sel_trm2_fsh_dev, along = 0)
      log_sel_fsh <- compute_selectivity1(styr, sel_dif1_fsh, sel_a501_fsh, sel_trm2_fsh, seldevs_tmp)
    } else {
      log_sel_fsh <- compute_selectivity1(styr, sel_dif1_fsh, sel_a501_fsh, sel_trm2_fsh)
    }
  }

  # BTS selectivity
  if (is_active(sel_a50_bts)) {
    if (is_active(sel_a50_bts_dev)) {
      log_sel_bts <- compute_selectivity(styr_bts, sel_slp_bts, sel_a50_bts, sel_slp_bts_dev, sel_a50_bts_dev)
    } else {
      log_sel_bts <- compute_selectivity(styr_bts, sel_slp_bts, sel_a50_bts)
    }
    for (i in styr_bts:endyr_r) {
      log_sel_bts[i, 1] <- sel_age_one_bts * exp(sel_age_one_bts_dev[i])
    }
  } else if (is_active(sel_devs_bts)) {
    log_sel_bts <- compute_selectivity(n_selages_bts, styr_bts, avgsel_bts, sel_coffs_bts, sel_devs_bts, group_num_bts)
  } else {
    log_sel_bts <- compute_selectivity(n_selages_bts, styr_bts, avgsel_bts, sel_coffs_bts)
  }

  # ATS selectivity
  if (is_active(sel_devs_ats)) {
    log_sel_ats <- compute_selectivity_ats_devs(n_selages_ats, styr_ats, avgsel_ats, sel_coffs_ats, sel_devs_ats)
  } else {
    log_sel_ats <- compute_selectivity_ats(n_selages_ats, styr_ats, avgsel_ats, sel_coffs_ats)
  }

  sel_fsh <- exp(log_sel_fsh)
  compute_Fut_selectivity()
}


GetNumbersAtAge <- function() {
  Get_Bzero() # assume this sets initial biomass quantities

  for (i in styr:endyr_r) {
    rec_epsilons[i] <- log_rec_devs[i] + larv_rec_devs[nsindex[i], ewindex[i]]
  }

  log_initage <- log_initdevs + log_avginit
  natage[styr, 2:nages] <- exp(log_initage)

  # Recruitment in subsequent years
  if (is_active(resid_temp_x1)) {
    SST_subset <- SST[(styr - 1):(endyr_r - 1)]
    pred_rec_alpha <- log(length(SST_subset) /
      sum(exp(resid_temp_x1 * SST_subset +
        resid_temp_x2 * (SST_subset^2))))
    for (i in styr:endyr_r) {
      natage[i, 1] <- exp(log_avgrec + rec_epsilons[i] +
        pred_rec_alpha +
        resid_temp_x1 * SST[i - 1] +
        resid_temp_x2 * SST[i - 1]^2)
      pred_rec[i] <- natage[i, 1]
    }
  } else if (do_srrdevs) {
    for (i in styr:(endyr_r - 1)) {
      if (i == styr) {
        SSB[styr] <- sum(natage[styr, ] *
          (S[styr, ]^yrfrac) *
          p_mature * wt_ssb[styr])
        natage[i, 1] <- exp(log_avgrec + rec_epsilons[i])

        natage[styr + 1, 2:nages] <- natage[styr, 1:(nages - 1)] * S[styr, 1:(nages - 1)]
        natage[styr + 1, nages] <- natage[styr + 1, nages] + natage[styr, nages] * S[styr, nages]
      } else {
        SSB[i] <- sum(natage[i, ] * (S[i, ]^yrfrac) * p_mature * wt_ssb[i])
        natage[i, 1] <- SRecruit(SSB[i - 1]) * exp(rec_epsilons[i])
        natage[i + 1, 2:nages] <- natage[i, 1:(nages - 1)] * S[i, 1:(nages - 1)]
        natage[i + 1, nages] <- natage[i + 1, nages] + natage[i, nages] * S[i, nages]
      }
    }
    natage[endyr_r, 1] <- SRecruit(SSB[endyr_r - 1]) * exp(rec_epsilons[endyr_r])
    SSB[endyr_r] <- sum(natage[endyr_r, ] * (S[endyr_r, ]^yrfrac) * p_mature * wt_ssb[endyr_r])
    meannatage <- (1 - S) / Z * natage # element-wise
    pred_rec[i] <- natage[endyr_r, 1]
    if (do_check) stop("check flag triggered")
  } else {
    for (i in styr:endyr_r) {
      natage[i, 1] <- exp(log_avgrec + rec_epsilons[i])
      pred_rec[i] <- natage[i, 1]
    }
    if (exists("ycin") && ycin > 0) natage["2019", 1] <- ycin # adapt as needed
  }

  # Forward projection if no predation estimation
  if (do_pred != 1 && !do_srrdevs) {
    SSB[styr] <- sum(natage[styr, ] * (S[styr, ]^yrfrac) * p_mature * wt_ssb[styr])
    natage[styr + 1, 2:nages] <- natage[styr, 1:(nages - 1)] * S[styr, 1:(nages - 1)]
    natage[styr + 1, nages] <- natage[styr + 1, nages] + natage[styr, nages] * S[styr, nages]

    for (i in (styr + 1):(endyr_r - 1)) {
      SSB[i] <- sum(natage[i, ] * (S[i, ]^yrfrac) * p_mature * wt_ssb[i])
      natage[i + 1, 2:nages] <- natage[i, 1:(nages - 1)] * S[i, 1:(nages - 1)]
      natage[i + 1, nages] <- natage[i + 1, nages] + natage[i, nages] * S[i, nages]
    }

    SSB[endyr_r] <- sum(natage[endyr_r, ] * (S[endyr_r, ]^yrfrac) * p_mature * wt_ssb[endyr_r])
    meannatage <- (1 - S) / Z * natage
  }
}
Get_Bzero <- function() {
  Bzero[] <- 0
  Rzero <- exp(log_Rzero)
  Ntmp <- numeric(nages)
  survtmp <- exp(-natmort)

  Ntmp[1] <- Rzero
  for (j in 1:(nages - 1)) {
    Ntmp[j + 1] <- Ntmp[j] * survtmp[j]
  }

  # Plus group adjustment
  Ntmp[nages] <- Ntmp[nages] / (1 - survtmp[nages])

  # Year fraction correction (equivalent to Ntmp[j] *= exp(yrfrac*log(survtmp[j])))
  Ntmp <- Ntmp * exp(yrfrac * log(survtmp))

  # Compute Bzero: spawning biomass in unfished state
  Bzero <- sum(wt_ssb[endyr_r, ] * p_mature * Ntmp)
  phizero <- Bzero / Rzero

  # Stock-recruit parameters (Eq. 13 and variants)
  if (SrType == 1) {
    alpha <- log(-4 * steepness / (steepness - 1))
    beta <- NA
  } else if (SrType == 2) {
    alpha <- Bzero * (1 - (steepness - 0.2) / (0.8 * steepness)) / Rzero
    beta <- (5 * steepness - 1) / (4 * steepness * Rzero)
  } else if (SrType == 4) {
    beta <- log(5 * steepness) / (0.8 * Bzero)
    alpha <- log(Rzero / Bzero) + beta * Bzero
  } else {
    alpha <- beta <- NA
    warning("Unrecognized SrType")
  }

  return(list(Bzero = Bzero, phizero = phizero, alpha = alpha, beta = beta))
}

Requil <- function(phi, SrType, alpha, beta, Bzero, phizero, log_avgrec) {
  switch(SrType,
    `1` = Bzero * (alpha + log(phi) - log(phizero)) / (alpha * phi),
    `2` = (phi - alpha) / (beta * phi),
    `3` = exp(log_avgrec),
    `4` = (log(phi) + alpha) / (beta * phi),
    stop("Unknown SrType in Requil()")
  )
}


# RTMB-style selectivity shape calculators translated from ADMB forms

logistic_selectivity <- function(age_vector, slp, a50, nyears) {
  log_sel <- matrix(NA, nrow = nyears, ncol = length(age_vector))
  for (i in 1:nyears) {
    log_sel[i, ] <- -log(1 + exp(-slp * (age_vector - a50)))
  }
  return(log_sel)
}

logistic_selectivity_a50dev <- function(age_vector, slp, a50, a50_dev) {
  nyears <- length(a50_dev)
  log_sel <- matrix(NA, nyears, length(age_vector))
  for (i in 1:nyears) {
    adj_a50 <- a50 * exp(a50_dev[i])
    log_sel[i, ] <- -log(1 + exp(-slp * (age_vector - adj_a50)))
  }
  return(log_sel)
}

logistic_selectivity_slopeinf_dev <- function(age_vector, slp, a50, se, ae) {
  nyears <- length(se)
  log_sel <- matrix(NA, nyears, length(age_vector))
  for (i in 1:nyears) {
    adj_slp <- exp(se[i]) * slp
    adj_a50 <- a50 * exp(ae[i])
    log_sel[i, ] <- -log(1 + exp(-adj_slp * (age_vector - adj_a50)))
  }
  return(log_sel)
}

double_logistic_selectivity <- function(age_vector, slp, a50, nyears) {
  log_sel <- matrix(NA, nyears, length(age_vector))
  for (i in 1:nyears) {
    asc <- exp(slp[1])
    inf1 <- a50[1]
    dsc <- exp(slp[2])
    inf2 <- a50[2]
    sel <- 1 / (1 + exp(-asc * (age_vector - inf1))) *
      (1 - 1 / (1 + exp(-dsc * (age_vector - inf2))))
    log_sel[i, ] <- log(sel) - max(log(sel))
  }
  return(log_sel)
}

slowgistic_selectivity <- function(age_vector, dif, a50, trm, nyears) {
  log_sel <- matrix(NA, nyears, length(age_vector))
  slp1 <- 2.944 / exp(dif)
  x1 <- dif / 2 + a50
  slp2 <- (0.95 - trm) / (dif / 2 + a50 - max(age_vector))
  intrcpt <- 0.95 - x1 * slp2
  for (i in 1:nyears) {
    log_sel[i, ] <- ifelse(
      age_vector < x1,
      -log(1 + exp(-slp1 * (age_vector - a50))),
      log(intrcpt + slp2 * age_vector)
    )
    log_sel[i, ] <- log_sel[i, ] - max(log_sel[i, ])
  }
  return(log_sel)
}

coefficient_selectivity <- function(coffs, nsel, nages, stsel, nyears, sel_devs = NULL, change_years = NULL, gn = NULL, avgsel_out = TRUE) {
  log_sel <- matrix(0, nyears, nages)
  coffs_exp <- exp(coffs)
  avgsel <- if (avgsel_out) log(mean(coffs_exp)) else NA

  # initialize first year
  log_sel[1, 1:nsel] <- coffs
  log_sel[1, (nsel + 1):nages] <- coffs[nsel]
  log_sel[1, ] <- log_sel[1, ] - mean(log_sel[1, ])

  if (!is.null(sel_devs)) {
    ii <- 1
    for (y in 2:nyears) {
      year_idx <- y - 1 + stsel
      if (!is.null(change_years) && ii <= length(change_years) && year_idx == change_years[ii]) {
        log_sel[y, 1:nsel] <- log_sel[y - 1, 1:nsel] + sel_devs[ii, ]
        log_sel[y, (nsel + 1):nages] <- log_sel[y, nsel]
        ii <- ii + 1
      } else {
        log_sel[y, ] <- log_sel[y - 1, ]
      }
      log_sel[y, ] <- log_sel[y, ] - mean(log_sel[y, ])
    }
  } else {
    for (y in 2:nyears) {
      log_sel[y, ] <- log_sel[y - 1, ]
    }
  }

  list(log_sel = log_sel, avgsel = avgsel)
}
