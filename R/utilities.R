build_model_inputs <- function(filepath, fn = here::here("Rtmb", "rpm.dat")) {
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
  inputs <- read_model_inputs(fn)

  # names(inputs)
  inputs$files <- read_model_files(filepath)
  inputs$files
  Cov_Filename <- here::here("runs", "data", "cov_2024.dat")
  Wtage_file <- here::here("runs", "data", "wtage2024.dat")
  waa <- read_data(Wtage_file)
  # inputs$files$Wtage_file
  inputs$cov_matrix <- if (file.exists(Cov_Filename)) read_matrix(Cov_Filename) else NULL
  # Read optional age comp or ragged data files
  return(inputs)
}

read_model_files <- function(filepath) {
  lines <- readLines(filepath)
  if (length(lines) < 10) stop("Insufficient lines in the input file.")
  # Filenames read from file
  files <- list(
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
  files
}

fn <- here::here("Rtmb", "rpm.dat")
read_model_inputs <- function(fn) {
  data_tmp <- read_data(fn)
  const <- list(
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
    pflag = 0,
    # Vectors and arrays
    selages = c(0.0, 0.0, rep(1.0, 13)),
    sel_avo_in = c(0.0, 1, 1, 0.85, 0.7, 0.55, 0.3, 0.15, 0.05, rep(0.01, 6)),
    Cat_Fut = rep(NA_real_, 10),
    # Local calculations
    lambda_spr_msy = 0.5 / (0.1^2),
    phase_nosr = -1,
    phase_Rzero = -1,
    Steepness_UB = 0.99
    # Assume these are values already defined in full model
  )
  obs_catch <- function(year) 1200 # mockup function for illustration
  endyr_r <- data_tmp$endyr - const$retroYr
  endyr_est <- endyr_r - const$omitSR
  n_selages_fsh <- data_tmp$nages - const$last_age_sel_fsh + 1
  n_selages_bts <- data_tmp$nages - const$last_age_sel_bts + 1
  n_selages_ats <- data_tmp$nages - const$last_age_sel_ats + 1
  nagecomp <- c(data_tmp$n_fsh - const$retroYr, data_tmp$n_bts - const$retroYr, data_tmp$n_ats - const$retroYr)
  # const$nages
  # const$last_age_sel_ats
  data <- c(
    data_tmp,
    const,
    list(
      mina_bts = 2,
      mina_ats = 2,
      nagecomp = nagecomp,
      n_selages_fsh = n_selages_fsh,
      n_selages_bts = n_selages_bts,
      n_selages_ats = n_selages_ats,
      endyr_r = endyr_r,
      endyr_est = endyr_est,
      dec_tab_catch = c(10, 0.25 * obs_catch(endyr_r), 0.50 * obs_catch(endyr_r), 0.75 * obs_catch(endyr_r), 1.00 * obs_catch(endyr_r), 1.25 * obs_catch(endyr_r), 1.50 * obs_catch(endyr_r), 2.00 * obs_catch(endyr_r))
    )
  )
  return(data)
}
preliminary_calcs <- function(data, parameters) {
  # Eq. 21 – initialize wt_fut
  parameters$wt_fut <- parameters$wt_fsh[data$endyr_r, ]

  # Natural mortality setup
  parameters$base_natmort <- data$natmort_in
  parameters$natmort <- parameters$base_natmort

  if (data$Mmatrix == 1 && !is.null(data$M_in)) {
    data$M <- data$M_in
    parameters$base_natmort <- data$M[as.character(data$endyr), ]
    parameters$natmort <- parameters$base_natmort
  }

  # Catch forecast: simple decrement from next_yrs_catch
  data$Cat_Fut <- numeric(10)
  data$Cat_Fut[1] <- data$next_yrs_catch
  for (i in 2:10) {
    data$Cat_Fut[i] <- data$Cat_Fut[i - 1] * 0.9
  }

  # Age composition likelihood offsets
  parameters$age_like_offset <- rep(0, data$ngears)
  parameters$len_like_offset <- 0
  MN_const <- 0.001

  for (igear in seq_len(data$ngears)) {
    for (i in seq_len(data$nagecomp[igear])) {
      if (igear == 1) {
        # Fishery
        p <- data$oac_fsh_data[i, ] / sum(data$oac_fsh_data[i, ])
        data$oac_fsh[i, ] <- p
        parameters$age_like_offset[igear] <- parameters$age_like_offset[igear] -
          data$sam_fsh[i] * sum(p * log(p + MN_const))
      } else if (igear == 2) {
        # BTS
        data$std_ob_bts[i] <- data$ob_bts_std[i]
        data$ot_bts[i] <- sum(data$oac_bts[i, data$mina_bts:data$nages])
        # data$ob_bts[i] <- data$obs_bts_data[i]
        p <- data$oac_bts[i, ] / sum(data$oac_bts[i, ])
        data$oac_bts[i, ] <- p
        parameters$age_like_offset[igear] <- parameters$age_like_offset[igear] -
          data$sam_bts[i] * sum(p * log(p + MN_const))
      } else if (igear == 3) {
        # ATS
        # data$std_ob_ats[i] <- data$std_ob_ats_data[i]
        data$oa1_ats[i] <- data$oac_ats[i, 1]
        data$ot_ats[i] <- sum(data$oac_ats[i, data$mina_ats:data$nages])
        p <- data$oac_ats[i, data$mina_ats:data$nages]
        p <- p / sum(p)
        data$oac_ats[i, data$mina_ats:data$nages] <- p
        parameters$age_like_offset[igear] <- parameters$age_like_offset[igear] -
          data$sam_ats[i] * sum(p * log(p + MN_const))
      }
    }
  }

  # Length likelihood offset
  parameters$len_like_offset <- parameters$len_like_offset -
    50 * sum(data$olc_fsh * log(data$olc_fsh + MN_const))

  # Ignore ATS age-1 if CV too high
  data$ot_ats[data$n_ats] <- sum(data$oac_ats_data[data$n_ats, data$mina_ats:data$nages])
  if (data$ot_ats_std[data$n_ats] / data$ot_ats[data$n_ats] > 0.4) {
    data$ignore_last_ats_age1 <- 1
  } else {
    data$ignore_last_ats_age1 <- 0
  }

  # ATS log-scale observation CV
  lse_ats <- sqrt(log((data$std_ot_ats[1:data$n_ats] / data$ot_ats[1:data$n_ats])^2 + 1))
  data$lvar_ats <- lse_ats^2

  # ATS biomass log-scale CV
  lseb_ats <- data$ob_ats_std / data$ob_ats
  lseb_ats <- sqrt(log(lseb_ats^2 + 1))
  data$lvarb_ats <- lseb_ats^2

  # Age-to-length mapping
  Get_Age2length <- function(age_len_matrix, age_dist) {
    # this just computes expected lengths from age comps
    return(as.numeric(age_dist %*% age_len_matrix))
  }

  data$olc_last <- Get_Age2length(data$age_len, data$oac_fsh[data$n_fsh, ])

  # Return updated objects
  list(data = data, parameters = parameters)
}
read_data <- function(file) {
  lines <- readLines(file)
  result <- list()
  current_name <- NULL
  buffer <- list()

  for (line in lines) {
    line <- trimws(line)
    if (line == "") next
    if (startsWith(line, "#")) {
      if (!is.null(current_name)) {
        result[[current_name]] <- if (length(buffer) == 1) buffer[[1]] else do.call(rbind, buffer)
      }
      current_name <- sub("^#", "", line)
      buffer <- list()
    } else {
      nums <- as.numeric(unlist(strsplit(line, "\\s+")))
      buffer[[length(buffer) + 1]] <- nums
    }
  }

  if (!is.null(current_name)) {
    result[[current_name]] <- if (length(buffer) == 1) buffer[[1]] else do.call(rbind, buffer)
  }

  return(result)
}
# Example usage:
# file_path <- "path/to/your/file.dat"
# data <- read_data(file_path)

# compute_fsh_selectivity <- function(nsel, stsel, endyr_r, nages, avgsel, coffs, sel_devs, nch_fsh, yrs_ch_fsh) {
compute_selectivity_fsh <- function(nsel, stsel, endyr_r, nages, coffs, sel_devs, yrs_ch_fsh) {
  # nsel: number of selectivity ages
  # stsel: starting year (integer)
  # endyr_r: ending year (integer)
  # nages: number of ages
  # avgsel: will be set inside function (pass as 0 or NA)
  # coffs: vector of selectivity coefficients (length nsel)
  # sel_devs: vector of selectivity deviations (length nch_fsh)
  # nch_fsh: number of change points (integer)
  # yrs_ch_fsh: vector of years where selectivity changes (length nch_fsh)

  # Pre-allocate log_sel matrix: rows = years, cols = ages
  nyrs <- endyr_r - stsel + 1
  log_sel <- matrix(0, nrow = nyrs, ncol = nages)
  rownames(log_sel) <- as.character(stsel:endyr_r)
  # 1. Set avgsel
  avgsel <- log(mean(exp(coffs)))
  # 2. Set first year selectivity
  log_sel[1, 1:nsel] <- coffs
  log_sel[1, (nsel + 1):nages] <- coffs[nsel]
  # 3. Center first year
  log_sel[1, ] <- log_sel[1, ] - log(mean(exp(log_sel[1, ])))
  ii <- 1
  nch_fsh <- dim(sel_devs)[1]
  for (i in 1:(nyrs - 1)) { # i is index for year, not actual year
    year <- stsel + i - 1
    if (ii <= nch_fsh) {
      if (year == yrs_ch_fsh[ii]) {
        # Apply deviation
        log_sel[i + 1, 1:nsel] <- log_sel[i, 1:nsel] + sel_devs[ii, ]
        log_sel[i + 1, (nsel + 1):nages] <- log_sel[i + 1, nsel]
        # print(log_sel[i+1,])
        ii <- ii + 1
      } else {
        log_sel[i + 1, ] <- log_sel[i, ]
      }
    } else {
      log_sel[i + 1, ] <- log_sel[i, ]
    }
    # Center
    log_sel[i + 1, ] <- log_sel[i + 1, ] - log(mean(exp(log_sel[i + 1, ])))
  }
  # log_sel
  return(list(avgsel = avgsel, log_sel = log_sel))
}

compute_selectivity_ind <- function(stsel, slp, a50, se, ae, age_vector, endyr_r) {
  # ages <- 1:15
  # stsel <- 1995
  # endyr_r <- 2022
  # slp <- 1.2
  # a50 <- 6
  # se <- rnorm(endyr_r, sd = 0.1)   # deviations on slope
  # ae <- rnorm(endyr_r, sd = 0.05)  # deviations on a50
  #
  nages <- length(age_vector)
  nyrs <- endyr_r - stsel + 1
  log_sel <- matrix(0, nrow = nyrs, ncol = nages)
  rownames(log_sel) <- as.character(stsel:endyr_r)

  for (i in 1:nyrs) {
    # Time-varying slope and a50
    slp_i <- exp(se[i]) * slp
    a50_i <- a50 * exp(ae[i])

    log_sel[i, ] <- -log(1 + exp(-slp_i * (age_vector - a50_i)))
  }
  return(log_sel)
}

# stsel=1994
compute_selectivity_ats_devs <- function(nsel, stsel, endyr_r, coffs, sel_devs) {
  nyrs <- endyr_r - stsel + 1
  log_sel <- matrix(0, nrow = nyrs, ncol = nages)
  rownames(log_sel) <- as.character(stsel:endyr_r)
  dim_sel_ats <- dim(sel_devs)[1]
  # 1. Compute avgsel (on log scale)
  avgsel <- log(mean(exp(coffs)))
  # 2. Set selectivity in first year
  log_sel[1, 2:nsel] <- coffs
  log_sel[1, (nsel + 1):nages] <- coffs[nsel - 1]
  # print(log_sel)
  # 3. Mean-center on arithmetic scale
  log_sel[1, ] <- log_sel[1, ] - log(mean(exp(log_sel[1, ])))
  ii <- 1
  for (i in 2:nyrs) {
    year <- stsel + i - 1
    prev <- log_sel[i - 1, ]
    if (ii <= dim_sel_ats) {
      # Apply deviations to selectivity coefficients
      prev[2:nsel] <- prev[2:nsel] + sel_devs[ii, ]
      prev[(nsel + 1):nages] <- prev[nsel]
      ii <- ii + 1
      # print(prev)
    }
    # Assign and center
    log_sel[i, ] <- prev
    log_sel[i, ] <- log_sel[i, ] - log(mean(exp(log_sel[i, ])))
  }
  return(log_sel)
}

BTS_likelihood <- function() {
  q_bts <- mean(ob_bts) / mean(eb_bts)
  eb_bts_scaled <- eb_bts * q_bts

  if (do_bts_bio) {
    srv_tmp <- ob_bts - eb_bts_scaled
  } else {
    srv_tmp <- ot_bts - et_bts
  }
  val <- switch(as.character(DoCovBTS),
    "0" = {
      if (do_bts_bio) {
        srv_tmp <- log(ob_bts) - log(eb_bts_scaled)
        sum(srv_tmp^2 / (2 * var_ob_bts))
      } else {
        0
      }
    },
    "1" = {
      0.5 * t(srv_tmp) %*% inv_bts_cov %*% srv_tmp
    },
    "2" = {
      sum(sapply(1:n_bts_r, function(i) {
        if (sd_GenGam[i] > 0) {
          dgengamma(ob_bts[i], eb_bts_scaled[i], sd_GenGam[i], q_GenGam[i])
        } else {
          (log(ob_bts[i]) - log(eb_bts_scaled[i]))^2 / (2 * var_ob_bts[i])
        }
      }))
    },
    "3" = {
      if (do_bts_bio) {
        srv_tmp <- log(ob_bts) - log(eb_bts_scaled)
      }
      0 # Placeholder: no likelihood implemented
    }
  )
  return(val)
}

ATS_likelihood <- function() {
  if (do_ats_bio) {
    sum((log(ob_ats + 0.01) - log(eb_ats + 0.01))^2 / (2 * lvarb_ats))
  } else {
    sum((log(ot_ats + 0.01) - log(et_ats + 0.01))^2 / (2 * lvar_ats))
  }
}

ATS_age1_likelihood <- function() {
  if (!use_age1_ats) return(0)
  
  qtmp <- exp(mean(log(oa1_ats) - log(ea1_ats)))
  i_range <- if (ignore_last_ats_age1) 1:(n_ats - 1) else 1:n_ats
  
  resids <- log(oa1_ats[i_range] + 0.01) - log(ea1_ats[i_range] * qtmp + 0.01)
  0.5 * sum(resids^2) / (age1_sigma_ats^2)
}

CPUE_likelihood <- function() {
  cpue_dev <- obs_cpue - pred_cpue
  sum(cpue_dev^2 / (2 * obs_cpue_var))
}

AVO_likelihood <- function() {
  avo_dev <- ob_avo - pred_avo
  sum(avo_dev^2 / (2 * obs_avo_var))
}

Surv_Likelihood <- function() {
  surv_like <- numeric(5)
  surv_like[1] <- BTS_likelihood() * btsEmph
  surv_like[2] <- ATS_likelihood() * atsEmph
  surv_like[3] <- ATS_age1_likelihood()
  surv_like[4] <-  CPUE_likelihood()
  surv_like[5] <- AVO_likelihood()
  return(surv_like)
}




# Helper function for generalized gamma distribution (if not available)
#--Utilities to read parameters from .dat files-----------
read_pars <- function(file) {
  lines <- readLines(file)
  result <- list()
  current_name <- NULL
  buffer <- character()

  flush_buffer <- function() {
    if (!is.null(current_name) && length(buffer) > 0) {
      # Parse all lines into a single numeric vector
      flat_vals <- scan(text = paste(buffer, collapse = "\n"), quiet = TRUE)

      if (length(buffer) == 1) {
        if (length(flat_vals) == 1) {
          result[[current_name]] <<- flat_vals[1] # Scalar
        } else {
          result[[current_name]] <<- flat_vals # Vector (1 row)
        }
      } else {
        # Try to parse as matrix (each line is a row)
        row_list <- lapply(buffer, function(x) scan(text = x, quiet = TRUE))
        row_lengths <- lengths(row_list)

        if (length(unique(row_lengths)) == 1) {
          result[[current_name]] <<- do.call(rbind, row_list) # Matrix
        } else {
          result[[current_name]] <<- unlist(row_list) # Fallback to flat vector
        }
      }
    }
  }

  for (line in lines) {
    line <- trimws(line)
    if (line == "") next

    if (grepl("^#\\s+.+:$", line)) {
      flush_buffer()
      current_name <- sub("^#\\s+", "", sub(":$", "", line))
      buffer <- character()
    } else {
      buffer <- c(buffer, line)
    }
  }

  flush_buffer()
  return(result)
}

read_pars_simple <- function(file) {
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
