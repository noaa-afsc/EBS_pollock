# Helper function for generalized gamma distribution (if not available)
#--Utilities to read parameters from .dat files-----------

norm2 <- function(x) sum(x^2, na.rm = TRUE)
first_difference <- fdiff <- function(x) diff(x) # first_difference
second_difference <- sdiff <- function(x) diff(diff(x)) # second_difference
cf <- function(ctrl_flag, i) if (i <= length(ctrl_flag)) ctrl_flag[i] else 0
sq <- function(x) x * x


# 1. Comparison function with max |% diff|
compare_outputs_max_pct <- function(rtmb, admb, tolerance = 1e-6) {
  is_ok <- function(x) is.numeric(x) || is.matrix(x) || is.array(x)
  shared_vars <- intersect(names(rtmb), names(admb))

  comps <- lapply(shared_vars, function(var) {
    rt <- rtmb[[var]]
    ad <- admb[[var]]

    if (!(is_ok(rt) && is_ok(ad))) {
      return(data.frame(
        variable = var, equal = NA, length_rtmb = NA_integer_,
        length_admb = NA_integer_, max_abs_pct_diff = NA_real_, cor = NA_real_
      ))
    }

    rt_vec <- as.vector(rt)
    ad_vec <- as.vector(ad)

    if (length(rt_vec) != length(ad_vec)) {
      return(data.frame(
        variable = var, equal = FALSE,
        length_rtmb = length(rt_vec), length_admb = length(ad_vec),
        max_abs_pct_diff = NA_real_, cor = NA_real_
      ))
    }

    # % diff relative to ADMB
    eps <- .Machine$double.eps
    pct_diff <- abs(rt_vec - ad_vec) / pmax(abs(ad_vec), eps) * 100
    max_abs_pct_diff <- suppressWarnings(max(pct_diff, na.rm = TRUE))
    if (!is.finite(max_abs_pct_diff)) max_abs_pct_diff <- NA_real_

    equal <- isTRUE(all.equal(rt_vec, ad_vec, tolerance = tolerance))
    cor_val <- suppressWarnings(cor(rt_vec, ad_vec, use = "complete.obs"))

    data.frame(
      variable = var,
      equal = equal,
      length_rtmb = length(rt_vec),
      length_admb = length(ad_vec),
      max_abs_pct_diff = max_abs_pct_diff,
      cor = cor_val
    )
  })

  do.call(rbind, comps)
}

# 2. gt table builder
gt_compare_table <- function(rtmb, admb, tolerance = 1e-4, sort_by_diff = TRUE) {
  requireNamespace("gt", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("scales", quietly = TRUE)

  is_ok <- function(x) is.numeric(x) || is.matrix(x) || is.array(x)
  rtmb_f <- rtmb[sapply(rtmb, is_ok)]
  admb_f <- admb[sapply(admb, is_ok)]

  comp <- compare_outputs_max_pct(rtmb_f, admb_f, tolerance = tolerance)

  # Preserve original variable order if requested
  if (!sort_by_diff) {
    comp <- dplyr::mutate(comp,
      orig_order = match(variable, names(rtmb_f))
    ) |>
      dplyr::arrange(orig_order) |>
      dplyr::select(-orig_order)
  } else {
    comp <- dplyr::arrange(comp, dplyr::desc(max_abs_pct_diff))
  }

  dom <- range(comp$max_abs_pct_diff, na.rm = TRUE)
  if (!all(is.finite(dom))) dom <- c(0, 1)

  gt::gt(comp) |>
    gt::data_color(
      columns = c(max_abs_pct_diff),
      fn = scales::col_numeric(
        palette = c("white", "yellow", "orangered"),
        domain  = dom
      )
    ) |>
    gt::data_color(
      columns = c(cor),
      fn = scales::col_numeric(
        palette = c("red", "white", "darkgreen"),
        domain  = c(-1, 1)
      )
    ) |>
    gt::fmt_number(
      columns = c(max_abs_pct_diff, cor),
      decimals = 6
    ) |>
    gt::cols_label(
      variable         = "Variable",
      equal            = "Equal (≤ tol)",
      length_rtmb      = "Len RTMB",
      length_admb      = "Len ADMB",
      max_abs_pct_diff = "Max |% diff| (RTMB vs ADMB)",
      cor              = "Correlation"
    ) |>
    gt::tab_header(
      title = "Comparison of RTMB and ADMB Outputs",
      subtitle = paste("Tolerance =", format(tolerance, scientific = TRUE))
    )
}

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
    catBio = 200, # 	1	Catch	Biomass	Emphasis
    btsEmph = 1, # 	2	BTS	Emphasis
    recDevs = 1, # 	3	Recruitment	Deviations
    fDevs = 1, # 	4	F_devs
    atsEmph = 1, # 	5	ATS	Emphasis
    avoEmph = 1, # 	6	AVO	Emphasis
    ageComp = 1, # 	7	Age	Comp	General
    ageFish = 1, # 	8	Fishery	AgeComp	Emph
    ageBTS = 1, # 	9	BTS	AgeComp	Emph
    selTFsh = 1, # 	10	Fishery	Selex	Time	Emph
    selCFsh = 1, # 	11	Fishery	Curv	Emph
    cpueFsh = 1, # 	12	Fishery	CPUE	Emph
    domFish = 12.5, # 	13	Fishery	Dome	Emph
    domBTS = 1, # 	14	BTS	Dome	Emph
    selATS = 1, # 	15	ATS	Selex	Emph
    yrsFixF = 1, # 	16	Fishery	Sel	Yrs	Fixed
    yrsFixB = 1, # 	17	BTS	Sel	Yrs	Fixed
    resv18 = 1, # 	18	Reserved
    selCurv = 1, # 	19	Survey	Sel	Curvature
    btsVarT = 3.125, # 	20	BTS	Time	Variability
    selTBTS = 5, # 	21	BTS	Selex	Time	Emph
    selTATS = 1, # 	22	ATS	Selex	Time	Emph
    larvDev = 5, # 	23	Larval	Rec-Devs
    rec78on = 1, # 	24	Recruits	1978+
    omit78 = 1, # 	25	Ignore	1978	in	SRR
    selVarbts = 2, # 	26	selVarbts
    sel3dif = 0, # 	27	Fishery	Selex	3rd	Diff
    retroYr = 0, # 	28	Retrospective	Year
    omitSR = 2, # 	29	Omit	Recent	SRR	Yrs
    srrPrior = 1, # 	30	SRR	Prior	Only
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
  data$MN_const <- MN_const
  # weight age readin
  # Usage:
  # wtage_data <- read_wtage_data("weight_data.dat")
  # assign_wtage_data(wtage_data)  # Now all variables are in global environment

  # Return updated objects
  # list(data = c(data,wtage_data) , parameters = parameters)
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
  # stsel = styr_bts;
  # slp = sel_slp_bts;
  # a50 = sel_a50_bts;
  # se = sel_slp_bts_dev;
  # ae = sel_a50_bts_dev;
  # age_vector = 1:nages;
  # endyr_r = endyr_r
  nages <- length(age_vector)
  nyrs <- endyr_r - stsel + 1
  log_sel <- matrix(0, nrow = nyrs, ncol = nages)
  rownames(log_sel) <- as.character(stsel:endyr_r)

  for (i in 1:nyrs) {
    # Time-varying slope and a50
    slp_i <- exp(se[i]) * slp
    a50_i <- a50 * exp(ae[i])

    log_sel[i, ] <- -log(1 + exp(-slp_i * (age_vector - a50_i)))
    # log_sel(i)  =  -log( 1.0 + exp(-exp(se(i)) * slp * ( age_vector - a50*exp(ae(i)) )  ))  ;
  }
  return(log_sel)
}
# exp(log_sel)
# # stsel=1994
# compute_selectivity_ats_devs <- function(nsel, stsel, endyr_r, coffs, sel_devs) {
#   nyrs <- endyr_r - stsel + 1
#   log_sel <- matrix(0, nrow = nyrs, ncol = nages)
#   rownames(log_sel) <- as.character(stsel:endyr_r)
#   dim_sel_ats <- dim(sel_devs)[1]
#   # 1. Compute avgsel (on log scale)
#   avgsel <- log(mean(exp(coffs)))
#   # 2. Set selectivity in first year
#   log_sel[1, 2:nsel] <- coffs
#   log_sel[1, (nsel + 1):nages] <- coffs[nsel - 1]
#   # print(log_sel)
#   # 3. Mean-center on arithmetic scale
#   log_sel[1, ] <- log_sel[1, ] - log(mean(exp(log_sel[1, ])))
#   ii <- 1
#   yrs_ch_ats <- 1995:2024
#   for (i in 2:nyrs) {
#     year <- stsel + i - 1
#     prev <- log_sel[i - 1, ]
#     if (year == yrs_ch_ats[ii]) {
#       # Apply deviations to selectivity coefficients
#       prev[2:nsel] <- prev[2:nsel] + sel_devs[ii, ]
#       prev[(nsel + 1):nages] <- prev[nsel]
#       if (ii <= dim_sel_ats){
#         ii <- ii + 1
#       }
#       # print(prev)
#     }else{
#
#     }
#
#     # Assign and center
#     log_sel[i, ] <- prev
#     log_sel[i, ] <- log_sel[i, ] - log(mean(exp(log_sel[i, ])))
#   }
#   return(log_sel)
# }

compute_selectivity_ats_devs <- function(nsel, nages, stsel, endyr_r, coffs, sel_devs,
                                         mina_ats, yrs_ch_ats) {
  dim_sel_ats <- dim(sel_devs)[1]
  # Initialize log selectivity matrix: years x ages
  log_sel <- matrix(0,
    nrow = endyr_r - stsel + 1, ncol = nages,
    dimnames = list(stsel:endyr_r, 1:nages)
  )

  # Calculate average selectivity from coffs
  avgsel <- log(mean(exp(coffs)))

  # Insert baseline selectivity for first year (stsel)
  log_sel[as.character(stsel), mina_ats:nsel] <- coffs
  log_sel[as.character(stsel), (nsel + 1):nages] <- log_sel[as.character(stsel), nsel] #<- coffs

  # Normalize selectivity for stsel
  log_sel[as.character(stsel), ] <- log_sel[as.character(stsel), ] -
    log(mean(exp(log_sel[as.character(stsel), ])))

  # Apply year-specific deviations
  ii <- 1
  yr <- stsel + 1
  for (yr in (stsel + 1):endyr_r) {
    ychar <- as.character(yr)
    yprev <- as.character(yr - 1)

    if (ii <= length(yrs_ch_ats) && yr == yrs_ch_ats[ii]) {
      # Apply deviation to first nsel ages
      log_sel[ychar, mina_ats:nsel] <- log_sel[yprev, mina_ats:nsel] + sel_devs[ii, ]
      # Extend last sel value to older ages
      log_sel[ychar, (nsel + 1):nages] <- log_sel[ychar, nsel]

      if (ii < dim_sel_ats) {
        ii <- ii + 1
      }
    } else {
      # Carry forward previous year's selectivity
      log_sel[ychar, ] <- log_sel[yprev, ]
    }

    # Normalize to ensure mean exp(log_sel) = 1
    log_sel[ychar, ] <- log_sel[ychar, ] - log(mean(exp(log_sel[ychar, ])))
  }

  return(log_sel)
}

# Multinomial likelihood for age compositions by gear
# Mirrors the ADMB snippet you provided.
multinomial_likelihood_age <- function(
    oac, # named list of observed proportions matrices, e.g. list(fsh=..., bts=..., ats=...)
    eac, # named list of expected proportions matrices (same shapes as oac)
    sam, # named list of effective sample-size vectors, e.g. list(fsh=..., bts=..., ats=...)
    MN_const = 0, # small constant added inside log (ADMB uses MN_const)
    age_like_offset = NULL, # numeric vector of length ngears; default 0
    mina_ats = NULL, # first ATS column (age) to include (integer)
    nages = NULL # last ATS column (age) to include (integer)
    ) {
  gears <- c("fsh", "bts", "ats")
  if (is.null(names(oac)) && length(oac) == 3) names(oac) <- gears
  if (is.null(names(eac)) && length(eac) == 3) names(eac) <- gears
  if (is.null(names(sam)) && length(sam) == 3) names(sam) <- gears
  # Gears expected (1=fsh, 2=bts, 3=ats) to mirror the ADMB switch
  gears <- c("fsh", "bts", "ats")
  stopifnot(
    all(gears %in% names(oac)),
    all(gears %in% names(eac)),
    all(gears %in% names(sam))
  )

  # Basic checks
  for (g in gears) {
    if (!is.matrix(oac[[g]]) || !is.matrix(eac[[g]])) {
      stop(sprintf("oac[['%s']] and eac[['%s']] must be matrices.", g, g))
    }
    if (!is.numeric(sam[[g]])) {
      stop(sprintf("sam[['%s']] must be numeric.", g))
    }
    if (nrow(oac[[g]]) != nrow(eac[[g]])) {
      stop(sprintf("Row counts differ for gear '%s'.", g))
    }
    if (length(sam[[g]]) != nrow(oac[[g]])) {
      stop(sprintf("Length of sam[['%s']] must equal nrow of oac[['%s']].", g, g))
    }
    if (ncol(oac[[g]]) != ncol(eac[[g]])) {
      stop(sprintf("Column counts differ for gear '%s'.", g))
    }
  }

  # Default offsets to zero if not supplied
  if (is.null(age_like_offset)) age_like_offset <- c(fsh = 0, bts = 0, ats = 0)
  if (is.null(names(age_like_offset))) names(age_like_offset) <- gears
  age_like <- setNames(numeric(3), gears)

  # Gear 1: fsh
  ll_fsh_i <- sam$fsh * rowSums(oac$fsh * log(eac$fsh + MN_const))
  age_like["fsh"] <- -sum(ll_fsh_i) - age_like_offset["fsh"]

  # Gear 2: bts
  ll_bts_i <- sam$bts * rowSums(oac$bts * log(eac$bts + MN_const))
  age_like["bts"] <- -sum(ll_bts_i) - age_like_offset["bts"]

  # Gear 3: ats, restricted to columns mina_ats:nages
  if (is.null(mina_ats) || is.null(nages)) {
    stop("For 'ats', please provide both 'mina_ats' and 'nages' (column indices).")
  }
  col_idx <- mina_ats:nages
  if (min(col_idx) < 1 || max(col_idx) > ncol(oac$ats)) {
    stop("ATS column range (mina_ats:nages) is outside matrix bounds.")
  }

  oac_ats <- oac$ats[, col_idx, drop = FALSE]
  eac_ats <- eac$ats[, col_idx, drop = FALSE]
  ll_ats_i <- sam$ats * rowSums(oac_ats * log(eac_ats + MN_const))
  age_like["ats"] <- -sum(ll_ats_i) - age_like_offset["ats"]

  # Return per-gear and total (to mimic an ADMB vector and a sum)
  structure(age_like, total = sum(age_like))
}
BTS_likelihood <- function(
    ob_bts, ot_bts, eb_bts, et_bts, # observed and expected values
    inv_bts_cov, # variance and inverse covariance
    var_ob_bts,
    DoCovBTS = 1, do_bts_bio = TRUE # flags for likelihood type and biology) {
    ) {
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
        (log(ob_bts[i]) - log(eb_bts_scaled[i]))^2 / (2 * var_ob_bts[i])
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

ATS_likelihood <- function(
    ob_ats, ot_ats, eb_ats, et_ats, # observed and expected values
    lvar_ats, lvarb_ats, # log-scale variances
    do_ats_bio = TRUE # flag for biology likelihood type) {
    ) {
  if (do_ats_bio) {
    sum((log(ob_ats + 0.01) - log(eb_ats + 0.01))^2 / (2 * lvarb_ats))
  } else {
    sum((log(ot_ats + 0.01) - log(et_ats + 0.01))^2 / (2 * lvar_ats))
  }
}

ATS_age1_likelihood <- function(
    oa1_ats, ea1_ats, # observed and expected age-1 ATS values
    age1_sigma_ats, # standard deviation for ATS age-1 likelihood
    use_age1_ats = TRUE, # flag to use age-1 ATS likelihood
    ignore_last_ats_age1 = FALSE, # flag to ignore last ATS age-1 value
    n_ats = length(oa1_ats) # number of ATS ages{
    ) {
  if (!use_age1_ats) {
    return(0)
  }

  qtmp <- exp(mean(log(oa1_ats) - log(ea1_ats)))
  i_range <- if (ignore_last_ats_age1) 1:(n_ats - 1) else 1:n_ats

  resids <- log(oa1_ats[i_range] + 0.01) - log(ea1_ats[i_range] * qtmp + 0.01)
  0.5 * sum(resids^2) / (age1_sigma_ats^2)
}

CPUE_likelihood <- function(
    obs_cpue, pred_cpue, obs_cpue_var # observed and predicted CPUE values and their variance) {
    ) {
  cpue_dev <- obs_cpue - pred_cpue
  sum(cpue_dev^2 / (2 * obs_cpue_var))
}

AVO_likelihood <- function(
    ob_avo, pred_avo, obs_avo_var # observed and predicted AVO values and their variance
    ) {
  avo_dev <- ob_avo - pred_avo
  sum(avo_dev^2 / (2 * obs_avo_var))
}

# Surv_Likelihood <- function() {
#   surv_like <- numeric(5)
#   surv_like[1] <- BTS_likelihood() * btsEmph
#   surv_like[2] <- ATS_likelihood() * atsEmph
#   surv_like[3] <- ATS_age1_likelihood()
#   surv_like[4] <-  CPUE_likelihood()
#   surv_like[5] <- AVO_likelihood()
#   return(surv_like)
# }

catch_like <- function(obs_catch, pred_catch, eps = 1e-4) {
  # obs_catch and pred_catch are numeric vectors for years styr:endyr_r
  stopifnot(
    is.numeric(obs_catch), is.numeric(pred_catch),
    length(obs_catch) == length(pred_catch)
  )
  diffs <- log(obs_catch + eps) - log(pred_catch + eps)
  sum(diffs^2, na.rm = TRUE) # norm2
}

# log_sel_fsh: matrix [year, age] (rows are years, cols are ages), values are *log* selectivity
# year_index: named integer vector mapping year -> row index in log_sel_* (e.g., setNames(seq_along(years), years))
selectivity_like_fsh <- function(
    log_sel_fsh,
    styr, endyr_r, # year numbers
    n_selages_fsh, # number of selectivity ages for fishery
    yrs_ch_fsh, nch_fsh, # change years and count
    nyrs, domFish, selTFsh,
    selCFsh, selCurv, sel_devs_fsh, # vector
    sel_ch_sig_fsh,
    year_index # length = nch_fsh
    ) {
  shape_pen <- 0
  # NOTE: ADMB loop shows j<=n_selages_fsh then uses j+1; the safe R version is 1:(n_selages_fsh-1)
  for (i in 1:nyrs) {
    # i <- year_index[[as.character(yy)]]
    for (j in 1:(n_selages_fsh - 1)) {
      if (log_sel_fsh[i, j] > log_sel_fsh[i, j + 1]) {
        d <- log_sel_fsh[i, j] - log_sel_fsh[i, j + 1]
        shape_pen <- shape_pen + domFish * d * d
      }
    }
  }
  p1 <- p2 <- p3 <- dev_pen <- 0
  # ctrl_flag(10)/group_num_fsh * norm2(sel_devs_fsh)
  p1 <- selTFsh * norm2(sel_devs_fsh)

  # For each change year: curvature + random‑walk between year and previous
  p2 <- selCFsh / nch_fsh * norm2(sdiff(log_sel_fsh[1, ]))

  k <- 55
  for (k in seq_len(nch_fsh)) {
    yy <- yrs_ch_fsh[k]
    ik <- year_index[[as.character(yy)]]
    p2 <- p2 + selCFsh / nch_fsh * norm2(sdiff(log_sel_fsh[ik, ]))

    ip <- year_index[[as.character(yy - 1)]]
    sigma <- sel_ch_sig_fsh[k]

    p3 <- p3 + norm2(log_sel_fsh[ip, ] - log_sel_fsh[ik, ]) / (2 * sigma^2)

    # print(c(yy, p3,norm2(log_sel_fsh[ip, ] - log_sel_fsh[ik, ]) / (2 * sigma^2)))
  }

  p1
  p2
  p3
  list(shape = shape_pen, dev = p1 + p2 + p3, total = shape_pen + dev_pen)
}
# If you later re-enable the ctrl_flag(19)/transpose time-smoothness block,
# we can tack it back on. For now this mirrors your pared-down code.
selectivity_like_bts <- function(
    q_amin = 3, q_amax = nages, # age band for BTS time-smooth penalty when ctrl_flag[19] > 0
    # timelines / indexing
    styr, endyr_r, # overall model start/end (year numbers)
    styr_bts, # survey starts (year numbers)
    year_index, # named integer vector mapping year -> row index in matrices
    selCurv,
    selVarbts,
    nages,
    # selectivity objects (log scale)
    log_sel_bts, # matrix [year, age]
    # “active()” flags corresponding to ADMB’s active()
    active_sel_coffs_bts = FALSE,
    active_sel_devs_bts = FALSE,
    active_sel_a501_fsh_dev = TRUE,
    active_sel_a50_bts_dev = TRUE,
    active_sel_age_one_bts_dev = TRUE,
    # deviation vectors (only used if active=TRUE)
    sel_devs_bts = numeric(0),
    sel_a50_bts_dev = numeric(0),
    sel_slp_bts_dev = numeric(0),
    sel_age_one_bts_dev) {
  shape_pen <- 0 # shape penalties (monotonicity etc.)
  dev_pen <- 0 # smoothness / deviation penalties
  log_sel_tmp <- rbind(rep(0, 15), log_sel_bts)
  rownames(log_sel_tmp) <- c(as.character(styr_bts - 1), rownames(log_sel_bts))

  ## ---- BTS smoothness & deviations ---------------------------------------
  if (isTRUE(active_sel_coffs_bts)) {
    if (isTRUE(active_sel_devs_bts)) {
      dev_pen <- dev_pen + btsVarT / group_num_bts * norm2(sel_devs_bts)

      # curvature at start year (global styr as in ADMB code)
      i0 <- year_index[[as.character(styr_bts)]]
      dev_pen <- dev_pen + selTBTS / ncol(log_sel_tmp) *
        norm2(second_difference(log_sel_tmp[i0, ]))

      # for i = styr .. endyr_r-1, if divisible by group_num_bts, penalize curvature at i+1
      for (yy in seq(styr_bts, endyr_r - 1)) {
        if ((yy %% group_num_bts) == 0) {
          inext <- year_index[[as.character(yy + 1)]]
          if (!is.null(inext)) {
            dev_pen <- dev_pen + selTBTS / ncol(log_sel_tmp) *
              norm2(second_difference(log_sel_tmp[inext, ]))
          }
        }
      }
    } else {
      # no time devs: curvature at start year
      i0 <- year_index[[as.character(styr_bts)]]
      dev_pen <- dev_pen + selTBTS *
        norm2(second_difference(log_sel_tmp[i0, ]))
    }
  }

  # BTS alternative penalties depending on ctrl_flag(19)
  if (isTRUE(active_sel_a50_bts_dev)) {
    if (selCurv > 0) {
      # transpose: operate across time for each age j in [q_amin, q_amax)
      for (j in q_amin:(q_amax - 1)) {
        # differences across years for a given age j
        dev_pen <- dev_pen + (selVarbts * norm2(first_difference(log_sel_tmp[, j])))
        # print(c(j, dev_pen))
      }
    } else {
      dev_pen <- dev_pen + 50.0 * norm2(first_difference(sel_a50_bts_dev))
      dev_pen <- dev_pen + 50.0 * norm2(first_difference(sel_slp_bts_dev))
    }
    if (isTRUE(active_sel_age_one_bts_dev)) {
      dev_pen <- dev_pen + 8.0 * norm2(first_difference(sel_age_one_bts_dev))
      # alt in comments: 3.125 -> 40% CV
    }
  }
  list(shape = 0, dev = dev_pen, total = dev_pen)
}
# log_sel_ats: matrix [year, age] (log scale)
selectivity_like_ats <- function(
    log_sel_ats,
    styr, styr_ats, endyr_r, # year numbers (styr used for curvature term; styr_ats for shape loop)
    mina_ats, n_selages_ats, # age bounds used in ADMB loop
    yrs_ch_ats, nch_ats, # change years and count
    selATS, selTATS, # selectivity coefficients
    sel_ch_sig_ats, # length = nch_ats
    year_index) {
  # Shape: penalize *increases* with age (j -> j+1)
  shape_pen <- 0
  yy <- 1995
  for (yy in seq(styr_ats, endyr_r)) {
    i <- year_index[[as.character(yy)]]
    if (is.null(i)) next
    for (j in mina_ats:(n_selages_ats - 1)) {
      if (log_sel_ats[i, j] < log_sel_ats[i, j + 1]) {
        d <- log_sel_ats[i, j] - log_sel_ats[i, j + 1]
        shape_pen <- shape_pen + selATS * d * d
      }
    }
  }

  # Dev: curvature at styr, plus curvature at change years, plus RW steps
  like_tmp1 <- 0
  like_tmp2 <- 0

  i0 <- year_index[[as.character(styr_ats)]]
  # like_tmp1 <- like_tmp1 + selTATS * norm2(sdiff(log_sel_ats[i0, ]))

  for (k in seq_len(nch_ats)) {
    yy <- yrs_ch_ats[k]
    ik <- year_index[[as.character(yy)]]
    like_tmp1 <- like_tmp1 + selTATS * norm2(sdiff(log_sel_ats[ik, ]))
    ip <- year_index[[as.character(yy - 1)]]
    sigma <- sel_ch_sig_ats[k]
    like_tmp2 <- like_tmp2 + norm2(log_sel_ats[ip, ] - log_sel_ats[ik, ]) / (2 * sigma^2)
  }

  dev_pen <- like_tmp1 + like_tmp2
  list(shape = shape_pen, dev = dev_pen, total = shape_pen + dev_pen)
}

# helper
SRecruit <- function(Stmp, phizero, alpha, Bzero) {
  RecTmp <- (Stmp / phizero) * exp(alpha * (1 - Stmp / Bzero))
  return(RecTmp)
}
# ADMB -> R: simplified Recruitment_Likelihood
# yrs_est<- 1979:2023 # example years for estimationbbbbbbbbbbbb
# eps=1e-8 # small value to avoid log(0)
# exclude_year=1979
recruitment_likelihood <- function(
    yrs_est, # integer vector: styr_est:endyr_est
    SSB,
    pred_rec,
    log_rec_devs,
    sigr,
    log_initdevs,
    phizero,
    Bzero,
    alpha,
    omit78,
    exclude_year = 1979, # special-case exclusion
    srrPrior,
    eps = 1e-8 # to avoid log(0)
    ) {
  yrs_est <- as.integer(yrs_est)
  yrs_lag <- yrs_est - 1L

  # helper: fetch by year names if available, otherwise assume aligned order
  fetch_by_year <- function(x, yrs) {
    if (!is.null(names(x))) {
      out <- x[as.character(yrs)]
      if (any(is.na(out))) stop("Missing values when matching by year names.")
      unname(out)
    } else {
      if (length(x) < length(yrs)) stop("Vector too short and has no names for matching.")
      as.numeric(x[seq_along(yrs)])
    }
  }

  B_lag <- fetch_by_year(SSB, yrs_lag)
  R_hat <- fetch_by_year(pred_rec, yrs_est)
  rec_devs <- fetch_by_year(log_rec_devs, yrs_est)

  sigmaRsq <- sigr * sigr
  rec_like <- numeric(7)

  # (2) penalty on all rec devs
  rec_like[2] <- 1.0 * norm2(log_rec_devs)

  # (4) penalty on initial age-comp devs
  rec_like[4] <- 0.1 * norm2(log_initdevs)

  # variability of historical rec devs over the estimation window
  sigmarsq_out <- norm2(rec_devs) / length(rec_devs)

  # model recruits: 1-year lag with SSB
  srmod_rec <- SRecruit(B_lag, phizero, alpha, Bzero)

  # residuals on log-scale (RAM expected value form uses SR_resids + sigmaRsq/2)
  SR_resids <- log(R_hat + eps) - log(srmod_rec + eps)

  # (1) main residual likelihood, with optional exclusion of 'exclude_year'
  if (omit78 < 1) {
    rec_like[1] <- 0.5 * norm2(SR_resids + sigmaRsq / 2) / sigmaRsq +
      length(yrs_est) * log(sigr)
  } else {
    keep <- yrs_est != exclude_year
    rec_like[1] <- sum(0.5 * (SR_resids[keep] + sigmaRsq / 2)^2 / sigmaRsq + log(sigr))
  }

  # final scaling of (1)
  rec_like[1] <- rec_like[1] * srrPrior

  list(
    rec_like = rec_like,
    rec_like_total = sum(rec_like)
    # sigmarsq_out = sigmarsq_out,
    # srmod_rec = setNames(srmod_rec, yrs_est),
    # SR_resids = setNames(SR_resids, yrs_est)
  )
}

read_wtage_data <- function(Wtage_file) {
  cat("Opening", Wtage_file, "\n")

  # Read the entire file as lines
  lines <- readLines(Wtage_file)

  # Remove comments and empty lines
  lines <- lines[!grepl("^\\s*#", lines) & nchar(trimws(lines)) > 0]

  # Convert to numeric values, splitting by whitespace
  all_values <- as.numeric(unlist(strsplit(paste(lines, collapse = " "), "\\s+")))
  all_values <- all_values[!is.na(all_values)] # Remove any NAs

  # Parse values sequentially
  idx <- 1

  # Basic parameters
  log_sd_coh <- all_values[idx]
  idx <- idx + 1
  log_sd_yr <- all_values[idx]
  idx <- idx + 1
  cur_yr <- all_values[idx]
  idx <- idx + 1
  styr_wt <- as.integer(all_values[idx])
  idx <- idx + 1
  endyr_wt <- as.integer(all_values[idx])
  idx <- idx + 1
  ndat_wt <- as.integer(all_values[idx])
  idx <- idx + 1

  # Number of years of observations in each dataset
  nyrs_data <- as.integer(all_values[idx:(idx + ndat_wt - 1)])
  idx <- idx + ndat_wt

  # Years of observations (ragged array)
  yrs_data <- matrix(NA, ndat_wt, max(nyrs_data))
  for (h in 1:ndat_wt) {
    if (nyrs_data[h] > 0) {
      yrs_data[h, 1:nyrs_data[h]] <- as.integer(all_values[idx:(idx + nyrs_data[h] - 1)])
      idx <- idx + nyrs_data[h]
    }
  }

  # LOCAL_CALCS equivalent - adjust nyrs_data if endyr_r < endyr
  # (This assumes endyr_r and endyr are available in the global environment)
  if (exists("endyr_r") && exists("endyr") && exists("styr")) {
    if (endyr_r < endyr) {
      for (h in 1:ndat_wt) {
        itmp <- 1
        for (i in styr:endyr_r) {
          if (!is.na(yrs_data[h, itmp]) && i == yrs_data[h, itmp]) {
            itmp <- itmp + 1
          }
        }
        nyrs_data[h] <- itmp - 1
      }
    }
  }

  # Age range
  age_st <- as.integer(all_values[idx])
  idx <- idx + 1
  age_end <- as.integer(all_values[idx])
  idx <- idx + 1

  # Derived values
  nages_wt <- age_end - age_st + 1
  nscale_parm <- ndat_wt - 1

  # Read 3D arrays - wt_obs and sd_obs
  # Dimensions: (ndat_wt, max(nyrs_data), nages_wt)

  # Initialize arrays
  wt_obs <- array(NA, dim = c(ndat_wt, max(nyrs_data), nages_wt))
  sd_obs <- array(NA, dim = c(ndat_wt, max(nyrs_data), nages_wt))

  # Read weight observations
  for (h in 1:ndat_wt) {
    for (i in 1:nyrs_data[h]) {
      for (j in 1:nages_wt) {
        wt_obs[h, i, j] <- all_values[idx]
        idx <- idx + 1
      }
    }
  }

  # Read standard deviation observations
  for (h in 1:ndat_wt) {
    for (i in 1:nyrs_data[h]) {
      for (j in 1:nages_wt) {
        sd_obs[h, i, j] <- all_values[idx]
        idx <- idx + 1
      }
    }
  }

  # Set phase for d_scale
  if (ndat_wt > 1) {
    phase_d_scale <- 3
  } else {
    phase_d_scale <- -1
  }

  # Override phase_d_scale based on phase_coheff if it exists
  if (exists("phase_coheff")) {
    if (phase_coheff > 0) {
      phase_d_scale <- 3
    } else {
      phase_d_scale <- -1
    }
  }

  # Return as a list
  wtage_data <- list(
    log_sd_coh = log_sd_coh,
    log_sd_yr = log_sd_yr,
    cur_yr = cur_yr,
    styr_wt = styr_wt,
    endyr_wt = endyr_wt,
    ndat_wt = ndat_wt,
    nyrs_data = nyrs_data,
    yrs_data = yrs_data,
    age_st = age_st,
    age_end = age_end,
    nages_wt = nages_wt,
    nscale_parm = nscale_parm,
    wt_obs = wt_obs,
    sd_obs = sd_obs,
    phase_d_scale = phase_d_scale
  )

  return(wtage_data)
}

# Usage example:
# wtage_data <- read_wtage_data("weight_data.dat")
#
# # Extract individual components if needed:
# log_sd_coh <- wtage_data$log_sd_coh
# wt_obs <- wtage_data$wt_obs
# # etc.

# Alternative: Assign to global environment
assign_wtage_data <- function(wtage_data) {
  list2env(wtage_data, envir = .GlobalEnv)
}

# Usage:
# wtage_data <- read_wtage_data("weight_data.dat")
# assign_wtage_data(wtage_data)  # Now all variables are in global environment

# Initialize all your working arrays first
initialize_arrays <- function() {
  mnwt <<- numeric(age_end)
  wt_inc <<- numeric(age_end - 1)
  wt_pre <<- matrix(0, nrow = endyr_wt - styr_wt + 1, ncol = age_end)
  wt_hat <<- array(0, dim = c(ndat_wt, max(nyrs_data), age_end))
  # ... other arrays as needed
}
