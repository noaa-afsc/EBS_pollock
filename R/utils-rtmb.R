#' RTMB/ADMB Utilities
#'
#' Collection of helper functions, readers, selectivity calculators, and likelihoods
#' used when porting ADMB stock assessment models to RTMB.
#' Functions avoid hidden globals, require explicit arguments, and include roxygen2
#' documentation for reproducibility and testing.
#' @keywords internal
NULL

# --- Math Helpers ---
#' @title Simple math helpers
#' @name rtmb_math_helpers
#' @description Utilities commonly used across likelihoods and penalties.
#' @return Numeric scalar or vector.
#' @examples
#' norm2(c(1,2,3))           # 14
#' first_difference(1:5)     # c(1,1,1,1)
#' second_difference(1:5)    # c(0,0,0)
#' cf(c(1,0,0), 5)           # 0
#' sq(3)                     # 9
#' @export
norm2 <- function(x) sum(x^2, na.rm = TRUE)

#' @rdname rtmb_math_helpers
#' @export
first_difference <- function(x) diff(x)

#' @rdname rtmb_math_helpers
#' @export
second_difference <- function(x) diff(diff(x))

#' @rdname rtmb_math_helpers
#' @param ctrl_flag integer/numeric vector of flags
#' @param i one-based index to query
#' @export
cf <- function(ctrl_flag, i) if (i <= length(ctrl_flag)) ctrl_flag[i] else 0

#' @rdname rtmb_math_helpers
#' @export
sq <- function(x) x * x

# --- Output Comparison ---
#' @title Compare RTMB vs ADMB outputs by max |% diff|
#' @description Compares numeric components shared by two named lists.
#' @param rtmb,list ADMB-like output as R objects
#' @param admb,list ADMB-like output as R objects
#' @param tolerance numeric tolerance for \code{all.equal}
#' @return data.frame with variable, equality flag, lengths, max % diff, and correlation
#' @export
compare_outputs_max_pct <- function(rtmb, admb, tolerance = 1e-6) {
  is_ok <- function(x) is.numeric(x) || is.matrix(x) || is.array(x)
  shared_vars <- intersect(names(rtmb), names(admb))

  comps <- lapply(shared_vars, function(var) {
    rt <- rtmb[[var]]; ad <- admb[[var]]
    if (!(is_ok(rt) && is_ok(ad))) {
      return(data.frame(
        variable = var, equal = NA, length_rtmb = NA_integer_,
        length_admb = NA_integer_, max_abs_pct_diff = NA_real_, cor = NA_real_
      ))
    }
    rt_vec <- as.vector(rt); ad_vec <- as.vector(ad)
    if (length(rt_vec) != length(ad_vec)) {
      return(data.frame(
        variable = var, equal = FALSE,
        length_rtmb = length(rt_vec), length_admb = length(ad_vec),
        max_abs_pct_diff = NA_real_, cor = NA_real_
      ))
    }
    eps <- .Machine$double.eps
    pct_diff <- abs(rt_vec - ad_vec) / pmax(abs(ad_vec), eps) * 100
    max_abs_pct_diff <- suppressWarnings(max(pct_diff, na.rm = TRUE))
    if (!is.finite(max_abs_pct_diff)) max_abs_pct_diff <- NA_real_
    equal <- isTRUE(all.equal(rt_vec, ad_vec, tolerance = tolerance))
    cor_val <- suppressWarnings(cor(rt_vec, ad_vec, use = "complete.obs"))
    data.frame(
      variable = var, equal = equal,
      length_rtmb = length(rt_vec), length_admb = length(ad_vec),
      max_abs_pct_diff = max_abs_pct_diff, cor = cor_val
    )
  })
  do.call(rbind, comps)
}

#' @title gt table for RTMB vs ADMB comparison
#' @description Nicely formatted \pkg{gt} table with heatmap styling.
#' @param rtmb,admb lists as in \code{compare_outputs_max_pct}
#' @param tolerance numeric tolerance for equality
#' @param sort_by_diff logical; if FALSE, preserve RTMB list order
#' @return a \code{gt_tbl}
#' @export
gt_compare_table <- function(rtmb, admb, tolerance = 1e-4, sort_by_diff = TRUE) {
  requireNamespace("gt", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("scales", quietly = TRUE)

  is_ok <- function(x) is.numeric(x) || is.matrix(x) || is.array(x)
  rtmb_f <- rtmb[sapply(rtmb, is_ok)]
  admb_f <- admb[sapply(admb, is_ok)]

  comp <- compare_outputs_max_pct(rtmb_f, admb_f, tolerance = tolerance)
  comp <- if (!sort_by_diff) {
    dplyr::mutate(comp, ..order = match(variable, names(rtmb_f))) |>
      dplyr::arrange(..order) |>
      dplyr::select(-..order)
  } else {
    dplyr::arrange(comp, dplyr::desc(max_abs_pct_diff))
  }

  dom <- range(comp$max_abs_pct_diff, na.rm = TRUE)
  if (!all(is.finite(dom))) dom <- c(0, 1)

  gt::gt(comp) |>
    gt::data_color(
      columns = c(max_abs_pct_diff),
      fn = scales::col_numeric(palette = c("white", "yellow", "orangered"), domain = dom)
    ) |>
    gt::data_color(
      columns = c(cor),
      fn = scales::col_numeric(palette = c("red", "white", "darkgreen"), domain = c(-1, 1))
    ) |>
    gt::fmt_number(columns = c(max_abs_pct_diff, cor), decimals = 6) |>
    gt::cols_label(
      variable = "Variable", equal = "Equal (≤ tol)",
      length_rtmb = "Len RTMB", length_admb = "Len ADMB",
      max_abs_pct_diff = "Max |% diff| (RTMB vs ADMB)",
      cor = "Correlation"
    ) |>
    gt::tab_header(
      title = "Comparison of RTMB and ADMB Outputs",
      subtitle = paste("Tolerance =", format(tolerance, scientific = TRUE))
    )
}

# --- Data Readers ---
#' @title Read named parameter blocks from ADMB-style .dat
#' @description Parses blocks like \code{# name:\n<values>}.
#' @param file path to file
#' @return named list of numeric scalars, vectors, or matrices
#' @export
read_pars <- function(file) {
  lines <- readLines(file, warn = FALSE)
  result <- list(); current_name <- NULL; buffer <- character()

  flush_buffer <- function() {
    if (!is.null(current_name) && length(buffer) > 0) {
      flat_vals <- scan(text = paste(buffer, collapse = "\n"), quiet = TRUE)
      if (length(buffer) == 1L) {
        result[[current_name]] <<- if (length(flat_vals) == 1L) flat_vals[1] else flat_vals
      } else {
        row_list <- lapply(buffer, function(x) scan(text = x, quiet = TRUE))
        row_lengths <- lengths(row_list)
        result[[current_name]] <<- if (length(unique(row_lengths)) == 1L) {
          do.call(rbind, row_list)
        } else {
          unlist(row_list)
        }
      }
    }
  }

  for (line in lines) {
    line <- trimws(line); if (line == "") next
    if (grepl("^#\\s+.+:$", line)) {
      flush_buffer()
      current_name <- sub("^#\\s+", "", sub(":$", "", line))
      buffer <- character()
    } else {
      buffer <- c(buffer, line)
    }
  }
  flush_buffer()
  result
}

#' @title Read simple key/value .dat file
#' @description Lightweight variant of \code{read_pars()} (same header format).
#' @param file path
#' @return named list
#' @export
read_pars_simple <- function(file) {
  lines <- readLines(file, warn = FALSE)
  result <- list(); current_name <- NULL; buffer <- character()
  flush_buffer <- function() {
    if (!is.null(current_name)) {
      values <- suppressWarnings(as.numeric(unlist(strsplit(paste(buffer, collapse = " "), "\\s+"))))
      values <- values[is.finite(values)]
      result[[current_name]] <<- if (length(values) <= 1L) as.numeric(values) else values
    }
  }
  for (line in lines) {
    line <- trimws(line); if (line == "") next
    if (grepl("^#\\s+.+:", line)) {
      flush_buffer()
      current_name <- sub("^#\\s+", "", sub(":", "", line))
      buffer <- character()
    } else {
      buffer <- c(buffer, line)
    }
  }
  flush_buffer()
  result
}

#' @title Read file list from control path
#' @param filepath path to a text file listing model filenames line-by-line
#' @return named list of filenames
#' @export
read_model_files <- function(filepath) {
  lines <- readLines(filepath, warn = FALSE)
  if (length(lines) < 10) stop("Expected at least 10 lines in file list.")
  list(
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
}

#' @title Read generic numeric blocks (ADMB-style)
#' @description Reads sections delimited by lines starting with \code{#}.
#' @param file path
#' @return named list of numeric vectors/matrices
#' @export
read_data <- function(file) {
  lines <- readLines(file, warn = FALSE)
  result <- list(); current_name <- NULL; buffer <- list()
  flush <- function() {
    if (!is.null(current_name)) {
      result[[current_name]] <<- if (length(buffer) == 1L) buffer[[1]] else do.call(rbind, buffer)
    }
  }
  for (line in lines) {
    line <- trimws(line); if (line == "") next
    if (startsWith(line, "#")) {
      flush()
      current_name <- sub("^#\\s*", "", line)
      buffer <- list()
    } else {
      nums <- suppressWarnings(as.numeric(unlist(strsplit(line, "\\s+"))))
      nums <- nums[is.finite(nums)]
      buffer[[length(buffer) + 1L]] <- nums
    }
  }
  flush()
  result
}

#' @title Build model inputs (explicit, no hidden globals)
#' @description Wraps \code{read_data()} and adds derived constants used in the RTMB bridge.
#' @param data_fn path to main \code{.dat}-style file
#' @param files_fn optional path to the file-list (see \code{read_model_files})
#' @param cov_fn optional covariance filename
#' @return named list combining raw data and constants
#' @export
build_model_inputs <- function(data_fn,
                               files_fn = NULL,
                               cov_fn = NULL) {

  read_matrix <- function(path) as.matrix(utils::read.table(path, header = FALSE))
  inputs <- list()
  if (!is.null(files_fn)) inputs$files <- read_model_files(files_fn)
  data_tmp <- read_data(data_fn)

  const <- list(
    DoCovBTS = 1L, SrType = 1L, Do_Combined = 0L,
    use_age_err = 0L, use_age1_ats = 1L, age1_sigma_ats = 1,
    use_endyr_len = 0L, use_popwts_ssb = 1L,
    natmortprior = 0.3, cvnatmortprior = 0.1,
    q_all_prior = 0, q_all_sigma = 2, q_bts_prior = 0, q_bts_sigma = 2,
    sigrprior = 1, cvsigrprior = 0.2, phase_sigr = -6,
    steepnessprior = 0.6, cvsteepnessprior = 0.12, phase_steepness = 5,
    use_spr_msy_pen = 0, sigma_spr_msy = 0.20, use_last_ats_ac = 1,
    nyrs_sel_avg = 5, do_bts_bio = 1, do_ats_bio = 1,
    nyrs_future = 5L, next_yrs_catch = 1350,
    nscen = 8L, fixed_catch_fut2 = 1400, fixed_catch_fut3 = 1200,
    phase_F40 = 6, robust_phase = 1350, ats_robust_phase = 1350,
    ats_like_type = 0, phase_logist_fsh = -1, phase_logist_bts = 2,
    phase_seldevs_fsh = 4, phase_seldevs_bts = 5, phase_age1devs_bts = 3,
    phase_selcoffs_ats = 3, phase_sel_ats_dev = 5,
    phase_natmort = -6, phase_q_bts = 3, phase_q_std_area = -4,
    phase_q_ats = 4, phase_bt = -6, phase_rec_devs = 3, phase_larv = -3,
    phase_sr = 5, wt_fut_phase = 6, last_age_sel_fsh = 4,
    last_age_sel_bts = 8, last_age_sel_ats = 8,
    q_amin = 3L, q_amax = 15L,
    selages = c(0.0, 0.0, rep(1.0, 13)),
    sel_avo_in = c(0.0, 1, 1, 0.85, 0.7, 0.55, 0.3, 0.15, 0.05, rep(0.01, 6)),
    lambda_spr_msy = 0.5 / (0.1^2),
    phase_nosr = -1, phase_Rzero = -1, Steepness_UB = 0.99
  )

  retroYr <- if (!is.null(data_tmp$retroYr)) as.integer(data_tmp$retroYr) else 0L
  endyr   <- as.integer(data_tmp$endyr)
  nages   <- as.integer(data_tmp$nages)

  endyr_r   <- endyr - retroYr
  omitSR    <- if (!is.null(data_tmp$omitSR)) as.integer(data_tmp$omitSR) else 0L
  endyr_est <- endyr_r - omitSR

  n_selages_fsh <- nages - const$last_age_sel_fsh + 1L
  n_selages_bts <- nages - const$last_age_sel_bts + 1L
  n_selages_ats <- nages - const$last_age_sel_ats + 1L

  data <- c(
    data_tmp,
    const,
    list(
      mina_bts = 2L, mina_ats = 2L,
      n_selages_fsh = n_selages_fsh,
      n_selages_bts = n_selages_bts,
      n_selages_ats = n_selages_ats,
      endyr_r = endyr_r,
      endyr_est = endyr_est
    )
  )

  if (!is.null(cov_fn) && file.exists(cov_fn)) {
    data$cov_matrix <- read_matrix(cov_fn)
  } else {
    data$cov_matrix <- NULL
  }

  c(inputs, list(data = data))
}

# --- Pre-calculations ---
#' @title Preliminary calculations (explicit I/O)
#' @description RTMB replacement for ADMB PRELIMINARY_CALCS_SECTION.
#' @param data list returned by \code{build_model_inputs()}
#' @param parameters list with at least \code{wt_fsh}, \code{natmort_in}
#' @return list with updated \code{data}, \code{parameters}
#' @export
preliminary_calcs <- function(data, parameters) {
  stopifnot(!is.null(parameters$wt_fsh))
  parameters$wt_fut <- parameters$wt_fsh[data$endyr_r, , drop = TRUE]

  parameters$base_natmort <- data$natmort_in
  parameters$natmort      <- parameters$base_natmort

  data$Cat_Fut <- numeric(10)
  data$Cat_Fut[1] <- data$next_yrs_catch
  for (i in 2:10) data$Cat_Fut[i] <- data$Cat_Fut[i - 1] * 0.9

  parameters$age_like_offset <- rep(0, data$ngears)
  parameters$len_like_offset <- 0
  MN_const <- 0.001

  lse_ats <- sqrt(log((data$std_ot_ats[1:data$n_ats] / data$ot_ats[1:data$n_ats])^2 + 1))
  data$lvar_ats  <- lse_ats^2
  lseb_ats       <- sqrt(log((data$ob_ats_std / data$ob_ats)^2 + 1))
  data$lvarb_ats <- lseb_ats^2

  data$MN_const <- MN_const
  list(data = data, parameters = parameters)
}

# --- Selectivity Calculations ---
#' @title Fishery selectivity with change years (log scale)
#' @description ADMB-style coefficient model with year changes and centering.
#' @param nsel,nages integers; number of sel ages, total ages
#' @param stsel,endyr_r starting/ending year (ints)
#' @param coffs numeric length \code{nsel}
#' @param sel_devs matrix [n_change, nsel]
#' @param yrs_ch_fsh integer vector of change years (length n_change)
#' @return list(avgsel, log_sel[year x age])
#' @export
compute_selectivity_fsh <- function(nsel, stsel, endyr_r, nages, coffs, sel_devs, yrs_ch_fsh) {
  stopifnot(length(coffs) == nsel, ncol(sel_devs) == nsel, length(yrs_ch_fsh) == nrow(sel_devs))
  nyrs <- endyr_r - stsel + 1L
  log_sel <- matrix(0, nrow = nyrs, ncol = nages, dimnames = list(as.character(stsel:endyr_r), NULL))
  avgsel <- log(mean(exp(coffs)))
  log_sel[1, 1:nsel] <- coffs
  log_sel[1, (nsel + 1):nages] <- coffs[nsel]
  log_sel[1, ] <- log_sel[1, ] - log(mean(exp(log_sel[1, ])))

  ii <- 1L
  for (i in 1:(nyrs - 1L)) {
    year <- stsel + i - 1L
    if (ii <= nrow(sel_devs) && year == yrs_ch_fsh[ii]) {
      log_sel[i + 1L, 1:nsel] <- log_sel[i, 1:nsel] + sel_devs[ii, ]
      log_sel[i + 1L, (nsel + 1):nages] <- log_sel[i + 1L, nsel]
      ii <- ii + 1L
    } else {
      log_sel[i + 1L, ] <- log_sel[i, ]
    }
    log_sel[i + 1L, ] <- log_sel[i + 1L, ] - log(mean(exp(log_sel[i + 1L, ])))
  }
  list(avgsel = avgsel, log_sel = log_sel)
}

#' @title Logistic survey selectivity with time-varying slope/a50
#' @param stsel,endyr_r ints; year range
#' @param slp,a50 baseline slope and a50
#' @param se,ae vectors (length endyr_r-stsel+1): log-deviations for slope and a50
#' @param age_vector integer ages (1:nages)
#' @return matrix [year x age] log selectivity
#' @export
compute_selectivity_ind <- function(stsel, slp, a50, se, ae, age_vector, endyr_r) {
  nages <- length(age_vector)
  nyrs  <- endyr_r - stsel + 1L
  stopifnot(length(se) == nyrs, length(ae) == nyrs)
  log_sel <- matrix(0, nrow = nyrs, ncol = nages, dimnames = list(as.character(stsel:endyr_r), NULL))
  for (i in 1:nyrs) {
    slp_i <- exp(se[i]) * slp
    a50_i <- a50 * exp(ae[i])
    log_sel[i, ] <- -log(1 + exp(-slp_i * (age_vector - a50_i)))
  }
  log_sel
}

#' @title ATS selectivity with deviations at change years
#' @param nsel,nages ints
#' @param stsel,endyr_r ints (years)
#' @param coffs length nsel baseline (ages >= mina_ats)
#' @param sel_devs matrix [n_change x nsel]
#' @param mina_ats first modeled age column for ATS (int, ≥1)
#' @param yrs_ch_ats integer vector of change years
#' @return matrix [year x age] (log scale)
#' @export
compute_selectivity_ats_devs <- function(nsel, nages, stsel, endyr_r, coffs, sel_devs,
                                         mina_ats, yrs_ch_ats) {
  stopifnot(ncol(sel_devs) == nsel)
  log_sel <- matrix(0, nrow = endyr_r - stsel + 1L, ncol = nages,
                    dimnames = list(as.character(stsel:endyr_r), as.character(1:nages)))
  avgsel <- log(mean(exp(coffs)))
  log_sel[as.character(stsel), mina_ats:nsel] <- coffs
  log_sel[as.character(stsel), (nsel + 1L):nages] <- log_sel[as.character(stsel), nsel]
  log_sel[as.character(stsel), ] <- log_sel[as.character(stsel), ] - log(mean(exp(log_sel[as.character(stsel), ])))

  ii <- 1L
  for (yr in (stsel + 1L):endyr_r) {
    ychar <- as.character(yr); yprev <- as.character(yr - 1L)
    if (ii <= length(yrs_ch_ats) && yr == yrs_ch_ats[ii]) {
      log_sel[ychar, mina_ats:nsel] <- log_sel[yprev, mina_ats:nsel] + sel_devs[ii, ]
      log_sel[ychar, (nsel + 1L):nages] <- log_sel[ychar, nsel]
      ii <- ii + 1L
    } else {
      log_sel[ychar, ] <- log_sel[yprev, ]
    }
    log_sel[ychar, ] <- log_sel[ychar, ] - log(mean(exp(log_sel[ychar, ])))
  }
  log_sel
}

# --- Likelihoods ---
#' @title Multinomial age-composition likelihood by gear
#' @param oac,eac named lists (fsh,bts,ats) of matrices (rows=years, cols=ages)
#' @param sam named list of effective sample sizes (vectors, length=nrow per gear)
#' @param MN_const small constant inside logs
#' @param age_like_offset named numeric of length 3 (fsh,bts,ats)
#' @param mina_ats,nages ATS column bounds (integers)
#' @return named numeric vector with attribute \code{total}
#' @export
multinomial_likelihood_age <- function(
    oac, eac, sam, MN_const = 0,
    age_like_offset = c(fsh = 0, bts = 0, ats = 0),
    mina_ats, nages) {

  gears <- c("fsh", "bts", "ats")
  if (is.null(names(oac))) names(oac) <- gears
  if (is.null(names(eac))) names(eac) <- gears
  if (is.null(names(sam))) names(sam) <- gears
  stopifnot(all(gears %in% names(oac)),
            all(gears %in% names(eac)),
            all(gears %in% names(sam)))

  for (g in gears) {
    stopifnot(is.matrix(oac[[g]]), is.matrix(eac[[g]]))
    stopifnot(nrow(oac[[g]]) == nrow(eac[[g]]),
              ncol(oac[[g]]) == ncol(eac[[g]]))
    stopifnot(length(sam[[g]]) == nrow(oac[[g]]))
  }

  age_like <- setNames(numeric(3), gears)
  age_like["fsh"] <- -sum(sam$fsh * rowSums(oac$fsh * log(eac$fsh + MN_const))) - age_like_offset["fsh"]
  age_like["bts"] <- -sum(sam$bts * rowSums(oac$bts * log(eac$bts + MN_const))) - age_like_offset["bts"]

  stopifnot(!missing(mina_ats), !missing(nages))
  col_idx <- mina_ats:nages
  oac_ats <- oac$ats[, col_idx, drop = FALSE]
  eac_ats <- eac$ats[, col_idx, drop = FALSE]
  age_like["ats"] <- -sum(sam$ats * rowSums(oac_ats * log(eac_ats + MN_const))) - age_like_offset["ats"]

  structure(age_like, total = sum(age_like))
}

#' @title BTS likelihood (biomass or numbers)
#' @description Supports covariance, lognormal, and placeholder variants.
#' @param ob_bts,ot_bts observed biomass/numbers (vectors)
#' @param eb_bts,et_bts expected biomass/numbers (vectors)
#' @param inv_bts_cov inverse covariance matrix (if \code{DoCovBTS==1})
#' @param var_ob_bts vector of variances for lognormal option
#' @param DoCovBTS 0=lognormal bio, 1=MVN on residuals, 2=lognormal elementwise, 3=reserved
#' @param do_bts_bio logical; use biomass (TRUE) or numbers (FALSE)
#' @return numeric scalar
#' @export
BTS_likelihood <- function(
    ob_bts, ot_bts, eb_bts, et_bts,
    inv_bts_cov = NULL,
    var_ob_bts = NULL,
    DoCovBTS = 1, do_bts_bio = TRUE) {

  if (do_bts_bio) {
    q_bts <- mean(ob_bts) / mean(eb_bts)
    eb_bts_scaled <- eb_bts * q_bts
    srv_tmp <- ob_bts - eb_bts_scaled
  } else {
    srv_tmp <- ot_bts - et_bts
  }

  switch(as.character(DoCovBTS),
    "0" = { # lognormal biomass
      stopifnot(do_bts_bio, !is.null(var_ob_bts))
      z <- log(ob_bts) - log(eb_bts_scaled)
      sum(z^2 / (2 * var_ob_bts))
    },
    "1" = { # MVN residuals
      stopifnot(!is.null(inv_bts_cov))
      as.numeric(0.5 * t(srv_tmp) %*% inv_bts_cov %*% srv_tmp)
    },
    "2" = { # elementwise lognormal
      stopifnot(do_bts_bio, !is.null(var_ob_bts))
      z <- log(ob_bts) - log(eb_bts_scaled)
      sum((z^2) / (2 * var_ob_bts))
    },
    "3" = 0 # placeholder
  )
}

#' @title ATS biomass/number likelihoods
#' @param ob_ats,ot_ats observed biomass / numbers
#' @param eb_ats,et_ats expected biomass / numbers
#' @param lvar_ats,lvarb_ats log-variance vectors (numbers/biomass)
#' @param do_ats_bio logical; TRUE=biomass, FALSE=numbers
#' @return numeric scalar
#' @export
ATS_likelihood <- function(ob_ats, ot_ats, eb_ats, et_ats, lvar_ats, lvarb_ats, do_ats_bio = TRUE) {
  if (do_ats_bio) {
    sum((log(ob_ats + 0.01) - log(eb_ats + 0.01))^2 / (2 * lvarb_ats))
  } else {
    sum((log(ot_ats + 0.01) - log(et_ats + 0.01))^2 / (2 * lvar_ats))
  }
}

#' @title ATS age-1 likelihood
#' @param oa1_ats,ea1_ats observed/expected age-1 numbers (vectors)
#' @param age1_sigma_ats SD for residuals
#' @param use_age1_ats logical; include term
#' @param ignore_last_ats_age1 logical; omit last element
#' @return numeric scalar
#' @export
ATS_age1_likelihood <- function(oa1_ats, ea1_ats, age1_sigma_ats,
                                use_age1_ats = TRUE,
                                ignore_last_ats_age1 = FALSE) {
  if (!use_age1_ats) return(0)
  qtmp <- exp(mean(log(oa1_ats) - log(ea1_ats)))
  i_range <- if (ignore_last_ats_age1) seq_len(length(oa1_ats) - 1L) else seq_len(length(oa1_ats))
  resids <- log(oa1_ats[i_range] + 0.01) - log(ea1_ats[i_range] * qtmp + 0.01)
  0.5 * sum(resids^2) / (age1_sigma_ats^2)
}

#' @title CPUE likelihood (Gaussian on residuals)
#' @param obs_cpue,pred_cpue numeric vectors
#' @param obs_cpue_var variance vector (same length)
#' @return numeric scalar
#' @export
CPUE_likelihood <- function(obs_cpue, pred_cpue, obs_cpue_var) {
  stopifnot(length(obs_cpue) == length(pred_cpue),
            length(obs_cpue_var) == length(obs_cpue))
  dev <- obs_cpue - pred_cpue
  sum(dev^2 / (2 * obs_cpue_var))
}

#' @title AVO likelihood (Gaussian on residuals)
#' @param ob_avo,pred_avo numeric vectors
#' @param obs_avo_var variance vector
#' @return numeric scalar
#' @export
AVO_likelihood <- function(ob_avo, pred_avo, obs_avo_var) {
  stopifnot(length(ob_avo) == length(pred_avo),
            length(obs_avo_var) == length(ob_avo))
  dev <- ob_avo - pred_avo
  sum(dev^2 / (2 * obs_avo_var))
}

#' @title Beverton-Holt with lognormal bias correction (SSB->R)
#' @param Stmp SSB vector (lagged)
#' @param phizero,alpha,Bzero SR parameters
#' @return expected recruits
#' @export
SRecruit <- function(Stmp, phizero, alpha, Bzero) {
  (Stmp / phizero) * exp(alpha * (1 - Stmp / Bzero))
}

#' @title Recruitment likelihood (RAM/EV form)
#' @description Penalizes residuals, deviations, and initials; supports omitting one year.
#' @param yrs_est integer vector of recruitment years (estimation window)
#' @param SSB named or aligned vector of SSB for yrs_est-1
#' @param pred_rec named or aligned vector of predicted recruits (yrs_est)
#' @param log_rec_devs vector of rec devs (yrs_est)
#' @param sigr log SD of recruitment
#' @param log_initdevs vector of initial-age devs
#' @param phizero,Bzero,alpha SR parameters
#' @param omit78 integer flag; if >=1, omit \code{exclude_year} from term (1)
#' @param exclude_year integer year to omit (default 1979)
#' @param srrPrior scalar multiplier on term (1)
#' @param eps small positive to avoid log(0)
#' @return list(rec_like = length-7 numeric, rec_like_total = scalar)
#' @export
recruitment_likelihood <- function(
    yrs_est, SSB, pred_rec, log_rec_devs, sigr, log_initdevs,
    phizero, Bzero, alpha, omit78, exclude_year = 1979, srrPrior = 1, eps = 1e-8) {

  yrs_est <- as.integer(yrs_est)
  yrs_lag <- yrs_est - 1L

  fetch_by_year <- function(x, yrs) {
    if (!is.null(names(x))) {
      out <- x[as.character(yrs)]
      if (any(is.na(out))) stop("Missing names for year-matched vector.")
      unname(out)
    } else {
      if (length(x) < length(yrs)) stop("Unnamed vector too short for yrs_est.")
      as.numeric(x[seq_along(yrs)])
    }
  }

  B_lag    <- fetch_by_year(SSB, yrs_lag)
  R_hat    <- fetch_by_year(pred_rec, yrs_est)
  rec_devs <- fetch_by_year(log_rec_devs, yrs_est)

  sigmaRsq <- sigr * sigr
  rec_like <- numeric(7)

  rec_like[2] <- 1.0 * norm2(log_rec_devs)
  rec_like[4] <- 0.1 * norm2(log_initdevs)

  srmod_rec <- SRecruit(B_lag, phizero, alpha, Bzero)
  SR_resids <- log(R_hat + eps) - log(srmod_rec + eps)

  if (omit78 < 1) {
    rec_like[1] <- 0.5 * norm2(SR_resids + sigmaRsq / 2) / sigmaRsq + length(yrs_est) * log(sigr)
  } else {
    keep <- yrs_est != exclude_year
    rec_like[1] <- sum(0.5 * (SR_resids[keep] + sigmaRsq / 2)^2 / sigmaRsq + log(sigr))
  }
  rec_like[1] <- rec_like[1] * srrPrior

  list(rec_like = rec_like, rec_like_total = sum(rec_like))
}

# --- Wtage Data ---
#' @title Read wtage data (cohort/year SDs and observations)
#' @description Parses compact numeric format with ragged year blocks.
#' @param Wtage_file path
#' @return list with fields: log_sd_coh, log_sd_yr, years, arrays \code{wt_obs}, \code{sd_obs}, etc.
#' @export
read_wtage_data <- function(Wtage_file) {
  lines <- readLines(Wtage_file, warn = FALSE)
  lines <- lines[!grepl("^\\s*#", lines) & nchar(trimws(lines)) > 0]
  all_values <- suppressWarnings(as.numeric(unlist(strsplit(paste(lines, collapse = " "), "\\s+"))))
  all_values <- all_values[is.finite(all_values)]
  idx <- 1L; nextv <- function(n = 1L) { out <- all_values[idx:(idx + n - 1L)]; idx <<- idx + n; out }

  log_sd_coh <- nextv()[1]; log_sd_yr <- nextv()[1]; cur_yr <- nextv()[1]
  styr_wt <- as.integer(nextv()[1]); endyr_wt <- as.integer(nextv()[1]); ndat_wt <- as.integer(nextv()[1])

  nyrs_data <- as.integer(nextv(ndat_wt))
  yrs_data  <- matrix(NA_integer_, ndat_wt, max(nyrs_data))
  for (h in seq_len(ndat_wt)) if (nyrs_data[h] > 0) yrs_data[h, 1:nyrs_data[h]] <- as.integer(nextv(nyrs_data[h]))

  age_st <- as.integer(nextv()[1]); age_end <- as.integer(nextv()[1])
  nages_wt <- age_end - age_st + 1L

  wt_obs <- array(NA_real_, dim = c(ndat_wt, max(nyrs_data), nages_wt))
  sd_obs <- array(NA_real_, dim = c(ndat_wt, max(nyrs_data), nages_wt))
  for (h in seq_len(ndat_wt)) for (i in seq_len(nyrs_data[h])) wt_obs[h, i, ] <- nextv(nages_wt)
  for (h in seq_len(ndat_wt)) for (i in seq_len(nyrs_data[h])) sd_obs[h, i, ] <- nextv(nages_wt)

  list(
    log_sd_coh = log_sd_coh, log_sd_yr = log_sd_yr, cur_yr = cur_yr,
    styr_wt = styr_wt, endyr_wt = endyr_wt, ndat_wt = ndat_wt,
    nyrs_data = nyrs_data, yrs_data = yrs_data,
    age_st = age_st, age_end = age_end, nages_wt = nages_wt,
    wt_obs = wt_obs, sd_obs = sd_obs,
    nscale_parm = ndat_wt - 1L,
    phase_d_scale = if (ndat_wt > 1L) 3L else -1L
  )
}

#' @title Assign wtage data to an environment
#' @description Prefer returning lists; use this only for legacy code expecting globals.
#' @param wtage_data list from \code{read_wtage_data}
#' @param envir environment to assign into (default \code{.GlobalEnv})
#' @export
assign_wtage_data <- function(wtage_data, envir = .GlobalEnv) {
  list2env(wtage_data, envir = envir)
}

#' @title Initialize weight-at-age arrays (functional)
#' @description Returns a list of pre-sized arrays instead of writing globals.
#' @param age_end integer last age
#' @param styr_wt,endyr_wt integers for year range
#' @param ndat_wt integer number of datasets
#' @param nyrs_data integer vector of lengths per dataset
#' @return list with elements \code{mnwt}, \code{wt_inc}, \code{wt_pre}, \code{wt_hat}
#' @export
initialize_arrays <- function(age_end, styr_wt, endyr_wt, ndat_wt, nyrs_data) {
  stopifnot(age_end >= 1L, endyr_wt >= styr_wt, length(nyrs_data) == ndat_wt)
  list(
    mnwt   = numeric(age_end),
    wt_inc = numeric(age_end - 1L),
    wt_pre = matrix(0, nrow = endyr_wt - styr_wt + 1L, ncol = age_end),
    wt_hat = array(0, dim = c(ndat_wt, max(nyrs_data), age_end))
  )
}


# Assuming your parameter object is called 'parameters'
create_map_from_zeros <- function(params) {
  map_list <- list()
  
  for (name in names(params)) {
    param_vals <- params[[name]]
    
    if (is.array(param_vals) || is.matrix(param_vals)) {
      # For arrays/matrices, turn off elements that are 0
      zero_indices <- which(param_vals == 0, arr.ind = TRUE)
      if (nrow(zero_indices) > 0) {
        # Create factor with NAs for zeros
        map_factor <- array(1:length(param_vals), dim = dim(param_vals))
        map_factor[param_vals == 0] <- NA
        map_list[[name]] <- as.factor(map_factor)
      }
    } else {
      # For vectors
      if (any(param_vals == 0)) {
        map_factor <- seq_along(param_vals)
        map_factor[param_vals == 0] <- NA
        map_list[[name]] <- as.factor(map_factor)
      }
    }
  }
  
  return(map_list)
}

# Usage
#map_obj <- create_map_from_zeros(parameters)