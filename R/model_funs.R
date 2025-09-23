#' Compute fishery selectivity with time-varying deviations
#'
#' @description
#' Computes log-scale selectivity for fishery gear across years and ages,
#' allowing for time-varying changes at specified years.
#'
#' @param nsel Integer. Number of selectivity ages (parameters)
#' @param stsel Integer. Starting year for selectivity calculations
#' @param endyr Integer. Ending year for selectivity calculations
#' @param nages Integer. Total number of ages in the model
#' @param coffs Numeric vector. Selectivity coefficients (length nsel)
#' @param sel_devs Numeric matrix. Selectivity deviations (nch_fsh x nsel)
#' @param yrs_ch_fsh Integer vector. Years where selectivity changes occur
#'
#' @return List containing:
#'   \item{avgsel}{Numeric. Average selectivity on log scale}
#'   \item{log_sel}{Matrix. Log selectivity by year (rows) and age (columns)}
#'
#' @export
compute_selectivity_fsh <- function(nsel, stsel, endyr, nages, coffs, sel_devs, yrs_ch_fsh) {
  # Handle ADMB compatibility
  "c" <- ADoverload("c")
  "[<-" <- ADoverload("[<-")

  # Initialize dimensions
  nyrs <- endyr - stsel + 1
  log_sel <- matrix(0, nrow = nyrs, ncol = nages)
  rownames(log_sel) <- as.character(stsel:endyr)

  # Calculate average selectivity
  avgsel <- log(mean(exp(coffs)))

  # Set initial year selectivity
  log_sel[1, 1:nsel] <- coffs
  log_sel[1, (nsel + 1):nages] <- coffs[nsel]

  # Center first year selectivity
  log_sel[1, ] <- log_sel[1, ] - log(mean(exp(log_sel[1, ])))

  # Apply time-varying changes
  ii <- 1
  nch_fsh <- nrow(sel_devs)

  for (i in 1:(nyrs - 1)) {
    year <- stsel + i - 1

    if (ii <= nch_fsh && year == yrs_ch_fsh[ii]) {
      # Apply deviation for change year
      log_sel[i + 1, 1:nsel] <- log_sel[i, 1:nsel] + sel_devs[ii, ]
      log_sel[i + 1, (nsel + 1):nages] <- log_sel[i + 1, nsel]
      ii <- ii + 1
    } else {
      # Carry forward previous year's selectivity
      log_sel[i + 1, ] <- log_sel[i, ]
    }

    # Center selectivity for current year
    log_sel[i + 1, ] <- log_sel[i + 1, ] - log(mean(exp(log_sel[i + 1, ])))
  }

  return(list(avgsel = avgsel, log_sel = log_sel))
}

#' Compute survey selectivity using logistic function
#'
#' @description
#' Computes time-varying logistic selectivity for survey gear using
#' age-at-50% and slope parameters with annual deviations.
#'
#' @param stsel Integer. Starting year for selectivity
#' @param slp Numeric. Base slope parameter (not currently used in calculation)
#' @param a50 Numeric. Base age-at-50% parameter (not currently used)
#' @param se Numeric vector. Annual slope deviations (length = endyr - stsel + 1)
#' @param ae Numeric vector. Annual age-at-50% deviations (length = endyr - stsel + 1)
#' @param age_vector Integer vector. Ages (typically 1:nages)
#' @param endyr Integer. Ending year
#'
#' @return Matrix. Log selectivity by year (rows) and age (columns)
#'
#' @export
compute_selectivity_ind <- function(stsel, slp, a50, se, ae, age_vector, endyr) {
  # Handle ADMB compatibility
  "c" <- ADoverload("c")
  "[<-" <- ADoverload("[<-")

  nages <- length(age_vector)
  nyrs <- endyr - stsel + 1
  log_sel <- matrix(0, nrow = nyrs, ncol = nages)
  rownames(log_sel) <- as.character(stsel:endyr)

  for (i in 1:nyrs) {
    # Time-varying logistic parameters
    slp_i <- exp(se[i])
    a50_i <- exp(ae[i])

    # Logistic selectivity function
    log_sel[i, ] <- -log(1 + exp(-slp_i * (age_vector - a50_i)))
  }

  return(log_sel)
}

#' Compute acoustic trawl survey (ATS) selectivity with deviations
#'
#' @description
#' Computes log-scale selectivity for ATS gear with time-varying deviations
#' at specified change years.
#'
#' @param nsel Integer. Number of selectivity parameters
#' @param nages Integer. Total number of ages
#' @param stsel Integer. Starting year
#' @param endyr Integer. Ending year
#' @param avgsel Numeric. Average selectivity (calculated internally, pass 0)
#' @param coffs Numeric vector. Base selectivity coefficients (length nsel)
#' @param sel_devs Numeric matrix. Selectivity deviations (nyrs_change x nsel)
#' @param mina_ats Integer. Minimum age for ATS selectivity
#' @param yrs_ch_ats Integer vector. Years with selectivity changes
#'
#' @return List containing:
#'   \item{avgsel}{Numeric. Average selectivity on log scale}
#'   \item{log_sel}{Matrix. Log selectivity by year and age}
#'
#' @export
compute_selectivity_ats_devs <- function(nsel, nages, stsel, endyr, avgsel, coffs, sel_devs,
                                         mina_ats, yrs_ch_ats) {
  # Handle ADMB compatibility
  "c" <- ADoverload("c")
  "[<-" <- ADoverload("[<-")

  dim_sel_ats <- nrow(sel_devs)

  # Initialize selectivity matrix
  log_sel <- matrix(0,
    nrow = endyr - stsel + 1, ncol = nages,
    dimnames = list(stsel:endyr, 1:nages)
  )

  # Calculate average selectivity
  avgsel <- log(mean(exp(coffs)))

  # Set baseline selectivity for first year
  log_sel[as.character(stsel), mina_ats:nsel] <- coffs
  log_sel[as.character(stsel), (nsel + 1):nages] <- log_sel[as.character(stsel), nsel]

  # Normalize first year selectivity
  log_sel[as.character(stsel), ] <- log_sel[as.character(stsel), ] -
    log(mean(exp(log_sel[as.character(stsel), ])))

  # Apply year-specific deviations
  ii <- 1
  for (yr in (stsel + 1):endyr) {
    ychar <- as.character(yr)
    yprev <- as.character(yr - 1)

    if (ii <= length(yrs_ch_ats) && yr == yrs_ch_ats[ii]) {
      # Apply deviation
      log_sel[ychar, mina_ats:nsel] <- log_sel[yprev, mina_ats:nsel] + sel_devs[ii, ]
      log_sel[ychar, (nsel + 1):nages] <- log_sel[ychar, nsel]

      if (ii < dim_sel_ats) {
        ii <- ii + 1
      }
    } else {
      # Carry forward previous year
      log_sel[ychar, ] <- log_sel[yprev, ]
    }

    # Normalize selectivity
    log_sel[ychar, ] <- log_sel[ychar, ] - log(mean(exp(log_sel[ychar, ])))
  }

  return(list(avgsel = avgsel, log_sel = log_sel))
}

#' Stock-recruitment function
#'
#' @description
#' Computes recruitment using a stock-recruitment relationship.
#' Appears to be a Ricker-type function.
#'
#' @param Stmp Numeric. Spawning stock biomass
#' @param phizero Numeric. Spawners-per-recruit at virgin conditions
#' @param alpha Numeric. Productivity parameter
#' @param Bzero Numeric. Virgin spawning biomass
#'
#' @return Numeric. Predicted recruitment
#'
#' @export
SRecruit <- function(Stmp, phizero, alpha, Bzero) {
  RecTmp <- (Stmp / phizero) * exp(alpha * (1 - Stmp / Bzero))
  return(RecTmp)
}

#' Bottom trawl survey (BTS) likelihood
#'
#' @description
#' Computes likelihood for bottom trawl survey data, with options for
#' covariance structure and biomass vs abundance.
#'
#' @param ob_bts Numeric vector. Observed BTS values
#' @param ot_bts Numeric vector. Observed total abundance (if do_bts_bio = FALSE)
#' @param eb_bts Numeric vector. Expected BTS values
#' @param et_bts Numeric vector. Expected total abundance (if do_bts_bio = FALSE)
#' @param inv_bts_cov Matrix. Inverse covariance matrix (if DoCovBTS = 1)
#' @param var_ob_bts Numeric vector. Observation variances (if DoCovBTS = 0)
#' @param DoCovBTS Integer. Flag for covariance structure (1 = use covariance, 0 = diagonal)
#' @param do_bts_bio Logical. Use biomass (TRUE) vs total abundance (FALSE)
#'
#' @return Numeric. Negative log-likelihood value
#'
#' @export
BTS_likelihood <- function(ob_bts, ot_bts, eb_bts, et_bts, inv_bts_cov,
                           var_ob_bts, DoCovBTS = 1, do_bts_bio = TRUE) {
  # Handle ADMB compatibility
  "c" <- ADoverload("c")
  "[<-" <- ADoverload("[<-")

  val <- 0
  q_bts <- mean(ob_bts) / mean(eb_bts)
  eb_bts_scaled <- eb_bts * q_bts

  # Calculate residuals
  if (do_bts_bio) {
    srv_tmp <- ob_bts - eb_bts_scaled
  } else {
    srv_tmp <- ot_bts - et_bts
  }

  # Compute likelihood based on covariance structure
  if (DoCovBTS == 1) {
    val <- 0.5 * t(srv_tmp) %*% inv_bts_cov %*% srv_tmp
  } else {
    val <- sum(srv_tmp^2 / (2 * var_ob_bts))
  }

  return(as.numeric(val))
}

#' Acoustic trawl survey (ATS) likelihood
#'
#' @description
#' Computes likelihood for acoustic trawl survey data on log scale.
#'
#' @param ob_ats Numeric vector. Observed ATS values
#' @param ot_ats Numeric vector. Observed total abundance
#' @param eb_ats Numeric vector. Expected ATS values
#' @param et_ats Numeric vector. Expected total abundance
#' @param lvar_ats Numeric vector. Log-scale variances for total abundance
#' @param lvarb_ats Numeric vector. Log-scale variances for biomass
#' @param do_ats_bio Logical. Use biomass likelihood (TRUE) vs abundance (FALSE)
#'
#' @return Numeric. Negative log-likelihood value
#'
#' @export
ATS_likelihood <- function(ob_ats, ot_ats, eb_ats, et_ats, lvar_ats, lvarb_ats, do_ats_bio = TRUE) {
  # Handle ADMB compatibility
  "c" <- ADoverload("c")
  "[<-" <- ADoverload("[<-")

  if (do_ats_bio) {
    sum((log(ob_ats + 0.01) - log(eb_ats + 0.01))^2 / (2 * lvarb_ats))
  } else {
    sum((log(ot_ats + 0.01) - log(et_ats + 0.01))^2 / (2 * lvar_ats))
  }
}

#' ATS age-1 likelihood
#'
#' @description
#' Computes likelihood for age-1 acoustic trawl survey data.
#'
#' @param oa1_ats Numeric vector. Observed age-1 ATS values
#' @param ea1_ats Numeric vector. Expected age-1 ATS values
#' @param age1_sigma_ats Numeric. Standard deviation for age-1 likelihood
#' @param use_age1_ats Logical. Whether to use age-1 likelihood
#' @param ignore_last_ats_age1 Logical. Whether to ignore last observation
#' @param n_ats Integer. Number of ATS observations
#'
#' @return Numeric. Negative log-likelihood value (0 if use_age1_ats = FALSE)
#'
#' @export
ATS_age1_likelihood <- function(oa1_ats, ea1_ats, age1_sigma_ats, use_age1_ats = TRUE,
                                ignore_last_ats_age1 = FALSE, n_ats = length(oa1_ats)) {
  if (!use_age1_ats) {
    return(0)
  }

  # Calculate catchability scaling
  qtmp <- exp(mean(log(oa1_ats) - log(ea1_ats)))

  # Determine observation range
  i_range <- if (ignore_last_ats_age1) 1:(n_ats - 1) else 1:n_ats

  # Calculate residuals and likelihood
  resids <- log(oa1_ats[i_range] + 0.01) - log(ea1_ats[i_range] * qtmp + 0.01)
  0.5 * sum(resids^2) / (age1_sigma_ats^2)
}

#' CPUE likelihood
#'
#' @description
#' Computes likelihood for catch-per-unit-effort (CPUE) data.
#'
#' @param obs_cpue Numeric vector. Observed CPUE values
#' @param pred_cpue Numeric vector. Predicted CPUE values
#' @param obs_cpue_var Numeric vector. CPUE observation variances
#'
#' @return Numeric. Negative log-likelihood value
#'
#' @export
CPUE_likelihood <- function(obs_cpue, pred_cpue, obs_cpue_var) {
  cpue_dev <- obs_cpue - pred_cpue
  sum(cpue_dev^2 / (2 * obs_cpue_var))
}

#' Acoustic survey (AVO) likelihood
#'
#' @description
#' Computes likelihood for acoustic vessel survey data.
#'
#' @param ob_avo Numeric vector. Observed AVO values
#' @param pred_avo Numeric vector. Predicted AVO values
#' @param obs_avo_var Numeric vector. AVO observation variances
#'
#' @return Numeric. Negative log-likelihood value
#'
#' @export
AVO_likelihood <- function(ob_avo, pred_avo, obs_avo_var) {
  # Handle ADMB compatibility
  "c" <- ADoverload("c")
  "[<-" <- ADoverload("[<-")

  avo_dev <- ob_avo - pred_avo
  sum(avo_dev^2 / (2 * obs_avo_var))
}

#' Catch likelihood
#'
#' @description
#' Computes likelihood for catch data using log-normal distribution.
#'
#' @param obs_catch Numeric vector. Observed catch values
#' @param pred_catch Numeric vector. Predicted catch values
#' @param eps Numeric. Small constant to avoid log(0)
#'
#' @return Numeric. Sum of squared log differences
#'
#' @export
catch_like <- function(obs_catch, pred_catch, eps = 1e-4) {
  stopifnot(
    is.numeric(obs_catch), is.numeric(pred_catch),
    length(obs_catch) == length(pred_catch)
  )

  diffs <- log(obs_catch + eps) - log(pred_catch + eps)
  sum(diffs^2, na.rm = TRUE)
}

#' Multinomial likelihood for age compositions
#'
#' @description
#' Computes multinomial likelihood for age composition data across multiple gears.
#' Mirrors ADMB implementation with gear-specific treatments.
#'
#' @param oac Named list. Observed age composition matrices (fsh, bts, ats)
#' @param eac Named list. Expected age composition matrices (fsh, bts, ats)
#' @param sam Named list. Effective sample size vectors (fsh, bts, ats)
#' @param MN_const Numeric. Small constant added inside log (default 0)
#' @param age_like_offset Numeric vector. Likelihood offsets by gear (default all 0)
#' @param mina_ats Integer. First ATS age column to include
#' @param nages Integer. Last ATS age column to include
#'
#' @return Named numeric vector with gear-specific likelihoods and total
#'
#' @export
multinomial_likelihood_age <- function(oac, eac, sam, MN_const = 0, age_like_offset = NULL,
                                       mina_ats = NULL, nages = NULL) {
  # Handle ADMB compatibility
  "c" <- ADoverload("c")
  "[<-" <- ADoverload("[<-")

  gears <- c("fsh", "bts", "ats")
  if (is.null(names(oac)) && length(oac) == 3) names(oac) <- gears
  if (is.null(names(eac)) && length(eac) == 3) names(eac) <- gears
  if (is.null(names(sam)) && length(sam) == 3) names(sam) <- gears

  # Validation
  stopifnot(
    all(gears %in% names(oac)),
    all(gears %in% names(eac)),
    all(gears %in% names(sam))
  )

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

  # Default offsets
  if (is.null(age_like_offset)) age_like_offset <- setNames(rep(0, 3), gears)
  if (is.null(names(age_like_offset))) names(age_like_offset) <- gears
  age_like <- setNames(numeric(3), gears)

  # Fishery likelihood
  ll_fsh_i <- sam$fsh * rowSums(oac$fsh * log(eac$fsh + MN_const))
  age_like["fsh"] <- -sum(ll_fsh_i) - age_like_offset["fsh"]

  # Bottom trawl survey likelihood
  ll_bts_i <- sam$bts * rowSums(oac$bts * log(eac$bts + MN_const))
  age_like["bts"] <- -sum(ll_bts_i) - age_like_offset["bts"]

  # Acoustic trawl survey likelihood (restricted age range)
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

  structure(age_like, total = sum(age_like))
}

#' Recruitment likelihood
#'
#' @description
#' Computes likelihood components for recruitment including stock-recruitment
#' residuals, recruitment deviations, and initial age structure deviations.
#'
#' @param yrs_est Integer vector. Years for estimation (styr_est:endyr_est)
#' @param SSB Numeric vector. Spawning stock biomass (named by year or aligned)
#' @param pred_rec Numeric vector. Predicted recruitment (named by year or aligned)
#' @param log_rec_devs Numeric vector. Log recruitment deviations
#' @param sigr Numeric. Recruitment standard deviation
#' @param log_initdevs Numeric vector. Initial age structure deviations
#' @param phizero Numeric. Spawners per recruit at virgin conditions
#' @param Bzero Numeric. Virgin spawning biomass
#' @param alpha Numeric. Stock-recruitment productivity parameter
#' @param omit78 Numeric. Flag to omit exclude_year (< 1 = include all)
#' @param exclude_year Integer. Specific year to exclude from likelihood
#' @param srrPrior Numeric. Prior weight on stock-recruitment relationship
#' @param eps Numeric. Small value to avoid log(0)
#'
#' @return List containing recruitment likelihood components and total
#'
#' @export
recruitment_likelihood <- function(yrs_est, SSB, pred_rec, log_rec_devs, sigr, log_initdevs,
                                   phizero, Bzero, alpha, omit78, exclude_year = 1979,
                                   srrPrior, eps = 1e-8) {
  # Handle ADMB compatibility
  "c" <- ADoverload("c")
  "[<-" <- ADoverload("[<-")

  yrs_est <- as.integer(yrs_est)
  yrs_lag <- yrs_est - 1L

  # Helper function for year-based indexing
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

  # Extract time series data
  B_lag <- fetch_by_year(SSB, yrs_lag)
  R_hat <- fetch_by_year(pred_rec, yrs_est)
  rec_devs <- fetch_by_year(log_rec_devs, yrs_est)

  sigmaRsq <- sigr * sigr
  rec_like <- numeric(7)

  # Penalty on recruitment deviations (mean-centered)
  rec_like[2] <- 1.0 * sum((log_rec_devs - mean(log_rec_devs))^2)

  # Penalty on initial age composition deviations
  rec_like[4] <- 0.1 * sum((log_initdevs - mean(log_initdevs))^2)

  # Stock-recruitment model predictions
  srmod_rec <- SRecruit(B_lag, phizero, alpha, Bzero)

  # Stock-recruitment residuals (bias-corrected)
  SR_resids <- log(R_hat + eps) - log(srmod_rec + eps)

  # Main stock-recruitment likelihood
  if (omit78 < 1) {
    # Include all years
    rec_like[1] <- 0.5 * sum((SR_resids + sigmaRsq / 2)^2) / sigmaRsq +
      length(yrs_est) * log(sigr)
  } else {
    # Exclude specific year
    keep <- yrs_est != exclude_year
    rec_like[1] <- sum(0.5 * (SR_resids[keep] + sigmaRsq / 2)^2 / sigmaRsq + log(sigr))
  }

  # Apply prior weighting
  rec_like[1] <- rec_like[1] * srrPrior

  return(list(
    rec_like = rec_like,
    rec_like_total = sum(rec_like)
  ))
}

#' Helper function: second difference for smoothness penalties
#'
#' @param x Numeric vector
#' @return Numeric vector of second differences
#'
#' @keywords internal
second_difference <- function(x) {
  n <- length(x)
  if (n < 3) {
    return(numeric(0))
  }
  x[3:n] - 2 * x[2:(n - 1)] + x[1:(n - 2)]
}

#' Helper function: first difference
#'
#' @param x Numeric vector
#' @return Numeric vector of first differences
#'
#' @keywords internal
first_difference <- function(x) {
  n <- length(x)
  if (n < 2) {
    return(numeric(0))
  }
  x[2:n] - x[1:(n - 1)]
}

#' Helper function: norm2 (sum of squares)
#'
#' @param x Numeric vector
#' @return Numeric. Sum of squared values
#'
#' @keywords internal
norm2 <- function(x) {
  sum(x^2, na.rm = TRUE)
}

#' Helper function: second difference alias
#'
#' @param x Numeric vector
#' @return Numeric vector of second differences
#'
#' @keywords internal
sdiff <- second_difference

#' Fishery selectivity likelihood
#'
#' @description
#' Computes penalties for fishery selectivity including shape constraints,
#' smoothness, and random walk components.
#'
#' @param log_sel_fsh Matrix. Log selectivity by year (rows) and age (columns)
#' @param styr Integer. Starting year
#' @param endyr Integer. Ending year
#' @param n_selages_fsh Integer. Number of selectivity ages for fishery
#' @param yrs_ch_fsh Integer vector. Years where selectivity changes
#' @param nch_fsh Integer. Number of change points
#' @param nyrs Integer. Number of years
#' @param domFish Numeric. Shape penalty weight
#' @param selTFsh Numeric. Temporal smoothness weight (not used in current version)
#' @param selCFsh Numeric. Curvature penalty weight
#' @param selCurv Numeric. Additional curvature parameter (not used)
#' @param sel_devs_fsh Matrix. Selectivity deviations
#' @param sel_ch_sig_fsh Numeric vector. Standard deviations for changes
#' @param year_index Named integer vector. Maps years to matrix row indices
#'
#' @return List with shape penalty, deviation penalty, and total
#'
#' @export
selectivity_like_fsh <- function(log_sel_fsh, styr, endyr, n_selages_fsh, yrs_ch_fsh, nch_fsh,
                                 nyrs, domFish, selTFsh, selCFsh, selCurv, sel_devs_fsh,
                                 sel_ch_sig_fsh, year_index) {
  # Handle ADMB compatibility
  "c" <- ADoverload("c")
  "[<-" <- ADoverload("[<-")

  shape_pen <- 0
  dev <- 0

  # Shape penalty: penalize decreasing selectivity with age
  for (i in 1:nyrs) {
    for (j in 6:(n_selages_fsh - 1)) {
      d <- log_sel_fsh[i, j] - log_sel_fsh[i, j + 1]
      shape_pen <- shape_pen + domFish * d * d
    }
  }

  # Deviation penalties
  dev <- dev + norm2(sel_devs_fsh)

  # Curvature penalty at first year
  dev <- dev + selCFsh / nch_fsh * norm2(sdiff(log_sel_fsh[1, ]))

  # Change year penalties
  for (k in seq_len(nch_fsh)) {
    yy <- yrs_ch_fsh[k]
    ik <- year_index[[as.character(yy)]]

    # Curvature at change year
    dev <- dev + selCFsh / nch_fsh * norm2(sdiff(log_sel_fsh[ik, ]))

    # Random walk penalty between consecutive years
    ip <- year_index[[as.character(yy - 1)]]
    sigma <- sel_ch_sig_fsh[k]

    dev <- dev + norm2(log_sel_fsh[ip, ] - log_sel_fsh[ik, ]) / (2 * sigma^2)
  }

  return(list(shape = shape_pen, dev = dev, total = shape_pen + dev))
}

#' ATS selectivity likelihood
#'
#' @description
#' Computes penalties for acoustic trawl survey selectivity including
#' shape constraints, curvature, and random walk components.
#'
#' @param log_sel_ats Matrix. Log selectivity by year (rows) and age (columns)
#' @param styr Integer. Overall model start year
#' @param styr_ats Integer. ATS survey start year
#' @param endyr Integer. Ending year
#' @param mina_ats Integer. Minimum age for ATS (not used in current loop)
#' @param n_selages_ats Integer. Number of selectivity ages for ATS
#' @param yrs_ch_ats Integer vector. Years where ATS selectivity changes
#' @param nch_ats Integer. Number of ATS change points
#' @param selATS Numeric. ATS shape penalty weight
#' @param selTATS Numeric. ATS curvature penalty weight
#' @param sel_ch_sig_ats Numeric vector. Standard deviations for ATS changes
#' @param year_index Named integer vector. Maps years to matrix row indices
#'
#' @return List with shape penalty, deviation penalty, and total
#'
#' @export
selectivity_like_ats <- function(log_sel_ats, styr, styr_ats, endyr, mina_ats, n_selages_ats,
                                 yrs_ch_ats, nch_ats, selATS, selTATS, sel_ch_sig_ats, year_index) {
  # Handle ADMB compatibility
  "c" <- ADoverload("c")
  "[<-" <- ADoverload("[<-")

  shape_pen <- 0

  # Shape penalty: penalize increasing selectivity with age (age 5 to n_selages_ats-1)
  for (yy in seq(styr_ats, endyr)) {
    i <- year_index[[as.character(yy)]]
    if (is.null(i)) next

    for (j in 5:(n_selages_ats - 1)) {
      d <- log_sel_ats[i, j] - log_sel_ats[i, j + 1]
      shape_pen <- shape_pen + selATS * d * d
    }
  }

  # Deviation penalties: curvature and random walk
  like_tmp1 <- 0
  like_tmp2 <- 0

  # Curvature penalties at change years
  for (k in seq_len(nch_ats)) {
    yy <- yrs_ch_ats[k]
    ik <- year_index[[as.character(yy)]]
    like_tmp1 <- like_tmp1 + selTATS * norm2(sdiff(log_sel_ats[ik, ]))

    # Random walk penalty between consecutive years
    ip <- year_index[[as.character(yy - 1)]]
    sigma <- sel_ch_sig_ats[k]
    like_tmp2 <- like_tmp2 + norm2(log_sel_ats[ip, ] - log_sel_ats[ik, ]) / (2 * sigma^2)
  }

  dev_pen <- like_tmp1 + like_tmp2
  return(list(shape = shape_pen, dev = dev_pen, total = shape_pen + dev_pen))
}

#' BTS selectivity likelihood
#'
#' @description
#' Computes penalties for bottom trawl survey selectivity including temporal
#' smoothness and deviation penalties. Handles different parameterizations
#' based on active parameter flags.
#'
#' @param q_amin Integer. Minimum age for BTS time-smooth penalty (default 3)
#' @param q_amax Integer. Maximum age for BTS time-smooth penalty (default nages)
#' @param styr Integer. Overall model start year
#' @param endyr Integer. Overall model end year
#' @param styr_bts Integer. BTS survey start year
#' @param year_index Named integer vector. Maps years to matrix row indices
#' @param selCurv Numeric. Curvature penalty flag/weight
#' @param selVarbts Numeric. BTS variance penalty weight
#' @param nages Integer. Number of ages in model
#' @param log_sel_bts Matrix. Log selectivity by year and age
#' @param active_sel_coffs_bts Logical. Whether selectivity coefficients are active
#' @param active_sel_devs_bts Logical. Whether selectivity deviations are active
#' @param active_sel_a50_bts_dev Logical. Whether a50 deviations are active
#' @param active_sel_age_one_bts_dev Logical. Whether age-1 deviations are active
#' @param sel_devs_bts Numeric vector. Selectivity deviations (if active)
#' @param sel_a50_bts_dev Numeric vector. A50 deviations (if active)
#' @param sel_slp_bts_dev Numeric vector. Slope deviations (if active)
#' @param sel_age_one_bts_dev Numeric vector. Age-1 selectivity deviations
#'
#' @return List with shape penalty, deviation penalty, and total
#'
#' @export
selectivity_like_bts <- function(q_amin = 3, q_amax = NULL, styr, endyr, styr_bts, year_index,
                                 selCurv, selVarbts, nages, log_sel_bts,
                                 active_sel_coffs_bts = FALSE, active_sel_devs_bts = FALSE,
                                 active_sel_a50_bts_dev = TRUE, active_sel_age_one_bts_dev = TRUE,
                                 sel_devs_bts = numeric(0), sel_a50_bts_dev = numeric(0),
                                 sel_slp_bts_dev = numeric(0), sel_age_one_bts_dev) {
  # Handle ADMB compatibility
  "c" <- ADoverload("c")
  "[<-" <- ADoverload("[<-")

  if (is.null(q_amax)) q_amax <- nages

  shape_pen <- 0
  dev_pen <- 0

  # Create extended selectivity matrix (add row for year before styr_bts)
  log_sel_tmp <- rbind(rep(0, ncol(log_sel_bts)), log_sel_bts)
  rownames(log_sel_tmp) <- c(as.character(styr_bts - 1), rownames(log_sel_bts))

  # BTS smoothness and deviation penalties
  if (isTRUE(active_sel_coffs_bts)) {
    if (isTRUE(active_sel_devs_bts)) {
      # Note: group_num_bts and btsVarT would need to be defined in calling environment
      # dev_pen <- dev_pen + btsVarT / group_num_bts * norm2(sel_devs_bts)
      dev_pen <- dev_pen + norm2(sel_devs_bts)

      # Curvature at start year
      i0 <- year_index[[as.character(styr_bts)]]
      dev_pen <- dev_pen + norm2(second_difference(log_sel_tmp[i0, ]))

      # Curvature penalties at regular intervals
      # Note: group_num_bts would need to be defined
      group_num_bts <- 5 # Example value - should be passed as parameter
      for (yy in seq(styr_bts, endyr - 1)) {
        if ((yy %% group_num_bts) == 0) {
          inext <- year_index[[as.character(yy + 1)]]
          if (!is.null(inext)) {
            dev_pen <- dev_pen + norm2(second_difference(log_sel_tmp[inext, ])) / ncol(log_sel_tmp)
          }
        }
      }
    } else {
      # No time deviations: curvature at start year only
      i0 <- year_index[[as.character(styr_bts)]]
      dev_pen <- dev_pen + norm2(second_difference(log_sel_tmp[i0, ]))
    }
  }

  # Time-varying penalties across ages
  for (j in q_amin:(q_amax - 1)) {
    dev_pen <- dev_pen + selVarbts * norm2(first_difference(log_sel_tmp[, j]))
  }

  # Age-1 selectivity deviation penalty
  if (isTRUE(active_sel_age_one_bts_dev)) {
    dev_pen <- dev_pen + 8.0 * norm2(first_difference(sel_age_one_bts_dev - mean(sel_age_one_bts_dev)))
  }

  return(list(shape = 0, dev = dev_pen, total = dev_pen))
}
