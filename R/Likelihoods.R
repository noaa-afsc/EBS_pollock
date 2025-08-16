BTS_likelihood <- function(
    ob_bts, ot_bts, eb_bts, et_bts, # observed and expected values
    inv_bts_cov, # variance and inverse covariance
    var_ob_bts,
    DoCovBTS = 1, 
    do_bts_bio = TRUE # flags for likelihood type and biology) {
) {
  "c" <- ADoverload("c")
  "[<-" <- ADoverload("[<-")
  val = 0
  q_bts <- mean(ob_bts) / mean(eb_bts)
  eb_bts_scaled <- eb_bts * q_bts
  
  if (do_bts_bio) {
    srv_tmp <- ob_bts - eb_bts_scaled
  } else {
    srv_tmp <- ot_bts - et_bts
  }
  if (DoCovBTS==1)
  {
    
     val <- 0.5 * t(srv_tmp) %*% inv_bts_cov %*% srv_tmp
  } else {
    # Need to correct if goinig back to total abundance instead of biomass
     val <- sum(srv_tmp^2 / (2 * var_ob_bts))
  }
                # "0" = {
                #   if (do_bts_bio) {
                #     srv_tmp <- log(ob_bts) - log(eb_bts_scaled)
                #     sum(srv_tmp^2 / (2 * var_ob_bts))
                #   } else {
                #     0
                #   }
                # },
                # "1" = {
                #   0.5 * t(srv_tmp) %*% inv_bts_cov %*% srv_tmp
                # },
                # "2" = {
                #   sum(sapply(1:n_bts_r, function(i) {
                #     (log(ob_bts[i]) - log(eb_bts_scaled[i]))^2 / (2 * var_ob_bts[i])
                #   }))
                # },
                # "3" = {
                #   if (do_bts_bio) {
                #     srv_tmp <- log(ob_bts) - log(eb_bts_scaled)
                #   }
                #   0 # Placeholder: no likelihood implemented
                # }
  return(val)
}

ATS_likelihood <- function(
    ob_ats, ot_ats, eb_ats, et_ats, # observed and expected values
    lvar_ats, lvarb_ats, # log-scale variances
    do_ats_bio = TRUE # flag for biology likelihood type) {
) {
  
  "c" <- ADoverload("c")
  "[<-" <- ADoverload("[<-")
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
  "c" <- ADoverload("c")
  "[<-" <- ADoverload("[<-")
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
  "c" <- ADoverload("c")
  "[<-" <- ADoverload("[<-")
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
  "c" <- ADoverload("c")
  "[<-" <- ADoverload("[<-")
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


# log_sel_fsh: matrix [year, age] (rows are years, cols are ages), values are *log* selectivity
# year_index: named integer vector mapping year -> row index in log_sel_* (e.g., setNames(seq_along(years), years))
selectivity_like_fsh <- function( log_sel_fsh,
    styr, endyr_r, # year numbers
    n_selages_fsh, # number of selectivity ages for fishery
    yrs_ch_fsh, nch_fsh, # change years and count
    nyrs, domFish, selTFsh,
    selCFsh, selCurv, sel_devs_fsh, # vector
    sel_ch_sig_fsh,
    year_index # length = nch_fsh
) {
  "c" <- ADoverload("c")
  "[<-" <- ADoverload("[<-")
  shape_pen <- 0
  dev <- 0
  # NOTE: ADMB loop shows j<=n_selages_fsh then uses j+1; the safe R version is 1:(n_selages_fsh-1)
  for (i in 1:nyrs) {
    # i <- year_index[[as.character(yy)]]
    for (j in 6:(n_selages_fsh - 1)) {
      # if (log_sel_fsh[i, j] > log_sel_fsh[i, j + 1]) {
        d <- log_sel_fsh[i, j] - log_sel_fsh[i, j + 1]
        shape_pen <- shape_pen + domFish * d * d
      # }
    }
  }
  # p1 <- p2 <- p3 <- dev_pen <- 0
  # ctrl_flag(10)/group_num_fsh * norm2(sel_devs_fsh)
  dev <- selTFsh * norm2(sel_devs_fsh)
  
  # For each change year: curvature + random‑walk between year and previous
  dev <- dev + selCFsh / nch_fsh * norm2(sdiff(log_sel_fsh[1, ]))
  
  for (k in seq_len(nch_fsh)) {
    yy <- yrs_ch_fsh[k]
    ik <- year_index[[as.character(yy)]]
    dev <- dev + selCFsh / nch_fsh * norm2(sdiff(log_sel_fsh[ik, ]))
    
    ip <- year_index[[as.character(yy - 1)]]
    sigma <- sel_ch_sig_fsh[k]
    
    dev <- dev + norm2(log_sel_fsh[ip, ] - log_sel_fsh[ik, ]) / (2 * sigma^2)
  }
  list(shape = shape_pen, dev = dev, total = shape_pen + dev)
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
  "c" <- ADoverload("c")
  "[<-" <- ADoverload("[<-")
  shape_pen <- 0
  for (yy in seq(styr_ats, endyr_r)) {
    i <- year_index[[as.character(yy)]]
    if (is.null(i)) next
    #for (j in mina_ats:(n_selages_ats - 1)) {
    for (j in 5:(n_selages_ats - 1)) {
      #if (log_sel_ats[i, j] < log_sel_ats[i, j + 1]) {
        d <- log_sel_ats[i, j] - log_sel_ats[i, j + 1]
        shape_pen <- shape_pen + selATS * d * d
      #}
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