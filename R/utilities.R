# Helper function for generalized gamma distribution (if not available)
#--Utilities to read parameters from .dat files-----------

norm2 <- function(x) {
  "c" <- ADoverload("c")
  "[<-" <- ADoverload("[<-")
  sum(x^2, na.rm = TRUE)
}
first_difference <- fdiff <- function(x) {
  "c" <- ADoverload("c")
  "[<-" <- ADoverload("[<-")
  diff(x) # first_difference
}
second_difference <- sdiff <- function(x) {
  "c" <- ADoverload("c")
  "[<-" <- ADoverload("[<-")
  diff(diff(x)) # second_difference
}


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
compare_max_pct <- function(rtmb, admb, tolerance = 1e-6) {
  is_ok <- function(x) is.numeric(x) || is.matrix(x) || is.array(x)
  shared_vars <- intersect(names(rtmb), names(admb))
  comps <- lapply(shared_vars, function(var) {
    rt <- rtmb[[var]]
    ad <- admb[[var]]
    if (!(is_ok(rt) && is_ok(ad))) {
      return(data.frame(
        variable = var, equal = NA, length = NA_integer_,
        max_abs_diff = NA_real_, max_abs_pct_diff = NA_real_, cor = NA_real_
      ))
    }
    rt_vec <- as.vector(rt)
    ad_vec <- as.vector(ad)
    if (length(rt_vec) != length(ad_vec)) {
      return(data.frame(
        variable = var, equal = FALSE,
        length = length(rt_vec),
        max_abs_diff = NA_real_, max_abs_pct_diff = NA_real_, cor = NA_real_
      ))
    }
    # Absolute differences
    abs_diff <- abs(rt_vec - ad_vec)
    max_abs_diff <- suppressWarnings(max(abs_diff, na.rm = TRUE))
    if (!is.finite(max_abs_diff)) max_abs_diff <- NA_real_
    
    # % diff relative to ADMB
    eps <- .Machine$double.eps
    pct_diff <- abs_diff / pmax(abs(ad_vec), eps) * 100
    max_abs_pct_diff <- suppressWarnings(max(pct_diff, na.rm = TRUE))
    if (!is.finite(max_abs_pct_diff)) max_abs_pct_diff <- NA_real_
    equal <- isTRUE(all.equal(rt_vec, ad_vec, tolerance = tolerance))
    cor_val <- suppressWarnings(cor(rt_vec, ad_vec, use = "complete.obs"))
    data.frame(
      variable = var,
      equal = equal,
      length = length(rt_vec),
      max_abs_diff = max_abs_diff,
      max_abs_pct_diff = max_abs_pct_diff,
      cor = cor_val
    )
  })
  do.call(rbind, comps)
}
cmb <- function(f, d) function(p) f(p, d)

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
        equal            = "Equal (≤ tol%)",
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
gt_compare_table2 <- function(rtmb, admb, tolerance = 1e-4, sort_by_diff = TRUE, font_size = "small") {
    requireNamespace("gt", quietly = TRUE)
    requireNamespace("dplyr", quietly = TRUE)
    requireNamespace("scales", quietly = TRUE)
    
    is_ok <- function(x) is.numeric(x) || is.matrix(x) || is.array(x)
    rtmb_f <- rtmb[sapply(rtmb, is_ok)]
    admb_f <- admb[sapply(admb, is_ok)]
    
    comp <- compare_max_pct(rtmb_f, admb_f, tolerance = tolerance)
    
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
    
    # Set color domain for percentage differences
    dom_pct <- range(comp$max_abs_pct_diff, na.rm = TRUE)
    if (!all(is.finite(dom_pct))) dom_pct <- c(0, 1)
    
    # Set color domain for absolute differences  
    dom_abs <- range(comp$max_abs_diff, na.rm = TRUE)
    if (!all(is.finite(dom_abs))) dom_abs <- c(0, 1)
    
    gt::gt(comp) |>
      gt::data_color(
        columns = c(max_abs_pct_diff),
        fn = scales::col_numeric(
          palette = c("white", "yellow", "orangered"),
          domain  = dom_pct
        )
      ) |>
      gt::data_color(
        columns = c(max_abs_diff),
        fn = scales::col_numeric(
          palette = c("white", "lightblue", "darkblue"),
          domain  = dom_abs
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
        columns = c(max_abs_diff, max_abs_pct_diff, cor),
        decimals = 6
      ) |>
      gt::fmt_integer(
        columns = c(length)
      ) |>
      gt::cols_label(
        variable         = "Variable",
        equal            = "Equal (≤ tol)",
        length           = "Length",
        max_abs_diff     = "Max |Abs diff|",
        max_abs_pct_diff = "Max |% diff|",
        cor              = "Correlation"
      ) |>
      gt::tab_header(
        title = "Comparison of RTMB and ADMB Outputs",
        subtitle = paste("Tolerance =", format(tolerance, scientific = TRUE))
      ) |>
      # Add font size styling
      gt::tab_style(
        style = gt::cell_text(size = font_size),
        locations = gt::cells_body()
      ) |>
      gt::tab_style(
        style = gt::cell_text(size = font_size),
        locations = gt::cells_column_labels()
      ) |>
      gt::tab_style(
        style = gt::cell_text(size = font_size),
        locations = gt::cells_title()
      )
  }
  
  # Option 2: Save to PNG (requires webshot2 package)
  save_gt_table_png <- function(gt_table, filename, width = 800, height = 600) {
    requireNamespace("webshot2", quietly = TRUE)
    gt::gtsave(gt_table, filename, vwidth = width, vheight = height)
  }
  
  # Example usage:
  # 
  # # Create table with smaller font
  # my_table<- gt_compare_table2(rtmb_out, pm, sort_by_diff = ) #, font_size = "xx-small")  # or "xx-small", "small", "medium", "large", etc.
  # my_table# 
  # # Display the table
  # my_table
  # 
  # # Save to PNG
  # save_gt_table_png(my_table, "comparison_table.png", width = 1000, height = 800)
  #
  # # Or save directly with gtsave
   # gt::gtsave(my_table, "comparison_table.png", vwidth = 1000, vheight = 800)
   
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
    domFish = 3., #12.5, # 	13	Fishery	Dome	Emph
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
  endyr <- data_tmp$endyr - const$retroYr
  endyr_est <- endyr - const$omitSR
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
      endyr = endyr,
      endyr_est = endyr_est,
      dec_tab_catch = c(10, 0.25 * obs_catch(endyr), 0.50 * obs_catch(endyr), 0.75 * obs_catch(endyr), 1.00 * obs_catch(endyr), 1.25 * obs_catch(endyr), 1.50 * obs_catch(endyr), 2.00 * obs_catch(endyr))
    )
  )
  return(data)
}

Get_Data <- function() {
  #--Includes PRELIMINARY Calcs stuff too-------
  data <- build_model_inputs(here::here("runs","rtmb", "pm.dat"), 
                            fn = here::here("runs","data", "pm_24.dat"))
  data$spawnmo <- 4. # scalar, month of spawning
  data$yrfrac <- (data$spawnmo - 1.) / 12 # scalar, fraction of year for spawning
  data$inv_bts_cov <- solve(data$cov_matrix)  # inverse covariance matrix
  data$obs_cpue_var <- data$obs_cpue_std^2 # observation variance for CPUE
  data$obs_avo_var <- data$ob_avo_std^2 # observation variance for AVO
  data$nyrs <- data$endyr-data$styr+1
  data$return_nll_only <- 1 # Flag to only return nll
  # Eq. 21 – initialize wt_fut
  data$wt_fut <- data$wt_fsh[data$nyrs, ]

  # Natural mortality setup
  data$base_natmort <- data$natmort_in
  data$natmort <- data$base_natmort

  if (data$Mmatrix == 1 && !is.null(data$M_in)) {
    data$M <- data$M_in
    data$base_natmort <- data$M[as.character(data$endyr), ]
    data$natmort <- data$base_natmort
  }
  Wtage_file <- here::here("runs", "data", "wtage2024.dat")
  waa <- read_wtage_data(Wtage_file)
  data <- (c(data,waa))

  # Catch forecast: simple decrement from next_yrs_catch
  data$Cat_Fut <- numeric(10)
  data$Cat_Fut[1] <- data$next_yrs_catch
  for (i in 2:10) {
    data$Cat_Fut[i] <- data$Cat_Fut[i - 1] * 0.9
  }

  # Age composition likelihood offsets
  data$age_like_offset <- rep(0, data$ngears)
  data$len_like_offset <- 0
  MN_const <- 0.001

  for (igear in seq_len(data$ngears)) {
    for (i in seq_len(data$nagecomp[igear])) {
      if (igear == 1) {
        # Fishery
        p <- data$oac_fsh_data[i, ] / sum(data$oac_fsh_data[i, ])
        data$oac_fsh[i, ] <- p
        data$age_like_offset[igear] <- data$age_like_offset[igear] -
          data$sam_fsh[i] * sum(p * log(p + MN_const))
      } else if (igear == 2) {
        # BTS
        data$std_ob_bts[i] <- data$ob_bts_std[i]
        data$ot_bts[i] <- sum(data$oac_bts[i, data$mina_bts:data$nages])
        # data$ob_bts[i] <- data$obs_bts_data[i]
        p <- data$oac_bts[i, ] / data$ot_bts[i]
        data$oac_bts[i, ] <- p
        data$age_like_offset[igear] <- data$age_like_offset[igear] -
          floor(data$sam_bts[i]) * sum(p * log(p + MN_const))
      } else if (igear == 3) {
        # ATS
        # data$std_ob_ats[i] <- data$std_ob_ats_data[i]
        data$oa1_ats[i] <- data$oac_ats[i, 1]
        data$ot_ats[i] <- sum(data$oac_ats[i, data$mina_ats:data$nages])
        p <- data$oac_ats[i, data$mina_ats:data$nages]
        p <- p / sum(p)
        data$oac_ats[i, data$mina_ats:data$nages] <- p
        data$age_like_offset[igear] <- data$age_like_offset[igear] -
          data$sam_ats[i] * sum(p * log(p + MN_const))
      }
    }
  }

  # Length likelihood offset
  data$len_like_offset <- data$len_like_offset -
    50 * sum(data$olc_fsh * log(data$olc_fsh + MN_const))

  # Ignore ATS age-1 if CV too high
  # data$ot_ats[data$n_ats] <- sum(data$oac_ats_data[data$n_ats, data$mina_ats:data$nages])
  # if (data$ot_ats_std[data$n_ats] / data$ot_ats[data$n_ats] > 0.4) {
    # data$ignore_last_ats_age1 <- 1
  # } else {
    data$ignore_last_ats_age1 <- 0
  # }

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
  return(data)
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

# compute_fsh_selectivity <- function(nsel, stsel, endyr, nages, avgsel, coffs, sel_devs, nch_fsh, yrs_ch_fsh) {


# helper

read_wtage_data <- function(Wtage_file) {
  # cat("Opening", Wtage_file, "\n")

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

  # LOCAL_CALCS equivalent - adjust nyrs_data if endyr < endyr
  # (This assumes endyr and endyr are available in the global environment)
  if (exists("endyr") && exists("endyr") && exists("styr")) {
    if (endyr < endyr) {
      for (h in 1:ndat_wt) {
        itmp <- 1
        for (i in styr:endyr) {
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


 flatten_numeric_lists <- function(x) {
   lapply(x, function(el) {
     # If it's a list of all numeric/integer elements
     if (is.list(el) && all(vapply(el, function(y) is.numeric(y) || is.integer(y), logical(1)))) {
       # Drop names unless you want to preserve them
       unlist(el, use.names = TRUE)
     } else {
       el
     }
   })
 }
 flatten_numeric_lists <- function(x) {
   lapply(x, function(el) {
     if (is.list(el) && all(vapply(el, is.numeric, logical(1)))) {
       v <- unlist(el, use.names = TRUE)
       # Heuristic: if all values are whole numbers, store as integer
       if (all(abs(v - round(v)) < .Machine$double.eps^0.5)) as.integer(v) else v
     } else {
       el
     }
   })
 }
 # Method 1: Recursive function to convert all list elements to appropriate numeric types
 convert_lists_to_numeric <- function(x) {
   if (is.list(x) && !is.data.frame(x)) {
     # Check if this is a list that should be converted to a vector/matrix
     if (length(x) > 0) {
       # Try to convert to numeric vector first
       tryCatch({
         # If all elements are numeric scalars, convert to vector
         if (all(sapply(x, function(item) is.numeric(item) && length(item) == 1))) {
           result <- as.numeric(unlist(x))
           names(result) <- names(x)  # Retain names
           return(result)
         }
         # If elements are vectors/matrices, try to combine appropriately
         else if (all(sapply(x, is.numeric))) {
           # Check if all elements have same dimensions for matrix conversion
           dims <- sapply(x, function(item) paste(dim(item), collapse="x"))
           if (length(unique(dims)) == 1 && !is.null(dim(x[[1]]))) {
             # Convert to array/matrix
             result <- do.call(rbind, x)
             rownames(result) <- names(x)  # Retain names as row names
             return(result)
           } else {
             # Convert to named list of vectors/matrices
             result <- lapply(x, as.numeric)
             names(result) <- names(x)
             return(result)
           }
         }
         # Recursively apply to each element
         else {
           result <- lapply(x, convert_lists_to_numeric)
           names(result) <- names(x)  # Retain names
           return(result)
         }
       }, error = function(e) {
         # If conversion fails, recursively apply to each element
         result <- lapply(x, convert_lists_to_numeric)
         names(result) <- names(x)  # Retain names
         return(result)
       })
     } else {
       return(x)  # Empty list
     }
   } else {
     return(x)  # Not a list or is a data.frame
   }
 }
 
 # Method 2: More targeted approach for specific data structures
 flatten_numeric_lists <- function(data_list) {
   result <- list()
   
   for (name in names(data_list)) {
     item <- data_list[[name]]
     
     if (is.list(item) && !is.data.frame(item)) {
       # Check if it's a simple numeric list
       if (all(sapply(item, function(x) is.numeric(x) && length(x) == 1))) {
         # Convert to named numeric vector
         result[[name]] <- setNames(as.numeric(unlist(item)), names(item))
       }
       # Check if it's a list of numeric vectors of same length
       else if (all(sapply(item, is.numeric)) && 
                length(unique(sapply(item, length))) == 1) {
         # Convert to matrix
         result[[name]] <- do.call(rbind, item)
         if (!is.null(names(item))) {
           rownames(result[[name]]) <- names(item)
         }
       }
       # Recursively process nested structures
       else {
         result[[name]] <- flatten_numeric_lists(item)
       }
     } else {
       # Keep as is if already numeric or other type
       result[[name]] <- item
     }
   }
   
   return(result)
 }
 
 # Method 3: Force conversion while preserving structure
 force_numeric_conversion <- function(x, preserve_names = TRUE) {
   if (is.list(x) && !is.data.frame(x)) {
     # Try to unlist if all elements are numeric scalars
     if (all(sapply(x, function(item) is.numeric(item) && length(item) <= 1))) {
       result <- as.numeric(unlist(x))
       if (preserve_names && !is.null(names(x))) {
         names(result) <- names(x)
       }
       return(result)
     }
     # Otherwise, process each element
     else {
       result <- lapply(x, force_numeric_conversion, preserve_names = preserve_names)
       if (preserve_names) {
         names(result) <- names(x)
       }
       return(result)
     }
   } else if (is.character(x)) {
     # Try to convert character to numeric
     tryCatch({
       as.numeric(x)
     }, error = function(e) x, warning = function(w) x)
   } else {
     return(x)
   }
 }
 

 
 create_map_from_zeros <- function(params, tolerance = 1e-10) {
   map_list <- list()
   
   for (name in names(params)) {
     param_vals <- params[[name]]
     
     # Check if values are effectively zero
     is_zero <- abs(param_vals) < tolerance
     
     if (any(is_zero)) {
       if (is.array(param_vals) || is.matrix(param_vals)) {
         # For arrays/matrices
         map_factor <- array(seq_along(param_vals), dim = dim(param_vals))
         map_factor[is_zero] <- NA
         # IMPORTANT: Convert to factor!
         map_list[[name]] <- factor(map_factor)
       } else {
         # For vectors
         map_factor <- seq_along(param_vals)
         map_factor[is_zero] <- NA
         # IMPORTANT: Convert to factor!
         map_list[[name]] <- factor(map_factor)
       }
     }
   }
   
   return(map_list)
 }
 
 create_map_from_par <- function(par_vector, 
                                 param_list,  # Original parameter list with structure
                                 exact_names = NULL, 
                                 patterns = NULL, 
                                 exclude_patterns = NULL) {
   map_list <- list()
   all_names <- names(par_vector)
   
   # Start with all parameter names
   selected_names <- character(0)
   
   # Add exact matches
   if (!is.null(exact_names)) {
     selected_names <- c(selected_names, exact_names[exact_names %in% all_names])
   }
   
   # Add pattern matches
   if (!is.null(patterns)) {
     for (pattern in patterns) {
       pattern_matches <- grep(pattern, all_names, value = TRUE)
       selected_names <- c(selected_names, pattern_matches)
     }
   }
   
   # Remove excluded patterns
   if (!is.null(exclude_patterns)) {
     for (exclude_pattern in exclude_patterns) {
       exclude_matches <- grep(exclude_pattern, selected_names, value = TRUE)
       selected_names <- setdiff(selected_names, exclude_matches)
     }
   }
   
   # Remove duplicates
   selected_names <- unique(selected_names)
   
   # Create map with proper structure
   for (name in selected_names) {
     if (name %in% names(param_list)) {
       original_param <- param_list[[name]]
       
       # Create NA array/matrix/vector with same structure
       if (is.array(original_param) || is.matrix(original_param)) {
         na_structure <- array(NA, dim = dim(original_param))
         map_list[[name]] <- factor(na_structure)
       } else {
         # For vectors
         na_structure <- rep(NA, length(original_param))
         map_list[[name]] <- factor(na_structure)
       }
     }
   }
   
   return(map_list)
 }
 
 # Usage - you need both fit$par and your original parameter list
 
 
 
 to_param_list <- function(par_vec) {
   # Remove [..] for grouping
   base_names <- sub("\\[.*\\]$", "", names(par_vec))
   split_vals <- split(par_vec, base_names)
   # Drop to scalar if length 1
   lapply(split_vals, function(x) if (length(x) == 1) as.numeric(x) else as.numeric(x))
 }
 
 create_value_ratio_plot <- function(df, variable_name = "SSB", numerator_model = "RTMB", denominator_model = "ADMB") {
   # Filter data for the specific variable
   filtered_data <- df %>%
     filter(name == variable_name)
   
   # Check if both models exist in the data
   models_present <- unique(filtered_data$model)
   if (!numerator_model %in% models_present) {
     stop(paste("Model", numerator_model, "not found in the data"))
   }
   if (!denominator_model %in% models_present) {
     stop(paste("Model", denominator_model, "not found in the data"))
   }
   
   # Separate data by model
   num_data <- filtered_data %>% filter(model == numerator_model)
   den_data <- filtered_data %>% filter(model == denominator_model)
   
   # Merge data by year to calculate ratios
   ratio_data <- merge(num_data, den_data, by = "year", suffixes = c("_num", "_den"))
   
   # Calculate ratio and propagated standard deviation
   ratio_data <- ratio_data %>%
     mutate(
       value_ratio = value_num / value_den
       # Error propagation for ratio: sqrt((sd_a/a)^2 + (sd_b/b)^2) * ratio
       #value_ratio_std = sqrt((std.dev_num/value_num)^2 + (std.dev_den/value_den)^2) * value_ratio
     )
   
   # Create the plot
   p <- ggplot(ratio_data, aes(x = year, y = value_ratio)) +
     geom_point(size = 2, color = "blue") +
     # geom_errorbar(aes(ymin = value_ratio - 2*value_ratio_std, ymax = value_ratio + 2*value_ratio_std), 
     # width = 0.5, color = "red", alpha = 0.7) +
     geom_hline(yintercept = 1, linetype = "dashed", color = "black", alpha = 0.5) +
     labs(
       title = paste0("Value Ratio: ", numerator_model, " / ", denominator_model, " for ", variable_name),
       # subtitle = "Error bars show ±2 standard deviations",
       x = "Year",
       y = paste0("Value Ratio") ) +
     theme_minimal() +
     theme(
       plot.title = element_text(hjust = 0.5, size = 14),
       plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray50"),
       axis.title = element_text(size = 12),
       axis.text = element_text(size = 10)
     )
   return(p)
 }
 
 # Function to create ratio plot for standard deviations (no error bars needed)
 create_stddev_ratio_plot <- function(df, variable_name = "SSB", numerator_model = "RTMB", denominator_model = "ADMB") {
   # Filter data for the specific variable
   filtered_data <- df %>%
     filter(name == variable_name)
   
   # Check if both models exist in the data
   models_present <- unique(filtered_data$model)
   if (!numerator_model %in% models_present) {
     stop(paste("Model", numerator_model, "not found in the data"))
   }
   if (!denominator_model %in% models_present) {
     stop(paste("Model", denominator_model, "not found in the data"))
   }
   
   # Separate data by model
   num_data <- filtered_data %>% filter(model == numerator_model)
   den_data <- filtered_data %>% filter(model == denominator_model)
   
   # Merge data by year to calculate ratios
   ratio_data <- merge(num_data, den_data, by = "year", suffixes = c("_num", "_den"))
   
   # Calculate standard deviation ratio
   ratio_data <- ratio_data %>%
     mutate(
       stddev_ratio = std.dev_num / std.dev_den
     )
   
   # Create the plot
   p <- ggplot(ratio_data, aes(x = year, y = stddev_ratio)) +
     geom_point(size = 2, color = "darkgreen") +
     geom_line(color = "darkgreen", alpha = 0.6) +
     geom_hline(yintercept = 1, linetype = "dashed", color = "black", alpha = 0.5) +
     labs(
       title = paste0("Standard Deviation Ratio: ", numerator_model, " / ", denominator_model, " for ", variable_name),
       subtitle = "Ratio of standard deviations between models",
       x = "Year",
       y = paste0("Std.Dev Ratio"  )
     ) +
     theme_minimal() +
     theme(
       plot.title = element_text(hjust = 0.5, size = 14),
       plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray50"),
       axis.title = element_text(size = 12),
       axis.text = element_text(size = 10)
     )
   
   return(p)
 }
