# Write Standard Projection Model inputs from the saved Rceattle fit.

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

format_spm_numbers <- function(x) {
  paste(format(signif(as.numeric(x), 8), scientific = FALSE, trim = TRUE), collapse = " ")
}

age_values <- function(x) {
  as.numeric(x[grep("^Age", names(x))])
}

write_spm_dat <- function(output_file, begin_year, alt_list = 1:7,
                          fixed_catches = c(1350, 1350), nproj_years = 14,
                          nsims = 1000, run_name = "rceattle_ebswp") {
  catch_years <- if (is.null(names(fixed_catches))) {
    begin_year + seq_along(fixed_catches) - 1L
  } else {
    as.integer(names(fixed_catches))
  }
  lines <- c(
    "#_SETUP_FILE_FOR_EBS_Pollock_Rceattle", paste(run_name, "# Run name"),
    "# Tier", "3", "#--------------------------------------------",
    paste(length(alt_list), "# Number of Alternatives"),
    "#--------------------------------------------", paste(as.integer(alt_list), collapse = " "),
    "#--------------------------------------------", "1    # Flag to set TAC equal to ABC",
    "#--------------------------------------------", "2    # Stock-recruitment type (1=Ricker, 2=Bholt)",
    "1    # projection recruitment form", "1    # SR-Conditioning",
    "0    # Recruitment prior CV condition", "#--------------------------------------------",
    "1    # Flag to write big file", "#--------------------------------------------",
    paste0(nproj_years, " #_Number of projection years"),
    paste0(nsims, " #_Number of simulations"), paste0(begin_year, " #_Begin Year"),
    "#_Number_of_years with specified catch", as.character(length(fixed_catches)),
    "# Number of species", "1", "# OY Min", "0", "# OY Max", "2.00E+06",
    "# data files for each species", "pm.prj", "# ABC Multipliers", "1",
    "# scalars", "1", "# New Alt 4 Fabc SPRs", "0.6",
    "# Number of TAC model categories", "1", "# TAC model indices", "1",
    "# Catch in each future year", paste(catch_years, as.numeric(fixed_catches))
  )
  writeLines(lines, output_file)
}

write_rceattle_pm_prj <- function(fit, output_file,
                                  n_selectivity_years = 5L) {
  d <- fit$data_list
  q <- fit$quantities
  terminal_year <- as.integer(d$endyr)
  years <- as.integer(dimnames(q$N_at_age)[[4]])
  iy <- match(terminal_year, years)
  if (is.na(iy)) stop("Rceattle terminal year is absent from N_at_age.")

  n_selectivity_years <- as.integer(n_selectivity_years)
  if (length(n_selectivity_years) != 1L || is.na(n_selectivity_years) ||
      n_selectivity_years < 1L || n_selectivity_years > iy) {
    stop("n_selectivity_years must identify available terminal assessment years.")
  }

  recent_rows <- seq.int(iy - n_selectivity_years + 1L, iy)
  recent_years <- years[recent_rows]
  avg_f <- mean(as.numeric(q$F_spp[1, recent_rows]), na.rm = TRUE)
  wt_spawn <- age_values(d$weight[d$weight$Wt_index == d$ssb_wt_index & d$weight$Year == terminal_year, ][1, ])
  wt_fishery <- age_values(d$weight[d$weight$Wt_index == 1 & d$weight$Year == terminal_year, ][1, ])
  maturity <- age_values(d$maturity[1, ])
  natmort <- as.numeric(q$M_at_age[1, 1, , iy])
  selectivity_by_year <- matrix(
    as.numeric(q$sel_at_age[1, 1, , recent_rows, drop = FALSE]),
    nrow = d$nages,
    ncol = length(recent_rows),
    dimnames = list(Age = seq_len(d$nages), Year = recent_years)
  )
  selectivity <- rowMeans(selectivity_by_year)
  natage <- as.numeric(q$N_at_age[1, 1, , iy])
  recruitment <- as.numeric(q$R[1, match(1978:terminal_year, years)])
  ssb <- as.numeric(q$ssb[1, match(1977:(terminal_year - 1L), years)])

  stopifnot(
    length(natmort) == d$nages, length(maturity) == d$nages,
    length(wt_spawn) == d$nages, length(wt_fishery) == d$nages,
    length(selectivity) == d$nages, length(natage) == d$nages,
    all(is.finite(c(avg_f, natmort, maturity, wt_spawn, wt_fishery,
                    selectivity, natage, recruitment, ssb)))
  )

  lines <- c(
    paste0("EBS_Pollock_Rceattle_", terminal_year),
    "1    # SSLn species...", "0    # Buffer of Dorn", "1    # Number of fsheries",
    "1    # Number of sexes", paste(format_spm_numbers(avg_f), "# average 5yr F"),
    "1  # author f", "0.4  # ABC SPR", "0.35 # MSY/OFL SPR",
    paste0(d$spawn_month + 1L, "  # Spawnmo"), paste0(d$nages, " # Number of ages"),
    "1  # Fratio", paste(format_spm_numbers(natmort), "# Natural Mortality"),
    "# Maturity", format_spm_numbers(maturity / max(maturity)),
    "# Wt spawn", format_spm_numbers(wt_spawn), "# Wt fsh", format_spm_numbers(wt_fishery),
    "# selectivity", format_spm_numbers(selectivity), "# natage", format_spm_numbers(natage),
    "# Nrec", as.character(length(recruitment)), "# rec", format_spm_numbers(recruitment),
    "# SpawningBiomass", format_spm_numbers(ssb)
  )
  writeLines(lines, output_file)

  list(
    schedules = data.frame(
      Age = seq_len(d$nages), Spawning_weight = wt_spawn,
      Fishery_weight = wt_fishery, Maturity = maturity / max(maturity),
      Natural_mortality = natmort, Selectivity = selectivity
    ),
    selectivity_history = data.frame(
      Year = rep(recent_years, each = d$nages),
      Age = rep(seq_len(d$nages), times = length(recent_years)),
      Selectivity = as.numeric(selectivity_by_year)
    ),
    selectivity_years = recent_years,
    average_f = avg_f
  )
}

write_spmr_projection_inputs <- function(
    method_file = "results/canonical_pm/ebs_pollock_method_fits.rds",
    fit_name = "nonparametric_pm",
    output_dir = "results/canonical_pm/spmR_projection", alt_list = 1:7,
    fixed_catches = c(1350, 1350), nproj_years = 14, nsims = 1000,
    run_name = "rceattle_ebswp", n_selectivity_years = 5L,
    template_dir = Sys.getenv(
      "SPMR_RUNTIME_DIR",
      file.path("results", "spmR_runtime")
    )) {
  method_file <- normalizePath(method_file, mustWork = TRUE)
  output_dir <- normalizePath(output_dir, mustWork = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  fits <- readRDS(method_file)
  fit <- fits[[fit_name]]
  if (is.null(fit) || !inherits(fit, "Rceattle")) {
    stop(
      "The requested Rceattle fit '", fit_name,
      "' is absent from ", method_file, "."
    )
  }

  projection_inputs <- write_rceattle_pm_prj(
    fit,
    file.path(output_dir, "pm.prj"),
    n_selectivity_years = n_selectivity_years
  )
  write_spm_dat(file.path(output_dir, "spm.dat"), fit$data_list$endyr + 1L,
                alt_list, fixed_catches, nproj_years, nsims, run_name)
  for (item in c("tacpar.dat", "spm")) {
    source <- file.path(template_dir, item)
    if (!file.exists(source)) stop("Missing SPM runtime template: ", source)
    file.copy(source, file.path(output_dir, item), overwrite = TRUE)
  }
  Sys.chmod(file.path(output_dir, "spm"), "0755")
  utils::write.csv(
    projection_inputs$schedules,
    file.path(output_dir, "age_schedules.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    projection_inputs$selectivity_history,
    file.path(output_dir, "fishery_selectivity_recent_years.csv"),
    row.names = FALSE
  )

  files <- file.path(output_dir, c(
    "spm.dat", "pm.prj", "tacpar.dat", "spm", "age_schedules.csv",
    "fishery_selectivity_recent_years.csv"
  ))
  manifest <- data.frame(
    file = basename(files),
    role = c("SPM setup", "Rceattle-derived species input", "TAC parameters",
             "SPM executable", "Rceattle-derived age schedules",
             "Annual fishery selectivity used in the recent-year average"),
    exists = file.exists(files), md5 = unname(tools::md5sum(files))
  )
  utils::write.csv(manifest, file.path(output_dir, "manifest.csv"), row.names = FALSE)
  utils::write.csv(data.frame(
    source_file = method_file, source_md5 = unname(tools::md5sum(method_file)),
    fit = fit_name, terminal_year = fit$data_list$endyr,
    fishery_selectivity_rule = "arithmetic mean of annual selectivity-at-age",
    fishery_selectivity_years = paste(projection_inputs$selectivity_years, collapse = ";"),
    fishery_selectivity_year_count = length(projection_inputs$selectivity_years),
    fishing_mortality_years = paste(projection_inputs$selectivity_years, collapse = ";"),
    recent_average_f = projection_inputs$average_f,
    generated = as.character(Sys.time()), Rceattle = as.character(utils::packageVersion("Rceattle")),
    spmR = as.character(utils::packageVersion("spmR")),
    spmR_commit = "e86fc6aa6f48a4de29d1deba0e8bd1df93b8717f",
    composition_likelihood = paste(
      unique(stats::na.omit(fit$data_list$fleet_control$Comp_distribution)),
      collapse = "; "
    ),
    composition_sample_sizes =
      "fishery, BTS, and ATS nominal integer sample sizes; ATS 2020 set to zero"
  ), file.path(output_dir, "lineage.csv"), row.names = FALSE)
  invisible(list(output_dir = output_dir, manifest = manifest,
                 begin_year = fit$data_list$endyr + 1L,
                 fixed_catch_years = if (is.null(names(fixed_catches))) fit$data_list$endyr + seq_along(fixed_catches) else as.integer(names(fixed_catches))))
}
