#!/usr/bin/env Rscript

# Model 26.0: simple EBS pollock Rceattle fit and diagnostics.
#
# This run uses the dsem-v5-integration branch at the pinned commit recorded
# below. The assessment fit retains the established non-parametric selectivity
# configuration and excludes the historical 2D age-by-year AR1 sensitivity.

rceattle_library <- Sys.getenv(
  "RCEATTLE_LIB",
  "/tmp/rceattle-dsem-v5-lib-01a05b46"
)
.libPaths(c(rceattle_library, .libPaths()))

suppressPackageStartupMessages({
  library(Rceattle)
  library(afscOSA)
  library(dplyr)
  library(ggridges)
  library(ggplot2)
  library(tidyr)
})

expected_version <- package_version("5.23.0")
expected_commit <- "adf22f84b399e84e3b9a707e0b3bbb1624179a27"
stopifnot(
  packageVersion("Rceattle") == expected_version,
  packageVersion("afscOSA") == package_version("0.0.1")
)

project_root <- normalizePath(getwd(), mustWork = TRUE)
output_directory <- Sys.getenv(
  "MODEL26_OUTPUT_DIR",
  file.path(project_root, "results", "model_26_0_rceattle_5.23.0")
)
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

input_workbook <- file.path(
  project_root,
  "Data",
  "EBS_24_pollock_m23_rceattle_full_1964-2024.xlsx"
)
admb_directory <- file.path(project_root, "ADMB", "m23_rceattle_full")
previous_fit_file <- file.path(
  project_root,
  "results",
  "canonical_pm",
  "ebs_pollock_method_fits.rds"
)

est <- suppressWarnings(Rceattle::read_data(input_workbook))
est$diet_data <- NULL
stopifnot(!any(grepl("2DAR1", est$fleet_control$Selectivity, fixed = TRUE)))

# The 2020 ATS survey collected no age-composition sample. The workbook
# retains placeholder proportions so the input dimensions and plotting grid
# remain continuous, but a sample size of zero excludes this row from the
# composition likelihood and all likelihood-based diagnostics.
ats_2020_row <- est$comp_data$Fleet_name == "ATS" &
  est$comp_data$Year == 2020L
stopifnot(sum(ats_2020_row) == 1L)
est$comp_data$Sample_size[ats_2020_row] <- 0
stopifnot(est$comp_data$Sample_size[ats_2020_row] == 0)

years <- est$styr:est$endyr
nyears <- length(years)
nages <- est$nages

# Start the current parameter structure from the previously validated simple
# fit. Only blocks with identical names and dimensions are transferred.
inits <- Rceattle::build_params(est)
previous_fit <- readRDS(previous_fit_file)$nonparametric_pm
previous_params <- previous_fit$estimated_params
transferable <- intersect(names(inits), names(previous_params))
transferable <- transferable[vapply(
  transferable,
  function(parameter) {
    identical(dim(inits[[parameter]]), dim(previous_params[[parameter]])) &&
      length(inits[[parameter]]) == length(previous_params[[parameter]])
  },
  logical(1)
)]
for (parameter in transferable) {
  inits[[parameter]] <- previous_params[[parameter]]
}

m1_function <- Rceattle::build_M1(updateM1 = TRUE, M1_model = "fixed")
control <- Rceattle::fit_control(
  verbose = 1,
  phase = TRUE,
  bias_adjust_proc = FALSE,
  bias_adjust_obs = FALSE,
  comp_offset = 1e-3
)

# Stage 1 establishes the abundance and selectivity scale with annual
# selectivity changes held constant. Stage 2 restores the established
# non-parametric random-walk changes.
stage_a_data <- est
stage_a_data$fleet_control$Time_varying_sel <- "Off"
stage_a <- Rceattle::fit_mod(
  data_list = stage_a_data,
  inits = inits,
  file = NULL,
  estimateMode = 0,
  random_rec = FALSE,
  msmMode = 0,
  initMode = "NonEquilibrium",
  M1Fun = m1_function,
  fit_control = control
)

model_26 <- Rceattle::fit_mod(
  data_list = est,
  inits = stage_a$estimated_params,
  file = NULL,
  estimateMode = 0,
  random_rec = FALSE,
  msmMode = 0,
  initMode = "NonEquilibrium",
  M1Fun = m1_function,
  fit_control = control
)

saveRDS(
  list(
    stage_a = stage_a,
    model_26 = model_26,
    data = est,
    years = years,
    Rceattle_version = as.character(packageVersion("Rceattle")),
    Rceattle_commit = expected_commit,
    afscOSA_version = as.character(packageVersion("afscOSA"))
  ),
  file.path(output_directory, "model_26_0_fit.rds")
)

convergence_value <- function(model, path, default = NA) {
  value <- tryCatch(Reduce(`[[`, path, init = model), error = function(e) default)
  if (is.null(value) || !length(value)) default else value
}

convergence_summary <- data.frame(
  Fit = c("Stage 1 scale fit", "Model 26.0"),
  Objective = c(stage_a$opt$objective, model_26$opt$objective),
  Maximum_gradient = c(stage_a$opt$max_gradient, model_26$opt$max_gradient),
  Positive_definite_Hessian = c(
    isTRUE(stage_a$sdrep$pdHess),
    isTRUE(model_26$sdrep$pdHess)
  ),
  Convergence_status = c(stage_a$convergence$status, model_26$convergence$status),
  Hessian_condition_number = c(
    convergence_value(stage_a, c("convergence", "checks", "hessian_conditioning", "data", "condition_number")),
    convergence_value(model_26, c("convergence", "checks", "hessian_conditioning", "data", "condition_number"))
  )
)
write.csv(
  convergence_summary,
  file.path(output_directory, "model_26_0_convergence.csv"),
  row.names = FALSE
)

write.csv(
  data.frame(
    Item = c(
      "Rceattle branch",
      "Rceattle commit",
      "Rceattle version",
      "Assessment years",
      "Modeled ages",
      "Fishery, AVO, ATS, and CPUE selectivity",
      "BTS selectivity retained from bridge",
      "Time-varying selectivity",
      "Composition distribution",
      "2020 ATS age composition",
      "2D age-by-year AR1 sensitivity",
      "afscOSA version"
    ),
    Setting = c(
      "dsem-v5-integration",
      expected_commit,
      as.character(packageVersion("Rceattle")),
      paste0(min(years), "-", max(years)),
      paste0("1-", nages),
      "NonParametricPM",
      "LogisticPM with the separate age-1 component",
      "Established RandomWalk configuration",
      paste(unique(est$fleet_control$Comp_distribution), collapse = "; "),
      "Excluded from fitting; no age-composition sample was collected",
      "Excluded from this simple fit",
      as.character(packageVersion("afscOSA"))
    )
  ),
  file.path(output_directory, "model_26_0_configuration.csv"),
  row.names = FALSE
)

# Model 23.2 is the Rceattle-aligned ADMB bridge. Its time series provide a
# direct reference without mixing the RTMB translation and Rceattle software.
report_lines <- readLines(file.path(admb_directory, "pm.rep"), warn = FALSE)
read_admb_series <- function(key) {
  start <- which(report_lines == key)[1]
  rows <- list()
  cursor <- start + 1L
  while (cursor <= length(report_lines)) {
    values <- suppressWarnings(as.numeric(strsplit(trimws(report_lines[cursor]), "[[:space:]]+")[[1]]))
    if (length(values) < 2L || anyNA(values[1:2])) break
    rows[[length(rows) + 1L]] <- values[1:2]
    cursor <- cursor + 1L
  }
  data.frame(Year = vapply(rows, `[[`, numeric(1), 1), Value = vapply(rows, `[[`, numeric(1), 2))
}
read_admb_matrix <- function(key, columns) {
  start <- which(report_lines == key)[1]
  rows <- list()
  cursor <- start + 1L
  while (cursor <= length(report_lines)) {
    values <- suppressWarnings(as.numeric(strsplit(trimws(report_lines[cursor]), "[[:space:]]+")[[1]]))
    if (length(values) < columns || anyNA(values[seq_len(columns)])) break
    rows[[length(rows) + 1L]] <- values[seq_len(columns)]
    cursor <- cursor + 1L
  }
  do.call(rbind, rows)
}

admb_ssb <- read_admb_series("SSB")
admb_recruitment <- read_admb_series("R")
admb_numbers <- read_admb_matrix("N", nages)
admb_selectivity <- list(
  Fishery = read_admb_matrix("sel_fsh", nages),
  BTS = read_admb_matrix("sel_bts", nages),
  ATS = read_admb_matrix("sel_ats", nages)
)
stopifnot(vapply(admb_selectivity, nrow, integer(1)) == nyears)

model_26_selectivity <- list(
  Fishery = model_26$quantities$sel_at_age[1, 1, , seq_len(nyears)],
  BTS = model_26$quantities$sel_at_age[3, 1, , seq_len(nyears)],
  ATS = model_26$quantities$sel_at_age[4, 1, , seq_len(nyears)]
)
selectivity_start_year <- c(Fishery = min(years), BTS = 1982L, ATS = 1994L)

selectivity_comparison <- bind_rows(lapply(names(admb_selectivity), function(fleet_name) {
  fleet_years <- years[years >= selectivity_start_year[[fleet_name]]]
  year_rows <- match(fleet_years, years)
  bind_rows(
    data.frame(
      Fleet = fleet_name,
      Model = "Model 23.2",
      Year = rep(fleet_years, each = nages),
      Age = rep(seq_len(nages), times = length(fleet_years)),
      Selectivity = as.numeric(t(admb_selectivity[[fleet_name]][year_rows, , drop = FALSE]))
    ),
    data.frame(
      Fleet = fleet_name,
      Model = "Model 26.0",
      Year = rep(fleet_years, each = nages),
      Age = rep(seq_len(nages), times = length(fleet_years)),
      Selectivity = as.numeric(model_26_selectivity[[fleet_name]][, year_rows, drop = FALSE])
    )
  ) |>
    group_by(Fleet, Model, Year) |>
    mutate(Relative_selectivity = Selectivity / max(Selectivity)) |>
    ungroup()
}))
write.csv(
  selectivity_comparison,
  file.path(output_directory, "model_23_2_vs_26_0_selectivity.csv"),
  row.names = FALSE
)

selectivity_colors <- c("Model 23.2" = "#0072B2", "Model 26.0" = "#009E73")
selectivity_min_age <- c(Fishery = 1L, BTS = 1L, ATS = 2L)
for (fleet_name in names(admb_selectivity)) {
  fleet_min_age <- selectivity_min_age[[fleet_name]]
  plot_data <- selectivity_comparison |>
    filter(Fleet == fleet_name, Age >= fleet_min_age) |>
    mutate(
      Model = factor(Model, levels = c("Model 23.2", "Model 26.0")),
      Year_plot = factor(Year, levels = rev(sort(unique(Year))))
    )
  ridge_figure <- ggplot(
    plot_data,
    aes(
      x = Age,
      y = Year_plot,
      height = Relative_selectivity,
      group = interaction(Model, Year_plot),
      color = Model,
      fill = Model
    )
  ) +
    ggridges::geom_density_ridges(
      stat = "identity",
      scale = 1.65,
      alpha = 0.40,
      linewidth = 0.28
    ) +
    facet_grid(cols = vars(Model)) +
    scale_color_manual(values = selectivity_colors) +
    scale_fill_manual(values = selectivity_colors) +
    scale_x_continuous(
      breaks = seq.int(fleet_min_age, nages),
      limits = c(fleet_min_age, nages),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    labs(x = "Age", y = "Year") +
    ggthemes::theme_few(base_size = 10) +
    theme(
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 5.5),
      strip.text = element_text(size = 9)
    )
  ggsave(
    file.path(
      output_directory,
      paste0("model_23_2_vs_26_0_", tolower(fleet_name), "_selectivity.png")
    ),
    ridge_figure,
    width = 8.5,
    height = 10.5,
    dpi = 220
  )
}

weight_rows <- est$weight[est$weight$Wt_index == est$pop_wt_index, ]
weight_matrix <- as.matrix(
  weight_rows[match(years, weight_rows$Year), paste0("Age", seq_len(nages))]
)
admb_biomass <- data.frame(
  Year = years,
  Value = rowSums(admb_numbers * weight_matrix)
)

model_series <- list(
  `Spawning biomass` = as.numeric(model_26$quantities$ssb[1, seq_len(nyears)]),
  `Age-1 recruitment` = as.numeric(model_26$quantities$R[1, seq_len(nyears)]),
  `Total biomass` = as.numeric(model_26$quantities$biomass[1, seq_len(nyears)])
)
bridge_series <- list(
  `Spawning biomass` = admb_ssb$Value,
  `Age-1 recruitment` = admb_recruitment$Value,
  `Total biomass` = admb_biomass$Value
)

trajectory_comparison <- bind_rows(lapply(names(model_series), function(quantity) {
  bind_rows(
    data.frame(Year = years, Quantity = quantity, Model = "Model 26.0", Value = model_series[[quantity]]),
    data.frame(Year = years, Quantity = quantity, Model = "Model 23.2 bridge", Value = bridge_series[[quantity]])
  )
}))
write.csv(
  trajectory_comparison,
  file.path(output_directory, "model_26_0_bridge_trajectories.csv"),
  row.names = FALSE
)

comparison_summary <- bind_rows(lapply(names(model_series), function(quantity) {
  current <- model_series[[quantity]]
  reference <- bridge_series[[quantity]]
  data.frame(
    Quantity = quantity,
    Correlation = cor(current, reference),
    Mean_absolute_percent_difference = mean(abs(100 * (current - reference) / reference)),
    Terminal_percent_difference = 100 * (tail(current, 1) - tail(reference, 1)) / tail(reference, 1)
  )
}))
write.csv(
  comparison_summary,
  file.path(output_directory, "model_26_0_bridge_summary.csv"),
  row.names = FALSE
)

trajectory_figure <- ggplot(
  trajectory_comparison,
  aes(x = Year, y = Value, color = Model, linetype = Model)
) +
  geom_line(linewidth = 0.8) +
  facet_wrap(vars(Quantity), ncol = 1, scales = "free_y") +
  scale_color_manual(values = c("Model 23.2 bridge" = "#0072B2", "Model 26.0" = "#D55E00")) +
  scale_linetype_manual(values = c("Model 23.2 bridge" = "longdash", "Model 26.0" = "solid")) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(x = "Year", y = NULL, color = NULL, linetype = NULL) +
  ggthemes::theme_few() +
  theme(legend.position = "bottom")
ggsave(
  file.path(output_directory, "model_26_0_bridge_trajectories.png"),
  trajectory_figure,
  width = 9,
  height = 9,
  dpi = 180
)

# Composition OSA residuals use Rceattle's internal values for fishery and
# BTS. ATS starts at age 2, so afscOSA 0.0.1 calculates its 14-bin residual
# sequence directly before producing the common AFSC display and diagnostics.
osa_internal <- Rceattle::osa_residuals(
  model_26,
  source = "comp",
  parallel = TRUE,
  seed = 20260831L
)
saveRDS(
  osa_internal,
  file.path(output_directory, "model_26_0_osa_residuals_internal.rds")
)

make_afscosa_composition <- function(fleet_code) {
  comp_data <- model_26$data_list$comp_data
  age_bins <- if (fleet_code == 4L) 2:nages else seq_len(nages)
  rows <- which(comp_data$Fleet_code == fleet_code)
  valid <- rowSums(round(
    comp_data$Sample_size[rows] *
      model_26$quantities$comp_obs[rows, age_bins, drop = FALSE]
  )) >= 1
  rows <- rows[valid]
  fleet_years <- comp_data$Year[rows]

  # ATS age 1 is a structural zero in both the observed and fitted
  # compositions. Its likelihood is therefore the ages 2--15 multinomial.
  # The generic Rceattle OSA recursion starts at the zero column and shifts the
  # diagnostic sequence, so afscOSA calculates ATS residuals directly from the
  # 14 contributing ages. Fishery and BTS retain Rceattle's internal residuals.
  osa_matrix <- NULL
  if (fleet_code != 4L) {
    internal_osa <- osa_internal |>
      filter(source == "comp", fleet == fleet_code, year %in% fleet_years) |>
      arrange(match(year, fleet_years), age_length_bin)
    stopifnot(nrow(internal_osa) == length(rows) * (length(age_bins) - 1L))
    osa_matrix <- matrix(
      internal_osa$residual,
      nrow = length(rows),
      ncol = length(age_bins) - 1L,
      byrow = TRUE
    )
  }

  afscOSA::run_osa(
    obs = model_26$quantities$comp_obs[rows, age_bins, drop = FALSE],
    exp = model_26$quantities$comp_hat[rows, age_bins, drop = FALSE],
    N = comp_data$Sample_size[rows],
    fleet = comp_data$Fleet_name[rows[1]],
    index = age_bins,
    years = fleet_years,
    index_label = "Age",
    seed = 20260831L,
    res = osa_matrix
  )
}

afscosa_composition <- lapply(c(1, 3, 4), make_afscosa_composition)
saveRDS(
  afscosa_composition,
  file.path(output_directory, "model_26_0_afscosa_composition.rds")
)

osa <- bind_rows(lapply(seq_along(afscosa_composition), function(i) {
  afscosa_composition[[i]]$res |>
    transmute(
      source = "comp",
      fleet = c(1L, 3L, 4L)[i],
      fleet_name = afscosa_composition[[i]]$res$fleet,
      year,
      age_length_bin = index,
      residual = resid
    )
}))
class(osa) <- c("rceattle_osa", class(osa))
osa_summary <- Rceattle::osa_diagnostics(osa, seed = 20260831L)
saveRDS(osa, file.path(output_directory, "model_26_0_osa_residuals.rds"))
write.csv(
  osa_summary,
  file.path(output_directory, "model_26_0_osa_diagnostics.csv"),
  row.names = FALSE
)

osa_panels <- afscOSA::plot_osa(
  afscosa_composition,
  plot = FALSE,
  use_agg_proportions = TRUE,
  figheight = 13,
  figwidth = 12
)
osa_panels$aggcomp <- osa_panels$aggcomp + ggthemes::theme_few(base_size = 10)
osa_panels$qq <- osa_panels$qq + ggthemes::theme_few(base_size = 10)
osa_panels$bubble <- osa_panels$bubble +
  ggthemes::theme_few(base_size = 10) +
  theme(legend.position = "top")
osa_panels$bubble_pearson <- osa_panels$bubble_pearson +
  ggthemes::theme_few(base_size = 10) +
  theme(legend.position = "none")
osa_figure <- cowplot::plot_grid(
  osa_panels$aggcomp,
  osa_panels$qq,
  osa_panels$bubble,
  osa_panels$bubble_pearson,
  nrow = 4,
  rel_heights = c(5, 5, 4.5, 4)
)
ggsave(
  file.path(output_directory, "model_26_0_afscosa.png"),
  osa_figure,
  width = 12,
  height = 11.5,
  dpi = 180
)

write.csv(
  bind_rows(lapply(afscosa_composition, function(x) {
    x$agg |>
      distinct(fleet, ISS, ESS)
  })),
  file.path(output_directory, "model_26_0_afscosa_sample_sizes.csv"),
  row.names = FALSE
)

# Fifty phased jitter refits test whether dispersed starting values return to
# the same solution. Standard-error calculations are skipped because this test
# compares objectives and point estimates.
jitter_seed <- 20260831L
jitter_result <- Rceattle::jitter(
  object = model_26,
  njitter = 50L,
  sd = 0.2,
  phase = TRUE,
  seed = jitter_seed,
  cores = 4L,
  getsd = FALSE,
  timeout = 3600
)
saveRDS(
  jitter_result,
  file.path(output_directory, "model_26_0_jitter_50.rds")
)

jitter_objective <- as.numeric(jitter_result$nll)
jitter_names <- names(jitter_result$nll)
if (is.null(jitter_names)) {
  jitter_names <- paste0("Jitter ", seq_along(jitter_objective))
}
jitter_run_summary <- data.frame(
  Run = jitter_names,
  Objective = jitter_objective,
  Delta_objective = jitter_objective - min(c(model_26$opt$objective, jitter_objective)),
  Returned_to_best = abs(jitter_objective - min(c(model_26$opt$objective, jitter_objective))) <= 1e-3
)
write.csv(
  jitter_run_summary,
  file.path(output_directory, "model_26_0_jitter_50_runs.csv"),
  row.names = FALSE
)

jitter_summary <- data.frame(
  Requested_runs = 50L,
  Completed_converged_runs = length(jitter_objective),
  Nonconverged_or_timed_out_runs = 50L - length(jitter_objective),
  Runs_returning_to_best = sum(jitter_run_summary$Returned_to_best),
  Best_objective = min(c(model_26$opt$objective, jitter_objective)),
  Largest_delta_objective = if (length(jitter_objective)) {
    max(jitter_run_summary$Delta_objective)
  } else {
    NA_real_
  },
  Jitter_standard_deviation = 0.2,
  Seed = jitter_seed
)
write.csv(
  jitter_summary,
  file.path(output_directory, "model_26_0_jitter_50_summary.csv"),
  row.names = FALSE
)

jitter_figure <- ggplot(jitter_run_summary, aes(x = Delta_objective)) +
  geom_histogram(bins = 20, fill = "#176b87", color = "white") +
  geom_vline(xintercept = 1e-3, linetype = "dashed", color = "#D55E00") +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.001)) +
  labs(
    x = "Objective difference from the best solution",
    y = "Number of completed jitter fits"
  ) +
  ggthemes::theme_few()
ggsave(
  file.path(output_directory, "model_26_0_jitter_50.png"),
  jitter_figure,
  width = 7.5,
  height = 4.8,
  dpi = 180
)

capture.output(
  sessionInfo(),
  file = file.path(output_directory, "model_26_0_session_info.txt")
)

print(convergence_summary, row.names = FALSE)
print(comparison_summary, row.names = FALSE)
print(osa_summary, row.names = FALSE)
print(jitter_summary, row.names = FALSE)
