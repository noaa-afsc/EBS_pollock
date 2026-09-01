#!/usr/bin/env Rscript

# Model 26.0 whole-index-fleet sensitivity analysis.
#
# Each sensitivity turns off one survey fleet in the likelihood while retaining
# the base Model 26.0 settings and starting from its converged estimates. The
# ATS sensitivity also turns off the separate ATS age-1 index fleet. CPUE is
# retained in every fit. These are data-influence diagnostics rather than
# competing model specifications.

rceattle_library <- Sys.getenv(
  "RCEATTLE_LIB",
  "/tmp/rceattle-dsem-v5-lib-01a05b46"
)
.libPaths(c(rceattle_library, .libPaths()))

suppressPackageStartupMessages({
  library(Rceattle)
  library(dplyr)
  library(ggplot2)
  library(tidyr)
})

expected_version <- package_version("5.23.0")
expected_commit <- "adf22f84b399e84e3b9a707e0b3bbb1624179a27"
stopifnot(packageVersion("Rceattle") == expected_version)

project_root <- normalizePath(getwd(), mustWork = TRUE)
base_result_directory <- Sys.getenv(
  "MODEL26_OUTPUT_DIR",
  file.path(project_root, "results", "model_26_0_rceattle_5.23.0")
)
output_directory <- file.path(base_result_directory, "index_series_sensitivity")
checkpoint_directory <- file.path(output_directory, "checkpoints")
dir.create(checkpoint_directory, recursive = TRUE, showWarnings = FALSE)

base_fit_file <- file.path(base_result_directory, "model_26_0_fit.rds")
stopifnot(file.exists(base_fit_file))
saved <- readRDS(base_fit_file)
reference <- saved$model_26
reference_data <- saved$data
stopifnot(
  identical(saved$Rceattle_version, as.character(expected_version)),
  identical(saved$Rceattle_commit, expected_commit),
  identical(reference$convergence$status, "OK"),
  isTRUE(reference$sdrep$pdHess)
)

assessment_years <- reference_data$styr:reference_data$endyr
assessment_columns <- match(
  as.character(assessment_years),
  dimnames(reference$quantities$ssb)[[2]]
)
stopifnot(!anyNA(assessment_columns))

scenario_manifest <- list(
  "Drop BTS" = "BTS",
  "Drop ATS" = c("ATS", "ATS_1"),
  "Drop AVO" = "AVO"
)
stopifnot(
  all(unique(unlist(scenario_manifest)) %in% reference_data$fleet_control$Fleet_name),
  reference_data$fleet_control$Fleet_type[
    match("CPUE", reference_data$fleet_control$Fleet_name)
  ] == "Survey"
)

fit_control_sensitivity <- Rceattle::fit_control(
  verbose = 0,
  phase = TRUE,
  bias_adjust_proc = FALSE,
  bias_adjust_obs = FALSE,
  comp_offset = 1e-3
)
m1_function <- Rceattle::build_M1(updateM1 = TRUE, M1_model = "fixed")

turn_off_fleets <- function(data, fleet_names) {
  changed <- data
  fleet_rows <- match(fleet_names, changed$fleet_control$Fleet_name)
  stopifnot(!anyNA(fleet_rows))
  changed$fleet_control$Fleet_type[fleet_rows] <- "Off"
  stopifnot(
    changed$fleet_control$Fleet_type[
      match("CPUE", changed$fleet_control$Fleet_name)
    ] == "Survey"
  )
  changed
}

fit_sensitivity <- function(scenario, fleet_names) {
  scenario_id <- gsub("[^a-z0-9]+", "_", tolower(scenario))
  checkpoint <- file.path(checkpoint_directory, paste0(scenario_id, ".rds"))
  if (file.exists(checkpoint)) {
    message("Using checkpoint: ", scenario)
    return(readRDS(checkpoint))
  }

  message("Fitting ", scenario, ": ", paste(fleet_names, collapse = ", "))
  sensitivity_data <- turn_off_fleets(reference_data, fleet_names)
  start_time <- proc.time()[["elapsed"]]
  fit <- suppressWarnings(Rceattle::fit_mod(
    data_list = sensitivity_data,
    inits = reference$estimated_params,
    file = NULL,
    estimateMode = 0,
    random_rec = FALSE,
    msmMode = 0,
    initMode = "NonEquilibrium",
    M1Fun = m1_function,
    fit_control = fit_control_sensitivity
  ))
  elapsed_seconds <- proc.time()[["elapsed"]] - start_time
  result <- list(
    Scenario = scenario,
    Fleets_off = fleet_names,
    Fit = fit,
    Elapsed_seconds = elapsed_seconds,
    Rceattle_version = as.character(packageVersion("Rceattle")),
    Rceattle_commit = expected_commit
  )
  saveRDS(result, checkpoint)
  message(
    "Completed ", scenario,
    ": status=", fit$convergence$status,
    ", objective=", format(fit$opt$objective, digits = 10),
    ", elapsed=", round(elapsed_seconds, 1), " s"
  )
  result
}

sensitivity_results <- Map(
  fit_sensitivity,
  names(scenario_manifest),
  unname(scenario_manifest)
)

convergence_value <- function(model, path, default = NA) {
  value <- tryCatch(Reduce(`[[`, path, init = model), error = function(e) default)
  if (is.null(value) || !length(value)) default else value
}

extract_series <- function(model, scenario) {
  tibble(
    Scenario = scenario,
    Year = assessment_years,
    `Spawning biomass` = as.numeric(model$quantities$ssb[1, assessment_columns]),
    `Total biomass` = as.numeric(model$quantities$biomass[1, assessment_columns]),
    `Age-1 recruitment` = as.numeric(model$quantities$R[1, assessment_columns]),
    `Fishing mortality` = as.numeric(model$quantities$F_spp[1, assessment_columns])
  ) |>
    pivot_longer(
      cols = -c(Scenario, Year),
      names_to = "Quantity",
      values_to = "Value"
    )
}

trajectory_values <- bind_rows(
  extract_series(reference, "Model 26.0 base"),
  bind_rows(lapply(sensitivity_results, function(result) {
    extract_series(result$Fit, result$Scenario)
  }))
)

reference_values <- trajectory_values |>
  filter(Scenario == "Model 26.0 base") |>
  select(Year, Quantity, Reference_value = Value)

trajectory_comparison <- trajectory_values |>
  left_join(reference_values, by = c("Year", "Quantity")) |>
  mutate(Percent_change = 100 * (Value / Reference_value - 1))

scenario_counts <- bind_rows(lapply(names(scenario_manifest), function(scenario) {
  fleet_names <- scenario_manifest[[scenario]]
  tibble(
    Scenario = scenario,
    Fleets_off = paste(fleet_names, collapse = ", "),
    Index_observations_excluded = sum(
      reference_data$index_data$Fleet_name %in% fleet_names &
        reference_data$index_data$Year > 0 &
        reference_data$index_data$Year <= reference_data$endyr
    ),
    Composition_years_excluded = sum(
      reference_data$comp_data$Fleet_name %in% fleet_names &
        reference_data$comp_data$Year > 0 &
        reference_data$comp_data$Year <= reference_data$endyr &
        reference_data$comp_data$Sample_size > 0
    )
  )
}))

terminal_changes <- trajectory_comparison |>
  filter(Scenario != "Model 26.0 base", Year == reference_data$endyr) |>
  select(Scenario, Quantity, Percent_change) |>
  pivot_wider(names_from = Quantity, values_from = Percent_change) |>
  rename(
    Terminal_SSB_percent_change = `Spawning biomass`,
    Terminal_total_biomass_percent_change = `Total biomass`,
    Terminal_recruitment_percent_change = `Age-1 recruitment`,
    Terminal_fishing_mortality_percent_change = `Fishing mortality`
  )

maximum_changes <- trajectory_comparison |>
  filter(Scenario != "Model 26.0 base") |>
  group_by(Scenario, Quantity) |>
  summarise(
    Maximum_absolute_percent_change = max(abs(Percent_change), na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(names_from = Quantity, values_from = Maximum_absolute_percent_change) |>
  rename(
    Maximum_absolute_SSB_percent_change = `Spawning biomass`,
    Maximum_absolute_total_biomass_percent_change = `Total biomass`,
    Maximum_absolute_recruitment_percent_change = `Age-1 recruitment`,
    Maximum_absolute_fishing_mortality_percent_change = `Fishing mortality`
  )

convergence_summary <- bind_rows(lapply(sensitivity_results, function(result) {
  fit <- result$Fit
  tibble(
    Scenario = result$Scenario,
    Convergence_status = fit$convergence$status,
    Maximum_gradient = as.numeric(convergence_value(
      fit,
      c("convergence", "checks", "max_gradient", "data", "max_gradient")
    )),
    Positive_definite_Hessian = isTRUE(fit$sdrep$pdHess),
    Hessian_condition_number = as.numeric(convergence_value(
      fit,
      c("convergence", "checks", "hessian_conditioning", "data", "condition_number")
    )),
    Elapsed_seconds = result$Elapsed_seconds
  )
}))

sensitivity_summary <- scenario_counts |>
  left_join(convergence_summary, by = "Scenario") |>
  left_join(terminal_changes, by = "Scenario") |>
  left_join(maximum_changes, by = "Scenario")

write.csv(
  sensitivity_summary,
  file.path(output_directory, "model_26_0_index_series_sensitivity_summary.csv"),
  row.names = FALSE
)
write.csv(
  trajectory_comparison,
  file.path(output_directory, "model_26_0_index_series_sensitivity_trajectories.csv"),
  row.names = FALSE
)

scenario_levels <- c("Drop BTS", "Drop ATS", "Drop AVO")
quantity_levels <- c(
  "Spawning biomass", "Total biomass", "Age-1 recruitment", "Fishing mortality"
)
plot_data <- trajectory_comparison |>
  filter(Scenario != "Model 26.0 base") |>
  mutate(
    Scenario = factor(Scenario, levels = scenario_levels),
    Quantity = factor(Quantity, levels = quantity_levels)
  )

sensitivity_colors <- c(
  "Drop BTS" = "#0072B2",
  "Drop ATS" = "#D55E00",
  "Drop AVO" = "#009E73"
)
sensitivity_linetypes <- c(
  "Drop BTS" = "solid",
  "Drop ATS" = "longdash",
  "Drop AVO" = "dotdash"
)

sensitivity_figure <- ggplot(
  plot_data,
  aes(x = Year, y = Percent_change, color = Scenario, linetype = Scenario)
) +
  geom_hline(yintercept = 0, color = "grey55", linewidth = 0.35) +
  geom_line(linewidth = 0.8) +
  facet_wrap(vars(Quantity), ncol = 2, scales = "free_y") +
  scale_color_manual(values = sensitivity_colors) +
  scale_linetype_manual(values = sensitivity_linetypes) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    x = "Year",
    y = "Change from Model 26.0 base",
    color = NULL,
    linetype = NULL
  ) +
  ggthemes::theme_few(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 10)
  )

ggsave(
  file.path(output_directory, "model_26_0_index_series_sensitivity.png"),
  sensitivity_figure,
  width = 9,
  height = 7,
  dpi = 220
)

stopifnot(
  nrow(sensitivity_summary) == 3L,
  all(sensitivity_summary$Convergence_status == "OK"),
  all(sensitivity_summary$Positive_definite_Hessian),
  all(is.finite(sensitivity_summary$Maximum_gradient)),
  all(reference_data$fleet_control$Fleet_type[
    match("CPUE", reference_data$fleet_control$Fleet_name)
  ] == "Survey")
)

message("Wrote Model 26.0 index-series sensitivities to ", output_directory)
