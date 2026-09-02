#!/usr/bin/env Rscript

# Script ID M26-11: observation continuity and event-level case deletion.
#
# The observation inventory uses an explicit schedule instead of inferring
# field operations from gaps. Each case deletion removes one complete fitted
# sampling event and refits Model 26.0 from the converged full-data estimates.
# Large checkpoint objects remain in the local analysis results directory.

rceattle_library <- Sys.getenv(
  "RCEATTLE_LIB",
  "/tmp/rceattle-dsem-v5-lib-01a05b46"
)
.libPaths(c(rceattle_library, .libPaths()))

suppressPackageStartupMessages({
  library(Rceattle)
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(tidyr)
})

expected_version <- package_version("5.23.0")
expected_commit <- "adf22f84b399e84e3b9a707e0b3bbb1624179a27"
stopifnot(packageVersion("Rceattle") == expected_version)

project_root <- normalizePath(getwd(), mustWork = TRUE)
fit_file <- Sys.getenv(
  "MODEL26_FIT_FILE",
  file.path(
    project_root, "results", "model_26_0_rceattle_5.23.0",
    "model_26_0_fit.rds"
  )
)
output_directory <- Sys.getenv(
  "MODEL26_CASE_OUTPUT_DIR",
  file.path(project_root, "results", "model_26_0_case_deletion_5.23.0")
)
checkpoint_directory <- file.path(output_directory, "checkpoints")
dir.create(checkpoint_directory, recursive = TRUE, showWarnings = FALSE)

saved <- readRDS(fit_file)
reference <- saved$model_26
stage_a <- saved$stage_a
stopifnot(
  identical(saved$Rceattle_version, as.character(expected_version)),
  identical(saved$Rceattle_commit, expected_commit),
  identical(reference$convergence$status, "OK"),
  isTRUE(reference$sdrep$pdHess)
)

assessment_end_year <- reference$data_list$endyr
display_years <- 2014:2026

component_schedule <- tibble::tribble(
  ~Component, ~Table, ~Fleet, ~First_year, ~Last_year, ~Schedule,
  "Fishery age composition", "comp_data", "Fishery", 1964L, assessment_end_year, "annual",
  "BTS biomass index", "index_data", "BTS", 1982L, assessment_end_year, "annual",
  "BTS age composition", "comp_data", "BTS", 1982L, assessment_end_year, "annual",
  "BTS age-1 index", "index_data", "BTS_1", 1982L, assessment_end_year, "annual",
  "ATS biomass index", "index_data", "ATS", 1994L, assessment_end_year, "even",
  "ATS age composition", "comp_data", "ATS", 1994L, assessment_end_year, "even",
  "ATS age-1 index", "index_data", "ATS_1", 1994L, assessment_end_year, "even",
  "AVO index", "index_data", "AVO", 2006L, assessment_end_year, "annual"
)

is_scheduled <- function(year, schedule) {
  if (schedule == "annual") return(TRUE)
  if (schedule == "even") return(year %% 2L == 0L)
  stop("Unsupported schedule: ", schedule)
}

index_weight <- function(data, fleet, year) {
  if (fleet == "BTS") {
    covariance <- data$index_cov$BTS
    key <- as.character(year)
    if (key %in% rownames(covariance)) return(1 / covariance[key, key])
  }
  row <- data$index_data |>
    filter(.data$Fleet_name == fleet, abs(.data$Year) == year)
  if (nrow(row) != 1L || !is.finite(row$Log_sd) || row$Log_sd <= 0) {
    return(NA_real_)
  }
  1 / row$Log_sd^2
}

inventory_row <- function(schedule_row, year) {
  source_data <- reference$data_list[[schedule_row$Table]]
  available <- source_data |>
    filter(
      .data$Fleet_name == schedule_row$Fleet,
      abs(.data$Year) == year
    )
  fleet_type <- reference$data_list$fleet_control$Fleet_type[
    match(
      schedule_row$Fleet,
      reference$data_list$fleet_control$Fleet_name
    )
  ]
  scheduled <- year >= schedule_row$First_year &&
    year <= schedule_row$Last_year &&
    is_scheduled(year, schedule_row$Schedule)

  status <- if (year < schedule_row$First_year || year > schedule_row$Last_year) {
    "Outside fitted span"
  } else if (!scheduled) {
    "Not scheduled"
  } else if (nrow(available) == 0L) {
    "No observation in scheduled year"
  } else if (
    fleet_type == "Off" ||
      available$Year[1] < 0 ||
      (schedule_row$Table == "comp_data" && available$Sample_size[1] <= 0)
  ) {
    "Available but excluded from likelihood"
  } else {
    "Represented in base fit"
  }

  weight_definition <- if (schedule_row$Table == "comp_data") {
    "Nominal composition sample size"
  } else if (schedule_row$Fleet == "BTS") {
    "Inverse diagonal BTS covariance"
  } else {
    "Inverse squared observation SD"
  }
  raw_weight <- NA_real_
  if (nrow(available) == 1L) {
    raw_weight <- if (schedule_row$Table == "comp_data") {
      available$Sample_size[1]
    } else {
      index_weight(reference$data_list, schedule_row$Fleet, year)
    }
  }

  tibble(
    Component = schedule_row$Component,
    Table = schedule_row$Table,
    Fleet = schedule_row$Fleet,
    Year = year,
    Scheduled = scheduled,
    Status = status,
    Raw_weight = raw_weight,
    Weight_definition = weight_definition
  )
}

continuity_inventory <- bind_rows(lapply(seq_len(nrow(component_schedule)), function(i) {
  bind_rows(lapply(display_years, function(year) {
    inventory_row(component_schedule[i, ], year)
  }))
})) |>
  group_by(.data$Component) |>
  mutate(
    Relative_weight = if (sum(is.finite(.data$Raw_weight)) > 1L) {
      scales::rescale(log1p(.data$Raw_weight), to = c(0.35, 1))
    } else ifelse(is.finite(.data$Raw_weight), 0.65, NA_real_),
    Display_weight = ifelse(
      .data$Status == "Represented in base fit",
      .data$Relative_weight,
      0.22
    )
  ) |>
  ungroup()

write_csv(continuity_inventory, file.path(output_directory, "continuity_inventory.csv"))
write_csv(component_schedule, file.path(output_directory, "continuity_schedule.csv"))

case_manifest <- tibble::tribble(
  ~Scenario, ~Table, ~Fleet, ~Year, ~Selection_reason,
  "Fishery composition 2020", "comp_data", "Fishery", 2020L, "2020--2021 transition",
  "Fishery composition 2023", "comp_data", "Fishery", 2023L, "Latest fitted event",
  "BTS biomass index 2021", "index_data", "BTS", 2021L, "2020--2021 transition",
  "BTS biomass index 2024", "index_data", "BTS", 2024L, "Latest fitted event",
  "BTS age composition 2021", "comp_data", "BTS", 2021L, "2020--2021 transition",
  "BTS age composition 2024", "comp_data", "BTS", 2024L, "Latest fitted event",
  "ATS biomass index 2024", "index_data", "ATS", 2024L, "Latest fitted event",
  "ATS age composition 2024", "comp_data", "ATS", 2024L, "Latest fitted event",
  "ATS age-1 index 2022", "index_data", "ATS_1", 2022L, "Latest fitted event",
  "AVO index 2021", "index_data", "AVO", 2021L, "2020--2021 transition",
  "AVO index 2024", "index_data", "AVO", 2024L, "Latest fitted event"
) |>
  mutate(
    Scenario_id = gsub("[^a-z0-9]+", "_", tolower(.data$Scenario)),
    Scenario_id = gsub("(^_|_$)", "", .data$Scenario_id),
    Checkpoint = file.path(
      checkpoint_directory, paste0(.data$Scenario_id, ".rds")
    )
  ) |>
  select(
    "Scenario_id", "Scenario", "Table", "Fleet", "Year",
    "Selection_reason", "Checkpoint"
  )
write_csv(
  select(case_manifest, -"Checkpoint"),
  file.path(output_directory, "case_deletion_manifest.csv")
)

fit_control_case <- Rceattle::fit_control(
  verbose = 0,
  phase = TRUE,
  bias_adjust_proc = FALSE,
  bias_adjust_obs = FALSE,
  comp_offset = 1e-3
)
m1_function <- Rceattle::build_M1(updateM1 = TRUE, M1_model = "fixed")

delete_event <- function(data, table, fleet, year) {
  changed <- data
  selected <- changed[[table]]$Fleet_name == fleet &
    changed[[table]]$Year == year
  if (sum(selected) != 1L) {
    stop(
      "Expected one row for ", table, "/", fleet, "/", year,
      "; found ", sum(selected), "."
    )
  }
  changed[[table]] <- changed[[table]][!selected, , drop = FALSE]
  changed
}

fit_variant <- function(data, inits) {
  suppressWarnings(Rceattle::fit_mod(
    data_list = data,
    inits = inits,
    map = reference$map,
    file = NULL,
    estimateMode = 0,
    random_rec = FALSE,
    msmMode = 0,
    initMode = "NonEquilibrium",
    M1Fun = m1_function,
    fit_control = fit_control_case
  ))
}

assessment_years <- as.integer(colnames(reference$quantities$R))
assessment_columns <- assessment_years <= assessment_end_year

extract_trajectory <- function(fit, scenario_id, scenario) {
  tibble(
    Scenario_id = scenario_id,
    Scenario = scenario,
    Year = assessment_years[assessment_columns],
    SSB = as.numeric(fit$quantities$ssb[1, assessment_columns]),
    Recruitment = as.numeric(fit$quantities$R[1, assessment_columns])
  )
}

reference_trajectory <- extract_trajectory(
  reference, "reference", "All reference observations"
)

summarize_fit <- function(fit, manifest_row, elapsed_seconds) {
  trajectory <- extract_trajectory(
    fit, manifest_row$Scenario_id, manifest_row$Scenario
  ) |>
    left_join(
      select(
        reference_trajectory, "Year",
        Reference_SSB = "SSB", Reference_Recruitment = "Recruitment"
      ),
      by = "Year"
    ) |>
    mutate(
      SSB_percent_change = 100 * (.data$SSB / .data$Reference_SSB - 1),
      Recruitment_percent_change = 100 *
        (.data$Recruitment / .data$Reference_Recruitment - 1)
    )
  event_row <- filter(trajectory, .data$Year == manifest_row$Year)
  terminal_row <- filter(trajectory, .data$Year == assessment_end_year)
  max_gradient <- fit$convergence$checks$max_gradient$data$max_gradient
  condition_number <-
    fit$convergence$checks$hessian_conditioning$data$condition_number

  list(
    summary = tibble(
      Scenario_id = manifest_row$Scenario_id,
      Scenario = manifest_row$Scenario,
      Table = manifest_row$Table,
      Fleet = manifest_row$Fleet,
      Deleted_year = manifest_row$Year,
      Selection_reason = manifest_row$Selection_reason,
      Objective = as.numeric(fit$opt$objective),
      Convergence_status = fit$convergence$status,
      Maximum_gradient = as.numeric(max_gradient),
      Positive_definite_Hessian = isTRUE(fit$sdrep$pdHess),
      Hessian_condition_number = as.numeric(condition_number),
      Event_year_SSB_percent_change = event_row$SSB_percent_change,
      Event_year_recruitment_percent_change =
        event_row$Recruitment_percent_change,
      Terminal_SSB_percent_change = terminal_row$SSB_percent_change,
      Terminal_recruitment_percent_change =
        terminal_row$Recruitment_percent_change,
      Maximum_absolute_SSB_percent_change =
        max(abs(trajectory$SSB_percent_change), na.rm = TRUE),
      Maximum_absolute_recruitment_percent_change =
        max(abs(trajectory$Recruitment_percent_change), na.rm = TRUE),
      Elapsed_seconds = elapsed_seconds
    ),
    trajectory = trajectory
  )
}

run_case <- function(manifest_row) {
  if (file.exists(manifest_row$Checkpoint)) {
    message("Using checkpoint: ", manifest_row$Scenario)
    return(readRDS(manifest_row$Checkpoint))
  }
  message("Refitting without: ", manifest_row$Scenario)
  changed <- delete_event(
    reference$data_list, manifest_row$Table, manifest_row$Fleet,
    manifest_row$Year
  )
  start <- proc.time()[["elapsed"]]
  fit <- fit_variant(changed, reference$estimated_params)
  elapsed <- proc.time()[["elapsed"]] - start
  result <- summarize_fit(fit, manifest_row, elapsed)
  saveRDS(result, manifest_row$Checkpoint)
  message(
    "Completed ", manifest_row$Scenario,
    ": status=", result$summary$Convergence_status,
    ", max|gradient|=", format(result$summary$Maximum_gradient, digits = 3)
  )
  result
}

case_results <- lapply(seq_len(nrow(case_manifest)), function(i) {
  run_case(case_manifest[i, ])
})
case_summary <- bind_rows(lapply(case_results, `[[`, "summary"))
case_trajectories <- bind_rows(
  reference_trajectory |>
    mutate(
      Reference_SSB = .data$SSB,
      Reference_Recruitment = .data$Recruitment,
      SSB_percent_change = 0,
      Recruitment_percent_change = 0
    ),
  bind_rows(lapply(case_results, `[[`, "trajectory"))
)
write_csv(case_summary, file.path(output_directory, "case_deletion_summary.csv"))
write_csv(
  case_trajectories,
  file.path(output_directory, "case_deletion_trajectories.csv")
)

continuity_status_levels <- c(
  "Represented in base fit", "No observation in scheduled year",
  "Available but excluded from likelihood", "Not scheduled",
  "Outside fitted span"
)
continuity_components <- unique(continuity_inventory$Component)
continuity_figure <- continuity_inventory |>
  mutate(
    Component = factor(.data$Component, levels = rev(continuity_components)),
    Status = factor(.data$Status, levels = continuity_status_levels),
    Point_size = 2.0 + 5.5 * .data$Display_weight
  ) |>
  ggplot(aes(
    x = .data$Year, y = .data$Component, color = .data$Status,
    fill = .data$Status, shape = .data$Status, size = .data$Point_size
  )) +
  geom_vline(
    xintercept = 2024.5, color = "grey55", linetype = "dashed",
    linewidth = 0.4
  ) +
  geom_point(stroke = 0.9) +
  scale_x_continuous(breaks = 2014:2026) +
  scale_color_manual(values = c(
    "Represented in base fit" = "#16876c",
    "No observation in scheduled year" = "#b2182b",
    "Available but excluded from likelihood" = "#6a3d9a",
    "Not scheduled" = "#969696", "Outside fitted span" = "#636363"
  )) +
  scale_fill_manual(values = c(
    "Represented in base fit" = "#16876c",
    "No observation in scheduled year" = "#b2182b",
    "Available but excluded from likelihood" = "white",
    "Not scheduled" = "white", "Outside fitted span" = "#636363"
  )) +
  scale_shape_manual(values = c(
    "Represented in base fit" = 16,
    "No observation in scheduled year" = 4,
    "Available but excluded from likelihood" = 24,
    "Not scheduled" = 1, "Outside fitted span" = 3
  )) +
  scale_size_identity() +
  labs(x = "Reference year", y = NULL, color = NULL, shape = NULL) +
  guides(
    color = guide_legend(override.aes = list(size = 4)),
    fill = "none", shape = guide_legend(override.aes = list(size = 4))
  ) +
  ggthemes::theme_few(base_size = 10) +
  theme(
    legend.position = "bottom", legend.box = "vertical",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_line(color = "grey88")
  )
ggsave(
  file.path(output_directory, "model_26_0_observation_continuity.png"),
  continuity_figure, width = 12, height = 6.7, dpi = 300
)

scenario_order <- case_summary |>
  mutate(
    Maximum_terminal_change = pmax(
      abs(.data$Terminal_SSB_percent_change),
      abs(.data$Terminal_recruitment_percent_change)
    )
  ) |>
  arrange(.data$Maximum_terminal_change) |>
  pull("Scenario")
influence_figure <- case_summary |>
  select(
    "Scenario", SSB = "Terminal_SSB_percent_change",
    Recruitment = "Terminal_recruitment_percent_change"
  ) |>
  pivot_longer(
    cols = c("SSB", "Recruitment"),
    names_to = "Quantity", values_to = "Percent_change"
  ) |>
  mutate(
    Scenario = factor(.data$Scenario, levels = scenario_order),
    Quantity = factor(.data$Quantity, levels = c("SSB", "Recruitment"))
  ) |>
  ggplot(aes(
    x = .data$Percent_change, y = .data$Scenario,
    color = .data$Quantity, shape = .data$Quantity
  )) +
  geom_vline(xintercept = 0, color = "grey45", linewidth = 0.45) +
  geom_segment(
    aes(x = 0, xend = .data$Percent_change, yend = .data$Scenario),
    linewidth = 0.6, alpha = 0.55
  ) +
  geom_point(size = 3) +
  scale_color_manual(values = c("SSB" = "#0072B2", "Recruitment" = "#D55E00")) +
  scale_shape_manual(values = c("SSB" = 16, "Recruitment" = 17)) +
  scale_x_continuous(labels = scales::label_number(suffix = "%", accuracy = 1)) +
  labs(
    x = "Change from complete reference fit", y = "Deleted sampling event",
    color = NULL, shape = NULL
  ) +
  ggthemes::theme_few(base_size = 10) +
  theme(legend.position = "bottom")
ggsave(
  file.path(output_directory, "model_26_0_case_deletion_influence.png"),
  influence_figure, width = 10.5, height = 7.5, dpi = 300
)

write_csv(
  tibble(
    Item = c(
      "Script ID", "Rceattle branch", "Rceattle commit", "Rceattle version",
      "Reference fit", "Assessment end year", "Displayed years",
      "Case-deletion refits"
    ),
    Value = c(
      "M26-11", "dsem-v5-integration", expected_commit,
      as.character(packageVersion("Rceattle")), normalizePath(fit_file),
      assessment_end_year, paste(range(display_years), collapse = "--"),
      nrow(case_summary)
    )
  ),
  file.path(output_directory, "case_deletion_run_manifest.csv")
)
writeLines(
  capture.output(sessionInfo()),
  file.path(output_directory, "case_deletion_session_info.txt")
)

stopifnot(
  nrow(case_summary) == nrow(case_manifest),
  all(case_summary$Convergence_status == "OK"),
  all(case_summary$Positive_definite_Hessian),
  all(is.finite(case_summary$Maximum_gradient))
)
message("Observation-continuity and case-deletion products are complete.")
