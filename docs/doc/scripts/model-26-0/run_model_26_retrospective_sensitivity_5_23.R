#!/usr/bin/env Rscript

# Model 26.0 retrospective validation under Rceattle 5.23.0.
#
# The primary analysis matches the established EBS pollock retrospective
# specification: preserve each observation stream's terminal lag, recalculate
# the empirical fishery-selectivity start for each peel, fit in two stages, and
# hold the terminal fishery/CPUE selectivity curve at its preceding-year value.
# The native Rceattle retrospective is retained as an implementation sensitivity.

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

stopifnot(packageVersion("Rceattle") == package_version("5.23.0"))

project_root <- normalizePath(
  Sys.getenv("MODEL26_PROJECT_ROOT", getwd()),
  mustWork = TRUE
)
output_directory <- Sys.getenv(
  "MODEL26_RETRO_SENSITIVITY_OUTPUT_DIR",
  file.path(project_root, "results", "model_26_0_retrospective_sensitivity_5.23.0")
)
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

base_result <- readRDS(file.path(
  project_root,
  "results",
  "model_26_0_rceattle_5.23.0",
  "model_26_0_fit.rds"
))
base_fit <- base_result$model_26
# The fitted object contains Rceattle's normalized control encodings. Retain
# those active controls in every peel; the pre-fit input copy uses earlier
# aliases for several fields.
base_data <- base_fit$data_list
base_endyr <- base_data$endyr
styr <- base_data$styr
peels <- 9L
peel_years <- (base_endyr - peels):(base_endyr - 1L)
n_selages_fsh <- 12L

native_file <- file.path(
  project_root,
  "results",
  "model_26_0_projection_retro_5.23.0",
  "model_26_0_retrospective_9.rds"
)
if (!file.exists(native_file)) {
  stop("Run R/run_model_26_projection_retro_5_23.R before this validation.")
}
native_retro <- readRDS(native_file)
native_models <- native_retro$Rceattle_list

apply_observation_lags <- function(peeled_data, reference_data, endyr_peel) {
  apply_group_lag <- function(x, reference, group_var) {
    if (!NROW(x) || !NROW(reference)) return(x)
    reference_max <- reference |>
      filter(.data$Year > 0) |>
      group_by(.data[[group_var]]) |>
      summarise(max_year = max(.data$Year), .groups = "drop") |>
      mutate(
        lag = base_endyr - .data$max_year,
        cutoff = if_else(
          .data$lag <= peels,
          endyr_peel - .data$lag,
          .data$max_year
        )
      ) |>
      select(all_of(group_var), .data$cutoff)

    x |>
      left_join(reference_max, by = group_var) |>
      filter(
        .data$Year <= .data$cutoff |
          .data$Year <= 0 |
          is.na(.data$cutoff)
      ) |>
      select(-.data$cutoff)
  }

  peeled_data$index_data <- apply_group_lag(
    peeled_data$index_data,
    reference_data$index_data,
    "Fleet_code"
  )
  peeled_data$comp_data <- apply_group_lag(
    peeled_data$comp_data,
    reference_data$comp_data,
    "Fleet_code"
  )
  peeled_data
}

empirical_start <- function(data_list) {
  years <- data_list$styr:data_list$endyr
  nyears <- length(years)
  fishery_code <- data_list$fleet_control$Fleet_code[
    data_list$fleet_control$Fleet_name == "Fishery"
  ]

  preliminary_fit <- Rceattle::fit_mod(
    data_list = data_list,
    inits = NULL,
    file = NULL,
    estimateMode = 0,
    random_rec = FALSE,
    msmMode = 0,
    initMode = "NonEquilibrium",
    M1Fun = Rceattle::build_M1(updateM1 = TRUE, M1_model = "fixed"),
    fit_control = Rceattle::fit_control(
      verbose = 0,
      phase = TRUE,
      bias_adjust_proc = FALSE,
      bias_adjust_obs = FALSE,
      comp_offset = 1e-3
    )
  )

  numbers_at_age <- preliminary_fit$quantities$N_at_age[
    1,
    1,
    ,
    seq_len(nyears)
  ]
  fishery_compositions <- data_list$comp_data[
    data_list$comp_data$Fleet_code == fishery_code &
      data_list$comp_data$Year > 0 &
      data_list$comp_data$Age0_Length1 == 0,
  ]
  composition_columns <- grep(
    "^Comp_",
    names(fishery_compositions),
    value = TRUE
  )[seq_len(data_list$nages)]
  empirical_selectivity <- matrix(
    NA_real_,
    nrow(fishery_compositions),
    data_list$nages
  )
  for (i in seq_len(nrow(fishery_compositions))) {
    year_index <- match(fishery_compositions$Year[i], years)
    if (is.na(year_index)) next
    proportion <- as.numeric(fishery_compositions[i, composition_columns])
    proportion <- proportion / sum(proportion, na.rm = TRUE)
    selectivity <- proportion / pmax(numbers_at_age[, year_index], 1e-8)
    empirical_selectivity[i, ] <- selectivity / max(selectivity, na.rm = TRUE)
  }
  average_selectivity <- colMeans(empirical_selectivity, na.rm = TRUE)[
    seq_len(n_selages_fsh)
  ]
  log_selectivity <- log(pmax(
    average_selectivity / max(average_selectivity),
    1e-3
  ))
  log_selectivity <- log_selectivity - mean(log_selectivity)

  inits <- Rceattle::build_params(data_list)
  inits$sel_coff[1, 1, seq_len(n_selages_fsh)] <- log_selectivity
  inits
}

make_stage_2_map <- function(data_list, inits, endyr_peel) {
  # Rceattle 5.23 sizes hindcast parameter blocks to each peel's end year.
  # Build the active peel map directly, then apply the stock-specific terminal
  # selectivity constraint. Forecast recruitment blocks are already pinned by
  # build_map() for this fixed-recruitment configuration.
  map <- Rceattle::build_map(
    data_list = data_list,
    params = inits,
    debug = FALSE,
    random_rec = FALSE
  )
  nyrs_peel <- endyr_peel - styr + 1L

  terminal_index <- nyrs_peel
  held_fleets <- which(
    data_list$fleet_control$Fleet_name %in% c("Fishery", "CPUE")
  )
  for (fleet_index in held_fleets) {
    map$mapList$sel_coff_dev[fleet_index, 1, , terminal_index] <- NA
    inits$sel_coff_dev[fleet_index, 1, , terminal_index] <- 0
  }

  zero_catch <- as.matrix(
    data_list$catch_data |>
      filter(.data$Year <= endyr_peel, .data$Catch == 0) |>
      mutate(Year = .data$Year - styr + 1L) |>
      select(.data$Fleet_code, .data$Year)
  )
  if (nrow(zero_catch)) {
    inits$log_F[zero_catch] <- -999
    map$mapList$log_F[zero_catch] <- NA
  }

  for (parameter in names(map$mapList)) {
    map$mapFactor[[parameter]] <- factor(map$mapList[[parameter]])
  }
  list(map = map, inits = inits)
}

run_primary_peel <- function(endyr_peel) {
  checkpoint <- file.path(
    output_directory,
    paste0("primary_active_config_year_", endyr_peel, ".rds")
  )
  if (file.exists(checkpoint)) return(readRDS(checkpoint))

  message("Starting lag-preserving two-stage peel ending in ", endyr_peel)
  scaffold_model <- native_models[[paste0("Year_", endyr_peel)]]
  peeled_data <- scaffold_model$data_list
  peeled_data <- apply_observation_lags(peeled_data, base_data, endyr_peel)
  peeled_data$fleet_control <- base_data$fleet_control
  peeled_data$diet_data <- NULL

  inits <- empirical_start(peeled_data)
  stage_1_data <- peeled_data
  stage_1_data$fleet_control$Time_varying_sel <- "Off"
  control <- Rceattle::fit_control(
    verbose = 0,
    phase = TRUE,
    bias_adjust_proc = FALSE,
    bias_adjust_obs = FALSE,
    comp_offset = 1e-3
  )
  m1_function <- Rceattle::build_M1(updateM1 = TRUE, M1_model = "fixed")

  stage_1 <- Rceattle::fit_mod(
    data_list = stage_1_data,
    inits = inits,
    file = NULL,
    estimateMode = 0,
    random_rec = FALSE,
    msmMode = 0,
    initMode = "NonEquilibrium",
    M1Fun = m1_function,
    fit_control = control
  )
  stage_2_setup <- make_stage_2_map(
    peeled_data,
    stage_1$obj$env$parList(),
    endyr_peel
  )
  stage_2 <- Rceattle::fit_mod(
    data_list = peeled_data,
    inits = stage_2_setup$inits,
    map = stage_2_setup$map,
    file = NULL,
    estimateMode = 0,
    random_rec = FALSE,
    msmMode = 0,
    initMode = "NonEquilibrium",
    M1Fun = m1_function,
    fit_control = control
  )

  terminal_index <- endyr_peel - styr + 1L
  held_fleets <- which(
    peeled_data$fleet_control$Fleet_name %in% c("Fishery", "CPUE")
  )
  terminal_selectivity_difference <- setNames(
    vapply(
      held_fleets,
      function(fleet_index) {
        max(abs(
          stage_2$quantities$sel_at_age[
            fleet_index,
            1,
            ,
            terminal_index
          ] -
            stage_2$quantities$sel_at_age[
              fleet_index,
              1,
              ,
              terminal_index - 1L
            ]
        ))
      },
      numeric(1)
    ),
    peeled_data$fleet_control$Fleet_name[held_fleets]
  )
  stopifnot(
    all(is.finite(terminal_selectivity_difference)),
    max(terminal_selectivity_difference) < 1e-10
  )

  result <- list(
    terminal_year = endyr_peel,
    stage_1 = stage_1,
    stage_2 = stage_2,
    terminal_selectivity_max_difference = terminal_selectivity_difference
  )
  saveRDS(result, checkpoint)
  result
}

primary_details <- lapply(peel_years, run_primary_peel)
names(primary_details) <- paste0("Year_", peel_years)
primary_models <- c(
  lapply(primary_details, `[[`, "stage_2"),
  setNames(list(base_fit), paste0("Year_", base_endyr))
)

objects <- c("biomass", "ssb", "R", "F_spp")
quantity_labels <- c(
  biomass = "Total biomass",
  ssb = "Spawning biomass",
  R = "Age-1 recruitment",
  F_spp = "Fishing mortality"
)

calculate_mohns <- function(model_list, label) {
  bind_rows(lapply(objects, function(object) {
    relative_errors <- vapply(peel_years, function(endyr_peel) {
      year_index <- endyr_peel - styr + 1L
      peel <- model_list[[paste0("Year_", endyr_peel)]]$quantities[[object]][
        1,
        year_index
      ]
      base <- base_fit$quantities[[object]][1, year_index]
      (peel - base) / base
    }, numeric(1))
    data.frame(
      Configuration = label,
      Object = object,
      Quantity = unname(quantity_labels[object]),
      Peels = length(relative_errors),
      Mohns_rho = mean(relative_errors)
    )
  }))
}

primary_mohns <- calculate_mohns(
  primary_models,
  "Lag-preserving two-stage"
)
write.csv(
  primary_mohns,
  file.path(output_directory, "model_26_0_retrospective_primary_mohns_rho.csv"),
  row.names = FALSE
)
native_mohns <- calculate_mohns(native_models, "Native 5.23 routine")

published_file <- file.path(
  project_root,
  "results",
  "canonical_pm",
  "ebs_pollock_nonparametric_two_stage_retro_9peels.rds"
)
published_retro <- readRDS(published_file)
published_models <- published_retro$Rceattle_list
published_mohns <- calculate_mohns(
  published_models,
  "Published two-stage (5.8.1)"
)

mohn_sensitivity <- bind_rows(
  primary_mohns,
  published_mohns,
  native_mohns
)
write.csv(
  mohn_sensitivity,
  file.path(output_directory, "model_26_0_retrospective_mohn_sensitivity.csv"),
  row.names = FALSE
)

terminal_comparison <- bind_rows(lapply(
  list(
    `Lag-preserving two-stage` = primary_models,
    `Published two-stage (5.8.1)` = published_models,
    `Native 5.23 routine` = native_models
  ),
  function(model_list) {
    configuration <- deparse(substitute(model_list))
    bind_rows(lapply(peel_years, function(endyr_peel) {
      year_index <- endyr_peel - styr + 1L
      bind_rows(lapply(objects, function(object) {
        peel <- model_list[[paste0("Year_", endyr_peel)]]$quantities[[object]][
          1,
          year_index
        ]
        base <- base_fit$quantities[[object]][1, year_index]
        data.frame(
          Terminal_year = endyr_peel,
          Object = object,
          Quantity = unname(quantity_labels[object]),
          Relative_difference = (peel - base) / base
        )
      }))
    }))
  }
), .id = "Configuration")
write.csv(
  terminal_comparison,
  file.path(output_directory, "model_26_0_retrospective_terminal_sensitivity.csv"),
  row.names = FALSE
)

convergence <- bind_rows(lapply(names(primary_models), function(model_name) {
  model <- primary_models[[model_name]]
  data.frame(
    Model = model_name,
    Terminal_year = as.integer(sub("Year_", "", model_name)),
    Convergence_status = model$convergence$status,
    Maximum_gradient = model$opt$max_gradient,
    Positive_definite_Hessian = isTRUE(model$sdrep$pdHess),
    Objective = model$opt$objective
  )
}))
write.csv(
  convergence,
  file.path(output_directory, "model_26_0_retrospective_primary_convergence.csv"),
  row.names = FALSE
)

terminal_selectivity <- bind_rows(lapply(names(primary_details), function(model_name) {
  values <- primary_details[[model_name]]$terminal_selectivity_max_difference
  data.frame(
    Model = model_name,
    Terminal_year = primary_details[[model_name]]$terminal_year,
    Fleet = names(values),
    Maximum_absolute_difference = as.numeric(values)
  )
}))
write.csv(
  terminal_selectivity,
  file.path(output_directory, "model_26_0_retrospective_terminal_selectivity_check.csv"),
  row.names = FALSE
)

fleet_name <- function(data_list, fleet_code) {
  name <- data_list$fleet_control$Fleet_name[
    match(fleet_code, data_list$fleet_control$Fleet_code)
  ]
  ifelse(is.na(name), as.character(fleet_code), name)
}
lag_audit <- bind_rows(lapply(names(primary_details), function(model_name) {
  endyr_peel <- primary_details[[model_name]]$terminal_year
  peeled_data <- primary_details[[model_name]]$stage_2$data_list
  bind_rows(lapply(c("index_data", "comp_data"), function(data_name) {
    reference <- base_data[[data_name]] |>
      filter(.data$Year > 0) |>
      group_by(.data$Fleet_code) |>
      summarise(Base_maximum_year = max(.data$Year), .groups = "drop") |>
      mutate(
        Base_terminal_lag = base_endyr - .data$Base_maximum_year,
        Cutoff_year = if_else(
          .data$Base_terminal_lag <= peels,
          endyr_peel - .data$Base_terminal_lag,
          .data$Base_maximum_year
        )
      )
    expected <- base_data[[data_name]] |>
      filter(.data$Year > 0) |>
      inner_join(reference, by = "Fleet_code") |>
      filter(.data$Year <= .data$Cutoff_year) |>
      group_by(.data$Fleet_code) |>
      summarise(Expected_maximum_year = max(.data$Year), .groups = "drop")
    actual <- peeled_data[[data_name]] |>
      filter(.data$Year > 0) |>
      group_by(.data$Fleet_code) |>
      summarise(Actual_maximum_year = max(.data$Year), .groups = "drop")
    reference |>
      left_join(expected, by = "Fleet_code") |>
      left_join(actual, by = "Fleet_code") |>
      mutate(
        Model = model_name,
        Terminal_year = endyr_peel,
        Data_stream = data_name,
        Fleet = fleet_name(base_data, .data$Fleet_code),
        Pass = .data$Actual_maximum_year == .data$Expected_maximum_year
      ) |>
      select(
        .data$Model,
        .data$Terminal_year,
        .data$Data_stream,
        .data$Fleet,
        .data$Base_maximum_year,
        .data$Base_terminal_lag,
        .data$Cutoff_year,
        .data$Expected_maximum_year,
        .data$Actual_maximum_year,
        .data$Pass
      )
  }))
}))
write.csv(
  lag_audit,
  file.path(output_directory, "model_26_0_retrospective_lag_audit.csv"),
  row.names = FALSE
)
stopifnot(all(lag_audit$Pass))

trajectory_data <- bind_rows(lapply(names(primary_models), function(model_name) {
  model <- primary_models[[model_name]]
  terminal_year <- as.integer(sub("Year_", "", model_name))
  model_years <- styr:terminal_year
  n <- length(model_years)
  bind_rows(lapply(objects, function(object) {
    data.frame(
      Model = model_name,
      Terminal_year = terminal_year,
      Year = model_years,
      Quantity = unname(quantity_labels[object]),
      Value = as.numeric(model$quantities[[object]][1, seq_len(n)])
    )
  }))
}))
write.csv(
  trajectory_data,
  file.path(output_directory, "model_26_0_retrospective_primary_trajectories.csv"),
  row.names = FALSE
)

primary_figure <- ggplot(
  trajectory_data,
  aes(
    x = Year,
    y = Value,
    group = factor(Terminal_year),
    color = factor(Terminal_year),
    linewidth = Terminal_year == base_endyr
  )
) +
  geom_line(alpha = 0.9) +
  facet_wrap(vars(Quantity), ncol = 2, scales = "free_y") +
  scale_linewidth_manual(
    values = c(`FALSE` = 0.55, `TRUE` = 1.15),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "C", direction = -1) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(x = "Year", y = NULL, color = "Terminal year") +
  ggthemes::theme_few() +
  theme(legend.position = "bottom")
ggsave(
  file.path(output_directory, "model_26_0_retrospective_primary.png"),
  primary_figure,
  width = 10,
  height = 8,
  dpi = 180
)

sensitivity_figure <- terminal_comparison |>
  ggplot(aes(
    x = Terminal_year,
    y = Relative_difference,
    color = Configuration,
    shape = Configuration
  )) +
  geom_hline(yintercept = 0, color = "grey55", linewidth = 0.4) +
  geom_line(linewidth = 0.65) +
  geom_point(size = 2) +
  facet_wrap(vars(Quantity), ncol = 2, scales = "free_y") +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  scale_color_manual(values = c(
    "Lag-preserving two-stage" = "#0072B2",
    "Published two-stage (5.8.1)" = "#009E73",
    "Native 5.23 routine" = "#D55E00"
  )) +
  labs(
    x = "Terminal year",
    y = "Difference from the full Model 26.0 fit",
    color = NULL,
    shape = NULL
  ) +
  ggthemes::theme_few() +
  theme(legend.position = "bottom")
ggsave(
  file.path(output_directory, "model_26_0_retrospective_sensitivity.png"),
  sensitivity_figure,
  width = 10,
  height = 8,
  dpi = 180
)

output <- list(
  Rceattle_list = primary_models,
  peel_details = primary_details,
  mohns = primary_mohns,
  specification = list(
    Rceattle_version = as.character(packageVersion("Rceattle")),
    peels = peels,
    data_lags_preserved = TRUE,
    terminal_fishery_cpue_increment = "fixed at zero",
    terminal_fishery_selectivity_equal_previous_year = TRUE,
    empirical_start_recalculated_by_peel = TRUE,
    two_stage_fit_by_peel = TRUE,
    dsem = NULL,
    native_routine_role = "implementation sensitivity"
  )
)
saveRDS(
  output,
  file.path(output_directory, "model_26_0_retrospective_primary_9.rds")
)

capture.output(
  sessionInfo(),
  file = file.path(output_directory, "session_info.txt")
)

print(mohn_sensitivity, row.names = FALSE)
print(convergence, row.names = FALSE)
