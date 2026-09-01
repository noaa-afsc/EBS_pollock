#!/usr/bin/env Rscript

# Controlled data-sensitivity evaluation for the lag-preserving Model 26.0
# retrospective under Rceattle 5.23.0.
#
# The 2020-to-2021 peel transition is evaluated because it contains a marked
# revision in the 2012--2014 cohorts and spawning biomass. Every refit uses the
# data dimensions, parameter map, and converged starting values from the
# lag-preserving 2021 peel. DSEM remains disabled. The tests therefore change
# observation availability while retaining the 5.23.0 assessment structure.

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
retrospective_file <- file.path(
  project_root,
  "results",
  "model_26_0_retrospective_sensitivity_5.23.0",
  "model_26_0_retrospective_primary_9.rds"
)
base_fit_file <- file.path(
  project_root,
  "results",
  "model_26_0_rceattle_5.23.0",
  "model_26_0_fit.rds"
)
output_directory <- Sys.getenv(
  "MODEL26_RETRO_DATA_SENSITIVITY_OUTPUT_DIR",
  file.path(
    project_root,
    "results",
    "model_26_0_retrospective_data_sensitivity_5.23.0"
  )
)
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

retrospective <- readRDS(retrospective_file)
base_result <- readRDS(base_fit_file)
stopifnot(
  identical(retrospective$specification$Rceattle_version, "5.23.0"),
  isTRUE(retrospective$specification$data_lags_preserved),
  is.null(retrospective$specification$dsem),
  identical(base_result$Rceattle_commit,
            "adf22f84b399e84e3b9a707e0b3bbb1624179a27")
)

models <- retrospective$Rceattle_list
model_2020 <- models$Year_2020
model_2021 <- models$Year_2021
cohorts <- 2012:2014

control <- Rceattle::fit_control(
  verbose = 0,
  phase = TRUE,
  bias_adjust_proc = FALSE,
  bias_adjust_obs = FALSE,
  comp_offset = 1e-3
)
m1_function <- Rceattle::build_M1(updateM1 = TRUE, M1_model = "fixed")

fit_variant <- function(data_list, scenario) {
  checkpoint <- file.path(
    output_directory,
    paste0("fit_", gsub("[^a-z0-9]+", "_", tolower(scenario)), ".rds")
  )
  if (file.exists(checkpoint)) return(readRDS(checkpoint))

  message("Fitting ", scenario)
  fit <- suppressWarnings(Rceattle::fit_mod(
    data_list = data_list,
    inits = model_2021$obj$env$parList(),
    map = model_2021$map,
    file = NULL,
    estimateMode = 0,
    random_rec = FALSE,
    dsem = NULL,
    msmMode = 0,
    initMode = "NonEquilibrium",
    M1Fun = m1_function,
    fit_control = control
  ))
  saveRDS(fit, checkpoint)
  fit
}

extract_state <- function(scenario, fit, experiment) {
  years <- as.integer(colnames(fit$quantities$R))
  reference_years <- as.integer(colnames(model_2021$quantities$R))
  values <- data.frame(
    Experiment = experiment,
    Scenario = scenario,
    Convergence_status = fit$convergence$status,
    Maximum_gradient = fit$opt$max_gradient,
    Positive_definite_Hessian = isTRUE(fit$sdrep$pdHess),
    Objective = fit$opt$objective,
    R2012 = fit$quantities$R[1, match(2012, years)],
    R2013 = fit$quantities$R[1, match(2013, years)],
    R2014 = fit$quantities$R[1, match(2014, years)],
    SSB2015 = fit$quantities$ssb[1, match(2015, years)],
    SSB2018 = fit$quantities$ssb[1, match(2018, years)],
    SSB2020 = fit$quantities$ssb[1, match(2020, years)]
  )
  for (quantity in c(
    "R2012", "R2013", "R2014", "SSB2015", "SSB2018", "SSB2020"
  )) {
    reference_value <- switch(
      quantity,
      R2012 = model_2021$quantities$R[1, match(2012, reference_years)],
      R2013 = model_2021$quantities$R[1, match(2013, reference_years)],
      R2014 = model_2021$quantities$R[1, match(2014, reference_years)],
      SSB2015 = model_2021$quantities$ssb[1, match(2015, reference_years)],
      SSB2018 = model_2021$quantities$ssb[1, match(2018, reference_years)],
      SSB2020 = model_2021$quantities$ssb[1, match(2020, reference_years)]
    )
    values[[paste0(quantity, "_percent_vs_all_2021")]] <-
      100 * (values[[quantity]] / reference_value - 1)
  }
  values
}

new_rows <- function(old, new, source) {
  key_old <- paste(old$Fleet_name, old$Year)
  added <- new |>
    filter(!paste(.data$Fleet_name, .data$Year) %in% key_old)
  if (source == "Index") {
    return(added |>
      transmute(
        Source = source,
        Fleet = .data$Fleet_name,
        Year = .data$Year,
        Value = .data$Observation,
        Value_definition = "Observation"
      ))
  }
  added |>
    transmute(
      Source = source,
      Fleet = .data$Fleet_name,
      Year = .data$Year,
      Value = .data$Sample_size,
      Value_definition = "Sample size"
    )
}

new_data_inventory <- bind_rows(
  new_rows(model_2020$data_list$index_data,
           model_2021$data_list$index_data, "Index"),
  new_rows(model_2020$data_list$comp_data,
           model_2021$data_list$comp_data, "Composition")
) |>
  arrange(.data$Source, .data$Fleet, .data$Year)
write.csv(
  new_data_inventory,
  file.path(output_directory, "new_data_inventory.csv"),
  row.names = FALSE
)

expected_inventory <- data.frame(
  Source = c("Composition", "Composition", "Index", "Index", "Index"),
  Fleet = c("BTS", "Fishery", "AVO", "BTS", "BTS_1"),
  Year = c(2021L, 2020L, 2021L, 2021L, 2021L)
) |>
  arrange(.data$Source, .data$Fleet, .data$Year)
observed_inventory <- new_data_inventory |>
  transmute(
    Source = as.character(.data$Source),
    Fleet = as.character(.data$Fleet),
    Year = as.integer(.data$Year)
  )
stopifnot(
  nrow(observed_inventory) == nrow(expected_inventory),
  nrow(anti_join(observed_inventory, expected_inventory,
                 by = c("Source", "Fleet", "Year"))) == 0L,
  nrow(anti_join(expected_inventory, observed_inventory,
                 by = c("Source", "Fleet", "Year"))) == 0L
)

remove_block <- list(
  no_Fishery_comp_2020 = function(data) {
    data$comp_data <- filter(
      data$comp_data,
      !(.data$Fleet_name == "Fishery" & .data$Year == 2020)
    )
    data
  },
  no_BTS_index_2021 = function(data) {
    data$index_data <- filter(
      data$index_data,
      !(.data$Fleet_name == "BTS" & .data$Year == 2021)
    )
    data
  },
  no_BTS_comp_2021 = function(data) {
    data$comp_data <- filter(
      data$comp_data,
      !(.data$Fleet_name == "BTS" & .data$Year == 2021)
    )
    data
  },
  no_AVO_index_2021 = function(data) {
    data$index_data <- filter(
      data$index_data,
      !(.data$Fleet_name == "AVO" & .data$Year == 2021)
    )
    data
  },
  no_BTS1_index_2021 = function(data) {
    data$index_data <- filter(
      data$index_data,
      !(.data$Fleet_name == "BTS_1" & .data$Year == 2021)
    )
    data
  }
)

block_results <- list(extract_state(
  "All 2021-peel data", model_2021, "Block deletion"
))
for (scenario in names(remove_block)) {
  fit <- fit_variant(remove_block[[scenario]](model_2021$data_list), scenario)
  block_results[[length(block_results) + 1L]] <- extract_state(
    scenario, fit, "Block deletion"
  )
}
block_results <- bind_rows(block_results)
write.csv(
  block_results,
  file.path(output_directory, "block_deletion_results.csv"),
  row.names = FALSE
)

add_rows <- function(data, source_data, source, fleet, year) {
  if (source == "Index") {
    data$index_data <- bind_rows(
      data$index_data,
      filter(
        source_data$index_data,
        .data$Fleet_name == fleet,
        .data$Year == year
      )
    ) |>
      arrange(.data$Fleet_code, .data$Year)
  } else {
    data$comp_data <- bind_rows(
      data$comp_data,
      filter(
        source_data$comp_data,
        .data$Fleet_name == fleet,
        .data$Year == year
      )
    ) |>
      arrange(.data$Fleet_code, .data$Year)
  }
  data
}

# Keep the 2021 state and process dimensions while restoring the observation
# availability of the lag-preserving 2020 peel. Add all five entering blocks in
# a stated order; the final step must reproduce the complete 2021 data set.
availability_2020 <- model_2021$data_list
for (scenario in names(remove_block)) {
  availability_2020 <- remove_block[[scenario]](availability_2020)
}
cumulative_data <- list("2020 observation availability" = availability_2020)
working <- availability_2020
working <- add_rows(working, model_2021$data_list,
                    "Composition", "Fishery", 2020)
cumulative_data[["Add 2020 fishery composition"]] <- working
working <- add_rows(working, model_2021$data_list, "Index", "BTS", 2021)
cumulative_data[["Add 2021 BTS biomass index"]] <- working
working <- add_rows(working, model_2021$data_list,
                    "Composition", "BTS", 2021)
cumulative_data[["Add 2021 BTS age composition"]] <- working
working <- add_rows(working, model_2021$data_list, "Index", "AVO", 2021)
cumulative_data[["Add 2021 AVO index"]] <- working
working <- add_rows(working, model_2021$data_list, "Index", "BTS_1", 2021)
cumulative_data[["Add 2021 BTS age-1 index"]] <- working

cumulative_results <- list()
for (scenario in names(cumulative_data)) {
  fit <- fit_variant(cumulative_data[[scenario]], paste0("cumulative_", scenario))
  cumulative_results[[length(cumulative_results) + 1L]] <- extract_state(
    scenario, fit, "Cumulative addition"
  )
}
cumulative_results <- bind_rows(cumulative_results) |>
  mutate(Step = row_number(), .before = 1)
write.csv(
  cumulative_results,
  file.path(output_directory, "cumulative_addition_results.csv"),
  row.names = FALSE
)

scenario_labels <- c(
  no_Fishery_comp_2020 = "Remove 2020 fishery age composition",
  no_BTS_index_2021 = "Remove 2021 BTS biomass index",
  no_BTS_comp_2021 = "Remove 2021 BTS age composition",
  no_AVO_index_2021 = "Remove 2021 AVO index",
  no_BTS1_index_2021 = "Remove 2021 BTS age-1 index"
)
block_plot_data <- block_results |>
  filter(.data$Scenario != "All 2021-peel data") |>
  select(
    Scenario,
    `2013 cohort` = R2013_percent_vs_all_2021,
    `2014 cohort` = R2014_percent_vs_all_2021,
    `2018 spawning biomass` = SSB2018_percent_vs_all_2021,
    `2020 spawning biomass` = SSB2020_percent_vs_all_2021
  ) |>
  pivot_longer(
    -.data$Scenario,
    names_to = "Quantity",
    values_to = "Percent_change"
  ) |>
  mutate(
    Scenario = recode(.data$Scenario, !!!scenario_labels),
    Scenario = factor(.data$Scenario, levels = rev(unname(scenario_labels)))
  )

data_sensitivity_figure <- ggplot(
  block_plot_data,
  aes(.data$Percent_change, .data$Scenario, fill = .data$Quantity)
) +
  geom_col(position = position_dodge(width = 0.82), width = 0.72) +
  geom_vline(xintercept = 0, color = "grey40", linewidth = 0.45) +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  scale_x_continuous(labels = scales::label_number(suffix = "%")) +
  labs(
    x = "Change from the complete 2021 peel",
    y = NULL,
    fill = "Estimate"
  ) +
  ggthemes::theme_few() +
  theme(legend.position = "bottom")
ggsave(
  file.path(output_directory, "retrospective_data_sensitivity.png"),
  data_sensitivity_figure,
  width = 10,
  height = 5.8,
  dpi = 200
)

all_converged <- all(
  c(block_results$Convergence_status, cumulative_results$Convergence_status) ==
    "OK"
)
all_pd_hessian <- all(c(
  block_results$Positive_definite_Hessian,
  cumulative_results$Positive_definite_Hessian
))
final_cumulative <- cumulative_results[nrow(cumulative_results), ]
final_reproduction <- data.frame(
  Objective_difference = final_cumulative$Objective - model_2021$opt$objective,
  R2012_percent_difference = final_cumulative$R2012_percent_vs_all_2021,
  R2013_percent_difference = final_cumulative$R2013_percent_vs_all_2021,
  R2014_percent_difference = final_cumulative$R2014_percent_vs_all_2021,
  SSB2020_percent_difference = final_cumulative$SSB2020_percent_vs_all_2021
)
write.csv(
  final_reproduction,
  file.path(output_directory, "final_cumulative_reproduction_check.csv"),
  row.names = FALSE
)
stopifnot(
  all_converged,
  all_pd_hessian,
  abs(final_reproduction$Objective_difference) < 1e-4,
  max(abs(final_reproduction[, -1])) < 1e-4
)

writeLines(
  c(
    paste("Generated:", format(Sys.time(), tz = "UTC", usetz = TRUE)),
    paste("Rceattle version:", as.character(packageVersion("Rceattle"))),
    paste("Rceattle source commit:", base_result$Rceattle_commit),
    paste("Source retrospective:", retrospective_file),
    "Retrospective configuration: stream-specific terminal lags preserved; terminal fishery and CPUE selectivity increments fixed at zero; two-stage peel fits.",
    "Controlled refits: converged 2021-peel start and parameter map; DSEM disabled.",
    "Block deletion: one of five observations entering between the 2020 and 2021 peels removed at a time.",
    "Cumulative addition order: 2020 fishery age composition, 2021 BTS biomass index, 2021 BTS age composition, 2021 AVO index, 2021 BTS age-1 index."
  ),
  file.path(output_directory, "lineage.txt")
)
capture.output(
  sessionInfo(),
  file = file.path(output_directory, "session_info.txt")
)

print(block_results, row.names = FALSE)
print(cumulative_results, row.names = FALSE)
