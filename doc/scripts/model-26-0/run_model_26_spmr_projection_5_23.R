#!/usr/bin/env Rscript

# Standard Projection Model projections from the fitted Model 26.0 state.

rceattle_library <- Sys.getenv(
  "RCEATTLE_LIB",
  "/tmp/rceattle-dsem-v5-lib-01a05b46"
)
.libPaths(c(
  rceattle_library,
  file.path(getwd(), ".r-lib-pm"),
  .libPaths()
))

suppressPackageStartupMessages({
  library(Rceattle)
  library(spmR)
  library(dplyr)
  library(ggplot2)
  library(readr)
})

stopifnot(
  packageVersion("Rceattle") == package_version("5.23.0"),
  packageVersion("spmR") == package_version("0.3.0")
)

project_root <- normalizePath(
  Sys.getenv("MODEL26_PROJECT_ROOT", getwd()),
  mustWork = TRUE
)
source(file.path(project_root, "R", "write_spmr_projection_inputs.R"))
model_file <- file.path(
  project_root,
  "results",
  "model_26_0_rceattle_5.23.0",
  "model_26_0_fit.rds"
)
output_root <- Sys.getenv(
  "MODEL26_SPMR_OUTPUT_DIR",
  file.path(project_root, "results", "model_26_0_spmR_5.23.0")
)
main_directory <- file.path(output_root, "spmR_projection")
alt2_directory <- file.path(output_root, "spmR_projection_alt2_fixed1300")
runtime_directory <- Sys.getenv(
  "SPMR_RUNTIME_DIR",
  file.path(project_root, "results", "canonical_pm", "spmR_projection")
)

main <- write_spmr_projection_inputs(
  method_file = model_file,
  fit_name = "model_26",
  output_dir = main_directory,
  alt_list = 1:7,
  fixed_catches = c(1350, 1350),
  nproj_years = 14L,
  nsims = 1000L,
  run_name = "model_26_0_spmR",
  n_selectivity_years = 5L,
  template_dir = runtime_directory
)

detail <- spmR::runSPM(main$output_dir, run = TRUE, engine = "admb")
readr::write_csv(detail, file.path(main$output_dir, "spm_detail.csv"))

projection_summary <- detail |>
  group_by(Alt, Year) |>
  summarise(
    SSB_mean = mean(SSB),
    SSB_median = median(SSB),
    SSB_lower_90 = quantile(SSB, 0.05),
    SSB_upper_90 = quantile(SSB, 0.95),
    Catch_mean = mean(Catch),
    Catch_median = median(Catch),
    Catch_lower_90 = quantile(Catch, 0.05),
    Catch_upper_90 = quantile(Catch, 0.95),
    ABC_mean = mean(ABC),
    OFL_mean = mean(OFL),
    F_mean = mean(F),
    B35_mean = mean(B35),
    B_relative_to_B35 = mean(SSB / B35),
    .groups = "drop"
  )
readr::write_csv(
  projection_summary,
  file.path(main$output_dir, "model_26_0_spmr_projection_summary.csv")
)

projection_years <- sort(unique(detail$Year))
comparison_years <- head(
  projection_years[projection_years > max(main$fixed_catch_years)],
  2L
)
tier3 <- spmR::tier3_scenario_table(
  detail,
  years = comparison_years,
  digits = 2
)
readr::write_csv(
  tier3,
  file.path(main$output_dir, "tier3_seven_scenario_table.csv")
)

alternative_names <- c(
  `1` = "Maximum permissible ABC",
  `2` = "Author-specified ABC",
  `3` = "Average recent F",
  `4` = "Alternative SPR rate",
  `5` = "No fishing",
  `6` = "OFL threshold determination",
  `7` = "Status-determination ramp"
)
plot_summary <- projection_summary |>
  mutate(
    Scenario = factor(
      paste0("Alt ", Alt, ": ", alternative_names[as.character(Alt)]),
      levels = paste0(
        "Alt ",
        seq_along(alternative_names),
        ": ",
        alternative_names
      )
    )
  )

stock_status_figure <- ggplot(plot_summary, aes(Year, SSB_median)) +
  geom_ribbon(
    aes(ymin = SSB_lower_90, ymax = SSB_upper_90),
    fill = "#2b7a9b",
    alpha = 0.22
  ) +
  geom_line(color = "#102f3d", linewidth = 0.85) +
  geom_line(aes(y = B35_mean), color = "#4d4d4d", linetype = "dashed") +
  facet_wrap(vars(Scenario), ncol = 2) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(x = "Year", y = "Spawning biomass (thousand t)") +
  ggthemes::theme_few()
ggsave(
  file.path(output_root, "model_26_0_spmr_stock_status.png"),
  stock_status_figure,
  width = 10,
  height = 9,
  dpi = 180
)

removal_data <- plot_summary |>
  select(
    Scenario,
    Year,
    `Projected catch` = Catch_mean,
    `Maximum permissible ABC` = ABC_mean,
    OFL = OFL_mean
  ) |>
  tidyr::pivot_longer(
    cols = c(`Projected catch`, `Maximum permissible ABC`, OFL),
    names_to = "Quantity",
    values_to = "Value"
  )
removals_figure <- ggplot(
  removal_data,
  aes(Year, Value, color = Quantity, linetype = Quantity)
) +
  geom_line(linewidth = 0.8) +
  facet_wrap(vars(Scenario), ncol = 2) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(x = "Year", y = "Thousand t", color = NULL, linetype = NULL) +
  ggthemes::theme_few() +
  theme(legend.position = "bottom")
ggsave(
  file.path(output_root, "model_26_0_spmr_removals.png"),
  removals_figure,
  width = 10,
  height = 9,
  dpi = 180
)

fixed_catches <- stats::setNames(rep(1300, length(2025:2032)), 2025:2032)
alt2 <- write_spmr_projection_inputs(
  method_file = model_file,
  fit_name = "model_26",
  output_dir = alt2_directory,
  alt_list = 2L,
  fixed_catches = fixed_catches,
  nproj_years = length(fixed_catches),
  nsims = 1000L,
  run_name = "model_26_0_alt2_fixed1300",
  n_selectivity_years = 5L,
  template_dir = runtime_directory
)
alt2_detail <- spmR::runSPM(alt2$output_dir, run = TRUE, engine = "admb")
readr::write_csv(
  alt2_detail,
  file.path(alt2$output_dir, "spm_detail.csv")
)
alt2_summary <- alt2_detail |>
  group_by(Year) |>
  summarise(
    Catch = mean(Catch),
    ABC = mean(ABC),
    OFL = mean(OFL),
    SSB = mean(SSB),
    F = mean(F),
    B35 = mean(B35),
    B_relative_to_B35 = mean(SSB / B35),
    .groups = "drop"
  )
readr::write_csv(
  alt2_summary,
  file.path(alt2$output_dir, "model_26_0_spmr_alt2_fixed1300_summary.csv")
)

capture.output(
  sessionInfo(),
  file = file.path(output_root, "session_info.txt")
)

print(tier3, n = Inf, width = Inf)
