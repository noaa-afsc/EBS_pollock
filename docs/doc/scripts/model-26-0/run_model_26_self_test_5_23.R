#!/usr/bin/env Rscript

# Observation-level self-test for Model 26.0 using Rceattle 5.23.0.

rceattle_library <- Sys.getenv(
  "RCEATTLE_LIB",
  "/tmp/rceattle-dsem-v5-lib-01a05b46"
)
.libPaths(c(rceattle_library, .libPaths()))

suppressPackageStartupMessages({
  library(Rceattle)
  library(dplyr)
  library(ggplot2)
})

expected_commit <- "adf22f84b399e84e3b9a707e0b3bbb1624179a27"
stopifnot(packageVersion("Rceattle") == package_version("5.23.0"))

project_root <- normalizePath(getwd(), mustWork = TRUE)
output_directory <- Sys.getenv(
  "MODEL26_SELF_TEST_OUTPUT_DIR",
  file.path(project_root, "results", "model_26_0_self_test_5.23.0")
)
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

base_fit <- readRDS(file.path(
  project_root,
  "results",
  "model_26_0_rceattle_5.23.0",
  "model_26_0_fit.rds"
))$model_26

self_test_seed <- 20260831L
self_tests <- Rceattle::self_test(
  object = base_fit,
  nsim = 50L,
  simulate = TRUE,
  seed = self_test_seed,
  cores = 4L,
  getsd = FALSE,
  phase = TRUE,
  start = "initial",
  timeout = 3600,
  process = FALSE
)

years <- base_fit$data_list$styr:base_fit$data_list$endyr
nyears <- length(years)
operating_values <- list(
  `Spawning biomass` = as.numeric(base_fit$quantities$ssb[1, seq_len(nyears)]),
  `Total biomass` = as.numeric(base_fit$quantities$biomass[1, seq_len(nyears)]),
  `Age-1 recruitment` = as.numeric(base_fit$quantities$R[1, seq_len(nyears)])
)

recovery <- bind_rows(lapply(names(self_tests), function(simulation) {
  model <- self_tests[[simulation]]
  simulated_values <- list(
    `Spawning biomass` = as.numeric(model$quantities$ssb[1, seq_len(nyears)]),
    `Total biomass` = as.numeric(model$quantities$biomass[1, seq_len(nyears)]),
    `Age-1 recruitment` = as.numeric(model$quantities$R[1, seq_len(nyears)])
  )
  bind_rows(lapply(names(operating_values), function(quantity) {
    truth <- operating_values[[quantity]]
    estimate <- simulated_values[[quantity]]
    data.frame(
      Simulation = simulation,
      Year = years,
      Quantity = quantity,
      Operating_value = truth,
      Refit_value = estimate,
      Percent_difference = 100 * (estimate - truth) / truth
    )
  }))
}))
write.csv(
  recovery,
  file.path(output_directory, "model_26_0_self_test_recovery.csv"),
  row.names = FALSE
)

recovery_summary <- recovery |>
  group_by(Quantity) |>
  summarise(
    Median_percent_difference = median(Percent_difference, na.rm = TRUE),
    Mean_absolute_percent_difference = mean(abs(Percent_difference), na.rm = TRUE),
    Lower_90_percent_difference = quantile(Percent_difference, 0.05, na.rm = TRUE),
    Upper_90_percent_difference = quantile(Percent_difference, 0.95, na.rm = TRUE),
    .groups = "drop"
  )
write.csv(
  recovery_summary,
  file.path(output_directory, "model_26_0_self_test_recovery_summary.csv"),
  row.names = FALSE
)

terminal_recovery <- recovery |>
  filter(Year == max(years)) |>
  group_by(Quantity) |>
  summarise(
    Median_terminal_percent_difference = median(Percent_difference, na.rm = TRUE),
    Lower_terminal_90_percent_difference = quantile(Percent_difference, 0.05, na.rm = TRUE),
    Upper_terminal_90_percent_difference = quantile(Percent_difference, 0.95, na.rm = TRUE),
    .groups = "drop"
  )
write.csv(
  terminal_recovery,
  file.path(output_directory, "model_26_0_self_test_terminal_summary.csv"),
  row.names = FALSE
)

gradient_values <- vapply(
  self_tests,
  function(model) model$opt$max_gradient,
  numeric(1)
)
self_test_summary <- data.frame(
  Requested_simulations = 50L,
  Converged_refits = length(self_tests),
  Dropped_refits = 50L - length(self_tests),
  Maximum_refit_gradient = max(gradient_values, na.rm = TRUE),
  Median_refit_gradient = median(gradient_values, na.rm = TRUE),
  Simulation_seed = self_test_seed,
  Starting_values = "Original pre-fit starting values",
  Simulated_process = "Observation error only; fitted recruitment deviations retained"
)
write.csv(
  self_test_summary,
  file.path(output_directory, "model_26_0_self_test_summary.csv"),
  row.names = FALSE
)

recovery_figure <- ggplot(
  recovery,
  aes(x = Quantity, y = Percent_difference, fill = Quantity)
) +
  geom_hline(yintercept = 0, color = "grey40", linetype = "dashed") +
  geom_boxplot(outlier.alpha = 0.12, width = 0.65) +
  coord_flip() +
  scale_fill_manual(values = c("#0072B2", "#D55E00", "#009E73")) +
  labs(
    x = NULL,
    y = "Percent difference from the operating-model value"
  ) +
  ggthemes::theme_few() +
  theme(legend.position = "none")
ggsave(
  file.path(output_directory, "model_26_0_self_test_recovery.png"),
  recovery_figure,
  width = 8.5,
  height = 5.2,
  dpi = 180
)

capture.output(
  sessionInfo(),
  file = file.path(output_directory, "model_26_0_self_test_session_info.txt")
)

print(self_test_summary, row.names = FALSE)
print(recovery_summary, row.names = FALSE)
print(terminal_recovery, row.names = FALSE)
