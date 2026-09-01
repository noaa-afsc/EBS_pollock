#!/usr/bin/env Rscript

# Native Rceattle projections and nine-peel retrospective for Model 26.0.

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

expected_commit <- "adf22f84b399e84e3b9a707e0b3bbb1624179a27"
stopifnot(packageVersion("Rceattle") == package_version("5.23.0"))

project_root <- normalizePath(getwd(), mustWork = TRUE)
output_directory <- Sys.getenv(
  "MODEL26_PROJECTION_RETRO_OUTPUT_DIR",
  file.path(project_root, "results", "model_26_0_projection_retro_5.23.0")
)
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

base_result <- readRDS(file.path(
  project_root,
  "results",
  "model_26_0_rceattle_5.23.0",
  "model_26_0_fit.rds"
))
base_fit <- base_result$model_26
est <- base_result$data
stopifnot(!any(grepl("2DAR1", est$fleet_control$Selectivity, fixed = TRUE)))
est$fleet_control$Proj_F_proportion <- 0
est$fleet_control$Proj_F_proportion[
  est$fleet_control$Fleet_name == "Fishery"
] <- 1
stopifnot(sum(est$fleet_control$Proj_F_proportion) == 1)

# Project the saved hindcast parameters under the native NPFMC Tier 3 rule.
# Ftarget and Flimit identify F40% and F35% in this Rceattle HCR interface.
projection_checkpoint <- file.path(output_directory, "model_26_0_projection.rds")
if (file.exists(projection_checkpoint)) {
  projection_fit <- readRDS(projection_checkpoint)
} else {
  projection_fit <- Rceattle::fit_mod(
    data_list = est,
    inits = base_fit$estimated_params,
    file = NULL,
    estimateMode = "Projection",
    random_rec = FALSE,
    msmMode = 0,
    initMode = "NonEquilibrium",
    HCR = Rceattle::build_hcr(
      HCR = "NPFMC",
      DynamicHCR = FALSE,
      Ftarget = 0.40,
      Flimit = 0.35,
      Alpha = 0.05
    ),
    M1Fun = Rceattle::build_M1(updateM1 = TRUE, M1_model = "fixed"),
    fit_control = Rceattle::fit_control(
      verbose = 1,
      phase = FALSE,
      getsd = FALSE,
      bias_adjust_proc = FALSE,
      bias_adjust_obs = FALSE,
      comp_offset = 1e-3
    )
  )
  saveRDS(projection_fit, projection_checkpoint)
}

all_years <- est$styr:est$projyr
projection_years <- (est$endyr + 1L):est$projyr
projection_index <- match(projection_years, all_years)
q <- projection_fit$quantities
q0 <- base_fit$quantities

reference_value <- function(x, index = projection_index) {
  values <- as.numeric(x)
  if (length(values) == 1L) rep(values, length(index)) else values[index]
}

projection_summary <- data.frame(
  Year = projection_years,
  SSB = as.numeric(q$ssb[1, projection_index]),
  Total_biomass = as.numeric(q$biomass[1, projection_index]),
  Catch = as.numeric(q$catch_hat[projection_index]),
  Fishing_mortality = as.numeric(q$proj_F[1, projection_index]),
  Age_1_recruitment = as.numeric(q$R[1, projection_index]),
  SSB_F40 = reference_value(q$SBF),
  SSB_no_fishing = as.numeric(q0$ssb[1, projection_index])
)
projection_summary$SSB_relative_to_F40 <-
  projection_summary$SSB / projection_summary$SSB_F40
write.csv(
  projection_summary,
  file.path(output_directory, "model_26_0_projection_summary.csv"),
  row.names = FALSE
)

write.csv(
  data.frame(
    Item = c(
      "Rceattle branch",
      "Rceattle commit",
      "Rceattle version",
      "Projection method",
      "Projection years",
      "Target fishing rate",
      "Limit fishing rate",
      "Lower stock threshold",
      "Recruitment",
      "Use in this document"
    ),
    Setting = c(
      "dsem-v5-integration",
      expected_commit,
      as.character(packageVersion("Rceattle")),
      "Native Rceattle NPFMC Tier 3 harvest control rule",
      paste0(min(projection_years), "-", max(projection_years)),
      "F40%",
      "F35%",
      "5% of the target spawning-biomass reference",
      "Model projection rule from the saved Model 26.0 configuration",
      "Development check; management advice continues to use the accepted assessment"
    )
  ),
  file.path(output_directory, "model_26_0_projection_configuration.csv"),
  row.names = FALSE
)

projection_plot_data <- bind_rows(
  projection_summary |>
    select(Year, Value = SSB) |>
    mutate(Quantity = "Spawning biomass", Scenario = "NPFMC Tier 3"),
  projection_summary |>
    select(Year, Value = SSB_no_fishing) |>
    mutate(Quantity = "Spawning biomass", Scenario = "No fishing"),
  projection_summary |>
    select(Year, Value = Catch) |>
    mutate(Quantity = "Catch", Scenario = "NPFMC Tier 3")
)
projection_figure <- ggplot(
  projection_plot_data,
  aes(x = Year, y = Value, color = Scenario, linetype = Scenario)
) +
  geom_line(linewidth = 0.9) +
  facet_wrap(vars(Quantity), ncol = 1, scales = "free_y") +
  scale_color_manual(values = c("NPFMC Tier 3" = "#D55E00", "No fishing" = "#0072B2")) +
  scale_linetype_manual(values = c("NPFMC Tier 3" = "solid", "No fishing" = "longdash")) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(x = "Projection year", y = "Thousand metric tons", color = NULL, linetype = NULL) +
  ggthemes::theme_few() +
  theme(legend.position = "bottom")
ggsave(
  file.path(output_directory, "model_26_0_projection.png"),
  projection_figure,
  width = 9,
  height = 7,
  dpi = 180
)

# Nine phased peels use the current branch's retrospective routine. It retains
# stream-specific observation lags and fixes peeled time-varying parameters at
# the peel terminal year before fitting the catch-only forecast extension.
retro_checkpoint <- file.path(output_directory, "model_26_0_retrospective_9.rds")
if (file.exists(retro_checkpoint)) {
  retro <- readRDS(retro_checkpoint)
} else {
  retro <- Rceattle::retrospective(
    object = base_fit,
    peels = 9L,
    nyrs_forecast = 3L,
    cores = 4L,
    getsd = FALSE,
    phase = TRUE,
    forecast_rec = "mean"
  )
  saveRDS(retro, retro_checkpoint)
}

retro_models <- retro$Rceattle_list
retro_convergence <- bind_rows(lapply(names(retro_models), function(model_name) {
  model <- retro_models[[model_name]]
  data.frame(
    Model = model_name,
    Terminal_year = model$data_list$endyr_peel,
    Convergence_status = model$convergence$status,
    Maximum_gradient = model$opt$max_gradient
  )
}))
write.csv(
  retro_convergence,
  file.path(output_directory, "model_26_0_retrospective_convergence.csv"),
  row.names = FALSE
)

rho_frame <- as.data.frame(retro$mohns)
rho_frame$Species <- rownames(rho_frame)
rownames(rho_frame) <- NULL
rho_frame <- relocate(rho_frame, Species)
write.csv(
  rho_frame,
  file.path(output_directory, "model_26_0_mohns_rho.csv"),
  row.names = FALSE
)

retro_trajectories <- bind_rows(lapply(names(retro_models), function(model_name) {
  model <- retro_models[[model_name]]
  terminal_year <- model$data_list$endyr_peel
  model_years <- est$styr:terminal_year
  n <- length(model_years)
  bind_rows(
    data.frame(
      Model = model_name,
      Terminal_year = terminal_year,
      Year = model_years,
      Quantity = "Spawning biomass",
      Value = as.numeric(model$quantities$ssb[1, seq_len(n)])
    ),
    data.frame(
      Model = model_name,
      Terminal_year = terminal_year,
      Year = model_years,
      Quantity = "Total biomass",
      Value = as.numeric(model$quantities$biomass[1, seq_len(n)])
    ),
    data.frame(
      Model = model_name,
      Terminal_year = terminal_year,
      Year = model_years,
      Quantity = "Age-1 recruitment",
      Value = as.numeric(model$quantities$R[1, seq_len(n)])
    ),
    data.frame(
      Model = model_name,
      Terminal_year = terminal_year,
      Year = model_years,
      Quantity = "Fishing mortality",
      Value = as.numeric(model$quantities$F_spp[1, seq_len(n)])
    )
  )
}))
write.csv(
  retro_trajectories,
  file.path(output_directory, "model_26_0_retrospective_trajectories.csv"),
  row.names = FALSE
)

base_terminal <- max(retro_trajectories$Terminal_year)
retro_figure <- ggplot(
  retro_trajectories,
  aes(
    x = Year,
    y = Value,
    group = factor(Terminal_year),
    color = factor(Terminal_year),
    linewidth = Terminal_year == base_terminal
  )
) +
  geom_line(alpha = 0.9) +
  facet_wrap(vars(Quantity), ncol = 2, scales = "free_y") +
  scale_linewidth_manual(values = c(`FALSE` = 0.55, `TRUE` = 1.15), guide = "none") +
  scale_color_viridis_d(option = "C", direction = -1) +
  scale_y_log10(labels = scales::label_comma()) +
  labs(x = "Year", y = NULL, color = "Terminal year") +
  ggthemes::theme_few() +
  theme(legend.position = "bottom")
ggsave(
  file.path(output_directory, "model_26_0_retrospective.png"),
  retro_figure,
  width = 10,
  height = 8,
  dpi = 180
)

capture.output(
  sessionInfo(),
  file = file.path(output_directory, "model_26_0_projection_retro_session_info.txt")
)

print(head(projection_summary, 10), row.names = FALSE)
print(retro_convergence, row.names = FALSE)
print(rho_frame, row.names = FALSE)
