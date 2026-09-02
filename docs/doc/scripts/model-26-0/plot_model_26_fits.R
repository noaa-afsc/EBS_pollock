#!/usr/bin/env Rscript

# Model 26.0 fitted index and age-composition displays.
#
# This script reads the saved Rceattle 5.23.0 fit. It does not refit the model.
# The figures follow the assessment convention of showing observations as
# bars or points and fitted values as lines and points.

rceattle_library <- Sys.getenv(
  "RCEATTLE_LIB",
  "/tmp/rceattle-dsem-v5-lib-01a05b46"
)
.libPaths(c(rceattle_library, .libPaths()))

suppressPackageStartupMessages({
  library(Rceattle)
  library(dplyr)
  library(ggplot2)
  library(ggthemes)
})

stopifnot(packageVersion("Rceattle") == package_version("5.23.0"))

script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_file <- if (length(script_argument)) {
  normalizePath(sub("^--file=", "", script_argument[[1]]), mustWork = TRUE)
} else {
  normalizePath("doc/scripts/model-26-0/plot_model_26_fits.R", mustWork = TRUE)
}

report_root <- Sys.getenv(
  "EBS_REPORT_ROOT",
  normalizePath(file.path(dirname(script_file), "..", "..", ".."), mustWork = TRUE)
)
analysis_root <- Sys.getenv(
  "EBS_ANALYSIS_ROOT",
  normalizePath(file.path(report_root, "..", "ebswp_rceattle"), mustWork = TRUE)
)

fit_file <- file.path(
  analysis_root,
  "results",
  "model_26_0_rceattle_5.23.0",
  "model_26_0_fit.rds"
)
asset_directory <- file.path(report_root, "doc", "assets", "model-26-0")
data_directory <- file.path(
  report_root,
  "doc",
  "data",
  "EBSpollock_Sept_2026",
  "model-26-0"
)
dir.create(asset_directory, recursive = TRUE, showWarnings = FALSE)
dir.create(data_directory, recursive = TRUE, showWarnings = FALSE)

saved_fit <- readRDS(fit_file)
stopifnot(
  identical(saved_fit$Rceattle_version, "5.23.0"),
  identical(saved_fit$Rceattle_commit, "adf22f84b399e84e3b9a707e0b3bbb1624179a27")
)
model_26 <- saved_fit$model_26

# Use the package's fitted-data builders so the displays follow the same row
# filtering and age-bin definitions as the fitted model.
index_data <- Rceattle::plot_index(model_26, error = TRUE)$data |>
  mutate(
    Fleet = recode(
      Fleet,
      AVO = "AVO index",
      BTS = "BTS biomass index",
      ATS = "ATS biomass index",
      ATS_1 = "ATS age-1 index",
      CPUE = "Historical CPUE"
    ),
    Fleet = factor(
      Fleet,
      levels = c(
        "Historical CPUE",
        "BTS biomass index",
        "ATS biomass index",
        "ATS age-1 index",
        "AVO index"
      )
    )
  )

index_summary <- index_data |>
  group_by(Fleet) |>
  summarise(
    Observations = n(),
    First_year = min(Year),
    Last_year = max(Year),
    Observed_fitted_correlation = cor(Observation, Predicted),
    Root_mean_squared_log_error = sqrt(mean((log(Observation) - log(Predicted))^2)),
    .groups = "drop"
  )

index_plot <- ggplot(index_data, aes(x = Year)) +
  geom_errorbar(
    aes(ymin = Lower95, ymax = Upper95),
    width = 0,
    linewidth = 0.35,
    colour = "grey55"
  ) +
  geom_point(
    aes(y = Observation, colour = "Observed"),
    size = 1.8,
    shape = 16
  ) +
  geom_line(
    aes(y = Predicted, colour = "Fitted"),
    linewidth = 0.7
  ) +
  facet_wrap(~Fleet, scales = "free_y", ncol = 2) +
  scale_colour_manual(
    name = NULL,
    values = c(Observed = "black", Fitted = "#0072B2")
  ) +
  labs(x = "Year", y = "Index value on its input scale") +
  ggthemes::theme_few(base_size = 10) +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(asset_directory, "model_26_0_index_fits.png"),
  index_plot,
  width = 8.5,
  height = 8.1,
  units = "in",
  dpi = 300,
  bg = "white"
)

# plot_comp() constructs the fitted composition rows from the active model.
# Suppress its intermediate printing while retaining the returned ggplot data.
grDevices::pdf(NULL)
composition_plots <- Rceattle::plot_comp(model_26)
grDevices::dev.off()

annual_names <- grep("^annual_", names(composition_plots), value = TRUE)
composition_data <- bind_rows(lapply(annual_names, function(plot_name) {
  composition_plots[[plot_name]]$data |>
    transmute(
      Fleet = Fleet_name,
      Year,
      Sample_size = N,
      Used_in_fit = N > 0,
      Age = bin,
      Cohort = Year - Age,
      Observed = obs,
      Fitted = hat,
      Difference = Observed - Fitted,
      Absolute_difference = abs(Difference)
  )
}))

# Rceattle's plotting helper returns likelihood-active rows. Add excluded
# placeholder rows explicitly so the annual display can show their supplied
# proportions while clearly identifying that they did not contribute to the
# fit. The current configuration has one such row: ATS 2020.
excluded_rows <- which(model_26$data_list$comp_data$Sample_size <= 0)
if (length(excluded_rows)) {
  excluded_composition <- bind_rows(lapply(excluded_rows, function(row) {
    fleet_name <- model_26$data_list$comp_data$Fleet_name[row]
    age_bins <- if (fleet_name == "ATS") {
      2:model_26$data_list$nages
    } else {
      seq_len(model_26$data_list$nages)
    }
    observed <- as.numeric(model_26$quantities$comp_obs[row, age_bins])
    fitted <- as.numeric(model_26$quantities$comp_hat[row, age_bins])
    data.frame(
      Fleet = fleet_name,
      Year = model_26$data_list$comp_data$Year[row],
      Sample_size = model_26$data_list$comp_data$Sample_size[row],
      Used_in_fit = FALSE,
      Age = age_bins,
      Cohort = model_26$data_list$comp_data$Year[row] - age_bins,
      Observed = observed,
      Fitted = fitted,
      Difference = observed - fitted,
      Absolute_difference = abs(observed - fitted)
    )
  }))
  composition_data <- bind_rows(composition_data, excluded_composition)
}

composition_summary <- composition_data |>
  filter(Used_in_fit) |>
  group_by(Fleet) |>
  summarise(
    Composition_years = n_distinct(Year),
    First_year = min(Year),
    Last_year = max(Year),
    Youngest_age = min(Age),
    Oldest_age = max(Age),
    Mean_absolute_difference = mean(Absolute_difference),
    Root_mean_squared_difference = sqrt(mean(Difference^2)),
    Root_mean_squared_difference_excluding_sample_size_1 =
      sqrt(mean(Difference[Sample_size > 1]^2)),
    Maximum_absolute_difference = max(Absolute_difference),
    .groups = "drop"
  )

cohort_palette <- c(
  "#E15759", "#4E79A7", "#59A14F", "#F28E2B", "#B07AA1",
  "#76B7B2", "#EDC948", "#9C755F", "#FF9DA7", "#A0CBE8",
  "#FFBE7D", "#8CD17D", "#B6992D", "#499894", "#D37295"
)

make_composition_plot <- function(data, years, ncol = 5L) {
  youngest_age <- min(data$Age)
  age_breaks <- if (youngest_age == 1) c(1, 5, 10, 15) else c(2, 5, 10, 15)
  data <- data |>
    mutate(
      Year_panel = factor(Year, levels = years),
      Cohort_colour = factor(
        ((Cohort - 1988L) %% length(cohort_palette)) + 1L,
        levels = seq_along(cohort_palette)
      )
    )
  excluded_panels <- data |>
    filter(!Used_in_fit) |>
    distinct(Year_panel) |>
    mutate(
      Age = mean(c(youngest_age, 15)),
      Proportion = Inf,
      Label = "No age data\nexcluded from fit"
    )
  ggplot(data, aes(x = Age)) +
    geom_col(
      aes(y = Observed, fill = Cohort_colour),
      width = 0.82,
      colour = "grey35",
      linewidth = 0.15
    ) +
    geom_line(
      aes(y = Fitted, colour = "Fitted", group = Year),
      linewidth = 0.55
    ) +
    geom_point(
      aes(y = Fitted, colour = "Fitted"),
      size = 0.7
    ) +
    geom_text(
      data = excluded_panels,
      aes(x = Age, y = Proportion, label = Label),
      inherit.aes = FALSE,
      vjust = 1.15,
      size = 2.1,
      colour = "grey25",
      lineheight = 0.9
    ) +
    facet_wrap(
      ~Year_panel,
      ncol = ncol,
      dir = "v",
      drop = FALSE
    ) +
    scale_x_continuous(
      breaks = age_breaks,
      limits = c(youngest_age - 0.45, 15.45),
      expand = expansion(mult = 0)
    ) +
    scale_y_continuous(
      limits = c(0, 1.05 * max(c(data$Observed, data$Fitted))),
      expand = expansion(mult = c(0, 0.02))
    ) +
    scale_fill_manual(values = cohort_palette, guide = "none") +
    scale_colour_manual(name = NULL, values = c(Fitted = "#8C2D04")) +
    labs(x = "Age", y = "Proportion") +
    ggthemes::theme_few(base_size = 8) +
    theme(
      legend.position = "top",
      legend.box = "horizontal",
      strip.text = element_text(size = 7, face = "bold"),
      axis.text = element_text(size = 6),
      axis.title = element_text(size = 8),
      panel.grid = element_blank(),
      panel.spacing = grid::unit(2.5, "pt")
    )
}

composition_specs <- list(
  list(
    fleet = "Fishery", years = 1964:1993,
    file = "model_26_0_fishery_age_fits_1964_1993.png", height = 8.5
  ),
  list(
    fleet = "Fishery", years = 1994:2023,
    file = "model_26_0_fishery_age_fits_1994_2023.png", height = 8.5
  ),
  list(
    fleet = "BTS", years = 1982:2002,
    file = "model_26_0_bts_age_fits_1982_2002.png", height = 7.4
  ),
  list(
    fleet = "BTS", years = 2003:2024,
    file = "model_26_0_bts_age_fits_2003_2024.png", height = 7.4
  ),
  list(
    fleet = "ATS", years = 1994:2024,
    file = "model_26_0_ats_age_fits_1994_2024.png", height = 8.5
  )
)

for (spec in composition_specs) {
  plot_data <- composition_data |>
    filter(Fleet == spec$fleet, Year %in% spec$years)
  stopifnot(nrow(plot_data) > 0)
  plot_object <- make_composition_plot(plot_data, years = spec$years)
  ggsave(
    file.path(asset_directory, spec$file),
    plot_object,
    width = 8.5,
    height = spec$height,
    units = "in",
    dpi = 300,
    bg = "white"
  )
}

write.csv(
  index_data,
  file.path(data_directory, "model_26_0_index_fits.csv"),
  row.names = FALSE
)
write.csv(
  composition_data,
  file.path(data_directory, "model_26_0_age_composition_fits.csv"),
  row.names = FALSE
)
write.csv(
  bind_rows(
    index_summary |>
      mutate(Diagnostic_group = "Index fit", .before = 1),
    composition_summary |>
      mutate(Diagnostic_group = "Age-composition fit", .before = 1)
  ),
  file.path(data_directory, "model_26_0_model_fit_summary.csv"),
  row.names = FALSE
)

message("Model 26.0 index and age-composition fit figures are complete.")
