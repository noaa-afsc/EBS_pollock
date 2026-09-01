#!/usr/bin/env Rscript

# Reproduce the matched ADMB comparison used in the September 2026 working
# paper. Model 23.0 is the accepted 2024 ADMB configuration and Model 23.2 is
# the ADMB configuration modified to align its structure and likelihood with
# the Rceattle bridge. Both directories must use the same pm.dat selector and
# shared data files.

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
  library(scales)
})

report_root <- normalizePath(getwd(), mustWork = TRUE)
admb_root <- normalizePath(
  Sys.getenv(
    "EBS_RCEATTLE_ADMB_ROOT",
    file.path(report_root, "..", "ebswp_rceattle", "ADMB")
  ),
  mustWork = TRUE
)

model_directories <- c(
  "Model 23.0" = file.path(admb_root, "m23"),
  "Model 23.2" = file.path(admb_root, "m23_rceattle_full")
)

required_files <- c("pm.tpl", "control.dat", "pm.dat", "pm.par", "pm.rep", "pm.std")
for (model_directory in model_directories) {
  stopifnot(all(file.exists(file.path(model_directory, required_files))))
}
stopifnot(identical(
  readLines(file.path(model_directories[["Model 23.0"]], "pm.dat"), warn = FALSE),
  readLines(file.path(model_directories[["Model 23.2"]], "pm.dat"), warn = FALSE)
))

data_directory <- normalizePath(file.path(admb_root, "data"), mustWork = TRUE)
pm_selector <- readLines(
  file.path(model_directories[["Model 23.0"]], "pm.dat"),
  warn = FALSE
)
shared_input_files <- basename(pm_selector[grepl("^\\.\\./data/", pm_selector)])
stopifnot(all(file.exists(file.path(data_directory, shared_input_files))))

read_report_series <- function(report_file, key, columns = 2L) {
  report_lines <- readLines(report_file, warn = FALSE)
  start <- which(report_lines == key)[1]
  stopifnot(length(start) == 1L, is.finite(start))

  rows <- list()
  cursor <- start + 1L
  while (cursor <= length(report_lines)) {
    values <- suppressWarnings(as.numeric(strsplit(
      trimws(report_lines[cursor]),
      "[[:space:]]+"
    )[[1]]))
    if (length(values) < columns || anyNA(values[seq_len(columns)])) break
    rows[[length(rows) + 1L]] <- values[seq_len(columns)]
    cursor <- cursor + 1L
  }

  result <- as.data.frame(do.call(rbind, rows))
  names(result) <- c("Year", "Estimate", "Std_error", "Lower", "Upper")[seq_len(columns)]
  result
}

read_recruitment_standard_errors <- function(standard_error_file) {
  standard_errors <- read.table(standard_error_file, header = TRUE)
  result <- standard_errors[standard_errors$name == "pred_rec", c("value", "std.dev")]
  names(result) <- c("Estimate_std", "Std_error")
  result
}

read_run_header <- function(parameter_file) {
  header <- readLines(parameter_file, n = 1L, warn = FALSE)
  pattern <- paste0(
    "# Number of parameters = ([0-9]+) Objective function value = ",
    "([^ ]+)  Maximum gradient component = ([^ ]+)"
  )
  matches <- regexec(pattern, header)
  values <- regmatches(header, matches)[[1]]
  stopifnot(length(values) == 4L)
  data.frame(
    Parameters = as.integer(values[2]),
    Objective = as.numeric(values[3]),
    Maximum_gradient = as.numeric(values[4])
  )
}

model_series <- lapply(names(model_directories), function(model_name) {
  model_directory <- model_directories[[model_name]]
  spawning_biomass <- read_report_series(
    file.path(model_directory, "pm.rep"),
    "SSB",
    columns = 5L
  )
  spawning_biomass$Lower <- pmax(
    0,
    spawning_biomass$Estimate - 2 * spawning_biomass$Std_error
  )
  spawning_biomass$Upper <-
    spawning_biomass$Estimate + 2 * spawning_biomass$Std_error
  spawning_biomass$Quantity <- "Spawning biomass"

  recruitment <- read_report_series(
    file.path(model_directory, "pm.rep"),
    "R",
    columns = 2L
  )
  recruitment_standard_errors <- read_recruitment_standard_errors(
    file.path(model_directory, "pm.std")
  )
  stopifnot(nrow(recruitment) == nrow(recruitment_standard_errors))
  recruitment$Std_error <- recruitment_standard_errors$Std_error
  recruitment$Lower <- pmax(0, recruitment$Estimate - 2 * recruitment$Std_error)
  recruitment$Upper <- recruitment$Estimate + 2 * recruitment$Std_error
  recruitment$Quantity <- "Age-1 recruitment"

  result <- rbind(
    spawning_biomass[, c("Year", "Estimate", "Std_error", "Lower", "Upper", "Quantity")],
    recruitment[, c("Year", "Estimate", "Std_error", "Lower", "Upper", "Quantity")]
  )
  result$Model <- model_name
  result
})

comparison_data <- do.call(rbind, model_series)
comparison_data$Model <- factor(
  comparison_data$Model,
  levels = c("Model 23.0", "Model 23.2")
)
comparison_data$Quantity <- factor(
  comparison_data$Quantity,
  levels = c("Spawning biomass", "Age-1 recruitment")
)
stopifnot(
  setequal(comparison_data$Year, 1964:2024),
  all(is.finite(comparison_data$Estimate)),
  all(is.finite(comparison_data$Std_error))
)

comparison_period <- comparison_data[comparison_data$Year >= 1978, ]
comparison_summary <- do.call(
  rbind,
  lapply(levels(comparison_data$Quantity), function(quantity_name) {
    selected <- comparison_period[comparison_period$Quantity == quantity_name, ]
    original <- selected[selected$Model == "Model 23.0", c("Year", "Estimate")]
    bridge <- selected[selected$Model == "Model 23.2", c("Year", "Estimate")]
    names(original)[2] <- "Estimate_23_0"
    names(bridge)[2] <- "Estimate_23_2"
    paired <- merge(original, bridge, by = "Year")
    percent_difference <- 100 * (paired$Estimate_23_2 / paired$Estimate_23_0 - 1)
    data.frame(
      Quantity = quantity_name,
      Start_year = min(paired$Year),
      End_year = max(paired$Year),
      Correlation = cor(paired$Estimate_23_0, paired$Estimate_23_2),
      Mean_absolute_percent_difference = mean(abs(percent_difference)),
      Terminal_percent_difference = tail(percent_difference, 1L)
    )
  })
)

file_sha256 <- function(path) {
  output <- system2("shasum", c("-a", "256", shQuote(path)), stdout = TRUE)
  strsplit(output, "[[:space:]]+")[[1]][1]
}

provenance_rows <- do.call(
  rbind,
  lapply(names(model_directories), function(model_name) {
    model_directory <- model_directories[[model_name]]
    run_header <- read_run_header(file.path(model_directory, "pm.par"))
    data.frame(
      Model = model_name,
      File_role = c(
        "ADMB template",
        "ADMB control",
        "input selector",
        "parameter and convergence output",
        "report output",
        "standard-error output",
        "run summary"
      ),
      Source_file = c(file.path(basename(model_directory), required_files), "pm.par header"),
      SHA256 = c(
        vapply(file.path(model_directory, required_files), file_sha256, character(1)),
        NA_character_
      ),
      Parameters = c(rep(NA_integer_, length(required_files)), run_header$Parameters),
      Objective = c(rep(NA_real_, length(required_files)), run_header$Objective),
      Maximum_gradient = c(rep(NA_real_, length(required_files)), run_header$Maximum_gradient),
      stringsAsFactors = FALSE
    )
  })
)

shared_input_rows <- data.frame(
  Model = "Models 23.0 and 23.2",
  File_role = "shared ADMB input",
  Source_file = file.path("data", shared_input_files),
  SHA256 = vapply(file.path(data_directory, shared_input_files), file_sha256, character(1)),
  Parameters = NA_integer_,
  Objective = NA_real_,
  Maximum_gradient = NA_real_,
  stringsAsFactors = FALSE
)
provenance <- rbind(provenance_rows, shared_input_rows)

data_output_directory <- file.path(
  report_root,
  "doc",
  "data",
  "EBSpollock_Sept_2026",
  "model-26-0"
)
figure_output_directory <- file.path(report_root, "doc", "assets", "model-26-0")
dir.create(data_output_directory, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_output_directory, recursive = TRUE, showWarnings = FALSE)

write.csv(
  comparison_data,
  file.path(data_output_directory, "model_23_0_vs_23_2_admb_trajectories.csv"),
  row.names = FALSE
)
write.csv(
  comparison_summary,
  file.path(data_output_directory, "model_23_0_vs_23_2_admb_summary.csv"),
  row.names = FALSE
)
write.csv(
  provenance,
  file.path(data_output_directory, "model_23_0_vs_23_2_admb_provenance.csv"),
  row.names = FALSE
)

model_colors <- c("Model 23.0" = "#00AEB8", "Model 23.2" = "#E76F61")
model_shapes <- c("Model 23.0" = 16, "Model 23.2" = 17)
model_linetypes <- c("Model 23.0" = "solid", "Model 23.2" = "22")
display_data <- comparison_data[comparison_data$Year >= 1978, ]

spawning_biomass_plot <- ggplot(
  display_data[display_data$Quantity == "Spawning biomass", ],
  aes(x = Year, y = Estimate, color = Model, fill = Model)
) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.16, color = NA) +
  geom_line(aes(linetype = Model), linewidth = 0.7) +
  geom_point(aes(shape = Model), size = 1.8) +
  scale_color_manual(values = model_colors) +
  scale_fill_manual(values = model_colors) +
  scale_shape_manual(values = model_shapes) +
  scale_linetype_manual(values = model_linetypes) +
  scale_x_continuous(breaks = seq(1980, 2025, 5)) +
  scale_y_continuous(labels = label_comma(), limits = c(0, NA)) +
  labs(x = "Year", y = "SSB (thousand t)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), legend.position = "right")

recruitment_plot <- ggplot(
  display_data[display_data$Quantity == "Age-1 recruitment", ],
  aes(x = Year, y = Estimate, color = Model, shape = Model)
) +
  geom_errorbar(
    aes(ymin = Lower, ymax = Upper),
    width = 0.25,
    alpha = 0.8,
    position = position_dodge(width = 0.45)
  ) +
  geom_point(size = 2, position = position_dodge(width = 0.45)) +
  scale_color_manual(values = model_colors) +
  scale_shape_manual(values = model_shapes) +
  scale_x_continuous(breaks = seq(1980, 2025, 5)) +
  scale_y_continuous(labels = label_comma(), limits = c(0, NA)) +
  labs(x = "Year", y = "Age-1 recruitment (thousands)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), legend.position = "none")

comparison_plot <- spawning_biomass_plot / recruitment_plot

ggsave(
  file.path(
    figure_output_directory,
    "model_23_0_vs_23_2_admb_trajectories.png"
  ),
  comparison_plot,
  width = 9,
  height = 9,
  units = "in",
  dpi = 220,
  bg = "white"
)

print(comparison_summary)
print(unique(provenance[provenance$File_role == "run summary", c(
  "Model", "Parameters", "Objective", "Maximum_gradient"
)]))
