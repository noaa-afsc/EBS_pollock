#!/usr/bin/env Rscript
# Descriptive saved-input histories only; no fit, DSEM call or imputation.
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(ggthemes)
})
args <- commandArgs(trailingOnly = TRUE)
option <- function(key) {
  at <- match(key, args)
  if (is.na(at) || at == length(args)) stop("Supply ", key)
  args[at + 1L]
}
input <- normalizePath(option("--input"))
output <- option("--output")
dir.create(output, recursive = TRUE, showWarnings = FALSE)
output <- normalizePath(output)
sha <- function(path) digest::digest(file = path, algo = "sha256")
source_hash <- sha(input)
stopifnot(source_hash == "2a96a4d07ee192a34c166809b0e758e0a79ed65252f655c5bb97d86cff54a4e9")
raw <- readr::read_csv(input, col_types = cols(.default = col_character()), na = character(),
                      show_col_types = FALSE)
stopifnot(nrow(raw) == 63L, identical(as.integer(raw$year), 1964:2026))
dictionary <- tibble::tribble(
  ~variable, ~display_name, ~group,
  "seaice", "Sea ice extent", "physical",
  "bloom", "Open-water blooms", "physical",
  "cp", "Cold-pool extent", "physical",
  "sst", "Sea-surface temperature", "physical",
  "bt", "Bottom temperature", "physical",
  "chla", "Chlorophyll a", "physical",
  "euph", "Euphausiids", "prey",
  "spsmcope", "Small copepods - spring", "prey",
  "splgcope", "Large copepods - spring", "prey",
  "smsmcope", "Small copepods - summer", "prey",
  "smlgcope", "Large copepods - summer", "prey",
  "jelly", "Jellyfish", "prey",
  "age1pol", "Assessment age-1 pollock", "fish-condition",
  "larvae", "Larval pollock", "fish-condition",
  "juv", "Age-0 pollock", "fish-condition",
  "energy", "Pollock energy density", "fish-condition",
  "atf", "Arrowtooth flounder", "fish-condition",
  "pcod", "Pacific cod", "fish-condition",
  "pol", "Adult pollock", "fish-condition"
)
stopifnot(setequal(dictionary$variable, setdiff(names(raw), "year")))
long <- raw |>
  mutate(year = as.integer(year)) |>
  pivot_longer(-year, names_to = "variable", values_to = "recorded_text") |>
  mutate(value = suppressWarnings(as.numeric(if_else(recorded_text %in% c("", "NA"),
                                                     NA_character_, recorded_text))),
         recorded = is.finite(value)) |>
  left_join(dictionary, by = "variable")
stopifnot(nrow(long) == 63L * 19L,
          all(long$recorded | long$recorded_text %in% c("", "NA")))
baselines <- long |>
  group_by(variable, display_name, group) |>
  summarise(n = sum(recorded),
            first_year = min(year[recorded]), latest_year = max(year[recorded]),
            baseline_years = paste(year[recorded], collapse = ";"),
            mean = mean(value[recorded]), sample_sd = sd(value[recorded]),
            .groups = "drop")
stopifnot(all(baselines$n > 1L), all(is.finite(baselines$sample_sd)),
          all(baselines$sample_sd > 0))
long <- long |>
  left_join(select(baselines, variable, n, first_year, latest_year, mean, sample_sd), by = "variable") |>
  mutate(standardized = (value - mean) / sample_sd,
         latest_record = recorded & year == latest_year)
observed <- long |>
  filter(recorded) |>
  arrange(variable, year) |>
  group_by(variable) |>
  mutate(segment = cumsum(c(TRUE, diff(year) != 1L))) |>
  ungroup()
long <- long |>
  left_join(select(observed, variable, year, segment), by = c("variable", "year")) |>
  arrange(match(variable, dictionary$variable), year)
readr::write_csv(long, file.path(output, "plot-data.csv"), na = "NA")
readr::write_csv(baselines, file.path(output, "baseline-summary.csv"), na = "NA")
readr::write_csv(dictionary, file.path(output, "display-dictionary.csv"))
stopifnot(sum(long$recorded) == 610L, sum(is.na(long$standardized)) == 587L,
          all(observed$standardized > -3 & observed$standardized < 5),
          all(baselines$latest_year[baselines$variable == "energy"] == 2025L),
          all(baselines$latest_year[baselines$variable == "age1pol"] == 2024L),
          all(baselines$latest_year[!baselines$variable %in% c("energy", "age1pol")] == 2026L))

titles <- c(physical = "Physical environment and production",
            prey = "Zooplankton and prey",
            `fish-condition` = "Fish populations and pollock condition")
plot_group <- function(which) {
  order <- dictionary$variable[dictionary$group == which]
  d <- observed |> filter(group == which) |>
    mutate(panel = paste0(display_name, "\n", n, " records | latest ", latest_year))
  panel_order <- baselines |>
    filter(variable %in% order) |>
    arrange(match(variable, order)) |>
    transmute(panel = paste0(display_name, "\n", n, " records | latest ", latest_year)) |>
    pull(panel)
  d$panel <- factor(d$panel, levels = panel_order)
  lines <- d |>
    add_count(variable, segment, name = "segment_n") |>
    filter(segment_n > 1L)
  p <- ggplot(d, aes(year, standardized)) +
    geom_hline(yintercept = 0, colour = "#7e8a90", linewidth = .35, linetype = "dashed") +
    geom_line(data = lines, aes(group = interaction(variable, segment)),
              colour = "#28627a", linewidth = .42) +
    geom_point(colour = "#26343c", size = .85) +
    geom_point(data = filter(d, latest_record), shape = 23, fill = "#d55e00",
               colour = "#6a310f", size = 2.2, stroke = .35) +
    facet_wrap(vars(panel), ncol = 2L, drop = FALSE) +
    scale_x_continuous(breaks = c(1964, 1980, 2000, 2026), limits = c(1964, 2026),
                       expand = expansion(mult = c(.015, .025))) +
    scale_y_continuous(breaks = c(-2, 0, 2, 4), limits = c(-3, 5),
                       expand = expansion(mult = c(.015, .015))) +
    labs(title = unname(titles[which]),
         subtitle = "Input histories, centered and scaled separately for each series",
         x = "Year", y = "Departure from series mean (sample SD)",
         caption = paste(
           "Means and sample SDs use each series' available records, including its latest year.",
           "Lines join consecutive years only; missing values remain blank.",
           sep = "\n")) +
    ggthemes::theme_few(base_size = 11, base_family = "Arial") +
    theme(plot.title = element_text(size = 15, face = "bold", colour = "#17394a"),
          plot.subtitle = element_text(size = 9, colour = "#456273", margin = margin(b = 10)),
          plot.caption = element_text(size = 8, hjust = 0, colour = "#456273", lineheight = 1.2,
                                      margin = margin(t = 9)),
          strip.background = element_rect(fill = "#edf2f4", colour = "#bdcbd0", linewidth = .25),
          strip.text = element_text(size = 9.3, face = "bold", hjust = 0,
                                     colour = "#17394a", lineheight = 1.13),
          panel.spacing.x = grid::unit(22, "pt"), panel.spacing.y = grid::unit(9, "pt"),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text = element_text(size = 9), axis.title = element_text(size = 10),
          plot.margin = margin(12, 14, 12, 12))
  # Save plotted layers so a separate Python check can reconcile every observation.
  built <- ggplot_build(p)
  panel_map <- built$layout$layout |>
    transmute(PANEL = as.integer(PANEL), panel = as.character(panel))
  values <- as_tibble(built$data[[3]]) |>
    transmute(PANEL = as.integer(PANEL), plotted_year = x, plotted_standardized = y) |>
    left_join(panel_map, by = "PANEL")
  readr::write_csv(values, file.path(output, paste0(which, "-built-points.csv")))
  segments <- as_tibble(built$data[[2]]) |>
    transmute(PANEL = as.integer(PANEL), plotted_year = x, plotted_standardized = y, group = group) |>
    left_join(panel_map, by = "PANEL")
  readr::write_csv(segments, file.path(output, paste0(which, "-built-lines.csv")))
  readr::write_csv(panel_map, file.path(output, paste0(which, "-panel-map.csv")))
  p
}
plots <- lapply(c("physical", "prey", "fish-condition"), plot_group)
names(plots) <- c("physical", "prey", "fish-condition")
for (which in names(plots)) {
  stem <- paste0("indicators-", which)
  ggsave(file.path(output, paste0(stem, ".png")), plots[[which]], width = 7, height = 8.5,
         units = "in", dpi = 300, device = ragg::agg_png, bg = "white")
  ggsave(file.path(output, paste0(stem, ".pdf")), plots[[which]], width = 7, height = 8.5,
         units = "in", device = grDevices::cairo_pdf, family = "Arial", bg = "white")
}
grDevices::cairo_pdf(file.path(output, "indicator-histories-three-pages.pdf"), width = 7,
                    height = 8.5, family = "Arial", onefile = TRUE, bg = "white")
for (p in plots) print(p)
invisible(dev.off())
stopifnot(sha(input) == source_hash)
record <- list(status = "PASS_SAVED_INPUT_STANDARDIZED_HISTORIES", input = input,
  input_sha256 = source_hash, source_unchanged = TRUE, years = c(1964, 2026),
  variables = 19L, recorded_values = 610L, missing_cells = 587L,
  standardization = "Within each variable, all available records including latest; arithmetic mean and sample SD (n-1); no transform or imputation",
  y_axis = "Departure from series mean in sample standard deviations; physical units unverified",
  reference = "Dashed zero line only; no uncertainty intervals or fitted states",
  line_rule = "Consecutive recorded calendar years only; all gaps retained",
  shared_axes = list(x = c(1964, 2026), y = c(-3, 5)),
  panels = list(physical = 6L, prey = 6L, fish_condition = 7L),
  latest_record_years = list(energy = 2025L, age1pol = 2024L, all_other_variables = 2026L),
  page_inches = c(7, 8.5), source_units_invented = FALSE, model_execution = FALSE,
  outputs = lapply(list.files(output, pattern = "[.](png|pdf)$", full.names = TRUE),
                   function(f) list(path = f, sha256 = sha(f), bytes = file.size(f))))
jsonlite::write_json(record, file.path(output, "render-validation.json"), pretty = TRUE,
                     auto_unbox = TRUE, digits = NA)
cat("PASS: 19 standardized recorded series on three pages; source unchanged\n")
