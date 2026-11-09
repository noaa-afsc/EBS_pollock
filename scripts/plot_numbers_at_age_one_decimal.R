#!/usr/bin/env Rscript
# Presentation adapter for the coordinated 20261029 saved CSV. Performs no fitting.
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(ggthemes)
})

plot_numbers_at_age <- function(output_dir) {
  cells <- read_csv(file.path(output_dir, "numbers_at_age_display_cells.csv"), show_col_types=FALSE)
  age_means <- read_csv(file.path(output_dir, "numbers_at_age_display_age_means.csv"), show_col_types=FALSE)
  stopifnot(nrow(cells)==630L, setequal(cells$year,1964:2026))
  cells$cell_label <- sprintf("%.1f (%s%%)", cells$ratio_to_age_mean, cells$cv_label)
  palette <- c("#2166AC", "#F7F7F7", "#B35806")
  # Both pages inherit denominators, colour values and CVs computed above from
  # the full 63-year data. Only the year window changes between pages.
  row_height_in <- 0.165
  content_width_in <- 6.5
  content_height_in <- 9
  windows <- list(1964:1989, 1990:2026)
  cells$page <- if_else(cells$year < 1990, 1L, 2L)
  make_page <- function(years, page) {
    decade_lines <- seq(1969.5, 2019.5, by = 10)
    decade_lines <- decade_lines[decade_lines > min(years) - .5 &
      decade_lines < max(years) + .5]
    p <- ggplot(filter(cells, year %in% years), aes(x = age, y = year)) +
      geom_tile(aes(fill = log2_ratio), colour = "white", linewidth = .12,
        width = 1, height = 1) +
      geom_hline(yintercept = decade_lines, colour = "#626262", linewidth = .25) +
      geom_text(aes(label = cell_label, colour = text_colour), size = 2.9,
        family = "sans", lineheight = 1) +
      scale_colour_identity() +
      scale_fill_gradientn(colours = palette, values = c(0, .5, 1),
        limits = c(-4, 4), breaks = c(-4, -2, -1, 0, 1, 2, 4),
        labels = c("1/16", "1/4", "1/2", "1", "2", "4", "16"),
        na.value = "#D9D9D9",
        name = "Multiples of the age-specific mean",
        guide = guide_colourbar(title.position = "top", title.hjust = .5,
          barwidth = grid::unit(5.4, "in"), barheight = grid::unit(.17, "in"),
          ticks.colour = "#555555", frame.colour = "#777777")) +
      scale_x_continuous(breaks = 1:10, labels = c(as.character(1:9), "10+"),
        limits = c(.5, 10.5), position = "top", expand = expansion(add = 0)) +
      scale_y_reverse(breaks = years, limits = c(max(years) + .5, min(years) - .5),
        expand = expansion(add = 0)) +
      labs(title = if (page == 1L) "Model 26.0 numbers at age" else
          "Model 26.0 numbers at age (continued)",
        subtitle = paste0(min(years), "-", max(years),
          " | abundance relative to each age's 1964-2026 mean\n",
          "Cells: abundance / age-specific mean (CV%); CV = asymptotic SE as a percentage of the estimate"),
        x = "Age at start of year", y = "Year",
        caption = paste0("Colour denominators: each age's arithmetic mean over 1964-2026 (all 63 years).\n",
          "Labels: multiples to 1 decimal; CV to whole %. 1 = age mean; 2 = twice the mean.\n",
          "10+ sums ages 10-14 and 15+ once; its SE retains all cross-age covariance.")) +
      theme_few(base_size = 11) +
      theme(plot.title.position = "plot", plot.caption.position = "plot",
        plot.title = element_text(face = "bold", size = 12.5, hjust = 0, margin = margin(b = 5)),
        plot.subtitle = element_text(size = 10, margin = margin(b = 7)),
        axis.text.x = element_text(size = 10.5, colour = "#222222"),
        axis.text.y = element_text(size = 9.5, colour = "#222222"),
        axis.title.x = element_text(size = 11, margin = margin(b = 5)),
        axis.ticks = element_blank(), panel.grid = element_blank(),
        panel.border = element_rect(colour = "#777777", linewidth = .4),
        legend.position = "bottom", legend.title = element_text(size = 10),
        legend.text = element_text(size = 10), legend.margin = margin(t = 7, b = 4),
        plot.caption = element_text(hjust = 0, size = 8.5, lineheight = 1.2,
          margin = margin(t = 5)),
        plot.margin = margin(3, 3, 3, 3))
    g <- ggplotGrob(p)
    panel_row <- unique(g$layout$t[g$layout$name == "panel"])
    stopifnot(length(panel_row) == 1L)
    # Absolute panel heights preserve the same physical row height on both pages.
    g$heights[panel_row] <- grid::unit(length(years) * row_height_in, "in")
    list(grob = g, years = years, panel_height_in = length(years) * row_height_in)
  }
  pages <- lapply(seq_along(windows), function(i) make_page(windows[[i]], i))
  draw_page <- function(page, letter = FALSE, canvas_height = content_height_in) {
    grid::grid.newpage()
    h <- grid::convertHeight(sum(page$grob$heights), "in", valueOnly = TRUE)
    stopifnot(h <= content_height_in)
    grid::pushViewport(grid::viewport(
      x = grid::unit(if (letter) 1 else 0, "in"),
      y = grid::unit(if (letter) 10 else canvas_height, "in"),
      width = grid::unit(content_width_in, "in"), height = grid::unit(h, "in"),
      just = c("left", "top")))
    grid::grid.draw(page$grob)
    grid::popViewport()
    h
  }
  # Measure on the PNG device before authoring the content-sized assets.
  # PDF pages retain the established US Letter size and one-inch margins.
  scratch <- tempfile(fileext = ".png")
  grDevices::png(scratch, width = content_width_in, height = content_height_in,
    units = "in", res = 240, bg = "white")
  grid::grid.newpage()
  image_heights <- vapply(pages, function(page)
    grid::convertHeight(sum(page$grob$heights), "in", valueOnly = TRUE), numeric(1)) + .07
  grDevices::dev.off()
  unlink(scratch)
  heights <- numeric(2)
  for (i in seq_along(pages)) {
    stem <- if (i == 1L) "model26_2_numbers_at_age_uncertainty" else
      "model26_2_numbers_at_age_uncertainty_continued"
    grDevices::png(file.path(output_dir, paste0(stem, ".png")),
      width = content_width_in, height = image_heights[i], units = "in", res = 240,
      bg = "white")
    heights[i] <- draw_page(pages[[i]], canvas_height = image_heights[i])
    grDevices::dev.off()
    svglite::svglite(file.path(output_dir, paste0(stem, ".svg")),
      width = content_width_in, height = image_heights[i], bg = "white")
    draw_page(pages[[i]], canvas_height = image_heights[i])
    grDevices::dev.off()
  }
  grDevices::pdf(file.path(output_dir, "model26_2_numbers_at_age_uncertainty.pdf"),
    width = 8.5, height = 11, onefile = TRUE, paper = "special",
    title = "Figure 24: Model 26.0 numbers at age and uncertainty", bg = "white")
  for (page in pages) draw_page(page, letter = TRUE)
  grDevices::dev.off()
  write_csv(cells, file.path(output_dir, "numbers_at_age_display_cells.csv"))
  write_csv(age_means, file.path(output_dir, "numbers_at_age_display_age_means.csv"))
  jsonlite::write_json(list(figure = 24L, page_size_in = c(8.5, 11),
    margins_in = 1, content_size_in = c(content_width_in, content_height_in),
    year_windows = windows, rows = c(26L, 37L), cells = c(260L, 370L),
    row_height_in = row_height_in, panel_heights_in = c(26, 37) * row_height_in,
    content_heights_in = heights, font_size_mm = 2.9,
    denominator_years = c(1964L, 2026L), colour_limits_log2 = c(-4, 4),
    colour_breaks_log2 = c(-4, -2, -1, 0, 1, 2, 4)),
    file.path(output_dir, "numbers_at_age_pagination.json"), pretty = TRUE,
    auto_unbox = TRUE, digits = 17)
  invisible(pages)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 1L) stop("Supply the directory containing the saved uncertainty CSV")
  plot_numbers_at_age(args[[1]])
}
