#!/usr/bin/env Rscript

# Script ID M26-12: Post-process the saved Model 26.0 SparseNUTS fit.
#
# This script does not rerun the sampler. It uses SparseNUTS methods to create
# a pairs plot for the six lowest-bulk-ESS parameters and to compare MCMC
# posterior standard deviations with Hessian-based asymptotic standard errors.

rceattle_library <- Sys.getenv(
  "RCEATTLE_LIB",
  "/tmp/rceattle-dsem-v5-lib-01a05b46"
)
.libPaths(c(rceattle_library, .libPaths()))

suppressPackageStartupMessages({
  library(SparseNUTS)
  library(dplyr)
  library(ggplot2)
})

stopifnot(
  packageVersion("SparseNUTS") == package_version("1.0.2"),
  requireNamespace("ellipse", quietly = TRUE),
  requireNamespace("ggrepel", quietly = TRUE),
  requireNamespace("ggthemes", quietly = TRUE)
)

command_line <- commandArgs(trailingOnly = FALSE)
script_argument <- command_line[grepl("^--file=", command_line)]
stopifnot(length(script_argument) == 1L)
script_path <- normalizePath(sub("^--file=", "", script_argument))
default_report_root <- file.path(dirname(script_path), "..", "..", "..")
report_root <- normalizePath(
  Sys.getenv("EBS_REPORT_ROOT", default_report_root),
  mustWork = TRUE
)
analysis_root <- normalizePath(
  Sys.getenv(
    "EBS_ANALYSIS_ROOT",
    file.path(dirname(report_root), "ebswp_rceattle")
  ),
  mustWork = TRUE
)

fit_file <- file.path(
  analysis_root, "results", "model_26_0_sparsenuts_5.23.0",
  "model_26_0_sparsenuts_fit.rds"
)
stopifnot(file.exists(fit_file))

asset_directory <- file.path(report_root, "doc", "assets", "model-26-0")
data_directory <- file.path(
  report_root, "doc", "data", "EBSpollock_Sept_2026", "model-26-0"
)
dir.create(asset_directory, recursive = TRUE, showWarnings = FALSE)
dir.create(data_directory, recursive = TRUE, showWarnings = FALSE)

fit <- readRDS(fit_file)
stopifnot(
  inherits(fit, "tmbfit"),
  identical(fit$algorithm, "SNUTS"),
  dim(fit$samples)[2] == 4L,
  fit$warmup == 1000L,
  fit$iter == 1000L,
  !is.null(fit$mle$se)
)

parameter_monitor <- fit$monitor |>
  filter(.data$variable != "lp__")
stopifnot(nrow(parameter_monitor) == 1218L)

slowest_parameters <- parameter_monitor |>
  arrange(.data$ess_bulk) |>
  slice_head(n = 6L) |>
  transmute(
    Parameter = .data$variable,
    Rhat = .data$rhat,
    Bulk_ESS = .data$ess_bulk,
    Tail_ESS = .data$ess_tail
  )

write.csv(
  slowest_parameters,
  file.path(
    data_directory,
    "model_26_0_sparsenuts_slowest_parameters.csv"
  ),
  row.names = FALSE
)

pairs_file <- file.path(
  asset_directory,
  "model_26_0_sparsenuts_slowest_pairs.png"
)
png(pairs_file, width = 3600, height = 3600, res = 300)
pairs(
  fit,
  pars = slowest_parameters$Parameter,
  diag = "hist",
  add.mle = TRUE,
  add.monitor = TRUE,
  point.col = grDevices::adjustcolor("#0072B2", alpha.f = 0.12),
  point.pch = 16,
  label.cex = 0.88
)
invisible(dev.off())

uncertainty_comparison <- SparseNUTS::plot_uncertainties(
  fit,
  log = TRUE,
  plot = FALSE
) |>
  transmute(
    Parameter = .data$par,
    MCMC_posterior_SD = .data$sd.post,
    Hessian_asymptotic_SE = .data$sd.mle,
    MCMC_to_Hessian_ratio = .data$sd.post / .data$sd.mle,
    Slowest_six = .data$par %in% slowest_parameters$Parameter
  )

stopifnot(
  nrow(uncertainty_comparison) == 1218L,
  all(is.finite(uncertainty_comparison$MCMC_posterior_SD)),
  all(is.finite(uncertainty_comparison$Hessian_asymptotic_SE)),
  all(uncertainty_comparison$MCMC_posterior_SD > 0),
  all(uncertainty_comparison$Hessian_asymptotic_SE > 0)
)

write.csv(
  uncertainty_comparison,
  file.path(
    data_directory,
    "model_26_0_sparsenuts_uncertainty_comparison.csv"
  ),
  row.names = FALSE
)

log_sd_correlation <- cor(
  log(uncertainty_comparison$MCMC_posterior_SD),
  log(uncertainty_comparison$Hessian_asymptotic_SE)
)
slowest_draws <- SparseNUTS::extract_samples(fit)[
  , slowest_parameters$Parameter,
  drop = FALSE
]
slowest_correlations <- cor(slowest_draws)
slowest_pairwise_correlations <- slowest_correlations[
  lower.tri(slowest_correlations)
]
slowest_ratios <- uncertainty_comparison$MCMC_to_Hessian_ratio[
  uncertainty_comparison$Slowest_six
]
uncertainty_summary <- data.frame(
  Metric = c(
    "Parameters compared",
    "Log-scale SD correlation",
    "Median MCMC-to-Hessian uncertainty ratio",
    "Minimum ratio among slowest six",
    "Maximum ratio among slowest six",
    "Minimum pairwise correlation among slowest six",
    "Maximum pairwise correlation among slowest six"
  ),
  Value = c(
    nrow(uncertainty_comparison),
    log_sd_correlation,
    median(uncertainty_comparison$MCMC_to_Hessian_ratio),
    min(slowest_ratios),
    max(slowest_ratios),
    min(slowest_pairwise_correlations),
    max(slowest_pairwise_correlations)
  )
)
write.csv(
  uncertainty_summary,
  file.path(
    data_directory,
    "model_26_0_sparsenuts_uncertainty_summary.csv"
  ),
  row.names = FALSE
)

plot_limits <- range(c(
  uncertainty_comparison$MCMC_posterior_SD,
  uncertainty_comparison$Hessian_asymptotic_SE
))
uncertainty_figure <- ggplot(
  uncertainty_comparison,
  aes(
    x = .data$MCMC_posterior_SD,
    y = .data$Hessian_asymptotic_SE
  )
) +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "grey45",
    linetype = "dashed",
    linewidth = 0.55
  ) +
  geom_point(color = "#0072B2", alpha = 0.28, size = 1.25) +
  geom_point(
    data = filter(uncertainty_comparison, .data$Slowest_six),
    color = "#D55E00",
    size = 2.3
  ) +
  ggrepel::geom_text_repel(
    data = filter(uncertainty_comparison, .data$Slowest_six),
    aes(label = .data$Parameter),
    color = "#9C3D00",
    size = 3.0,
    min.segment.length = 0,
    box.padding = 0.35,
    max.overlaps = Inf,
    seed = 20260901
  ) +
  scale_x_log10(limits = plot_limits) +
  scale_y_log10(limits = plot_limits) +
  coord_equal() +
  labs(
    x = "MCMC posterior standard deviation",
    y = "Hessian-based asymptotic standard error"
  ) +
  ggthemes::theme_few(base_size = 11)

ggsave(
  file.path(
    asset_directory,
    "model_26_0_sparsenuts_uncertainty_comparison.png"
  ),
  uncertainty_figure,
  width = 7.2,
  height = 7.2,
  dpi = 300
)

message("SparseNUTS pairs and uncertainty diagnostics are complete.")
