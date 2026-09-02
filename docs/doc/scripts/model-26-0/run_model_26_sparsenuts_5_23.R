#!/usr/bin/env Rscript

# Script ID M26-10: Model 26.0 exploratory SparseNUTS run.
#
# The sampler uses the complete joint Model 26.0 objective. Recruitment
# deviations remain penalized fixed effects, matching the operational fit;
# no Laplace random-effect reconstruction is introduced. The large fit object
# stays in the local analysis results directory. Compact CSV and PNG products
# can be copied into the assessment-document repository after validation.

rceattle_library <- Sys.getenv(
  "RCEATTLE_LIB",
  "/tmp/rceattle-dsem-v5-lib-01a05b46"
)
.libPaths(c(rceattle_library, .libPaths()))

suppressPackageStartupMessages({
  library(Rceattle)
  library(SparseNUTS)
  library(posterior)
  library(dplyr)
  library(ggplot2)
  library(tidyr)
})

expected_rceattle_version <- package_version("5.23.0")
expected_rceattle_commit <- "adf22f84b399e84e3b9a707e0b3bbb1624179a27"
stopifnot(
  packageVersion("Rceattle") == expected_rceattle_version,
  packageVersion("SparseNUTS") == package_version("1.0.2")
)

project_root <- normalizePath(getwd(), mustWork = TRUE)
fit_file <- Sys.getenv(
  "MODEL26_FIT_FILE",
  file.path(
    project_root, "results", "model_26_0_rceattle_5.23.0",
    "model_26_0_fit.rds"
  )
)
output_directory <- Sys.getenv(
  "MODEL26_SNUTS_OUTPUT_DIR",
  file.path(project_root, "results", "model_26_0_sparsenuts_5.23.0")
)
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

chains <- 4L
warmup <- 1000L
retained_per_chain <- 1000L
cores <- min(chains, as.integer(Sys.getenv("MODEL26_SNUTS_CORES", "4")))
seed <- as.integer(Sys.getenv("MODEL26_SNUTS_SEED", "20260901"))
adapt_delta <- 0.95
max_treedepth <- 12L

saved <- readRDS(fit_file)
model <- saved$model_26
stopifnot(
  identical(saved$Rceattle_version, as.character(expected_rceattle_version)),
  identical(saved$Rceattle_commit, expected_rceattle_commit),
  identical(model$convergence$status, "OK"),
  isTRUE(model$sdrep$pdHess),
  length(model$obj$env$random) == 0L
)

# The serialized TMB object retains its original initial vector in obj$par.
# SparseNUTS starts at the converged optimizer vector instead.
joint_object <- model$obj
joint_object$par <- as.numeric(model$opt$par)
names(joint_object$par) <- names(model$opt$par)
joint_object$env$last.par.best <- joint_object$par
start_objective <- joint_object$fn(joint_object$par)
start_gradient <- joint_object$gr(joint_object$par)
maximum_start_gradient <- max(abs(start_gradient))
parameter_count <- length(joint_object$par)
stopifnot(
  parameter_count == 1218L,
  is.finite(start_objective),
  maximum_start_gradient < 1e-3
)

message(
  "Sampling ", parameter_count, " joint parameters with ", chains,
  " chains; each chain has ", warmup, " warmup and ",
  retained_per_chain, " retained draws."
)
start_time <- Sys.time()
fit <- SparseNUTS::sample_snuts(
  obj = joint_object,
  num_samples = retained_per_chain,
  num_warmup = warmup,
  chains = chains,
  cores = cores,
  seed = seed,
  metric = "dense",
  skip_optimization = TRUE,
  laplace = FALSE,
  init = "last.par.best",
  control = list(
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth
  ),
  model_name = "EBS pollock Model 26.0",
  refresh = 100
)
end_time <- Sys.time()

saveRDS(
  fit,
  file.path(output_directory, "model_26_0_sparsenuts_fit.rds"),
  compress = "xz"
)

parameter_summary <- as.data.frame(fit$monitor)
write.csv(
  parameter_summary,
  file.path(output_directory, "model_26_0_sparsenuts_parameter_summary.csv"),
  row.names = FALSE
)

sampler_diagnostics <- SparseNUTS::check_snuts_diagnostics(fit, print = TRUE)
write.csv(
  sampler_diagnostics,
  file.path(output_directory, "model_26_0_sparsenuts_sampler_diagnostics.csv"),
  row.names = FALSE
)

sampler_parameters <- SparseNUTS::extract_sampler_params(fit)
write.csv(
  sampler_parameters,
  file.path(output_directory, "model_26_0_sparsenuts_sampler_parameters.csv"),
  row.names = FALSE
)

chain_diagnostics <- sampler_parameters |>
  group_by(.data$chain) |>
  summarise(
    Divergences = sum(.data$divergent__ > 0),
    Maximum_tree_depth_events = sum(.data$treedepth__ >= max_treedepth),
    Mean_acceptance_probability = mean(.data$accept_stat__),
    Median_leapfrog_steps = median(.data$n_leapfrog__),
    E_BFMI = sum(diff(.data$energy__)^2) /
      sum((.data$energy__ - mean(.data$energy__))^2),
    .groups = "drop"
  )
write.csv(
  chain_diagnostics,
  file.path(output_directory, "model_26_0_sparsenuts_chain_diagnostics.csv"),
  row.names = FALSE
)

minimum_ebfmi <- min(chain_diagnostics$E_BFMI)
maximum_rhat <- max(parameter_summary$rhat, na.rm = TRUE)
minimum_bulk_ess <- min(parameter_summary$ess_bulk, na.rm = TRUE)
minimum_tail_ess <- min(parameter_summary$ess_tail, na.rm = TRUE)
total_divergences <- sum(chain_diagnostics$Divergences)
total_tree_depth_events <- sum(chain_diagnostics$Maximum_tree_depth_events)

diagnostic_summary <- tibble(
  Diagnostic = c(
    "Chains", "Warmup iterations per chain", "Retained draws per chain",
    "Total retained draws", "Joint parameters", "Metric",
    "Maximum R-hat", "Minimum bulk ESS", "Minimum tail ESS",
    "Divergences", "Maximum-tree-depth events", "Minimum E-BFMI",
    "Maximum gradient at sampler start", "Elapsed hours"
  ),
  Value = c(
    chains, warmup, retained_per_chain, chains * retained_per_chain,
    parameter_count, NA_real_, maximum_rhat, minimum_bulk_ess,
    minimum_tail_ess, total_divergences, total_tree_depth_events,
    minimum_ebfmi, maximum_start_gradient,
    as.numeric(difftime(end_time, start_time, units = "hours"))
  ),
  Text_value = c(
    rep(NA_character_, 5), "dense", rep(NA_character_, 8)
  ),
  Target = c(
    "4", "1000", "1000", "4000", "recorded", "dense",
    "<= 1.01", ">= 400", ">= 400", "0", "0", "> 0.30",
    "< 0.001", "recorded"
  ),
  Outcome = c(
    rep("Pass", 6),
    ifelse(maximum_rhat <= 1.01, "Pass", "Review"),
    ifelse(minimum_bulk_ess >= 400, "Pass", "Review"),
    ifelse(minimum_tail_ess >= 400, "Pass", "Review"),
    ifelse(total_divergences == 0, "Pass", "Review"),
    ifelse(total_tree_depth_events == 0, "Pass", "Review"),
    ifelse(minimum_ebfmi > 0.30, "Pass", "Review"),
    ifelse(maximum_start_gradient < 0.001, "Pass", "Review"),
    "Recorded"
  )
)
write.csv(
  diagnostic_summary,
  file.path(output_directory, "model_26_0_sparsenuts_diagnostic_summary.csv"),
  row.names = FALSE
)

run_manifest <- tibble(
  Item = c(
    "Script ID", "Rceattle branch", "Rceattle commit", "Rceattle version",
    "SparseNUTS version", "RTMB version", "Fit file", "Output directory",
    "Objective", "Random-effect blocks", "Seed", "Cores", "Started",
    "Completed"
  ),
  Value = c(
    "M26-10", "dsem-v5-integration", expected_rceattle_commit,
    as.character(packageVersion("Rceattle")),
    as.character(packageVersion("SparseNUTS")),
    as.character(packageVersion("RTMB")), normalizePath(fit_file),
    normalizePath(output_directory), format(start_objective, digits = 12),
    "none; complete joint objective", seed, cores,
    format(start_time, tz = "America/Los_Angeles"),
    format(end_time, tz = "America/Los_Angeles")
  )
)
write.csv(
  run_manifest,
  file.path(output_directory, "model_26_0_sparsenuts_run_manifest.csv"),
  row.names = FALSE
)

diagnostic_plot_data <- bind_rows(
  transmute(parameter_summary, Metric = "R-hat", Value = .data$rhat),
  transmute(parameter_summary, Metric = "Bulk ESS", Value = .data$ess_bulk),
  transmute(parameter_summary, Metric = "Tail ESS", Value = .data$ess_tail)
) |>
  filter(is.finite(.data$Value)) |>
  mutate(
    Metric = factor(.data$Metric, levels = c("R-hat", "Bulk ESS", "Tail ESS"))
  )

diagnostic_figure <- ggplot(diagnostic_plot_data, aes(x = .data$Value)) +
  geom_histogram(bins = 36, fill = "#0072B2", color = "white") +
  facet_wrap(vars(.data$Metric), scales = "free", ncol = 1) +
  labs(x = NULL, y = "Parameter count") +
  ggthemes::theme_few(base_size = 11)
ggsave(
  file.path(output_directory, "model_26_0_sparsenuts_diagnostics.png"),
  diagnostic_figure,
  width = 7.2,
  height = 8.6,
  dpi = 300
)

sample_array <- fit$samples
sample_variables <- dimnames(sample_array)[[3]]
preferred_variables <- c(
  "rec_pars", "rec_dev[61]", "sel_coff[1,1,5]", "sel_coff_dev[1,1,61,5]"
)
selected_variables <- preferred_variables[preferred_variables %in% sample_variables]
if (length(selected_variables) < 4L) {
  selected_variables <- unique(c(
    selected_variables,
    sample_variables[round(seq(1, length(sample_variables), length.out = 4L))]
  ))[seq_len(4L)]
}
retained_rows <- seq.int(fit$warmup + 1L, fit$warmup + fit$iter)
trace_data <- bind_rows(lapply(seq_along(selected_variables), function(variable_index) {
  variable <- selected_variables[[variable_index]]
  array_index <- match(variable, sample_variables)
  bind_rows(lapply(seq_len(chains), function(chain) {
    tibble(
      Variable = variable,
      Chain = factor(chain),
      Iteration = seq_len(retained_per_chain),
      Value = sample_array[retained_rows, chain, array_index]
    )
  }))
}))

trace_figure <- ggplot(
  trace_data,
  aes(x = .data$Iteration, y = .data$Value, color = .data$Chain)
) +
  geom_line(linewidth = 0.23, alpha = 0.78) +
  facet_wrap(vars(.data$Variable), scales = "free_y", ncol = 1) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Retained iteration", y = "Parameter value", color = "Chain") +
  ggthemes::theme_few(base_size = 10) +
  theme(legend.position = "bottom")
ggsave(
  file.path(output_directory, "model_26_0_sparsenuts_trace.png"),
  trace_figure,
  width = 8.0,
  height = 9.5,
  dpi = 300
)

writeLines(
  capture.output(sessionInfo()),
  file.path(output_directory, "model_26_0_sparsenuts_session_info.txt")
)

message("SparseNUTS run and compact diagnostic products are complete.")
