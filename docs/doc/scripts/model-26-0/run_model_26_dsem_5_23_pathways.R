#!/usr/bin/env Rscript

# Exploratory DSEM recruitment pathways for Model 26.0.
#
# The assessment structure and simple selectivity configuration are inherited
# from the fitted Model 26.0 object. Four common-data DSEM specifications test
# a no-edge reference, cold-pool extent, age-0 late-summer SST, and both paths.

rceattle_library <- Sys.getenv(
  "RCEATTLE_LIB",
  "/tmp/rceattle-dsem-v5-lib-01a05b46"
)
.libPaths(c(rceattle_library, .libPaths()))

suppressPackageStartupMessages({
  library(Rceattle)
  library(dsem)
  library(dplyr)
  library(ggplot2)
  library(tidyr)
})

expected_commit <- "adf22f84b399e84e3b9a707e0b3bbb1624179a27"
stopifnot(
  packageVersion("Rceattle") == package_version("5.23.0"),
  packageVersion("dsem") == package_version("3.0.0")
)

project_root <- normalizePath(getwd(), mustWork = TRUE)
output_directory <- Sys.getenv(
  "MODEL26_DSEM_OUTPUT_DIR",
  file.path(project_root, "results", "model_26_0_dsem_5.23.0")
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

cohort_table_file <- Sys.getenv(
  "EBSWP_ESP_COHORT_TABLE",
  file.path(project_root, "..", "ebswp_esp", "data", "cohort_table.csv")
)
cohort <- read.csv(cohort_table_file, check.names = FALSE)

standardize_observed <- function(x) {
  observed <- is.finite(x)
  answer <- rep(NA_real_, length(x))
  answer[observed] <- as.numeric(scale(x[observed]))
  answer
}

# The DSEM year is the year when the cohort enters the model at age 1. SST is
# measured in that cohort's age-0 year; cold-pool extent is for the age-1 year.
covariates <- data.frame(
  Year = cohort$year + 1L,
  ColdPool = standardize_observed(cohort$cold_pool_t1),
  LateSummerSST = standardize_observed(cohort$late_summer_SST_t)
)
est$env_data <- merge(
  data.frame(Year = est$styr:est$endyr),
  covariates,
  by = "Year",
  all.x = TRUE
)
write.csv(
  est$env_data,
  file.path(output_directory, "cohort_aligned_covariates.csv"),
  row.names = FALSE
)

sem_common <- c(
  "ColdPool -> ColdPool, 1, AR_ColdPool, 0",
  "LateSummerSST -> LateSummerSST, 1, AR_LateSummerSST, 0",
  "recdevs1 <-> recdevs1, 0, sigmaR1, 0.6"
)
specifications <- list(
  iid = sem_common,
  cold_pool = c(sem_common, "ColdPool -> recdevs1, 0, ColdPool_to_R, 0"),
  late_summer_sst = c(sem_common, "LateSummerSST -> recdevs1, 0, LateSummerSST_to_R, 0"),
  combined = c(
    sem_common,
    "ColdPool -> recdevs1, 0, ColdPool_to_R, 0",
    "LateSummerSST -> recdevs1, 0, LateSummerSST_to_R, 0"
  )
)
model_labels <- c(
  iid = "No-edge reference",
  cold_pool = "Cold pool",
  late_summer_sst = "Age-0 late-summer SST",
  combined = "Cold pool + age-0 late-summer SST"
)

fit_one <- function(name, paths) {
  checkpoint <- file.path(output_directory, paste0("dsem_", name, ".rds"))
  if (file.exists(checkpoint)) return(readRDS(checkpoint))

  inits <- Rceattle::build_params(est)
  source_params <- base_fit$estimated_params
  transferable <- intersect(names(inits), names(source_params))
  transferable <- transferable[vapply(
    transferable,
    function(parameter) {
      identical(dim(inits[[parameter]]), dim(source_params[[parameter]])) &&
        length(inits[[parameter]]) == length(source_params[[parameter]])
    },
    logical(1)
  )]
  for (parameter in transferable) inits[[parameter]] <- source_params[[parameter]]

  dsem_spec <- Rceattle::build_DSEM(
    sem = paste(paths, collapse = "\n"),
    family = dsem::gaussian_fixed_sd("identity", 0.1),
    sigmaR_prior_sd = 0.5,
    estimate_projection = FALSE
  )

  started <- Sys.time()
  model <- Rceattle::fit_mod(
    data_list = est,
    inits = inits,
    file = NULL,
    estimateMode = 0,
    random_rec = TRUE,
    msmMode = 0,
    initMode = "NonEquilibrium",
    M1Fun = Rceattle::build_M1(updateM1 = TRUE, M1_model = "fixed"),
    dsem = dsem_spec,
    fit_control = Rceattle::fit_control(
      verbose = 1,
      phase = TRUE,
      bias_adjust_proc = FALSE,
      bias_adjust_obs = FALSE,
      comp_offset = 1e-3
    )
  )
  attr(model, "model_26_dsem_run") <- list(
    name = name,
    sem = paste(paths, collapse = "\n"),
    elapsed_seconds = as.numeric(difftime(Sys.time(), started, units = "secs"))
  )
  saveRDS(model, checkpoint)
  model
}

fits <- lapply(names(specifications), function(name) {
  message("Fitting DSEM configuration: ", name)
  fit_one(name, specifications[[name]])
})
names(fits) <- names(specifications)

fit_summary <- bind_rows(lapply(names(fits), function(name) {
  model <- fits[[name]]
  run <- attr(model, "model_26_dsem_run")
  data.frame(
    model = name,
    Model = unname(model_labels[name]),
    objective = model$opt$objective,
    AIC = AIC(model),
    maximum_gradient = model$opt$max_gradient,
    positive_definite_hessian = isTRUE(model$sdrep$pdHess),
    convergence_status = model$convergence$status,
    elapsed_seconds = run$elapsed_seconds
  )
})) |>
  mutate(delta_AIC = AIC - min(AIC))
write.csv(
  fit_summary,
  file.path(output_directory, "dsem_model_summary.csv"),
  row.names = FALSE
)

path_table <- bind_rows(lapply(names(fits), function(name) {
  model <- fits[[name]]
  paths <- as.data.frame(summary(model$dsem))
  fixed <- as.data.frame(summary(model$sdrep, "fixed"))
  beta <- fixed[grepl("^dsem_beta_z", rownames(fixed)), , drop = FALSE]
  stopifnot(nrow(paths) == nrow(beta))
  data.frame(
    model = name,
    Model = unname(model_labels[name]),
    path = paths$path,
    lag = paths$lag,
    parameter = paths$name,
    estimate = beta$Estimate,
    standard_error = beta$`Std. Error`,
    lower_95 = beta$Estimate - 1.96 * beta$`Std. Error`,
    upper_95 = beta$Estimate + 1.96 * beta$`Std. Error`
  )
}))
write.csv(
  path_table,
  file.path(output_directory, "dsem_path_estimates.csv"),
  row.names = FALSE
)

recruitment_scale <- path_table[path_table$parameter == "sigmaR1", ]
iid_variance <- recruitment_scale$estimate[recruitment_scale$model == "iid"]^2
recruitment_scale$unexplained_variance <- recruitment_scale$estimate^2
recruitment_scale$variance_reduction_from_iid <-
  1 - recruitment_scale$unexplained_variance / iid_variance
write.csv(
  recruitment_scale,
  file.path(output_directory, "dsem_recruitment_variance.csv"),
  row.names = FALSE
)

write.csv(
  data.frame(
    Rceattle_branch = "dsem-v5-integration",
    Rceattle_commit = expected_commit,
    Rceattle_version = as.character(packageVersion("Rceattle")),
    dsem_version = as.character(packageVersion("dsem")),
    Cold_pool_observations = sum(is.finite(est$env_data$ColdPool)),
    Late_summer_SST_observations = sum(is.finite(est$env_data$LateSummerSST)),
    Selectivity_sensitivity = "Simple assessment fit; 2D age-by-year AR1 excluded"
  ),
  file.path(output_directory, "dsem_run_design.csv"),
  row.names = FALSE
)

aic_figure <- ggplot(fit_summary, aes(x = reorder(Model, AIC), y = delta_AIC)) +
  geom_col(fill = "#176b87", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f", delta_AIC)), hjust = -0.15) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
  labs(x = NULL, y = expression(Delta * "AIC")) +
  ggthemes::theme_few()
ggsave(
  file.path(output_directory, "dsem_aic_comparison.png"),
  aic_figure,
  width = 7.5,
  height = 4.8,
  dpi = 180
)

edge_coefficients <- path_table |>
  filter(grepl("_to_R$", parameter)) |>
  mutate(
    Covariate = recode(
      parameter,
      ColdPool_to_R = "Cold pool",
      LateSummerSST_to_R = "Age-0 late-summer SST"
    )
  )
edge_figure <- ggplot(
  edge_coefficients,
  aes(x = estimate, y = Covariate, color = Model)
) +
  geom_vline(xintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(
    aes(xmin = lower_95, xmax = upper_95),
    orientation = "y",
    width = 0,
    linewidth = 0.7,
    position = position_dodge(width = 0.45)
  ) +
  geom_point(size = 2.5, position = position_dodge(width = 0.45)) +
  labs(x = "Standardized coefficient (95% interval)", y = NULL, color = "DSEM model") +
  ggthemes::theme_few() +
  theme(legend.position = "bottom")
ggsave(
  file.path(output_directory, "dsem_recruitment_paths.png"),
  edge_figure,
  width = 9,
  height = 5.2,
  dpi = 180
)

capture.output(
  sessionInfo(),
  file = file.path(output_directory, "dsem_session_info.txt")
)

print(fit_summary, row.names = FALSE)
print(edge_coefficients, row.names = FALSE)
print(recruitment_scale, row.names = FALSE)
