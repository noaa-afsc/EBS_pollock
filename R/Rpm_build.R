# ----------------------------------------------------------------------------
# Function-based build wrapper for RTMB model components
# Saves side-effects and makes unit testing easier.
# ----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(RTMB)
  library(here)
})

#' Build RTMB inputs and preliminaries for the pm model
#'
#' @param par_path Path to ADMB-style parameter file (e.g., here('Rtmb','pm.par'))
#' @param dat_in   Path to ADMB-style data file (e.g., here('Rtmb','input.dat'))
#' @param rpm_out  Optional path for RTMB-friendly data (e.g., here('Rtmb','rpm.dat'))
#' @param lastyr_rep Optional rep file for comparisons (path or NULL)
#' @return list(data=dat, parameters=parms, prelim=prel_vars)
#' @examples
#' out <- rpm_build(par_path=here('Rtmb','pm.par'),
#'                  dat_in=here('Rtmb','input.dat'),
#'                  rpm_out=here('Rtmb','rpm.dat'))
rpm_build <- function(par_path,
                      dat_in,
                      rpm_out = here::here('Rtmb','rpm.dat'),
                      lastyr_rep = NULL) {

  # --- IO -------------------------------------------------------------------
  if (!file.exists(par_path)) stop('par_path not found: ', par_path)
  if (!file.exists(dat_in))   stop('dat_in not found: ', dat_in)

  # Optional last-year rep
  if (!is.null(lastyr_rep) && file.exists(lastyr_rep)) {
    if (requireNamespace('ebswp', quietly = TRUE)) {
      pm_prev <- ebswp::read_rep(lastyr_rep)
    } else {
      warning('Package ebswp not available; skipping lastyr rep read.')
      pm_prev <- NULL
    }
  } else pm_prev <- NULL

  # Utilities expected in your repo
  util_path <- here::here('R','utilities.R')
  if (file.exists(util_path)) source(util_path) else warning('utilities.R not found: ', util_path)

  # --- Read parameters and build data --------------------------------------
  parms <- read_pars(par_path)  # from your utilities
  dat   <- build_model_inputs(dat_in, fn = rpm_out)

  # Spawn timing
  dat$spawnmo <- 4.
  dat$yrfrac  <- (dat$spawnmo - 1.) / 12

  # BTS covariance inverse (safe)
  if (!is.null(dat$cov_matrix)) {
    inv_try <- try({ L <- chol(dat$cov_matrix); chol2inv(L) }, silent = TRUE)
    if (inherits(inv_try, 'try-error')) {
      warning('Cholesky failed; falling back to solve(); consider regularization.')
      dat$inv_bts_cov <- solve(dat$cov_matrix)
    } else dat$inv_bts_cov <- inv_try
  }

  # Observation variances
  if (!is.null(dat$obs_cpue_std)) dat$obs_cpue_var <- dat$obs_cpue_std^2
  if (!is.null(dat$obs_avo_std))  dat$obs_avo_var  <- dat$obs_avo_std^2

  # --- Preliminary calcs (ADMB PRELIMINARY_CALCS_SECTION) ------------------
  prel_vars <- preliminary_calcs(data = dat, parameters = parms)
  getAll(prel_vars$data, prel_vars$parameters, warn = TRUE)  # expose to caller if sourced
  # NOTE: Prefer returning structured objects instead of relying on getAll()

  # --- TODO: Selectivity ----------------------------------------------------
  # Example: fishery selectivity with deviations; ensure normalization order matches ADMB
  # tmp <- compute_selectivity_fsh(nsel = n_selages_fsh, stsel = styr, endyr_r = endyr_r,
  #                                nages = nages, coffs = sel_coffs_fsh, sel_devs = sel_devs_fsh,
  #                                yrs_ch_fsh = 1965:2023)
  # log_sel_fsh <- tmp$log_sel; avgsel_fsh <- tmp$avgsel

  # Return structured output for tests/use
  list(data = dat, parameters = parms, prelim = prel_vars, lastyr = pm_prev)
}
