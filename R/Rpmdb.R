rpm <- function(parms, data) {
  getAll(parms, data, warn = TRUE)
  ## Initialize joint negative log likelihood
  nll <- 0
  tmp <- compute_selectivity_fsh(
    nsel = n_selages_fsh,
    stsel = styr,
    endyr_r = endyr_r,
    nages = nages,
    coffs = sel_coffs_fsh,
    sel_devs = sel_devs_fsh,
    yrs_ch_fsh = 1965:2023
  )
  return( (log_avgrec^2))
}