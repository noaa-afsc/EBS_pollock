compute_selectivity_fsh <- function(nsel, stsel, endyr_r, nages, coffs, sel_devs, yrs_ch_fsh) {
  # nsel: number of selectivity ages
  # stsel: starting year (integer)
  # endyr_r: ending year (integer)
  # nages: number of ages
  # avgsel: will be set inside function (pass as 0 or NA)
  # coffs: vector of selectivity coefficients (length nsel)
  # sel_devs: vector of selectivity deviations (length nch_fsh)
  # nch_fsh: number of change points (integer)
  # yrs_ch_fsh: vector of years where selectivity changes (length nch_fsh)
  
  # Pre-allocate log_sel matrix: rows = years, cols = ages
  "c" <- ADoverload("c")
  "[<-" <- ADoverload("[<-")
  nyrs <- endyr_r - stsel + 1
  log_sel <- matrix(0, nrow = nyrs, ncol = nages)
  rownames(log_sel) <- as.character(stsel:endyr_r)
  # 1. Set avgsel
  avgsel <- log(mean(exp(coffs)))
  # 2. Set first year selectivity
  log_sel[1, 1:nsel] <- coffs
  log_sel[1, (nsel + 1):nages] <- coffs[nsel]
  # 3. Center first year
  log_sel[1, ] <- log_sel[1, ] - log(mean(exp(log_sel[1, ])))
  ii <- 1
  nch_fsh <- dim(sel_devs)[1]
  for (i in 1:(nyrs - 1)) { # i is index for year, not actual year
    year <- stsel + i - 1
    if (ii <= nch_fsh) {
      if (year == yrs_ch_fsh[ii]) {
        # Apply deviation
        log_sel[i + 1, 1:nsel] <- log_sel[i, 1:nsel] + sel_devs[ii, ]
        log_sel[i + 1, (nsel + 1):nages] <- log_sel[i + 1, nsel]
        # print(log_sel[i+1,])
        ii <- ii + 1
      } else {
        log_sel[i + 1, ] <- log_sel[i, ]
      }
    } else {
      log_sel[i + 1, ] <- log_sel[i, ]
    }
    # Center
    log_sel[i + 1, ] <- log_sel[i + 1, ] - log(mean(exp(log_sel[i + 1, ])))
  }
  # log_sel
  return(list(avgsel = avgsel, log_sel = log_sel))
}

compute_selectivity_ind <- function(stsel, slp, a50, se, ae, age_vector, endyr_r) {
  # stsel = styr_bts;
  # slp = sel_slp_bts;
  # a50 = sel_a50_bts;
  # se = sel_slp_bts_dev;
  # ae = sel_a50_bts_dev;
  # age_vector = 1:nages;
  # endyr_r = endyr_r
  "c" <- ADoverload("c")
  "[<-" <- ADoverload("[<-")
  nages <- length(age_vector)
  nyrs <- endyr_r - stsel + 1
  log_sel <- matrix(0, nrow = nyrs, ncol = nages)
  rownames(log_sel) <- as.character(stsel:endyr_r)
  
  for (i in 1:nyrs) {
    # Time-varying slope and a50
    slp_i <- exp(se[i]) * slp
    a50_i <- a50 * exp(ae[i])
    
    log_sel[i, ] <- -log(1 + exp(-slp_i * (age_vector - a50_i)))
    # log_sel(i)  =  -log( 1.0 + exp(-exp(se(i)) * slp * ( age_vector - a50*exp(ae(i)) )  ))  ;
  }
  return(log_sel)
}
# exp(log_sel)
# # stsel=1994
# compute_selectivity_ats_devs <- function(nsel, stsel, endyr_r, coffs, sel_devs) {
#   nyrs <- endyr_r - stsel + 1
#   log_sel <- matrix(0, nrow = nyrs, ncol = nages)
#   rownames(log_sel) <- as.character(stsel:endyr_r)
#   dim_sel_ats <- dim(sel_devs)[1]
#   # 1. Compute avgsel (on log scale)
#   avgsel <- log(mean(exp(coffs)))
#   # 2. Set selectivity in first year
#   log_sel[1, 2:nsel] <- coffs
#   log_sel[1, (nsel + 1):nages] <- coffs[nsel - 1]
#   # print(log_sel)
#   # 3. Mean-center on arithmetic scale
#   log_sel[1, ] <- log_sel[1, ] - log(mean(exp(log_sel[1, ])))
#   ii <- 1
#   yrs_ch_ats <- 1995:2024
#   for (i in 2:nyrs) {
#     year <- stsel + i - 1
#     prev <- log_sel[i - 1, ]
#     if (year == yrs_ch_ats[ii]) {
#       # Apply deviations to selectivity coefficients
#       prev[2:nsel] <- prev[2:nsel] + sel_devs[ii, ]
#       prev[(nsel + 1):nages] <- prev[nsel]
#       if (ii <= dim_sel_ats){
#         ii <- ii + 1
#       }
#       # print(prev)
#     }else{
#
#     }
#
#     # Assign and center
#     log_sel[i, ] <- prev
#     log_sel[i, ] <- log_sel[i, ] - log(mean(exp(log_sel[i, ])))
#   }
#   return(log_sel)
# }

compute_selectivity_ats_devs <- function(nsel, nages, stsel, endyr_r, coffs, sel_devs,
                                         mina_ats, yrs_ch_ats) {
  dim_sel_ats <- dim(sel_devs)[1]
  # Initialize log selectivity matrix: years x ages
  log_sel <- matrix(0,
                    nrow = endyr_r - stsel + 1, ncol = nages,
                    dimnames = list(stsel:endyr_r, 1:nages)
  )
  "c" <- ADoverload("c")
  "[<-" <- ADoverload("[<-")
  
  # Calculate average selectivity from coffs
  avgsel <- log(mean(exp(coffs)))
  
  # Insert baseline selectivity for first year (stsel)
  log_sel[as.character(stsel), mina_ats:nsel] <- coffs
  log_sel[as.character(stsel), (nsel + 1):nages] <- log_sel[as.character(stsel), nsel] #<- coffs
  
  # Normalize selectivity for stsel
  log_sel[as.character(stsel), ] <- log_sel[as.character(stsel), ] -
    log(mean(exp(log_sel[as.character(stsel), ])))
  
  # Apply year-specific deviations
  ii <- 1
  yr <- stsel + 1
  for (yr in (stsel + 1):endyr_r) {
    ychar <- as.character(yr)
    yprev <- as.character(yr - 1)
    
    if (ii <= length(yrs_ch_ats) && yr == yrs_ch_ats[ii]) {
      # Apply deviation to first nsel ages
      log_sel[ychar, mina_ats:nsel] <- log_sel[yprev, mina_ats:nsel] + sel_devs[ii, ]
      # Extend last sel value to older ages
      log_sel[ychar, (nsel + 1):nages] <- log_sel[ychar, nsel]
      
      if (ii < dim_sel_ats) {
        ii <- ii + 1
      }
    } else {
      # Carry forward previous year's selectivity
      log_sel[ychar, ] <- log_sel[yprev, ]
    }
    
    # Normalize to ensure mean exp(log_sel) = 1
    log_sel[ychar, ] <- log_sel[ychar, ] - log(mean(exp(log_sel[ychar, ])))
  }
  
  return(log_sel)
}
SRecruit <- function(Stmp, phizero, alpha, Bzero) {
  RecTmp <- (Stmp / phizero) * exp(alpha * (1 - Stmp / Bzero))
  return(RecTmp)
}