
# View(comp)  # for interactive viewing in RStudio

# Optionally save to CSV
# write.csv(comp, "comparison_table.csv", row.names = FALSE)
# 

rtmb <- list(
  N = natage,                               # from GetNumbersAtAge()
  Z = Z,                                    # from Get_Mortality_Rates()
  F = F,                                    # from Get_Mortality_Rates()
  M = M,                                    # from Get_Mortality_Rates()
  S = exp(-Z),                              # or from Get_Mortality_Rates()
  C = F / Z * (1 - exp(-Z)) * natage,       # Baranov catch
  pred_catch = rowSums(F / Z * (1 - exp(-Z)) * natage),
  obs_catch = obs_catch,               # if available
  SSB = SSB,                                # computed inside GetNumbersAtAge()
  phizero = phizero,                  # from Get_Bzero()
  Bzero = Bzero,                      # from Get_Bzero()
  steepness = steepness,              # input parameter
  sel_fsh = sel_fsh,                        # selectivity-at-age
  sel_bts = exp(log_sel_bts),
  sel_ats = exp(log_sel_ats)
  # rec_like = rec_like_contribs,             # vector of contributions
  # tot_like = total_likelihood,
  # SRR_SSB = ssb_t,                          # SSB time series
  # rechat = recr_pred,                       # predicted recruitment
  # SR_resids = log(recr_obs) - log(recr_pred)
)
pm1$sel_fsh
compare_row_means <- function(pm1, rtmb, variables = 
                                c("sel_fsh", "N", "F", "M", "Z", "S", "C")) {
  results <- lapply(variables, function(vname) {
    x <- pm1[[vname]]
    y <- rtmb[[vname]]
    
    if (!all(dim(x) == dim(y))) {
      warning(sprintf("Dimension mismatch in %s", vname))
      return(NULL)
    }
    
    mean_x <- rowMeans(x)
    mean_y <- rowMeans(y)
    diff <- mean_x - mean_y
    
    data.frame(
      year = seq_along(mean_x),
      mean_pm1 = mean_x,
      mean_rtmb = mean_y,
      abs_diff = abs(diff),
      rel_diff = abs(diff) / pmax(abs(mean_x), 1e-8),
      variable = vname
    )
  })
  
  do.call(rbind, results)
}

compare_models <- function(pm1, rtmb) {
  vars_to_check <- c("N", "F", "M", "Z", "S", "C","sel_fsh", "sel_bts", "sel_ats")
  
  compare_matrix <- function(name) {
    diff <- pm1[[name]] - rtmb[[name]]
    max_diff <- max(abs(diff))
    if (max_diff > 1e-6) {
      cat(sprintf("⚠️  %s mismatch (max diff = %.3g)\n", name, max_diff))
    } else {
      cat(sprintf("✅ %s matches (max diff = %.3g)\n", name, max_diff))
    }
    invisible(max_diff)
  }
  
  # Compare core matrices
  sapply(vars_to_check, compare_matrix)
  
  # Compare vectors/scalars
  scalar_diffs <- list(
    Bzero      = abs(pm1$Bzero - rtmb$Bzero),
     phizero    = abs(pm1$phizero - rtmb$phizero)
  #   pred_catch = max(abs(pm1$pred_catch - rtmb$pred_catch)),
  #   SSB        = max(abs(pm1$SRR_SSB - rtmb$SSB))
   )
  # 
  for (name in names(scalar_diffs)) {
    if (scalar_diffs[[name]] > 1e-6) {
      cat(sprintf("⚠️  %s mismatch (diff = %.3g)\n", name, scalar_diffs[[name]]))
    } else {
      cat(sprintf("✅ %s matches (diff = %.3g)\n", name, scalar_diffs[[name]]))
    }
  }
}
image_diff <- function(a, b, title = "") {
  diff <- a - b
  image(diff, main = title)
}

image_diff(pm1$N, rtmb$N, "N difference")
image_diff(pm1$sel_fsh, rtmb$sel_fsh, "F difference")
# Assuming your new output is stored in a list `rtmb_out` matching structure
compare_models(pm1, rtmb)
pm1$SRR_SSB

mean_comp <- compare_row_means(pm1, rtmb)
(mean_comp)
library(ggplot2)

ggplot(mean_comp, aes(x = year, y = abs_diff, color = variable)) +
  geom_line() +
  labs(title = "Absolute difference in mean by year", y = "Abs diff", x = "Year") +
  theme_minimal()

names(mean_comp)
library(dplyr)
plotly::ggplotly(mean_comp |> filter(variable != "N" & variable != "C") |>  ggplot(aes(x = year, y = rel_diff, color = variable)) +
  geom_line() +
  labs(title = "Relative difference in mean by year", y = "Relative diff", x = "Year") +
  theme_minimal() )
natage[2,11:15]
pm1$N[1,11:15]
pm1$sel_fsh[61,1:8]
sel_fsh[61,1:8]

#--Junk from here down------------------
final_yr <- as.character(endyr)
penult_yr <- as.character(endyr - 1)

natage[final_yr, 1] <- SRecruit(SSB[penult_yr]) * exp(log_rec_devs[final_yr])

SSB[final_yr] <- sum(natage[final_yr, ] * S[final_yr, ]^yrfrac * p_mature * wt_ssb[final_yr])
pred_rec[final_yr] <- natage[final_yr, 1]

# Mean recruitment from 1978 onward
pred_years <- as.character(1978:endyr_r)
meanrec <- mean(pred_rec[pred_years])

# meannatage = (1 - S) / Z * natage  → elementwise division
Z <- -log(S)  # assuming S = exp(-Z) ⇒ Z = -log(S)
meannatage <- (1 - S) / Z * natage

return(list(
  natage = natage,
  SSB = SSB,
  pred_rec = pred_rec,
  meanrec = meanrec,
  meannatage = meannatage
))
}
# 1. Initialize log_sel for the first year
avgsel <- log(mean(exp(coffs)))

# Set first row (stsel)
log_sel[1, 1:nsel] <- coffs
log_sel[1, (nsel + 1):nages] <- coffs[nsel]

# Center the first year
log_sel[1, ] <- log_sel[1, ] - log(mean(exp(log_sel[1, ])))

ii <- 1
for (i in stsel:(endyr_r - 1)) {
  idx <- i - stsel + 1 # R row index for year i
  idx_next <- idx + 1 # R row index for year i+1
  
  if (ii <= nch_fsh) {
    if (i == yrs_ch_fsh[ii]) {
      # Apply selectivity deviation
      log_sel[idx_next, 1:nsel] <- log_sel[idx, 1:nsel] + sel_devs[ii]
      log_sel[idx_next, (nsel + 1):nages] <- log_sel[idx_next, nsel]
      ii <- ii + 1
    } else {
      log_sel[idx_next, ] <- log_sel[idx, ]
    }
  } else {
    log_sel[idx_next, ] <- log_sel[idx, ]
  }
  
  # Center the selectivity for year i+1
  log_sel[idx_next, ] <- log_sel[idx_next, ] - log(mean(exp(log_sel[idx_next, ])))
}


