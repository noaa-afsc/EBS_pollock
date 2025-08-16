wt_like <- 0.0
  wt_nll <- numeric(4)
  mnwt <- numeric(age_end)
  wt_inc <- numeric(age_end - 1)
  wt_pre <- matrix(0, nrow = endyr_wt - styr_wt + 1, ncol = age_end)
  wt_hat <- array(0, dim = c(ndat_wt, max(nyrs_data), age_end))
  # ... other arrays as needed
  for (j in age_st:age_end) {
    # mnwt[j] <<- alphawt * (L1 + (L2 - L1) * (1 - K^(j - age_st)) / (1 - K^(nages - 1)))^3
    mnwt[j] <- alphawt * (L1 + (L2 - L1) * (1 - K^(j - age_st)) / (1 - K^(nages - 1)))^3
  }

  # Calculate weight increments
  wt_inc <- mnwt[(age_st + 1):age_end] - mnwt[age_st:(age_end - 1)]

  # Initialize first year
  wt_pre[1, ] <- mnwt

  # Subsequent years
  for (i in 2:(endyr_wt - styr_wt + 1)) {
    # Youngest age class
    # wt_pre(i,age_st) = mnwt(age_st)   * exp(square(sigma_coh)/2.+sigma_coh*coh_eff(i));
    # print(c(i+styr_wt,coh_eff[i]))
    wt_pre[i, age_st] <- mnwt[age_st] * exp((sigma_coh^2) / 2 + sigma_coh * coh_eff[nages + i])
    # wt_pre(i)(age_st+1,age_end) = ++(wt_pre(i-1)(age_st,age_end-1) +
    wt_pre[i, (age_st + 1):age_end] <- wt_pre[i - 1, age_st:(age_end - 1)] +
      wt_inc * exp((sigma_yr^2) / 2 + sigma_yr * yr_eff[i])
    # wt_inc * exp(                sigma_yr * yr_eff[i])
    #   wt_inc * exp(square(sigma_yr)/2. + sigma_yr*yr_eff(i)));
  }
  # wt_pre
  for (h in 1:ndat_wt) {
    for (i in 1:nyrs_data[h]) {
      iyr <- yr_index[as.character(yrs_data[h, i])] # - styr_wt  # Adjust year index to start from 1
      if (h > 1) {
        wt_hat[h, i, ] <- c(0, 0, d_scale) * wt_pre[iyr, ]
      } else {
        # note 10 year diff between h=1 and h>1
        wt_hat[h, i, ] <- wt_pre[iyr, ]
      }
      for (j in age_st:age_end) {
        # Fit to global mean
        wt_nll[1] <- wt_nll[1] + (wt_obs[h, i, j - 2] - mnwt[j])^2 / (2 * sd_obs[h, i, j - 2]^2)
        # Fit to predicted values
        wt_nll[2] <- wt_nll[2] + (wt_obs[h, i, j - 2] - wt_hat[h, i, j])^2 /
          (2 * sd_obs[h, i, j - 2]^2)
        # print(wt_nll[2])
        # wt_nll(2) +=       square(wt_obs(h, i, j )  - wt_hat(h, i, j))   / (2.*square(sd_obs(h,i,j)));
      }
      # print(c(iyr+styr_wt,wt_like,wt_hat[h,i,1:10]))
    }
  }

  # wt_pre[20:22,] (cbind(wt_hat[1,,3:7], wt_obs[1,,1:5]))
  wt_nll[3] <- wt_nll[3] + 0.5 * sum(coh_eff^2) # norm2 equivalent
  wt_nll[4] <- wt_nll[4] + 0.5 * sum(yr_eff^2) # norm2 equivalent
  # 1 5568.18 # 2 732.671 # 3 20.6849 # 4 23.3254 dim(wt_pre) dim(wt_obs) dim(wt_hat) dim(sd_obs) (wt_obs[1,42,]) dim(wt_pre_hat) (wt_pre_hat[2,2,])

  # Add penalty terms
  wt_like <- sum(wt_nll[1:4])

