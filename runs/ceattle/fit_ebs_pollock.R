# Code to fit 2024 GOA Pollock model in Rceattle

# Install dependencies ----
#install.packages("pacman")
#install.packages("TMB", type = "source")
#install.packages("Matrix", type = "source")
pacman::p_load(dplyr, ggplot2, MASS, oce, readxl, TMB, devtools, writexl, reshape2, gplots, tidyr,
               testthat, foreach, R.utils, knitr, doParallel)
#devtools::install_github("kaskr/TMB_contrib_R/TMBhelper",force=TRUE)
#remotes::install_github("grantdadams/Rceattle", ref = "dev",force=TRUE) # dev_srr branch is most up to date
# Load libraries ----
library(Rceattle)
library(readxl)
library(dplyr)


Fit_bsp <- function (fn = "bsp0.xlsx", rand_rec=FALSE, rand_sel=FALSE, 
                     sigR=FALSE, sigSel=FALSE, verbose=1) {
  bsp <- Rceattle::read_data( file = here::here("runs","ceattle",fn) )
  bsp$estDynamics = 0
  bsp$index_data$Log_sd <- bsp$index_data$Log_sd/bsp$index_data$Observation
  bsp$catch_data$Catch <- bsp$catch_data$Catch*1000
  bsp$catch_data$Log_sd <- 0.05
  bsp$initMode = 1
  bsp$M1_model = 0
  bsp$srr_fun = 0
  bsp$srr_pred_fun = 0
  bsp$srr_est_mode = 0
  bsp$estDynamics = 0
  bsp$msmMode = 0
  bsp$srr_indices = 1
  bsp$Diet_comp_weights = 1
  bsp$M1_re = 0
  bsp$fleet_control$Fleet_type[5:6] <- 2 # Setting as survey
  #bsp$env_data 
  #names(bsp)
 pars<-build_params(bsp)
 #pars$beta_rec_pars
 mymap <- build_map(bsp, params = pars, debug = FALSE, random_rec = sigR, random_sel = sigSel)
 #mymap$mapList$beta_rec_pars 
  # - Fix M
  fit <- Rceattle::fit_mod(data_list = bsp,
                                    inits = pars, # Initial parameters = 0
                                    file = NULL, # Don't save
                                    estimateMode = 0, # Estimate
                                    map = mymap, # Estimate
                                    random_rec = rand_rec, # Random recruitment
                                    random_sel = rand_sel, # selectivity RE
                                    msmMode = 0, # Single species mode
                                    verbose = verbose,
                                    phase = TRUE,
                                    initMode = 2) # Unfished equilibrium with init_dev's turned on
  return(fit)
}

#fm00    <- Fit_bsp(fn = "bsp00.xlsx",rand_sel=FALSE, rand_rec = FALSE)
fm0     <- Fit_bsp(fn = "bsp0.xlsx", rand_sel=FALSE, rand_rec = FALSE)
tail(getFs(fm0) |> filter(year %in% 2010:2024))
#wham::check_estimability(fm0$obj)
fm0_re1 <- Fit_bsp(fn = "bsp0.xlsx", rand_sel=FALSE, rand_rec = TRUE)
wham::check_estimability(fm0_re1$obj)
fm0_re2 <- Fit_bsp(fn = "bsp0.xlsx", rand_sel=TRUE, rand_rec = FALSE)
wham::check_estimability(fm0_re2$obj)
fm0_re3 <- Fit_bsp(fn = "bsp0.xlsx", rand_sel=TRUE, rand_rec = TRUE)
wham::check_estimability(fm0_re3$obj)

fm0_re4 <- Fit_bsp(fn = "bsp0.xlsx", rand_sel=TRUE, rand_rec = TRUE,
                   sigR=TRUE, sigSel=TRUE)
wham::check_estimability(fm0_re4$obj)

cbind((fm0_re3$obj$par), (fm0_re3$obj$gr()) ) #$gra
cbind((fm0_re4$obj$par), (fm0_re4$obj$gr()) ) #$gra
summary(fm0_re1$obj$gr()) #$gra
summary(fm0_re2$obj$gr()) #$gra
summary(fm0_re3$obj$gr()) #$gra
summary(fm0_re4$obj$gr()) #$gra

wham::check_estimability(fm0_re4$obj)

library(dplyr)
library(tibble)

getFs <- function(fm0) {
  # 1. extract the age×year matrix for fleet = "Fishery", sex = "Sex combined"
  F_mat <- fm0$quantities$F_flt_age["Fishery", "Sex combined", , , drop = TRUE]
  #    dimnames(F_mat)[[1]] → ages (“Age1”,…)
  #    dimnames(F_mat)[[2]] → years (“1964”,…)
  
  # 2. transpose so rows = years, cols = ages, then wrap in a tibble
  df <- as_tibble(t(F_mat))
  # 3. name the age-columns
  colnames(df) <- dimnames(F_mat)[[1]]
  # 4. add a year column (as integer) up front
  df <- df %>%
    tibble::add_column(year = as.integer(dimnames(F_mat)[[2]]), .before = 1)
  
  # result: tibble with columns
  #    year | Age1 | Age2 | … | Age15
  # and one row per year
  df
}
write_csv(df, here::here("runs","ceattle","Fishery_F.csv"))



##--Dirichlet multinomial runs---------------
fm0DM   <- Fit_bsp(fn = "bsp0DM.xlsx",rand_sel=FALSE, rand_rec = FALSE, verbose=2)
fm0DM_re<- Fit_bsp(fn = "bsp0DM.xlsx",rand_sel=TRUE, rand_rec = TRUE, verbose=1)
names(fm0DM)
names(fm0DM$opt)
names(fm0DM$opt$par)
names(fm0DM$quantities)
(fm0DM$initial_params)

names(fm0$sdrep$par.fixed)
names(fm0$sdrep$value)
names(fm0$sdrep)

(fm0$bounds$upper$sel_inf)
(fm0$bounds$lower$sel_inf)
names(fm0$quantities)
is.list(fm0$quantities)
str(fm0$quantities$F_flt_age)
names(fm0$quantities$sel)
dim(fm0$quantities$sel)
(fm0$sdrep$value)


# - Estimate age-invariant M
c(exp(fm0_re2$estimated_params$sel_dev_ln_sd[1]),exp(fm0_re2$estimated_params$R_ln_sd))
c(exp(fm4$estimated_params$sel_dev_ln_sd[1]),exp(fm4$estimated_params$R_ln_sd))
c(exp(fm5$estimated_params$sel_dev_ln_sd[1]),exp(fm5$estimated_params$R_ln_sd))


# - SAFE model
#FIXME: NEED to get from Jim

library(readxl)
SAFE2022_mod <- bridging_model_1
SAFE2022_mod$quantities$biomass[1,1:length(1954:2022)] <- read_excel("Data/2022_ADMB_estimate.xlsx", sheet = 4)$Est * 1000
SAFE2022_mod$quantities$biomassSSB[1,1:length(1954:2022)] <- read_excel("Data/2022_ADMB_estimate.xlsx", sheet = 3)$Est * 1000
SAFE2022_mod$quantities$R[1,1:length(1954:2022)] <- read_excel("Data/2022_ADMB_estimate.xlsx", sheet = 2)$Est * 1000


plot_biomass(list(bridging_model_3, SAFE2022_mod), model_names = c("CEATTLE", "SAFE")); mtext(side = 2, "Biomass", line = 1.8)
plot_ssb(list(bridging_model_3, SAFE2022_mod), model_names = c("CEATTLE", "SAFE")); mtext(side = 2, "SSB", line = 1.8)
plot_recruitment(list(bridging_model_3, SAFE2022_mod), model_names = c("CEATTLE", "SAFE")); mtext(side = 2, "Recruitment", line = 1.8)

dev.off()
plot_selectivity(pollock_base)
