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


# Read in data ----
#bsp0 <- Rceattle::read_data( file = here::here("runs", "ceattle", "bsp0.xlsx"))


# - Fit single-species models
#mf0 <- fit_mod(data_list = bsp0,
               #inits = NULL, # Initial parameters = 0
               #file = NULL, # Don't save
               #estimateMode = 0, # Estimate
               #random_rec = FALSE, # No random recruitment
               #msmMode = 0, # Single species mode
               #verbose = 2,
               #phase = TRUE,
               #initMode = 2)
#

# Code to run the bering sea pollock model in CEATTLE
# model is a single sex, single-species model

# DATA
# - Fishery catch
# - Fishery age composition
# - Fishery weight-at-age
# - Surveys
# -- Bottom trawl (random walk-logistic for age > 1, normal deviates for age = 1), additional penalty on selectivity
# -- AT (age-1 is an index, age > 1 have selectivity smoother)
# - Bottom temperature
# - Survey age composition
# - Catch-at-age methodology
# - Annual length-at-age and weight-at-age from surveys
# - Age at maturity

# MODEL
# - One sex
# - Ricker recruitment (1978-2017) w/ prior on steepness
# - Empirical weight-at-age
# - M = 0.3 for females, estimated for males

# Load data ----
#ebs_pollock <- Rceattle::read_data( file = "Data/EBS_2024_pollock_single_species.xlsx")
#fn = "bsp00.xlsx"
#rand_sel=TRUE

#library(Rceattle) # https://github.com/grantdadams/Rceattle/tree/dev-name-change
#fn = "bsp0.xlsx"; rand_rec=FALSE; rand_sel=FALSE; sigSel=FALSE; sigR=FALSE
#fn = "bsp0.xlsx"; rand_rec=FALSE; rand_sel=TRUE; sigSel=TRUE; sigR=FALSE
#fn = "bsp0.xlsx"; rand_rec=FALSE; rand_sel=FALSE; sigSel=FALSE; sigR=FALSE

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
wham::check_estimability(fm0$obj)
fm0_re1 <- Fit_bsp(fn = "bsp0.xlsx", rand_sel=FALSE, rand_rec = TRUE)
wham::check_estimability(fm0_re1$obj)
fm0_re2 <- Fit_bsp(fn = "bsp0.xlsx", rand_sel=TRUE, rand_rec = FALSE)
wham::check_estimability(fm0_re2$obj)
fm0_re3 <- Fit_bsp(fn = "bsp0.xlsx", rand_sel=TRUE, rand_rec = TRUE)
wham::check_estimability(fm0_re3$obj)

cbind((fm0_re3$obj$par), (fm0_re3$obj$gr()) ) #$gra
summary(fm0_re1$obj$gr()) #$gra
summary(fm0_re2$obj$gr()) #$gra
summary(fm0_re3$obj$gr()) #$gra
summary(fm0_re4$obj$gr()) #$gra

fm0_re4 <- Fit_bsp(fn = "bsp0.xlsx", rand_sel=TRUE, rand_rec = TRUE,
                   sigR=TRUE, sigSel=TRUE)
wham::check_estimability(fm0_re4$obj)



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
