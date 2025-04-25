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
names(bsp0)
?fit_mod

build_map(bsp0)
pars<-build_params(bsp0)

library(Rceattle) # https://github.com/grantdadams/Rceattle/tree/dev-name-change
Fit_bsp <- function (fn = "bsp0.xlsx", rand_rec=FALSE, rand_sel=FALSE) {
  bsp0 <- Rceattle::read_data( file = here::here("runs","ceattle",fn) )
  bsp0$estDynamics = 0
  bsp0$index_data$Log_sd <- bsp0$index_data$Log_sd/bsp0$index_data$Observation
  bsp0$catch_data$Catch <- bsp0$catch_data$Catch*1000
  bsp0$catch_data$Log_sd <- 0.05
  # - Fix M
  fit <- Rceattle::fit_mod(data_list = bsp0,
                                    inits = NULL, # Initial parameters = 0
                                    file = NULL, # Don't save
                                    estimateMode = 0, # Estimate
                                    random_rec = rand_rec, # Random recruitment
                                    random_sel = rand_sel, # selectivity RE
                                    msmMode = 0, # Single species mode
                                    verbose = 1,
                                    phase = TRUE,
                                    initMode = 2) # Unfished equilibrium with init_dev's turned on
  return(fit)
}
fm0DM<- Fit_bsp(fn = "bsp0DM.xlsx",rand_sel=FALSE)
fm0 <- Fit_bsp(fn = "bsp0.xlsx")
#,rand_sel=TRUE, rand_rec = TRUE)
fm1 <- Fit_bsp(fn = "bsp1.xlsx")
fm2 <- Fit_bsp(fn = "bsp2.xlsx")
(t(fm0$quantities$sel[1,1,,1:61]))[,1:8]
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
library(Rceattle)
fm3 <- fit_mod(data_list = bsp0,
                        inits = NULL,       # Initial parameters = 0
                        file = NULL,        # Don't save
                        estimateMode = 0,   # Estimate
                        random_rec = FALSE, # No random recruitment
                        random_sel = TRUE, # No random recruitment
                        msmMode = 0,        # Single species mode
                        verbose = 2,        # Minimal messages
                        initMode = 2,       # Unfished equilibrium with init_dev's turned on
                        phase = TRUE)       # Phase

fm4 <- fit_mod(data_list = bsp0,
                        inits = NULL,       # Initial parameters = 0
                        file = NULL,        # Don't save
                        estimateMode = 0,   # Estimate
                        random_rec = TRUE, # No random recruitment
                        random_sel = FALSE, # No random recruitment
                        msmMode = 0,        # Single species mode
                        verbose = 2,        # Minimal messages
                        initMode = 2,       # Unfished equilibrium with init_dev's turned on
                        phase = TRUE)       # Phase
fm5 <- fit_mod(data_list = bsp0,
                        inits = NULL,       # Initial parameters = 0
                        file = NULL,        # Don't save
                        estimateMode = 0,   # Estimate
                        random_rec = TRUE, # No random recruitment
                        random_sel = TRUE, # No random recruitment
                        msmMode = 0,        # Single species mode
                        verbose = 2,        # Minimal messages
                        initMode = 2,       # Unfished equilibrium with init_dev's turned on
                        phase = TRUE)       # Phase

#- Estimate age-invariant M and Ricker SRR
pollock_estM_ricker <- fit_mod(data_list = mydata_pollock,
                               inits = NULL,       # Initial parameters = 0
                               file = NULL,        # Don't save
                               estimateMode = 0,   # Estimate
                               random_rec = FALSE, # No random recruitment
                               msmMode = 0,        # Single species mode
                               verbose = 1,        # Minimal messages
                               M1Fun = build_M1(M1_model = 1), # Estimate age and time invariant M: see ?build_M1 for more details
                               recFun = build_srr(srr_fun = 0, # Default no-stock recruit curve
                                                  srr_pred_fun = 4, # Ricker curve as additional penalty (if srr_fun and srr_pred_fun are the same, no penalty is used)
                                                  srr_est_mode = 1, # Freely estimate alpha
                                                  srr_hat_styr = 1977, # Estimate starting 7 years after styr = 1970
                                                  srr_hat_endyr = 2020
                               ),
                               initMode = 2,       # Unfished equilibrium with init_dev's turned on
                               phase = TRUE)
c(exp(fm3$estimated_params$sel_dev_ln_sd[1]),exp(fm3$estimated_params$R_ln_sd))
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
