rm(list=ls())
source(here::here("R","utilities.R"))
#--Get the parameters (from ADMB converged model!)------------
parms <- read_pars(here::here("Rtmb", "pm.par"))

#--Get the data------------

dat <-  build_model_inputs(here::here("Rtmb","input.dat")) 

names(in1$constants)
in1 <- c(in1, in1$constants)
in1$constants <- NULL
in1 <- c(in1, in1$ctrl_flag)
in1$ctrl_flag <- NULL
names(in1)
str(in1)
in1$data
par1 <- make_parameters(in1) 


dat<-read_data(here::here("runs","data", "rpm.dat"))
pars <- read_pars(here::here("Rtmb","pm.par"))
str(pars)
names(pars)
?with(pars)
