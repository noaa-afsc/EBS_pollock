library(ebswp)
library(tidyverse)
library(janitor)
library(here)

#---Read in reportfile to get FW etc
mod_names <- c("m23")
mod_dir <- c("m23")
M <- get_results(rundir = "2024/runs")[[1]]
M$FW_ats
M$FW_bts
M$FW_fsh
M$FW_fsh1
M$FW_fsh2
M$FW_fsh3
M$sdnr_ats
M$sdnr_bts
M$sdnr_avo
#---Read in full 2024 dataset
df<- read_dat(here("2024","runs","data","pm_24.dat") )
#--Change input samplesize
update_ISS <- function(df, M) {
  df$sam_ats <- df$sam_ats*M$FW_ats
  df$sam_bts <- df$sam_bts*M$FW_bts
  write_dat( here("2024","runs","data","pm_24_tune.dat") ,df)
  run_model(rundir="2024/runs/",moddir="tune")
}


# Set dir name
# mod_dir <- c("tune")
mod_names <- c("tuned")
df<- read_dat(here("2024","runs","data","pm_24_tune.dat") )
Mt <- get_results(rundir = "2024/runs")[[1]]
update_ISS(df,M=Mt)
Mt <- get_results(rundir = "2024/runs")[[1]]

Mt$FW_bts 
Mt$FW_ats 
Mt$sdnr_ats
Mt$sdnr_bts
Mt$sdnr_avo
