library(ebswp)
library(tidyverse)
library(janitor)
library(here)

#---Read in full 2024 dataset
#df<- read_dat(here("2024","runs","data","pm_24_tune.dat") )
df<- read_dat(here("2024","runs","data","pm_24.dat") )
#--decrement bts data by one year
n <-  df$n_bts <- df$n_bts - 1
df$yrs_bts_data <-  df$yrs_bts_data[1:n]
df$sam_bts <-  df$sam_bts[1:n]
df$err_bts <-  df$err_bts[1:n]
df$ob_bts <-  df$ob_bts[1:n]
df$ob_bts_std <-  df$ob_bts_std[1:n]
df$ot_bts <-  df$ot_bts[1:n]
df$bottom_temp <-  df$bottom_temp[1:n]
df$oac_bts <- df$oac_bts[1:n,]
df$wt_bts <- df$wt_bts[1:n,]
write_dat( here("2024","runs","data","pm_24.1.dat") ,df)
n <-  df$n_ats <- df$n_ats - 1
df$yrs_ats_data <-  df$yrs_ats_data[1:n]
df$sam_ats <-  df$sam_ats[1:n]
df$err_ats <-  df$err_ats[1:n]
df$ob_ats <-  df$ob_ats[1:n]
df$ob_ats_std <-  df$ob_ats_std[1:n]
df$ot_ats <-  df$ot_ats[1:n]
df$ot_ats_std <-  df$ot_ats_std[1:n]
df$oac_ats <- df$oac_ats[1:n,]
df$wt_ats <- df$wt_ats[1:n,]
write_dat( here("2024","runs","data","pm_24.2.dat") ,df)
n <-  df$n_avo <- df$n_avo - 1
df$yrs_avo <-  df$yrs_avo[1:n]
df$ob_avo <-  df$ob_avo[1:n]
df$ob_avo_std <-  df$ob_avo_std[1:n]
df$wt_avo <- df$wt_avo[1:n,]
write_dat( here("2024","runs","data","pm_24.3.dat") ,df)
