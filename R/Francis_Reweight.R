library(ebswp)
library(tidyverse)
library(janitor)
library(here)

#---Read in reportfile to get FW etc
#mod_names <- c("base")
#mod_dir <- c("lastyrdbae")
#M <- get_results(rundir = "runs")
setwd(here::here())
M <- read_rep(here::here("runs","lastyrdbae","pm.rep") )
M$FW_ats; M$FW_bts; M$FW_fsh; M$FW_fsh1; M$FW_fsh2; M$FW_fsh3; df$sam_ats; M$sdnr_ats; M$sdnr_bts; M$sdnr_avo;
#---Read in full 2024 dataset
df<- ebswp::read_dat(here::here("runs","data","pm_24_db.dat") )
#--Change input samplesize
update_ISS <- function(df, M, rundir) {
  df$sam_ats <- df$sam_ats*M$FW_ats
  df$sam_bts <- df$sam_bts*M$FW_bts
  df$sam_fsh <- c(M$FW_fsh1*df$sam_fsh[1:14] , M$FW_fsh2*df$sam_fsh[15:27], M$FW_fsh3*df$sam_fsh[28:60])
  ebswp::write_dat( here::here("runs","data","pm_24_tune.dat") ,df)
  
  rn        <- paste0("cd ",rundir,"; pm -nox -iprint 500 -nohess")
  system(rn)
}

for (i in 1:4){
  M <- read_rep(here::here("runs","rewt","pm.rep") )
  df<- ebswp::read_dat(here::here("runs","data","pm_24_tune.dat") )
  update_ISS(df=df, M=M, rundir = here::here("runs","rewt") )
}

