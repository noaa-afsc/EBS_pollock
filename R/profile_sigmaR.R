#---profile over sigmaR---------------------------------
library(ebswp)
library(tidyverse)

#---profile over sigmaR---------------------------------
ctl.orig <- read_dat(here::here("2024","runs","sigr","control.dat"))
ctl <- ctl.orig
ctl$phase_sigr
ctl$sigrprior
setwd(here::here("2024","runs","sigr"))
#length(seq(.85,1.05,0.02))
#for (i in  seq(.85,1.05,0.02)){
for (i in  seq(.53,.83,0.02)){
  ctl$sigrprior <- i
  write_dat(output_file="control.dat",ctl)
  system("./pm -iprint 300 ")
  file.remove(paste0("sigr_",i,".rep"))
  file.copy(from="pm.rep",to=paste0("sigr_",i,".rep"))
}
write_dat(output_file="control.dat",ctl.orig)

