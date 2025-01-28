#---profile over different selectivities by year---------------------------------
library(ebswp)
ctl.orig <- read_dat(here::here("2024","runs","selsens_sept","control.dat"))
ctl <- ctl.orig
setwd(here::here("2024","runs","selsens_sept"))
getwd()
for (i in 3:20) {
  ctl$nyrs_sel_avg <- -i
  write_dat(output_file="control.dat",ctl)
  system("./pm -iprint 300 -binp pm.bar -phase 22")
  file.remove(paste0("sel_",23-i,".rep"))
  file.copy(from="pm.rep",to=paste0("sel_",23-i,".rep"))
}
write_dat(output_file="control.dat",ctl.orig)
setwd(here::here())