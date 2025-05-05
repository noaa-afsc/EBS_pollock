# some loo runs using ebswp

library(ebswp)
library(tidyverse) #need read_table from somewhere in tidyverse

dat_base<-read_dat(here::here("runs/data/pm_24.dat"))

#--------------------------------------------
# Leave out whole BTS
dat_loo_bts<-dat_base
dat_loo_bts$ob_bts_std<-sqrt(10000*(dat_base$ob_bts_std^2))
write_dat(output_file = here::here("runs/data/pm_24_loo_bts_index.dat"),indata = dat_loo_bts)

#Create directory and copy in key files
dir.create(here::here("runs/loo_bts_all"))
file.copy(here::here("runs/lastyr/control.dat"),here::here("runs/loo_bts_all/"),overwrite = TRUE)
file.copy(here::here("runs/lastyr/compweights.ctl"),here::here("runs/loo_bts_all/"),overwrite = TRUE)
#Read pm.dat then write
pm.dat<-read_table(here::here("runs/lastyr/pm.dat"),col_names = FALSE)
pm.dat[1,]<-"Increased_bts_index_std"
pm.dat[2,]<-"../data/pm_24_loo_bts_index.dat"
writeLines(as.character(pm.dat$X1),here::here("runs/loo_bts_all/pm.dat"))

#change the control file for lambda for the comp data
ctl_base<-read_ctl(here::here("runs/lastyr/control.dat"))
ctl_loo_bts<-ctl_base
ctl_loo_bts$`#ctrl_flag`[8,]<- 0.1
write_dat(output_file = here::here("runs/loo_bts_all/control.dat"),indata = ctl_loo_bts)

#-----------------------------------------------
# Leave out whole ATS
dat_loo_ats<-dat_base
dat_loo_ats$ob_ats_std<-sqrt(10000*(dat_base$ob_ats_std^2))
write_dat(output_file = here::here("runs/data/pm_24_loo_ats_index.dat"),indata = dat_loo_ats)

#Create directory and copy in key files
dir.create(here::here("runs/loo_ats_all"))
file.copy(here::here("runs/lastyr/control.dat"),here::here("runs/loo_ats_all"),overwrite = TRUE)
file.copy(here::here("runs/lastyr/compweights.ctl"),here::here("runs/loo_ats_all"),overwrite = TRUE)

#read pm.dat then write
pm.dat<-read_table(here::here("runs/lastyr/pm.dat"),col_names = FALSE)
pm.dat[1,]<-"Increased_ats_index_std"
pm.dat[2,]<-"../data/pm_24_loo_ats_index.dat"
writeLines(as.character(pm.dat$X1),here::here("runs/loo_ats_all/pm.dat"))

#change the control file for lambda for the ats comp data
ctl_base<-read_ctl(here::here("runs/lastyr/control.dat"))
ctl_loo_ats<-ctl_base
ctl_loo_ats$`#ctrl_flag`[9,]<- 0.1
ctl_loo_ats$'#age1_sigma_ats'[1]<-10000
write_dat(output_file = here::here("runs/loo_ats_all/control.dat"),indata = ctl_loo_ats)


#-------------------------------------------------
#Leave out AVO index
#Create directory and copy in key files
dir.create(here::here("runs/loo_avo"))
file.copy(here::here("runs/lastyr/control.dat"),here::here("runs/loo_avo/"),overwrite = TRUE)
file.copy(here::here("runs/lastyr/compweights.ctl"),here::here("runs/loo_avo/"),overwrite = TRUE)
file.copy(here::here("runs/lastyr/pm.dat"),here::here("runs/loo_avo"),overwrite=TRUE)
#change the control file for lambda for the comp data
ctl_base<-read_ctl(here::here("runs/lastyr/control.dat"))
ctl_loo_avo<-ctl_base
ctl_loo_avo$`#ctrl_flag`[6,]<- 0.001
write_dat(output_file = here::here("runs/loo_avo/control.dat"),indata = ctl_loo_avo)

#----------------------------------------------------


