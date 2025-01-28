library(ebswp)
library(tidyverse)
library(janitor)
library(here)

#---FSH data----
catage23 <- read_table("/Users/jim/_mymods/sampler/cases/ebswpSAM/results/catage2023.rep",col_names = FALSE)[,c(1,4:18)]
head(catage23)
tail(catage23)
names(catage23) <- c("bs",1:15)
c23_l <- pivot_longer(catage23, cols=2:16, names_to="age",values_to = "catch") |> mutate(age=as.numeric(age) ) 
head(c23_l  )
tail(c23_l  )
write_csv(c23_l,"/Users/jim/_mymods/sampler/cases/ebswpSAM/results/catage2023_long.csv")
summary(c23_l  )
c23_l |> 
  group_by(age) |>  summarise(Catch=mean(catch),sd=sd(catch), cv=sd/Catch)
glimpse(catage23)
  

# View the result
print(column_means)
glimpse(catage23)
summary_stats <- catage23 %>%
  summarise(across(4:18, 
                   list(mean = ~ mean(.x, na.rm = TRUE), 
                        sd = ~ sd(.x, na.rm = TRUE))))

# View the result
print(summary_stats)

head(catage23[,4:18])
rowMeans(catage23[,4:18])
# RE wt-age data
wtredf<- read_dat(here("2024","runs","data","wtage2024.dat") )
# RE wt-age report/results
wtdf<- read_rep("/Users/jim/_mymods/ebs_main/data/fishery/WtAgeRE/examples/ebspollock/wt24.rep")
wtdf

#---BTS data----
btsdf<- read_csv("/Users/jim/_mymods/ebs_main/data/bts/VAST_2024_Index.csv") |> filter(Stratum=="Both") |> 
  transmute(Year=Time,Est=Estimate/1e6, Std=`Std. Error for Estimate`/1e6) |> filter(Year~=2022)
names(btsdf) <- make_clean_names(names(btsdf))
(btsdf) |> ggplot(aes(x=year,y=est)) + geom_line() #+ geom_bw()
glimpse(btsdf)
df$ob_bts
df$ob_bts_std
tail(btsdf)

#---ATS data----
df$ob_ats
df$ob_ats_std
df$ob_ats_std/ df$ob_ats
# New ATS age data
ats_age_df<- read_dat(here("2024","runs","data","ats_2024_age.dat") )
ats_age_df$age_est
ats_age_df$wt_ats


df<- read_dat(here("2024","runs","data","pm_23.8.dat") )
df$endyr <- df$endyr+1
df$ew_ind <- cbind(df$ew_ind, c(4,4))
#df$ew_ind
df$wt_fsh <-  rbind(df$wt_fsh,
      df$wt_fsh[dim(df$wt_fsh)[1],])
df$wt_ssb <-  rbin(df$wt_ssb,
      df$wt_ssb[dim(df$wt_ssb)[1],])
df$obs_catch <- c(df$obs_catch, 1300)
df$obs_effort <- c(df$obs_effort, 4)
df$yrs_fsh_data <- c(df$yrs_fsh_data , 2023)
#df$yrs_fsh_data 
df$n_fsh <- df$n_fsh +1
df$sam_fsh <- c(df$sam_fsh , 666)
df$sam_fsh 
df$err_fsh <- c(df$err_fsh , 1)
df$err_bts <- c(df$err_bts , 1)
df$err_ats <- c(df$err_ats , 1)
df$oac_fsh_data <- rbind(df$oac_fsh_data, 1)

names(df)


write_dat( here("2024","runs","data","pm_24.1.dat") ,df)