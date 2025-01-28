## see
## https://github.com/fishfollower/compResidual#composition-residuals
## for installation instructions

# TMB:::install.contrib("https://github.com/vtrijoulet/OSA_multivariate_dists/archive/main.zip")
## devtools::install_github("fishfollower/compResidual/compResidual")
library(compResidual)
library(ggplot2)
library(cowplot)
library(reshape2)
library(dplyr)
library(tidyr)
theme_set(theme_bw())
library(here)
#source(here::here("tools", "plot_osa_comps.R"))
get_osa_out <- function(M=M,label="BTS") {
  o    <- M$pobs_bts[,2:16]
  p    <- M$phat_bts[,2:16]
  M$EffN_bts
  Neff <- M$sam_bts
  #Neff <- M$EffN_bts[,2]
  yrs  <- M$yr_bts
  age  <- 1:15
  pearson <- Neff*(o-p)/sqrt(p*Neff)
  years <- M$pobs_bts[,1]
  out <- afscOSA::run_osa(fleet = label, index_label = 'Age',
                           obs = o, exp = p, N = Neff, index = age, years = yrs)
  return(out)
}

out1 <- get_osa_out(M=M,label="BTS")
out2 <-get_osa_out(M=Fix,label="Fix")
input <- list(out1, out2)
osaplots <- plot_osa(input)


o    <- M$pobs_ats[,3:16]
p    <- M$phat_ats[,3:16]
Neff <- M$EffN_ats[,2]
Neff <- M$sam_ats
yrs  <- M$yr_ats
age  <- 2:15
pearson <- Neff*(o-p)/sqrt(p*Neff)
years <- M$pobs_ats[,1]
yrs
out3 <- afscOSA::run_osa(fleet = 'ATS', index_label = 'Age',
                         obs = o, exp = p, N = Neff, index = age, years = yrs)

o    <- M$pobs_fsh[29:60,2:16]
p    <- M$phat_fsh[29:60,2:16]
Neff <- M$sam_fsh[29:60]
yrs  <- 1992:2023
age  <- 1:15
pearson <- Neff*(o-p)/sqrt(p*Neff)
yrs
out1 <- afscOSA::run_osa(fleet = 'Fishery', index_label = 'Age',
                         obs = o, exp = p, N = Neff, index = age, years = yrs)
# o    <- M$pobs_fsh[,2:16]
# p    <- M$phat_fsh[,2:16]
# Neff <- M$sam_fsh[]
# yrs  <- 1964:2023
# age  <- 1:15

input <- list(out1, out2, out3)
osaplots <- plot_osa(input)
ggsave("doc/figs/mod_osa.png", width = 8.4, height = 8, units = "in")

osaplots$bubble
osaplots$qq
names(osaplots$qq)
(osaplots$qq$layers)




pak::pkg_install("r4ss/r4ss")
library(r4ss)

# other afscOSA package dependencies:
library(ggplot2)
library(cowplot)
library(here)
library(dplyr)
library(reshape2)
library(here)

library(afscOSA)

# create directory for analysis
# out_path <- "test_aicod"
if(!exists("out_path")) out_path = getwd()
if(!dir.exists(out_path)) dir.create(out_path)

# copy all data files to working directory
pkg_path <- find.package('afscOSA')
example_data_files <- list.files(path = file.path(pkg_path, "inst", "examples", "AI_PCOD"))
example_data_files
file.copy(from = file.path(path = file.path(pkg_path, "inst", "examples", "AI_PCOD"),
                           example_data_files),
          to = file.path(file.path(out_path), example_data_files),
          overwrite = TRUE)

setwd(out_path)

sx = 1 # USER INPUT define sex
fleet = c(1,2) # USER INPUT define fleets
model_path <- c("inst/examples/AI_PCOD")

mod <- r4ss::SSgetoutput(dirvec = model_path)

# comps for the fleets defined in "fleet" and "sx"
comps <- as.data.frame(mod[[1]]$lendbase[,c(1,6,13,16:18)])
comps <- comps[comps$Fleet %in% fleet & comps$Sex %in% sx, ]
comps <- reshape2::melt(comps,id.vars = c('Yr','Fleet','Sex','Bin'))

# effective sample sizes for the fleets defined in "fleet" and "sx"
Neffdf <- as.data.frame(mod[[1]]$lendbase[,c(1,6,13,16,22)])
Neffdf <- Neffdf[Neffdf$Bin == min(Neffdf$Bin),]

# length bins
lens <- sort(unique(comps$Bin))

# fishery (fleet 1) ----

flt <- 1 # USER INPUT

# this is a 1 sex model but if it had sex structure you would define each sex as
# a separate fleet (e.g., Fishery F and Fishery M)
tmp <- comps[comps$Fleet==flt,]

# effective sample sizes (vector)
Neff <- Neffdf$effN[Neffdf$Fleet==flt]

# observed values -> put in matrix format (nrow = nyr, ncol = age/length)
obs <- tmp[tmp$variable=='Obs',]
obs <- reshape2::dcast(obs, Yr~Bin, value.var = "value")
yrs <- obs$Yr # years sampled
obs <- as.matrix(obs[,-1])

# expected values -> put in matrix format (nrow = nyr, ncol = age/length
exp <- tmp[tmp$variable=='Exp',]
exp <- reshape2::dcast(exp, Yr~Bin, value.var = "value")
exp <- as.matrix(exp[,-1])

# should all be true!
length(Neff) == length(yrs); length(Neff) == nrow(obs); nrow(obs) == nrow(exp)
ncol(obs);ncol(exp);length(lens)

out1 <- afscOSA::run_osa(fleet = 'Fishery', index_label = 'Length',
                         obs = obs, exp = exp, N = Neff, index = lens, years = yrs)


# AI bottom trawl survey (fleet 2) ----

flt <- 2 # USER INPUT

# this is a 1 sex model but if it had sex structure you would define each sex as
# a separate fleet (e.g., Survey F and Survey M)
tmp <- comps[comps$Fleet==flt,]

# effective sample sizes (vector)
Neff <- Neffdf$effN[Neffdf$Fleet==flt]

# observed values -> put in matrix format (nrow = nyr, ncol = age/length)
obs <- tmp[tmp$variable=='Obs',]
obs <- reshape2::dcast(obs, Yr~Bin, value.var = "value")
yrs <- obs$Yr # years sampled
obs <- as.matrix(obs[,-1])

# expected values -> put in matrix format (nrow = nyr, ncol = age/length
exp <- tmp[tmp$variable=='Exp',]
exp <- reshape2::dcast(exp, Yr~Bin, value.var = "value")
exp <- as.matrix(exp[,-1])

# should all be true!
length(Neff) == length(yrs); length(Neff) == nrow(obs); nrow(obs) == nrow(exp)
ncol(obs);ncol(exp);length(lens)

Neff
out2 <- afscOSA::run_osa(fleet = 'AI Trawl Survey', index_label = 'Length',
                         obs = obs, exp = exp, N = Neff, index = lens, years = yrs)

# plot results ----
input <- list(out1, out2)
osaplots <- plot_osa(input)
osaplots$bubble
osaplots$qq
osaplots$aggcomp

===================
plot_osa_comps(o,p, pearson, index=ages, years=years, index_label='age', Neff=Neff,
               stock='EBSpollock', survey='tuned_ats', outpath='doc/figs')

# GOA RE/BS ----

#lengths
rebslens <- readRDS(here::here('data', 'goa_rebs_lencomps.RDS'))
for(i in 1:length(unique(rebslens$index))) {
  lens <- unique(rebslens$length)
  tmp <- rebslens %>% dplyr::filter(index == unique(rebslens$index)[i])
  o <- tmp %>% pivot_wider(id_cols = year, names_from = length, values_from = obs) %>% select(-year) %>% as.matrix()
  p <- tmp %>% pivot_wider(id_cols = year, names_from = length, values_from = pred) %>% select(-year) %>% as.matrix()
  pearson <-  tmp %>% pivot_wider(id_cols = year, names_from = length, values_from = pearson) %>% select(-year) %>% as.matrix()
  years <- tmp %>% distinct(year, n) %>% pull(year)
  Neff <- tmp %>% distinct(year, n) %>% pull(n)
  plot_osa_comps(o, p, pearson, index=lens, years=years, index_label='length', Neff=Neff,
                 stock='GOArougheye', survey=unique(rebslens$index)[i], outpath='figs')
}

# ages
rebsage <- readRDS(here::here('data', 'goa_rebs_agecomps.RDS'))
for(i in 1:length(unique(rebsage$index))) {
  ages <- unique(rebsage$age)
  tmp <- rebsage %>% dplyr::filter(index == unique(rebsage$index)[i])
  o <- tmp %>% pivot_wider(id_cols = year, names_from = age, values_from = obs) %>% select(-year) %>% as.matrix()
  p <- tmp %>% pivot_wider(id_cols = year, names_from = age, values_from = pred) %>% select(-year) %>% as.matrix()
  pearson <-  tmp %>% pivot_wider(id_cols = year, names_from = age, values_from = pearson) %>% select(-year) %>% as.matrix()
  years <- tmp %>% distinct(year, n) %>% pull(year)
  Neff <- tmp %>% distinct(year, n) %>% pull(n)
  plot_osa_comps(o, p, pearson, index=ages, years=years, index_label='age', Neff=Neff,
                 stock='GOArougheye', survey=unique(rebsage$index)[i], outpath='figs')
  }

# sablefish

# lengths
sablelens <- readRDS(here::here('data', 'sable_lencomps.RDS'))
for(i in 1:length(unique(sablelens$index))) {
  lens <- unique(sablelens$length)
  tmp <- sablelens %>% dplyr::filter(index == unique(sablelens$index)[i])
  o <- tmp %>% pivot_wider(id_cols = year, names_from = length, values_from = obs) %>% select(-year) %>% as.matrix()
  p <- tmp %>% pivot_wider(id_cols = year, names_from = length, values_from = pred) %>% select(-year) %>% as.matrix()
  pearson <-  tmp %>% pivot_wider(id_cols = year, names_from = length, values_from = pearson) %>% select(-year) %>% as.matrix()
  years <- tmp %>% distinct(year, n) %>% pull(year)
  Neff <- tmp %>% distinct(year, n) %>% pull(n)
  plot_osa_comps(o, p, pearson, index=lens, years=years, index_label='length', Neff=Neff,
                 stock='AKsablefish', survey=unique(sablelens$index)[i], outpath='figs')
}
# ages
sableage <- readRDS(here::here('data', 'sable_agecomps.RDS'))
for(i in 1:length(unique(sableage$index))) {
  ages <- unique(sableage$age)
  tmp <- sableage %>% dplyr::filter(index == unique(sableage$index)[i])
  o <- tmp %>% pivot_wider(id_cols = year, names_from = age, values_from = obs) %>% select(-year) %>% as.matrix()
  p <- tmp %>% pivot_wider(id_cols = year, names_from = age, values_from = pred) %>% select(-year) %>% as.matrix()
  pearson <-  tmp %>% pivot_wider(id_cols = year, names_from = age, values_from = pearson) %>% select(-year) %>% as.matrix()
  years <- tmp %>% distinct(year, n) %>% pull(year)
  Neff <- tmp %>% distinct(year, n) %>% pull(n)
  plot_osa_comps(o, p, pearson, index=ages, years=years, index_label='age', Neff=Neff,
                 stock='AKsablefish', survey=unique(sableage$index)[i], outpath='figs')
}
