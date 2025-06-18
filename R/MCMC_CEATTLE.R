remove.packages(adnuts)
library(TMB)
install.packages('StanEstimators', repos = c('https://andrjohns.r-universe.dev', 'https://cloud.r-project.org'))
devtools::install_github("Cole-Monnahan-NOAA/adnuts", ref='sparse_M',force=TRUE)
library(adnuts)

obj <- (pollock_RE$obj)
obj<- mod_fixed_selinf$obj
obj<-pollock_init4$obj
obj<-pollock_init3$obj
names(obj$env$map)
saveRDS(pollock_base$obs,"c1.RDS")
saveRDS(obj,"c2.RDS")
obj<- readRDS("cea_obj.RDS")
mcmc <- adnuts::sample_sparse_tmb(obj,skip_optimization=TRUE)
mcmc <- adnuts::sample_sparse_tmb(obj,skip_optimization=TRUE,iter=3000, chains = 8)
#mcpilot <- adnuts::sample_sparse_tmb(obj,skip_optimizatioon=TRUE,iter=2000, chains = 5)
plot_uncertainties(mcmc)

pairs_admb(mcmc, pars=1:8, order='slow')
pairs_admb(mcmc, pars=1:8, order='fast')
pairs_admb(mcmc, pars=c("R_ln_sd",'sel_dev_ln_sd')) 
pairs_admb(mcmc, pars=1:8, order='slow')

pairs_admb(mcmc, pars=c("R_ln_sd",'sel_dev_ln_sd',"lp__"))
pairs_admb(mcmc, pars=c("R_ln_sd",'sel_dev_ln_sd')) 
names(fm0$estimated_params)
(fm4$estimated_params$sel_inf)
(fm4$estimated_params$index_ln_q)
(fm4$estimated_params$sel_inf[1])
(fm00$map$mapList$sel_inf)
names(fm00)
names(fm00$bounds$upper)
(fm00$bounds$upper$sel_inf)
names(fm00$initial_params)
(fm00$initial_params$sel_inf)
(fm00$initial_params$ln_sel_slp)
exp(fm00$initial_params$ln_sel_slp)
# Need to map off upper bound of sel_inf[] 
# Age-error?



