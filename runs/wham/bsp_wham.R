# load wham
library(wham)

# create directory for analysis, E.g.,
write.dir <- here::here("runs","wham")
if(!exists("write.dir")) write.dir = getwd()
if(!dir.exists(write.dir)) dir.create(write.dir)
setwd(write.dir)

# copy asap3 data file to working directory
wham.dir <- here::here("runs","wham")
# file.copy(from=file.path(wham.dir,"extdata","ex1_SNEMAYT.dat"), to=write.dir, overwrite=FALSE)

# confirm you are in the working directory and it has the ASAP_SNEMAYT.dat file
list.files()

# read asap3 data file and convert to input list for wham
asap3 <- read_asap3_dat("ebswp.dat")

# m1-m5 logistic, m6-m9 age-specific
sel_model <- c(rep("logistic",5), rep("age-specific",4))

# time-varying options for each of 3 blocks (b1 = fleet, b2-3 = indices)
sel_re <- list(c("none","none","none"), # m1-m5 logistic
                c("iid","none","none"),
                c("ar1","none","none"),
                c("ar1_y","none","none"),
                c("2dar1","none","none"),
                c("none","none","none"), # m6-m9 age-specific
                c("iid","none","none"),
                c("ar1_y","none","none"),
                c("2dar1","none","none"))
n.mods <- length(sel_re)
df.mods <- data.frame(Model=paste0("m",1:n.mods), 
    Selectivity=sel_model, # Selectivity model (same for all blocks)
    Block1_re=sapply(sel_re, function(x) x[[1]])) # Block 1 random effects
rownames(df.mods) <- NULL
df.mods
asap3$dat$sel_block_assign 
asap3$dat$sel_block_option 
asap3$dat$index_sel_option 
asap3$dat$sel_ini 
asap3$dat$index_sel_ini 

m=2
for(m in 1:n.mods){
    if(sel_model[m] == "logistic"){ # logistic selectivity
        # overwrite initial parameter values in ASAP data file (ex1_SNEMAYT.dat)
        input <- prepare_wham_input(asap3, model_name=paste(paste0("Model ",m), sel_model[m], paste(sel_re[[m]], collapse="-"), sep=": "), recruit_model=2,
                    selectivity=list(model=rep("logistic",3), re=sel_re[[m]], initial_pars=list(c(2,0.2),c(2,0.2))),
                    NAA_re = list(sigma='rec+1',cor='iid'),
                    age_comp = "logistic-normal-miss0") # logistic normal, treat 0 obs as missing
    } else { # age-specific selectivity
        # # you can try not fixing any ages first
        # input <- prepare_wham_input(asap3, model_name=paste(paste0("Model ",m), sel_model[m], paste(sel_re[[m]], collapse="-"), sep=": "), recruit_model=2, 
        #           selectivity=list(model=rep("age-specific",3), re=sel_re[[m]], 
        #               initial_pars=list(rep(0.5,6), rep(0.5,6), rep(0.5,6))),
        #           NAA_re = list(sigma='rec+1',cor='iid'),
        #           age_comp = "logistic-normal-miss0") # logistic normal, treat 0 obs as missing

        # often need to fix selectivity = 1 for at least one age per age-specific block: ages 4-5 / 4 / 2-4
        input <- prepare_wham_input(asap3, model_name=paste(paste0("Model ",m), sel_model[m], paste(sel_re[[m]], collapse="-"), sep=": "), recruit_model=2,
                    selectivity=list(model=rep("age-specific",3), re=sel_re[[m]], 
                                    initial_pars=list(
                                        c(0.01,0.5,0.5,0.5,1,1.0,0.96,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5),
                                        c(0.5,0.05,0.5,0.5,1,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5) ), 
                        fix_pars=list(4:5,4,2:4)),
                    NAA_re = list(sigma='rec+1',cor='iid'),
                    age_comp = "logistic-normal-miss0") # logistic normal, treat 0 obs as missing
    }
    
    # fit model
    mod <- fit_wham(input, do.check=T, do.osa=F, do.proj=F, do.retro=F) 
    saveRDS(mod, file=paste0("m",m,".rds"))
}

# ---------------------------------------------------------------
# model 1
#   recruitment expectation (recruit_model): random about mean (no S-R function)
#   recruitment deviations (NAA_re): independent random effects
#   selectivity: age-specific (fix sel=1 for ages 4-5 in fishery, age 4 in index1, and ages 2-4 in index2)
input1 <- prepare_wham_input(asap3)
input1 <- prepare_wham_input(asap3, recruit_model=2, model_name="EBS pollock example",
	                            selectivity=list(model=rep("age-specific",2), 
                                	re=rep("none",2), 
                                	initial_pars=list(
                                		c(0.01,0.5,0.5,0.5,1,1.0,0.96,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5),
                                		c(0.5,0.05,0.5,0.5,1,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5) 
                                		), 
                                	fix_pars=list(5:7,c(2,6))),
	                            NAA_re = list(sigma="rec", cor="iid"))
m1 <- fit_wham(input1, do.retro=FALSE,do.osa = FALSE) # turn off OSA residuals to save time
m1_proj <- project_wham(model=mods$m1)

# Check that m1 converged (m1$opt$convergence should be 0, and the maximum gradiet should be < 1e-06)
check_convergence(m1)

# ---------------------------------------------------------------
# model 2
#   as m1, but change age comp likelihoods to logistic normal (treat 0 observations as missing)
input2 <- prepare_wham_input(asap3, recruit_model=2, model_name="Ex 1: SNEMA Yellowtail Flounder",
                                    selectivity=list(model=rep("age-specific",3), 
                                        re=rep("none",3), 
                                        initial_pars=list(c(0.5,0.5,0.5,0.5,1,0.5),c(0.5,0.5,0.5,1,0.5,0.5),c(0.5,1,0.5,0.5,0.5,0.5)), 
                                        fix_pars=list(4:5,4,2:4)),
                                    NAA_re = list(sigma="rec", cor="iid"),
                                    age_comp = "logistic-normal-miss0")
m2 <- fit_wham(input2, do.osa = F) # turn off OSA residuals to save time

# Check that m2 converged
check_convergence(m2)

# ---------------------------------------------------------------
# model 3
#   full state-space model, numbers at all ages are random effects (NAA_re$sigma = "rec+1")
input3 <- prepare_wham_input(asap3, recruit_model=2, model_name="Ex 1: SNEMA Yellowtail Flounder",
	                            selectivity=list(model=rep("age-specific",3), 
                                	re=rep("none",3), 
                                	initial_pars=list(c(0.5,0.5,0.5,0.5,1,0.5),c(0.5,0.5,0.5,1,0.5,0.5),c(0.5,1,0.5,0.5,0.5,0.5)), 
                                	fix_pars=list(4:5,4,2:4)),
	                            NAA_re = list(sigma="rec+1", cor="iid"))
m3 <- fit_wham(input3, do.osa = F) # turn off OSA residuals to save time

# Check that m3 converged
check_convergence(m3)

# ---------------------------------------------------------------
# model 4
#   as m3, but change age comp likelihoods to logistic normal
input4 <- prepare_wham_input(asap3, recruit_model=2, model_name="Ex 1: SNEMA Yellowtail Flounder",
                                    selectivity=list(model=rep("age-specific",3), 
                                        re=rep("none",3), 
                                        initial_pars=list(c(0.5,0.5,0.5,0.5,1,0.5),c(0.5,0.5,0.5,1,0.5,0.5),c(0.5,1,0.5,0.5,0.5,0.5)), 
                                        fix_pars=list(4:5,4,2:4)),
                                    NAA_re = list(sigma="rec+1", cor="iid"),
                                    age_comp = "logistic-normal-miss0")
m4 <- fit_wham(input4, do.osa = T) # do OSA residuals for m4 bc we'll show that output

# Check that m4 converged
check_convergence(m4)

# ------------------------------------------------------------
# Save list of all fit models
mods <- list(m1=m1, m2=m2, m3=m3, m4=m4)
save("mods", file="ex1_models.RData")

# Compare models by AIC and Mohn's rho
res <- compare_wham_models(mods, fname="ex1_table", sort=TRUE)
res$best

# Project best model, m4,
# Use default values: 3-year projection, use average selectivity, M, etc. from last 5 years
m4_proj <- project_wham(model=mods$m4)

# WHAM output plots for best model with projections
# plot_wham_output(mod=m4, out.type='html')
plot_wham_output(mod=m4_proj, out.type='html')
