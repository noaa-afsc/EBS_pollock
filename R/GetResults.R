library(ebswp)
library(here)
library(GOApollock)
#---Main pollock model run-----
here::here()
mod_names <- c("Pollock_model")   
mod_dir <- c("lastyr")   
nextyr=2025
#M<- ebswp::get_results(rundir = "pm2023")
setwd(here("runs","lastyr"))
pm1 <- read_rep("pm.rep")
setwd(here("runs","lastyrdb"))
pm2 <- read_rep("pm.rep")
#names(pm1)
pm_obj <- function(pm_run=pm1, yrmin=1964,yrmax=2024, src="Pollock_model") {
  sel_pm <- as_tibble(pm_run$sel_fsh) |> rowwise() |> 
    mutate(across(everything(),~ ./max(c_across(everything())))) |>  ungroup()
  sel_pm <- cbind(1964:2023,sel_pm)
  names(sel_pm) = c("Year",1:15)
  sel_pm <- sel_pm |> filter(Year>=yrmin,Year<=yrmax) |> 
    mutate(source=src) 
  ts <- rbind(
    data.frame(
      Year  = pm_run$SSB[,1],
      source= src, 
      type  = "SSB",
      se    = pm_run$SSB[,3],
      value = pm_run$SSB[,2]),
  data.frame(
    Year =pm_run$R[,1],
    source= src, 
    type  = "Recruits",
    se   =pm_run$R[,3],
    value=pm_run$R[,2])
)
return(list(sel=sel_pm,ts=ts, lst=pm_run))
}
setwd(here())
#pm_run<-pm_obj()
#glimpse(pm_run)

#---SS run-----
# get FMSY
#SS_plots(r1)
#ss_run=r1; yrmin=1964;yrmax=2024; atype="Asel"; Fleet=NULL 
#atype=c("Asel2","Asel") 
#r1$derived_quants
#ss_run<-r1
#ss_run=r1; yrmin=1964;yrmax=2023; atype=c("Asel") 

SS_obj <- function(ss_run=r1, ssdir="runs/ss/base",yrmin=1964,yrmax=2024, 
                   atype=c("Asel"),src="SS") {
  r1 <- SS_output(dir = here(ssdir)); 
  sel_ss <- ss_run$ageselex |> filter(Yr>=yrmin,Yr<=yrmax, Factor %in% atype) |> 
    mutate(fleet=ss_run$FleetNames[Fleet]) |> select(fleet,Year=Yr, c(9:23)) |> 
    mutate(source=src) |> filter(fleet=="Fishery") |>  select(-fleet)

  #ts <- ss_run$sprseries |> filter(Era=="TIME",Yr>=yrmin,Yr<=yrmax ) |> 
  #  select(Year=Yr,source="SS",SSB,MnAge=MnAge_Smry,MnAgeCatch=MnAge_Catch,Recruits,Catch=Dead_Catch_B) 
  tmp <- rbind(
    as_tibble(ss_run$derived_quants |> dplyr::filter(grepl("SSB_", Label))) |> 
      mutate(value=Value/2,
         se=StdDev,type="SSB",
         Year=as.numeric(substr(Label,5,9)),
         source=src ) |>  filter(!is.na(Year)) |> 
    select(Year,source,type,se,value) ,
   as_tibble(ss_run$derived_quants |> dplyr::filter(grepl("Recr_", Label)) |> 
      mutate(value=Value,
         se=StdDev,type="Recruits",
         Year=as.numeric(substr(Label,6,10)),
         source=src ) |>  filter(!is.na(Year)) |> 
    transmute(Year=Year+1,source,type,se=se*exp(-.9),value=value*exp(-.9)) |> 
    select(Year,source,type,se,value) 
      ) )
  return(list(sel=sel_ss,ts=tmp))
}
#ss_run<-SS_obj()
#ss_run$ts
#ss_run$sel
#tail(tmp)
   #ss_run$derived_quants |> dplyr::filter(grepl("Recr_", Label))  

#--Cole's in results from Cole's run----
#ssb <- filter(asdrep, name=='Espawnbio')
#unique(asdrep$name)
#ggplot(ssb, aes(year, est, ymin=lwr, ymax=upr)) + geom_line() +
#  geom_ribbon(alpha=.5) + labs(y='SSB (Mt)')
#rec <- filter(asdrep, name=='recruit')
#ggplot(rec, aes(year, est, ymin=lwr, ymax=upr)) + geom_pointrange() +
#  labs(y='Recruits (billions)')
#gp_sd=asdrep; gp_rep=arep; yrmin=1964;yrmax=2024 
gp_obj <- function(path=here("colepoll","2023"), gp_sd=asdrep, gp_rep=arep, yrmin=1964,yrmax=2024) {
  gp_rep <- read_pk_rep(path=path,model='gp', version='gp', styr=1964, endyr=2024)
  gp_sd  <- read_pk_std(path=path,model='gp', version='gp', styr=1964, endyr=2024)
  ssb <- filter(gp_sd, name=='Espawnbio')
  rec <- filter(gp_sd, name=='recruit')
  
  mysel <-  gp_rep$Fishery_selectivity
  mysel
  dim(mysel)
  new_columns <- matrix(rep(mysel[, 10], 5), nrow = nrow(mysel))
  
  # Combine the original matrix with the new columns
  mysel <- cbind(mysel, new_columns)
  
  sel_am <- as_tibble(cbind(1964:2023, as.data.frame(mysel)))
  names(sel_am) = c("Year",1:15)
  sel_am <- sel_am |> filter(Year>=yrmin,Year<=yrmax) |> mutate(source="GOA_model")
  ts <- rbind(
    data.frame(
      Year=ssb$year,
      source=ssb$version,
      type="SSB",
      se = ssb$se,
      value=ssb$est*500),
    data.frame(
      Year=rec$year,
      source=rec$version,
      type="Recruits",
      se = rec$se,
      value=rec$est*1000)
  )
  return(list(sel=sel_am,ts=ts))
}
#gp_run<-gp_obj()

#--Read in results from SAM run----
#here()
#load("SAM/poll23/baserun/model.RData")
#load(here("SAM","poll23","run","model2.RData"))
#SAM_obj <- function(myfit=fit, yrmin=1964,yrmax=2024) {
  #ssb <-  data.frame(stockassessment::ssbtable(myfit))
  #ssb$Year<- as.numeric(row.names(ssb))
  #ssb <- ssb |> mutate(value=Estimate/2,type="SSB",se=(log(High)-log(Estimate)),source="SAM")  |>  
    #select(Year,source,type,se,value)
  #rec <-  data.frame(stockassessment::rectable(myfit))
  #rec$Year<- as.numeric(row.names(rec))
  #rec <- rec |> mutate(value=Estimate,type="Recruits",se=(log(High)-log(Estimate)),source="SAM") |>  
    #select(Year,source,type,se,value)
  #sel <-  data.frame(stockassessment::faytable(myfit))
  #sel <- as.data.frame(t(apply(sel, 1, function(x) x / max(x))))
  #sel <- cbind(as.numeric(row.names(sel)),sel)
  #sel$source <- "SAM"
  #names(sel)<-c("Year",1:15,"source")
  #return(list(sel=sel,ts=rbind(rec,ssb)))
#}
#sam_run<- SAM_obj()

#--Read in results from AMAK2 run----
#setwd("~/_mymods/afsc-assessments/ebs_pollock_mod_alts/amak2/runs")
#jjmR::runJJM(models="amak",output="results",exec="../../src/amak")
#am1 <- jjmR::runit("amak",  pdf=TRUE,portrait=F,est=TRUE,exec="amak")
#am2 <- jjmR::readJJM(model="amak")
#names(am1[[1]][[1]][[5]][[1]])
AMAK_obj <- function(am_run=am2, yrmin=1964, 
                     yrmax=2024, src="AMAK", nind=5,
                     run_dir=here("amak2","runs","base") ) {
  setwd(run_dir)
  am_run <- PBSadmb::readRep("For_R_1")
  setwd(here())
 # sel_am <- as_tibble(am_run[[1]]$ebswp$output$Stock_1$sel_fsh_1 )
  sel_am <- as_tibble(am_run$sel_fsh_1) 
  tmp <- (sel_am[,3:17]) |> rowwise() |> 
    mutate(across(everything(),~ ./max(c_across(everything())))) |>  ungroup()
  sel_am[,3:17]=tmp
  names(sel_am) = c("stock","Year",1:15)
  sel_am <- sel_am |> filter(Year>=yrmin,Year<=yrmax) |> select(-stock) |> 
    mutate(source=src)
  sel_am
  ts <- rbind(
    data.frame(
      #Year  = am_run[[1]]$ebswp$output$Stock_1$SSB[,1],
      Year  = am_run$SSB[,1],
      source= src,
      type  = "SSB",
      #se    = am_run[[1]]$ebswp$output$Stock_1$SSB[,3]),
      #value = am_run[[1]]$ebswp$output$Stock_1$SSB[,2]),
      se    = 0.5*am_run$SSB[,3],
      value = 0.5*am_run$SSB[,2]),
  data.frame(
    Year =am_run$R[,1],
    #Year =am_run[[1]]$ebswp$output$Stock_1$R[,1],
    source= src, 
    type  = "Recruits",
    #se   =am_run[[1]]$ebswp$output$Stock_1$R[,3])
    #value=am_run[[1]]$ebswp$output$Stock_1$R[,2])
    se   =am_run$R[,3],
    value=am_run$R[,2])
)
fit_df <- NULL
for (i in 1:nind){
  fit_df <- rbind(fit_df, data.frame(
    Year =get(paste0("Obs_Survey_",i),am_run)[,1],
    #Year =am_run[[1]]$ebswp$output$Stock_1$R[,1],
    source= src, 
    type  = paste0("Index_",i),
    pred =get(paste0("Obs_Survey_",i),am_run)[,3],
    obs  =get(paste0("Obs_Survey_",i),am_run)[,2],
    se   =get(paste0("Obs_Survey_",i),am_run)[,4]
  )
  )
}
return(list(sel=sel_am,ts=ts,fit=fit_df,lst=am_run))
}

#---Plotting functions-----
Plot_index <- function(df, idx=c(1,2,5) ) {
  p1  <- df |> filter(type %in% paste0("Index_",idx), Year<2024,Year>1963) |> ggplot(aes( x=Year,
                                                          y= obs,
                                                          color=source)) + 
    geom_line(aes(y=pred), stat='identity') +
    geom_point(stat='identity') +
    ggthemes::theme_few() + ylab("Index") + xlab("Year") +
    ylim(0,NA) +
    facet_grid(type~.,scales="free_y")  
  return(p1)
} 

Plot_SSB <- function(df=all_ts) {
  p1  <- df |> filter(Year<2024,Year>1963) |> ggplot(aes( x=Year,
        y= value,color=source)) + 
    geom_line(stat='identity') +
    geom_point(stat='identity') +
    ggthemes::theme_few() + ylab("SSB") + xlab("Year") +
    ylim(0,NA) +
    facet_grid(type~.,scales="free_y")  
  return(p1)
} 
# glimpse(all_ts)

#all_ts |> pivot_wider(names_from=type,values_from=value,-se) |> select(1:5) |> tail()

Plot_SRR <- function(df=all_ts) {
    ssbtmp<-df |> filter(type=="SSB") |> mutate(Year=Year-1) 
    rectmp<-df |> filter(type=="Recruits")
    srr <- merge(ssbtmp, rectmp, by = c("Year", "source"), suffixes = c("_SSB", "_R"))
  p1  <- srr |> filter(Year<2024,Year>1963) |> 
    ggplot(aes( x=value_SSB, y= value_R,label=Year,color=source)) + 
    geom_text(stat='identity',size=2) +
    ggthemes::theme_few() + xlab("SSB") + ylab("Recruitment") +
    xlim(0,NA) +
    ylim(0,NA) +
    facet_grid(source~.,scales="fixed")  
    #facet_grid(source~.,scales="free_y")  
  return(p1)
} 

#sel=all_sel;fage=1;lage=15;nages=15
Plot_Sel_age <- function(sel=all_sel,fage=1,lage=15,nages=15) {
  sdf       <-  pivot_longer(sel,names_to="age",values_to="sel",cols=2:(nages+1)) %>% filter(Year>=1991,Year<2024) %>% mutate(age=as.numeric(age)) |> 
    filter(age>2, age<11)
  p1  <- sdf |> ggplot(aes( x=Year,
        y= sel,color=source)) + 
    geom_line(stat='identity') +
    geom_point(stat='identity') +
    ggthemes::theme_few() + ylab("Selectivity") + xlab("Year") +
    facet_wrap(.~age) ; 
  p1
}
#nages=15;styr=1964;endyr=2023;fage=1;lage=10

Plot_Sel <- function(sel=all_sel,fage=1,lage=10,nages=15,styr=1964,endyr=2023) {
  sdf <-  pivot_longer(sel,names_to="age",values_to="sel",cols=2:(nages+1)) |>  
    filter(Year>=styr,Year<=endyr) |>  mutate(age=as.numeric(age)) 
  p1  <- sdf |> ggplot(
    aes( x=age, y=as.factor(Year),height = sel)) + 
    geom_density_ridges(stat = "identity",scale = 2.8, 
                        alpha = .3, fill="goldenrod",color="tan") + 
    ggthemes::theme_few() + ylab("Year") + xlab("Age (years)") +
    scale_x_continuous(limits=c(fage,lage),breaks=fage:lage) +
    scale_y_discrete(limits=rev(levels(as.factor(sdf$Year)))) +
    facet_grid(.~source)
  return(p1)
}

#--Other functions--------------
# Function to evaluate variability for year-age matrix (e.g., selectivity) where
# rows are years and columns are ages
compute_matrix_summary <- function(matrix_data){
  # Compute column means
  column_means <- colMeans(matrix_data)
  # Normalize the matrix by column means
  normalized_matrix <- sweep(matrix_data, 2, column_means, "/")
  
  # Calculate standard deviations by rows
  row_cv <- apply(normalized_matrix, 1, function(x) sd(x)/mean(x) );
  col_cv <- apply(normalized_matrix, 2, function(x) sd(x)/mean(x) );
  return(list(CV_row=mean(row_cv),CV_col= mean(col_cv) ))
}

#compute_matrix_summary(matrix_data)

#xplot_sel <- function(Year=M$Yr,sel=M$sel_fsh, styr=1977, fage=NULL, lage=NULL, alpha=0.2,scale=3.8,fill="purple")
#{
  #df        <- data.frame(Year=Year,sel=sel );
  #if (is.null(fage)) fage      <- 1
  #if (is.null(lage)) lage      <- length(sel[1,])
  #df <- df |> select(1:(lage-fage+2))
  #names(df) <- c("Year",fage:lage)
  #nages     <- length(fage:lage)
  #names(df)
  #sdf       <-  pivot_longer(df,names_to="age",values_to="sel",cols=2:(nages+1)) %>% filter(Year>=styr) %>% mutate(age=as.numeric(age)) #+ arrange(age,yr)
  #p1  <- ggplot(sdf,aes(x=age,y=as.factor(Year),height = sel)) + geom_density_ridges(stat = "identity",scale = scale, alpha = alpha,
                                                                                     #fill=fill,color="black") + ggthemes::theme_few() +
    #ylab("Year") + xlab("Age (years)") +
    #scale_x_continuous(limits=c(fage,lage),breaks=fage:lage) +
    #scale_y_discrete(limits=rev(levels(as.factor(sdf$Year))))
  #return(p1)
#}

