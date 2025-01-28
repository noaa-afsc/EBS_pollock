library(tidyverse)
library(ggthemes)
library(patchwork)
.THEME <- ggthemes::theme_few()
  #---Historical assessment retrospectives--------------------------------------------------------
  dd <- as.data.frame(read.csv("doc/data/Age3history.csv",header=TRUE))
  head(dd)
  names(dd) <- c("Year",2021:2006,2001:1998)
  dd.g <- pivot_longer(dd,cols=2:21,names_to="Assessment",values_to="Biomass")
  head(dd.g)
  # this line to add current year estimate!!
  t <- data.frame(Year=1964:(thisyr+1), Assessment=thisyr,Biomass=c(M$age3plus,M$age3plus1)  )
  dd.g <- rbind(dd.g,t) %>% filter(as.numeric(Assessment) >2006)
  tmp <- dd.g %>% filter(Year>2006,Year==1+as.numeric(Assessment)) # %>% summarise(max(Year))
  p1 <- ggplot(dd.g,aes(x=Year,y=Biomass,color=Assessment)) + geom_line(alpha=.8,size=.75) +
    scale_x_continuous(breaks=seq(1980,thisyr+2,2),limits=c(1980,thisyr+1))  +  xlab("Year") + ylim(0,16000) + ylab("Age 3+ biomass (kt)") +
    geom_point(data=tmp,size=2) + theme_few() +
    guides(size=FALSE,shape=FALSE,alpha=FALSE,col=FALSE)
  p1
  ggsave("doc/figs/mod_hist.pdf",plot=p1,width=9.2,height=4.0,units="in")
  
  #----------------------------------------------------------
  # Extract Fmsy for different selectivity years
 M 
  idx=grep("msy2_dec",M$fit$name)
  length(idx)
  idx
  M[idx]
  
  fdf <- data.frame(year=rep(2015:2024,2),
                    Source=M$fit$name[idx],
                    est = M$fit$est[idx],
                    std = M$fit$std[idx])
  unique(fdf$Source)
  
  p1<-  fdf %>% filter(Source!="Fmsy2_decwt") %>%
    ggplot(aes(x=year+.1,y=est,ymax=est+2*std,ymin=est-2*std))+
    geom_errorbar(width=.3) + geom_point(size=5,color="purple") + theme_few(base_size=12) + 
    xlab("Year selected for MSY calculation") +
    scale_x_continuous(breaks=seq(2015,thisyr,1),limits=c(2014,thisyr+1))  +
    ylab("Fmsy");p1
  ggsave("doc/figs/fmsy_sel_hist.pdf",plot=p1,width=5.2,height=4.0,units="in")
  
  ##
  sel <- M$sel_fsh
  seldf=NULL
  yrs <- (thisyr-10):(thisyr)
  yrs
  lrow<-dim(sel)[1]
  seldf<-rbind(seldf, data.frame(yrs,rep(thisyr,11),sel[(lrow-10):lrow,])) |>  
    rbind(c("proj",thisyr,M$sel_fut))
  #assign(mn,read_rep(rn))
  names(seldf) <- c("Year","Assessment",1:15)
  seldfm <- seldf %>%  pivot_longer(cols=3:17,names_to="age",values_to="Selectivity") %>%
    mutate(
      age=as.numeric(age),
      Selectivity=as.numeric(Selectivity),
      Assessment=as.numeric(Assessment),
      case = ifelse(Year=="proj","Proj.","Est."),
      Year=as.numeric(ifelse(Year=="proj",Assessment+1,Year))
    )
  glimpse(seldfm)
  assdf <- seldfm %>% filter(Year>Assessment) %>% mutate(age=as.factor(age),Year=as.factor(Year))
  yrdf  <- seldfm %>% filter(Year<=Assessment,Year>thisyr-10) %>%mutate(age=as.factor(age),Year=as.factor(Year))
  glimpse(assdf)
  glimpse(yrdf)
  #----------------------
  p1<- seldfm %>% filter(age<9,Year>thisyr-10)%>% group_by(Year,Assessment,case) %>%
    summarise(mnage=sum(Selectivity*age)/sum(Selectivity)) %>%
    ggplot(aes(x=Year,y=mnage,shape=case,color=case)) + geom_point(size=2) +
    theme_few() + ylim(c(0,10)) + ylab( "Mean age selected")+
    scale_x_continuous(breaks=seq(2010,2024,by=2))
  p1
  p1 <-    ggplot(assdf,aes(x=age,y=Selectivity,color=case,group=Year)) +
    geom_point(size=.5) + geom_line() + theme_few() +
    facet_grid(Year~.) + geom_point(data=yrdf,size=.5) + facet_grid(Year~.)
  p1
  #----------------------
  ggsave("doc/figs/retro_sel.pdf",plot=p1,width=5.2,height=9.0,units="in")
  #plot_recruitment(retouts,xlim=c(1990,2017),rel=T,legend=FALSE,alpha=.2)
  
  
  #---Plot of regimes on base model-------------------------
  regime=c("1964-77", "1978-present", "1978-99", "1978-89", "1990-present", "1990-99", "2000-present", "1964-present")
  hlr <- M$regime[2]
  p3 <- tibble(regime=regime,Mean=M$regime,ub=Mean+2*M$regime.sd,lb=Mean-2*M$regime.sd) %>%
    ggplot(aes(x=regime,y=Mean,ymax=ub,ymin=lb)) + geom_linerange(size=1.5) + theme_few(base_size=12) +
    ylab("Mean recruitment (age 1)") + geom_point(size=4,color="red") + geom_hline(yintercept=hlr,linetype=2)
  ggsave("doc/figs/mod_regimes.pdf",plot=p3,width=9.2,height=5.0,units="in")
  
  #----Read in retro results-----------------
  .MODELDIR #<- c("2024","runs","retro")
  i=2
  mod=2
  setwd(here::here())
  getret <- function(ret_dir="retro", nyrs=10, mod=1){
    retouts<- list()
    for (i in 0:nyrs) {
      rn=paste0(.MODELDIR[mod],ret_dir,"/r_",i,".rep")
      mn=paste0("r_",i)
      assign(mn,read_rep(rn))
      retouts[[mn]] <- (get(mn))
    }
    return(retouts)
  }
  library(patchwork)
  ret1<-getret(ret_dir="retro1", mod=2,nyrs=10)
  retlyr<-getret(mod=1,nyrs=20)
  ret2<-getret(ret_dir="retro2",mod=2,nyrs=10)
  ret3<-getret(ret_dir="retro3", mod=2,nyrs=10)
  ret4<-getret(ret_dir="retro4", mod=2,nyrs=10)
  ret5<-getret(ret_dir="retro5", mod=2,nyrs=10)
  names(ret2)
  p1 <- plot_ssb(retlyr,xlim=c(2000,thisyr),legend=F,breaks=seq(2000,2025,2),ylim=c(0,6900)) + xlab("") +ggtitle(" Last year") + coord_cartesian(ylim=c(0,NA));p1
  p2 <- plot_ssb(ret2,xlim=c(2000,thisyr),legend=F,breaks=seq(2000,2025,2),ylim=c(0,6900))+ggtitle("Updated model 23") + coord_cartesian(ylim=c(0,NA));p2
  #p3 <- plot_ssb(ret3,xlim=c(2000,thisyr),legend=F,breaks=seq(2000,2022,2),ylim=c(0,6900))+ggtitle("w/o BTS") + coord_cartesian(ylim=c(0,NA));p3
  p3 <- p1/p2   + plot_layout(axis_titles="collect") + plot_annotation(title = 'Spawning biomass retrospectives ');p3
  #subtitle = 'EBS walleye pollock assessment', caption = 'Disclaimer: Draft results, please do not cite' ); p3
  ggsave("doc/figs/retcompSSB.pdf",plot=p3,width=7.2,height=9.0,units="in")
  
  # recruitment
  p1 <- plot_R_rel(retlyr,xlim=c(thisyr-20,thisyr-1),legend=F,rel=FALSE,ylim=c(0,120000),ylab="Age-1 recruitment") + xlab("") +ggtitle("Updated model 2023")+coord_cartesian(ylim=c(0,125000));p1
  p2 <- plot_R_rel(ret2,xlim=c(thisyr-20,thisyr-1),legend=F,rel=FALSE,ylim=c(0,120000),ylab="Age-1 recruitment") + ggtitle("Last year") + coord_cartesian(ylim=c(0,125000));p2
  p3 <- p2/p1  +  plot_layout(axis_titles = "collect"); p3
  #  subtitle = 'EBS walleye pollock assessment', caption = 'Disclaimer: Draft results, please do not cite' ); p3
  ggsave("doc/figs/retcompR.pdf",plot=p3,width=7.2,height=9.0,units="in")
  p3
  
  ## Make dataframe to plot by cohort
  nyrs=10
  i=2
  retdf
  
  i=12
  run="shit" 
  names(ret2[[1]])
  ret=ret1
  mean(ret1[["r_0"]]$R[,2])
  get_coh <- function(ret=ret1,modyr=2024, run="Base",nyrs=20){
    M <- as.data.frame(NULL)
    for (i in 0:nyrs) {
      mn=paste0("r_",i)
      A <- as.data.frame(ret[[mn]]$R)
      is.data.frame(A)
      A$ret <- i
      A$run <- run
      A$cohort <- A[,1]-1
      A$term_yr <- modyr-A$ret
      A$Yrs_of_data <- A$term_yr-A$cohort
      names(A) <- c("Year","est","sd","lb","ub","peel","run","cohort","term_yr","Years_data")
      M <- rbind(M,A)
    }
    return(M)
  }
  glimpse(M)
  # Plot by years of data
  ret_df <- rbind(get_coh(ret1, run="1-yr mean",nyrs=10),
                  get_coh(ret2, run="2-yr mean",nyrs=10),
                  get_coh(ret3, run="3-yr mean",nyrs=10),
                  get_coh(ret4, run="4-yr mean",nyrs=10),
                  get_coh(ret5, run="5-yr mean",nyrs=10) ) #|> mutate(Years_data=ifelse(run=="Base",Years_data-.14,Years_data+.14))
  
  #ret_df <- rbind(get_coh(ret1),get_coh(ret2,run="Without ATS"),get_coh(ret3,run="Without BTS")) |> mutate(Years_data=ifelse(run=="Base",Years_data-.18,ifelse(run=="Without ATS",Years_data,Years_data+.18))  )
  
  plot_coh <- function(dat=ret_df,coh=cohsub,logscale=FALSE,grid=T){
    p1 <- dat |>  filter(cohort %in% coh) |>
      ggplot( aes(x=Years_data,label=term_yr,y=est,color=run,ymin=lb,ymax=ub)) + geom_point(size=1) + geom_errorbar(size=.5,width=0) +
      theme_few(base_size = 11) +
      ylim(c(0,NA)) +
      geom_hline(yintercept=22.5e3,color="blue",size=.1) +
      ylab("Millions") +
      ggtitle("Age-1 recruitment estimates by cohort") #+ geom_text( aes(x=mean(Yeaes()x=mean()Years_data
    if (logscale) p1 <- p1 +  scale_y_log10()
    if (grid) p1 <- p1 +facet_grid(cohort~.) else p1 <- p1 +facet_wrap(cohort~.)
    return(p1)
  }
  
  ret_df <- get_coh(ret2)#|> mutate(Years_data=ifelse(run=="Base",Years_data-.14,Years_data+.14))
  ret_df <- rbind(get_coh(retlyr,modyr=2023, run="Last year"), get_coh(ret2,run="This year") )|> 
    mutate(Years_data=ifelse(run=="Last year",Years_data-.14,Years_data+.14))
  #cohsub <- c(2003:2005,2008) cohsub <- c(2012:2013,2008) cohsub <- c(2012:2013,2008)
  library(scales)
  plot_coh( grid=FALSE, coh= cohsub)
  plot_coh( grid=FALSE, coh= c(2005,2008,2012:2013,2016,2018) )
  plot_coh( grid=FALSE, coh= c(2005,2008,2012:2013,2016,2018) )
  plot_coh( grid=FALSE, coh= c(2003:2005,2008) )
  plot_coh( grid=T, coh= c(2003:2005) )
  plot_coh( grid=T, coh= c(2008,2012:2013,2018) , logscale=TRUE)
  plot_coh( grid=T, coh= c(2008,2012:2013,2016,2018) ) + 
    scale_x_continuous(breaks=seq(0,15,2)) + scale_y_log10(label=comma)  + ylab("Millions") + xlab("Years of data")
  theme_few(base_size=13) + ylab("millions") + xlab("Years of data")
  ggsave("doc/figs/retro_cohort.pdf",width=7.2,height=9.0,units="in")
  #plot_coh(logscale=TRUE)
  
  # Plot by endyr
  ggplot(ret_df |>
           #         filter(cohort %in% c(2008:2022)),
           filter(cohort %in% c(2008,2012:2013,2016,2018)),
         aes(x=term_yr,y=est,color=run,ymin=lb,ymax=ub)) + geom_point(size=2) + geom_errorbar(size=1,width=0) +
    theme_few() + facet_grid(cohort~.) + scale_y_log10(label=comma) + geom_hline(yintercept=22.5e3,color="blue",size=.1) +
    ggtitle("Age-1 recruitment estimates by cohort") + theme_few(base_size=14) + xlab("Terminal year") + ylab("millions")
  ggsave("doc/figs/retro_cohort_termyr.pdf",width=7.2,height=9.0,units="in")
  
  p1 <- plot_R_rel(ret2,xlim=c(thisyr-10,thisyr),legend=F,rel=FALSE,ylab="Age 1 recruitment");p1
  p2 <- plot_R_rel(ret2,xlim=c(thisyr-10,thisyr),ylim=c(0.,3.5),legend=FALSE,alpha=.2);p2 <- p2+ylim(0,4); p2
  p3 <- p1/p2 #+ plot_layout(axis_titles="collect");p3
  ggsave("doc/figs/mod_retroR.pdf",plot=p3,width=7.2,height=9.0,units="in")
  p1 <- plot_R_rel(ret2,xlim=c(thisyr-20,thisyr),legend=F,rel=FALSE, ylab="Age 1 recruitment");p1
  p2 <- plot_R_rel(ret2,xlim=c(thisyr-20,thisyr),ylim=c(0.,3.5),legend=FALSE,alpha=.2);p2 <- p2+ylim(0,4); p2
  p3 <- p1/p2 + plot_layout(axis_titles="collect");p3
  ggsave("doc/figs/mod_retroR20.pdf",plot=p3,width=7.2,height=9.0,units="in")
  p3
  ###
  
  
  #---Get selectivity from models----------------jjjjj
  ret2[[3]]$sel_fsh
  i=3
  
  ret=ret2
  get_sel_retro <- function(ret=ret2, thisyr=2024, case="2-yr mean", nyrs=10) {
    seldf<-NULL
    for (i in 1:(nyrs+1) ) {
     # sel <-get(mn)$sel_fsh
      M <- ret[[i]]
      sel <- M$sel_fsh
      yrs <- (thisyr-i-nyrs):(thisyr-i)
      #yrs
      lrow<-dim(sel)[1]
      #lrow
      #seldf<-rbind(seldf, data.frame(yrs,rep(thisyr-i,11),sel[(lrow-10):lrow,])) %>% rbind(c("proj",thisyr-i,get(mn)$sel_fut))
      seldf<-rbind(seldf, data.frame(yrs,rep(thisyr-i+1,11),sel[(lrow-10):lrow,])) %>% rbind(c("proj",thisyr-i+1,M$sel_fut))
    }
    return(seldf)
  }
 get_sel_retro() 
  names(seldf) <- c("Year","Assessment",1:15)
  glimpse(seldf)
  seldfm <- seldf %>%  pivot_longer(cols=3:17,names_to="age",values_to="Selectivity") %>%
    mutate(
      age=as.numeric(age),
      Selectivity=as.numeric(Selectivity),
      Assessment=as.numeric(Assessment),
      case = ifelse(Year=="proj","Proj.","Est."),
      Year=as.numeric(ifelse(Year=="proj",Assessment+1,Year))
    )
  glimpse(seldf)
  glimpse(seldfm)
  tail(seldfm)
  assdf <- seldfm %>% filter(Year>Assessment) %>% mutate(age=as.factor(age),Year=as.factor(Year))
  yrdf  <- seldfm %>% filter(Year<=Assessment,Year>thisyr-10) %>%mutate(age=as.factor(age),Year=as.factor(Year))
  p1<- seldfm %>% filter(age<9,Year>thisyr-10)%>% group_by(Year,Assessment,case) %>%
    summarise(mnage=sum(Selectivity*age)/sum(Selectivity)) %>%
    ggplot(aes(x=Year,y=mnage,shape=case,color=case)) + geom_point(size=2) +
    theme_few() + ylim(c(4,8)) + ylab( "Mean age selected")+
    scale_x_continuous(breaks=seq(2010,2022,by=2))
  p1
  ggsave(here::here("doc/figs/","retro_sel_mnage.pdf"),plot=p1,width=5.2,height=4.0,units="in")
  assdf %>% print(n=Inf)
  
 # yrdf %>% filter(Assessment==2021) %>%inner_join(assdf, join_by= Year)# %>% print(n=Inf)
  
  p1 <-    ggplot(assdf,aes(x=age,y=Selectivity,color=case,group=Year)) +
    geom_point(size=.8) + geom_line() + theme_few() +
    facet_grid(Year~.) + geom_point(data=yrdf,size=1) + facet_grid(Year~.)
  p1
  ggsave(here::here("doc/figs/retro_sel.pdf"),plot=p1,width=5.2,height=9.0,units="in")
  #plot_recruitment(retouts,xlim=c(1990,2017),rel=T,legend=FALSE,alpha=.2)
  #plot_R_rel(retouts,xlim=c(1980,2017),ylim=c(0.5,1.5),legend=FALSE,alpha=.2)
  # Mohn's rho
  
  #icesAdvice::mohn()
  #base   -1   -2   -3   -4   -5
  #2009 0.96 0.95 0.94 0.92 0.89 0.86
  #2010 0.72 0.71 0.70 0.67 0.63 0.59
  #2011 0.82 0.79 0.78 0.73 0.67 0.60
  #2012 0.80 0.75 0.73 0.66 0.54   NA
  #2013 0.67 0.62 0.61 0.51   NA   NA
  #2014 0.74 0.66 0.63   NA   NA   NA
  #2015 0.63 0.52   NA   NA   NA   NA
  #2016 0.57   NA   NA   NA   NA   NA
  #r$> mohn(shake)
  #[1] -0.2310701
  
  names(ret2)
  i=2
  getMohn <- function(retouts=ret2,nyrs=11){
    #rc = retouts[[1]]$SSB[,2]
    rc = retouts[[1]]$sel_fsh[,6]
    ntmp=0
    rho=0
    df <- data.frame(peel=1:nyrs,rho=1:nyrs)
    for (i in 1:nyrs) {
      X <- retouts[[i]]
      rn <- names(X)
      dtmp = X$SSB 
      dtmp = X$sel_fsh[,6] 
      #lr   = length(dtmp[,1])
      lr   = length(dtmp)
      #rho  = rho +(dtmp[lr,2] -rc[lr])/rc[lr]
      rho  = rho +(dtmp[lr] -rc[lr])/rc[lr]
      df$peel[i] <- i-1
      df$rho[i] <- rho/i
      print(paste(i,rho/i))
    }
    return(df)
  }
  
  retouts<-ret4
  df1 <- getMohn(ret1)
  df2 <- getMohn(ret2)
  df4 <- getMohn(ret4)
  df5 <- getMohn(ret5)
  df2
  write_csv(df,here::here("doc/data/mohnrho.csv"))
  here::here("doc/data/mohnrho.csv")
  library(flextable)
  # A try at a catch table not quite there...
  cd <- read_csv("../data/fishery/sampler/imported/akfin_cat.csv")
  names(cd) <- c("Year","Area","Subarea","NMFS_area","Trip_code","Trip_name","Species","ret_disc","Gear_code","catch","WED")
  cd_tab<- cd %>% filter(Area=="BSAI") %>% mutate(Subarea=ifelse(NMFS_area==518,"Bogoslof",
                                                                 ifelse(NMFS_area<520,"SE",
                                                                        ifelse(NMFS_area<540,"NW","Aleutians")))) %>%
    group_by(Year,Subarea,ret_disc) %>% summarise(catch=sum(catch)) %>%
    pivot_wider(names_from=c(Subarea,ret_disc),values_from=catch) %>% flextable() %>%  print(n=Inf)
  print(cd_tab)
  #--------new catch/biomass figure-------------
  #M$SSB
  #df <- data.table(Year=M$SSB[,1],SSB=2*M$SSB[,2],Catch=M$obs_catch,U=M$obs_catch/(2*M$SSB[,2]))
  #ggplot(df,aes(x=Year,y=U)) + geom_line(size=2,color="red") + .THEME + xlim(c(1990,2020)) + ylab("Catch / spawning biomass")
  #--------phase plane figure-------------------
  source("../R/do-phase.R")
  
  
  #Look at SER
  
  idx
  idx=grep("SER_Fmsy",M$fit$name)
  est = M$fit$est[idx]
  std = M$fit$std[idx]
  std/est
  
  #---Catch grid and future effort consequences---------------------------------------------------
  dc <-data.frame(Catch=M$future_catch,scen=c(seq(.5,1.5,.05),2),"catch");
  dc[1:5] <- dc[1:5]/1350
  dc <-data.frame(Year=thisyr:(thisyr+5),Catch=c(1350,M$future_catch[22,]), F=M$Future_F[22,], SSB=M$future_SSB[22,])
  dc <-data.frame(Year=thisyr:(thisyr+5),F=M$Future_F[22,], SSB=M$future_SSB[22,])
  M$Future_F
  dc$F <- dc$F/dc$F[1]
  dc$SSB <- dc$SSB/dc$SSB[1]
  df.g <- gather(dc,key=Measure,val,-Year)
  df.g
  p1 <-   df.g %>% ggplot(aes(x=Year,y=val,col=Measure)) + .THEME + geom_line(size=2) + ylim(0.25,2.00) + ggtitle(paste0("Projected trend relative to ",thisyr," given future catch=1,350 kt")) + ylab("Relative value")
  p1
  ggsave("doc/figs/future_F.pdf",plot=p1,width=7.4,height=5,units="in")
  
  # ,scen=c(seq(.5,1.5,.05),2),"catch");
  names(dc) <- c(2020:2024,"scen","var")
  db <-data.frame(SSB=M$future_SSB,scen=seq(.5,1.5,.05),"SSB");
  db<- cbind(db[,2:6]/db[,1],db[,7:8])
  db$scen <- dc[,1]
  names(db) <- c(2018:2022,"scen","var")
  df.g <- rbind( gather(dc,year,val,1:5) , gather(db,year,val,1:5) )
  df.g$year <- as.numeric(df.g$year)
  head(df.g)
  # df.g %>% filter(var=="catch") %>% ggplot(aes(x=year,y=scen,z=val)) + .THEME + geom_contour() + ()
  df.g %>% filter(Measure=="catch") %>% select(year,scen,val) %>% ggplot(aes(x=year,y=scen)) + .THEME + geom_density_2d()
  
  # Look at some of the retros and mcmc runs
  # mcmc first
  srdf <-read.table(paste0(.MODELDIR[thismod],"/mcmc/mcSRR.rep"))
  srdf <-read_table("mcmc/mcSRR.rep")
  srdf
  srsm <- sample(1:5000,2000)
  names(srdf) <- c("draw","stock","recruits")
  srdf<-srdf %>% filter(draw %in% srsm) %>% mutate(stock=as.numeric(stock),recruits=as.numeric(recruits),draw=as.factor(draw)) #%>%
  srdf %>% ggplot(aes(x=stock,y=recruits,group=draw)) + geom_line(size=.1,alpha=.4,color="coral") + theme_few() +
    xlim(c(0,6000)) + ylim(c(0,50000))
  srsm
  
  # retros maybe
  fmdf<-NULL
  i=2
  for (i in 0:10) {
    mn=paste0("r_Fmort_",i)
    
    #---Compare selectivity for base w/ vast
    #df <- data.frame(sel=modlst[[2]]$sel_fut,Age=1:15,Model="base")
    #df <- rbind(df,data.frame(sel=modlst[[4]]$sel_fut,Age=1:15,Model="With 2021"))
    ##df %>% group_by(Model) %>% summarize(mean(sel))
    #p1 <- df %>% ggplot(aes(x=Age,y=sel,color=Model)) + geom_line(size=1.5) + theme_few() + ylab("Selectivity") + scale_x_continuous(breaks=1:15)
    #ggsave("doc/figs/sel_comp_vast.pdf",plot=p1,width=8,height=4.0,units="in")
    #---Age diversity
    df <- data.frame(Year=M$Yr,Age=M$H,Measure="Population Age\n diversity")
    df <- rbind(df,data.frame(Year=M$Yr,Age=M$avg_age_mature,Measure="SSB Age\n diversity"))
    p1 <- df %>% filter(Year>1979) %>% ggplot(aes(x=Year,y=Age,color=Measure)) + geom_line(size=2) + theme_few() + scale_x_continuous(limits=c(1980,2022), breaks=seq(1980,2022,5));p1
    ggsave("doc/figs/age_diversity.pdf",plot=p1,width=8,height=3.0,units="in")
    #---Recruit
    #p1 <- plot_recruitment(modlst[c(1,2,3)],xlim=c(2008.5,2019.5));p1
    #p1 <- plot_ssb(modlst[c(1,2,3)],xlim=c(2008.5,2019.5));p1
    #p1 <- plot_bts(modlst[c(1,2,3)],xlim=c(2008.5,2019.5));p1
    #p1 <- plot_Nage_3(modlst[c(1,2)],xlim=c(2000.5,2023.5));p1
    p1 <- plot_recruitment(modlst[c(3,2)],xlim=c(2000.5,2022.5))+ylab("");p1
    
    p2 <- plot_recruitment(modlst[c(2,3)],xlim=c(2000.5,2022.5))+ylab("");
    p3 <- plot_recruitment(modlst[c(3,4)],xlim=c(2000.5,2022.5))+ylab("");
    p4 <- plot_recruitment(modlst[c(4,5)],xlim=c(2000.5,2022.5))+ylab("");
    p5 <- plot_recruitment(modlst[c(5,6)],xlim=c(2000.5,2022.5))+ylab("");
    p6 <- plot_recruitment(modlst[c(6,7)],xlim=c(2000.5,2022.5))+ylab("");
    p7 <- plot_recruitment(modlst[c(7,8)],xlim=c(2000.5,2022.5))+ylab("");
    p8 <- plot_recruitment(modlst[c(8,9)],xlim=c(2000.5,2022.5))+ylab("");
    p1 <- plot_recruitment(modlst[c(1,8)],xlim=c(2000.5,2022.5))+ylab("");p1
    (p1+p2)/
      (p3+p4)/
      (p5+p6)/
      (p7+p8)
    #p2 <- plot_ssb(modlst[c(1:7)],xlim=c(2008.5,2021.5),breaks=seq(2008,2021,by=2),alpha=.2); p2
    #p2 <- plot_ssb(modlst[c(1,4)],xlim=c(2008.5,2021.5),breaks=seq(2008,2021,by=2),alpha=.2); p2
    #p2 <- plot_ssb(modlst[c(1,5)],xlim=c(2008.5,2021.5),breaks=seq(2008,2021,by=2),alpha=.2); p2
    #---SSB-------------------------
    modlst[[3]]$SSB
    p1 <- plot_ssb(modlst[c(1,6,5,4)],xlim=c(2008.5,2022.5),breaks=seq(2008,2024,by=2),alpha=.2); p1
    
    p1 <- plot_ssb(modlst[c(6,5,4)],xlim=c(2008.5,2022.5),breaks=seq(2008,2024,by=2),alpha=.2); p1
    p1 <- plot_recruitment(modlst[c(1,2)],xlim=c(2008.5,2022.5),breaks=seq(2008,2024,by=2),alpha=.2); p1
    p1 <- plot_ssb(modlst[c(6,5,4)],xlim=c(2008.5,2022.5),breaks=seq(2008,2024,by=2),alpha=.2); p1
    p1 <- plot_ssb(modlst[3],xlim=c(2008.5,2022.5),breaks=seq(2008,2024,by=2),alpha=.2); p1
    p2 <- plot_ssb(modlst[c(8,9)],xlim=c(2008.5,2022.5),breaks=seq(2008,2024,by=2),alpha=.2); p2
    # Comparing base with 2 vast configurations
    p3 <- p1/p2; p3
    ggsave("doc/figs/mod_bridge.pdf",plot=p1,width=8,height=5.0,units="in")
    
    #---BTS Fit---------------------
    # Comparing base with 2 vast configurations
    #p1 <- plot_recruitment(modlst[c(2,4,5)],xlim=c(2010.8,2019.5));p1
    p1 <- plot_bts(modlst[c(3,2)],xlim=c(1981.5,2023.5),ylim=c(0,15500)) ;p1
    ggsave("doc/figs/mod_bts_fit.pdf",plot=p1,width=8,height=4.0,units="in")
    #p1 <- plot_ssb(modlst[c(3,4)],xlim=c(2000.5,2020.5),alpha=.2); p1
    #plot_recruitment(modlst,xlim=c(2004.5,2018.5))
    #plot_ssb(modlst,xlim=c(2004.5,2020.5),alpha=.1)
    #plot_ssb(modlst[c(2,3)],xlim=c(2004.5,2020.5),alpha=.1)
    #plot_bts(modlst,xlim=c(1981.5,2019.5),ylim=c(0,35000))
    #ggsave("doc/figs/mod_eval0a.pdf",plot=p1,width=6,height=4,units="in")
    p1 <- plot_bts(modlst[c(1,2)],xlim=c(2009.5,2022.5),ylim=c(0,15000)) ;p1
    #  plot_bts(modlst[c(3)],xlim=c(1982,2019.5),ylim=c(0,15000))
    ##  plot_bts(modlst[c(2)],xlim=c(1982,2019.5),ylim=c(0,15000))
    ggsave("doc/figs/mod_bts_bridge.pdf",plot=p1,width=8,height=4,units="in")
    #M$maxabc2s
    #M$sel_fut
    #  names(M)
    #M$sel_fsh
    #sel
    #M<- modlst[[thismod]]
    #---Selectivity-----------------
    yr=c(M$Yr,2023);sel<-rbind(M$sel_fsh,M$sel_fut)
    p1 <- plot_sel(Year=yr,sel=sel,scale=3); p1
    #p1 <- plot_sel();p1
    #dtmp <- p1$data %>% filter(Year==2022)
    #p1 <- p1 + geom_density_ridges(data=dtmp, stat="identity",fill="gold",alpha=.02) ;p1
    #p1 <- p1 + geom_density_ridges(data=dtmp, stat="identity",fill="gold",alpha=.2) ;p1
    
    #plot_sel(sel=Alt$sel_fsh)
    ggsave("doc/figs/mod_fsh_sel.pdf",plot=p1,width=4,height=9,units="in")
    
    p1 <- plot_sel(sel=M$sel_bts,styr=1982,fill="darkblue") ;p1
    #plot_sel(sel=M$sel_eit,styr=1994,fill="darkblue")
    M<-modlst[[1]]
    p1 <- plot_sel(sel=M$sel_ats[,-1],styr=2020) ;p1
    ggsave("doc/figs/mod_bts_sel.pdf",plot=p1,width=4,height=8,units="in")
    # p1 <- plot_mnage(modlst[c(2,3)])
    #p1 <- plot_mnage(modlst[thismod]) ;p1
    p1 <- plot_mnage(modlst[thismod]) ;p1 # Note used model 1 for figure...because of kludge for age compos
    ggsave("doc/figs/mod_mean_age.pdf",plot=p1,width=5.8,height=8,units="in")
    p1 <- plot_bts(modlst[thismod]) ; p1
    ggsave("doc/figs/mod_bts_biom.pdf",plot=p1,width=5.2,height=3.7,units="in")
    
    
    ggsave("doc/figs/mod_ats_bridging.pdf",plot=p1,width=5.2,height=3.7,units="in")
    
    #  p1 <- p1+ geom_vline(xintercept=2006.5,color="grey",size=1)
    #p1 <- p1+scale_y_log10()
    p1 <- plot_ats(modlst[c(1,8)]) +theme_few(base_size=11) ;p1
    ggsave("doc/figs/mod_ats_biom.pdf",plot=p1,width=9.2,height=3.7,units="in");
    p1 <- plot_avo(modlst[c(1,7,8)], ylim=NULL) + xlim(c(2005,2023)) + facet_grid(Model~.,scales='free')+theme_few(base_size = 18);p1
    #  p1 <- plot_avo(modlst[c(2)]) ;p1
    ggsave("doc/figs/mod_avo_fit.pdf",plot=p1,width=9.2,height=4.7,units="in")
    #p1 <- plot_cope(modlst[[2]]) ;p1
    p1 <- plot_cpue(modlst[[thismod]])
    ggsave("doc/figs/mod_cpue_fit.pdf",plot=p1,width=5.2,height=3.7,units="in")
    p1 <- plot_recruitment(modlst[thismod],xlim=c(1963.5,2021.5),fill="yellow");p1
    p1 <- plot_recruitment(modlst[c(1:2)],xlim=c(2000.5,2022.5),fill="yellow") + theme_few(base_size = 14);p1
    p1 <- plot_ssb(modlst[c(1:2)],xlim=c(2000.5,2022.5),fill="yellow");p1
    ggsave("doc/figs/mod_rec.pdf",plot=p1,width=9,height=4,units="in")
    ggsave("doc/figs/mod_rec.pdf",plot=p1,width=9,height=4,units="in")
    
    #  p1 <- plot_srr(modlst[c(thismod)],alpha=.2,xlim=c(0,5200),ylim=c(0,77000));p1
    #  ggsave("doc/figs/mod_srr_sq_a.pdf",plot=p1,width=9,height=4,units="in")
    
    #srr_dir  = c("../runs/sr0/","../runs/sr1/", "../runs/sr2/","../runs/sr3/")
    #srr_names  = c("Standard","Ignore 1978 YC","Diffuse prior","FMSY=F35%")
    #fn       <- paste0(srr_dir,"pm")
    #srrlst   <- lapply(fn, read_admb)
    #names(srrlst) <- c(srr_names)
    #nsrrmods <- length(srr_names)
    ##for (i in 1:nmods) {
    #p1 <- plot_srr(srrlst[c(1)],alpha=.2,xlim=c(0,5200),ylim=c(0,70000)                    );p1
    #p1 <- plot_srr(srrlst[c(1)],alpha=.2,xlim=c(0,5200),ylim=c(0,70000),yrsin=1978,sizeout=1,sizein=5);p1
    #p1 <- plot_srr(srrlst[c(1)],alpha=.2,xlim=c(0,5200),ylim=c(0,70000),yrsin=c(1977,1979:2018),sizeout=1,sizein=3);p1
    #p1 <- plot_srr(srrlst[c(1,8)],alpha=.2,xlim=c(0,5200),ylim=c(0,80000),sizeout=3,sizein=3);p1
    #p1 <- plot_srr(srrlst[c(1)],alpha=.2,xlim=c(0,5200),ylim=c(0,70000),sizeout=3,sizein=1);p1
    #p1 <- plot_srr(srrlst[c(1)],alpha=.2,xlim=c(0,5200),ylim=c(0,70000),sizeout=1,sizein=3);p1
    #p1 <- plot_srr(srrlst[c(2)],alpha=.2,xlim=c(0,5200),ylim=c(0,70000),yrsin=c(1977,1979:2018),sizeout=1,sizein=3);p1
    #p1 <- plot_srr(srrlst[c(1,3)],alpha=.2,xlim=c(0,5200),ylim=c(0,95000),yrsin=c(1977,1979:2018),sizeout=1,sizein=3);p1
    #p1 <- plot_srr(srrlst[c(1,4)],alpha=.2,xlim=c(0,5200),ylim=c(0,70000),yrsin=c(1977,1979:2018),sizeout=1,sizein=3);p1
    #p1 <- plot_srr(srrlst[c(1,2)],alpha/ssb=.2,xlim=c(0,5200),ylim=c(0,100000));p1
    #
    #  p1 <- plot_srr(modlst[c(1,8)],alpha=.2,xlim=c(0,5200),ylim=c(0,80000),sizeout=2,sizein=4,yrsin=c(1977,1979:2020));p1
    
    modlst
    p1 <- plot_srr(modlst[c(4:6)],alpha=.2,xlim=c(0,5200),ylim=c(0,80000),sizeout=2,sizein=4,yrsin=c(1977,1979:2020));p1
    #  p1 <- plot_srr(modlst[c(1,2)],alpha=.2,xlim=c(0,5200),ylim=c(0,100000));p1
    ggsave("doc/figs/mod_srr_M",plot=p1,width=9,height=4,units="in")
    #p1 <- plot_srr(modlst[c(3,5)],alpha=.2,xlim=c(0,5200),ylim=c(0,80000));p1
    #p1 <- plot_srr(modlst[c(3,6)],alpha=.2,xlim=c(0,5200),ylim=c(0,80000));p1
    #p1 <- plot_srr(modlst[c(1,8)],alpha=.2,xlim=c(0,5200),ylim=c(0,80000));p1
    
    #p1 <- plot_srr(srrlst[c(1)],alpha=.2,xlim=c(0,5200),ylim=c(0,70000),
    #Save status quo (sq) and c and d for comparisons
    #ggsave("doc/figs/mod_srr_sq_c_d.pdf",plot=p1,width=9,height=4,units="in")
    #names(modlst) modlst[[3]]$abc1s modlst[[4]]$abc1s modlst[[3]]$maxabc1s
    #p1 <- p1 + theme_few(base_size=16) p1
    #p1 <- plot_srr(modlst[c(thismod)],alpha=.2,xlim=c(0,4500),ylim=c(0,80000),
    p1 <- plot_srr(modlst[c(1,6)],alpha=.2,xlim=c(0,4500),ylim=c(0,80000),
                   yrsin=c(1977,1979:2020),sizeout=2,sizein=4);p1
    ggsave("doc/figs/mod_srr.pdf",plot=p1,width=7.4,height=4.9,units="in")
    #p1 <- plot_srr(modlst[c(2,4)],alpha=.2,xlim=c(0,5200),ylim=c(0,75000))
    #ggsave("doc/figs/bholt_ricker.pdf",plot=p1,width=7.4,height=3.9,units="in")
    af_title <- paste(thisyr,"Assessment")
    #plot_agefit(modlst[[thismod]],case_label=af_title,gear="bts",type="survey")
    pdf("doc/figs/mod_bts_age.pdf",width=6,height=8)
    plot_agefit(modlst[[thismod]],case_label=af_title,gear="bts",type="survey",styr=1982,ageplus=10)
    dev.off()
    
    pdf("doc/figs/mod_ats_age.pdf",width=6,height=8)
    plot_agefit(modlst[[2]],case_label=af_title,gear="ats",type="survey",styr=2001)
    plot_agefit(modlst[[1]],case_label=af_title,gear="ats",type="survey",styr=2001)
    dev.off()
    #  plot_agefit(modlst[[2]],case_label=af_title,gear="fsh",type="fishery",styr=1992)
    pdf("doc/figs/mod_fsh_age.pdf",width=6,height=8)
    plot_agefit(modlst[[thismod]],case_label=af_title,gear="fsh",type="fishery",styr=1992,ageplus=10)
    plot_agefit(modlst[[thismod]],case_label=af_title,gear="fsh",type="fishery",styr=1978,ageplus=10)
    dev.off()
    #plot_agefit(modlst[[2]],case_label=af_title,gear="fsh",type="fishery",styr=1992,ageplus=10)
    #plot_agefit(M,case_label=af_title,gear="fsh",type="fishery")
    
    #---Data influence------------
    #XXXXXX
    #CAB_names <- factor(c("Model 16.1 \nlast year", "Catch added", "Add ATS", "Add BTS", "Add AVO"),levels=c("Model 16.1 \nlast year", "Catch added", "Add ATS", "Add BTS", "Add AVO"))
    #CAB_names <- c("Model 16.1 \nlast year", "Catch added","C")
    #factor(sizes, levels = c("small", "medium", "large"))
    #.CABMODELDIR = c( "../runs/lastyr/", "../runs/C/","../runs/CA/","../runs/CAB/","../runs/CABA/")
    #CAB_names <- (c("Model 16.2 \nlast year", "Catch added", "Add Catch-age", "Add DB USV", "VAST USV")) ,levels=c("Model 16.2 \nlast year", "Catch added", "Add Catch-age", "Add DB USV", "VAST USV"))
    #  .CABMODELDIR = c( "../runs/2020/", "../runs/ca/","../runs/cab/","../runs/base/")
    
    # Read report file and make list
    #fn       <- paste0(.CABMODELDIR, "pm")
    #CABmodlst   <- lapply(fn, read_admb)
    CAB_names <- (c("last year", "Add 2021 catch", "Add BTS", "Add ATS", "Add AVO"))
    CABmodlst   <- modlst[c(2,4,6,7,8)]
    names(CABmodlst)
    str(CABmodlst)
    
    #CAB_names <- c("last year", "Catch added", "+Catch-age", "+BTS", "+AVO")
    #  CAB_names <- c("last year", "Catch added","Agin")
    names(CABmodlst) <- CAB_names
    names(CABmodlst)
    nmods <- length(CAB_names)
    #for (i in 1:nmods) CABmodlst[[i]] <- c(CABmodlst[[i]],get_vars(CABmodlst[[i]]))
    #p1 <-  plot_ssb(CABmodlst[c(1,3,5)],xlim=c(2009.5,2019.5),alpha=.1,ylim=c(0,5200)); p1
    A <- .get_ssb_df(CABmodlst)
    names(A)
    A$Model <- factor(A$Model ,levels= c("last year", "Add 2021 catch", "Add BTS", "Add ATS","Add AVO"))
    p1 <- ggplot(A,aes(x=year,y=ssb,ymin=lb,ymax=ub,fill=Model)) + labs(x = "Year", y = "Spawning biomass") +
      theme_few(base_size=14) +
      expand_limits(y = 0) + geom_line(aes(linetype=Model,color=Model),size=1.2) +
      scale_x_continuous(limits=c(2010,2022), breaks=2010:2022)  +
      geom_ribbon(alpha = 0.1)  ;p1
    ggsave("doc/figs/mod_data.pdf",plot=p1,width=8,height=4,units="in")
    #plot_ssb(CABmodlst,xlim=c(2009.5,2020.5),alpha=.05,ylim=c(0,6000),breaks=2009:2020)
    #p2 <-  plot_recruitment(CABmodlst,xlim=c(2009.5,2020.5),alpha=.05) ;p2
    #p2 <-  p2 + scale_fill_discrete(labels=CAB_names) + scale_color_discrete(labels=CAB_names) + theme_few()
    #p3 <- p1/p2
    #p1
    #plot_ssb(modlst[],xlim=c(2004.5,2018.5),alpha=.1,ylim=c(0,5200))
    #for (i in 1:length(mod_names)) modlst[[i]] <- c(modlst[[i]],get_vars(modlst[[i]]))
    #plot_recruitment(CABmodlst[c(1,5)],xlim=c(2000.5,2019.5),fill="yellow")
    
    #--q sensitivity----------
    # taken from tab.Rmd (proflst)
    #names(proflst)
    #.MODELFN  = c("r_1","r_2","r_3","r_4","r_5")
    #prof_names  = c("CV70%","CV50%","CV20%","CV10%","CV05%")
    #fn       <- paste0("../runs/q_sens/prof/",.MODELFN)
    #proflst   <- lapply(fn, read_admb)
    #proflst[[6]] <- M
    #names(proflst) <- c(prof_names,"16.1")
    #nmods <- length(prof_names)
    #for (i in 1:nmods) {
    ##print(i)k
    #proflst[[i]] <- c(proflst[[i]],get_vars(proflst[[i]]))
    #}
    
    #p1 <- plot_ssb(proflst[c(2,6)],ylim=c(0,5500),xlim=c(1990.5,2019.5),alpha=.1) + scale_x_continuous(limits=c(1990,2020),breaks=seq(1990,2020,5))
    #p2 <- plot_ssb(proflst[c(5,6)],ylim=c(0,5500),xlim=c(1990.5,2019.5),alpha=.1)+ scale_x_continuous(limits=c(1990,2020),breaks=seq(1990,2020,5))
    #p3 <- arrangeGrob(p1,p2,nrow=2)
    #ggsave("doc/figs/q_sens_ssb.pdf",plot=p3,width=5.2,height=7.5,units="in")
    #q<-NULL
    #for (i in 1:5)
    #q <- rbind(q,data.frame(Year=1982:2019,Model=names(proflst[i]),q=rowMeans(proflst[[i]]$q_bts_3_8)))
    #
    #head(q)
    #av <- read.table("../runs/dat/avail.dat",header=TRUE)
    #av <- av %>% transmute(Year=Year,Model="COLE",q=q)
    #q <- rbind(q,av)
    #q <- q %>% group_by(Model) %>% mutate(mq=mean(q)) %>% ungroup() %>% transmute(Year=Year,Model=Model,q=q/mq)
    #
    #q %>% group_by(Model) %>% summarize(mean(q))
    #p3 <- q %>% filter(Model %in% c("CV70%")) %>% ggplot(aes(y=q,x=Year,col=Model)) + geom_line(size=1.3) + theme_few() +  scale_x_continuous(limits=c(1987,2019),breaks=seq(1987,2019,by=3)) ;p3
    #p3 <- q %>% filter(Model %in% c("COLE","CV05%","CV50%")) %>% ggplot(aes(y=q,x=Year,col=Model)) + geom_line(size=1.3) + theme_few() +  scale_x_continuous(limits=c(1987,2019),breaks=seq(1987,2019,by=3)) ;p3
    #ggsave("doc/figs/q_sens.pdf",plot=p3,width=8.2,height=3.5,units="in")
    
    #--CV sensitivity----------
    #.MODELFN  = c("../runs/usv/","../runs/usv_vast/","../runs/estcvdb/","../runs/estcvvast/")
    #prof_names  = c("DB CV20%","VAST CV20%","DB Sampling CV","VAST est. CV")
    #fn       <- paste0(.MODELFN,"pm")
    #cvlst   <- lapply(fn, read_admb)
    ##proflst[[6]] <- M
    #names(cvlst) <- c(prof_names)
    #nmods <- length(prof_names)
    #for (i in 1:nmods) {
    #cvlst[[i]] <- c(cvlst[[i]],get_vars(cvlst[[i]]))
    #}
    #p1 <- plot_ssb(cvlst, ylim=c(0,6500),breaks=seq(1990,2020,2), xlim=c(1990.5,2020.5),alpha=.1) ;p1
    #
    #p1 <- plot_ats(cvlst[c(1:2)]) +theme_few(base_size=11) ;p1
    #p2 <- plot_ats(cvlst[c(3:4)]) +theme_few(base_size=11) ;p2
    #p5 <- p1/p2
    #p5
    #ggsave("doc/figs/mod_ats_eval1.pdf",plot=p5,height=7,width=6.0,units="in");
    #p3 <- plot_ssb(cvlst[c(1,3)] , ylim=c(0,6000),breaks=seq(1990,2020,5), xlim=c(1990.5,2020.5),alpha=.1) ;p3
    #p4 <- plot_ssb(cvlst[c(2,4)] , ylim=c(0,6000),breaks=seq(1990,2020,5), xlim=c(1990.5,2020.5),alpha=.1) ;p4
    #p5 <- p3/p4
    #ggsave("doc/figs/mod_ats_eval2.pdf",plot=p5,height=7,width=6.0,units="in");
    
    #---SSB figure----------------------------
    df <- rbind(
      rbind(data.frame(Model="This year",Year= M$SSB[,1], SSB=M$SSB[,2], lb=M$SSB[,4], ub=M$SSB[,5]),
            data.frame(Model="This year",Year= nextyr:(nextyr+4), SSB=M$future_SSB[4,2:6], lb = M$future_SSB[4,2:6] -2*M$future_SSB.sd[4,2:6], ub = M$future_SSB[4,2:6] +2*M$future_SSB.sd[4,2:6])),
      rbind(data.frame(Model="Last year",Year= P$SSB[,1], SSB=P$SSB[,2], lb=P$SSB[,4], ub=P$SSB[,5]),
            data.frame(Model="Last year",Year= (nextyr-1):(nextyr+3), SSB=P$future_SSB[4,2:6], lb = P$future_SSB[4,2:6] -2*P$future_SSB.sd[4,2:6], ub = P$future_SSB[4,2:6] +2*P$future_SSB.sd[4,2:6]))
    )
    p1 <- ggplot(df,aes(x=Year,y=SSB,ymax=ub,ymin=lb,fill=Model)) + geom_ribbon(alpha=.6) + geom_line() +  theme_few() +
      scale_x_continuous(limits=c(2002,2028),breaks=seq(2002,2028,2)) +
      ylab("Female spawning biomass (kt)") +
      geom_vline(xintercept=2022,col="grey",size=1.2); p1
    ggsave("doc/figs/proj_ssb.pdf",plot=p1,width=7.4,height=4.5,units="in")
    
    #---R/S------------------
    nyrs=length(M$SSB[,1])
    dt <- data.table(yr=M$SSB[1:nyrs,1],ssb= M$SSB[1:nyrs,2], r=M$R[2:(nyrs-1),2] )
    dt <- dt %>% mutate(Year=substr(as.character(yr),3,4),rs= log(dt$r/dt$ssb))
    head(dt)
    p1 <- ggplot(dt,aes(x=yr,y=rs)) + geom_point(size=4,col="red") + geom_line() +geom_smooth() + theme_few(base_size=12) + ylab("ln(Recruits/spawning biomass)") + xlab("Year")
    #ggplot(dt,aes(x=ssb,y=rs)) + geom_point(size=4,col="red") + geom_path() +geom_smooth(method="lm") + .THEME + ylab("ln(Recruits/spawning biomass)") + xlab("Spawning biomass (kt)")
    p2 <- ggplot(dt,aes(x=ssb,y=rs,label=Year)) + geom_text() + geom_path(size=.5,alpha=.4) +geom_smooth(method="lm") + theme_few(base_size=12) +
      ylab("ln(Recruits/spawning biomass)") + xlab("Spawning biomass (kt)") +  guides(size=FALSE,shape=FALSE,alpha=FALSE,col=FALSE)
    p3 <- p1 / p2;p3
    ggsave("doc/figs/mod_rs.pdf",plot=p3,width=5.2,height=7.5,units="in")
    
    p1 <- plot_ser(modlst[thismod],xlim=c(1964,thisyr+1),alpha=.7) + scale_x_continuous(breaks=seq(1965,thisyr+1,5))
    ggsave("doc/figs/mod_ser.pdf",plot=p1,width=9.2,height=7.0,units="in"); p1
    
    #---fishing mortality mod_F.pdf-----------------------------------------------------------------
    df <-data.frame(Year=M$Yr,M$F); names(df) <- c("Year",1:15); df.g <- gather(df,age,F,2:16,-Year)
    p1 <- df.g %>% mutate(age=as.numeric(age)) %>% filter(age<11)%>% ggplot(aes(y=age,x=Year,fill=F)) + geom_tile() + .THEME + ylab("Age")+ geom_contour(aes(z=F),color="darkgrey",size=.5,alpha=.4) +
      scale_fill_gradient(low = "white", high = "red") + scale_x_continuous(breaks=seq(1965,thisyr,5)) + geom_line(data=df.g[df.g$age=="6",],aes(x=Year,y=F*10)) +
      annotate("text", label = "Age 6 F (x10)" , x = 2015, y = 1.2, size = 5, colour = "black") + scale_y_continuous(breaks=seq(0,10,1))
    ggsave("doc/figs/mod_F.pdf",plot=p1,width=9.2,height=6.0,units="in")
    
    #---Historical assessment retrospectives--------------------------------------------------------
    dd <- as.data.frame(read.csv("data/Age3history.csv",header=TRUE))
    head(dd)
    names(dd) <- c("Year",2021:2006,2001:1998)
    dd.g <- pivot_longer(dd,cols=2:21,names_to="Assessment",values_to="Biomass")
    head(dd.g)
    # this line to add current year estimate!!
    t <- data.frame(Year=1964:(thisyr+1), Assessment=thisyr,Biomass=c(M$age3plus,M$age3plus1)  )
    dd.g <- rbind(dd.g,t) %>% filter(as.numeric(Assessment) >2006)
    tmp <- dd.g %>% filter(Year>2006,Year==1+as.numeric(Assessment)) # %>% summarise(max(Year))
    p1 <- ggplot(dd.g,aes(x=Year,y=Biomass,color=Assessment)) + geom_line(alpha=.8,size=.75) +
      scale_x_continuous(breaks=seq(1980,thisyr+2,2),limits=c(1980,thisyr+1))  +  xlab("Year") + ylim(0,16000) + ylab("Age 3+ biomass (kt)") +
      geom_point(data=tmp,size=2) + theme_few() +
      guides(size=FALSE,shape=FALSE,alpha=FALSE,col=FALSE)
    p1
    ggsave("doc/figs/mod_hist.pdf",plot=p1,width=9.2,height=4.0,units="in")
    
    #----------------------------------------------------------
    # Extract Fmsy for different selectivity years
    
    idx=grep("msy2_dec",M$fit$name)
    length(idx)
    
    fdf <- data.frame(year=rep(2013:2022,2),
                      Source=M$fit$name[idx],
                      est = M$fit$est[idx],
                      std = M$fit$std[idx])
    unique(fdf$Source)
    
    p1<-  fdf %>% filter(Source!="Fmsy2_decwt") %>%
      ggplot(aes(x=year+.1,y=est,ymax=est+2*std,ymin=est-2*std))+
      geom_errorbar(width=.3) + geom_point(size=5,color="purple") + theme_few(base_size=12) + xlab("Year selected for MSY calculation") +
      scale_x_continuous(breaks=seq(2013,thisyr,1),limits=c(2012,thisyr+1))  +
      ylab("Fmsy");p1
    ggsave("doc/figs/fmsy_sel_hist.pdf",plot=p1,width=5.2,height=4.0,units="in")
    
    ##
    sel <- M$sel_fsh
    seldf=NULL
    yrs <- (thisyr-10):(thisyr)
    lrow<-dim(sel)[1]
    seldf<-rbind(seldf, data.frame(yrs,rep(thisyr,11),sel[(lrow-10):lrow,])) %>% rbind(c("proj",thisyr,M$sel_fut))
    #assign(mn,read_rep(rn))
    names(seldf) <- c("Year","Assessment",1:15)
    seldfm <- seldf %>%  pivot_longer(cols=3:17,names_to="age",values_to="Selectivity") %>%
      mutate(
        age=as.numeric(age),
        Selectivity=as.numeric(Selectivity),
        Assessment=as.numeric(Assessment),
        case = ifelse(Year=="proj","Proj.","Est."),
        Year=as.numeric(ifelse(Year=="proj",Assessment+1,Year))
      )
    glimpse(seldfm)
    assdf <- seldfm %>% filter(Year>Assessment) %>% mutate(age=as.factor(age),Year=as.factor(Year))
    yrdf  <- seldfm %>% filter(Year<=Assessment,Year>thisyr-10) %>%mutate(age=as.factor(age),Year=as.factor(Year))
    p1<- seldfm %>% filter(age<9,Year>thisyr-10)%>% group_by(Year,Assessment,case) %>%
      summarise(mnage=sum(Selectivity*age)/sum(Selectivity)) %>%
      ggplot(aes(x=Year,y=mnage,shape=case,color=case)) + geom_point(size=2) +
      theme_few() + ylim(c(0,10)) + ylab( "Mean age selected")+
      scale_x_continuous(breaks=seq(2010,2022,by=2))
    p1
    p1 <-    ggplot(assdf,aes(x=age,y=Selectivity,color=case,group=Year)) +
      geom_point(size=.5) + geom_line() + theme_few() +
      facet_grid(Year~.) + geom_point(data=yrdf,size=.5) + facet_grid(Year~.)
    p1
    ggsave("doc/figs/retro_sel.pdf",plot=p1,width=5.2,height=9.0,units="in")
    #plot_recruitment(retouts,xlim=c(1990,2017),rel=T,legend=FALSE,alpha=.2)
    
    
    # Plot of regimes on base model
    
    regime=c("1964-77", "1978-present", "1978-99", "1978-89", "1990-present", "1990-99", "2000-present", "1964-present")
    hlr <- M$regime[2]
    p3 <- tibble(regime=regime,Mean=M$regime,ub=Mean+2*M$regime.sd,lb=Mean-2*M$regime.sd) %>%
      ggplot(aes(x=regime,y=Mean,ymax=ub,ymin=lb)) + geom_linerange(size=1.5) + theme_few(base_size=12) +
      ylab("Mean recruitment (age 1)") + geom_point(size=4,color="red") + geom_hline(yintercept=hlr,linetype=2)
    ggsave("doc/figs/mod_regimes.pdf",plot=p3,width=9.2,height=5.0,units="in")
    
    #---Fits to ats age mod_ats_age.pdf-------------------------------------------------------------
    pdf("doc/figs/mod_ats_age.pdf",width=6,height=8)
    plot_agefit(M,case_label=paste(thisyr,"Assessment"),gear="ats")
    dev.off()
    
    #----Read in retro results-----------------
    i=0
    thismod <- 2 # the selected model
    getret <- function(nyrs=15, mod=1){
      retouts<- list()
      for (i in 0:nyrs) {
        rn=paste0(.MODELDIR[mod],"retro/r_",i,".rep")
        mn=paste0("r_",i)
        assign(mn,read_rep(rn))
        retouts[[mn]] <- (get(mn))
      }
      return(retouts)
    }
    ret1<-getret(mod=1,nyrs=20)
    ret2<-getret(mod=2,nyrs=20)
    ret3<-getret(mod=3,nyrs=20)
    p1 <- plot_ssb(ret1,xlim=c(2000,thisyr),legend=F,breaks=seq(2000,2022,2),ylim=c(0,6900)) + xlab("") +ggtitle("Base")+ coord_cartesian(ylim=c(0,NA));p1
    p2 <- plot_ssb(ret2,xlim=c(2000,thisyr),legend=F,breaks=seq(2000,2022,2),ylim=c(0,6900))+ggtitle("w/o ATS") + coord_cartesian(ylim=c(0,NA));p2
    p3 <- plot_ssb(ret3,xlim=c(2000,thisyr),legend=F,breaks=seq(2000,2022,2),ylim=c(0,6900))+ggtitle("w/o BTS") + coord_cartesian(ylim=c(0,NA));p3
    p3 <- p1/p2   + plot_annotation(title = 'Spawning biomass with and without acoustic trawl survey data',
                                    subtitle = 'EBS walleye pollock assessment', caption = 'Disclaimer: Draft results, please do not cite' ); p3
    ggsave("doc/figs/retcompSSB.pdf",plot=p3,width=7.2,height=9.0,units="in")
    # recruitment
    p1 <- plot_R_rel(ret1,xlim=c(thisyr-15,thisyr-1),legend=F,rel=FALSE,ylim=c(0,120000),ylab="Age-1 recruitment") + xlab("") +ggtitle("Base")+coord_cartesian(ylim=c(0,125000));p1
    p2 <- plot_R_rel(ret2,xlim=c(thisyr-15,thisyr-1),legend=F,rel=FALSE,ylim=c(0,120000),ylab="Age-1 recruitment") + ggtitle("w/o ATS") + coord_cartesian(ylim=c(0,125000));p2
    p3 <- p1/p2   + plot_annotation(title = 'Age 1 recruitment with and without acoustic trawl survey data',
                                    subtitle = 'EBS walleye pollock assessment', caption = 'Disclaimer: Draft results, please do not cite' ); p3
    ggsave("doc/figs/retcompR.pdf",plot=p3,width=7.2,height=9.0,units="in")
    
    ## Make dataframe to plot by cohort
    nyrs=15
    i=2
    retdf
    
    i=12
    
    names(ret1)
    mean(ret1[["r_0"]]$R[,2])
    get_coh <- function(ret=ret1,run="Base",nyrs=20){
      M <- as.data.frame(NULL)
      for (i in 0:nyrs) {
        mn=paste0("r_",i)
        A <- as.data.frame(ret[[mn]]$R)
        is.data.frame(A)
        A$ret <- i
        A$run <- run
        A$cohort <- A[,1]-1
        A$term_yr <- 2022-A$ret
        A$Yrs_of_data <- A$term_yr-A$cohort
        names(A) <- c("Year","est","sd","lb","ub","peel","run","cohort","term_yr","Years_data")
        M <- rbind(M,A)
      }
      return(M)
    }
    # Plot by years of data
    ret_df <- rbind(get_coh(ret1),get_coh(ret3,run="Without BTS")) |> mutate(Years_data=ifelse(run=="Base",Years_data-.14,Years_data+.14))
    ret_df <- rbind(get_coh(ret1),get_coh(ret2,run="Without ATS"),get_coh(ret3,run="Without BTS")) |> mutate(Years_data=ifelse(run=="Base",Years_data-.18,ifelse(run=="Without ATS",Years_data,Years_data+.18))  )
    
    plot_coh <- function(dat=ret_df,coh=cohsub,logscale=FALSE,grid=T){
      p1 <- dat |>  filter(cohort %in% coh) |>
        ggplot( aes(x=Years_data,label=term_yr,y=est,color=run,ymin=lb,ymax=ub)) + geom_point(size=1) + geom_errorbar(size=.5,width=0) +
        theme_few(base_size = 11) +
        ylim(c(0,NA)) +
        geom_hline(yintercept=22.5e3,color="blue",size=.1) +
        ylab("Millions") +
        ggtitle("Age-1 recruitment estimates by cohort") #+ geom_text( aes(x=mean(Yeaes()x=mean()Years_data
      if (logscale) p1 <- p1 +  scale_y_log10()
      if (grid) p1 <- p1 +facet_grid(cohort~.) else p1 <- p1 +facet_wrap(cohort~.)
      return(p1)
    }
    
    cohsub <- c(2003:2005,2008)
    cohsub <- c(2012:2013,2008)
    cohsub <- c(2012:2013,2008)
    plot_coh( grid=FALSE, coh= cohsub)
    plot_coh( grid=FALSE, coh= c(2005,2008,2012:2013,2016,2018) )
    plot_coh( grid=FALSE, coh= c(2005,2008,2012:2013,2016,2018) )
    plot_coh( grid=FALSE, coh= c(2003:2005,2008) )
    plot_coh( grid=T, coh= c(2003:2005) )
    plot_coh( grid=T, coh= c(2012:2013) )
    plot_coh(logscale=TRUE)
    
    # Plot by endyr
    ret_df <- rbind(plot_coh(ret1),plot_coh(ret2,run="Without ATS")) |> mutate(term_yr=ifelse(run=="Base",term_yr-.1,term_yr+.1))
    ggplot(ret_df |>
             #         filter(cohort %in% c(2008:2022)),
             filter(cohort %in% c(2008,2012:2013,2016,2018)),
           aes(x=term_yr,y=est,color=run,ymin=lb,ymax=ub)) + geom_point(size=2) + geom_errorbar(size=1,width=0) +
      theme_few() + facet_grid(cohort~.) + scale_y_log10() + geom_hline(yintercept=22.5e3,color="blue",size=.1) +
      ggtitle("Age-1 recruitment estimates by cohort")
    
    
    
    ggsave("doc/figs/mod_retro.pdf",plot=p3,width=7.2,height=9.0,units="in")
    p1 <- plot_R_rel(retouts,xlim=c(thisyr-10,thisyr),legend=F,rel=FALSE,ylab="Age 1 recruitment");p1
    p2 <- plot_R_rel(retouts,xlim=c(thisyr-10,thisyr),ylim=c(0.,3.5),legend=FALSE,alpha=.2);p2 <- p2+ylim(0,4); p2
    p3 <- p1/p2;p3
    ggsave("doc/figs/mod_retroR.pdf",plot=p3,width=7.2,height=9.0,units="in")
    p1 <- plot_R_rel(retouts,xlim=c(thisyr-20,thisyr),legend=F,rel=FALSE);p1
    p2 <- plot_R_rel(retouts,xlim=c(thisyr-20,thisyr),ylim=c(0.,3.5),legend=FALSE,alpha=.2);p2 <- p2+ylim(0,4); p2
    p3 <- p1/p2
    ggsave("doc/figs/mod_retroR20.pdf",plot=p3,width=7.2,height=9.0,units="in")
    p3
    ###
    # Get sel_fut from retrospectives and compare
    i=2
    getwd()
    seldf<-NULL
    for (i in 0:10) {
      mn=paste0("r_",i)
      #seldf<-rbind(seldf,
      sel <-get(mn)$sel_fsh
      yrs <- (thisyr-i-10):(thisyr-i)
      lrow<-dim(sel)[1]
      seldf<-rbind(seldf, data.frame(yrs,rep(thisyr-i,11),sel[(lrow-10):lrow,])) %>% rbind(c("proj",thisyr-i,get(mn)$sel_fut))
      #assign(mn,read_rep(rn))
      #retouts[[mn]] <- (get(mn))
      #seldf <- rbind()
    }
    names(seldf) <- c("Year","Assessment",1:15)
    seldfm <- seldf %>%  pivot_longer(cols=3:17,names_to="age",values_to="Selectivity") %>%
      mutate(
        age=as.numeric(age),
        Selectivity=as.numeric(Selectivity),
        Assessment=as.numeric(Assessment),
        case = ifelse(Year=="proj","Proj.","Est."),
        Year=as.numeric(ifelse(Year=="proj",Assessment+1,Year))
      )
    glimpse(seldf)
    glimpse(seldfm)
    tail(seldfm)
    assdf <- seldfm %>% filter(Year>Assessment) %>% mutate(age=as.factor(age),Year=as.factor(Year))
    yrdf  <- seldfm %>% filter(Year<=Assessment,Year>thisyr-10) %>%mutate(age=as.factor(age),Year=as.factor(Year))
    p1<- seldfm %>% filter(age<9,Year>thisyr-10)%>% group_by(Year,Assessment,case) %>%
      summarise(mnage=sum(Selectivity*age)/sum(Selectivity)) %>%
      ggplot(aes(x=Year,y=mnage,shape=case,color=case)) + geom_point(size=2) +
      theme_few() + ylim(c(4,8)) + ylab( "Mean age selected")+
      scale_x_continuous(breaks=seq(2010,2022,by=2))
    p1
    ggsave("doc/figs/retro_sel_mnage.pdf",plot=p1,width=5.2,height=4.0,units="in")
    
    assdf %>% print(n=Inf)
    yrdf %>% filter(Assessment==2021) %>%inner_join(assdf,join_by Year) %>% print(n=Inf)
    
    p1 <-    ggplot(assdf,aes(x=age,y=Selectivity,color=case,group=Year)) +
      geom_point(size=.8) + geom_line() + theme_few() +
      facet_grid(Year~.) + geom_point(data=yrdf,size=1) + facet_grid(Year~.)
    p1
    ggsave("doc/figs/retro_sel.pdf",plot=p1,width=5.2,height=9.0,units="in")
    #plot_recruitment(retouts,xlim=c(1990,2017),rel=T,legend=FALSE,alpha=.2)
    #plot_R_rel(retouts,xlim=c(1980,2017),ylim=c(0.5,1.5),legend=FALSE,alpha=.2)
    # Mohn's rho
    
    #icesAdvice::mohn()
    #base   -1   -2   -3   -4   -5
    #2009 0.96 0.95 0.94 0.92 0.89 0.86
    #2010 0.72 0.71 0.70 0.67 0.63 0.59
    #2011 0.82 0.79 0.78 0.73 0.67 0.60
    #2012 0.80 0.75 0.73 0.66 0.54   NA
    #2013 0.67 0.62 0.61 0.51   NA   NA
    #2014 0.74 0.66 0.63   NA   NA   NA
    #2015 0.63 0.52   NA   NA   NA   NA
    #2016 0.57   NA   NA   NA   NA   NA
    #r$> mohn(shake)
    #[1] -0.2310701
    
    retouts<-ret1
    i=2
    getMohn <- function(retouts=ret1,nyrs=20, var="SSB"){
      if (var=="SSB"){
        rc = retouts[[1]]$SSB[,2]
        ntmp=0
        rho=0
        df <- data.frame(peel=1:20,rho=1:20)
        for (i in 1:20) {
          rn <- names(retouts[i])
          dtmp = (get(paste0(rn))$SSB )
          lr   = length(dtmp[,1])
          rho  = rho +(dtmp[lr,2] -rc[lr])/rc[lr]
          df$peel[i] <- i-1
          df$rho[i] <- rho/i
          print(paste(i,rho/i))
        }
      }
      
      }
    write.csv(df,"data/mohnrho.csv")
