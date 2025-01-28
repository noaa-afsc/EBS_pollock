
#---Tasks: loop through the 5 model directories (retro1, retro2, retro3, retro4, retro5) and
#get the results for each model and retrospective peel. so 5*20 = 100 models
#Make a single data frame with the results with a column for year, assessment year, 
#estimate or prediction. Estimates or predictions are when year>= a_year (assessment year) 
#for each model and retrospective peel
library(here)
library(tidyverse)
library(ggthemes)
doc_dir=paste0(here(),"/doc/")
setwd(here("2024", "runs","m23","retro"))
theme_set(ggthemes::theme_few())
i=2
j=2
retout <- NULL
for (i in 1:5){
  setwd(here("2024", "runs/m23/retro",paste0("retro",i)))
  for (j in 0:20){
    retout <- rbind(retout, read_table(paste0("r_F40_",j,".rep")) |> 
                 select(Year, F35, Fmsy) |> mutate(Scenario=i, endyr=2024-j,
                                                   est=ifelse(Year>=(2024-j), 
                                                       "prediction", "estimate")
                                             )
                 )
  }
}
summary(retout)
glimpse(retout)
write_csv(retout, paste0(doc_dir,"retro_F40.csv"))
(retout) |> filter(Scenario==1:5, endyr %in% c(2004,2010,2015,2020, 2023),Year<(endyr)) |> ggplot(aes(x=Year,y=F35,color=as.factor(endyr))) + 
  geom_line() +  geom_point() + facet_grid(Scenario~est)  + xlim(c(NA,2026))
tt<-(retout) |> filter(Scenario==1:1, endyr %in% c(2024))
tt |> filter((endyr+1)<(Year)) |> ggplot(aes(x=Year,y=F35,color=as.factor(endyr))) + geom_line() +  geom_point() + facet_grid(Scenario~est)  + xlim(c(NA,2026))
plotly::ggplotly(retout |> filter(Year>2003) |> ggplot(aes(x=Year,y=F35,color=as.factor(endyr))) + geom_line() + 
  geom_point() + facet_grid(Scenario~est)  + xlim(c(2004,2026)) )

#---Definitions, pred_df is the prediction data frame, est_df is the estimate data frame
#  goal is to find which prediction best matches the estimates (from each retro)

pred_df <- retout |> filter(Year>2003, Year==(endyr+1), est=="prediction")  |> transmute(Year,Scenario, F35hat=F35,endyr=endyr)
glimpse(pred_df)
# > ggplot(aes(x=Year,y=F35,color=as.factor(endyr))) + geom_likne() 
 
pred_df
est_df <- retout |> filter(Year>2003,endyr==2024, est=="estimate") |> 
  transmute(Year,Scenario, F35fin=F35,endyrfin=endyr)  
# all peels
est_df <- retout |> filter(Year>2003,            est=="estimate") |> 
  transmute(Year,Scenario, F35fin=F35,endyrfin=as.factor(endyr)  )
tail(est_df)
#bdf<- left_join(pred_df,est_df, by = c("Year", "Scenario")) 
bdf<-inner_join(pred_df,est_df, by = c("Year", "Scenario")) 
#bdf<- inner_join(pred_df,est_df,by="Year") 
glimpse(bdf)
table(bdf$endyrfin)
bdf |> select(endyrfin) |> ggplot(aes(x=endyrfin)) + geom_histogram()

bdf |> mutate(Retro_year=as.factor(endyrhat)) |> 
  ggplot(aes(x=Year,y=F35hat,color=as.factor(endyrhat),shape=as.factor(Scenario))) + 
   ylab("F35%") + ylim(c(0,NA)) + #geom_point() + 
  geom_point(aes(x=Year, y=F35fin) ) #+ facet_grid(Scenario~.)  #+ xlim(c(2004,2026)

bdf |> mutate(Retro_year=as.factor(peelhat)) |> 
ggplot(aes(x=Year,y=F35,color=as.factor(peel))) + 
  ylim(c(0,NA) ) + geom_line() + geom_point(data=pred_df) + facet_grid(Scenario~.)  + xlim(c(2004,2026))
      
df <- read_table(paste0(.MODELDIR[thismod],"F40_t.rep")) 

  names(df)
  names(M)
  pt <- df %>% filter(Year>thisyr) %>% transmute(F=meanF,Fmsy,F.Fmsy=meanF/Fmsy,Bmsy=Bmsy,Yr=substr(as.character(Year),3,4))
  df <- df %>%filter(Year<=thisyr,Year>1977)  %>%
  transmute(F=meanF,Fmsy,Bmsy,SSB=SSB,F.Fmsy=meanF/Fmsy,B.Bmsy=SSB/Bmsy,Bmsy=Bmsy,Yr=substr(as.character(Year),3,4))
  tail(df)
  #df$`F/Fmsy`
  # Note that row 4 has catch of 1.15 Mt, closest to expectatoin in 2022
  M$future_catch #[4,2:3],
  M$future_F#[4,2:3],
  df2 <- data.frame(Year=(thisyr+1):(thisyr+2),
                     C=M$future_catch[5,1:2],
                     SSB=M$future_SSB[5,2:3],
                     B.Bmsy= M$future_SSB[5,2:3]/bmsy)
                     #%>%
               #mutate(Yr=substr(as.character(Year),3,4),
                     #Fmsy=c(fmsy,fmsy)) #,
                     #Fmsy=rep(fmsy,2),
                     #F.Fmsy=F/Fmsy,
                     #Bmsy=rep(bmsy,2),
  df2
  pt
  pt2
  #pt2  <- pt2 %>% transmute(F.Fmsy,B.Bmsy=SSB/Bmsy,Bmsy,Year,Yr)
  pt2 <- cbind(df2,pt)
  head(df)
         #mutate(F.Fmsy=meanF/Fmsy,B.Bmsy=SSB/Bmsy,Year=substr(as.character(Year),3,4)) %>%
  p1 <- ggplot(df,aes(x=B.Bmsy,y=F.Fmsy,label=Yr)) +
         geom_text(aes(color=as.factor(Year)),size=4,col="blue")+ .THEME + xlim(c(0,2.0))+ ylim(c(0,1.1)) + xlab("B/Bmsy") + ylab("F/Fmsy") +
         geom_label(data=pt2,size=5,fill="yellow",color="red",alpha=.4) +
         geom_line(data=pt2,color="red",alpha=.4) +
         geom_hline(size=.5,yintercept=1) + geom_vline(size=0.5,linetype="dashed",xintercept=.2) + geom_vline(size=.5,xintercept=1) + geom_path(size=.4) +
          guides(size=FALSE,fill=FALSE,alpha=FALSE,col=FALSE) ;p1
  ggsave("doc/figs/mod_phase.pdf",plot=p1,width=7.2,height=5.7,units="in")
  ggsave("doc/figs/mod_phase.png",plot=p1,width=7.2,height=5.7,units="in")

sdf <- tibble(data.frame(year=1964:2025,rbind(M$sel_fsh,M$sel_fut)))
tail(sdf)
names(sdf) <- c("Year",1:15)
wtdf <- tibble(data.frame(year=1964:2025,rbind(M$wt_fsh,M$wt_fut)))
names(wtdf) <- c("Year",1:15)
#sdf[,2:16] <- wtdf[,2:16]*sdf[,2:16]
  sdfm <- sdf %>%  pivot_longer(cols=2:16,names_to="age",values_to="Selectivity") %>%
                  mutate(
                    age=as.numeric(age),
                    Selectivity=as.numeric(Selectivity)
                    )

df <- read_table(paste0(.MODELDIR[thismod],"F40_t.rep")) #,header=TRUE)
df %>% filter(Year>2000) %>% summarise(hm=n()/sum(1/AM_fmsyr),am=mean(AM_fmsyr),1-hm/am)
df %>% filter(Year>1977) %>% summarise(hm=n()/sum(1/Fmsy),am=mean(Fmsy),1-hm/am)
df %>% filter(Year>1977) %>% ggplot(aes(x=SSB,y=Fmsy,label=Year)) + geom_text() + theme_few() + geom_path(color="grey") +
     geom_smooth(method='lm')
tail(df)
names(df)
#
wtdf
  # plot of selected age vs Fmsy
  p1 <- df %>% select(Year,Fmsy,AM_fmsyr,F35) %>% left_join(sdfm) %>%
  filter(age<11,Year>2000)%>% mutate( Year=substr(as.character(Year),3,4)) %>% group_by(Year) %>%
 summarise(F35=mean(F35), Fmsy=mean(Fmsy),mnage=sum(Selectivity*age)/sum(Selectivity)) %>%
 #ggplot(aes(y=F35,x=mnage,label=Year)) + geom_text() +
 ggplot(aes(y=Fmsy,x=mnage,label=Year)) + geom_text() +
 theme_few() + geom_path(size=.2,color="grey") + xlab( "Mean age selected") #+ geom_smooth();p1
 p1
  ggsave("doc/figs/fmsy_sel.pdf",plot=p1,width=5.2,height=4.0,units="in")
ndf <- tibble(data.frame(year=1964:2024,rbind(M$N)))
names(ndf) <- c("Year",1:15)
  ndfm <- ndf %>%  pivot_longer(cols=2:16,names_to="age",values_to="Numbers") %>%
                  mutate( age=as.numeric(age),
                    Numbers=as.numeric(Numbers) )

sdfm

  p1 <- sdfm %>% left_join(ndfm) %>%
  filter(age<11,Year<2099)%>% mutate( Year=substr(as.character(Year),3,4)) %>% group_by(Year) %>%
 summarise(
  mnagesel=sum(Selectivity*age)/sum(Selectivity),
 mnage=sum(Numbers*age)/sum(Numbers)) %>%
 ggplot(aes(y=mnagesel,x=mnage,label=Year)) + geom_text() + ylab("Mean age selected") +
 theme_few() + geom_path(size=.2,color="grey") + xlab( "Mean age in stock") + 
    geom_smooth(method='lm');p1

  ggsave("doc/figs/mn_age_stock_vs_sel.pdf",plot=p1,width=7.2,height=5.7,units="in")

nrows=dim(M$sel_fsh)[1]
  yr=2001:2024;sel<-M$sel_fsh[(nrows-23):nrows,1:13]
  p1 <- plot_sel(Year=yr,sel=sel,scale=3);
  p1 + geom_vline(xintercept=3.5,color="grey")
  library(ggridges)
      #scale_x_continuous(breaks=seq(2010,2022,by=2))
p1
