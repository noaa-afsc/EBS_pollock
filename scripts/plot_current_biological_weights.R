#!/usr/bin/env Rscript
# Replot saved results; no fitting or uncertainty recalculation.
suppressPackageStartupMessages({library(dplyr);library(tidyr);library(readr);library(ggplot2);library(ggthemes);library(patchwork)})
args <- commandArgs(trailingOnly=TRUE);stopifnot(length(args)==2);out<-args[1]
theme_report<-function() theme_few(base_size=13)+theme(plot.title=element_text(face='bold',size=17),plot.subtitle=element_text(size=13),plot.caption=element_text(size=10,hjust=0),strip.text=element_text(size=13,face='bold'),axis.text=element_text(size=12),axis.title=element_text(size=13),legend.position='bottom',legend.text=element_text(size=10),panel.grid.minor=element_blank(),plot.margin=margin(10,12,10,12))
bio<-read_csv(file.path(out,'biological_schedules_annual.csv'),show_col_types=FALSE)|>filter(year==2026)
prop<-bio|>select(age,maturity_proportion,female_proportion)|>pivot_longer(-age,names_to='schedule',values_to='value')|>
  mutate(schedule=recode(schedule,maturity_proportion='Fraction mature',female_proportion='Female fraction'))
p1<-ggplot(prop,aes(age,value,colour=schedule,linetype=schedule))+geom_line(linewidth=.8)+
  scale_colour_manual(values=c('Fraction mature'='#0072B2','Female fraction'='#222222'))+
  scale_x_continuous(breaks=c(1,5,10,15),labels=c(1,5,10,'15+'))+ylim(0,1)+
  labs(x='Age',y='Proportion',title='Maturity and sex composition',colour=NULL,linetype=NULL)+theme_report()+theme(plot.title=element_text(size=14))
p2<-ggplot(bio,aes(age,natural_mortality_per_year))+geom_line(linewidth=.8)+
  scale_x_continuous(breaks=c(1,5,10,15),labels=c(1,5,10,'15+'))+
  labs(x='Age',y='Natural mortality (per year)',title='Natural mortality')+theme_report()+theme(plot.title=element_text(size=14))
re <- read_csv(file.path(args[2],'selected_RE_weights.csv'),show_col_types=FALSE) |> filter(year==2026) |> arrange(age_bin)
stopifnot(nrow(re)==13)
bio$fishery_weight_kg[bio$age>=3] <- re$weight_kg
write_csv(bio,file.path(out,'biological_schedules_2026_selected.csv'))
weights<-bio|>select(age,population_weight_kg,spawning_weight_kg,fishery_weight_kg)|>
  pivot_longer(-age,names_to='schedule',values_to='value')|>
  mutate(schedule=recode(schedule,population_weight_kg='Population (BTS)',spawning_weight_kg='Spawning',fishery_weight_kg='Fishery'))
projection <- read_csv(file.path(args[2],'age_schedules.csv'),show_col_types=FALSE) |> filter(model=='26_0',case_id=='primary') |> transmute(age,schedule='2027 catch and spawning',value=fishery_weight)
stopifnot(nrow(projection)==15)
weights <- bind_rows(weights,projection)
write_csv(weights,file.path(out,'figure34_weight_curves.csv'))
p3<-ggplot(weights,aes(age,value,colour=schedule,linetype=schedule))+geom_line(linewidth=.8)+
  scale_colour_manual(values=c('Population (BTS)'='#0072B2','Spawning'='#882255','Fishery'='#D55E00','2027 catch and spawning'='#009E73'))+
  scale_x_continuous(breaks=c(1,5,10,15),labels=c(1,5,10,'15+'))+
  labs(x='Age',y='Weight (kg)',title='2026 weights and selected 2027 projection',colour=NULL,linetype=NULL)+theme_report()+theme(plot.title=element_text(size=14))
sel<-read_csv(file.path(out,'terminal_selectivity_uncertainty.csv'),show_col_types=FALSE)|>filter(series!='AVO', !(series=='ATS' & age==1))|>
  mutate(series=recode(series,ATS='ATS and AVO'))
stopifnot(!any(sel$series=='ATS and AVO' & sel$age==1))
write_csv(sel,file.path(out,'figure34_displayed_selectivity.csv'))
p4<-ggplot(sel,aes(age,estimate,colour=series,linetype=series))+geom_ribbon(aes(ymin=lower_95,ymax=upper_95,fill=series),alpha=.1,colour=NA,show.legend=FALSE)+
  geom_line(linewidth=.8)+scale_colour_manual(values=c('Fishery'='#D55E00','BTS'='#0072B2','ATS and AVO'='#222222'))+
  scale_fill_manual(values=c('Fishery'='#D55E00','BTS'='#0072B2','ATS and AVO'='#222222'))+
  scale_x_continuous(breaks=c(1,5,10,15),labels=c(1,5,10,'15+'))+
  labs(x='Age',y='Relative selectivity',title='2026 fishing and survey selectivity',colour=NULL,linetype=NULL)+theme_report()+theme(plot.title=element_text(size=14))
pbio<-(p1+p2)/(p3+p4)+plot_annotation(title='Model 26.0 biological and selectivity schedules',
  caption='Mortality, maturity, sex proportions and weights are inputs. The 2027 projection uses the selected WtAgeRE fishery forecast for catch and spawning. Shaded selectivity bands show 95% uncertainty ranges.',theme=theme_report())
for(ext in c('png','svg'))ggsave(file.path(out,paste0('model26_2_biological_schedules.',ext)),pbio,width=13.5,height=8.4,dpi=240,bg='white')
