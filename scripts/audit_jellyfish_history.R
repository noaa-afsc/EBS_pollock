#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(dplyr);library(readr);library(tidyr)})
a<-commandArgs(trailingOnly=TRUE);stopifnot(length(a)==2)
x<-read_csv(a[1],show_col_types=FALSE)
read_revision<-function(rev) {s<-system2('git',c('-C',shQuote(a[2]),'show',paste0(rev,':doc/data/byc_nontarg.csv')),stdout=TRUE);read_csv(I(paste(s,collapse='\n')),show_col_types=FALSE)}
sep<-read_revision('c29d1e0');nov<-read_revision('d38de13')
j<-x |> filter(`Species Group Name`=='Scypho jellies')
z<-j |> group_by(Year) |> summarise(BSAI_mt=sum(`Nontarget Estimate (mt)`[`FMP Area`=='BSAI'],na.rm=TRUE),BS_mt=sum(`Nontarget Estimate (mt)`[`FMP Subarea`=='BS'],na.rm=TRUE),AI_mt=sum(`Nontarget Estimate (mt)`[`FMP Subarea`=='AI'],na.rm=TRUE),all_areas_mt=sum(`Nontarget Estimate (mt)`,na.rm=TRUE),discarded_mt=sum(`Nontarget Estimate (mt)`[`FMP Area`=='BSAI' & `Retained or Discarded`=='Discarded'],na.rm=TRUE),retained_mt=sum(`Nontarget Estimate (mt)`[`FMP Area`=='BSAI' & `Retained or Discarded`=='Retained'],na.rm=TRUE),missing_BSAI_weights=sum(is.na(`Nontarget Estimate (mt)`[`FMP Area`=='BSAI'])),.groups='drop') |> left_join(sep |> select(Year,September2024_mt=Jellies),by='Year') |> left_join(nov |> select(Year,November2024_mt=Jellies),by='Year') |> mutate(November_over_current=November2024_mt/BSAI_mt,November_over_September=November2024_mt/September2024_mt)
stopifnot(all(abs(z$BSAI_mt-z$BS_mt-z$AI_mt)<1e-7),all(abs(z$BSAI_mt-z$discarded_mt-z$retained_mt)<1e-7),all(z$missing_BSAI_weights==0),sum(duplicated(x))==0)
write_csv(z,'2026/validation/jellyfish-history-audit.csv',na='NA')
# Disaggregate both target categories and disposition for each area.
write_csv(j |> group_by(Year,`FMP Area`,`FMP Subarea`,`Trip Target Fishery`,`Retained or Discarded`) |> summarise(catch_mt=sum(`Nontarget Estimate (mt)`,na.rm=TRUE),records=n(),.groups='drop'),'2026/validation/jellyfish-filter-audit.csv')
print(z |> filter(Year>=2019,Year<=2022),width=Inf)
