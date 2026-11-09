#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(dplyr);library(tidyr);library(readr)})
a<-commandArgs(trailingOnly=TRUE);stopifnot(length(a)==2)
out<-'2026/tables'
x<-read_csv(a[1],show_col_types=FALSE);y<-read_csv(a[2],show_col_types=FALSE)
stopifnot(all(x$`FMP Subarea`=='BS'),all(y$`FMP Subarea`=='BS'),all(x$`Trip Target Name` %in% c('Pollock - bottom','Pollock - midwater')),all(y$`Species Group Name`=='Pollock'),!anyNA(x$`Catch (mt)`),!anyNA(y$`Catch (mt)`))
# Preserve source taxonomic groups; source-only groups outside the requested scope are audited separately.
key<-c('Pacific Cod'='P.cod','Rock Sole'='Rock.Sole','Flathead Sole'='Flathead','Arrowtooth Flounder'='Arrowtooth','Pacific Ocean Perch'='POP','Yellowfin Sole'='Yellowfin','Sablefish'='Sablefish','Shark'='Shark','Atka Mackerel'='Atka.Mkrl','BSAI Skate and GOA Skate, Other'='Skates')
excluded<-c('Pollock','Chum Salmon','Chinook Salmon','Pink Salmon','Coho Salmon','Unspecified Salmon','Halibut','Non TAC Species')
x<-x |> mutate(in_scope=!is.na(`Species Group Name`) & !(`Species Group Name` %in% excluded),column=if_else(`Species Group Name` %in% names(key),unname(key[`Species Group Name`]),'Other'))
# Missing source categories remain NA, rather than being recoded to measured zeros.
s<-x |> filter(in_scope) |> group_by(Year,column) |> summarise(catch=sum(`Catch (mt)`),.groups='drop') |> pivot_wider(names_from=column,values_from=catch) |> select(Year,all_of(c(unname(key),'Other'))) |> arrange(Year)
write_csv(s,file.path(out,'fmp-species-in-pollock.csv'),na='NA')
write_csv(x |> distinct(`Species Group Name`,in_scope,column) |> arrange(`Species Group Name`),file.path(out,'fmp-species-column-map.csv'))
flats<-c('Flathead Sole','Arrowtooth Flounder','Other Flatfish - BSAI','Alaska Plaice - BSAI','Kamchatka Flounder - BSAI','Greenland Turbot - BSAI')
y<-y |> mutate(in_scope=!is.na(`Trip Target Name`) & !(`Trip Target Name` %in% c('Pollock - bottom','Pollock - midwater','No retained catch')),column=case_when(`Trip Target Name`=='Rock Sole - BSAI'~'Rock.sole',`Trip Target Name`=='Yellowfin Sole - BSAI'~'Yellowfin.sole',`Trip Target Name`=='Pacific Cod'~'P.cod',`Trip Target Name` %in% flats~'Other.flats',TRUE~'Other.spp'))
t<-y |> filter(in_scope) |> group_by(Year,column) |> summarise(catch=sum(`Catch (mt)`),.groups='drop') |> pivot_wider(names_from=column,values_from=catch) |> select(Year,Rock.sole,Other.flats,Other.spp,P.cod,Yellowfin.sole) |> mutate(Total=rowSums(pick(-Year),na.rm=TRUE)) |> arrange(Year)
write_csv(t,file.path(out,'pollock-in-other-fisheries.csv'),na='NA')
write_csv(y |> distinct(`Trip Target Name`,in_scope,column) |> arrange(`Trip Target Name`),file.path(out,'pollock-bycatch-target-map.csv'))
for(n in c('x','y')) {
 d<-get(n);audit<-d |> group_by(Year,in_scope) |> summarise(catch_mt=sum(`Catch (mt)`),records=n(),last_week=max(`Week Ending Date`),.groups='drop');write_csv(audit,file.path(out,paste0(if(n=='x')'fmp-species' else 'pollock-bycatch','-coverage.csv')))
}
# Reconcile annual grouped values against a separately aggregated input total.
stopifnot(all.equal(s |> mutate(total=rowSums(pick(-Year),na.rm=TRUE)) |> pull(total),x |> filter(in_scope) |> group_by(Year) |> summarise(total=sum(`Catch (mt)`),.groups='drop') |> arrange(Year) |> pull(total),tolerance=1e-10)==TRUE)
stopifnot(all.equal(t$Total,y |> filter(in_scope) |> group_by(Year) |> summarise(total=sum(`Catch (mt)`),.groups='drop') |> arrange(Year) |> pull(total),tolerance=1e-10)==TRUE)
print(s |> filter(Year>=2024),width=Inf);print(t |> filter(Year>=2024),width=Inf)
