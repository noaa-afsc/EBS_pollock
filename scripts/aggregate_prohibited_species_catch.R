#!/usr/bin/env Rscript
# Public annual summaries only; the vessel-level input stays outside this repo.
suppressPackageStartupMessages({library(dplyr);library(tidyr);library(readr);library(gt)})
args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==1)
x <- read_csv(args[[1]],show_col_types=FALSE)
stopifnot(nrow(x)==551030,all(x$`FMP Area`=='BSAI'),all(x$`FMP Subarea`=='BS'),all(x$`Trip Target Name` %in% c('Pollock - bottom','Pollock - midwater')))
sum_available <- function(v) if(all(is.na(v))) NA_real_ else sum(v,na.rm=TRUE)
key <- c('Bairdi Tanner Crab'='Bairdi','Chinook Salmon'='Chinook','Halibut'='Halib','Herring'='Herring','Non-Chinook Salmon'='NonChin','Opilio Tanner (Snow) Crab'='Snow Crab','Red King Crab'='Red King','Blue King Crab'='Blue King','Golden (Brown) King Crab'='Golden King')
s <- x |> filter(`Species Group Name` %in% names(key)) |> group_by(Year,`Species Group Name`) |> summarise(estimate=sum_available(`PSCNQ Estimate (*)`),.groups='drop') |> mutate(species=unname(key[`Species Group Name`])) |> select(Year,species,estimate) |> pivot_wider(names_from=species,values_from=estimate)
m <- x |> filter(`Species Group Name`=='Halibut') |> group_by(Year) |> summarise(`Halib.M`=sum_available(`Halibut Mortality (mt)`),.groups='drop')
s <- left_join(s,m,by='Year') |> select(Year,Bairdi,Chinook,Halib,Halib.M,Herring,NonChin,`Snow Crab`,`Red King`,`Blue King`,`Golden King`) |> arrange(Year)
stopifnot(nrow(s)==36,abs(s$Halib.M[s$Year==2024]-53.178)<.1,all(s$Halib.M<=s$Halib | is.na(s$Halib.M)))
write_csv(s,'2026/tables/prohibited-species-catch.csv',na='NA')
coverage <- x |> group_by(Year) |> summarise(last_week=max(`Week Ending Date`),input_records=n(),missing_catch_records=sum(is.na(`PSCNQ Estimate (*)`)),.groups='drop')
write_csv(coverage,'2026/tables/prohibited-species-coverage.csv')
caption <- 'Bycatch estimates of prohibited species in the BSAI directed pollock fishery, 1991–2026.'
note <- 'Source: author-provided AKFIN / NMFS Alaska Regional Office export, received from AKR October 27, 2026 and loaded November 7, 2026 (earlier source vintages remain in some historical records). Both pollock target categories and all reported gears are included. Herring, halibut catch (Halib), and halibut mortality (Halib.M) are metric tons; other columns are estimated numbers of individuals. Values are summed before rounding. NA denotes unavailable estimates; zero denotes a reported zero. Sums use available values when individual records are missing; halibut mortality is unavailable for 1991–1993 and has some missing records in 1998–2003. The 2026 values are preliminary, through the week ending October 24. Other King Crab is outside the species columns of the example table.'
t <- s |> gt() |> tab_header(title=caption) |> fmt_number(columns=-Year,decimals=0,use_seps=TRUE) |> sub_missing(missing_text='NA') |> tab_source_note(note) |> tab_options(table.font.names='Times New Roman',table.font.size=12,data_row.padding=2)
gtsave(t,'2026/tables/prohibited-species-catch.html')
writeLines(c(caption,note),'2026/tables/prohibited-species-notes.txt')
print(s |> filter(Year>=2023),width=Inf)
