#!/usr/bin/env Rscript
# Saved outputs only; run from the publication repository root.
suppressPackageStartupMessages({library(dplyr);library(tidyr);library(readr);library(ggplot2);library(ggthemes);library(grid)})
args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==1)
w <- read_csv(file.path(args[1],'weights_with_se.csv'),show_col_types=FALSE)
d <- bind_rows(
  w |> filter(dataset=='fishery', observation_available) |> transmute(year,age=age_bin,weight_kg=observed_kg,panel='Observed fishery'),
  w |> filter(dataset=='BTS', observation_available) |> transmute(year,age=age_bin,weight_kg=observed_kg,panel='Observed BTS'),
  w |> filter(dataset=='fishery') |> transmute(year,age=age_bin,weight_kg,panel='Predicted fishery-scale weights')) |>
  filter(between(age,3,10),is.finite(weight_kg),weight_kg>0) |>
  group_by(panel,age) |> mutate(reference_kg=mean(weight_kg[year<=2025]),anomaly=weight_kg/reference_kg-1) |> ungroup() |>
  mutate(panel=factor(panel,levels=c('Observed fishery','Observed BTS','Predicted fishery-scale weights')))
stopifnot(max(d$year[d$panel=='Observed BTS'])==2025,max(d$year[d$panel=='Observed fishery'])==2025,max(d$year)==2028)
write_csv(d,'2026/tables/waa-omit-bts2026-figure.csv')
make_plot <- function(recent=FALSE) {
  x <- if(recent)filter(d,year>=2014) else d
  p <- ggplot(x,aes(age,year,fill=anomaly))+geom_tile()+
    annotate('rect',xmin=2.5,xmax=10.5,ymin=2026.5,ymax=2028.5,fill=NA,colour='grey35',linetype=2,linewidth=.4)+
    geom_hline(yintercept=2025.5,linewidth=.55)+
    facet_grid(.~panel)+scale_x_continuous(breaks=3:10,expand=expansion(add=.5))+
    scale_y_reverse(breaks=if(recent)2014:2028 else c(seq(1970,2020,10),2026,2028),limits=if(recent)c(2028.5,2013.5) else c(2028.5,1969.5),expand=c(0,0))+
    scale_fill_gradient2(low='#756BB1',mid='white',high='#B33B32',limits=c(-.5,.5),oob=scales::squish,name='Relative anomaly',breaks=c(-.5,0,.5),labels=c('≤ −50%','0','≥ +50%'))+
    labs(x='Age',y='Year',title=if(recent)'Recent years: mean weight (kg)' else 'A. Full history',subtitle='Solid line: start of 2026 estimates. Dashed box: 2027–2028 forecasts.')+
    theme_few(base_size=11)+theme(strip.background=element_blank(),strip.text=element_text(size=10),legend.position=if(recent)'bottom' else 'none',panel.grid.major=element_line(colour='grey93',linewidth=.2),plot.title=element_text(size=12,face='bold'),plot.subtitle=element_text(size=9))
  if(recent)p <- p+geom_text(aes(label=sprintf('%.2f',weight_kg)),size=2.65)
  p
}
b <- make_plot(TRUE)
ggsave('2026/assets/waa-omit-bts2026.png',b,width=9.55,height=5.4,dpi=220,bg='white')
ggsave('2026/tables/waa-omit-bts2026.pdf',b,width=9.55,height=5.4,device=cairo_pdf,bg='white')
