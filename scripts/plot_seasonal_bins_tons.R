#!/usr/bin/env Rscript
# Replot preserved bin summaries; original raw records and binning stay fixed.
suppressPackageStartupMessages({library(dplyr);library(readr);library(ggplot2);library(ggridges);library(ggthemes)})
out <- '2026/tables/terminology-tons'
for (kind in c('annual','weekly-A','weekly-B')) {
  d <- read_csv(file.path(out,paste0(kind,'-bins.csv')),show_col_types=FALSE) |> filter(series=='pooled') |> mutate(Year=factor(Year))
  if(kind=='annual') {
    d <- d |> mutate(seas=factor(seas))
    p <- ggplot(d,aes(wtbin,reorder(Year,desc(Year)),height=freq))+facet_wrap(~seas)+
      geom_density_ridges(stat='identity',scale=3,alpha=.2,fill='salmon',colour='black')+
      geom_vline(xintercept=500,colour='coral',alpha=.3,linewidth=1)+xlim(200,1500)+ylab('Year')+theme_few(base_size=18)
    name <- 'fsh_wt_frq_yr';title <- 'Mean fish weight by year and season';width <- 6.4
  } else {
    d <- d |> mutate(week=factor(week))
    p <- ggplot(d,aes(wtbin,reorder(week,desc(week)),height=freq))+facet_wrap(~Year)+
      geom_density_ridges(stat='identity',scale=3,alpha=.2,fill='salmon',colour='black')+
      geom_vline(xintercept=500,colour='blue',alpha=.4,linewidth=1)+xlim(100,1250)+ylab('Week')+theme_few(base_size=14)+theme(axis.text=element_text(size=7))
    name <- if(kind=='weekly-A')'fsh_wt_freq_week_A' else 'fsh_wt_freq_week_B'
    title <- if(kind=='weekly-A')'A-season weekly mean fish weight' else 'B-season weekly mean fish weight';width <- 8.4
  }
  p <- p+labs(title=title,subtitle='Pooled legacy series\n2026 data through September 18; B-season partial',x='Mean fish weight per haul (grams)',caption='Ridge heights: pollock catch (tons) per weight bin. Source: inseas26.csv.')+
    theme(plot.title=element_text(size=14),plot.subtitle=element_text(size=10),plot.caption=element_text(size=8))
  ggsave(file.path(out,paste0(name,'.png')),p,width=width,height=9,dpi=300,bg='white')
}
