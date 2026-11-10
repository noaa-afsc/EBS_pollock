#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(ggplot2);library(dplyr);library(readr)})
a<-commandArgs(trailingOnly=TRUE);stopifnot(length(a)==1L);out<-a[[1]]
ftnir<-read_csv(file.path(out,'ats_ftnir_abundance_at_age.csv'),show_col_types=FALSE)
tma15<-read_csv(file.path(out,'ats_tma_15plus_abundance_at_age.csv'),show_col_types=FALSE)
plot<-bind_rows(ftnir |> filter(region=='Core EBS') |> transmute(age=ftnir_age,abundance_billion=abundance_million/1000,basis='BTS FT-NIRS length key'),
 tma15 |> filter(region=='Core EBS') |> transmute(age=age_bin,abundance_billion=abundance_million/1000,basis='BTS tester-age conversion'))
g<-ggplot(plot,aes(age,abundance_billion,fill=basis))+geom_col(width=.8)+
 facet_wrap(vars(basis),ncol=1)+scale_fill_manual(values=c('#D55E00','#0072B2'))+
 scale_x_continuous(breaks=1:15,labels=c(as.character(1:14),'15 / 15+'))+
 labs(x='Age (released FT-NIRS 15 above; TMA 15+ below)',y='Abundance (billion fish)',
 title='Provisional 2026 core-area ATS abundance at age')+
 theme_minimal(base_size=12)+theme(legend.position='none',panel.grid.minor=element_blank())
ggsave(file.path(out,'ats-age-composition.png'),g,width=9,height=7,dpi=180)
ggsave(file.path(out,'ats-age-composition.svg'),g,width=9,height=7)
