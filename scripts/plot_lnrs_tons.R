suppressPackageStartupMessages({library(dplyr);library(readr);library(ggplot2);library(ggthemes);library(patchwork);library(jsonlite)})
out <- '2026/tables/terminology-tons'
z <- read_csv(file.path(out,'paired-estimates.csv'),show_col_types=FALSE)
g1 <- ggplot(z,aes(birth_year,lnRS))+geom_line(color='grey55')+geom_point(aes(color=recent,shape=recent),size=2)+theme_few()+labs(x='Spawning / birth year',y='ln(R/S)')
g2 <- ggplot(z,aes(SSB_kt,lnRS))+geom_path(color='grey75',linewidth=.3)+geom_text(aes(label=label,color=recent),size=3)+theme_few()+labs(x='Female spawning biomass (thousand tons)',y='ln(R/S)')
colors <- scale_color_manual(values=c('FALSE'='#156082','TRUE'='#B45120'),labels=c('1964–2022 year classes','2023–2025 year classes'),name=NULL)
g <- ((g1+colors)+(g2+colors))+plot_layout(ncol=1,guides='collect')+plot_annotation(title='Model 26.0: recruits per unit of spawning biomass',subtitle='Age-1 recruits in year t + 1 paired with female spawning biomass in year t',caption='R: million age-1 fish; S: thousand tons. Ratios are numerically recruits per kg of female SSB.\nOrange: 2023–2025 year classes, which are less certain. Blue: earlier year classes.
Lower-panel labels identify birth years; all values are point estimates.') & theme(legend.position='none')
ggsave(file.path(out,'lnRS.png'),g,width=8,height=10,dpi=180)
