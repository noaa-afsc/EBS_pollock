suppressPackageStartupMessages({library(tidyverse)})
w_core <- read_csv('/Users/jim/_mymods/pollock/ebswp_rceattle/analysis/ATS_age_composition_2026/output/core_tma_weight_at_age.csv',show_col_types=FALSE)
w_plot <- ggplot(w_core,aes(age_bin,weight_kg_per_fish))+
  geom_line(colour='#0072B2',linewidth=.7)+geom_point(colour='#0072B2',size=2.7)+
  scale_x_continuous(breaks=1:15,labels=c(as.character(1:14),'15+'))+
  labs(x='TMA tester age',y='Mean weight (kg/fish)',
    title='Provisional 2026 core-area ATS mean weight-at-age',
    subtitle='BTS FT-NIRS length key followed by the conditional TMA key',
    caption='Biomass and abundance use identical supported lengths; uncertainty is not propagated.')+
  theme_minimal(base_size=12)+theme(panel.grid.minor=element_blank())
ggsave('2026/assets/figure-017.png',w_plot,width=9,height=5,dpi=180)
