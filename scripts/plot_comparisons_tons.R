suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(ggplot2); library(ggthemes)
})
display_model <- function(x) dplyr::recode(x,`Model 26.2`="Model 26.0")
quantities <- c("Standardized total biomass", "Spawning biomass",
                "Age-1 recruitment", "Maximum fishing mortality")
labels <- c("Biomass at common weights (million tons)",
            "Spawning biomass (million tons)",
            "Age-1 recruitment (billion fish)",
            "Fishing mortality on most exposed age (per year)")
axis_numbers <- function(x) scales::number(x, accuracy = if (max(x,na.rm = TRUE) <= 1.1) .01 else .1)
prepare <- function(x) x |>
  mutate(divisor = if_else(quantity == "Maximum fishing mortality", 1, 1000),
         value = estimate / divisor,
         panel = factor(quantity, levels = quantities, labels = labels))
annual <- read_csv("2026/tables/terminology-tons/annual_comparisons.csv",show_col_types = FALSE) |> prepare()
uncertainty <- read_csv("2026/tables/terminology-tons/configuration_uncertainty.csv",show_col_types = FALSE) |>
  prepare() |> mutate(low = lower_95 / divisor, high = upper_95 / divisor)
common <- theme_few(base_size = 14) +
  theme(legend.position = "bottom", legend.text = element_text(size = 13),
        strip.background = element_blank(), strip.text = element_text(hjust = 0, face = "bold",size = 13.5),
        panel.spacing = grid::unit(1.1,"lines"),
        plot.title = element_text(face = "bold",size = 18),
        plot.subtitle = element_text(size = 13),
        plot.caption = element_text(hjust = 0,size = 12.5),
        plot.margin = margin(12,15,10,10))
data <- annual |> filter(step <= 7,year >= 2000) |>
  mutate(model = factor(model,levels = unique(model)))
p <- ggplot(data,aes(year,value,colour = model,linetype = model)) +
  geom_line(linewidth = .85) +
  facet_wrap(vars(panel),ncol = 2,scales = "free_y") +
  scale_colour_manual(values = c("#202020","#999999","#E69F00","#0072B2","#009E73","#CC79A7","#D55E00")) +
  scale_linetype_manual(values = c("solid","dotted","longdash","dashed","dotdash","solid","longdash")) +
  scale_y_continuous(limits = c(0,NA),labels = axis_numbers) +
  labs(title = "Cumulative input effects within Model 23.0",
       subtitle = "Each fit inherits the preceding updates; model formulation is retained.",
       x = "Year",y = NULL,colour = NULL,linetype = NULL,
       caption = paste("Biomass uses the same reference population weight schedule in every comparison.",
          "Input releases combine added observations, historical revisions and stated uncertainty updates. Lines show point estimates.",sep = "\n")) +
  guides(colour = guide_legend(ncol = 2),linetype = guide_legend(ncol = 2)) + common
for (ext in c("png","svg")) ggsave(paste0("2026/tables/terminology-tons/data_input_effects.",ext),p,width = 12,height = 9,dpi = 220,bg = "white")

plot_config <- function(models,name,title) {
  x <- uncertainty |> filter(model %in% models) |> mutate(model = factor(model,levels = models))
  p <- ggplot(x,aes(year,value,colour = model,linetype = model,fill = model)) +
    geom_ribbon(aes(ymin = low,ymax = high),colour = NA,alpha = .12) +
    geom_line(linewidth = .85) + facet_wrap(vars(panel),ncol = 2,scales = "free_y") +
    scale_colour_manual(values = c("Model 23.0" = "#202020","Model 23.2" = "#0072B2","Model 26.2" = "#D55E00"), labels = display_model) +
    scale_fill_manual(values = c("Model 23.0" = "#202020","Model 23.2" = "#0072B2","Model 26.2" = "#D55E00"), labels = display_model) +
    scale_linetype_manual(values = c("Model 23.0" = "solid","Model 23.2" = "longdash","Model 26.2" = "solid"), labels = display_model) +
    scale_y_continuous(limits = c(0,NA),labels = axis_numbers) +
    labs(title = title,subtitle = "The observations and supplied biological schedules are held fixed.",
       x = "Year",y = NULL,colour = NULL,fill = NULL,linetype = NULL,
       caption = paste("Lines: estimates; shading: approximate 95% uncertainty ranges conditional on each model.",
                       "Biomass uses common population weights; fishing mortality uses the most exposed age.",sep = "\n")) + common
  for (ext in c("png","svg")) ggsave(paste0("2026/tables/terminology-tons/",name,".",ext),p,width = 12,height = 8.2,dpi = 220,bg = "white")
}
plot_config(c("Model 23.0","Model 23.2"),"model23_0_vs23_2_uncertainty","Model configuration comparison: 23.0 and 23.2")
plot_config(c("Model 23.2","Model 26.2"),"model23_2_vs26_2_uncertainty","Model configuration comparison: 23.2 and 26.0")
writeLines(capture.output(sessionInfo()),"2026/tables/terminology-tons/plot_session_info.txt")
