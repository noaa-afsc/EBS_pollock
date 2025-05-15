# Carey making plots with bigger fonts for presentation:
library(plotly)
library(r4ss)
library(ggridges)
library(ebswp)
library(tidyverse)
library(patchwork)
library(gganimate)
library(ggridges)
library(viridis)
library(gt)
# Set ggplot theme
# devtools::install_github("seananderson/ggsidekick")
thisyr=2024
library(ggsidekick)
theme_set(theme_sleek())
.THEME <- theme_sleek()
.OVERLAY <- TRUE
options(warn = -1)


do_loo<-1
if (do_loo) {
  thisyr <- 2024
  nextyr <- thisyr + 1
  dec_tab_ord <<- 1:8
  mod_names <- c(
    "2024 accepted",
    "Drop BTS", 
    "Drop ATS ", 
    "Drop AVO ", 
    "Drop indices",
    "Drop BTS age ", 
    "Drop ATS age " 
  )
  mod_dir <- c(
    "lastyr",
    "loo_bts_all",
    "loo_ats_all",
    "loo_avo",
    "loo_srv_indices",
    "loo_bts_comp",
    "loo_ats_comp"
  )
  loo_lst <- get_results(rundir = here::here("runs"))
  # source(here::here("R","pm_CIE.R"))
  saveRDS(loo_lst, here::here("runs", "loo.rds"))
  
} else {
  loo_lst <- readRDS(here::here("runs", "loo.rds"))
}

#do some plots
p1 <- plot_ssb(loo_lst, xlim = c(1964, 2024), breaks = seq(1964, 2024, 4))
ggsave(filename = here::here("doc", "figs", "loo_ssb.png"), plot = p1, width = 8, height = 4, units = "in", device = "png")

p1b<-plot_ssb_rel(loo_lst, xlim = c(1964, 2024), breaks = seq(1964, 2024, 4))
ggsave(filename = here::here("doc", "figs", "loo_rel_ssb.png"), plot = p1b, width = 8, height = 4, units = "in", device = "png")

#this one is pretty ugly:
p2<-plot_recruitment(loo_lst, xlim = c(1964, 2024))
ggsave(filename = here::here("doc", "figs", "loo_recruits.png"), plot = p2, width = 15, height = 5, units = "in", device = "png")

#plot the indices:  
p3<-plot_bts(loo_lst,error_bars = FALSE)
ggsave(filename = here::here("doc", "figs", "loo_bts_index.png"), plot = p3, width = 8, height = 4, units = "in", device = "png")

p4<-plot_ats(loo_lst,error_bars = FALSE)
ggsave(filename = here::here("doc", "figs", "loo_ats_index.png"), plot = p4, width = 8, height = 4, units = "in", device = "png")

p5<-plot_avo(loo_lst,error_bars = FALSE)
ggsave(filename = here::here("doc", "figs", "loo_avo_index.png"), plot = p5, width = 8, height = 4, units = "in", device = "png")
