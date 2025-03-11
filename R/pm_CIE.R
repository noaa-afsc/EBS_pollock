# setwd("~/_mymods/ebswp/doc")
rm(list = ls())
.THEME <- ggthemes::theme_few()
.OVERLAY <- TRUE
# install.packages("ggridges")
# source("R/prelims.R")
# source("prelims.R")
setwd(here::here())
library(ebswp)
library(tidyverse)
thisyr <<- 2024
lastyr <<- thisyr - 1
nextyr <<- thisyr + 1
thismod <<- 2
dec_tab_ord <<- 1:8

# Made a tbl one for the library
#source("R/print_Tier3_tables.R")
#source("tools/get-tier3-res.R")
# The model specs

#--Main models to presesnt in Sept   -----------
# NOTE: sequence weird because ATS FTNIR data messed up
mod_names <- c("2024 TMA", "Base", "FT-NIR fleets aggregated", "FT-NIR fleet-specific", "FT-NIR funky acoustic" )
mod_dir <- c("lastyrdb", "lastyrdbae", "ftnir3", "ftnir4", "ftnir1")
# run_proj(rundir="2023_runs")
modlst <- get_results(rundir = "runs")
# names(modlst)
M <<- modlst[[thismod]]
#Fix <<- modlst[[6]]
#M <<- modlst[[3]];saveRDS(M, "~/m8.rds")

#.MODELDIR <<- paste0("2023_runs/", mod_dir, "/")kkkkkk
#
.MODELDIR <<- paste0("runs/", mod_dir, "/")

# Q sum ages 3 and younger from M$N over time
#names(M)
# Compare SRR
#tab_fit(modlst, mod_scen = c(1:2)) |> gt::gt()
# tab_ref(modlst[c(2:9)]) |> gt::gt() |>  gt::fmt_markdown()
# names(modlst)
# Save result so it can be used by the document
# save(modlst,file="doc/novmod.rdata")
# names(modlst)
# plot_avo(modlst[3:5])

#---Covariance diagonal extraction--------
#---Mohno rho read-----
#rhodf <- read.csv("doc/data/mohnrho.csv", header = T)
##rhodf
#rhoMohn10 <- rhodf[11, 2]# |> pull(rho)
#rhoMohn20 <- rhodf[20, 2]
#rhoMohn20

# Figure captions
#fc <- (read_csv("doc/data/fig_captions.csv"))
#figcap <<- fc$cap
#figlab <<- fc$label
#fnum <<- fc$no
#reffig <<- function(i) {
  #cat(paste0("\\ref{fig:", figlab[fnum == i], "}"))
#}
#
  #printfig <<- function(tmp, i) {
    #cat(paste0("\n![", figcap[fnum == i], "](doc/figs/", tmp, "){#fig-", figlab[fnum == i], "}\n"))
  #}
#
# printfig <<- function(tmp,i){ cat(paste0("\n![",figcap[fnum==i],"\\label{fig:",figlab[fnum==i],"}](doc/figs/",tmp,")   \n ")) }

# Table captions
#tc <- (read_csv("doc/data/table_captions.csv"))
#tablab <- tc$label
#tabcap <- paste0("\\label{tab:", tablab, "}", tc$cap)
## tc
#reftab <<- function(i) {
  #cat(paste0("@tbl-", tablab[i]))
#}
#printtab <<- function(tmp, i) {
  #tab <- xtable(tmp, digits = 0, auto = TRUE, caption = tabcap[i], label = paste0("tbl:", tablab[i]))
  #print(tab, caption.placement = "top", include.rownames = FALSE, sanitize.text.function = function(x) {
    #x })
#}
  #

# print(tablab)

# source("../R/Do_Plots.R")
# source("../R/Do_MCMC.R")
# source("../R/Do_Proj.R")

