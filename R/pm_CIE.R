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
# Read report file and create gmacs report object (a list):
#mod_names <- c(
  #"Last year", # 1
  #"m1",
  #"m2 ",
  #"m3",
  #"m4",
  #"m5",
  #"m6",
  #"m7",
  #"m8"
#)
mod_names <- c(
  "Last year", # 1
  "Model 23","Drop new BTS", "and ATS", "and AVO" )#, "Fix BTS Sel.")
# BTS age compositions included through 2023
# "Sept version",
# "2023 AVO point",
# "2022 ATS age updated",
# "added 2022 catch-age",
# "BTS to 2023",
# "BTS db Age")
# BTS age compositions included through 2023
# but with Hulsons BTS input sample sizes

mod_dir <- c("lastyr",
  "m23","m1", "m2", "m3")#, "fixsel_bts")
# mod_names <- c("MSY=1.2","Fmsy=F35%", "Base est.")
# mod_dir <- c("condmn", "condF35", "m8")
# WARNING,  commented out line will re-run all the models in the mod_dir directories within "runs"
# Won't do tier 3 spm (proj) model in the subdirectory at the moment
#---Read in the results for modelsl already run--------------
# run_model(rundir="2023_runs")

# run_proj(rundir="2023_runs")
modlst <- get_results(rundir = "2024/runs")
# names(modlst)
M <<- modlst[[thismod]]
#Fix <<- modlst[[6]]
#M <<- modlst[[3]];saveRDS(M, "~/m8.rds")

#.MODELDIR <<- paste0("2023_runs/", mod_dir, "/")
.MODELDIR <<- paste0("2024/runs/", mod_dir, "/")

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
rhodf <- read.csv("doc/data/mohnrho.csv", header = T)
#rhodf
rhoMohn10 <- rhodf[11, 2]# |> pull(rho)
rhoMohn20 <- rhodf[20, 2]
rhoMohn20

# Figure captions
fc <- (read_csv("doc/data/fig_captions.csv"))
figcap <<- fc$cap
figlab <<- fc$label
fnum <<- fc$no
reffig <<- function(i) {
  cat(paste0("\\ref{fig:", figlab[fnum == i], "}"))
}

# ![Results of the EBS pollock model for recent spawning biomass estimates comparing the base model using the covariance matrix with the one where only the diagonal is applied.]
# (doc/figs/mod_diag_ssb.pdf){#fig-diagssb}
  
  #' Print Figure Markdown with Captions
  #'
  #' This function prints the markdown syntax for a figure with a caption and label, useful for dynamic document generation.
  #'
  #' @param tmp A character string representing the name of the figure file (without path).
  #' @param i An integer or index that matches the figure number, used to retrieve the corresponding caption and label.
  #' 
  #' @details This function assumes that there are two vectors, `figcap` and `figlab`, and a vector or variable `fnum`. 
  #' It matches the index `i` with `fnum` to extract the appropriate caption and label for the figure.
  #' 
  #' @examples
  #' # Assuming figcap, figlab, and fnum are properly defined:
  #' printfig("figure1.png", 1)
  #'
  #' @export
  printfig <<- function(tmp, i) {
    cat(paste0("\n![", figcap[fnum == i], "](doc/figs/", tmp, "){#fig-", figlab[fnum == i], "}\n"))
  }

# printfig <<- function(tmp,i){ cat(paste0("\n![",figcap[fnum==i],"\\label{fig:",figlab[fnum==i],"}](doc/figs/",tmp,")   \n ")) }

# Table captions
tc <- (read_csv("doc/data/table_captions.csv"))
tablab <- tc$label
tabcap <- paste0("\\label{tab:", tablab, "}", tc$cap)
# tc
reftab <<- function(i) {
  cat(paste0("@tbl-", tablab[i]))
}
# reffig(1)
# tabcap[1]
# tap <- data_frame(t=c(1,2),c=c(1,2))
# printtab <<- function(tmp,i){ cat(paste0("\n![",tabcap[fnum==i],"](doc/figs/",tmp,"){#tbl-",tablab[fnum==i],"}\n") )
#' Print Table with Captions and Labels
#'
#' This function prints a table using `xtable` with a specified caption and label.
#'
#' @param tmp A data frame or matrix to be converted into a table.
#' @param i An integer or index used to retrieve the corresponding caption and label for the table.
#' 
#' @details This function utilizes the `xtable` package to create LaTeX or HTML representations of the table.
#' It matches the index `i` with vectors `tabcap` and `tablab` to assign the appropriate caption and label.
#' 
#' @examples
#' # Assuming tabcap and tablab are properly defined:
#' printtab(data.frame(A = 1:3, B = 4:6), 1)
#'
#' @export
printtab <<- function(tmp, i) {
  tab <- xtable(tmp, digits = 0, auto = TRUE, caption = tabcap[i], label = paste0("tbl:", tablab[i]))
  print(tab, caption.placement = "top", include.rownames = FALSE, sanitize.text.function = function(x) {
    x })
}
  

# print(tablab)

# source("../R/Do_Plots.R")
# source("../R/Do_MCMC.R")
# source("../R/Do_Proj.R")

