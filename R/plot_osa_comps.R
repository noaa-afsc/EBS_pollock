#' Explore OSA residuals for multinomial composition data and
#' compare to Pearson
#' @param obs,exp,pearson the observed, expected and Pearson
#'   residual matrices with rows as years and columns as ages (or
#'   lengths)
#' @param index,years vectors giving the index of ages (or length) and years
#' @param index_label character value indicating 'age' or 'length bin' depending on comp type
#' @param stock,survey characters given the stock and survey,
#' used to create filename stock_survey.pdf
#' @param outpath folder name for output, e.g. 'figs'
#' @return returns nothing but creates a PDF file in the working
#' directory
#'
plot_osa_comps <- function(obs, exp, pearson, index, years, index_label, Neff,
                           stock, survey, outpath = '',do_pdf=FALSE){
  stopifnot(all.equal(nrow(obs), nrow(exp), nrow(pearson),
                      length(years)))
  stopifnot(all.equal(ncol(obs), ncol(exp), ncol(pearson), length(index)))
  filename <- paste0(stock,"_",survey,"_", gsub('\\s', '_', index_label), ".pdf")

  if(do_pdf){
    if(is.null(outpath)) {
      pdf(here::here(filename), onefile=TRUE, width=7, height=7)
    } else {
      pdf(here::here(outpath, filename), onefile=TRUE, width=7, height=7)
    }
  }

  on.exit(dev.off())
  ## Neff <- ceiling(Neff)
  o <- round(Neff*obs/rowSums(obs),0); p=exp/rowSums(exp)
  ## default output
  res <- resMulti(t(o), t(p))
  if(!all(is.finite(res))){
    warning("failed to calculate OSA residuals for ", stock)
    return(NULL)
  }
  plot(res)
  ## compare to Pearson side by side
  mat <- t(matrix(res, nrow=nrow(res), ncol=ncol(res)))
  dimnames(mat) <- list(year=years, index=index[-1])
  reslong <- reshape2::melt(mat, value.name='resid')
  g1 <- ggplot(reslong, aes(year, index, size=abs(resid),
                            color=resid>0)) + geom_point() +
    ggtitle(paste0('OSA w/o ', index_label, ' 1')) + ylim(range(index)) +
    ylab(index_label)
  dimnames(pearson) <- list(year=years, index=index)
  pearsonlong <- reshape2::melt(pearson, value.name='resid')
  g2 <- ggplot(pearsonlong, aes(year, index, size=abs(resid),
                                color=resid>0)) + geom_point() +
    ggtitle('Pearson') + ylab(index_label)
  print(cowplot::plot_grid(g1,g2, nrow=2))

  ## ind is age/len bin to drop
  for(ind in 1:length(index)){
    ## assumes first column dropped so put it there
    index2 <- index[-ind]
    o2 <- cbind(o[,ind], o[,-ind])
    p2 <- cbind(p[,ind], p[,-ind])
    res <- resMulti(t(o2), t(p2))
    ## not sure why these fail sometimes?
    if(!all(is.finite(res))) {warning('failed when ind=',ind); break}
    mat <- t(matrix(res, nrow=nrow(res), ncol=ncol(res)))
    dimnames(mat) <- list(year=years, index=index2)
    reslong <- reshape2::melt(mat, value.name='resid')
    g <- ggplot(reslong, aes(year, index, size=abs(resid),
                             color=resid>0)) + geom_point()+
      ggtitle(paste0('OSA w/o ', index_label, ' ', index[ind])) + ylim(range(index)) +
      ylab(index_label)
    print(g)
  }
  message("wrote file ", filename)
}

pak::pkg_install("r4ss/r4ss")
library(r4ss)

# other afscOSA package dependencies:
library(ggplot2)
library(cowplot)
library(here)
library(dplyr)
library(reshape2)
library(here)

library(afscOSA)

# create directory for analysis
# out_path <- "test_aicod"
if(!exists("out_path")) out_path = getwd()
if(!dir.exists(out_path)) dir.create(out_path)

# copy all data files to working directory
pkg_path <- find.package('afscOSA')
example_data_files <- list.files(path = file.path(pkg_path, "inst", "examples", "AI_PCOD"))
example_data_files
file.copy(from = file.path(path = file.path(pkg_path, "inst", "examples", "AI_PCOD"),
                           example_data_files),
          to = file.path(file.path(out_path), example_data_files),
          overwrite = TRUE)

setwd(out_path)

sx = 1 # USER INPUT define sex
fleet = c(1,2) # USER INPUT define fleets
model_path <- c("inst/examples/AI_PCOD")

mod <- r4ss::SSgetoutput(dirvec = model_path)

# comps for the fleets defined in "fleet" and "sx"
comps <- as.data.frame(mod[[1]]$lendbase[,c(1,6,13,16:18)])
comps <- comps[comps$Fleet %in% fleet & comps$Sex %in% sx, ]
comps <- reshape2::melt(comps,id.vars = c('Yr','Fleet','Sex','Bin'))

# effective sample sizes for the fleets defined in "fleet" and "sx"
Neffdf <- as.data.frame(mod[[1]]$lendbase[,c(1,6,13,16,22)])
Neffdf <- Neffdf[Neffdf$Bin == min(Neffdf$Bin),]

# length bins
lens <- sort(unique(comps$Bin))

# fishery (fleet 1) ----

flt <- 1 # USER INPUT

# this is a 1 sex model but if it had sex structure you would define each sex as
# a separate fleet (e.g., Fishery F and Fishery M)
tmp <- comps[comps$Fleet==flt,]

# effective sample sizes (vector)
Neff <- Neffdf$effN[Neffdf$Fleet==flt]

# observed values -> put in matrix format (nrow = nyr, ncol = age/length)
obs <- tmp[tmp$variable=='Obs',]
obs <- reshape2::dcast(obs, Yr~Bin, value.var = "value")
yrs <- obs$Yr # years sampled
obs <- as.matrix(obs[,-1])

# expected values -> put in matrix format (nrow = nyr, ncol = age/length
exp <- tmp[tmp$variable=='Exp',]
exp <- reshape2::dcast(exp, Yr~Bin, value.var = "value")
exp <- as.matrix(exp[,-1])

# should all be true!
length(Neff) == length(yrs); length(Neff) == nrow(obs); nrow(obs) == nrow(exp)
ncol(obs);ncol(exp);length(lens)

out1 <- afscOSA::run_osa(fleet = 'Fishery', index_label = 'Length',
                         obs = obs, exp = exp, N = Neff, index = lens, years = yrs)


# AI bottom trawl survey (fleet 2) ----

flt <- 2 # USER INPUT

# this is a 1 sex model but if it had sex structure you would define each sex as
# a separate fleet (e.g., Survey F and Survey M)
tmp <- comps[comps$Fleet==flt,]

# effective sample sizes (vector)
Neff <- Neffdf$effN[Neffdf$Fleet==flt]

# observed values -> put in matrix format (nrow = nyr, ncol = age/length)
obs <- tmp[tmp$variable=='Obs',]
obs <- reshape2::dcast(obs, Yr~Bin, value.var = "value")
yrs <- obs$Yr # years sampled
obs <- as.matrix(obs[,-1])

# expected values -> put in matrix format (nrow = nyr, ncol = age/length
exp <- tmp[tmp$variable=='Exp',]
exp <- reshape2::dcast(exp, Yr~Bin, value.var = "value")
exp <- as.matrix(exp[,-1])

# should all be true!
length(Neff) == length(yrs); length(Neff) == nrow(obs); nrow(obs) == nrow(exp)
ncol(obs);ncol(exp);length(lens)

Neff
out2 <- afscOSA::run_osa(fleet = 'AI Trawl Survey', index_label = 'Length',
                         obs = obs, exp = exp, N = Neff, index = lens, years = yrs)

# plot results ----
input <- list(out1, out2)
osaplots <- plot_osa(input)
osaplots$bubble
osaplots$qq
osaplots$aggcomp
