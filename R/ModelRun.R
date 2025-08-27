#Top######################################
source(here::here("R/config.R"))
data.frame(par = names(obj$par), gr = abs(as.numeric(obj$gr())), gr_orig = as.numeric(obj$gr())) |> 
arrange(desc(gr))  |> select(Parameter=par,gradient=gr_orig) |> head(10)

fit <- nlminb(obj$par, obj$fn, obj$gr); 
cest <- check_estimability(obj);cest$BadParams[cest$BadParams$Param_check=="Bad",]
unique(names(parms))
sdr <- sdreport(obj)
rtmb_df <- tibble( name = names(sdr$value), sd   = sdr$sd )
rtmb_df |> filter(sd==0) |> print(n=Inf)
vals <- sdr$value
sds  <- sdr$sd

df <- rbind(data.frame(
  name  = names(vals),
  value = as.numeric(vals),
  std.dev = as.numeric(sds),
  model = "RTMB"
) , #|> filter(name=="SSB"), 
data.frame(
  read_table(here::here("runs/rtmb", "pm.std")) |> 
    filter(name == "SSB" | name == "pred_rec") |>  
    transmute( name = ifelse(name == "SSB", "SSB", "Recruits"), 
      value,  std.dev,  model = "ADMB" )
)) |> 
  mutate( name = ifelse(name == "recruitment", "Recruits", name) )
df1 <- df |> filter(name %in% c("SSB", "Recruits"))
df1$year <- rep(1964:2024,4) 
p1<- df1 |> filter(name=="SSB") |>   ggplot(aes(x=year,y=value,color=model)) +
  geom_line(linetype=2) +
  geom_line(aes(x=year,y=std.dev*10)) +
  labs(title = "RTMB vs ADMB SSB Standard Deviations", x = "Year ", y = "SSB, Standard Deviation x 10") +
theme_minimal() 
p2<- df1 |> filter(name!="SSB") |>   ggplot(aes(x=year,y=value,color=model)) +
  geom_line(linetype=2) +
  geom_line(aes(x=year,y=std.dev*10)) +
  labs(title = "RTMB vs ADMB recruitment Standard Deviations", x = "Year ", y = "recruits, Standard Deviation x 10") +
  theme_minimal()
p1/ p2 
# numDeriv::grad(obj$fn, obj$par)
# numDeriv::hessian(obj$fn, obj$par)
# saveRDS(obj,"rtmb2.RDS")
#obj<- readRDS("cea_obj.RDS")
#Ratio plots######################################
# Function to create ratio plot for values (with error bars)


# Example usage:
# Create separate plots for values and standard deviations
plot_ssb_value_ratio <- create_value_ratio_plot(df1, variable_name = "SSB", 
                                                numerator_model = "RTMB", 
                                                denominator_model = "ADMB")

plot_ssb_stddev_ratio <- create_stddev_ratio_plot(df1, variable_name = "SSB", 
                                                  numerator_model = "RTMB", 
                                                  denominator_model = "ADMB")
rec_ratio <- create_value_ratio_plot(df1, variable_name = "Recruits", 
                                                numerator_model = "RTMB", 
                                                denominator_model = "ADMB")

rec_sd_ratio <- create_stddev_ratio_plot(df1, variable_name = "Recruits", 
                                                  numerator_model = "RTMB", 
                                                  denominator_model = "ADMB")


(plot_ssb_value_ratio)/ (plot_ssb_stddev_ratio)/
(rec_ratio)/ (rec_sd_ratio)

#--MCMC stuff--------
library(adnuts)
obj$env$data 
names(obj$env$parList() )
?sample_snuts
mcmc <- sample_snuts(obj, chains=1, cores=1, iter=5000, control = list(adapt_delta=.98))
#mcmc <- adnuts::sample_sparse_tmb(obj,skip_optimization=TRUE,iter=3000, chains = 8)
#mcpilot <- adnuts::sample_sparse_tmb(obj,skip_optimizatioon=TRUE,iter=2000, chains = 5)
plot_uncertainties(mcmc)
pairs_admb(mcmc, pars=1:8, order='slow')
pairs_admb(mcmc, pars=1:8, order='mismatch')
pairs_admb(mcmc, pars=1200:1208)
sum(grepl("log_F_devs", mcmc$par_names))

#Merge RTMB and ADMB sds ######################################
# RTMB standard deviations
sdr = sdrep #sdreport(obj) # obj is your optimized model object
names(sdr$value)
str(as.list(sdr, "Std", report=TRUE)) # Standard errors for parameters
as.data.frame(as.list(sdr, "Std", report = TRUE) )# Standar
is.list(sdr)
head(sdr)
names(sdr)
names(sdr$sd)
(sdr$sd)
rtmb_df <- tibble(
  name = names(sdr$value),
  sd   = sdr$sd
)
rtmb_df |> filter(sd==0) |> print(n=Inf)
rtmb_df |> filter(name=="sel_slp_bts_dev")
rtmb_df |> filter(name=="sel_a50_bts_dev")

admb_df  <- read_table(here::here("runs/rtmb", "pm.std"))[1:1209, ] |> transmute(name,sd=std.dev)
dim(admb_df)
dim(rtmb_df)
tail(rtmb_df[1:1109,],20)
# join by name
comp <- merge(rtmb_df, admb_df, by = "name", suffixes = c(".rtmb", ".admb"))
comp <- left_join(rtmb_df, admb_df, by = "name") #, suffixes = c(".rtmb", ".admb"))

# compute absolute and relative differences
comp$abs_diff <- abs(comp$sd.rtmb - comp$sd.admb)
comp$rel_diff <- comp$abs_diff / comp$sd.admb

# sort by largest difference
comp_sorted <- comp[order(-comp$abs_diff), ]

# top differences
head(comp_sorted, 20)
tail(comp_sorted, 200)
#######################################
# Plot the parameter estimates with error bars
p_grouped <- df %>%
  group_by(name) %>%
  mutate(param_id = paste0(name, "_", row_number())) %>%
  ggplot(aes(x = reorder(param_id, index), y = value)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = value - 2*std.dev, ymax = value + 2*std.dev), 
                width = 0.2, color = "red") +
  labs(title = "Parameter Estimates with Error Bars (±2 SD)",
       x = "Parameter",
       y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        plot.title = element_text(hjust = 0.5))

# Display the grouped version
print(p_grouped)

df <- read_table(here::here("runs/rtmb","pm.std"))[1:1209, ] 
#--Compare all parameter values---------------
p <- ggplot(df, aes(x = factor(index), y = value)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = value - 2*std.dev, ymax = value + 2*std.dev), 
                width = 0.2, color = "red") +
  scale_x_discrete(labels = df$name) +
  labs(title = "Parameter Estimates with Error Bars (±2 SD)",
       x = "Parameter",
       y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        plot.title = element_text(hjust = 0.5))

# Display the plot
print(p)

rpm(parms)
obj$fn()
fit$par


write_csv(df, here::here("Rtmb","gr.csv"))
head(df)
max(df$gr)
#obj$gr() sum(obj$par ==0) max(abs(obj$gr()))
fit <- nlminb(obj$par, obj$fn, obj$gr)
df2 <- data.frame(par = names(obj$par), gr = as.numeric(obj$gr()))
max(df2$gr)
df3<- left_join(df, df2, by = "par", suffix = c(".old", ".new")) |>
  dplyr::mutate(gr_diff = abs(gr.old - gr.new)) |>
  dplyr::arrange(desc(gr_diff)) |>
  dplyr::filter(gr_diff > 1e-6) |>
  dplyr::select(par, gr.old, gr.new, gr_diff)

head(df3)
for (i in 1:3) fit<- nlminb(fit$par, obj$fn, obj$gr )
cest <- (check_estimability(obj)) #|>  filter(Param_check=="Bad") 
cest$BadParams[cest$BadParams$Param_check=="Bad",]
names(cest$BadParams)
max(obj$gr)
obj$fn()
obj$gr()
(fit$par["log_avgrec"])

df<-NULL

fit <- nlminb(fit$par, obj$fn, obj$gr )
names(map_obj)
str(map_obj)
r1 <- obj$report()
fit1 <- nlminb(obj$par, obj$fn, obj$gr )
fit1<- nlminb(fit1$par, obj$fn, obj$gr )
obj$fn()
r2 <- obj$report()
r3 <- obj$report()
plot(data.frame(r1=log(unlist(r1)), r2=log(unlist(r2))))
(data.frame(r1=log(unlist(r1)), r2=log(unlist(r2))))
lines(1:10,1:10)
unlist(r1)/ unlist(r2)
unlist(r1); unlist(r2)
unlist(r2)/ unlist(r3)
r2
fit
names(fit)
unique(names(fit$par))
unique(names(fit$par))

data$return_nll_only <- TRUE # Flag to only return nll
res <- rpm(parms)
(res$nll)
gt_compare_table(res$rtmb, pm, tolerance = 1e-2, sort_by_diff=FALSE)

names(fit)
fit$objective
fit$message
fit$convergence
report(fit)
fit$par


 # parms <- force_numeric_conversion(vars$parameters)

 #parms <- ensure_tmb_types(parms)
 #str(parms)

rpm(parms)
data


getAll(vars$parameters, vars$data )

obj <- MakeADFun(cmb(rpm, data), parms )
obj <- MakeADFun(rpm, vars$data, vars$parameters )




source(here::here("R/rpm.R"))
rtmb <- list(
  N = natage,                               # from GetNumbersAtAge()
  Z = Z,                                    # from Get_Mortality_Rates()
  F = F,                                    # from Get_Mortality_Rates()
  M = M,                                    # from Get_Mortality_Rates()
  S = exp(-Z),                              # or from Get_Mortality_Rates()
  C = C,  #F / Z * (1 - exp(-Z)) * natage,       # Baranov catch
  pred_catch = pred_catch,
  obs_catch = obs_catch,               # if available
  SSB = SSB,                                # computed inside GetNumbersAtAge()
  phizero = phizero,                  # from Get_Bzero()
  Bzero = Bzero,                      # from Get_Bzero()
  steepness = steepness,              # input parameter
  pred_cpue=pred_cpue,
  pred_avo=pred_avo,
  eb_bts = eb_bts,                        # BTS biomass
  eb_ats = eb_ats,                        # BTS biomass
  sel_fsh = sel_fsh,                        # selectivity-at-age
  sel_bts = exp(log_sel_bts),
  sel_ats = exp(log_sel_ats),
  phat_fsh = eac_fsh,
  phat_bts = eac_bts,
  phat_ats = eac_ats,
  Priors   = Priors, 
  rec_like = rec_like$rec_like,
  age_like = age_like,
  age_like_offset = age_like_offset,
  sel_like = (sel_like),
  sel_like_dev = (sel_like_dev),
  bts_like = bts_like,
  ats_like = ats_like,
  ats_age1_like = ats_age1_like,
  cpue_like = cpue_like, 
  avo_like = avo_like, 
  cat_like = cat_like,
  tot_like = tot_like +pm$wt_like
)

# matplot(rtmb$phat_bts/pm$phat_bts, type="b")
# rowSums(rtmb$phat_bts[,1:15])
# rowSums(pm$phat_bts[,1:15])


gt_compare_table(rtmb, pm, tolerance = 1e-2, sort_by_diff=FALSE)
sel_like_dev/pm$sel_like_dev
sel_like_dev;pm$sel_like_dev
tot_like;pm$tot_like
pm$rec_like

names(pm)
#--Need to fix admb matrices that have year as a column
pm$et_ats 
et_ats
log(pm$sel_ats[31:61,1])
exp(log_sel_ats[,1])
pm$ea1_ats
rbind(data.frame( est=pm$eb_ats, year=yrs_ats_data, model="pm"),
data.frame( est=eb_ats, year=yrs_ats_data, model="RTMB")) |> 
  ggplot(aes(x=year, y=est, color=model)) +
  geom_line() +
  labs(title = "Estimated ATS Biomass", x = "Year", y = "Biomass (kt)") +
  theme_minimal()
matplot((eac_ats[,2:10])/pm$phat_ats[,3:11],type='b')
dim((eac_ats[,2:10]))
dim(pm$phat_ats[,2:10])

matplot((eac_ats[,2:10]),type='b')
matplot((pm$phat_ats[,3:11]),type='b')
rowSums((eac_ats[,2:15]))
rowSums((pm$phat_ats[,2:15]))
matplot((eac_ats[,2:10]/pm$phat_ats[,2:10]),type='b')

plot(pm$eb_ats)
dim(pm$sel_ats)
dim(log_sel_ats)
dim(pm$sel_ats[(1994:endyr-1)-1963,])
matplot(exp(log_sel_ats[,1:15])/pm$sel_ats[,1:15],type='b')
matplot(exp(log_sel_ats[,1:10]), type='b')
rowMeans(exp(log_sel_ats[,1:15]))
rowMeans(pm$sel_ats[,1:15])
matplot(pm$sel_ats[(1994:endyr)-1963,1:10],type='b')
pm$bts_like <- pm$surv_like[1]
pm$ats_like <- pm$surv_like[2]
pm$ats_age1_like <- pm$surv_like[3]
pm$surv_like
pm$cpue_like
pm$ats_like
rtmb$ats_like
rtmb$bts_like
names(pm)
matplot(rtmb$N/pm$N, type="b")
matplot(rtmb$F/pm$F, type="b")
matplot(rtmb$Z/pm$Z, type="b")
matplot(rtmb$S/pm$S, type="b")
matplot(rtmb$sel_fsh/pm$sel_fsh, type="b")
matplot(rtmb$sel_bts/pm$sel_bts, type="b")

c(rtmb$bts_like, pm$bts_like)
pm$sel_fsh
pm$pred_catch 
rtmb$pred_catch
rtmb$obs_catch
pm$obs_catch

plot(pm$pred_catch / rtmb$pred_catch)

compare_row_means <- function(pm, rtmb, variables = 
                                c("sel_fsh", "N", "F", "M", "Z", "S", "C")) {
  results <- lapply(variables, function(vname) {
    x <- pm[[vname]]
    y <- rtmb[[vname]]
    
    if (!all(dim(x) == dim(y))) {
      warning(sprintf("Dimension mismatch in %s", vname))
      return(NULL)
    }
    
    mean_x <- rowMeans(x)
    mean_y <- rowMeans(y)
    diff <- mean_x - mean_y
    
    data.frame(
      year = seq_along(mean_x),
      mean_pm = mean_x,
      mean_rtmb = mean_y,
      abs_diff = abs(diff),
      rel_diff = abs(diff) / pmax(abs(mean_x), 1e-8),
      variable = vname
    )
  })
  
  do.call(rbind, results)
}

compare_models <- function(pm, rtmb) {
  vars_to_check <- c("N", "F", "M", "Z", "S", "C","sel_fsh", "sel_bts", "sel_ats")
  
  compare_matrix <- function(name) {
    diff <- pm[[name]] - rtmb[[name]]
    max_diff <- max(abs(diff))
    if (max_diff > 1e-6) {
      cat(sprintf("⚠️  %s mismatch (max diff = %.3g)\n", name, max_diff))
    } else {
      cat(sprintf("✅ %s matches (max diff = %.3g)\n", name, max_diff))
    }
    invisible(max_diff)
  }
  
  # Compare core matrices
  sapply(vars_to_check, compare_matrix)
  
  # Compare vectors/scalars
  scalar_diffs <- list(
    Bzero      = abs(pm$Bzero - rtmb$Bzero),
     phizero    = abs(pm$phizero - rtmb$phizero)
  #   pred_catch = max(abs(pm$pred_catch - rtmb$pred_catch)),
  #   SSB        = max(abs(pm$SRR_SSB - rtmb$SSB))
   )
  # 
  for (name in names(scalar_diffs)) {
    if (scalar_diffs[[name]] > 1e-6) {
      cat(sprintf("⚠️  %s mismatch (diff = %.3g)\n", name, scalar_diffs[[name]]))
    } else {
      cat(sprintf("✅ %s matches (diff = %.3g)\n", name, scalar_diffs[[name]]))
    }
  }
}
image_diff <- function(a, b, title = "") {
  diff <- a - b
  image(diff, main = title)
}

image_diff(pm$N, rtmb$N, "N difference")
image_diff(pm$sel_fsh, rtmb$sel_fsh, "F difference")
# Assuming your new output is stored in a list `rtmb_out` matching structure
compare_models(pm, rtmb)
pm$SRR_SSB

mean_comp <- compare_row_means(pm, rtmb)
(mean_comp)
library(ggplot2)

plotly::ggplotly(ggplot(mean_comp, aes(x = year, y = abs_diff, color = variable)) +
  geom_line() +
  labs(title = "Absolute difference in mean by year", y = "Abs diff", x = "Year") +
  theme_minimal())

names(mean_comp)
library(dplyr)
plotly::ggplotly(mean_comp |> filter(variable != "N" & variable != "C") |>  ggplot(aes(x = year, y = rel_diff, color = variable)) +
  geom_line() +
  labs(title = "Relative difference in mean by year", y = "Relative diff", x = "Year") +
  theme_minimal() )
natage[2,11:15]
pm$N[1,11:15]
pm$sel_fsh[61,1:8]
sel_fsh[61,1:8]


parse_admb_bar <- function(par_file, bar_file, verbose = TRUE) {
  # ---- Step 1: Parse ASCII .par file ----
  par_lines <- readLines(par_file)
  par_values <- list()
  param_names <- c()
  param_lengths <- c()
  
  i <- 1
  while (i <= length(par_lines)) {
    line <- trimws(par_lines[i])
    if (startsWith(line, "#")) {
      # Extract variable name
      param_name <- sub("^#\\s*", "", line)
      param_name <- sub(":\\s*$", "", param_name)
      
      # Read subsequent value lines
      i <- i + 1
      value_lines <- c()
      while (i <= length(par_lines) && !startsWith(trimws(par_lines[i]), "#")) {
        value_lines <- c(value_lines, par_lines[i])
        i <- i + 1
      }
      
      values <- scan(text = paste(value_lines, collapse = " "), quiet = TRUE)
      par_values[[param_name]] <- values
      param_names <- c(param_names, param_name)
      param_lengths <- c(param_lengths, length(values))
    } else {
      i <- i + 1
    }
  }
  
  if (verbose) cat("Parsed", length(par_values), "parameters from", par_file, "\n")
  
  # ---- Step 2: Read binary .bar file ----
  con <- file(bar_file, "rb")
  bin_vec <- readBin(con, what = "double", n = sum(param_lengths), size = 8, endian = "little")
  close(con)
  
  if (length(bin_vec) != sum(param_lengths)) {
    stop("Binary length does not match .par variable lengths")
  }
  
  # ---- Step 3: Reconstruct named list from binary vector ----
  bar_values <- list()
  offset <- 1
  for (j in seq_along(param_names)) {
    len <- param_lengths[j]
    bar_values[[param_names[j]]] <- bin_vec[offset:(offset + len - 1)]
    offset <- offset + len
  }
  
  names(bar_values) <- param_names
  
  # ---- Step 4: Optional comparison ----
  mismatches <- which(!mapply(all.equal, par_values, bar_values, tolerance = 1e-8))
  if (verbose && length(mismatches) == 0) {
    cat("✅ All binary parameters match ASCII values.\n")
  } else if (length(mismatches) > 0) {
    cat("⚠️  Mismatches found in parameters:\n")
    cat("   ", paste(param_names[mismatches], collapse = ", "), "\n")
  }
  
  invisible(list(from_par = par_values, from_bar = bar_values))
}

parsed <- parse_admb_bar(here::here("Rtmb","pm.par"), 
                         here::here("Rtmb","pm.bar"))

# Access parameter values
parsed$from_bar$log_Rzero
parsed$from_bar$log_initdevs[1:5]

# Check exact match
all.equal(parsed$from_bar$log_avgrec, parsed$from_par$log_avgrec)
