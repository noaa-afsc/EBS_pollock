source(here::here("R/rpm.R"))
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
ATS_likelihood()
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
