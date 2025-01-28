if (doprofile) {
  # srr_dir  = c("../runs/sr0/","../runs/sr1/", "../runs/sr2/","../runs/sr3/")
  # srr_names  = c("Standard","Ignore 1978 YC","Diffuse prior","FMSY=F35%")
  i=1
  setwd(here("2024/runs/m23/prof"))
  getwd()
  fn <- NULL 
  proflst <- list()
  for (i in 1:33) {
    fn[i] <- paste0("pm_", i, ".rep")
    print(fn[i])
    proflst[[i]] <- read_rep(fn[i])
  }
  df <- NULL
  n <- length(proflst)
  mdf <- NULL
  for (i in 1:n)
  {
    A <- proflst[[i]]
    df <- data.frame(YC_2018 = A$N[56, 1])
    df$age <- sum(A$age_like)
    df$rec <- sum(A$rec_like)
    df$bts <- A$surv_like[1]
    df$ats <- A$surv_like[2]
    # df$ats1  <- A$surv_like[3]
    df$avo <- A$avo_like
    df$tot <- A$tot_like
    df$priors <- sum(A$Priors)
    mdf <- rbind(mdf, df)
  }
mdf %>%
    mutate(
      age = age - min(age),
      rec = rec - min(rec),
      bts = bts - min(bts),
      ats = ats - min(ats),
      # ats1=ats1-min(ats1),
      avo = avo - min(avo),
      tot = tot - min(tot)
    ) %>%
    pivot_longer(cols = 2:7, names_to = "type", values_to = "NLL") %>%
    ggplot(aes(x = YC_2018, y = NLL, color = type, linetype = type)) +
    geom_line(size = 1.2) +
    ggthemes::theme_few() +
    scale_x_continuous( limits=c(0,NA),label = comma) + 
    ylim(c(0, 3)) +
    xlab("Estimate of 2018 year-class (at age 1, millions)") +
    ylab("Negative log-likelihood (lack of fit score)")
  ggsave("~/_mymods/afsc-assessments/ebs_pollock_safe/doc/figs/like_profile.pdf", width = 7, height = 4, units = "in")
}
