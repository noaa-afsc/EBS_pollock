#' Plot Retro Function
#'
#' This function plots the retro analysis for the given model outputs.
#'
#' @param M A list containing model outputs for the retro analysis.
#' @param main A string representing the main title of the plots. Default is an empty string.
#'
#' @return A ggplot object with the retro analysis visualization.
#' @importFrom gganimate transition_states shadow_mark anim_save transition_reveal
#' @importFrom ggplot2 geom_ribbon geom_line scale_x_continuous scale_y_continuous
#' @importFrom scales comma
#' @importFrom data.table data.table rbind
#' @importFrom tidyr gather
#' @importFrom dplyr filter mutate transmute
#' @export
library(gganimate)
library(scales)
plot_retro <- function(M, main = ""){

  # Assuming retouts is a global variable or passed from some context
  M <- retouts
  M <- ret1

  df <- data.table(M[[1]]$SSB, case = 0)
  df <- data.frame(M[[1]]$SSB, case = 0)

  for (i in 1:length(M)){
    df <- rbind(df, data.frame(M[[i]]$SSB, case = i))
  }

  names(df) <- c("yr", "SSB", "SE", "lb", "ub", "case")
  df$peel = df$case
  df$case <- as.factor(df$case)

  df <- df |> filter(yr > 1977, yr <= 2023) |>
            mutate(peel = ifelse(peel == 0, 99, peel))

  p1 <- ggplot(df, aes(x = yr, y = SSB, fill = case)) +
        geom_ribbon(aes(ymin = lb, ymax = ub), alpha = .2) +
        geom_line(linewidth = 0.6, aes(col = case)) +
        scale_x_continuous(limits = c(2000, 2025), breaks = seq(2000, 2025, 2)) +
        scale_y_continuous(label = comma, limits = c(0, 6.2e3)) +
        ylab("Spawning biomass (kt)") +
        xlab("Year") +
        theme(legend.position = "none") +
        guides(fill = FALSE, alpha = FALSE, col = FALSE)

  p1 <- p1 + transition_states(-peel, transition_length = .3, state_length = 3) +
            shadow_mark(alpha = 0.2) + ggthemes::theme_few()
  p1

  anim_save("doc/figs/retro_ssb_an.gif")


  # Rest of the function goes here, similarly cleaned and formatted.
	df <- NULL
	for (i in 1:length(M))
		df <- rbind(df,data.frame(M[[i]]$R,case=i))
	names(df) <- c("yr","R","SE","lb","ub","case")
	df$peel=df$case
	df <- filter(df,yr>1978,yr<=2024) |> mutate(peel=ifelse(peel==0,99,peel))
  df$coh <- df$yr-1
  df$termyr <- 2025-df$case
	df$case <- as.factor(df$case)
  # Pick out selected cohorts for squidding...
	df
	dt_dc <- dcast(df, yr ~ case, value.var = c("R"))
	dt_dc <- df %>%
	  pivot_wider(names_from = case, values_from = R, names_prefix = "R_", id_cols = yr)
	dt_dc <-data.frame(dt_dc)
	dt_dc
  names(dt_dc) <- c("Year",0:20)
	glimpse(dt_dc)
	glimpse(df)
	tail(df)
	theme_set(theme_bw())
	dim(dt_dc)
	bdft <- dt_dc %>% transmute(Year,dt_dc[,2:17]/dt_dc[,2])
	glimpse(bdft)
	tail(bdft)
	#df.m <- gather(bdft,peel,2:17)
	df.m <- bdft %>%
	  pivot_longer(cols = 2:17, names_to = "peel", values_to = "value")
	tail(df,30 )
	theme_set(ggthemes::theme_few(base_size=16))
	p1<- ggplot(df%>%filter(yr<(termyr)),aes(x=yr,y=R,fill=case)) +
         geom_ribbon(aes(ymin=lb,ymax=ub),alpha=.2)  +
         geom_line(size=0.7,aes(col=case)) + scale_x_continuous(limits=c(2000,2024),breaks=seq(2000,2024,2))  +
         scale_y_continuous(label=comma) + ggthemes::theme_few(base_size = 16) +
	       coord_cartesian(ylim=c(0,1.1e5)) +
	       ylab("Age-1 Recruitment (millions)") + xlab("Year") +
         geom_ribbon(data=df %>% filter(termyr==2024,yr<termyr),aes(ymin=lb,ymax=ub),alpha=.2)  +
         geom_line(data=df %>% filter(termyr==2024,yr<termyr),aes(y=R,x=yr))  +
         guides(fill=FALSE,alpha=FALSE,col=FALSE) ;p1

	options(gganimate.dev_args = list(width = 1000, height = 600))
  p1 + transition_states(-peel, transition_length = .3, state_length = 3) + shadow_mark(alpha = 0.2)
  p1
	anim_save("doc/figs/retro_R_an.gif")

	p1<- ggplot(df%>%filter(yr<(termyr)),aes(x=yr,y=R,fill=case,color=case)) +
	      geom_line() +
         #geom_ribbon(aes(ymin=lb,ymax=ub),alpha=.2)  +
         #geom_line(size=1.0) + scale_x_continuous(limits=c(2000,2022),breaks=seq(2000,2022,2))  +
         #scale_y_continuous(label=comma,limits=c(0,1.1e5)) +
	       ylab("Age-1 Recruitment (millions)") + xlab("Year") +
         guides(fill=FALSE,alpha=FALSE,col=FALSE) ;p1
         glimpse(p1$data)
         p1 + transition_reveal(termyr) #+ labs(title = "Year: {frame_time}")
         p1
	ggsave("doc/figs/mod_retro2.pdf",plot=p1,width=6,height=4,units="in")
	p1 <- ggplot(df.m,aes(x=yr,y=SSB,col=case)) +geom_line(size=1.3) + scale_x_continuous(limits=c(1990,2017)) + scale_y_continuous(label=comma, limits=c(0,5e3)) + ylab("Spawning biomass (kt)") + xlab("Year") +  .THEME   + theme(legend.position="none")
	p2 <- ggplot(df,aes(x=yr,y=RSSB,col=case)) +geom_line(size=1.3) + scale_x_continuous(limits=c(1990,2017)) + scale_y_continuous(label=comma, limits=c(0,2)) + ylab("Relative spawning biomass ") + xlab("Year") +  .THEME   + theme(legend.position="none")
	for (i in 1:10) {
	  tdf <- data.frame(cbind(bdft[1],SSB=bdft[5+i]))[1:(40-i),]
	  names(tdf) <- c("yr", "SSB")
	  p2 <- p2 + geom_line(data=tdf, aes(x=yr,y=SSB),col=i,size=1.5)
	  tdf <- tdf[dim(tdf)[1],]
	  p2 <- p2 + geom_point(data=tdf,aes(x=yr,y=SSB),size=4,col=i)
	}
	p2 <-p2 +geom_hline(aes(yintercept=1),size=3,linetype="dotted")
	p2 <-p2 +geom_hline(aes(yintercept=1),size=1,col="grey")
	p2
	# Mohn's rho
	ret1[["r_0"]]$SSB[,2]
	rc = ret1[["r_0"]]$SSB[,2]
	ntmp=0
	rho=0
	rn
	i=1
	for (i in 1:20) {
		rn <- names(ret2[i])
	  dtmp = (get(paste0(rn))$SSB )
	  lr   = length(dtmp[,1])
	  ntmp = ntmp+(dtmp[lr,2] -rc[lr])/rc[lr]
	  #rho = rho + (-(ALL[i,2]-ALL[*tsyrs-i,2+i]))/ALL[(j)*tsyrs-i,2]
	  rho  = rho + (-(dtmp[i,2]-rc[i]))/rc[lr]
	  print(paste(i,ntmp/i,rho))
	}
	if (main=="") p2 = p2+ggtitle(rn)
	if (main!="") p2 = p2+ggtitle(main)
	return(p2)
}

