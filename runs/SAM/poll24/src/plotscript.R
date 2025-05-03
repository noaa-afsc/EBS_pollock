library(stockassessment)
source("src/common.R")
# load what has been saved
setwd("run")
for(f in dir(pattern="RData"))load(f) 
setwd("..")

basefit<-NULL
if(file.exists("baserun/model.RData")){
  local({load("baserun/model.RData"); basefit<<-fit})
}else{
  basefit <- fit
}
fits <- c(base=basefit,current=fit)

exfitname <- scan("conf/viewextra.cfg", what="", comment.char="#", quiet=TRUE)
for(nam in exfitname){
  local({
    fit<-urlLoadFit(paste0("https://www.stockassessment.org/datadisk/stockassessment/userdirs/user3/",nam,"/run/model.RData"))
    if(!is.null(fit)){
      i <- length(fits)
      fits[[i+1]] <<- fit
      names(fits)[i+1] <<- nam
    }else{
      warning(paste0("View extra stock ", nam, " not found or of incompatible format (skipped)"))
    }
  })
}

plotcounter<-1
tit.list<-list()

setcap<-function(title="", caption=""){   
 tit.list[length(tit.list)+1]<<-paste("# Title",plotcounter,"\n")
 tit.list[length(tit.list)+1]<<-paste(title,"\n")
 tit.list[length(tit.list)+1]<<-paste("# Caption",plotcounter,"\n")
 tit.list[length(tit.list)+1]<<-paste(caption,"\n")
 plotcounter<<-plotcounter+1 
}



sdplot<-function(fit){
  cf <- fit$conf$keyVarObs
  fn <- attr(fit$data, "fleetNames")
  ages <- fit$conf$minAge:fit$conf$maxAge
  pt <- partable(fit)
  sd <- unname(exp(pt[grep("logSdLogObs",rownames(pt)),1]))
  v<-cf
  v[] <- c(NA,sd)[cf+2]
  res<-data.frame(fleet=fn[as.vector(row(v))],name=paste0(fn[as.vector(row(v))]," age ",ages[as.vector(col(v))]), sd=as.vector(v))
  res<-res[complete.cases(res),]
  o<-order(res$sd)
  res<-res[o,]
  par(mar=c(10,6,2,1))
  barplot(res$sd, names.arg=res$name,las=2, col=colors()[as.integer(as.factor(res$fleet))*10], ylab="SD"); box()
}


############################## plots ##############################
plots<-function(){
  par(cex.lab=1, cex.axis=1, mar=c(5,5,1,1))
    
  if(exists("fits")){
    ssbplot(fits, addCI=TRUE)
    stampit(fit)
    setcap("Spawning stock biomass", "Spawning stock biomass. 
            Estimates from the current run and point wise 95% confidence 
            intervals are shown by line and shaded area.")
    
    fbarplot(fits, addCI=TRUE)
    stampit(fit)
    setcap("Average fishing mortality", "Average fishing mortality for the shown age range. 
            Estimates from the current run and point wise 95% confidence 
            intervals are shown by line and shaded area.")

    recplot(fits, addCI=TRUE, las=0, lagR=TRUE)
    stampit(fit)
    setcap("Recruitment", "Yearly resruitment. 
        Estimates from the current run and point wise 95% confidence 
        intervals are shown by line and shaded area.")

    catchplot(fits, addCI=TRUE)
    stampit(fit)
    setcap("Catch", "Total catch in weight. 
        Prediction from the current run and point wise 95% confidence 
        intervals are shown by line and shaded area. The yearly
        observed total catch weight (crosses) are calculated as Cy=sum(WayCay).")


    srlagplot<-function (fit, textcol = "red", add = FALSE, ...) {
        X <- summary(fit)
        n <- nrow(X)
        lag <- fit$conf$minAge+1
        idxR <- (lag + 1):n
        idxS <- 1:(n - lag)
        R <- rectable(fit, lag=TRUE)[idxR, 1]
        S <- X[idxS, 4]
        Rnam <- paste0("R(age ",lag,")")
        Snam <- colnames(X)[4]
        y <- rownames(X)
        if (add) {
            lines(S, R)
        }
        else {
            plot(S, R, xlab = Snam, ylab = Rnam, type = "l", xlim = range(0, S), ylim = range(0, R), ...)
        }
        text(S, R, labels = y[idxR], cex = 0.7, col = textcol)
    }


    srlagplot(fit)
    stampit(fit)
    setcap("Spawner-resruits", "Estimated recruitment as a function of spawning stock biomass.")


    sdplot(fit)
    stampit(fit)
    setcap("...")


    plot(ypr(fit))
    stampit(fit)
    setcap("Yield per Recruit", "Yield per recruit (solid line) and spawning stock biomass plotted against different levels of fishing")
   
    if(!all(fit$conf$obsCorStruct=="ID")){ 
      corplot(fit)			  
      setcap("Estimated correlations", "Estimates correlations between age groups for each fleet")
      stampit(fit)
    }
    
    for(f in 1:fit$data$noFleets){
      fitplot(fit, fleets=f)
      setcap("Fit to data", "Predicted line and observed points (log scale)")
      stampit(fit)
    }
    
    

    #Q<-fit$pl$logFpar
    #Qsd<-fit$plsd$logFpar
    #key<-fit$conf$keyLogFpar
    #fun<-function(x)if(x<0){NA}else{Q[x+1]}
    #FF<-Vectorize(fun)
    #ages<-fit$conf$minAge:fit$conf$maxAge
    #matplot(ages, exp(t(matrix(FF(key), nrow=5))), type="l", lwd=5, lty="solid", xlab="Ages", ylab="Q")
    #legend("topright", lwd=5, col=2:5, legend=attr(fit$data, "fleetNames")[2:5])
    #stampit(fit)
    
    # Selectivity of the Fishery
    sel <- t(faytable(fit)/fbartable(fit)[,1])
    sel[is.na(sel)]<-0
    op <- par(mfrow=c(3,3), mai=c(0.4,0.5,0.2,0.2))
    age.sel<-as.integer(rownames(sel))
    for(i in round(seq(1,dim(sel)[2],length=9))){
      plot(age.sel, sel[,i], type="l", xlab="", ylab="", lwd=1.5, ylim=c(0,max(sel)))
      if (i+1<dim(sel)[2])try(lines(age.sel, sel[,i+1], col="red", lwd=1.5))
      if (i+2<dim(sel)[2]) try(lines(age.sel, sel[,i+2], col="blue", lwd=1.5))
      if (i+3<dim(sel)[2]) try(lines(age.sel, sel[,i+3], col="green", lwd=1.5))
      if (i+4<dim(sel)[2]) try(lines(age.sel, sel[,i+4], col="blue3", lwd=1.5))
      legend("bottomright",paste(c(colnames(sel)[i],colnames(sel)[i+1],colnames(sel)[i+2],colnames(sel)[i+3])), lty=rep(1,4), col=c("black","red","blue","green"),bty="n")
    }
    mtext("Age", 1, outer=T, line=1)
    mtext("F/Fbar", 2, outer=T, line=1)
    par(op)
    setcap("Selection pattern", "")
    stampit(fit)

    mn.sel <- apply(sel,1,mean)
    sd.sel <- apply(sel,1,quantile,probs=c(0.025,0.975))
    plot(age.sel, mn.sel, ylim=c(0,max(sd.sel)), type="l", xlab='Age', ylab="F/Fbar", lwd=2)
    for(i in 1:length(age.sel)){
      lines(rep(age.sel[i],2), sd.sel[,i])
    }
    setcap("Selection pattern", "")
    stampit(fit)
  }  
  
  if(exists("RES")){  
    plot(RES)
    setcap("One-observation-ahead residuals", "Standardized one-observation-ahead residuals.")
    stampit(fit)
    par(mfrow=c(1,1))
    #empirobscorrplot(RES)
    #setcap("OOA residual correlations", "Empirical correlations between ages in one-observation-ahead residuals.")
    #stampit(fit)
  }
  
   
  if(exists("RESP")){  
    plot(RESP)
    setcap("Process residuals", "Standardized single-joint-sample residuals of process increments")
    stampit(fit)
    par(mfrow=c(1,1))
  } 
 
  
  if(exists("LO")){  
    ssbplot(LO)
    setcap("Leaveout (SSB)", "")
    stampit(fit)
    
    fbarplot(LO)
    setcap("Leaveout (Average F)", "")
    stampit(fit)

    recplot(LO, lagR=TRUE)
    setcap("Leaveout (Recruitment)", "")
    stampit(fit)

    catchplot(LO)
    setcap("Leaveout (Catch)", "")
    stampit(fit)
    
  } 
  
  if(exists("RETRO")){  
    mlag1<-mohn(RETRO, lag=1)
    cm<-mohn(RETRO, lag=1, catchtable)[1]
    ssbplot(RETRO, las=0, drop=1)
    legend("topright", legend=round(mlag1[2],2), bty="n")
    setcap("Retrospective (SSB)", "")
    stampit(fit)
    
    fbarplot(RETRO, las=0, drop=1)
    legend("topright", legend=round(mlag1[3],2), bty="n")
    setcap("Retrospective (Average F)", "")
    stampit(fit)
    
    recplot(RETRO, las=0, drop=1, lagR=TRUE)
    legend("topright", legend=round(mlag1[1],2), bty="n")
    setcap("Retrospective (Recruitment)", "")
    stampit(fit)

    catchplot(RETRO)
    legend("topright", legend=round(cm,2), bty="n")
    setcap("Retrospective (Catch)", "")
    stampit(fit)
  } 
  if(exists("FC")){  
    fcplot<-function (x, ...){
      op <- par(mfrow = c(3, 1), mar=c(5,5,.1,.1))
      ssbplot(x, ...)
      fbarplot(x, drop = 1, ...)
      recplot(x, lagR=TRUE, ...)
      par(op)
    }

  
    lapply(FC, function(f){fcplot(f); title(attr(f,"label"), outer=TRUE, line=-1); stampit(fit)})
  }  
  
}


setwd('res')
file.remove(dir(pattern='png$'))
stamp<-gsub('-[[:digit:]]{4}$','',gsub(':','.',gsub(' ','-',gsub('^[[:alpha:]]{3} ','',date()))))
png(filename = paste(stamp,"_%03d.png", sep=''), width = 480, height = 480,
    units = "px", pointsize = 10, bg = "white")
  plots()    
dev.off()

writeLines(unlist(tit.list),'titles.cfg') 

png(filename = paste("big_",stamp,"_%03d.png", sep=''), width = 1200, height = 1200, 
    units = "px", pointsize = 20, bg = "white")
  plots()    
dev.off()

#pdf(onefile=FALSE, width = 8, height = 8)
#  plots()    
#dev.off()

file.remove(dir(pattern='html$'))

tsb<-tsbtable(fit)
colnames(tsb)<-c("TSB","Low", "High")
tab.summary <- cbind(summary(fit,lagR=TRUE), tsb)
xtab(tab.summary, caption=paste('Table 1. Estimated recruitment, spawning stock biomass (SSB), 
     and average fishing mortality','.',sep=''), cornername='Year', 
     file=paste(stamp,'_tab1.html',sep=''), dec=c(0,0,0,0,0,0,3,3,3,0,0,0))

ftab <- faytable(fit)
xtab(ftab, caption=paste('Table 2. Estimated fishing mortality at age','.',sep=''), cornername='Year \ Age', 
     file=paste(stamp,'_tab2.html',sep=''), dec=rep(3,ncol(ftab)))

ntab <- ntable(fit)
xtab(ntab, caption=paste('Table 3. Estimated stock numbers at age','.',sep=''), cornername='Year \ Age', 
     file=paste(stamp,'_tab3.html',sep=''), dec=rep(0,ncol(ntab)))

ptab <- partable(fit)
xtab(ptab, caption=paste('Table 4. Table of model parameters','.',sep=''), cornername='Parameter name', 
     file=paste(stamp,'_tab4.html',sep=''), dec=rep(3,ncol(ptab)))

mtab <- modeltable(c(Current=fit, base=basefit))
mdec <- c(2,0,2,6)[1:ncol(mtab)]
xtab(mtab, caption=paste('Table 5. Model fitting','.',sep=''), cornername='Model', 
     file=paste(stamp,'_tab5.html',sep=''), dec=mdec)
     
sdState<-function(fit, y=max(fit$data$years)-1:0){
  idx <- names(fit$sdrep$value) == "logR"
  sdLogR<-fit$sdrep$sd[idx][fit$data$years%in%y]
  idx <- names(fit$sdrep$value) == "logssb"
  sdLogSSB<-fit$sdrep$sd[idx][fit$data$years%in%y]
  idx <- names(fit$sdrep$value) == "logfbar"
  sdLogF<-fit$sdrep$sd[idx][fit$data$years%in%y]
  ret<-cbind(sdLogR, sdLogSSB, sdLogF)
  rownames(ret)<-y
  colnames(ret)<-c("sd(log(R))", "sd(log(SSB))", "sd(log(Fbar))")
  return(ret)
}

sdtab <- sdState(fit)
xtab(sdtab, caption=paste('Table 6. Table of selected sd','.',sep=''), cornername='Year', 
     file=paste(stamp,'_tab6.html',sep=''), dec=rep(3,ncol(sdtab)))

ct<-catchtable(fit, obs=TRUE)
ctab<-round(cbind(ct,delta=ct[,4]-ct[,1], deltaPct=round(100*(ct[,4]-ct[,1])/ct[,1])))
xtab(ctab, caption=paste('Table 7. Table of total catch','.',sep=''), cornername='year', 
     file=paste(stamp,'_tab7.html',sep=''), dec=rep(0,ncol(ctab)))
     

nftab<-function(forecast){
  mat<-t(sapply(forecast, function(yr)exp(apply(yr$sim,2,median))))
  minA<-attr(forecast, "fit")$conf$minAge
  maxA<-attr(forecast, "fit")$conf$maxAge
  matN<-mat[,(minA:maxA)-minA+1]
  matF<-mat[,-c((minA:maxA)-minA+1)]  
  yr<-sapply(forecast,function(f)f$year)
  colnames(matN)<-paste0("N",minA:maxA)
  idx <- attr(forecast,"fit")$conf$keyLogFsta[1, ] + 2
  matF <- cbind(NA, matF)[, idx]
  matF[, idx == 1] <- 0
  colnames(matF)<-paste0("F",minA:maxA)
  res<-cbind(matN,matF)
  rownames(res)<-yr
  return(res)
}


if(exists("FC")){  
    ii<-0
    lapply(FC, function(f){
       ii<<-ii+1;
       tf<-attr(f,"tab");
       dec<-c(3,3,3,rep(0,ncol(tf)-3));
       xtab(tf, caption=paste0('Forecast table ',ii,'. ', attr(f,"label"),'.'), 
       cornername='Year', file=paste(stamp,'_tabX',ii,'.html',sep=''), dec=dec);      
       })
       
    ii=ii+1   
  nft<-nftab(FC[[length(FC)]])
  dec<-ifelse(colMeans(nft)>10,0,3)
  xtab(nft, caption=paste0('Forecast table ',ii,'. N and F for option ', attr(FC[[length(FC)]],"label"),'.'), 
       cornername='Year', file=paste(stamp,'_tabXX',ii,'.html',sep=''), dec=dec);      

  xtab(Pabove, caption=paste0('Forecast table ',ii,'. Probability above Blim.'), 
       cornername='', file=paste(stamp,'_tabXX',ii,'.html',sep=''), dec=1);      

  colMedian<-function(X){
    apply(X, 2,median)
  }

  fore<-FC[[length(FC)]]
  years<-sapply(fore,function(x)x$year)
  Nstr<-paste0("N",colnames(ntable(fit)))
  Fstr<-paste0("F",sapply(sapply(0:max(fit$conf$keyLogFsta), function(k)((fit$conf$minAge):(fit$conf$maxAge))[which(fit$conf$keyLogFsta[1,]==k)]),paste,collapse=","))
  states<-t(sapply(fore,function(x)colMedian(exp(x$sim))))
  rownames(states)<-years
  colnames(states)<-c(Nstr,Fstr)
  xtab(states, caption='Table Xd. Forecasts', cornername='Description\\Variable', 
     file=paste(stamp,'_tabXd.html',sep=''), dec=c(rep(0,length(Nstr)),rep(2,length(Fstr))))  


}  

setwd("..") 

