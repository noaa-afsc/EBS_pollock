rm(list=ls());  options(show.error.locations = TRUE)
library(AgeingError)
library(FSA)
library(dplyr)

setwd("C:/Users/Derek.Chamberlin/Work/Research/FT-NIR/Pollock_1_NIR_2_TMA_2013_2023/All_Fleets")

#reading in the TMA and NIR age data to make a frequency table
#I only use the code in lines 11-17 when first making a frequency table which I manually paste into the .dat file

df <- read.csv("C:/Users/Derek.Chamberlin/Work/Research/FT-NIR/Pollock_1_NIR_2_TMA_2013_2023/merged_file_with_source.csv")
#df <- subset(df, test_age >= 0)
#df<- subset(df, fleet == "survey")
df[is.na(df)] <- -999

df <- rename(Freq = n, count(df, final_age, test_age, round_nir))
df <- df[,c(4,1,2,3)]
write.csv(df, file = "freq_table.csv", row.names = FALSE)


pollock_dat <- AgeingError:::CreateData("pollock.dat", NDataSet=1, 
                                        verbose=TRUE, EchoFile="pollockEcho.out")

pollock_spc <- AgeingError:::CreateSpecs("pollock.spc",
                                         DataSpecs=pollock_dat, verbose=TRUE)

pollock_mod <- AgeingError::DoApplyAgeError(Species = "pollock",
                                            DataSpecs = pollock_dat,
                                            ModelSpecsInp = pollock_spc,
                                            AprobWght = 1e-06,
                                            SlopeWght = 0.01,
                                            SaveDir = "Results",
                                            verbose = FALSE)

pollock_out <- AgeingError:::ProcessResults(Species = "pollock", SaveDir = "Results", CalcEff = F, verbose = FALSE)

save.image(file="./workspace.RData")

pollock_out$ModelSelection




Species = "pollock"
SaveDir = "Results"
verbose = FALSE

{
  SaveFile <- file.path(SaveDir, paste0("/", Species, ".lda"))
  if (verbose) {
    print(SaveFile)
  }
  ReportFile <- file.path(SaveDir, paste0("/", Species, ".rpt"))
  if (verbose) {
    print(ReportFile)
  }
  load(SaveFile)
  if (verbose) {
    print(str(SaveAll))
  }
  NDataSet <- SaveAll$data$NDataSet
  Index <- which(abs(SaveAll$gradient) == max(abs(SaveAll$gradient)))
  
  Npars <- length(SaveAll$sdreport$par.fixed)
  for (IDataSet in 1:NDataSet) {
    Data <- SaveAll$data$TheData[IDataSet, 1:SaveAll$data$Npnt[IDataSet], 
    ]
  }
  MaxAge = SaveAll$data$MaxAge
  ReaderNames = NULL
  Report = SaveAll$report
  Nreaders <- ncol(Data) - 1
  Ages <- Nages <- MaxAge + 1
  MisclassArray <- array(NA, dim = c(Nreaders, Ages, Ages), 
                         dimnames = list(paste("Reader", 1:Nreaders), paste("TrueAge", 
                                                                            0:MaxAge), paste("EstAge", 0:MaxAge)))
  for (i in 1:Nreaders) {
    MisclassArray[i, , ] <- Report$AgeErrOut[i, , ]
  }
  
  Temp <- matrix(0, nrow = 5 * Nages * Nreaders, ncol = 5)
  for (Ireader in 1:Nreaders) {
    yrange <- (Ireader - 1) * Nages
    Temp[yrange + 1:Nages, 1] <- Ireader
    Temp[yrange + 1:Nages, 2] <- 0:MaxAge
    Temp[yrange + 1:Nages, 3] <- Report$TheSD[Ireader, ]/c(1, 
                                                           1:MaxAge)
    Temp[yrange + 1:Nages, 4] <- Report$TheSD[Ireader, ]
    Temp[yrange + 1:Nages, 5] <- Report$TheBias[Ireader, 
    ]
  }
  Temp <- t(Temp)
  ErrorAndBiasArray <- array(as.numeric(Temp), 
                             dim = c(5, Nages,Nreaders), 
                             dimnames = list(c("Reader", "True_Age", "CV", "SD", "Expected_age"), 
                                             paste("Age", 0:MaxAge), paste("Reader", 1:Nreaders)))
  AgeProbs <- array(NA, dim = c(nrow(Data), Ages), 
                    dimnames = list(paste("Otolith", 1:nrow(Data)), paste("TrueAge", 0:MaxAge)))
  OtI <- AgeI <- ReadI <- 1
  for (OtI in 1:nrow(Data)) {
    for (AgeI in 1:Ages) {
      AgeProbs[OtI, AgeI] <- 1
      for (ReadI in 1:Nreaders) {
        if (Data[OtI, ReadI + 1] != -999) {
          AgeRead <- Data[OtI, ReadI + 1]
          AgeProbs[OtI, AgeI] <- AgeProbs[OtI, AgeI] * 
            (MisclassArray[ReadI, AgeI, AgeRead + 1])^Data[OtI, 
                                                           1]
        }
      }
    }
  }
  TrueAge <- apply(AgeProbs, MARGIN = 1, FUN = function(Vec) {
    order(Vec[-length(Vec)], decreasing = TRUE)[1]
  }) - 1
  
  
  
  dev.new(width=15, height=5, unit="in",noRStudioGD = TRUE)
  par(oma=c(3,3,0,3),mai = c(0.25,0.5,0.25,0.5),mfrow=c(1,3))
  
  for (i in 1:Nreaders) {
    {
      # Add 0.5 to match convention in Punt model that otoliths are read
      # half way through year
      Temp <- cbind(TrueAge, Data[, i+1] + 0.5)
      # Exclude rows with no read for this reader
      Temp <- Temp[which(Data[, i+1] != -999), ]
      plot(
        x = Temp[, 1], y = Temp[, 2],
        ylim = c(0, MaxAge), xlim = c(0, MaxAge),
        col = grDevices::rgb(red = 0, green = 0, blue = 0, alpha = 0.2),
        xlab = "",
        ylab = "",
        main = paste("Reader", i),
        lwd = 2, frame.plot = F, pch = 16, cex = 2.5,yaxs="i",xaxs="i",axes=F
      )
      axis(1,at=c("",seq(0,25,by=5)),labels=c("",seq(0,25,by=5)),cex.axis=1.5)
      axis(1,at=seq(0,25,by=1),labels=F,tck=-0.01)
      axis(2,at=seq(0,25,by=5),labels=seq(0,25,by=5),las=1,cex.axis=1.5)
      axis(2,at=seq(0,25,by=1),labels=F,tck=-0.01)
      mtext(text="Predicted Age y",side=1,line=1.25,outer=TRUE,cex=1.5)
      mtext(text="Read Age y",side=2,line=1.25,outer=TRUE,cex=1.5)
      graphics::lines(
        x = c(0, MaxAge),
        y = c(0.5, MaxAge+0.5),
        lwd = 2, lty = "dashed"
      )
      graphics::lines(
        x = ErrorAndBiasArray["True_Age", , i],
        y = ErrorAndBiasArray["Expected_age", , i],
        type = "l", col = "red", lwd = 2
      )
      graphics::lines(
        x = ErrorAndBiasArray["True_Age", , i],
        y = ErrorAndBiasArray["Expected_age", , i] +
          2 * ErrorAndBiasArray["SD", , i],
        type = "l", col = "red", lwd = 2, lty = "dashed"
      )
      graphics::lines(
        x = ErrorAndBiasArray["True_Age", , i],
        y = ErrorAndBiasArray["Expected_age", , i] -
          2 * ErrorAndBiasArray["SD", , i],
        type = "l", col = "red", lwd = 2, lty = "dashed"
      )
      par(new = TRUE)
      plot(x = ErrorAndBiasArray["True_Age", , i], ErrorAndBiasArray["SD", , i],
           ylim = c(0, 3.0),
           type = "l", yaxs="i",xaxs="i",axes=F, col = "blue", lwd = 2, ylab='')
      axis(4,at=seq(0,3.0,by=0.5),labels = c('0', '0.5', '1.0', '1.5', '2.0', '2.5', '3.0'),las=1,cex.axis=1.5)
      axis(4,at=seq(0,3.0,by=0.25),labels=F,tck=-0.01)
      mtext(text="SD y",side=4,line=1.25,outer=TRUE,cex=1.5)
    }
  }
}
