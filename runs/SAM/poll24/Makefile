whichR = $(shell if [ -e /usr/bin/Rnewest ]; then echo "Rnewest"; else echo "R"; fi;)

myRlib :=  $(shell if [ -f ~/.Renviron ]; then . ~/.Renviron; R_LIBS=.:$$R_LIBS; else R_LIBS=.; fi; echo $$R_LIBS)
useR = R_LIBS=$(myRlib) $(whichR) --slave --no-environ

BD = run
RD = res
SD = src
DD = data
CF = conf
LD = log
plotit = echo 'source("$(SD)/plotscript.R")' | $(useR) 1> $(LD)/plot.out 2> $(LD)/plot.err; touch $(RD)/plotOK
datafiles := $(wildcard $(DD)/*.dat)
sourcefiles := $(wildcard $(SD)/*)
desfile = $(shell if [ -f $(BD)/curver ]; then tail -1 $(BD)/curver; fi; )

BASE = baserun

.PHONY = data model plot sim leaveout retro forecast updatabase button

$(BD)/curver: $(CF)/locked.ver 
	echo "x<-read.table('conf/locked.ver', col.names=c('pkg','sha'));\
	      if(nrow(x)>0){\
	        for(i in 1:nrow(x))devtools::install_github(x[i,1], ref=x[i,2]);\
	      };\
	      pd<-packageDescription('stockassessment');\
	      cat('Version:', pd[['Version']], '\n', file='$(BD)/curver');\
	      cat('RemoteSha:', substr(pd[['RemoteSha']],1,12), '\n', file='$(BD)/curver', append=TRUE);\
	      cat(attr(packageDescription('stockassessment'), 'file'), '\n', file='$(BD)/curver', append=TRUE)" | $(useR)

data: $(BD)/data.RData  
$(BD)/data.RData: $(SD)/datascript.R $(datafiles) $(BD)/curver $(desfile) 
	echo 'source("$(SD)/datascript.R")' | $(useR) 1> $(LD)/data.out 2> $(LD)/data.err

defcon: $(CF)/model.cfg 
$(CF)/model.cfg: $(BD)/data.RData
	echo 'library(stockassessment); load("$(BD)/data.RData"); saveConf(defcon(dat),"$(CF)/model.cfg") ' | $(useR) 1> $(LD)/conf.out 2> $(LD)/conf.err

model: $(BD)/model.RData
$(BD)/model.RData: $(SD)/model.R $(BD)/data.RData $(CF)/model.cfg 
	echo 'source("$(SD)/model.R")' | $(useR) 1> $(LD)/model.out 2> $(LD)/model.err
	rm -f $(BD)/leaveout.RData $(BD)/retro.RData $(BD)/forecast.RData $(BD)/residuals.RData
	echo "pd<-packageDescription('stockassessment');\
	      cat('Version:', pd[['Version']], '\n', file='$(BD)/ver');\
	      cat('RemoteSha:', substr(pd[['RemoteSha']],1,12), '\n', file='$(BD)/ver', append=TRUE)" | $(useR)


plot: $(RD)/plotOK
$(RD)/plotOK: $(BD)/model.RData $(SD)/plotscript.R $(CF)/viewextra.cfg
	$(plotit)

sim: $(BD)/residuals.RData
$(BD)/residuals.RData: $(BD)/model.RData $(SD)/residuals.R
	echo 'source("$(SD)/residuals.R")' | $(useR) 1> $(LD)/res.out 2> $(LD)/res.err
	$(plotit)

leaveout: $(BD)/leaveout.RData
$(BD)/leaveout.RData: $(BD)/model.RData $(SD)/leaveout.R
	echo 'source("$(SD)/leaveout.R")' | $(useR) 1> $(LD)/lo.out 2> $(LD)/lo.err
	$(plotit)

retro: $(BD)/retro.RData
$(BD)/retro.RData: $(BD)/model.RData $(SD)/retro.R
	echo 'source("$(SD)/retro.R")' | $(useR) 1> $(LD)/retro.out 2> $(LD)/retro.err
	$(plotit)

forecast: $(BD)/forecast.RData
$(BD)/forecast.RData: $(BD)/model.RData $(SD)/forecast.R
	echo 'source("$(SD)/forecast.R")' | $(useR) 1> $(LD)/model.out 2> $(LD)/model.err
	$(plotit)

updatebase: $(BASE)/model.RData
$(BASE)/model.RData: $(BD)/model.RData
	rm -f $(BASE)/*
	cp $(BD)/model.RData $(BASE)
	$(plotit)

checkalldata: $(LD)/checkdata.tab
$(LD)/checkdata.tab: $(datafiles) $(SD)/datavalidator.R 
	echo 'source("$(SD)/datavalidator.R"); write.table(check.all("$(DD)"),sep="|",file="$(LD)/checkdata.tab",eol="\r\n")' | $(useR) 1> $(LD)/data.out 2> $(LD)/data.err

checkallsource: $(LD)/checksource.tab
$(LD)/checksource.tab: $(sourcefiles) $(SD)/sourcevalidator.R 
	echo 'source("$(SD)/sourcevalidator.R"); write.table(check.all.source("$(SD)"),sep="|",file="$(LD)/checksource.tab",eol="\r\n")' | $(useR) 1> $(LD)/source.out 2> $(LD)/source.err

button: 
	@echo Upgrade to baserun\; updatebase\; use current run as new baserun
	@echo Add leave-one-out runs\; leaveout\; Add leave-one-out runs based on the current model 
	@echo Add retro runs\; retro\; Add retrospective runs based on the current model 
	@echo Add forecasts\; forecast\; Add forecast runs to the output
	@echo Add residuals\; sim\; Add prediction and single joint sample residuals 

doclink: 
	@echo Functions\; https://github.com/fishfollower/SAM/blob/master/docs/index.md \; Documentation for all functions

getR: 
	@echo $(useR)

getPackageVersion: $(BD)/curver
	@head -2 $(BD)/curver | sed s/Version://g | sed s/RemoteSha://g

