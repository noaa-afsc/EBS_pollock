# Script to validate data to match the state-space assessment model 
# 
# Anders Nielsen <anders@nielsensweb.org> Jul. 2012,...
#library(stockassessment)

formals(readLines)$warn <- FALSE

read.table.nowarn<-function(...){
  tryCatch.W.E <- function(expr)
  {
    W <- NULL
    w.handler <- function(w){ # warning handler
      if(!grepl('incomplete final line',w))W<<-w
      invokeRestart("muffleWarning")
    }
    list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),warning = w.handler),warning = W)
  }
  lis<-tryCatch.W.E(read.table(...))
  if(!is.null(lis$warning))warning(lis$warning)
  lis$value
}

is.whole.positive.number <- function(x, tol = .Machine$double.eps^0.5){
  (abs(x - round(x)) < tol)&(x>=0)
}

read.surveys<-function(filen){
  # Function to read ices survey file 
  lin<-readLines(filen,warn=FALSE)[-c(1:2)]
  empty<-which(lapply(lapply(strsplit(lin, split='[[:space:]]+'), 
               paste, collapse=''), nchar)==0)
  if(length(empty)>0){
    lin<-lin[-empty]
  }
  lin<-sub("^\\s+", "",lin)
  idx1<-grep('^[A-Z#]', lin, ignore.case=TRUE)
  idx2<-c(idx1[-1]-1,length(lin))
  names<-lin[idx1]
  years<-matrix(as.numeric(unlist(strsplit(lin[idx1+1], '[[:space:]]+'))), ncol=2, byrow=TRUE)
  times<-matrix(as.numeric(unlist(strsplit(lin[idx1+2], '[[:space:]]+'))), ncol=4, byrow=TRUE)[,3:4,drop=FALSE]
  ages<-matrix(as.numeric(unlist(lapply(strsplit(lin[idx1+3], '[[:space:]]+'), function(x)x[1:2]))), ncol=2, byrow=TRUE)
  for(i in 1:length(names)){
    # Check years 
    if(!is.whole.positive.number(years[i,1])){
      stop(paste("In file",filen, ": Minimum year is expected to be a positive integer number for fleet number",i))
    }
    if(!is.whole.positive.number(years[i,2])){
      stop(paste("In file",filen, ": Maximum year is expected to be a positive integer number for fleet number",i))
    }
    if(years[i,1]>years[i,2]){
      stop(paste("In file",filen, ": Maximum year is expected to be greater than minimum year for fleet number",i))
    }
    # Check ages 
    ##if(!is.whole.positive.number(ages[i,1])){
    ##  stop(paste("In file",filen, ": Minimum age is expected to be a positive integer number for fleet number",i))
    ##}
    ##if(!is.whole.positive.number(ages[i,2])){
    ##  stop(paste("In file",filen, ": Maximum age is expected to be a positive integer number for fleet number",i))
    ##}    
    if(ages[i,1]>ages[i,2]){
      stop(paste("In file",filen, ": Maximum age is expected to be greater than minimum age for fleet number",i))
    }
    # Check times
    if((times[i,1]<0)|(times[i,1]>1)){
      stop(paste("In file",filen, ": Minimum survey time is expected to be within [0,1] for fleet number",i))
    } 
    if((times[i,2]<0)|(times[i,2]>1)){
      stop(paste("In file",filen, ": Maximum survey time is expected to be within [0,1] for fleet number",i))
    } 
    if(times[i,2]<times[i,1]){
      stop(paste("In file",filen, ": Maximum survey time is expected greater than minimum survey time for fleet number",i))
    } 
  }
    
  as.num <- function(x, na.strings = "NA") {
    stopifnot(is.character(x))
    na = x %in% na.strings
    x[na] = 0
    x = as.numeric(x)
    x[na] = NA_real_
    x
  }
    
  onemat<-function(i){
    lin.local<-gsub('^[[:blank:]]*','',lin[(idx1[i]+4):idx2[i]])
    nr<-idx2[i]-idx1[i]-3
    ret<-matrix(as.num(unlist((strsplit(lin.local,'[[:space:]]+')))),nrow=nr, byrow=TRUE)[,,drop=FALSE]   #[,1:(2+ages[i,2]-ages[i,1]),drop=FALSE]
    if(nrow(ret)!=(years[i,2]-years[i,1]+1)){
      stop(paste("In file",filen, ": Year range specified does not match number of rows for survey fleet number",i))
    } 
    if((ncol(ret)-1)<(ages[i,2]-ages[i,1]+1)){
      stop(paste("In file",filen, ": Fewer columns than indicated by age range for survey fleet number",i))
    } 
    if(!is.numeric(ret)){
      stop(paste("In file",filen, ": Non numeric data values detected for survey fleet number",i))
    }
    ret<-as.matrix(ret[,-1]/ret[,1])
    rownames(ret)<-years[i,1]:years[i,2]
    ret<-ret[,1:length(ages[i,1]:ages[i,2]),drop=FALSE]
    colnames(ret)<-ages[i,1]:ages[i,2]
    attr(ret,'time')<-times[i,]
    ret[ret<0]<-NA
    ret
  }
  obs<-lapply(1:length(names),onemat)  
  names(obs)<-names
  obs
}

read.ices<-function(filen){
  if(grepl("^[0-9]", scan(filen, skip=2, n=1, quiet=TRUE, what=""))){ # is not a survey file 
    
    head<-scan(filen, skip=2, n=5, quiet=TRUE)
    minY<-head[1]
    maxY<-head[2]
    minA<-head[3]
    maxA<-head[4]
    datatype<-head[5]
  
    if(!is.whole.positive.number(minY)){
      stop(paste("In file",filen, ": Minimum year is expected to be a positive integer number"))
    }
    if(!is.whole.positive.number(maxY)){
      stop(paste("In file",filen, ": Maximum year is expected to be a positive integer number"))
    }
    if(!is.whole.positive.number(minA)){
      stop(paste("In file",filen, ": Minimum age is expected to be a positive integer number"))
    }
    if(!is.whole.positive.number(maxA)){
      stop(paste("In file",filen, ": Maximum age is expected to be a positive integer number"))
    }
  
    if(!(datatype%in%c(1,2,3,5))){
      stop(paste("In file",filen, ": Datatype code is expected to be one of the numbers 1, 2, 3, or 5"))
    }
  
    if(minY>maxY){
      stop(paste("In file",filen, ": Minimum year is expected to be less than maximum year"))
    }
    if(minA>maxA){
      stop(paste("In file",filen, ": Minimum age is expected to be less than maximum age"))
    }
  
    C<-as.matrix(read.table.nowarn(filen, skip=5, header=FALSE))
  
    if(datatype==1){
      if((maxY-minY+1)!=nrow(C)){
        stop(paste("In file",filen, ": Number of rows does not match the year range given"))
      }
      if((maxA-minA+1)>ncol(C)){
        stop(paste("In file",filen, ": Fewer columns than the age range given"))
      }
    }
  
    if(datatype==2){
      C<-as.matrix(read.table.nowarn(filen, skip=5, header=FALSE))
      if(1!=nrow(C)){
        stop(paste("In file",filen, ": For datatype 2 only one row of data is expected"))
      }
      if((maxA-minA+1)>ncol(C)){
        stop(paste("In file",filen, ": Fewer columns than the age range given"))
      }
      C<-C[rep(1,maxY-minY+1),]
    }
  
    if(datatype==3){
      C<-as.matrix(read.table.nowarn(filen, skip=5, header=FALSE))
      if(1!=nrow(C)){
        stop(paste("In file",filen, ": For datatype 3 only one row of data is expected"))
      }
      if(1!=ncol(C)){
        stop(paste("In file",filen, ": For datatype 3 only one column of data is expected"))
      }
      C<-C[rep(1,maxY-minY+1),rep(1,maxA-minA+1)]
    }
  
    if(datatype==5){
      C<-as.matrix(read.table.nowarn(filen, skip=5, header=FALSE))
      if((maxY-minY+1)!=nrow(C)){
        stop(paste("In file",filen, ": Number of rows does not match the year range given"))
      }
      if(1!=ncol(C)){
        stop(paste("In file",filen, ": For datatype 5 only one column of data is expected"))
      }
      C<-C[,rep(1,maxA-minA+1)]
    }
    rownames(C)<-minY:maxY
    C<-C[,1:length(minA:maxA)]
    colnames(C)<-minA:maxA
  
    if(!is.numeric(C)){
        stop(paste("In file",filen, ": Non numeric data values detected (could for instance be comma used as decimal operator)"))
    }
    return(C)
  }else{
    return(read.surveys(filen))  
  }
}



test.filenames<-function(path, filelist=c('cn.dat','survey.dat',
                                          'nm.dat','mo.dat',
                                          'sw.dat','cw.dat','lw.dat','dw.dat',
                                          'lf.dat','pf.dat','pm.dat')){
  # Function to test if specified filenames exist  
  owd<-getwd()
  setwd(path)
  ret<-file.exists(filelist)
  setwd(owd)
  return(data.frame(file=filelist,exists=ret))  
}

check.one<-function(filen){
  dummy<-read.ices(filen)
}

check.all<-function(path,filen=c('cn.dat','cw.dat','lw.dat','dw.dat','lf.dat',
                                 'survey.dat','nm.dat','mo.dat',
                                 'sw.dat', 'pf.dat','pm.dat')){
  
  owd<-getwd()
  setwd(path)
  out<-test.filenames('.',filen)
  
  ret<-list()
  self<-rep(NA,length(filen))
  message<-rep("",length(filen))
  wrap<-function(expr){
    tryCatch(expr,error=function(e){E<<-e})
  }
  for(i in 1:length(filen)){
    if(out[i,2]){
      fil<-filen[i]
      E<-NULL
      dummy<-wrap(read.ices(fil))
      if(is.null(E)){
        self[i]<-TRUE
      }else{
        self[i]<-FALSE
        message[i]<-E$message
      }
    }
  }

  out$self<-self

  cross<-rep(NA,length(filen))
  getAgeRange<-function(x)range(as.numeric(colnames(x)))
  getYearRange<-function(x)range(as.numeric(rownames(x)))
  ageRangeOK<-function(x,compare){
    message<-"" 
    ran<-getAgeRange(x)
    ret<-NA
    if(identical(ran,compare)){
      ret<-TRUE
    }else{
      ret<-FALSE
      message<-"Age range mismatch"
    }
    return(list(ret,message))
  }
  yearRangeOK<-function(x,compare){
    message<-"" 
    ran<-getYearRange(x)
    ret<-NA
    if(identical(ran,compare)){
      ret<-TRUE
    }else{
      ret<-FALSE
      message<-"year range mismatch"
    }
    return(list(ret,message))
  }

  check.local<-function(fil,ar,yr,ageAlso=c('cn.dat'),yearAlso=c('cn.dat')){
    test<-read.ices(fil)
    retA<-ageRangeOK(test,ar)
    retY<-yearRangeOK(test,yr)
    idx<-which(out[,1]==fil)
    if(retA[[1]]&&retY[[1]]){
      cross[idx]<<-TRUE
    }else{
      cross[idx]<<-FALSE
      message[idx]<<-paste("In file",fil,":",retA[[2]],retY[[2]])
      if(!retA[[1]])cross[out[,1]%in%ageAlso]<<-FALSE
      if(!retY[[1]])cross[out[,1]%in%yearAlso]<<-FALSE
    }    
  }

  if(!any(is.na(self))&all(self)){
    cross[out[,1]=='cn.dat']<-TRUE
    cross[out[,1]=='survey.dat']<-TRUE

    catch.no<-read.ices('cn.dat')
    ageRangeCatch<-getAgeRange(catch.no)
    yearRangeCatch<-getYearRange(catch.no)
    surveys<- read.ices('survey.dat')
    ageRangeSur<-do.call(range,lapply(surveys,getAgeRange))
    yearRangeSur<-do.call(range,lapply(surveys,getYearRange))
    ageRangeTotal<-range(c(ageRangeCatch,ageRangeSur))
    yearRangeTotal<-range(c(yearRangeCatch,yearRangeSur))
    check.local('cw.dat',ageRangeTotal,yearRangeCatch,ageAlso=c('survey.dat','cn.dat'),yearAlso=c('cn.dat'))
    check.local('dw.dat',ageRangeTotal,yearRangeCatch,ageAlso=c('survey.dat','cn.dat'),yearAlso=c('cn.dat'))
    check.local('lw.dat',ageRangeTotal,yearRangeCatch,ageAlso=c('survey.dat','cn.dat'),yearAlso=c('cn.dat'))
    check.local('lf.dat',ageRangeTotal,yearRangeCatch,ageAlso=c('survey.dat','cn.dat'),yearAlso=c('cn.dat'))
    check.local('pf.dat',ageRangeTotal,yearRangeTotal,ageAlso=c('survey.dat','cn.dat'),yearAlso=c('survey.dat','cn.dat'))
    check.local('pm.dat',ageRangeTotal,yearRangeTotal,ageAlso=c('survey.dat','cn.dat'),yearAlso=c('survey.dat','cn.dat'))
    check.local('sw.dat',ageRangeTotal,yearRangeTotal,ageAlso=c('survey.dat','cn.dat'),yearAlso=c('survey.dat','cn.dat'))
    check.local('mo.dat',ageRangeTotal,yearRangeTotal,ageAlso=c('survey.dat','cn.dat'),yearAlso=c('survey.dat','cn.dat'))
    check.local('nm.dat',ageRangeTotal,yearRangeTotal,ageAlso=c('survey.dat','cn.dat'),yearAlso=c('survey.dat','cn.dat'))
  }

  out$cross<-cross
  out$message<-message

  setwd(owd)
  return(out)
}


gen.default<-function(x,outfile){
  if(!file.exists('cn.dat')){
    stop("File cn.dat does not exsist")
  }
  if(!file.exists('survey.dat')){
    stop("File survey.dat does not exsist")
  }
  s<-read.surveys('survey.dat')
  cn<-read.ices('cn.dat')
  # calculate overall age range 
  ager<-range(as.vector(unlist(lapply(s, function(m)as.numeric(colnames(m))))),as.numeric(colnames(cn)))
  # calculate overall year range 
  yearr<-range(as.vector(unlist(lapply(s, function(m)as.numeric(rownames(m))))),as.numeric(rownames(cn)))
  mat<-matrix(x,nrow=yearr[2]-yearr[1]+1,ncol=ager[2]-ager[1]+1)
  cat("Auto generated default file\n1 1\n",file=outfile)
  cat(yearr,"\n",file=outfile, append=TRUE)
  cat(ager,"\n",file=outfile, append=TRUE)
  cat(1,"\n",file=outfile, append=TRUE)
  write.table(mat,file=outfile, append=TRUE, row.names=FALSE, col.names=FALSE, quote=FALSE)
}



gen.default.catchdim<-function(x,outfile){
  if(!file.exists('cn.dat')){
    stop("File cn.dat does not exsist")
  }
  cn<-read.ices('cn.dat')
  # calculate overall age range 
  ager<-range(as.numeric(colnames(cn)))
  # calculate overall year range 
  yearr<-range(as.numeric(rownames(cn)))
  mat<-matrix(x,nrow=yearr[2]-yearr[1]+1,ncol=ager[2]-ager[1]+1)
  cat("Auto generated default file\n1 1\n",file=outfile)
  cat(yearr,"\n",file=outfile, append=TRUE)
  cat(ager,"\n",file=outfile, append=TRUE)
  cat(1,"\n",file=outfile, append=TRUE)
  write.table(mat,file=outfile, append=TRUE, row.names=FALSE, col.names=FALSE, quote=FALSE)
}
