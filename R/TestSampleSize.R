#########################
# Testing effective sample size computation for bootstrapping
#########################

install.packages("dirmult")
library(dirmult)
# Norm = function(Vec) Vec / sum(Vec)
Norm = function(Vec) Vec / sum(Vec)
# Rmultinom = function(n, size, prob){
Rmultinom = function(n, size, prob){
  Return = NULL
  if(length(size)==1 & is.vector(prob)) Return = rmultinom(n=n, size=size, prob=prob)
  if(length(size)>=2 & is.vector(prob)) for(i in 1:n) Return = cbind(Return, rmultinom(n=1, size=size[i], prob=prob))
  if(length(size)==1 & is.matrix(prob)) for(i in 1:n) Return = cbind(Return, rmultinom(n=1, size=size, prob=prob[,i]))
  if(length(size)>=2 & is.matrix(prob)) for(i in 1:n) Return = cbind(Return, rmultinom(n=1, size=size[i], prob=prob[,i]))
  return(Return)
}
# Sample function
SampFn = function(Vec){
  Long = rep.int(1:length(Vec), times=Vec)
  Samp = sample(Long, replace=TRUE)
  New = tabulate(Samp, nbins=length(Vec))
  return(New)
}

Prop = rdirichlet(1, alpha=rep(1,10))
#Prop = Norm(matrix(N_atsm[,Nyears,StrataI,Mode_i][,1],nrow=1))

#####################
# Check effective sample size theory
#####################
# effective sample size of each raw data row
Size = 100
N = 1e2
Rand = t( Rmultinom(N, size=Size, prob=Prop[1,]) )
#Rand = CompTrue
Rand = Rand / Size
Neff = apply(Rand, MARGIN=1, FUN=function(Boot){sum( Boot * (1-Boot)) / sum( (Boot-Prop)^2 )})
mean(1/Neff)
1 / mean(1/Neff)    # Should be equal to Size

#####################
# Check boostrap for:
# 1. rows (tows) and samples within rows (individuals)          -- BIASED (double counts variance)
# 2. Aggregating across tows, and then samples within aggregate -- FINE
# 3. Just rows (tows)                                           -- FINE (MY RECOMMENDATION)
# 4. Just samples within rows (individuals)                     -- FINE
#####################
# effective sample size for mean across rows
Size = 4
N = 100
Nsims = 100
Save = matrix(NA, nrow=Nsims, ncol=8)
for(SimI in 1:nrow(Save)){
  # two-stage bootstrap
  Prop = rdirichlet(1, alpha=rep(1,10))
  Rand = t( Rmultinom(N, size=Size, prob=Prop[1,]) )
  aOrig = Norm(colSums(Rand))
  Neff = aBoot = NULL
  for(BootI in 1:Nsims){
    WhichRow = sample(1:nrow(Rand), replace=TRUE)
    CompBoot = Rand[WhichRow,]
    CompBoot = t(apply(CompBoot, MARGIN=1, FUN=SampFn))
    aBoot = rbind(aBoot, Norm(colSums(CompBoot)) )
    Neff = c(Neff, sum( aBoot[BootI,] * (1-aBoot[BootI,])) / sum( (aBoot[BootI,]-aOrig)^2 ) )
  }
  Save[SimI,1] = mean(Neff)
  Save[SimI,2] = 1 / mean( 1/Neff )
  
  # one-stage "aggregated" bootstrap
  Neff = aBoot = NULL
  for(BootI in 1:Nsims){
    CompBoot = SampFn(colSums(Rand))
    aBoot = rbind(aBoot, Norm(CompBoot) )
    Neff = c(Neff, sum( aBoot[BootI,] * (1-aBoot[BootI,])) / sum( (aBoot[BootI,]-aOrig)^2 ) )
  }
  Save[SimI,3] = mean(Neff)
  Save[SimI,4] = 1 / mean( 1/Neff )
  
  # one-stage "data-level" bootstrap
  Prop = rdirichlet(1, alpha=rep(1,10))
  Rand = t( Rmultinom(N, size=Size, prob=Prop[1,]) )
  aOrig = Norm(colSums(Rand))
  Neff = aBoot = NULL
  for(BootI in 1:Nsims){
    CompBoot = t(apply(Rand, MARGIN=1, FUN=SampFn))
    aBoot = rbind(aBoot, Norm(colSums(CompBoot)) )
    Neff = c(Neff, sum( aBoot[BootI,] * (1-aBoot[BootI,])) / sum( (aBoot[BootI,]-aOrig)^2 ) )
  }
  Save[SimI,5] = mean(Neff)
  Save[SimI,6] = 1 / mean( 1/Neff )
  
  # one-stage "tow-level" bootstrap
  Prop = rdirichlet(1, alpha=rep(1,10))
  Rand = t( Rmultinom(N, size=Size, prob=Prop[1,]) )
  aOrig = Norm(colSums(Rand))
  Neff = aBoot = NULL
  for(BootI in 1:Nsims){
    WhichRow = sample(1:nrow(Rand), replace=TRUE)
    CompBoot = Rand[WhichRow,]
    aBoot = rbind(aBoot, Norm(colSums(CompBoot)) )
    Neff = c(Neff, sum( aBoot[BootI,] * (1-aBoot[BootI,])) / sum( (aBoot[BootI,]-aOrig)^2 ) )
  }
  Save[SimI,7] = mean(Neff)
  Save[SimI,8] = 1 / mean( 1/Neff )
}
colMeans(Save)
