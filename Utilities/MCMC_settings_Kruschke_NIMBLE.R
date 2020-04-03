jagsToNimbleMCMC <- function(numSavedSteps, burnInSteps = 500, thinSteps = 1, nChains = 4){
  ## nIteration in jags
  nIteration <- ceiling(numSavedSteps*thinSteps/nChains)
  
  nimbleSettings <- list()
  nimbleSettings$nchains = nChains
  nimbleSettings$burnin <- burnInSteps       # burn in is for each chain 
  
  nimbleSettings$nthin <- thinSteps/nchains
  
  ## niter is for each chain when nchain > 1
  ## ceiling(numSavedSteps*thinSteps/nChains) (from Krutsche) plus NIMBLE comprises the burnin
  
  nimbleSettings$niter <- nIteration/nchains + burnin 
  
  return(nimbleSettings)
}
##---------------------------------------------------##
### Kruschke default

## arguments that can be passed to genMCMC()
## final number of steps 
numSavedSteps=50000 
## thinning
thinSteps=1 
## n. of chains
nChainsDefault = 4 ## this is actually specified in DBDA_utilites

nChains=nChainsDefault 

## internal arguments of genMCMC()

## number of adaptation steps
adaptSteps = 500             # Number of steps to adapt the samplers
burnInSteps = 500            # Number of steps to burn-in the chains

nIteration = ceiling(numSavedSteps*thinSteps/nChains)

#############
## Batting average example 
numSavedSteps=11000
thinSteps=20
nChainsDefault = 4
nChains=nChainsDefault 

adaptSteps = 500             # Number of steps to adapt the samplers
burnInSteps = 500            # Number of steps to burn-in the chains


nIteration = ceiling(numSavedSteps*thinSteps/nChains)
nIteration

## Matching these settings in NIMBLE
## Note: MCMC settings of iterations, burning & thinning are defined for each chain
## let's use for n
nchains = nChains
nchains
burnin <- burnInSteps       # burn in is for each chain 
burnin

nthin <- thinSteps/nchains
nthin

niter <- nIteration/nchains + burnin # niter is for each chain when nchain > 1
## ceiling(numSavedSteps*thinSteps/nChains) (from Krutsche) plus NIMBLE comprises the burnin
niter

###################
## function to convert MCMC settings from Krusche code (double check on this)
## ignore adaptSteps
jagsToNimbleMCMC <- function(numSavedSteps, burnInSteps = 500, thinSteps = 1, nChains = 4){
  ## nIteration in jags
  nIteration <- ceiling(numSavedSteps*thinSteps/nChains)
  
  nimbleSettings <- list()
  nimbleSettings$nchains = nChains
  nimbleSettings$burnin <- burnInSteps       # burn in is for each chain 
  
  nimbleSettings$nthin <- thinSteps/nchains
  
  ## niter is for each chain when nchain > 1
  ## ceiling(numSavedSteps*thinSteps/nChains) (from Krutsche) plus NIMBLE comprises the burnin
  
  nimbleSettings$niter <- nIteration/nchains + burnin 
  
  return(nimbleSettings)
}

## Batting average example  
jagsToNimbleMCMC(numSavedSteps = 11000, burnInSteps = 500, thinSteps = 20, nChains = 4)

###################
## other info - saving objects
## you can save R objects via 
save(nIteration, nchains, file = "example.RData")
## and use 
load("example.RData")
## to load them in a workspace (they will have the same name you used while saving)

## Alternatively, with big objects such as samples from NIMBLE/JAGS you can use a zipped form
## you can save only one object
saveRDS(nIteration, file = "example2.rds")
## and then you read the saved file as
iterationNumber <- readRDS("example2.rds")
## you need to assign the read object to a variables since the name is lost
###################

  