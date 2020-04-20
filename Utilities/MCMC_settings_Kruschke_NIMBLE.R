jagsToNimbleMCMC <- function(numSavedSteps, burnInSteps = 500, thinSteps = 1, nChains = 4){
  ## nIteration in jags
  
  nIteration <- ceiling(numSavedSteps*thinSteps/nChains)
  
  nimbleSettings <- list()
  nimbleSettings$nchains <- nChains
  nimbleSettings$burnin <- burnInSteps       # burn in is for each chain 
  
  nimbleSettings$nthin <- thinSteps
    
  nimbleSettings$niter <- nIteration + nimbleSettings$burnin
  
  return(nimbleSettings)
}

## Batting average example  
jagsToNimbleMCMC(numSavedSteps = 11000, burnInSteps = 500, thinSteps = 20, nChains = 4)
##---------------------------------------------------##
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

  