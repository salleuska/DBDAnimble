# Modeling Baseball Batting Abilities by Position

# This is an example from Kruschke(2015)[^1], section 9.5.1. A generic description of the model and the model's specification in JAGS can be found in section 9.5.

data <- read.csv('BattingAverage.csv')

################
library(nimble)
library(dplyr)
source("DBDA2E-utilities.R")
source("plotMCMC.R")


## modelCode
batCode <- nimbleCode({
omegaO ~ dbeta(1, 1)
kappaMinusTwoO ~ dgamma(0.01, 0.01)
kappaO <- kappaMinusTwoO + 2 
for (c in 1:nCat) {
omega[c] ~ dbeta(omegaO*(kappaO-2)+1, (1-omegaO)*(kappaO-2)+1 )
kappaMinusTwo[c] ~ dgamma(0.01, 0.01)
kappa[c] <- kappaMinusTwo[c] + 2
}
for (s in 1:nSubj) {
theta[s] ~ dbeta(omega[cat[s]]*(kappa[cat[s]]-2)+1,
(1-omega[cat[s]])*(kappa[cat[s]]-2)+1)
z[s] ~ dbinom(size = n[s], prob = theta[s])
}
})


## define additional information needed for the model
## prepare data
z <- data$Hits
n <- data$AtBats

## prepare constants
nCat <- length(unique(data$PriPos))
nSubj <- length(unique(data$Player))

## prepare initial values
thetaInits <- rep(NA, nSubj)
for (s in 1:nSubj) {
  resampledZ <- rbinom(n = 1, size = n[s], prob = z[s]/n[s]) 
  thetaInits[s] <- resampledZ/n[s]
}
thetaInits <- 0.001 + 0.998 * thetaInits
omegaInits <- aggregate(thetaInits, by=list(data$PriPosNumber), FUN=mean)$x 
omegaInitsO <- mean(thetaInits)
kappaInits <- rep(100, nCat)
kappaInitO <- 100

## data
batData <- list(z = z, n = n)

## constants of the model 
batConsts <- list(subjects = data$Player, cat = data$PriPosNumber, nCat = nCat, nSubj = nSubj)

## values to initialize the algorithm
batInits <- list(theta = thetaInits, omega = omegaInits, omegaO = omegaInitsO,
                 kappaMinusTwo = kappaInits - 2, kappaMinusTwoO = kappaInitO - 2)


## define model properties
nchains <- 4
nburnin <- 500 
niter <- 13750 + nburnin  
thin <- 20/nchains 

## run model
mcmcCoda <- nimbleMCMC(code = batCode, constants = batConsts,
                       data = batData, inits = batInits,
                       niter = niter, nburnin = nburnin, 
                       thin = thin, nchains = nchains,
                       monitors = c("theta","omega","kappa","omegaO","kappaO"), 
                       samplesAsCodaMCMC = TRUE)


## prepare plotting
tbl <- unique(data[, c('PriPos', 'PriPosNumber')])
tblAsc <- arrange(tbl, PriPosNumber)
tblAsc[['PriPos']] <- as.character(tblAsc[['PriPos']])
mainLab <- c(tblAsc$PriPos, "Overall")
data[['PriPosNumber']] <- as.factor(data[['PriPosNumber']])

## plot
plotMCMC( mcmcCoda , data=data , 
          zName="Hits", NName="AtBats", sName="Player", cName="PriPosNumber",
          compVal=NULL ,
          diffCList=list( c(1, 2) ,    ## 1=Pitcher, 2=Catcher  ## check tbl for more information
                          c(2, 3) ) ,  ## 2=Catcher, 3=1st Base
          diffSList=list( c("Kyle Blanks","Bruce Chen") , 
                          c("Mike Leake","Wandy Rodriguez") , 
                          c("Andrew McCutchen","Brett Jackson") , 
                          c("ShinSoo Choo","Ichiro Suzuki") ) , 
          compValDiff=0.0, 
          saveName="Batting_Average_" , saveType="jpg")

# Kruschke, J.K.,2015. Doing Bayesian data analysis: a tutorial with R, JAGS, and stan. 2E [edition]. Academic Press.