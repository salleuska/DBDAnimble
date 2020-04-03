
# This is an example from Kruschke(2015)[^1], section 7.4. The model is implemented in JAGS in section 8.4. 

data <- read.csv('z6N8z2N7.csv')

################
library(nimble)

## modelCode
coinCode <- nimbleCode({
  for (i in 1:Nsubj){
    theta[i] ~ dbeta(2, 2)    # prior distributions the two parameters
  }
	for (i in 1:Ntotal){
		y[i] ~ dbern(prob = theta[s[i]])    # likelihoods of the outcomes based on the subjects 
	}
})

## define additional information needed for the model
## data 
coinData <- list(y = data$y)

## constants of the model (e.g. number of observations, fixed values)
coinConsts <- list(s = as.numeric(data$s), Ntotal = nrow(data), Nsubj = length(unique(data$s)))

## values to initialize the algorithm
coinInits <- list(theta = c(0.5, 0.5))

 

## fast use of NIMBLE (one-line invocation)
mcmc.out <- nimbleMCMC(code = coinCode, constants = coinConsts,
						data = coinData, inits = coinInits,
						niter = 5000, nburnin = 1000,
						monitors = c('theta'))


#########################################################
## If you want to use the plot functions from the book
source("DBDA2E-utilities.R")
library(coda)

## Convert posterior samples to mcmc object (coda package)
mcmcCoda <- as.mcmc(mcmc.out)
plotPost(mcmcCoda[, c('theta[1]')], xlab = c("theta1"))


## this won't properly and will show only the traceplot since for other plots you need more than one chain
## I don't think it is strictly necessary but if you want to use it you can just use more chains
diagMCMC( codaObject=mcmcCoda , parName="theta[1]" ) 

## if you want to run multiple chains, just specify nchains
mcmcCoda <- nimbleMCMC(code = coinCode, constants = coinConsts,
						data = coinData, inits = coinInits,
						niter = 500, nburnin = 100, nchains = 4,
						monitors = c('theta'),
						samplesAsCodaMCMC = TRUE)   ## this will output samples as a mcmc object

plotPost(mcmcCoda[, c('theta[1]')], xlab = c("theta1"))
plotPost(mcmcCoda[, c('theta[2]')], xlab = c("theta2"))

diagMCMC( codaObject=mcmcCoda , parName= "theta[1]" ) 
diagMCMC( codaObject=mcmcCoda , parName= "theta[2]" ) 


## Plots

# par(mfrow=c(2,1))
# hist(mcmc.out[, 1], breaks = 100, xlim = c(0, 1.2), prob = T, main = "Histogram of theta1 posterior samples")
# hist(mcmc.out[, 2], breaks = 100, xlim = c(0, 1.2), prob = T, main = "Histogram of theta2 posterior samples")



# plot(mcmc.out, type = "p", main = "Traceplot")



# plot(mcmc.out[,1], type = "l", main = "Traceplot of theta1")
# lines(cumsum(mcmc.out[,1])/1:length(mcmc.out[,1]), col = "gold")



# plot(mcmc.out[,2], type = "l", main = "Traceplot of theta2")
# lines(cumsum(mcmc.out[,2])/1:length(mcmc.out[,2]), col = "gold")


# Marginal posterior distributions of $\theta_1$ and $\theta_2$


# hist(mcmc.out[,'theta[1]'], main = "Marginal posterior of theta1", breaks = 100)
# hist(mcmc.out[,'theta[2]'], main = "Marginal posterior of theta2", breaks = 100)
