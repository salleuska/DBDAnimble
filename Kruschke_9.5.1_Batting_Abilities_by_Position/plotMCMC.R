## plotting function from
######################################################################
# Jags-Ybinom-XnomSsubjCcat-MbinomBetaOmegaKappa.R 
# Accompanies the book:
#   Kruschke, J. K. (2014). Doing Bayesian Data Analysis: 
#   A Tutorial with R, JAGS, and Stan. 2nd Edition. Academic Press / Elsevier.
######################################################################

#===============================================================================

plotMCMC = function( codaSamples , 
                     data , zName="z" , NName="N" , sName="s" , cName="c" ,
                     compVal=0.5 , rope=NULL , 
                     diffSList=NULL , diffCList=NULL , 
                     compValDiff=0.0 , ropeDiff=NULL , 
                     saveName=NULL , saveType="jpg" ) {
  #-----------------------------------------------------------------------------
  # N.B.: This function expects the data to be a data frame, 
  # with one component z being a vector of integer # successes,
  # one component N being a vector of integer # attempts,
  # one component s being a factor of subject identifiers,
  # and one component c being a factor of category identifiers, with
  # subjects nested in categories.
  z = data[[zName]]
  N = data[[NName]]
  s = data[[sName]]
  c = data[[cName]]
  Nsubj = length(unique(s))
  Ncat =  length(unique(c))
  # Now plot the posterior:
  mcmcMat = as.matrix(codaSamples,chains=TRUE)
  chainLength = NROW( mcmcMat )
  # kappa:
  parNames = sort(grep("kappa",colnames(mcmcMat),value=TRUE))
  nPanels = length(parNames)
  nCols = 4
  nRows = ceiling(nPanels/nCols)
  openGraph(width=2.5*nCols,height=2.0*nRows)
  par( mfcol=c(nRows,nCols) )
  par( mar=c(3.5,1,3.5,1) , mgp=c(2.0,0.7,0) )
  #xLim = range( mcmcMat[,parNames] )
  xLim=quantile(mcmcMat[,parNames],probs=c(0.000,0.995))
  
  # mainLab = c(levels(data[[cName]]),"Overall")
  mainLab = mainLab
  
  mainIdx = 0
  for ( parName in parNames ) {
    mainIdx = mainIdx+1
    postInfo = plotPost( mcmcMat[,parName] , compVal=compVal , ROPE=rope ,
                         xlab=bquote(.(parName)) , cex.lab=1.25 , 
                         main=mainLab[mainIdx] , cex.main=1.5 ,
                         xlim=xLim , border="skyblue" )
  }  
  if ( !is.null(saveName) ) {
    saveGraph( file=paste(saveName,"Kappa",sep=""), type=saveType)
  }
  
  # omega:
  parNames = sort(grep("omega",colnames(mcmcMat),value=TRUE))
  nPanels = length(parNames)
  nCols = 4
  nRows = ceiling(nPanels/nCols)
  openGraph(width=2.5*nCols,height=2.0*nRows)
  par( mfcol=c(nRows,nCols) )
  par( mar=c(3.5,1,3.5,1) , mgp=c(2.0,0.7,0) )
  #xLim = range( mcmcMat[,parNames] )
  xLim=quantile(mcmcMat[,parNames],probs=c(0.001,0.999))
  
  # mainLab = c(levels(data[[cName]]),"Overall")
  mainLab = mainLab
  
  mainIdx = 0
  for ( parName in parNames ) {
    mainIdx = mainIdx+1
    postInfo = plotPost( mcmcMat[,parName] , compVal=compVal , ROPE=rope ,
                         xlab=bquote(.(parName)) , cex.lab=1.25 , 
                         main=mainLab[mainIdx] , cex.main=1.5 ,
                         xlim=xLim , border="skyblue" )
  }  
  if ( !is.null(saveName) ) {
    saveGraph( file=paste(saveName,"Omega",sep=""), type=saveType)
  }
  
  # Plot individual omega's and differences:
  if ( !is.null(diffCList) ) {
    for ( compIdx in 1:length(diffCList) ) {
      diffCVec <- diffCList[[compIdx]] 
      Nidx = length(diffCVec)
      temp <- NULL
      for ( i in 1:Nidx ) {
        temp <- c( temp , which(levels(data[[cName]])==diffCVec[i]) )
      }
      diffCVec = temp
      openGraph(width=2.5*Nidx,height=2.0*Nidx)
      par( mfrow=c(Nidx,Nidx) )
      xLim = range(c( compVal, rope,
                      mcmcMat[,paste0("omega[",diffCVec,"]")] ))
      for ( t1Idx in 1:Nidx ) {
        for ( t2Idx in 1:Nidx ) {
          parName1 = paste0("omega[",diffCVec[t1Idx],"]")
          parName2 = paste0("omega[",diffCVec[t2Idx],"]")
          if ( t1Idx > t2Idx) {  
            # plot.new() # empty plot, advance to next
            par( mar=c(3,3,3,1) , mgp=c(2.0,0.7,0) , pty="s" )
            nToPlot = 700
            ptIdx = round(seq(1,chainLength,length=nToPlot))
            plot ( mcmcMat[ptIdx,parName2] , mcmcMat[ptIdx,parName1] , 
                   cex.main=1.25 , cex.lab=1.25 , 
                   
                   xlab=tblAsc$PriPos[diffCVec[t2Idx]] ,
                   ylab=tblAsc$PriPos[diffCVec[t1Idx]] ,
                   # xlab=levels(data[[cName]])[diffCVec[t2Idx]] , 
                   # ylab=levels(data[[cName]])[diffCVec[t1Idx]] , 
                   
                   col="skyblue" )
            abline(0,1,lty="dotted")
          } else if ( t1Idx == t2Idx ) {
            par( mar=c(3,1.5,3,1.5) , mgp=c(2.0,0.7,0) , pty="m" )
            postInfo = plotPost( mcmcMat[,parName1] , 
                                 compVal=compVal , ROPE=rope , 
                                 cex.main=1.25 , cex.lab=1.25 , 
                                 xlab=bquote(.(parName1)) ,
                                 main=tblAsc$PriPos[diffCVec[t1Idx]] ,  
                                 xlim=xLim )
          } else if ( t1Idx < t2Idx ) {
            par( mar=c(3,1.5,3,1.5) , mgp=c(2.0,0.7,0) , pty="m" )
            postInfo = plotPost( mcmcMat[,parName1]-mcmcMat[,parName2] , 
                                 compVal=compValDiff , ROPE=ropeDiff , 
                                 cex.main=1.25 , cex.lab=1.25 , 
                                 xlab=bquote("Difference of "*omega*"'s") , 
                                 main=paste( 
                                   tblAsc$PriPos[diffCVec[t1Idx]] ,
                                   "-",
                                   tblAsc$PriPos[diffCVec[t2Idx]] ) )
          }
        }
      }
      if ( !is.null(saveName) ) {
        saveGraph( file=paste0(saveName,"OmegaDiff",compIdx), type=saveType)
      }
    }
  }
  
  # Plot individual theta's and differences:
  if ( !is.null(diffSList) ) {
    for ( compIdx in 1:length(diffSList) ) {
      diffSVec = diffSList[[compIdx]]
      Nidx = length(diffSVec)
      temp=NULL
      for ( i in 1:Nidx ) {
        temp = c( temp , which(data[[sName]]==diffSVec[i]) )
      }
      diffSVec = temp
      openGraph(width=2.5*Nidx,height=2.0*Nidx)
      par( mfrow=c(Nidx,Nidx) )
      xLim = range(c(compVal,rope,mcmcMat[,paste0("theta[",diffSVec,"]")],
                     z[diffSVec]/N[diffSVec]))
      for ( t1Idx in 1:Nidx ) {
        for ( t2Idx in 1:Nidx ) {
          parName1 = paste0("theta[",diffSVec[t1Idx],"]")
          parName2 = paste0("theta[",diffSVec[t2Idx],"]")
          if ( t1Idx > t2Idx) {  
            # plot.new() # empty plot, advance to next
            par( mar=c(3,3,3,1) , mgp=c(2.0,0.7,0) , pty="s" )
            nToPlot = 700
            ptIdx = round(seq(1,chainLength,length=nToPlot))
            plot ( mcmcMat[ptIdx,parName2] , mcmcMat[ptIdx,parName1] , cex.lab=1.25 ,
                   xlab=s[diffSVec[t2Idx]] , 
                   ylab=s[diffSVec[t1Idx]] , 
                   col="skyblue" )
            abline(0,1,lty="dotted")
          } else if ( t1Idx == t2Idx ) {
            par( mar=c(3,1.5,3,1.5) , mgp=c(2.0,0.7,0) , pty="m" )
            postInfo = plotPost( mcmcMat[,parName1] , 
                                 compVal=compVal , ROPE=rope , 
                                 cex.main=1.25 , cex.lab=1.25 , 
                                 xlab=bquote(.(parName1)) ,
                                 main=paste0( s[diffSVec[t1Idx]] , 
                                              
                                              " (",data[['PriPos']][diffSVec[t1Idx]],")") ,  
                                              # " (",c[diffSVec[t1Idx]],")") ,  
                                 
                                 xlim=xLim )
            points( z[diffSVec[t1Idx]]/N[diffSVec[t1Idx]] , 0 , 
                    pch="+" , col="red" , cex=3 )
            text( z[diffSVec[t1Idx]]/N[diffSVec[t1Idx]] , 0 , 
                  bquote(list( z==.(z[diffSVec[t1Idx]]) ,
                               N==.(N[diffSVec[t1Idx]]) )) , 
                  adj=c( (z[diffSVec[t1Idx]]/N[diffSVec[t1Idx]]-xLim[1])/
                           (xLim[2]-xLim[1]),-3.25) , col="red" )
          } else if ( t1Idx < t2Idx ) {
            par( mar=c(3,1.5,3,1.5) , mgp=c(2.0,0.7,0) , pty="m" )
            postInfo = plotPost( mcmcMat[,parName1]-mcmcMat[,parName2] , 
                                 compVal=compValDiff , ROPE=ropeDiff , 
                                 cex.main=0.67 , cex.lab=1.25 , 
                                 xlab=bquote("Difference of "*theta*"'s") , 
                                 main=paste( 
                                   s[diffSVec[t1Idx]] ,
                                   
                                   " (",data[['PriPos']][diffSVec[t1Idx]],")" ,
                                   # " (",c[diffSVec[t1Idx]],")" ,
                                   
                                   "\n -",
                                   s[diffSVec[t2Idx]] , 
                                   
                                   " (",data[['PriPos']][diffSVec[t2Idx]],")" ) )
                                   # " (",c[diffSVec[t2Idx]],")" ) )
            
            points( z[diffSVec[t1Idx]]/N[diffSVec[t1Idx]]
                    - z[diffSVec[t2Idx]]/N[diffSVec[t2Idx]] , 0 , 
                    pch="+" , col="red" , cex=3 )
          }
        }
      }
      if ( !is.null(saveName) ) {
        saveGraph( file=paste0(saveName,"ThetaDiff",compIdx), type=saveType)
      }
    }
  }
}

#===============================================================================
