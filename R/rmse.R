## Created by Brian Walters; bfwalters83@yahoo.com
## Creation date: September 11, 2009
## Modified date: October 20, 2009

#########################################################
## Function to calculate root mean square error (RMSE) ##
#########################################################

## Package Dependencies:

rmse <- function(reference.set, predicted.set){

  ## Checks ##
  if(missing(reference.set)){stop("reference.set must be specified")}
  if(missing(predicted.set)){stop("predicted.set must be specified")}
  if(is.vector(reference.set)!=TRUE){stop("The reference set must be a vector.")}
  ##========##

  ## FOR ONLY ONE PREDICTED SET ##
  if(is.vector(predicted.set)==TRUE){

    ## Checks ##
    if(length(reference.set) != length(predicted.set)){stop("reference.set and predicted.set are not the same length")}
    ##========##

    y <- reference.set
    yHat <- predicted.set
    x <- sqrt(sum((y - yHat)^2)/length(y))

    return(x)
    
  }else{## FOR MULTIPLE PREDICTED SETS ##

    ## Checks ##
    if(is.matrix(predicted.set)!=TRUE){stop("The predicted.set must be either a vector (for one set) or a matrix (for multiple sets).")}
    if(length(reference.set) != nrow(predicted.set)){stop("reference.set and predicted.set are not the same length")}
    ##========##

    y <- reference.set
    yHat <- predicted.set

    x <- rep(NA, ncol(yHat))

    for(i in 1:length(x)){
      x[i] <- sqrt(sum((y - yHat[,i])^2)/length(y))
    }

    return(x)
  }
}
