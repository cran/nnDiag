## Created by Brian Walters; bfwalters83@yahoo.com
## Creation date: June 29, 2009
## Modified Date: October 21, 2009

###########################################
## Function to do a test of scedasticity ##
###########################################

## Package Dependencies:

scedast <- function(groups){

  ## Checks ##
  if(class(groups) != "nnDgrps"){stop("Object must be of class nnDgrps")}
  ##========##

  pred <- groups$predicted.groups
  res <- groups$residual.groups

  ## Take the standard deviation of the groups of residuals and the mean of the groups of predictions ## 
  mean.predictions <- apply(pred, 2, mean, na.rm=TRUE)
  stdev.residuals <- apply(res, 2, sd, na.rm=TRUE)
  ##==================================================================================================##

  ## Make the graph data into a list ##
  dat <- vector("list", 2)
  names(dat) <- c("mean.prediction", "stdev.residuals")
  dat[[1]] <- mean.predictions
  dat[[2]] <- stdev.residuals
  ##=================================##

  class(dat) <- "nnDsced"
  
  return(dat)
}
