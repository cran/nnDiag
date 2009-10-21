## Created by Brian Walters; bfwalters83@yahoo.com
## Creation date: June 29, 2009
## Modified Date: October 21, 2009

############################################################
## Function to find outliers and influential observations ##
############################################################

outInflu <- function(object){

  ## Checks ##
  if(class(object) != "nnDgrps"){stop("Object must be of class nnDgrps")}
  ##========##

  pred <- object$predicted.groups
  res <- object$residual.groups
  
  ## Take the Standard Deviation of the groups of residuals and the mean of the groups of predictions ##
  mean.predictions <- apply(pred, 2, mean, na.rm=TRUE)
  stdev.residuals <- apply(res, 2, sd, na.rm=TRUE)
  ##==================================================================================================##

  ## Make a linear model of stdev.residuals vs. mean.predictions and get the coefficients ##
  stmod <- lm(stdev.residuals ~ mean.predictions)
  int <- coef(stmod)[1]
  slp <- coef(stmod)[2]
  ##======================================================================================##

  ## Find the standardizer for each group ##
  stdz <- rep(0, length(mean.predictions))

  for(i in 1:length(stdz)){
    stdz[i] <- slp*mean.predictions[i]+int
  }
  ##======================================##

  ## Standardize each residual ##
  stdz.res.grps <- matrix(data = NA, nrow = nrow(res), ncol = ncol(res))

  for(i in 1:length(stdz)){
    stdz.res.grps[,i] <- res[,i]/stdz[i]
  }
  ##===========================##

  ## Get data to make the graph ##
  stdz.res.vect <- as.vector(stdz.res.grps)
  stdz.res.vect <- na.omit(stdz.res.vect)
  nCount <- object$ordered.data[,4]
  ##============================##

  ## Make the graph data into a list ##
  dat <- vector("list", 2)
  names(dat) <- c("neighbor.count", "standardized.residuals")
  dat[[1]] <- nCount
  dat[[2]] <- stdz.res.vect
  ##=================================##

  class(dat) <- "nnDoi"
  return(dat)
}

