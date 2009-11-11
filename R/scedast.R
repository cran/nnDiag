## Created by Brian Walters; bfwalters83@yahoo.com
## Creation date: June 29, 2009
## Modified Date: October 28, 2009

###########################################
## Function to do a test of scedasticity ##
###########################################

## Package Dependencies:

scedast <- function(object){

  ## Checks ##
  if(class(object) != "nnDgrps"){stop("Object must be of class nnDgrps")}
  ##========##

  pred <- object$predicted.groups
  res <- object$residual.groups

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


#################################
## Default Generic Plot Method ##
#################################

plot.nnDsced <- function(x, ...){
  
  ## Checks ##
  elip.args <- names(list(...))
  if(length(elip.args) != 0){
    for(i in 1:length(elip.args)){
      if(elip.args[i] %in% names(formals(plot.default)) == TRUE){next}
      else if(elip.args[i] %in% names(par()) == TRUE){next}
      else{stop("'",elip.args[i], "' is not a graphical parameter")}
    }
  }
  ##========##
  xy <- list(x = x$mean.prediction, y = x$stdev.residuals)
  el <- list(...)
  argu <- c(xy, el)
  
  if(! "xlab" %in% names(argu)){argu <- append(argu, list(xlab = "Mean Prediction"))} #Default x axis label
  if(! "ylab" %in% names(argu)){argu <- append(argu, list(ylab = "Standard Deviation of Residuals"))} #Default y axis label
  if(! "xlim" %in% names(argu)){argu <- append(argu, list(xlim = c(loLim(min(xy$x)), hiLim(max(xy$x)))))} #Default x axis limits
  if(! "ylim" %in% names(argu)){argu <- append(argu, list(ylim = c(loLim(min(xy$y)), hiLim(max(xy$y)))))} #Default y axis limits
  if(! "main" %in% names(argu)){argu <- append(argu, list(main = "Scedasticity Diagnostic Test"))} #Default main graph title
  
  do.call("plot", argu)
}
