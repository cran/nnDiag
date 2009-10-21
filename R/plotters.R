## Default plot methods for nnD* objects
## Created by Brian Walters; bfwalters83@yahoo.com
## Creation date: October 21, 2009
## Modified Date: 
## Package Dependencies: gplots

##################
## Scedasticity ##
##################
plot.nnDsced <- function(x, y, ...){
  
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

###########################################
## Outliers and Influential Observations ##
###########################################
plot.nnDoi <- function(x, y, ...){

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
  xy <- list(x = x$neighbor.count, y = x$standardized.residuals)
  el <- list(...)
  argu <- c(xy, el)

  if(! "xlab" %in% names(argu)){argu <- append(argu, list(xlab = "Number of Times Used as Neighbor"))} #Default x axis label
  if(! "ylab" %in% names(argu)){argu <- append(argu, list(ylab = "Standardized Residuals"))} #Default y axis label
  if(! "xlim" %in% names(argu)){argu <- append(argu, list(xlim = c(loLim(min(xy$x)), hiLim(max(xy$x)))))} #Default x axis limits
  if(! "ylim" %in% names(argu)){argu <- append(argu, list(ylim = c(floor(min(xy$y)-1), ceiling(max(xy$y)+1))))} #Default y axis limits
  if(! "main" %in% names(argu)){argu <- append(argu, list(main = "Outliers and Influential Observations"))} #Default main graph title
  
  do.call("plot", argu)
}

#####################
## Bias Assessment ##
#####################
plot.nnDbias <- function(x, y, ...){

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

  m <- attributes(x)

  if(m$mode == "groups"){
    xy <- list(x = x$mean.predictions, y = x$mean.observations)
    el <- list(...)
    argu <- c(xy, el)

    if(! "xlab" %in% names(argu)){argu <- append(argu, list(xlab = "Mean Predictions"))} #Default x axis label
    if(! "ylab" %in% names(argu)){argu <- append(argu, list(ylab = "Mean Observations"))} #Default y axis label
    if(! "xlim" %in% names(argu)){argu <- append(argu, list(xlim = c(loLim(min(xy$x)), hiLim(max(xy$x)))))} #Default x axis limits
    if(! "ylim" %in% names(argu)){argu <- append(argu, list(ylim = c(loLim(min(xy$y)), hiLim(max(xy$y)))))} #Default y axis limits
    if(! "main" %in% names(argu)){argu <- append(argu, list(main = "Bias Diagnostic Test"))} #Default main graph title
    
    do.call("plot", argu)
    segments(loLim(min(c(xy$x, xy$y))), loLim(min(c(xy$x, xy$y))), hiLim(max(c(xy$x, xy$y))), hiLim(max(c(xy$x, xy$y))), lty = 2)
    smartlegend(x="left", y="top", "1:1 Line", inset = 0.02, lty=2, bty="n")

  }else{
    xy <- list(x = x$predictions, y = x$observations)
    el <- list(...)
    argu <- c(xy, el)
    
    if(! "xlab" %in% names(argu)){argu <- append(argu, list(xlab = "Predictions"))} #Default x axis label
    if(! "ylab" %in% names(argu)){argu <- append(argu, list(ylab = "Observations"))} #Default y axis label
    if(! "xlim" %in% names(argu)){argu <- append(argu, list(xlim = c(loLim(min(xy$x)), hiLim(max(xy$x)))))} #Default x axis limits
    if(! "ylim" %in% names(argu)){argu <- append(argu, list(ylim = c(loLim(min(xy$y)), hiLim(max(xy$y)))))} #Default y axis limits
    if(! "main" %in% names(argu)){argu <- append(argu, list(main = "Bias Diagnostic Test"))} #Default main graph title
    
    do.call("plot", argu)
    segments(loLim(min(c(xy$x, xy$y))), loLim(min(c(xy$x, xy$y))), hiLim(max(c(xy$x, xy$y))), hiLim(max(c(xy$x, xy$y))), lty = 2)
    smartlegend(x="left", y="top", "1:1 Line", inset = 0.005, lty=2, bty="n")
  }
}
