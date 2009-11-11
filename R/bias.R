## Created by Brian Walters; bfwalters83@yahoo.com
## Creation date: October 7, 2009
## Modified Date: October 28, 2009

################################
## Function to do a bias test ##
################################

## Package Dependencies: gplots

bias <- function(object, mode = "groups"){

  ## Checks ##
  if(class(object) != "nnDgrps"){stop("Object must be of class nnDgrps.")}
  if(mode %in% c("groups", "points") == FALSE){stop("Mode must be set to 'groups' or 'points'")}
  ##========##

  ## Make the plot data ##
  if(mode == "groups"){
    dat <- vector("list", 2)
    dat[[1]] <- apply(object$predicted.groups, 2, mean, na.rm=TRUE)
    dat[[2]] <- apply(object$reference.groups, 2, mean, na.rm=TRUE)

    attributes(dat) <- list(names = c("mean.predictions", "mean.observations"), class = "nnDbias", mode = "groups")
    return(dat)

  }else{
    dat <- vector("list", 2)
    dat[[1]] <- object$ordered.data[,2]
    dat[[2]] <- object$ordered.data[,1]

    attributes(dat) <- list(names = c("predictions", "observations"), class = "nnDbias", mode = "points")
    return(dat)    
  }
  ##====================##
}


#################################
## Default Generic Plot Method ##
#################################

plot.nnDbias <- function(x, ...){

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
