## Created by Brian Walters; bfwalters83@yahoo.com
## Creation Date: October 5, 2009
## Modified Date: October 28, 2009

#############################################################
## Function to perform an areal bias test of kNN estimates ##
#############################################################

## Package Dependencies: gplots

arealBias <- function(object, reference.set){
  
  ## Checks ##
  if(missing(object)){stop("object of class nnDaoi must be specified")}
  if(missing(reference.set)){stop("reference.set must be specified")}
  if(class(object) != "nnDaoi"){stop("object must be of class nnDaoi")}
  if(!is.vector(reference.set)){stop("reference.set must be a vector")}
  ##========##
  
  ref.indx <- object$refPoint.index
  ref.dat <- reference.set
  aoi.dat <- object$AOI.data
  
  ## Calculate the mean for the reference points in each AOI ##
  mn.ref <- rep(NA, length(ref.indx))
  
  for(i in 1:length(ref.indx)){
    mn.ref[i] <- mean(ref.dat[ref.indx[[i]]])
  }
  ##=========================================================##
  
  ## Calculate the mean for the pixels in each AOI (assuming no-data = 0) ##
  mn.aoi <- rep(NA, length(aoi.dat))

  for(i in 1:length(aoi.dat)){
    mn.aoi[i] <- mean(aoi.dat[[i]])
  }
  ##======================================================================##
  
  ## Calculate the standard error for the points in each AOI ##
  std.err <- rep(NA, length(ref.indx))
  
  for(i in 1:length(ref.indx)){
    std.err[i] <- sqrt((sd(ref.dat[ref.indx[[i]]]))^2 / length(ref.indx[[i]]))
  }
  ##=========================================================##
  
  ## Calculate confidence interval for the points in each AOI ##
  t.val <- rep(0, length(ref.indx))

  for(i in 1:length(t.val)){
    t.val[i] <- qt(0.95,(length(ref.indx[[i]]))-1)
  }

  lower.cl <- mn.ref - t.val*std.err
  upper.cl <- mn.ref + t.val*std.err
  ##==========================================================##
  
  ## Make the graph data into a list ##
  dat <- vector("list", 4)
  names(dat) <- c("knn.estimates", "probability.estimates", "lower.confidence", "upper.confidence")
  dat[[1]] <- mn.aoi
  dat[[2]] <- mn.ref
  dat[[3]] <- lower.cl
  dat[[4]] <- upper.cl
  ##=================================##

  class(dat) <- "nnDarbias"
  
  return(dat)
}


##################
## Default Plot ##
##################

plot.nnDarbias <- function(x, ...){
  
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
  
  mn.aoi <- x[[1]]
  mn.ref <- x[[2]]
  lower.cl <- x[[3]]
  upper.cl <- x[[4]]
  
  ## Make the plot ##
  xy <- list(x = mn.aoi, y = mn.ref)
  el <- list(...)
  argu <- c(xy, el)
  
  if(! "xlab" %in% names(argu)){argu <- append(argu, list(xlab = "kNN Estimates"))} #Default x axis label
  if(! "ylab" %in% names(argu)){argu <- append(argu, list(ylab = "Probability-Based Estimates"))} #Default y axis label
  if(! "xlim" %in% names(argu)){argu <- append(argu, list(xlim = c(loLim(min(mn.aoi)), hiLim(max(mn.aoi)))))} #Default x axis limits
  if(! "ylim" %in% names(argu)){argu <- append(argu, list(ylim = c(loLim(min(lower.cl)), hiLim(max(upper.cl)))))} #Default y axis limits
  if(! "main" %in% names(argu)){argu <- append(argu, list(main = "Areal Bias Diagnostic Test"))} #Default main graph title
  
  do.call("plot", argu)
  segments(loLim(min(mn.aoi)), loLim(min(mn.aoi)), hiLim(max(mn.aoi)), hiLim(max(mn.aoi)))
  segments(mn.aoi, lower.cl, mn.aoi, upper.cl, lty = 2)
  smartlegend(x="left", y="top", c("2-SE Interval","1:1 Line"), lty=c(2,1), bty="n")
  ##===============##
}
