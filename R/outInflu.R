## Created by Brian Walters; bfwalters83@yahoo.com
## Creation date: June 29, 2009
## Modified Date: October 28, 2009

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


#################################
## Default Generic Plot Method ##
#################################

plot.nnDoi <- function(x, ...){

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
