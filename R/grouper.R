## Created by Brian Walters; bfwalters83@yahoo.com
## Creation Date: October 6, 2009
## Modified Date: November 4, 2009

##############################################################
## Function to make ordered groups of refernces/predictions ##
##############################################################

## Package Dependencies: 

grouper <- function(reference.set, predicted.set, nnIndex, group.size = 25, best = TRUE){

  ## Checks ##
  if(missing(reference.set)){stop("reference.set must be specified")}
  if(missing(predicted.set)){stop("predicted.set must be specified")}
  if(missing(nnIndex)){stop("nnIndex must be specified")}
  if(!is.vector(reference.set)){stop("reference.set must be a vector")}
  if(!is.vector(predicted.set)){stop("predicted.set must be a vector")}
  if(class(nnIndex) != "data.frame"){stop("nnIndex must be a data.frame")}
  if(length(reference.set) != length(predicted.set)){stop("number of data elements in reference set and predicted set are not equal")}
  if(group.size > length(reference.set) || group.size > length(predicted.set)){stop("group size cannot be larger than the number of data elements")}
  if(group.size < 10){stop("group size cannot be less than 10")}
  ##========##

  ## Make the neighbor count data ##
  id <- as.numeric(rownames(nnIndex))
  a <- as.matrix(nnIndex)
  a <- as.vector(a, mode="numeric")
  a <- a[a != -1]
  a <- na.omit(a)
  b <- cbind(as.numeric(names(table(a))), matrix(table(a)))
  d <- id[!(id %in% b[,1])]
  e <- rbind(b, cbind(d, rep(0, length(d))))
  f <- e[order(e[,1]),]
  nCount <- f[,2]
  ##==============================##

  ## Another Check ##
  if(length(nCount) != length(reference.set)){stop("The potential neighbors in nnIndex and the reference.set do not match.  Leave-One-Out cross validation must be used to perform these diagnostic tests.")}
  ##===============##
  
  ## Prepare data frame ##
  residual <- reference.set - predicted.set

  m <- cbind(reference.set, predicted.set, residual, nCount)
  colnames(m) <- c("reference", "predicted", "residuals", "neighbor.count")
  df <- as.data.frame(m)

  dfs <- df[order(df$predicted),]
  ##====================##

  ## Find the best group size (can't be less than 10) ##
  if(best == TRUE){
    size <- 0
    rng <- 0
    
    while(size == 0){
      rng <- rng + 10
      low <- group.size - rng
      high <- group.size + rng
      
      if(low <= 9){
        low <- 10
      }
      
      rem <- cbind(low:high, nrow(dfs)%%low:high)
      sel <- rem[(rem[,1] - rem[,2]) <= (group.size*0.2) | rem[,2] == 0,]
      
      if(length(sel) != 0){
        if(is.vector(sel)==TRUE){
          size <- sel[1]
        }else{
          size <- sel[abs(group.size - sel[,1]) == min(abs(group.size - sel[,1])), 1]
          if(length(size) > 1){
            size <- max(size)
          }
        }
      }
    }
  }else{
    size <- group.size
  }
  ##==================================================##

  ## Make a matrix of groups of reference values ##
  full.grps <- length(dfs$reference)%/%size

  if(length(dfs$reference) %% size == 0){
    ref.grps <- matrix(data=dfs$reference, nrow=size, ncol=full.grps, byrow=FALSE)
  }else{
    group <- matrix(data=dfs$reference[1:(length(dfs$reference)-(length(dfs$reference)-(full.grps*size)))], nrow=size, ncol=full.grps, byrow=FALSE)
    last.grp <- rep(NA, size)
    last.dat <- dfs$reference[((length(dfs$reference)-(length(dfs$reference)-(full.grps*size)))+1):length(dfs$reference)]

    for(i in 1:length(last.grp)){
      last.grp[i] <- last.dat[i]
    }

    ref.grps <- cbind(group, last.grp)
    colnames(ref.grps) <- NULL
  }
  ##=============================================##

  ## Make a matrix of groups of predicted values ##
  full.grps <- length(dfs$predicted)%/%size

  if(length(dfs$predicted) %% size == 0){
    pred.grps <- matrix(data=dfs$predicted, nrow=size, ncol=full.grps, byrow=FALSE)
  }else{
    group <- matrix(data=dfs$predicted[1:(length(dfs$predicted)-(length(dfs$predicted)-(full.grps*size)))], nrow=size, ncol=full.grps, byrow=FALSE)
    last.grp <- rep(NA, size)
    last.dat <- dfs$predicted[((length(dfs$predicted)-(length(dfs$predicted)-(full.grps*size)))+1):length(dfs$predicted)]

    for(i in 1:length(last.grp)){
      last.grp[i] <- last.dat[i]
    }

    pred.grps <- cbind(group, last.grp)
    colnames(pred.grps) <- NULL
  }
  ##=============================================##

  ## Make a matrix of groups of residual values ##
  full.grps <- length(dfs$residuals)%/%size

  if(length(dfs$residuals)%%size==0){
    res.grps <- matrix(data=dfs$residuals, nrow=size, ncol=full.grps, byrow=FALSE)
  }else{
    group <- matrix(data=dfs$residuals[1:(length(dfs$residuals)-(length(dfs$residuals)-(full.grps*size)))], nrow=size, ncol=full.grps, byrow=FALSE)
    last.grp <- rep(NA, size)
    last.dat <- dfs$residuals[((length(dfs$residuals)-(length(dfs$residuals)-(full.grps*size)))+1):length(dfs$residuals)]

    for(i in 1:length(last.grp)){
      last.grp[i] <- last.dat[i]
    }

    res.grps <- cbind(group,last.grp)
    colnames(res.grps) <- NULL 
  }
  ##============================================##

  ## Make an object of class nnDiag_groups ##
  grpData <- vector("list", 5)
  names(grpData) <- c("ordered.data", "reference.groups", "predicted.groups", "residual.groups", "group.size")
  class(grpData) <- "nnDgrps"

  grpData[[1]] <- dfs
  grpData[[2]] <- ref.grps
  grpData[[3]] <- pred.grps
  grpData[[4]] <- res.grps
  grpData[[5]] <- size
  ##====================================##

  return(grpData)
  
}


##################################
## Default Generic Print Method ##
##################################

print.nnDgrps <- function(x, ...){
  cat("Data Ordered by Predictions, First 10 Rows:\n\n")
  print(x[[1]][1:10,])
  cat("\nFirst 3 Reference Groups:\n")
  print(x[[2]][,1:3])
  cat("\nFirst 3 Predicted Groups:\n")
  print(x[[3]][,1:3])
  cat("\nFirst 3 Residual Groups:\n")
  print(x[[4]][,1:3])
  cat("\nGroup Size:", x[[5]],"\n\n", sep=" ")
}
