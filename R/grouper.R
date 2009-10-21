## Created by Brian Walters; bfwalters83@yahoo.com
## Creation Date: October 6, 2009
## Modified Date: October 20, 2009

##############################################################
## Function to make ordered groups of refernces/predictions ##
##############################################################

## Package Dependencies: 

grouper <- function(ref.set, pred.set, nnIndex, group.size = 25, best = TRUE){

  ## Checks ##
  if(length(ref.set) != length(pred.set)){stop("Number of data points in reference set and predicted set are not equal.")}
  if(group.size > length(ref.set) || group.size > length(pred.set)){stop("Group size larger than the number of data points.")}
  if(group.size < 10){stop("Group size cannot be less than 10.")}
  if(class(nnIndex) != "matrix"){stop("nnIndex must be a matrix")}
  ##========##

  ## Make the neighbor count data ##
  a <- as.vector(nnIndex)
  b <- a[a != -1]
  d <- cbind(as.numeric(names(table(b))), matrix(table(b)))
  e <- 0:(nrow(nnIndex)-1)
  f <- e[!(e%in%d[,1])]
  g <- rbind(d,cbind(f,rep(0,length(f))))
  h <- g[order(g[,1]),]

  nCount <- as.vector(g[,2])
  ##==============================##

  ## Prepare data frame ##
  residual <- ref.set - pred.set

  m <- cbind(ref.set, pred.set, residual, nCount)
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
