## Created by Brian Walters; bfwalters83@yahoo.com
## Creation date: October 7, 2009
## Modified Date: October 21, 2009

################################
## Function to do a bias test ##
################################

## Package Dependencies: 

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
