## Created by Brian Walters; bfwalters83@yahoo.com
## Creation date: October 20, 2009
## Modified date: 

######################################################
## Function to perform categorical diagnostic tests ##
######################################################

## Package Dependencies: vcd

categorical <- function(reference.set, predicted.set, class.names=NULL){
  
  ## Checks ##
  if(missing(reference.set)){stop("reference.set must be specified")}
  if(missing(predicted.set)){stop("predicted.set must be specified")}
  if(is.vector(reference.set)!=TRUE){stop("The reference set must be a vector.")}
  if(is.vector(predicted.set)!=TRUE){stop("The predicted set must be a vector.")}
  if(!is.null(class.names)){
    if(length(unique(reference.set))!=nrow(class.names)){stop("The number of class names does not equal the number of classes in the reference set.")}
  }
  ##========##

  ## Make the Confusion Matrix ##
  confusion.matrix <- table(predicted.set, reference.set, dnn=c("Predicted Data","Reference Data"))
  
  if(dim(confusion.matrix)[1] > dim(confusion.matrix)[2]){

    unq.c <- unique(predicted.set)
    unq.r <- unique(reference.set)
    
    m <- unq.c[!(unq.c%in%unq.r)]
    
    for(i in 1:length(m)){
      co <- matrix(0, nrow=length(confusion.matrix[,1]), ncol=1)
      colnames(co) <- m[i]
      confusion.matrix <- cbind(confusion.matrix[,colnames(confusion.matrix) < colnames(co)], co, confusion.matrix[,colnames(confusion.matrix) > colnames(co)])
    }
  }
  
  if(dim(confusion.matrix)[1] < dim(confusion.matrix)[2]){
    
    unq.c <- unique(predicted.set)
    unq.r <- unique(reference.set)
    
    m <- unq.r[!(unq.r%in%unq.c)]

    for(i in 1:length(m)){
      ro <- matrix(0, nrow=1, ncol=length(confusion.matrix[1,]))
      rownames(ro) <- m[i]
      confusion.matrix <- rbind(confusion.matrix[rownames(confusion.matrix) < rownames(ro),], ro, confusion.matrix[rownames(confusion.matrix) > rownames(ro),])
    }
  }
  ##===========================##

  ## Add Class Names to the Confusion Matrix if requested (default no) ##
  if(!is.null(class.names)){
    
    x <- dimnames(confusion.matrix)
    
    if(all(x[[1]]==class.names[,1]) && all(x[[2]]==class.names[,1])){
      x[[1]] <- class.names[,2]
      x[[2]] <- class.names[,2]
    }else{
      stop("The class numbers given do not match the class numbers in the reference set.")
    }
    
    dimnames(confusion.matrix) <- x
  }
  ##===================================================================##

  agree <- diag(confusion.matrix)
  n <- sum(confusion.matrix)

  ## User's Accuracy ##  
  row.marg <- apply(confusion.matrix, 1, sum)
  ua <- agree/row.marg
  ##=================##

  ## Producer's Accuracy ##
  col.marg <- apply(confusion.matrix, 2, sum)
  pa <- agree/col.marg
  ##=====================##

  ## User's Conditional Kappa ##
  col.marg.p <- col.marg/n
  uck <- (ua - col.marg.p)/(1 - col.marg.p)
  ##==========================##

  ## Producer's Conditional Kappa ##
  row.marg.p <- row.marg/n
  pck <- (pa - row.marg.p)/(1 - row.marg.p)
  ##==============================##

  ## Make the final object ##
  cat.list <- vector("list", 7)
  cat.list[[1]] <- confusion.matrix
  cat.list[[2]] <- round(100*(sum(agree)/n),1)
  cat.list[[3]] <- round(100*ua,1)
  cat.list[[4]] <- round(100*pa,1)
  cat.list[[5]] <- round(Kappa(confusion.matrix)$Unweighted[1],4)
  cat.list[[6]] <- round(uck,4)
  cat.list[[7]] <- round(pck,4)
  names(cat.list) <- c("Confusion.Matrix", "Overall.Accuracy", "Users.Accuracy", "Producers.Accuracy", "Overall.Kappa", "Users.Cond.Kappa", "Producers.Cond.Kappa")
  class(cat.list) <- "nnDcat"
  
  return(cat.list)
  ##=======================##
}
