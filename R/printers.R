## Default print methods for nnD* objects
## Created by Brian Walters; bfwalters83@yahoo.com
## Creation date: October 21, 2009
## Modified Date: 

#########################
## Categorical objects ##
#########################
print.nnDcat <- function(x, ...){
  cat("Confusion Matrix:\n")
  print(x[[1]])
  cat("\nOverall Accuracy:\n")
  cat(x[[2]][1], "%\n", sep="")
  cat("\nUser's Accuracy (%):\n")
  print(x[[3]])
  cat("\nProducer's Accuracy (%):\n")
  print(x[[4]])
  cat("\nOverall Kappa Coefficient:\n")
  cat(x[[5]][1],"\n")
  cat("\nUser's Conditional Kappa:\n")
  print(x[[6]])
  cat("\nProducer's Conditional Kappa:\n")
  print(x[[7]])
}

####################
## Groups objects ##
####################
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

#######################
## Scedastic Objects ##
#######################
print.nnDsced <- function(x, ...){
  cat("Mean of Predicted Groups:\n")
  print(x[[1]])
  cat("\nStandard Deviation of Residual Groups:\n")
  print(x[[2]])
}

##################################################
## Outliers and Influential Observation Objects ##
##################################################
print.nnDoi <- function(x, ...){
  cat("Times Used as Nearest Neighbor:\n")
  cat("(Only first 10 shown.)\n")
  print(x[[1]][1:10])
  cat("\nStandardized Residuals:\n")
  cat("(Only first 10 shown.)\n")
  print(x[[2]][1:10])
}

###################
## Bias  Objects ##
###################
print.nnDbias <- function(x, ...){
  m <- attributes(x)

  if(m$mode == "groups"){  
    cat("Means of Predicted Groups:\n")
    print(x[[1]])
    cat("\nMeans of Reference Groups:\n")
    print(x[[2]])
  }else{
    cat("Predicted Points:\n")
    cat("(Only first 20 shown.)\n")
    print(x[[1]][1:20])
    cat("\nReference Points:\n")
    cat("(Only first 20 shown.)\n")
    print(x[[2]][1:20])
  }
}
