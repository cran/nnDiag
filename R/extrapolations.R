## Created by Brian Walters; bfwalters83@yahoo.com
## Creation date: June 29, 2009
## Modified date: November 11, 2009

#########################################
## Function to test for extrapolations ##
#########################################

extrap <- function(image, refSet.spectral, mask = NULL, verbose = TRUE){

  beg.tim <- Sys.time()
  cat("extrap function began at: ", date(), "\n")
    
  ## Checks ##
  if(missing(image)){stop("image must be specified")}
  if(class(image) != "nnDaoi" && class(image) != "GDALReadOnlyDataset"){stop("image must be either an object of class nnDaoi or GDALReadOnlyDataset")}
  if(missing(refSet.spectral)){stop("refSet.spectral must be specified")}
  if(!is.matrix(refSet.spectral)){stop("refSet.spectral must be a matrix")}
  if(is.null(mask) == FALSE){
    if(class(mask) != "GDALReadOnlyDataset"){
      stop("mask must be an object of class GDALReadOnlyDataset or NULL")
    }
  }
  ##========##

  ## Where Image is the path to an image ##
  if(class(image)=="GDALReadOnlyDataset"){

    pb <- txtProgressBar(0, 100, style=3)##set the progress bar
    
    row.col <- dim(image)
    
    ## Get the Numbers to Break Down the Image ##
    rd <- row.col[1]%/%10## row dimension
    cd <- row.col[2]%/%10## column dimension
    
    r.pts <- matrix(rep(0, 10), nrow = 10)
    for(i in 2:10){
      r.pts[i,1] <- r.pts[i-1] + (rd + 1)
    }## y offset points
    r.lst <- rd + (row.col[1] - (r.pts[length(r.pts)] + rd))## last row dimension
        
    c.pts <- rep(0,10)
    for(i in 2:10){
      c.pts[i] <- c.pts[i-1] + (cd + 1)
    }## x offset points
    c.lst <- cd + (row.col[2] - (c.pts[length(c.pts)] + cd))## last column dimension

    os <- cbind(rep(r.pts, each = 10), rep(c.pts, 10))## matrix of offsets
    reg <- cbind(c(rep(rd, 90),rep(r.lst, 10)), rep(c(rep(cd, 9),c.lst),10))## matrix of region.dims
    ##=========================================##
    
    if(is.null(mask) == TRUE){      
      if(length(row.col) == 2){

        ## NO MASK, IMAGE HAS 1 BAND ##
        minMax <- matrix(data = c(Inf, -Inf), nrow = 2, ncol = 1, byrow = TRUE, dimnames=list(c("min", "max")))
        
        for(i in 1:100){
          imgDat <- getRasterTable(image, offset = os[i,], region.dim = reg[i,])[,3]
          
          if(suppressWarnings(min(imgDat, na.rm=TRUE)) < minMax[1,1]){
            minMax[1,1] <- suppressWarnings(min(imgDat, na.rm=TRUE))
          }
          if(suppressWarnings(max(imgDat, na.rm=TRUE)) > minMax[2,1]){
            minMax[2,1] <- suppressWarnings(max(imgDat, na.rm=TRUE))
          }
          if(verbose==TRUE){Sys.sleep(0.25);setTxtProgressBar(pb, i)}
        }
      }else{

        ## NO MASK, IMAGE HAS MULTIPLE BANDS ##
        minMax <- matrix(data = c(rep(Inf, row.col[3]),rep(-Inf, row.col[3])), nrow = 2, ncol = row.col[3], byrow = TRUE, dimnames=list(c("min", "max")))

        for(i in 1:100){
          imgDat <- getRasterTable(image, offset = os[i,], region.dim = reg[i,])[,3:(2+row.col[3])]
          
          for(j in 1:ncol(imgDat)){
            if(suppressWarnings(min(imgDat[,j], na.rm=TRUE)) < minMax[1,j]){
              minMax[1,j] <- suppressWarnings(min(imgDat[,j], na.rm=TRUE))
            }
            if(suppressWarnings(max(imgDat[,j], na.rm=TRUE)) > minMax[2,j]){
              minMax[2,j] <- suppressWarnings(max(imgDat[,j], na.rm=TRUE))
            }
          }
          if(verbose==TRUE){Sys.sleep(0.25);setTxtProgressBar(pb, i)}
        }
      }
    }else{
      if(length(row.col) == 2){

        ## MASK, IMAGE HAS 1 BAND ##
        minMax <- matrix(data = c(Inf, -Inf), nrow = 2, ncol = 1, byrow = TRUE, dimnames=list(c("min", "max")))

        for(i in 1:100){
          imgDat <- getRasterTable(image, offset = os[i,], region.dim = reg[i,])[,3]
          mskDat <- getRasterTable(mask, offset = os[i,], region.dim = reg[i,])[,3]
          
          imDat <- cbind(imgDat, mskDat)
          dat <- rep(NA, nrow(imDat))
          
          for(j in 1:nrow(imDat)){
            if(imDat[j,2] != 0){
              dat[j] <-  imDat[j,1]
            }
          }
        
          if(suppressWarnings(min(dat, na.rm=TRUE)) < minMax[1,1]){
            minMax[1,1] <- suppressWarnings(min(dat, na.rm=TRUE))
          }
          if(suppressWarnings(max(dat, na.rm=TRUE)) > minMax[2,1]){
            minMax[2,1] <- suppressWarnings(max(dat, na.rm=TRUE))
          }
          if(verbose==TRUE){Sys.sleep(0.25);setTxtProgressBar(pb, i)}
        }
      }else{
        
        ## MASK, IMAGE HAS MULTIPLE BANDS ##
        minMax <- matrix(data = c(rep(Inf, row.col[3]),rep(-Inf, row.col[3])), nrow = 2, ncol = row.col[3], byrow = TRUE, dimnames=list(c("min", "max")))
        
        for(i in 1:100){
          imgDat <- getRasterTable(image, offset = os[i,], region.dim = reg[i,])[,3:(2+row.col[3])]
          mskDat <- getRasterTable(mask, offset = os[i,], region.dim = reg[i,])[,3]
          
          imDat <- cbind(imgDat, mskDat)
          dat <- vector("list", ncol(imgDat))
          
          for(j in 1:length(dat)){
            tdat <- rep(NA, nrow(imDat))
            
            for(k in 1:nrow(imDat)){
              if(imDat[k,ncol(imDat)] != 0){
                tdat[k] <- imDat[k,j]
              }
            }
            
            dat[[j]] <- tdat
          }
          
          for(k in 1:length(dat)){
            if(suppressWarnings(min(dat[[k]], na.rm=TRUE)) < minMax[1,k]){
              minMax[1,k] <- suppressWarnings(min(dat[[k]], na.rm=TRUE))
            }
            if(suppressWarnings(max(dat[[k]], na.rm=TRUE)) > minMax[2,k]){
              minMax[2,k] <- suppressWarnings(max(dat[[k]], na.rm=TRUE))
            }
          }
          if(verbose==TRUE){Sys.sleep(0.25);setTxtProgressBar(pb, i)}
        }
      }
    }
    
    
    ## Get the range of spectral values for the reference points ##
    ref.range <- matrix(data=NA, nrow=2, ncol=ncol(refSet.spectral), dimnames=list(c("min", "max")))
    
    for(i in 1:ncol(refSet.spectral)){
      ref.range[,i] <- range(refSet.spectral[,i])
    }
    ##===========================================================##
    
    ## Make the return object ##
    extObj <- vector("list", 2)
    names(extObj) <- c("image.range", "refSet.range")
    class(extObj) <- "nnDext"
    
    extObj[[1]] <- minMax
    extObj[[2]] <- ref.range

    Sys.sleep(0.25)
    close(pb)
    end.tim <- Sys.time()
    cat("\nextrap function ended at: ", date(), "\n")
    cat("elapsed time is: ", round(difftime(end.tim, beg.tim, units="mins"),2), " minutes\n")
    return(extObj)
    ##========================##
    
  }else{
    ## Where image is an object of nnDaoi ##
    
    pb <- txtProgressBar(0, length(image$AOI.data), style=3)##set the progress bar
    
    ref.spec <- refSet.spectral[image$refPoint.index[[1]],]
    for(i in 2:length(image$refPoint.index)){
      ref.spec <- rbind(ref.spec, refSet.spectral[image$refPoint.index[[i]],])
    }
    
    ref.range <- matrix(data=NA, nrow=2, ncol=ncol(ref.spec), dimnames=list(c("min", "max")))
    for(i in 1:ncol(ref.spec)){
      ref.range[,i] <- range(ref.spec[,i])
    }
    
    
    minMax <- matrix(data = c(rep(Inf, ncol(image$AOI.data[[1]])),rep(-Inf,ncol(image$AOI.data[[1]]))), nrow = 2, ncol = ncol(image$AOI.data[[1]]), byrow = TRUE, dimnames=list(c("min", "max")))
    
    for(i in 1:ncol(minMax)){
      for(j in 1:length(image$AOI.data)){
        if(suppressWarnings(min(image$AOI.data[[j]][,i], na.rm=TRUE)) < minMax[1,i]){
          minMax[1,i] <- suppressWarnings(min(image$AOI.data[[j]][,i], na.rm=TRUE))
        }
        if(suppressWarnings(max(image$AOI.data[[j]][,i], na.rm=TRUE)) > minMax[2,i]){
          minMax[2,i] <- suppressWarnings(max(image$AOI.data[[j]][,i], na.rm=TRUE))
        }
      }
      if(verbose==TRUE){Sys.sleep(0.25);setTxtProgressBar(pb, i)}
    }
    
    ## Make the return object ##
    extObj <- vector("list", 2)
    names(extObj) <- c("image.range", "refSet.range")
    class(extObj) <- "nnDext"
    
    extObj[[1]] <- minMax
    extObj[[2]] <- ref.range

    Sys.sleep(0.25)
    close(pb)
    end.tim <- Sys.time()
    cat("\nextrap function ended at: ", date(), "\n")
    cat("elapsed time is: ", round(difftime(end.tim, beg.tim, units="mins"), 2), " minutes\n")
    return(extObj)
    ##========================##
  }
}


#################################
## Default Generic Plot Method ##
#################################

plot.nnDext <- function(x, spectral.labels = NULL, ...){
  
  ## Checks ##
  elip.args <- names(list(...))
  
  if(length(elip.args) != 0){
    for(i in 1:length(elip.args)){
      if(elip.args[i] %in% names(formals(plot.default)) == TRUE){next}
      else if(elip.args[i] %in% names(par()) == TRUE){next}
      else{stop("'",elip.args[i], "' is not a graphical parameter")}
    }
  }
  if(!is.null(spectral.labels)){
    if(ncol(x$image.range) != length(spectral.labels)){stop("the number of spectral.labels must equal the number of spectral layers in the object")}
  }
  if(is.null(spectral.labels) == TRUE){
    spectral.labels <- rep(NA, ncol(x$image.range))
    for(i in 1:length(spectral.labels)){
      spectral.labels[i] <- paste("Spectral Layer ", i, sep="")
    }
  }
  if(any(nchar(spectral.labels) > 20) == TRUE){stop("The labels for each spectral layer can only be a maximum of 20 characters long.")}
  ##========##
  
  ## Find the best range for each x-axis ##
  grph.rng <- matrix(NA, nrow = 2, ncol = ncol(x$image.range))
  
  for(i in 1:ncol(x$image.range)){
    grph.rng[1,i] <- loLim(min(c(x$image.range[1,i], x$refSet.range[1,i])))
    grph.rng[2,i] <- hiLim(max(c(x$image.range[2,i], x$refSet.range[2,i])))
  }
  ##=====================================##
  
  ## Make the graph of extrapolations ##
  maxLab <- max(nchar(spectral.labels))
  lMar <- NA
  if(maxLab > 18){lMar <- 11}
  if(maxLab <= 18 && maxLab > 10){lMar <- 9}
  if(maxLab <= 10 && maxLab > 5){lMar <- 7}
  if(maxLab <= 5 && maxLab >= 0){lMar <- 5}

  el <- list(...)
  if(! "main" %in% names(el)){main <- "kNN Extrapolation Diagnostic Test"}
  
  l <- layout(matrix(seq(1,ncol(x$image.range)+2), nrow=ncol(x$image.range)+2, ncol=1, byrow=TRUE), widths=rep(20, ncol(x$image.range)+2), heights=c(3, 4, rep(5, ncol(x$image.range)-1), 8))
  layout.show(l)
  par(mar = c(1,1,1,1))
  plot(1:2, 1:2, type = "n", axes = FALSE)
  title(main = main, outer=TRUE, cex.main=1.5, line=-3)
  plot.new()
  smartlegend(x="center", y="top", c("Target Set", "Reference Set"), lty=1, lwd=5, col=c("black", "gray"), horiz=TRUE, bty="n")
  
  par(mar=c(2,lMar,2,1))
  for(i in 1:(ncol(x$image.range)-1)){
    plot(grph.rng[,i], 1:2, type="n", ylab="", yaxt="n", bty="n", xlab="")
    mtext(spectral.labels[i], side=2, line = 1, cex=0.75, las=1)
    lines(x=c(x$image.range[1,i],x$image.range[2,i]), y=c(1.5,1.5), lwd=15, lend=1, col="black")
    lines(x=c(x$refSet.range[1,i],x$refSet.range[2,i]), y=c(1.5,1.5), lwd=12, lend=1, col="gray")
  }
  
  par(mar=c(5,lMar,2,1))
  plot(grph.rng[,ncol(grph.rng)], 1:2, type="n", ylab="", yaxt="n", bty="n", xlab="")
  title(xlab = "Spectral Values")
  mtext(spectral.labels[length(spectral.labels)], side=2, line = 1, cex=0.75, las=1)
  lines(x=c(x$image.range[1,ncol(x$image.range)],x$image.range[2,ncol(x$image.range)]), y=c(1.5,1.5), lwd=15, lend=1, col="black")
  lines(x=c(x$refSet.range[1,ncol(x$refSet.range)],x$refSet.range[2,ncol(x$refSet.range)]), y=c(1.5,1.5), lwd=12, lend=1, col="gray")
  
}
