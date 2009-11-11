## Created by Brian Walters; bfwalters83@yahoo.com
## Creation Date: September 17, 2009
## Modified Date: November 16, 2009

####################################################################
## Function to make a set of AOIs for use in kNN diagnostic tests ##
####################################################################

## Package Dependencies: rgdal, spBayes

aoiMaker <- function(ref.coords, num.aoi, image, mask = NULL, aoi.size = 1000, min.points = 25, seed = NULL, verbose = TRUE){

  beg.tim <- Sys.time()
  cat("aoiMaker function began at: ", date(), "\n")
  
  ## Checks ##
  if(missing(ref.coords)){stop("ref.coords must be specified")}
  if(missing(num.aoi)){stop("num.aoi must be specified")}
  if(missing(image)){stop("image must be specified")}
  if(!is.matrix(ref.coords)){stop("ref.coords must be a matrix")}
  if(ncol(ref.coords) != 2){stop("ref.coords must be a two column matrix")}
  if(class(image) != "GDALReadOnlyDataset"){stop("image must be a an object of the class \"GDALReadOnlyDataset\"")}
  if(is.null(mask) == FALSE){
    if(class(mask) != "GDALReadOnlyDataset"){
      stop("mask must be a an object of the class \"GDALReadOnlyDataset\" or NULL")
    }
  }
  if(is.null(seed) == FALSE){
    set.seed(seed)
  }
  ##===============================================================##

  ## Prep Steps ##
  pb <- txtProgressBar(0, num.aoi, style=3)##set the progress bar
  
  aoi.list <- vector("list", num.aoi)
  aoiDat.list <- vector("list", num.aoi)
  pip.list <- vector("list", num.aoi)

  row.col <- dim(image)
  image.path <- getDescription(image)

  img.inf <- GDALinfo(image.path)
  ll <- c(img.inf["ll.x"], img.inf["ll.y"]); names(ll) <- NULL
  lr <- c((img.inf["columns"] * img.inf["res.x"] + img.inf["ll.x"]), img.inf["ll.y"]); names(lr) <- NULL
  ul <- c(img.inf["ll.x"], (img.inf["rows"] * img.inf["res.y"] + img.inf["ll.y"])); names(ul) <- NULL
  ur <- c((img.inf["columns"] * img.inf["res.x"] + img.inf["ll.x"]), (img.inf["rows"] * img.inf["res.y"] + img.inf["ll.y"])); names(ur) <- NULL
  img.extnt <- rbind(ll, lr, ur, ul)

  i <- 0
  ##=======================================================##

  
  ## Create AOIs where mask = NULL and image.path only has one band ##
  if(is.null(mask) == TRUE){
    if(length(row.col) == 2){
      
      while(i < num.aoi){
        
        row.off <- floor(runif(1, 0, row.col[1] - aoi.size))
        col.off <- floor(runif(1, 0, row.col[2] - aoi.size))
        
        aoi <- readGDAL(image.path, offset = c(row.off, col.off), region.dim = c(aoi.size, aoi.size), silent = TRUE)
        extnt <- bbox(aoi)
        poly <- rbind(c(extnt[1,1],extnt[2,2]), c(extnt[1,2],extnt[2,2]), c(extnt[1,2],extnt[2,1]), c(extnt[1,1],extnt[2,1]))
        pip <- pointsInPoly(poly,ref.coords)
        
        if(length(pip) >= min.points){
          i <- i+1
          aoi.list[[i]] <- aoi
          aoiDat.list[[i]] <- attr(aoi, "data")
          pip.list[[i]] <- pip
          if(verbose==TRUE){Sys.sleep(0.25);setTxtProgressBar(pb, i)}
        }
      }

  ## Create AOIs where mask = NULL and image.path has multiple bands ##    
    }else{
      
      while(i < num.aoi){
        
        row.off <- floor(runif(1, 0, row.col[1] - aoi.size))
        col.off <- floor(runif(1, 0, row.col[2] - aoi.size))
        
        aoi <- readGDAL(image.path, offset = c(row.off, col.off), region.dim = c(aoi.size, aoi.size), silent = TRUE)
        extnt <- bbox(aoi)
        poly <- rbind(c(extnt[1,1],extnt[2,2]), c(extnt[1,2],extnt[2,2]), c(extnt[1,2],extnt[2,1]), c(extnt[1,1],extnt[2,1]))
        pip <- pointsInPoly(poly,ref.coords)
        
        if(length(pip) >= min.points){
          i <- i+1
          aoi.list[[i]] <- aoi
          aoiDat.list[[i]] <- attr(aoi, "data")
          pip.list[[i]] <- pip
          if(verbose==TRUE){Sys.sleep(0.25);setTxtProgressBar(pb, i)}
        }
      }
    }
    
  ## Create AOIs with a mask and image.path only has one band ##  
  }else{
    if(length(row.col) == 2){
      
      mask.path <- getDescription(mask)
      
      while(i < num.aoi){
        
        row.off <- floor(runif(1, 0, row.col[1] - aoi.size))
        col.off <- floor(runif(1, 0, row.col[2] - aoi.size))
        
        img.aoi <- readGDAL(image.path, offset = c(row.off, col.off), region.dim = c(aoi.size, aoi.size), silent = TRUE)
        msk.aoi <- readGDAL(mask.path, offset = c(row.off, col.off), region.dim = c(aoi.size, aoi.size), silent = TRUE)
        
        extnt <- bbox(img.aoi)
        poly <- rbind(c(extnt[1,1],extnt[2,2]), c(extnt[1,2],extnt[2,2]), c(extnt[1,2],extnt[2,1]), c(extnt[1,1],extnt[2,1]))
        pip <- pointsInPoly(poly,ref.coords)
        
        if(length(pip) >= min.points){
          i <- i+1
          aoi.list[[i]] <- img.aoi
          pip.list[[i]] <- pip
          
          iDat <- attr(img.aoi, "data")
          mDat <- attr(msk.aoi, "data")
          imDat <- cbind(iDat, mDat)

          aoiDat <- rep(NA, nrow(imDat))
          for(j in 1:nrow(imDat)){
            if(imDat[j,2] != 0){
              aoiDat[j] <- imDat[j,1]
            }
          }
          aoiDat.list[[i]] <- na.omit(aoiDat)
          
          if(verbose==TRUE){Sys.sleep(0.25);setTxtProgressBar(pb, i)}
        }
      }

  ## Create AOIs using a mask and image.path has multiple bands ##    
    }else{
      
      mask.path <- getDescription(mask)
      
      while(i < num.aoi){
        
        row.off <- floor(runif(1, 0, row.col[1] - aoi.size))
        col.off <- floor(runif(1, 0, row.col[2] - aoi.size))
        
        img.aoi <- readGDAL(image.path, offset = c(row.off, col.off), region.dim = c(aoi.size, aoi.size), silent = TRUE)
        msk.aoi <- readGDAL(mask.path, offset = c(row.off, col.off), region.dim = c(aoi.size, aoi.size), silent = TRUE)
        
        extnt <- bbox(img.aoi)
        poly <- rbind(c(extnt[1,1],extnt[2,2]), c(extnt[1,2],extnt[2,2]), c(extnt[1,2],extnt[2,1]), c(extnt[1,1],extnt[2,1]))
        pip <- pointsInPoly(poly,ref.coords)
        
        if(length(pip) >= min.points){
          i <- i+1
          aoi.list[[i]] <- img.aoi
          pip.list[[i]] <- pip
          
          iDat <- attr(img.aoi, "data")
          mDat <- attr(msk.aoi, "data")
          imDat <- cbind(iDat, mDat)

          aoiDat <- vector("list", ncol(iDat))
          
          for(j in 1:length(aoiDat)){
            adat <- rep(NA, nrow(imDat))
            
            for(k in 1:nrow(imDat)){
              if(imDat[k,ncol(imDat)] != 0){
                adat[k] <- imDat[k,j]
              }
            }
            
            aoiDat[[j]] <- na.omit(adat)
          }
          aoiDatM <- matrix(data = unlist(aoiDat), nrow = length(aoiDat[[1]]), ncol = length(aoiDat), byrow = FALSE)
          aoiDat.list[[i]] <- aoiDatM
        }       
        if(verbose==TRUE){Sys.sleep(0.25);setTxtProgressBar(pb, i)}
      }
    }
  }
  ##==================================================================================================##

  ## Make an object of class nnDaoi ##
  aoiObj <- vector("list", 5)
  names(aoiObj) <- c("AOI.spatial", "AOI.data", "refPoint.index", "ref.coordinates", "image.extent")
  class(aoiObj) <- "nnDaoi"

  aoiObj[[1]] <- aoi.list
  aoiObj[[2]] <- aoiDat.list
  aoiObj[[3]] <- pip.list
  aoiObj[[4]] <- ref.coords
  aoiObj[[5]] <- img.extnt
  ##================================##

  Sys.sleep(0.25)
  close(pb)
  end.tim <- Sys.time()
  cat("\naoiMaker function ended at: ", date(), "\n")
  cat("elapsed time is: ", round(difftime(end.tim, beg.tim, units="mins"),2), " minutes\n")
  return(aoiObj)
}


###################################################
## Function to visualize AOIs over set of points ##
###################################################

plot.nnDaoi <- function(x, ...){

  ## Checks ##
  if(class(x) != "nnDaoi"){stop("object must be of class nnDaoi")}
  
  elip.args <- names(list(...))
  
  if(length(elip.args) != 0){
    for(i in 1:length(elip.args)){
      if(elip.args[i] %in% names(formals(plot.default)) == TRUE){next}
      else if(elip.args[i] %in% names(par()) == TRUE){next}
      else{stop("'",elip.args[i], "' is not a graphical parameter")}
    }
  }
  ##========##
  
  ## Make the plot ##
  xy <- list(x = x$image.extent, pch = NA)
  el <- list(...)
  argu <- c(xy, el)

  if(! "xlab" %in% names(argu)){argu <- append(argu, list(xlab = "Easting"))} #Default x axis label
  if(! "ylab" %in% names(argu)){argu <- append(argu, list(ylab = "Northing"))} #Default y axis label
  if(! "main" %in% names(argu)){argu <- append(argu, list(main = "AOI Map"))} #Default main graph title
  
  do.call("plot", argu)
  polygon(x$image.extent, border = "red", lwd = 2)
  points(x$ref.coordinates, pch = 20)
  for(i in 1:length(x$AOI.spatial)){
    exnt <- attr(x$AOI.spatial[[i]], "bbox")
    pxt <- rbind(c(exnt[1,1],exnt[2,2]), c(exnt[1,2],exnt[2,2]), c(exnt[1,2],exnt[2,1]), c(exnt[1,1],exnt[2,1]))
    polygon(pxt, border = "blue", lwd = 2)
  }
  ##===============##
}
