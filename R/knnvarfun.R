

knnvarfun <- function(k=2, ref, y.ref, target, R, ...){
  
  formal.args <- names(formals(sys.function(sys.parent())))
  elip.args <- names(list(...))
  for(i in elip.args){
    if(! i %in% formal.args)
      warning("'",i,"' is not an argument")
  }
  if(missing(ref)){stop("error: ref must be specified")}
  if(missing(target)){stop("error: target must be specified")}
  if(missing(y.ref)){stop("erorr: reference data must be specified")}
  if(missing(R)){stop("error: R must be specified")}
  if(! is.matrix(ref)){stop("error: ref must be matrix")}
  if(! is.matrix(target)){stop("error: target must be matrix")}
  if(! is.matrix(R)){stop("error: R must be specified")}
  if(k <= 0){stop("error: k must be int > 0")}

  M <- dim(ref)[1]; p <- dim(ref)[2]
  N <- dim(target)[1]; q <- dim(target)[2]
  if(p != q){stop("error: dimensions must be consistent")}
  
  varyhat <- varmuhat <- varterm2 <- ytilde <- numeric(N)
  
  storage.mode(k) <- storage.mode(M) <- storage.mode(p) <- storage.mode(N) <- "integer"
  storage.mode(ref) <- storage.mode(y.ref) <- storage.mode(target) <- storage.mode(R) <- "double"
  storage.mode(ytilde) <- storage.mode(varyhat) <- storage.mode(varmuhat) <- storage.mode(varterm2) <- "double"
  
  .Call("knnvarfun_wrap", k, ref, M, p, y.ref, target, N, p, R, ytilde, varyhat, varmuhat, varterm2)

  list("yhat"=ytilde, "var1"=varmuhat, "var2"=varyhat, "varterm2"=varterm2)#, "Ybar"=Ybar, "VarYm1"= VarYm1, "VarYm2"=VarYm2)
  #invisible()
}
