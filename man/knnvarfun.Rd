\name{knnvarfun}
\Rdversion{1.1}
\alias{knnvarfun}
\title{Variance Estimators for  kNN}
\description{
  The purpose of this function is to calculate variance estimates for kNN
  predictions.  The function performs a kNN prediction on a data set
  split into reference and target sets and outputs variance estimates
  that correspond to equations (9a) and (11a) in McRoberts, et al. (2007).
}

\usage{
knnvarfun(k = 2, ref, y.ref, target, R, ...)
}

\arguments{
  \item{k}{the number of nearest neighbors, default is 2}
  \item{ref}{matrix of reference set x variables}
  \item{y.ref}{vector of reference set observations}
  \item{target}{matrix of target set x variables}
  \item{R}{spatial correlation matrix of all points}
  \item{\dots}{currently not used}
}

\details{
  Estimates are undefined for k = 1.
}

\value{
  Returns a list containing the following elements:
  
  \item{yhat}{vector of estimated values at target points}
  \item{var1}{variance of mu hat, pertaining to equation (9a) in
    McRoberts, et al. (2007)}
  \item{var2}{variance of y hat, pertaining to equation (11a)}
  \item{varterm2}{the second term in equation (11a): \eqn{-2{k}  \Sigma
  \rho}{-2k*sum(rho)}}
}

\references{
  McRoberts, R.E., Tomppo, E.O., Finley, A.O., Heikkinen, J. (2007)
  Estimating areal means and variances of forest attributes using the
  k-Nearest Neighbors technique and satellite imagery, \emph{Remote
  Sensing of Environment}. \bold{111}, 466--480.
}

\author{
  Zhen Zhang \email{zhangz19@stt.msu.edu}, Brian Walters \email{walte137@msu.edu}
}

\seealso{
  Functions that are used in the examples below:
  \code{\link[geoR]{as.geodata}}, \code{\link[geoR]{variog}},
  \code{\link[geoR]{variofit}}, \code{\link[spBayes]{iDist}},
  \code{\link[MBA]{mba.surf}}, \code{\link[fields]{image.plot}}.
}

\examples{
##########################
## Using synthetic data ##
##########################

coords.tar <- as.matrix(expand.grid(seq(0,2000,100), seq(0,2000,100)))
coords.ref <- as.matrix(expand.grid(seq(10,990,length.out=2), seq(10,990,length.out=2)))
coords <- rbind(coords.ref, coords.tar)

n <- nrow(coords)
sigma.sq <- 1
phi <- 3/500
R <- exp(-phi*as.matrix(dist(coords)))

y <- mvrnorm(1, rep(0,n), sigma.sq*R)
refs <- 1:nrow(coords.ref)
y.ref <- y[refs]
y.tar <- y[-refs]

x <- knnvarfun(k=2, coords.ref, y.ref, coords.tar, R)

## Create and graph surfaces from \code{x}
par(mfrow=c(2,3))
surf <- mba.surf(cbind(coords, y), no.X=100, no.Y=100, extend=TRUE)$xyz.est
image.plot(surf, main="observed", ylim=c(0,2000), xlim=c(0,2000))

surf <- mba.surf(cbind(coords.tar, x$yhat), no.X=100, no.Y=100, extend=TRUE)$xyz.est
image.plot(surf, main="y.hat", ylim=c(0,2000), xlim=c(0,2000))
points(coords.ref, pch=19, cex=0.5)

surf <- mba.surf(cbind(coords.tar, x$var1), no.X=100, no.Y=100, extend=TRUE)$xyz.est
image.plot(surf, main="var_mu.hat", ylim=c(0,2000), xlim=c(0,2000))
points(coords.ref, pch=19, cex=2)
points(coords.tar, cex=2)

surf <- mba.surf(cbind(coords.tar, x$var2), no.X=100, no.Y=100, extend=TRUE)$xyz.est
image.plot(surf, main="var_y.hat", ylim=c(0,2000), xlim=c(0,2000))
points(coords.ref, pch=19, cex=2)
points(coords.tar, cex=2)

surf <- mba.surf(cbind(coords.tar, x$varterm2), no.X=100, no.Y=100, extend=TRUE)$xyz.est
image.plot(surf, main="var_y.hat_term2", ylim=c(0,2000), xlim=c(0,2000))
points(coords.ref, pch=19, cex=2)
points(coords.tar, cex=2)


########################
## Using package data ##
########################
data(LuceVolume)

## Obtain the \code{R} correlation matrix from a variogram
xy.coords <- LuceVolume[,10:11]
max.dist <- max(iDist(xy.coords))

gd <- as.geodata(cbind(xy.coords, LuceVolume[,"ref.volume"]))

vario.1 <- variog(gd, uvec=(seq(0, 0.5*max.dist, length=25)))

vario.fit.1 <-variofit(vario.1, ini.cov.pars=c(750000, 1000),
                       cov.model="exponential",
                       minimisation.function="nls",
                       weights="equal")

phi <-  1/vario.fit.1$cov.pars[2]

R.matrix <- exp(-phi*as.matrix(dist(xy.coords)))

## Split the data into reference and target sets
ref <- sample(1:nrow(LuceVolume), nrow(LuceVolume)*0.5)

ref.coord <- as.matrix(LuceVolume[ref, 10:11])
tar.coord <- as.matrix(LuceVolume[-ref, 10:11])
ref.y <- LuceVolume[ref, "ref.volume"]
tar.y <- LuceVolume[-ref, "ref.volume"]
ref.spect <- as.matrix(LuceVolume[ref, 12:14])
tar.spect <- as.matrix(LuceVolume[-ref, 12:14])
coord <- rbind(ref.coord,tar.coord)
y <- c(ref.y, tar.y)


kvf <- knnvarfun(k=5, ref.spect, ref.y, tar.spect, R.matrix)

## Create and graph surfaces from \code{kvf}
par(mfrow=c(2,3))
surf <- mba.surf(cbind(coord, y), no.X=100, no.Y=100, extend=TRUE)$xyz.est
image.plot(surf, main="observed")

surf <- mba.surf(cbind(tar.coord, kvf$yhat), no.X=100, no.Y=100, extend=TRUE)$xyz.est
image.plot(surf, main="y.hat")
points(ref.coord, pch=20)

surf <- mba.surf(cbind(tar.coord, kvf$var1), no.X=100, no.Y=100, extend=TRUE)$xyz.est
image.plot(surf, main="var_mu.hat")
points(ref.coord, pch=20)
points(tar.coord, cex=2)

surf <- mba.surf(cbind(tar.coord, kvf$var2), no.X=100, no.Y=100, extend=TRUE)$xyz.est
image.plot(surf, main="var_y.hat")
points(ref.coord, pch=20)
points(tar.coord, cex=2)

surf <- mba.surf(cbind(tar.coord, kvf$varterm2), no.X=100, no.Y=100, extend=TRUE)$xyz.est
image.plot(surf, main="var_y.hat_term2")
points(ref.coord, pch=20)
points(tar.coord, cex=2)
}

\keyword{misc}