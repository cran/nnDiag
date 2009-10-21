\name{nnDiag-package}
\Rdversion{1.1}
\alias{nnDiag-package}
\alias{nnDiag}
\docType{package}

\title{
  A suite of graphical diagnostic tools to evaluate kNN classifications.
}

\description{
  Contains diagnostic functions that can be applied to k-nearest
  neighbor classifications, both continuous and categorical data.
}

\details{
  \tabular{ll}{
    Package: \tab nnDiag\cr
    Type: \tab Package\cr
    Version: \tab 0.0-1\cr
    Date: \tab 2009-10-21\cr
    License: \tab GPL (>= 2)\cr
    LazyLoad: \tab yes\cr
  }
}

\author{
  Brian Walters \email{bfwalters83@yahoo.com}

  Maintainer: Brian Walters
}

\references{
  Hudson, W.D. and Ramm, C.W. (1987) Correct Formulation of the Kappa
  Coefficient of Agreement, \emph{Photogrammetric Engineering and Remote
    Sensing}. \bold{53}, 421--422.  

  McRoberts, R.E. (2009) Diagnostic tools for nearest neighbors
  techniques when used with satellite imagery, \emph{Remote Sensing of
    Environment}. \bold{113}, 489--499.

  Rosenfield, G.H. and Fitzpatrick-Lins, K. (1986) A Coefficient of
  Agreement as a Measure of Thematic Classification Accuracy,
  \emph{Photogrammetric Engineering and Remote Sensing}. \bold{52},
  223--227.

  Story, M. and Congalton, R.G. (1986) Accuracy Assessment: A User's
  Perspective, \emph{Photogrammetric Engineering and Remote
  Sensing}. \bold{52}, 397--399.  
}

\keyword{package}

\examples{
##Categorical Data
data(FORGRP)

x <- categorical(reference.set = FORGRP$refFORGRP,
predicted.set = FORGRP$predFORGRP)
x

##Continuous Data
data(VOLUME)
data(VOLUMEnnIndex_k20)
nnIndex <- as.matrix(VOLUMEnnIndex_k20)

grps <- grouper(ref.set = VOLUME$refVOLUME, pred.set = VOLUME$predVOL_k20,
nnIndex = nnIndex)

sc <- scedast(groups = grps) ##Test of scedasticity
plot(sc)

oi <- outInflu(object = grps)##Potential outliers and influential observations
plot(oi, pch = 20)

bg <- bias(grps, mode = "groups")##Bias assessment
plot(bg)
}
