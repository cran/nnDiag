\name{categorical}
\Rdversion{1.1}
\alias{categorical}
\title{Diagnostics for categorical variable classifications}

\description{
  Given a reference set of categorical variable observations and its
  corresponding predictions this function produces the classification
  confusion matrix, accuracy and kappa coefficient.  Accuracy and kappa
  are given as overall, user's and producer's.
}

\usage{
categorical(reference.set, predicted.set, class.names = NULL)
}

\arguments{
  \item{reference.set}{vector of observed values.}
  \item{predicted.set}{vector of predicted values.}
  \item{class.names}{optional \eqn{n\times 2}{nx2} matrix of class names. The first
    column being the class numbers with the second column being their corresponding class names.}
}

\details{
  Using the optional argument \code{class.names} will insert the name of
  each class into the output, otherwise the class numbers extracted from
  the reference set are used.
}

\value{
  \code{categorical} returns an object of \code{\link[base]{class}}
  \code{nnDcat}, which is a list containing the following components:
  
  \item{Confusion.Matrix}{matrix comparing the reference set to the
    predicted set. The major diagonal is where the two agree.}
  \item{Overall.Accuracy}{overall accuracy of the classification.}
  \item{Users.Accuracy}{user's accuracy for each class.}
  \item{Producers.Accuracy}{producer's accuracy for each class.}
  \item{Overall.Kappa}{kappa coefficient of the overall classification.}
  \item{Users.Cond.Kappa}{user's conditional kappa coefficient for
    each class.}
  \item{Producers.Cond.Kappa}{producer's conditional kappa coefficient for
    each class.}
}

\references{
  Hudson, W.D. and Ramm, C.W. (1987) Correct Formulation of the Kappa
  Coefficient of Agreement, \emph{Photogrammetric Engineering and Remote
    Sensing}. \bold{53}, 421--422.
  
  Rosenfield, G.H. and Fitzpatrick-Lins, K. (1986) A Coefficient of
  Agreement as a Measure of Thematic Classification Accuracy,
  \emph{Photogrammetric Engineering and Remote Sensing}. \bold{52},
  223--227.

  Story, M. and Congalton, R.G. (1986) Accuracy Assessment: A User's
  Perspective, \emph{Photogrammetric Engineering and Remote
  Sensing}. \bold{52}, 397--399.
}

\author{
  Brian Walters \email{bfwalters83@yahoo.com}
}

\examples{
data(FORGRP)

x <- categorical(reference.set = FORGRP$refFORGRP,
predicted.set = FORGRP$predFORGRP)
x

##Add class names
clsnms <- cbind(c(100,120,380,400,700,800,900,999), c("WRJ Pine",
"Spruce/Fir", "Exotic SW", "Oak/Pine", "Elm/Ash/Cottonwood",
"Maple/Beech/Birch", "Aspen/Birch", "Nonstocked"))

x <- categorical(reference.set = FORGRP$refFORGRP,
predicted.set = FORGRP$predFORGRP, class.names = clsnms)
x
}

\keyword{misc}
