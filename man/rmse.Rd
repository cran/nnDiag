\name{rmse}
\Rdversion{1.1}
\alias{rmse}
\title{Root Mean Square Error}

\description{
  Given a reference set of continuous variable observations and a set of
  corresponding predictions, this function will calculate the root mean
  square error of the classification.
}

\usage{
rmse(reference.set, predicted.set)
}

\arguments{
  \item{reference.set}{vector of observed values.}
  \item{predicted.set}{vector or matrix of predicted values.}
}

\details{
  The \code{predicted.set} may be a vector if there is only one
  classification to find RMSE for, or a matrix if there is multiple
  classifications.  For example, if kNN classifications were run using
  the same reference set with multiple values of k.
}

\value{
  A vector of root mean square error value(s).
}

\references{
  McRoberts, R.E. (2009) Diagnostic tools for nearest neighbors
  techniques when used with satellite imagery, \emph{Remote Sensing of
  Environment}. \bold{113}, 489--499.
}

\author{
  Brian Walters \email{bfwalters83@yahoo.com}
}

\examples{
data(VOLUME)

##Using one predicted set
x <- rmse(reference.set = VOLUME$refVOLUME, predicted.set =
VOLUME$predVOL_k10)
x

##Using multiple predicted sets
x <- rmse(reference.set = VOLUME$refVOLUME, predicted.set =
as.matrix(VOLUME[,4:10]))
x
}

\keyword{misc}
