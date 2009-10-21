\name{outInflu}
\Rdversion{1.1}
\alias{outInflu}
\title{Find potential outliers and influential observations}

\description{
  Assesses the relationship between standardized residuals and the sum
  of times used as a neighbor.  Outliers/influential observations will
  have a high standardized residual absolute value and/or have been used
  as a neighbor numerous times.
}

\usage{
outInflu(object)
}

\arguments{
  \item{object}{object of class \code{nnDgrps}.}
}

\value{
  An object of \code{\link[base]{class}} \code{nnDoi}, which is a list
  containing the following components:
  
  \item{neighbor.count}{number of times used as a neighbor in kNN classification.}
  \item{standardized.residuals}{standardized residuals (the ratios of
  residuals and their standard deviations.}
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
data(VOLUMEnnIndex_k20)
nnIndex <- as.matrix(VOLUMEnnIndex_k20)

##First use grouper to make the nnDgrps object
grps <- grouper(VOLUME$refVOLUME, VOLUME$predVOL_k20, nnIndex = nnIndex)

x <- outInflu(object = grps)

plot(x, pch = 20)

}

\keyword{misc}