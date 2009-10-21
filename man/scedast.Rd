\name{scedast}
\Rdversion{1.1}
\alias{scedast}
\title{Test of Scedasticity}

\description{
  Assesses the relationship between standard deviations of residuals
  with respect to response variable predictions for a test of
  scedasticity.
}

\usage{
scedast(groups)
}

\arguments{
  \item{groups}{object of class \code{nnDgrps}.}
}

\value{
  An object of \code{\link[base]{class}} \code{nnDsced}, which is a list
  containing the following components:

  \item{mean.prediction}{means of the groups of predictions.}
  \item{stdev.residuals}{the standard deviations of the residuals groups.}
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

x <- scedast(groups = grps)

plot(x)
plot(x, ylab = "Standard Deviation of Volume Residuals (ft^3/acre)",
xlab = "Mean Volume Predictions(ft^3/acre)", ylim = c(500,2500), xlim =
c(500,2500), pch = 20)
}

\keyword{misc}
