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
scedast(object)
}

\arguments{
  \item{object}{object of class \code{"\link[=grouper]{nnDgrps}"}}
}

\value{
  An object of \code{\link[base]{class}} \code{"nnDsced"}, which is a list
  containing the following components:

  \item{mean.prediction}{a vector of means of the groups of predictions}
  \item{stdev.residuals}{a vector of the standard deviations of the residual groups}
}

\references{
  McRoberts, R.E. (2009) Diagnostic tools for nearest neighbors
  techniques when used with satellite imagery, \emph{Remote Sensing of
  Environment}. \bold{113}, 489--499.
}

\author{
  Brian Walters \email{walte137@msu.edu}
}

\examples{
data(LuceVolume)
data(LuceVolume_indx)

##First use grouper to make the \code{"nnDgrps"} object
grps <- grouper(LuceVolume$ref.volume, LuceVolume$pred.vol_k18, LuceVolume_indx)

x <- scedast(object = grps)
\dontrun{
plot(x)
plot(x, ylab = "Standard Deviation of Volume Residuals (ft^3/acre)",
xlab = "Mean Volume Predictions(ft^3/acre)", ylim = c(500,2500), xlim =
c(500,2500), pch = 20)
}
}

\keyword{misc}
