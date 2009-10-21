\name{bias}
\Rdversion{1.1}
\alias{bias}
\title{Bias Assessment}

\description{
  Assesses bias by the relationship between observations and
  predictions, whether in ordered groups or by individual data elements.
}

\usage{
bias(object, mode = "groups")
}

\arguments{
  \item{object}{object of class \code{nnDgrps}.}
  \item{mode}{can either be set to \code{groups} to assess means of
  groups of observations against means of predictions, or \code{points}
  to assess each observation against its corresponding prediction.}
}

\value{
  An object of \code{\link[base]{class}} \code{nnDbias}.

  When mode = \code{groups}, \code{nnDbias} is a list that contains the
  following components:
  \item{mean.predictions}{means of the groups of predictions.}
  \item{mean.observations}{means of the groups of observations.}

  When mode = \code{points} the list contains:
  \item{predictions}{vector of predictions.}
  \item{observations}{vector of observations.}

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

##Using mode \code{groups}
bg <- bias(grps, mode = "groups")
plot(bg)

##Using mode \code{points}
bp <- bias(grps, mode = "points")
plot(bp)
}

\keyword{misc}
