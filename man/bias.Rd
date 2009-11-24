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
  \item{object}{object of class \code{"\link[=grouper]{nnDgrps}"}}
  \item{mode}{can either be set to \code{"groups"} to assess means of
  groups of observations against means of predictions, or \code{"points"}
  to assess each observation against its corresponding prediction.}
}

\value{
  An object of \code{\link[base]{class}} \code{"nnDbias"}.\cr

  When mode = \code{"groups"}, object is a list that contains the
  following components:
  \tabular{ll}{
    \code{mean.predictions}\tab a vector of means of the groups of predictions.\cr
    \code{mean.observations}\tab a vector of means of the groups of observations.\cr
  }
  
  When mode = \code{"points"} the list contains:
  \tabular{ll}{
    \code{predictions}\tab vector of predictions.\cr
    \code{observations}\tab vector of observations.\cr
  }
}

\references{
  McRoberts, R.E. (2009) Diagnostic tools for nearest neighbors
  techniques when used with satellite imagery, \emph{Remote Sensing of
  Environment}. \bold{113}, 489--499.
}

\author{
  Brian Walters \email{walte137@msu.edu}
}

\seealso{
  \code{\link{grouper}}, \code{\link{arealBias}}
}

\examples{
data(LuceVolume)
data(LuceVolume_indx)

##First use grouper to make the \code{"nnDgrps"} object
grps <- grouper(LuceVolume$ref.volume, LuceVolume$pred.vol_k18, LuceVolume_indx)

##Using mode \code{"groups"}
bg <- bias(grps, mode = "groups")
plot(bg)

##Using mode \code{"points"}
bp <- bias(grps, mode = "points")
plot(bp)
}

\keyword{misc}
