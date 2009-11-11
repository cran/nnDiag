\name{outInflu}
\Rdversion{1.1}
\alias{outInflu}
\title{Find Potential Outliers and Influential Observations}

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
  \item{object}{object of class \code{"\link[=grouper]{nnDgrps}"}}
}

\value{
  An object of \code{\link[base]{class}} \code{"nnDoi"}, which is a list
  containing the following components:
  
  \item{neighbor.count}{number of times used as a neighbor in kNN classification}
  \item{standardized.residuals}{standardized residuals (the ratios of
  residuals and their standard deviations)}
}

\references{
  McRoberts, R.E. (2009) Diagnostic tools for nearest neighbors
  techniques when used with satellite imagery, \emph{Remote Sensing of
  Environment}. \bold{113}, 489--499.
}

\author{
  Brian Walters \email{bfwalters83@yahoo.com}
}

\seealso{
  \code{\link{grouper}}
}

\examples{
data(LuceVolume)
data(LuceVolume_indx)

##First use grouper to make the \code{"nnDgrps"} object
grps <- grouper(LuceVolume$ref.volume, LuceVolume$pred.vol_k18, LuceVolume_indx)

x <- outInflu(object = grps)
\dontrun{plot(x)}

}

\keyword{misc}