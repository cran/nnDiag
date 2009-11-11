\name{arealBias}
\Rdversion{1.1}
\alias{arealBias}
\title{Bias Assessment Using an Areal Approach}
\description{
  Using the AOIs created from \code{\link{aoiMaker}}, this function
  makes an object that can be plotted to assess the areal bias of kNN
  classification.  See McRoberts (2009) for a description of the plot.
}

\usage{
arealBias(object, reference.set)
}

\arguments{
  \item{object}{object of class \code{"\link[=aoiMaker]{nnDaoi}"}}
  \item{reference.set}{vector of reference set observations}
}

\value{
  Returns an object of \code{\link{class}} \code{"nnDarbias"}, which is
  a list containing the following:

  \item{knn.estimates}{vector of the mean values of each AOI}
  \item{probability.estimates}{vector of the mean value of the reference
    points contained within each AOI}
  \item{lower.confidence}{vector of the lower confidence interval values}
  \item{upper.confidence}{vector of the upper confidence intercal values}
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
  \code{\link{aoiMaker}}, \code{\link{bias}}
}

\examples{
data(LuceVolume_subset)
LuceVolImg <- GDAL.open(system.file("data/LuceSubset_Volume.tif", package = "nnDiag"))
LuceMask <- GDAL.open(system.file("data/LuceSubset_Mask.tif", package = "nnDiag"))

## First create \code{"nnDaoi"} object
coords <- as.matrix(LuceVolume_subset[,10:11])
vol.aoi <- aoiMaker(coords, 3, LuceVolImg, LuceMask, aoi.size = 20,
min.points = 4)

ref.vol <- LuceVolume_subset$ref.volume

x <- arealBias(vol.aoi, ref.vol)
plot(x)
}

\keyword{misc}
