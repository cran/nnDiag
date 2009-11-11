\name{aoiMaker}
\Rdversion{1.1}
\alias{aoiMaker}
\title{Create Random Areas of Interest (AOI) From a Larger Image}
\description{
  The purpose of this function is to create Areas of Interest (AOI) from
  a larger image that include a minimum number of reference set
  elements.  The object created can then be used in kNN
  diagnostic tests.
}

\usage{
aoiMaker(ref.coords, num.aoi, image, mask = NULL, aoi.size = 1000, min.points = 25, seed = NULL, verbose = TRUE)
}

\arguments{
  \item{ref.coords}{matrix of reference point coordinates}
  \item{num.aoi}{a single integer, number of AOIs desired}
  \item{image}{the image to make AOIs from, as an object of class \code{\link[=GDALReadOnlyDataset-class]{GDALReadOnlyDataset}}}
  \item{mask}{optional mask image, as an object of class \code{\link[=GDALReadOnlyDataset-class]{GDALReadOnlyDataset}}}
  \item{aoi.size}{a single integer, size of the AOI in pixels}
  \item{min.points}{a single integer, minimum number of reference points in each AOI desired}
  \item{seed}{a single value, interpreted as an integer}
  \item{verbose}{logical indicating whether to display a progress bar}
}

\details{
  The \code{ref.coords} must be a two column matrix with an X and Y
  location.

  Both the \code{image} and \code{mask} have to be in the class
  \code{"GDALReadOnlyDataset"}.  This can be achieved by using the
  command \code{\link[=GDALReadOnlyDataset-class]{GDAL.open}} from the package \pkg{rgdal}.

  Each AOI created by this function is a square with the length of each
  side being the \code{aoi.size} in number of pixels.  For example, if
  the desired AOI size was 300 meters on a side and the \code{image} had
  30 meter pixels the \code{aoi.size} should be 100.

  The minimum number of reference points within each AOI cannot be less
  than 10.  An error will display if \code{min.points} is set less than that.

  The seed option is available so identical AOIs can be made from
  differenct images.  See \code{\link[=.Random.seed]{set.seed}} for more information.
}

\value{
  Returns an object of \code{\link{class}} \code{"nnDaoi"}, which is a list
  containing the following components:

  \item{AOI.spatial}{a list of the AOIs.  Each element in the list is an
    object of class \code{\link[=SpatialGridDataFrame-class]{SpatialGridDataFrame}}}
  \item{AOI.data}{a list of the pixel values contained in each AOI.
    Each element in the list is a matrix.}
  \item{refPoint.index}{a list with the index of reference points
    contained in each AOI.  Each element in the list is a vector.}
  \item{ref.coordinates}{a matrix of the reference point coordinates}
}

\note{
  Plotting an \code{"nnDaoi"} object will display the set of reference
  points and outlines of each AOI.
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
  Functions that use \code{"nnDaoi"} objects: \code{\link{arealBias}}, \code{\link{extrap}}.
}

\examples{
data(LuceVolume_subset)
LuceVolImg <- GDAL.open(system.file("data/LuceSubset_Volume.tif", package = "nnDiag"))
LuceTasscap <- GDAL.open(system.file("data/LuceSubset_Tcap.tif", package = "nnDiag"))
LuceMask <- GDAL.open(system.file("data/LuceSubset_Mask.tif", package = "nnDiag"))

coords <- as.matrix(LuceVolume_subset[,10:11])

vol.aoi <- aoiMaker(coords, 3, LuceVolImg, aoi.size = 20, min.points = 4, seed = 89)
plot(vol.aoi)

tcap.aoi <- aoiMaker(coords, 3, LuceTasscap, LuceMask, aoi.size = 20, min.points = 4, seed = 89)
plot(tcap.aoi)
}

\keyword{misc}
