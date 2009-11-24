\name{extrap}
\Rdversion{1.1}
\alias{extrap}
\title{Extrapolations Test}
\description{
  Comparison of the range of observations in the feature space of both
  the reference set and target set.  Any target set pixel whose value is
  beyond the range of the reference set's feature space requires an
  extrapolation during kNN classification.  This function prepares an
  object to plot the comparison and find if and where an extrapolation
  is necessary.
}

\usage{
extrap(image, refSet.spectral, mask = NULL, verbose = TRUE)
}

\arguments{
  \item{image}{the target set image, either an object of class
    \code{\link[=GDALReadOnlyDataset-class]{GDALReadOnlyDataset}}
    or an object of class \code{"\link[=aoiMaker]{nnDaoi}"}}
  \item{refSet.spectral}{matrix of the feature space values of the reference set}
  \item{mask}{optional mask, as an object of class
    \code{\link[=GDALReadOnlyDataset-class]{GDALReadOnlyDataset}}}
  \item{verbose}{logical indicating whether to display a progress bar}
}

\details{
  Both the \code{image} and \code{mask} have to be in the class
  \code{"GDALReadOnlyDataset"}.  This can be achieved by using the
  command \code{\link[=GDALReadOnlyDataset-class]{GDAL.open}} from the package \pkg{rgdal}.
}

\value{
  Returns an object of \code{\link{class}} \code{"nnDext"}, which is a
  list containing the following components:

  \item{image.range}{a matrix of the range of spectral values found on the image
    or in the AOIs for each spectral layer}
  \item{refSet.range}{a matrix of the range of spectral values for the
    reference set of data elements}
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
  \code{\link{aoiMaker}}
}

\examples{
data(LuceVolume_subset)
LuceTasscap <- GDAL.open(system.file("data/LuceSubset_Tcap.tif", package = "nnDiag"))
LuceMask <- GDAL.open(system.file("data/LuceSubset_Mask.tif", package = "nnDiag"))

spect <- as.matrix(LuceVolume_subset[12:14])
\dontrun{
x <- extrap(LuceTasscap, spect, LuceMask)
plot(x)

## Using an \code{"nnDaoi"} object
coords <- as.matrix(LuceVolume_subset[,10:11])
tcap.aoi <- aoiMaker(coords, 3, LuceTasscap, LuceMask, aoi.size = 20, min.points = 4)

x <- extrap(tcap.aoi, spect)
plot(x)}
}

\keyword{misc}

