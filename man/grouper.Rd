\name{grouper}
\Rdversion{1.1}
\alias{grouper}
\title{Produce ordered groups of data elements for use in diagnostic tests}

\description{
  The purpose of this function is to produce an object that can be used
  in many of the diagnostic tools from this package.  It orders the
  reference set elements with respect to predictions and breaks them
  into groups with an arbitrary number of elements.  Also included in
  the output is the number of times each element was used as a neighbor
  in the kNN classification and the residuals.
}

\usage{
grouper(ref.set, pred.set, nnIndex, group.size = 25, best = TRUE)
}

\arguments{
  \item{ref.set}{vector of observed values.}
  \item{pred.set}{vector of predicted values.}
  \item{nnIndex}{matrix of nearest neighbors index.}
  \item{group.size}{number of elements to be in each group.}
  \item{best}{logical indicating whether the function will use the exact
    \code{group.size} input or if it will find the \dQuote{best} size
    nearest to the \code{group.size} input.}
}

\details{
  The \code{nnIndex} matrix will have a column for each corresponding k
  used in the kNN classification.  Each element in the matrix should be
  in reference to the \code{ref.set} vector position of the observed
  value.

  If the remainder number of data elements do not fill a
  full group, the rest of that group will have \code{NA}s as place
  fillers.  The \code{best} argument is to reduce the number of
  \code{NA}s in the last group.  When \code{best} = \code{TRUE} 
  the function will find the \dQuote{best} group size, which is near the
  \code{group.size} input, that best fits the number of elements in the
  reference set.  If \code{best} is set to \code{FALSE} it will use the
  \code{group.size} input exactly.
}
\value{
  Returns an object of \code{\link[base]{class}} \code{nnDgrps},
  which is a list containing the following components:

  \item{ordered.data}{data frame of the inputed data ordered by
    predictions}
  \item{reference.groups}{matrix of the groups of reference set elements
    where each column is a group.}
  \item{predicted.groups}{matrix of the groups of predicted set elements
    where each column is a group.}
  \item{residual.groups}{matrix of the groups of residuals where each
    column is a group.}
  \item{group.size}{the number of data elements in each group.}
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

##Using the defaults
x <- grouper(ref.set = VOLUME$refVOLUME, pred.set = VOLUME$predVOL_k20,
nnIndex = nnIndex)
x

##Not using the defaults
x <- grouper(ref.set = VOLUME$refVOLUME, pred.set = VOLUME$predVOL_k20,
nnIndex = nnIndex, group.size = 37, best = FALSE)
x
}

\keyword{misc}