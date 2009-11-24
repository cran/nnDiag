\name{LuceForgrp}
\Rdversion{1.1}
\alias{LuceForgrp}
\docType{data}
\title{Forest Inventory Forest Group Type Data on Subplots in Luce County, Michigan}

\description{
  This data was extracted from the freely available Forest Inventory and
  Analysis National Program of the USDA Forest Service Database.  The
  coordinates of each subplot are not exact, they have been fuzzed and
  swapped to meet privacy requirements established by Congress.  See
  pages 10 - 12 in The Forest Inventory and Analysis Database: Database
  Description and Users Manual Version 4.0 for Phase 2, Revision for
  more information on the fuzzing and swapping of plot coordinates.
}

\usage{data(LuceForgrp)}

\format{
  A data frame containing 734 rows the following 4 columns:
  \describe{
    \item{\code{plt_cn}}{the plot identifier}
    \item{\code{subp}}{the subplot identifier}
    \item{\code{ref.forgrp}}{forest group on the subplot}
    \item{\code{pred.forgrp}}{predicted forest group on the subplot for
      k = 12}
  }
}

\source{
  USDA Forest Service Forest Inventory and Analysis National Program
  \url{http://fiatools.fs.fed.us/fiadb-downloads/datamart.html}
}
\references{
  Bechtold, W.A. and Patterson, P.L. (2005) \emph{The Enhanced Forest
  Inventory and Analysis Program - National Sampling Design and
  Estimation Procedures}.
  
  Forest Inventory and Analysis Program (2009) \emph{The Forest
  Inventory and Analysis Database: Database Description and Users
  Manual Version 4.0 for Phase 2, Revision
  1}. \url{http://fia.fs.fed.us/library/database-documentation/draft
  FIADB_user guide_v4-0_p2_05_27_09_pdf.pdf}.
}

\keyword{datasets}
