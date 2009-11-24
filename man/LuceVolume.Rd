\name{LuceVolume}
\Rdversion{1.1}
\alias{LuceVolume}
\docType{data}
\title{Forest Inventory Volume Data for Subplots in Luce County, Michigan}
\description{
  This data was extracted from the freely available Forest Inventory and
  Analysis National Program of the USDA Forest Service Database.  The
  coordinates of each subplot are not exact, they have been fuzzed and
  swapped to meet privacy requirements established by Congress.  See
  pages 10 - 12 in The Forest Inventory and Analysis Database: Database
  Description and Users Manual Version 4.0 for Phase 2, Revision for
  more information on the fuzzing and swapping of plot coordinates.
}

\usage{data(LuceVolume)}

\format{
  A data frame containing 671 rows and the following 14 columns:
  \describe{
    \item{\code{plt_cn}}{the plot identifier}
    \item{\code{subp}}{the subplot identifier}
    \item{\code{ref.volume}}{volume on the subplot in cu.ft./acre}
    \item{\code{pred.vol_k1}}{predicted volume on subplot for k = 1}
    \item{\code{pred.vol_k5}}{predicted volume on subplot for k = 5}
    \item{\code{pred.vol_k10}}{predicted volume on subplot for k = 10}
    \item{\code{pred.vol_k15}}{predicted volume on subplot for k = 15}
    \item{\code{pred.vol_k18}}{predicted volume on subplot for k = 18}
    \item{\code{pred.vol_k22}}{predicted volume on subplot for k = 22}
    \item{\code{xAlb}}{x location of the subplot projected in Albers
      Equal Area}
    \item{\code{yAlb}}{y location of the subplot projected in Albers
      Equal Area}
    \item{\code{tcap_B}}{spectral value of Tasseled Cap Transformation brightness}
    \item{\code{tcap_G}}{spectral value of Tasseled Cap Transformation greenness}
    \item{\code{tcap_W}}{spectral value of Tasseled Cap Transformation wetness}
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
