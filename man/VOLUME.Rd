\name{VOLUME}
\Rdversion{1.1}
\alias{VOLUME}
\docType{data}
\title{Forest Inventory Volume Data for Subplots in Luce County, Michigan}
\description{
  This data was extracted from the freely available Forest Inventory and
  Analysis National Program of the USDA Forest Service Database.
}

\usage{data(VOLUME)}

\format{
  A data frame containing 671 rows and the following 10 columns:
  \describe{
    \item{\code{plt_cn}}{the plot identifier}
    \item{\code{subp}}{the subplot identifier}
    \item{\code{refVOLUME}}{volume on the subplot in cu.ft./acre}
    \item{\code{predVOL_k1}}{predicted volume on subplot for k = 1}
    \item{\code{predVOL_k5}}{predicted volume on subplot for k = 5}
    \item{\code{predVOL_k10}}{predicted volume on subplot for k = 10}
    \item{\code{predVOL_k15}}{predicted volume on subplot for k = 15}
    \item{\code{predVOL_k20}}{predicted volume on subplot for k = 20}
    \item{\code{predVOL_k25}}{predicted volume on subplot for k = 25}
    \item{\code{predVOL_k30}}{predicted volume on subplot for k = 30}
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
