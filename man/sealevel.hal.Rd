\name{sealevel.hal}

\docType{data}

\alias{sealevel.hal}

\title{Sea-level data set, from Halifax Harbour}

\description{This sample sea-level dataset is the 2003 record from
  Halifax Harbour in Nova Scotia, Canada. Note that Hurricane Juan hit
  Halifax a half hour before midnight on September 28th of 2003, 
  causing a strong storm surge; see example at \code{\link{plot.sealevel}}.
}

\usage{data(sealevel.hal)}

\source{The data were downloaded from MEDS and then read as follows
\preformatted{sealevel.hal <- read.oce("490-01-jan-2003_slev.csv")}}

\references{The MEDS archive is at
  \url{http://www.meds-sdmm.dfo-mpo.gc.ca/meds/Databases/TWL/TWL_inventory_e.htm}.
  An entry to the literature about Hurricane Juan is at
  \url{http://www.atl.ec.gc.ca/weather/hurricane/juan/}.}

\author{Dan Kelley}

\keyword{misc}
