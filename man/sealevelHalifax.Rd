\name{sealevelHalifax}

\docType{data}

\alias{sealevelHalifax}

\title{Sea-level data set, from Halifax Harbour}

\description{This sample sea-level dataset is the 2003 record from Halifax
    Harbour in Nova Scotia, Canada. Note that Hurricane Juan hit Halifax a half
    hour before midnight on September 28th of 2003, causing a strong storm
    surge; see example at \code{\link{plot.sealevel}}.  For reasons that are
    not mentioned on the data archive website, the record ends on the 8th of
    October.}

\usage{data(sealevelHalifax)}

\source{The data were downloaded from MEDS, and treated as follows.
   \preformatted{
sealevelHalifax <- read.oce("490-01-jan-2003_slev.csv")
sealevelHalifax[["longitude"]] <- -sealevelHalifax[["longitude"]]
save(sealevelHalifax, file="data/sealevelHalifax.rda")
   }}

\references{The MEDS archive is at
  \url{http://www.meds-sdmm.dfo-mpo.gc.ca/meds/Databases/TWL/TWL_inventory_e.htm}.
  An entry to the literature about Hurricane Juan is at
  \url{http://www.atl.ec.gc.ca/weather/hurricane/juan/}.}

\examples{
data(sealevelHalifax)
plot(sealevelHalifax)
plot(sealevelHalifax, which=1, xlim=as.POSIXct(c("2003-01-01","2003-02-01")))
}

\seealso{The documentation for \code{\link{sealevel-class}} explains the
    structure of sealevel objects, and also outlines the other functions
    dealing with them.}

\author{Dan Kelley}

\keyword{datasets}
