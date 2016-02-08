\name{sealevel}

\docType{data}

\alias{sealevel}

\title{Sea-level data set, from Halifax Harbour}

\description{This sample sea-level dataset is the 2003 record from Halifax
    Harbour in Nova Scotia, Canada.  For reasons that are not mentioned on the
    data archive website, the record ends on the 8th of October.}

\usage{data(sealevel)}

\source{The data were created as
    \preformatted{
sealevel <- read.oce("490-01-JAN-2003_slev.csv")
sealevel <- oce.edit(sealevel, "longitude", -sealevel[["longitude"]],
                     reason="Fix longitude hemisphere")
}
where the csv file was downloaded from [1]. Note the correction of longitude
sign, which is required because the data file has no indication that 
this is the western hemisphere.}

\references{1. The data archive is at
    \url{http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/sd-ds-eng.asp?user=isdm-gdsi&region=ATL&tst=1&no=490}
    (link checked April 15, 2015).}

\seealso{The documentation for \code{\link{sealevel-class}} explains the
    structure of sealevel objects, and also outlines the other functions
    dealing with them.  Data for Tuktoyaktuk are stored in
    \code{\link{sealevelTuktoyaktuk}}.  See \code{\link{plot.sealevel}} for an
    example, showing the effect of Hurrican Juan.}

\author{Dan Kelley}

\keyword{datasets}

