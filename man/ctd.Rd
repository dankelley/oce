\name{ctd}

\docType{data}

\alias{ctd}

\title{A CTD profile in Halifax Harbour}

\description{A CTD profile measured in Halifax Harbour in 2003.}

\usage{data(ctd)}

\examples{
\dontrun{
library(oce)
data(ctd)
plot(ctd, which=c(1,2,3,5), coastline="coastlineWorldFine")
}
}

\source{This station was sampled by students enrolled in the Dan Kelley's
    Physical Oceanography class at Dalhousie University.}

\seealso{The documentation for \code{\link{ctd-class}} explains the structure
    of \code{ctd} objects, and also outlines the other functions dealing with
    them.}

\keyword{datasets}

