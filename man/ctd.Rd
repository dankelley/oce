\name{ctd}

\docType{data}

\alias{ctd}

\title{A CTD profile in Halifax Harbour}

\description{A CTD profile measured in Halifax Harbour in 2003, based on
    \code{\link{ctdRaw}}, but trimmed to just the downcast with
    \code{\link{ctdTrim}}, using indices inferred by inspection of the results
    from \code{\link{plotScan}}.}

\usage{data(ctd)}

\examples{
\dontrun{
library(oce)
data(ctd)
plot(ctd)
}
}

\source{This station was sampled by students enrolled in the Dan Kelley's
    Physical Oceanography class at Dalhousie University.}

\seealso{The documentation for \code{\link{ctd-class}} explains the structure
    of \code{ctd} objects, and also outlines the other functions dealing with
    them.}

\keyword{datasets}

