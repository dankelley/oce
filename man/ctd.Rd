\name{ctd}

\docType{data}

\alias{ctd}

\title{Seawater CTD profile}

\description{This is sample CTD profile provided for testing, created by
  the code given in \dQuote{Examples}.}

\usage{data(ctd)}

\examples{
library(oce)
data(ctd)
plot(ctd, which=c(1,2,3,5), coastline="coastlineMaritimes")
}

\source{This is a station within the \code{\link{section}} dataset, and details
   are provided in the documentation for the latter.}

\seealso{The documentation for \code{\link{ctd-class}} explains the structure
    of CTD objects, and also outlines the other functions dealing with them.}

\keyword{datasets}
