\name{ctd}

\docType{data}

\alias{ctd}

\title{A CTD profile in Halifax Harbour}

\description{A CTD profile measured in Halifax Harbour in 2003, by students
    enrolled in the author's Physical Oceanography class at Dalhousie
    University.}

\usage{data(ctd)}

\examples{
\dontrun{
library(oce)
data(ctd)
plot(ctd, which=c(1,2,3,5), coastline="coastlineWorldFine")
}
}

\source{This is a station within the \code{\link{section}} dataset, and details
   are provided in the documentation for the latter.}

\seealso{The documentation for \code{ctd-class} in the Oce package explains the
    structure of CTD objects, and also outlines the other functions dealing
    with them.}

\keyword{datasets}
