\name{met}

\docType{data}

\alias{met}

\title{Sample meteorological object}

\description{This is sample \code{met} object, provided for testing, created by
    the code given in \dQuote{Examples}.}

\usage{data(met)}

\examples{
library(oce)
data(met)
plot(met)
}

\source{Environment Canada website (downloaded 2011-11-17)
\preformatted{met<-read.met("http://climate.weatheroffice.gc.ca/climateData/bulkdata_e.html?timeframe=1&Prov=XX&StationID=6358&Year=2011&Month=7&Day=15&format=csv")}
}


\seealso{The documentation for \code{\link{met-class}} explains the structure
    of \code{met} objects, and also outlines the other functions dealing with them.}

\keyword{datasets}
