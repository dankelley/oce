\name{met}

\docType{data}

\alias{met}

\title{Sample meteorological object}

\description{This is sample \code{met} object containing data for Halifax, Nova
    Scotia, during September of 2003 (the period during which Hurricane Juan
    struck the city).}

\usage{data(met)}

\examples{
\dontrun{
library(oce)
data(met)
plot(met)
}
}

\source{Downloaded from the Environment Canada website on May 29, 2014, and processed as follows.
\preformatted{
met <- read.met('http://climate.weather.gc.ca/climateData/bulkdata_e.html?format=csv&stationID=6358&Year=2003&Month=9&Day=17&timeframe=1&submit=Download+Data')
met[['time']] <- met[['time']] + 4 * 3600
}
(Note the conversion from local standard time to UTC.)
}

\seealso{The documentation for \code{\link{met-class}} explains the structure
    of \code{met} objects, and also outlines the other functions dealing with
    them.}

\keyword{datasets}

