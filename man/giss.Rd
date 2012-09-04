\name{giss}

\docType{data}

\alias{giss}

\title{Time series of NASA/GISS land-ocean temperature index}

\description{This contains the Goddard Institute for Space Studies (GISS) time
    series of surface temperature anomaly.  The details of the original data
    processing are provided in [2] and [3].  Here, \code{giss} is a list
    containing \code{year}, time in years, and \code{Ta}, temperature anomaly.}
    
\usage{data(giss)}

\examples{
library(oce)
data(giss)
## Compare the graph with Fig3 of Hansen et al. (2010)
plot(giss$year, giss$Ta, type='l', xlab="Year", ylab="Temperature Anomaly")
}

\source{The data were downloaded in summer 2012 from [1] and trimmed to just
    the years 1880 to 2011 during which values are available for each month.
    See the NASA webpage [2] for a description of the data processing done by
    NASA, and [3] for a scientific paper on the matter.}
    
\references{

1. \url{http://data.giss.nasa.gov/gistemp/tabledata_v3/GLB.Ts+dSST.txt}

2. \url{http://data.giss.nasa.gov/gistemp}

3. J. Hansen, R. Ruedy, M. Sato and K. Lo, 2010.  Global surface temperature
change.  Rev. Geophys., 48, RG4004.
(\url{http://pubs.giss.nasa.gov/docs/2010/2010_Hansen_etal.pdf})
    
}

\keyword{datasets}
