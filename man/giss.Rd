\name{giss}

\docType{data}

\alias{giss}

\title{Time series of NASA/GISS land-ocean temperature index}

\description{This contains the NASA/GISS time series of monthly values of the
    global mean of surface temperature, in the form of a list containing
    \code{y}, the fractional year, and \code{Ta}, the temperature anomaly.
    Hansen et al. (2010) suggest that \code{Ta} can be converted to actual
    temperature by adding 14C, with uncertainty of ``several tenths of a degree
    Celcius.''}
    
\usage{data(giss)}

\examples{
library(oce)
data(giss)
## Compare the graph with Fig3 of Hansen et al. (2010)
plot(giss$y, giss$Ta, type='l', xlab="Year", ylab="Temperature Anomaly")
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
