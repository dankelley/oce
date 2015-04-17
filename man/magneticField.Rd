\name{magneticField}

\alias{magneticField}

\title{Earth magnetic declination, inclination, and intensity}

\description{Earth magnetic declination, inclination, and intensity}

\usage{magneticField(longitude, latitude, time)}

\arguments{

  \item{longitude}{longitude in degrees east (negative for degrees west).  The
      dimensions must conform to lat.}

  \item{latitude}{latitude in degrees north, a number, vector, or matrix.}

  \item{time}{either a decimal year or a POSIX time corresponding to the
      \code{longitude} and \code{latitude} values, or a vector or matrix
      matching these location values.}
}

\details{Implements the International geomagnetic reference field, based
on a reworked version of a Fortran program downloaded from a NOAA
website.  The code seems to have been written by Susan Macmillon of the
British Geological Survey.  Comments in the code indicate that it
employs coefficients agreed to in December 2009 by the IAGA Working
Group V-MOD.  The code also suggests that the valid time interval is
1900.0 to 2015.0, with values up to 2020 having reduced accuracy.  And,
finally, values for dates 1945 to 2005.0 are said to be
``non-definitative,'' with other values in the range being declared
``definitative.''}

\value{A list containing \code{declination}, \code{inclination}, and
    \code{intensity}.}

\section{Caution}{This code has not been adequately checked.  The hope is to
    get some independent test values sometime in Oct 2013.}

\examples{
library(oce)
# Halifax NS
magneticField(-(63+36/60), 44+39/60, 2013)

## map of North American values
data(coastlineWorld)
mapPlot(coastlineWorld, longitudelim=c(-130,-55), latitudelim=c(35,60),
        projection="+proj=lcc +lat_0=20 +lat_1=60 +lon_0=-100")
lon <- seq(-180, 180, 1)
lat <- seq(-90, 90)
lonm <- rep(lon, each=length(lat))
latm <- rep(lat, times=length(lon))
## Note the counter-intuitive nrow argument
decl <- matrix(magneticField(lonm, latm, 2013)$declination,
               nrow=length(lon), byrow=TRUE)
mapContour(lon, lat, decl, col='red', levels=seq(-90, 90, 5))
incl <- matrix(magneticField(lonm, latm, 2013)$inclination,
               nrow=length(lon), byrow=TRUE)
mapContour(lon, lat, incl, col='blue', levels=seq(-90, 90, 5))
}

\references{

1. The underlying Fortran code is from a NOAA website
(\url{http://www.ngdc.noaa.gov/IAGA/vmod/igrf.html}).

2. A website with a calculator for these fields is at
\url{http://www.geomag.bgs.ac.uk/data_service/models_compass/wmm_calc.html},
and, as of October 7 2013, this site produces values that agree with those
calculated by the present function to three digits after the decimal.

}

\author{Dan Kelley}

\keyword{misc}

\concept{tide}

