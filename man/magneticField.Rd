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

\details{Implements the 12th generation International Geomagnetic Reference
    Field (IGRF), based on a reworked version of a Fortran program downloaded
    from a NOAA website [1,2].  The code (subroutine \code{igrf12syn}) seems to
    have been written by Susan Macmillan of the British Geological Survey.
    Comments in the code indicate that it employs coefficients agreed to in
    December 2014 by the IAGA Working Group V-MOD.  Comments in the
    \code{igrf12syn} source code also suggest that the valid time interval is
    from years 1900 to 2020, with only the values from 1945 to 2010 being
    considered definitive.}

\value{A list containing \code{declination}, \code{inclination}, and
    \code{intensity}.}

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

1. The underlying Fortran code is from \code{igrf12.f}, downloaded the NOAA
website (\url{http://www.ngdc.noaa.gov/IAGA/vmod/igrf.html}) on June 7, 2015.

}

\author{Dan Kelley wrote the R code and a fortran wrapper to the
    \code{igrf12.f} subroutine, which was written by Susan Macmillan of the
    British Geological Survey and distributed ``without limitation'' (email
    from SM to DK dated June 5, 2015).}

\keyword{misc}

\concept{tide}

