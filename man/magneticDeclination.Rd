\name{magneticDeclination}

\alias{magneticDeclination}
\title{Earth magneticDeclination}

\description{Earth magneticDeclination}

\usage{magneticDeclination(lat, lon, date)}

\arguments{
  \item{lat}{latitude in degrees north, a number, vector, or matrix.}
  \item{lon}{longitude in degrees east (negative for degrees west).  The
    dimensions must conform to lat.}
  \item{date}{decimal year, either a single number or a vector or matrix
    whose dimensions conform to those of lat.)}
}

\details{Implements the International geomagnetic reference field, based
on a reworked version of a Fortran program downloaded from a NOAA
website.  The code seems to have been written by Susan Macmillon of the
British Geological Survey.  Comments in the code indicate that it
employs coefficients agreed to in December 2009 by the IAGA Working
Group V-MOD.  The code also suggests that the valid time interval is
1900.0 to 2015.0, with values up to 2020 having reduced accuracy.  And,
finally, values for dates 1945 to 2005.0 are said to be
"non-definitative", with other values in the range being declared
"definitative".}

\value{Magnetic declination.}

\examples{
library(oce)
lat <- seq(47.80, 48.00, length.out=30)
lon <- seq(-69.9, -69.6, length.out=30)
lonm <- matrix(rep(lon,length(lat)),ncol=length(lat),byrow=TRUE)
latm <- matrix(rep(lat,length(lon)),nrow=length(lon),byrow=FALSE)
dec <- magneticDeclination(latm, lonm, 2008 + 7 / 12)
data(coastlineSLE)
plot(coastlineSLE)
contour(x=lon, y=lat, z=dec, add=TRUE)
}

\references{
  http://www.ngdc.noaa.gov/IAGA/vmod/igrf.html
}

\author{Dan Kelley}

\keyword{misc}

\concept{tide}
