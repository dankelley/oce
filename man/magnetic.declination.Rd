\name{magnetic.declination}

\alias{magnetic.declination}
\title{Earth magnetic declination}

\description{Earth magnetic declination}

\usage{magnetic.declination(lat, lon, date)}

\arguments{
  \item{lat}{latitude in degrees north.}
  \item{lon}{longiitude in degrees east.}
  \item{date}{decimal year}
}

\details{Implements the International geomagnetic reference field,
based on a reworked version of a Fortran program downloaded from a
NOAA website.  The code seems to have been written by Susan Macmillon
of the British Geological Survey.  Comments in the code indicate that
it employs coefficients agreed by the IAGA Working Group V-MOD in
December 2009.  The valid time interval is stated to be 1900.0 to 2015.0,
with values up to 2020, albeit with reduced accuracy.  Values for dates
1945 to 2005.0 are said to be "non-definitative", with other
values in the range being declared "definitative".}

\value{Magnetic declination.}

\examples{
library(oce)
print(magnetic.declination(44+55/60,-(69+46/60),2008)) # -16.80410
}

\references{
  http://www.ngdc.noaa.gov/IAGA/vmod/igrf.html
}

\author{Dan Kelley}

\keyword{misc}

\concept{tide}
