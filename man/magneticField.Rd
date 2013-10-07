\name{magneticField}

\alias{magneticField}

\title{Earth magnetic declination, inclination, and intensity}

\description{Earth magnetic declination, inclination, and intensity}

\usage{magneticField(lat, lon, date)}

\arguments{
  \item{lat}{latitude in degrees north, a number, vector, or matrix.}
  \item{lon}{longitude in degrees east (negative for degrees west).  The
    dimensions must conform to lat.}

  \item{date}{either a decimal year or a POSIX time corresponding to the
      \code{lat} and \code{lon} values, or a vector or matrix matching these
      location values.}
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
magneticField(44+39/60, -(63+36/60), 2013)
}

\references{
1. The underlying Fortran code is from http://www.ngdc.noaa.gov/IAGA/vmod/igrf.html

2. A website with a calculator for these fields is at
http://www.geomag.bgs.ac.uk/data_service/models_compass/wmm_calc.html, and it
is from that site that test values (used in checking the Oce package) were
found.

}

\author{Dan Kelley}

\keyword{misc}

\concept{tide}
