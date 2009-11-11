\name{sun.angle}

\alias{sun.angle}

\title{Solar angle as function of space and time.}

\description{Solar angle as function of space and time.}

\usage{sun.angle(t, lat, lon)}

\arguments{
  \item{t}{Time, a POSIXt object (must be in GMT).}
  \item{lat}{latitude in degrees north}
  \item{lon}{longitude in degrees east}
}

\details{Based on NASA-provided fortran program, in turn (according to
  comments in the code) based on "The Astromincal Almanac".}

\value{A data frame containing
\item{azimuth}{azimuth, in degrees eastward of north, from 0 to 360}
\item{elevation}{elevation, in degrees from -90 to 90}
\item{solar.diameter}{solar diameter, in degrees}
\item{solar.distance}{distance to sun, in astronomical units}}

\examples{
t0 <- as.POSIXct("2008-11-11 00:00:01", tz="UTC")
t1 <- t0 + 86400
t <- seq(from=t0, to=t1, length.out=24*60)
t <- seq(from=t0, to=t1, length.out=1440) # minute by minute
lat <- 44+39/60
lon <- -(63+34/60)
a <- sun.angle(t, lat, lon)
day <- a$el > 0
plot(a$az[day], a$el[day], type='l', xlab="Azimuth", ylab="Elevation")
title("Halifax, Nova Scotia [Rembrance Day 2009]")
}

\references{Based on Fortran code retrieved from
ftp://climate1.gsfc.nasa.gov/wiscombe/Solar_Rad/SunAngles/sunae.f on
2009-11-1.  Comments in that code list as references:

Michalsky, J., 1988: The Astronomical Almanac's algorithm for
approximate solar position (1950-2050), Solar Energy 40, 227-235

The Astronomical Almanac, U.S. Gov't Printing Office, Washington,
D.C. (published every year).

The code comments suggest that the appendix in Michalsky (1988)
contains errors, and delcares the use of the following formulae in the
1995 version the Almanac:
        p. A12: approximation to sunrise/set times;
        p. B61: solar elevation ("altitude") and azimuth;
        p. B62: refraction correction;
        p. C24: mean longitude, mean anomaly, ecliptic longitude,
                obliquity of ecliptic, right ascension, declination,
                Earth-Sun distance, angular diameter of Sun;
        p. L2:  Greenwich mean sidereal time (ignoring T^2, T^3 terms).

The code lists authors as 
Dr. Joe Michalsky
and 
Dr. Lee Harrison (State University of New York), 
with modifications 
by 
Dr. Warren Wiscombe
(NASA Goddard Space Flight Center).
}

\author{Dan Kelley}

\keyword{misc}
