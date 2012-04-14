\name{moonAngle}

\alias{moonAngle}

\title{Lunar angle as function of space and time.}

\description{Lunar angle as function of space and time.}

\usage{moonAngle(t, latitude, longitude, useRefraction=TRUE)}

\arguments{
    \item{t}{time, a POSIXt object (must be in timezone UTC), or a numeric
        value that corresponds to such a time.}
    \item{latitude}{observer latitude in degrees north}
    \item{longitude}{observer longitude in degrees east}
    \item{useRefraction}{boolean, set to \code{TRUE} to apply a correction for atmospheric
        refraction.  (Ignored at present.)}
}

\details{Based on formulae provided by Meeus [1982], chapters 6, 18, and 30.
    (Similar formulae appear in Meeus [1991].) The first step is to compute
    geocentric longitude, geocentric latitude, and parallax, using formulae in
    Meeus [1982] chapter 30.  (Meeus denotes these quantities
    \eqn{lambda}{lambda}, \eqn{beta}{beta}, and \eqn{pi}{pi}, respectively.)
    Note that some small terms are skipped in the formulae provided by Meeus,
    but the results match of a test case to 4 significant digits.  The next
    step is to compute the obliquity of the ecliptic, denoted
    \eqn{epsilon}{epsilon} in Meeus [1982] equation 18.4.  Then, equations 8.3
    and 8.4 from Meeus [1982] are used to calculate equatorial coordinates.
    Finally, the hour angle is computed using the un-numbered equation
    preceeding Meeus's [1982] equation 8.1, using the observer longitude and
    the sideral Greenwich time calculated with Meeus [1982] equation 7.2.
    Finally, Meeus [1982] equations 8.5 and 8.6 are used to calculate the local
    azimuth and altitude of the moon.}

\value{A data frame containing the following quantities
    \item{azimuth}{azimuth, in degrees eastward of north, from 0 to 360}
    \item{altitude}{ialtitude, in degrees from -90 to 90}
    \item{diameter}{lunar diameter, in degrees.  (FIXME: using parallax here,
        which is unlikely to be right!)}
    \item{distance}{distance to moon, in kilometers)}
    \item{illuminatedFraction}{fraction of moon that is illuminated}
}

\examples{
library(oce)
par(mfrow=c(3,2))
y <- 2012
m <- 4
days <- 1:3
## Halifax sunrise/sunset (see e.g. http://www.timeanddate.com/worldclock)
rises <- ISOdatetime(y, m, days,c(13,15,16), c(55, 04, 16),0,tz="UTC") + 3 * 3600 # ADT
sets <- ISOdatetime(y, m,days,c(3,4,4), c(42, 15, 45),0,tz="UTC") + 3 * 3600
azrises <- c(69, 75, 82)
azsets <- c(293, 288, 281)
latitude <- 44.65
longitude <- -63.6
for (i in 1:3) {
    t <- ISOdatetime(y, m, days[i],0,0,0,tz="UTC") + seq(0, 24*3600, 3600/4)
    ma <- moonAngle(t, latitude, longitude)
    oce.plot.ts(t, ma$altitude, type='l', mar=c(2, 3, 1, 1), cex=1/2, ylab="Altitude")
    abline(h=0)
    points(rises[i], 0, col='red', pch=3, lwd=2, cex=1.5)
    points(sets[i], 0, col='blue', pch=3, lwd=2, cex=1.5)
    oce.plot.ts(t, ma$azimuth, type='l', mar=c(2, 3, 1, 1), cex=1/2, ylab="Azimuth")
    points(rises[i], -180+azrises[i], col='red', pch=3, lwd=2, cex=1.5)
    points(sets[i], -180+azsets[i], col='blue', pch=3, lwd=2, cex=1.5)
}
}

\references{Meeus, Jean, 1982.  Astronomical formulae for calculators.
    Willmann-Bell. Richmond VA, USA. 201 pages.

Meeus, Jean, 1991. Astronomical algorithms.  Willmann-Bell, Richmond VA, USA. 429 pages.}

\author{Dan Kelley (based directly on formulae provided by Meeus).}

\keyword{misc}
