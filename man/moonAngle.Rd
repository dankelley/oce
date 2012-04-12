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

\details{Based on formulae provided by Meeus (1982), chapters 6, 18, and 30.
    The first step is to compute geocentric longitude, geocentric latitude, and
    parallax, using formulae in Meuus (1982) chapter 30.  (Meuss denotes these
    quantities \eqn{lambda}{lambda}, \eqn{beta}{beta}, and \eqn{pi}{pi},
    respectively.) Note that some small terms are skipped in Meuss' formulae,
    but the results match of the Meuss test case to 4 significant digits.  The
    next step is to compute the obliquity of the ecliptic, denoted
    \eqn{epsilon}{epsilon} in Meuss (1982) equation 18.4.  Then, Meuss (1982)
    equations 8.3 and 8.4 are used to calculate equatorial coordinates.
    Finally, the hour angle is computed using the un-numbered equation
    preceeding Meuss (1982) equation 8.1, using the observer longitude and the
    sideral Greenwich time calculated with Meuss (1982) equation 7.2.  Finally,
    Meuss (1982) equations 8.5 and 8.6 are used to calculate the local azimuth
    and altitude of the moon.}

\value{A data frame containing
    \item{azimuth}{azimuth, in degrees eastward of north, from 0 to 360}
    \item{elevation}{elevation, in degrees from -90 to 90}
    \item{diameter}{lunar diameter, in degrees.  (FIXME: using parallax here,
        which is unlikely to be right!)}
    \item{distance}{distance to moon, in kilometers)}
}

\examples{
## Example 1: daily variation in moon's elevation
dayplot <- function(day="2009-12-22", lat=44+39/60, lon=-(63+34/60),
                    location="Halifax, Nova Scotia", add=FALSE, ...)
{
    t0 <- as.POSIXct(paste(day, "00:00:00"), tz="UTC")
    t <- seq(from=t0, to=t0+86400, by="15 min")
    a <- moonAngle(t, lat, lon)
    day <- a$elevation > 0
    a$azimuth[!day] <- NA
    if (add) {
        points(a$azimuth, a$elevation, cex=0.5*par("cex"), ...)
    } else {
        plot(a$azimuth, a$elevation, type='p', xlab='Azimuth',
             ylab='Elevation', ylim=c(0, 90), main=location,
             cex=0.5*par("cex"), ...)
    }
    h <- which(trunc(t,"hours")==t)
    points(a$azimuth[h], a$elevation[h], pch=16, ...)
    hour <- as.POSIXlt(t[h])$hour
    text(a$azimuth[h], a$elevation[h], paste(hour,"h",sep=""), pos=1,
         offset=0.7, cex=0.7*par("cex"), ...)
}
dayplot("2009-06-22")
dayplot("2009-12-22", add=TRUE)
dayplot(trunc(Sys.time(),"days"), add=TRUE, col="red")
}

\references{Meeus, Jean, 1982.  Astronomical formuae for Calculators.
    Willmann-Bell. Richmond VA, USA. 201 pages}

\author{Dan Kelley (based directly on formulae provided by Meeus).}

\keyword{misc}
