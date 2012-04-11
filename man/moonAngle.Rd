\name{moonAngle}

\alias{moonAngle}

\title{Lunar angle as function of space and time.}

\description{Lunar angle as function of space and time.}

\usage{moonAngle(t, lat, lon, useRefraction=TRUE)}

\arguments{
    \item{t}{time, a POSIXt object (must be in timezone UTC), or a numeric
        value that corresponds to such a time.}
    \item{lat}{latitude in degrees north}
    \item{lon}{longitude in degrees east}
    \item{useRefraction}{boolean, set to \code{TRUE} to apply a correction for atmospheric
        refraction}
}

\details{Based on formulae provided by Meeus (1982 chapter 30).}

\value{A data frame containing
    \item{azimuth}{azimuth, in degrees eastward of north, from 0 to 360}
    \item{elevation}{elevation, in degrees from -90 to 90}
    \item{diameter}{lunar diameter, in degrees (FIXME: not provided yet)}
    \item{distance}{distance to moon, in astronomical units (FIXME: not provided yet)}
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
