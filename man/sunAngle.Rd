\name{sunAngle}

\alias{sunAngle}

\title{Solar angle as function of space and time.}

\description{Solar angle as function of space and time.}

\usage{sunAngle(t, lat, lon, use.refraction=TRUE)}

\arguments{
    \item{t}{Time, a POSIXt object (must be in timezone UTC), or a numeric value that corresponds to
        such a time.}
    \item{lat}{latitude in degrees north}
    \item{lon}{longitude in degrees east}
    \item{use.refraction}{boolean, set to \code{TRUE} to apply a correction for atmospheric
        refraction}
}

\details{Based on NASA-provided fortran program, in turn (according to
    comments in the code) based on "The Astromincal Almanac".}

\value{A data frame containing
    \item{azimuth}{azimuth, in degrees eastward of north, from 0 to 360}
    \item{elevation}{elevation, in degrees from -90 to 90}
    \item{solar.diameter}{solar diameter, in degrees}
    \item{solar.distance}{distance to sun, in astronomical units}
}

\examples{
## Example 1: daily variation in sun elevation
dayplot <- function(day="2009-12-22", lat=44+39/60, lon=-(63+34/60),
                    location="Halifax, Nova Scotia", add=FALSE, ...)
{
    t0 <- as.POSIXct(paste(day, "00:00:00"), tz="UTC")
    t <- seq(from=t0, to=t0+86400, by="15 min")
    a <- sunAngle(t, lat, lon)
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

## Example 2: infer location from sunrise and sunset times
rise <- as.POSIXct("2011-03-03 06:49:00", tz="UTC") + 4*3600
set <- as.POSIXct("2011-03-03 18:04:00", tz="UTC") + 4*3600
mismatch <- function(latlon) 
{
    sunAngle(rise, latlon[1], latlon[2])$elevation^2 +
    sunAngle(set, latlon[1], latlon[2])$elevation^2
}
result <- optim(c(1,1), mismatch)
lat.hfx <- 44.65
lon.hfx <- (-63.55274)
dist <- geodDist(result$par[1], result$par[2], lat.hfx, lon.hfx)
cat(sprintf("Infer Halifax latitude \%.2f and longitude \%.2f; distance mismatch \%.0f km", 
            result$par[1], result$par[2], dist))
}

\references{Based on Fortran code retrieved from
    ftp://climate1.gsfc.nasa.gov/wiscombe/Solar_Rad/SunAngles/sunae.f on
    2009-11-1.  Comments in that code list as references:

    Michalsky, J., 1988: The Astronomical Almanac's algorithm for
    approximate solar position (1950-2050), Solar Energy 40, 227-235

    The Astronomical Almanac, U.S. Gov't Printing Office, Washington,
    D.C. (published every year).

    The code comments suggest that the appendix in Michalsky (1988)
    contains errors, and declares the use of the following formulae in the
    1995 version the Almanac:
    \itemize{
        \item        p. A12: approximation to sunrise/set times;
        \item        p. B61: solar elevation ("altitude") and azimuth;
        \item        p. B62: refraction correction;
        \item        p. C24: mean longitude, mean anomaly, ecliptic longitude,
        obliquity of ecliptic, right ascension, declination,
        Earth-Sun distance, angular diameter of Sun;
        \item        p. L2:  Greenwich mean sidereal time (ignoring T^2, T^3 terms).
    }

    The code lists authors as 
    Dr. Joe Michalsky
    and 
    Dr. Lee Harrison (State University of New York), 
    with modifications by 
    Dr. Warren Wiscombe (NASA Goddard Space Flight Center).
}

\author{Dan Kelley}

\keyword{misc}
