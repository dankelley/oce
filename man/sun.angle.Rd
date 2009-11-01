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

\value{A data frame containing az, el, soldia, and soldst.}

\examples{
t0 <- as.POSIXct("2008-01-01 00:00:01", tz="UTC")
t1 <- as.POSIXct("2009-01-01 00:00:01", tz="UTC")
t <- seq(from=t0, to=t1, length.out=365)
lat <- rep(0, 365)
lon <- rep(0, 365)
a <- sun.angle(t, lat, lon)
par(mfrow=c(2,1))
plot(t, a$az, type='l', ylab="Azimuth [deg]")
plot(t, a$el, type='l', ylab="Elevation [deg]")
}

\references{Uses on Fortran code retrieved from
ftp://climate1.gsfc.nasa.gov/wiscombe/Solar_Rad/SunAngles/sunae.f
on 2008-11-1.}

\author{Dan Kelley}

\keyword{misc}
