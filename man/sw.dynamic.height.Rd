\name{sw.dynamic.height}

\alias{sw.dynamic.height}

\title{Dynamic height of seawater profile}

\description{Compute the dynamic height of a column of seawater.}

\usage{
dh <- sw.dynamic.height(section, pref=2000)
h <- sw.dynamic.height(ctd, pref=2000)
}

\arguments{
  \item{section}{a \code{section} object}
  \item{ctd}{an object of \code{\link[base]{class}} "\code{ctd}".}
  \item{pref}{reference pressure [dbar]}
}

\details{The first form calculates dynamic height for each station
  within a section, and returns a list containing distance along the
  section along with dynamic height.

  The second form calculates dynamic height for a single station, and
  returns just a single value, the dynamic height.

  Stations are analysed in steps. First, a piecewise-linear model of the
  density variation with pressure is developed using
  \code{\link[stats]{approxfun}}. (The option \code{rule=2} is used to
  extrapolate the uppermost density up to the surface, preventing a
  possible a bias for bottle data, in which the first depth may be a few
  metres below the surface.) A second function is constructed as the
  density of water with salinity 35PSU, temperature of
  0\eqn{^\circ}{deg}C, and pressure as in the \code{ctd}. The reciprocal
  difference of these densities is then integrated using
  \code{\link[stats]{integrate}} with pressure limits \code{0} to
  \code{pref}, and divided by the acceleration due to gravity, to
  calculate the dynamic height.

  If the water column is too short to have bottom pressure exceeding
  \code{pref}, a missing value is reported.}

\value{In the first form, a list containing \code{distance}, the
  distance [km] from the first station in the section and \code{height},
  the dynamic height [m].

  In the second form, a single value, containing the dynamic height [m].
}

\examples{
library(oce)
data(a03)

# Dynamic height and geostrophy
par(mfcol=c(2,2))
par(mar=c(4.5,4.5,2,1))

# Left-hand column: whole section
# (The smoothing lowers Gulf Stream speed greatly)
a03.west.to.east <- subset(a03, indices=124:1)
dh <- sw.dynamic.height(a03.west.to.east)
plot(dh$distance, dh$height, type='p', xlab="", ylab="Dyn. Height [m]")
ok <- !is.na(dh$height)
smu <- supsmu(dh$distance, dh$height)
lines(smu, col="blue")
f <- coriolis(a03$data$station[[1]]$metadata$latitude)
g <- gravity(a03$data$station[[1]]$metadata$latitude)
v <- diff(smu$y)/diff(smu$x) * g / f / 1e3 # 1e3 converts to m
plot(smu$x[-1], v, type='l', col="blue", xlab="Distance [km]", ylab="Velocity [m/s]")
abline(h=0)

# Right-hand column: Gulf Stream region, unsmoothed
Gulf.Stream <- subset(a03, indices=124:102)
dh.GS <- sw.dynamic.height(Gulf.Stream)
plot(dh.GS$distance, dh.GS$height, type='b', xlab="", ylab="Dyn. Height [m]")
grid()
v <- diff(dh.GS$height)/diff(dh.GS$distance) * g / f / 1e3
plot(dh.GS$distance[-1], v, type='l', col="blue", xlab="Distance [km]", ylab="Velocity [m/s]")
grid()
abline(h=0)
}

\references{Gill, A.E., 1982. \emph{Atmosphere-ocean Dynamics}, Academic
  Press, New York, 662 pp.}

\author{Dan Kelley}

\keyword{misc}
