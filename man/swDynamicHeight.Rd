\name{swDynamicHeight}

\alias{swDynamicHeight}

\title{Dynamic height of seawater profile}

\description{Compute the dynamic height of a column of seawater.}

\usage{swDynamicHeight(x, referencePressure=2000)}

\arguments{
  \item{x}{a \code{section} object, \strong{or} a \code{ctd} object.}
  \item{referencePressure}{reference pressure [dbar]}
}

\details{If the first argument is a \code{section}, then dynamic height
  is calculated for each station within a section, and returns a list
  containing distance along the section along with dynamic height.

  If the first argument is a \code{ctd}, then this returns just a single
  value, the dynamic height.

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
  \code{referencePressure}, and divided by the acceleration due to gravity, to
  calculate the dynamic height.

  If the water column is too short to have bottom pressure exceeding
  \code{referencePressure}, a missing value is reported.}

\value{In the first form, a list containing \code{distance}, the
  distance [km] from the first station in the section and \code{height},
  the dynamic height [m].

  In the second form, a single value, containing the dynamic height [m].
}

\examples{
    \dontrun{
library(oce)
data(a03)

# Dynamic height and geostrophy
par(mfcol=c(2,2))
par(mar=c(4.5,4.5,2,1))

# Left-hand column: whole section
# (The smoothing lowers Gulf Stream speed greatly)
a03WestToEast <- subset(a03, 1<=stationId&stationId<=123)
dh <- swDynamicHeight(a03WestToEast)
plot(dh$distance, dh$height, type='p', xlab="", ylab="dyn. height [m]")
ok <- !is.na(dh$height)
smu <- supsmu(dh$distance, dh$height)
lines(smu, col="blue")
f <- coriolis(a03[["station", 1]][["latitude"]])
g <- gravity(a03[["station", 1]][["latitude"]])
v <- diff(smu$y)/diff(smu$x) * g / f / 1e3 # 1e3 converts to m
plot(smu$x[-1], v, type='l', col="blue", xlab="distance [km]", ylab="velocity [m/s]")

# right-hand column: gulf stream region, unsmoothed
gs <- subset(a03, 102<=stationId&stationId<=124)
dh.gs <- swDynamicHeight(gs)
plot(dh.gs$distance, dh.gs$height, type='b', xlab="", ylab="dyn. height [m]")
v <- diff(dh.gs$height)/diff(dh.gs$distance) * g / f / 1e3
plot(dh.gs$distance[-1], v, type='l', col="blue",
  xlab="distance [km]", ylab="velocity [m/s]")
}
}

\references{Gill, A.E., 1982. \emph{Atmosphere-ocean Dynamics}, Academic
  Press, New York, 662 pp.}

\author{Dan Kelley}

\keyword{misc}
