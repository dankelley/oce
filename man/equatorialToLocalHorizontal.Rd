\name{equatorialToLocalHorizontal}

\alias{equatorialToLocalHorizontal}

\title{Convert from equatorial coordinates to local horizontal coordinates}

\description{Convert from equatorial coordinates to local horizontal
    coordinates, i.e. azimuth and altitude.}

\usage{equatorialToLocalHorizontal(rightAscension, declination, t, latitude, longitude)}

\arguments{
    \item{rightAscension}{right ascension, e.g. calculated with \code{\link{eclipticalToEquatorial}}.}
    \item{declination}{declination, e.g. calculated with \code{\link{eclipticalToEquatorial}}.}
    \item{t}{time of observation.}
    \item{latitude}{latitude of observation, positive in northern hemisphere.}
    \item{longitude}{longitude of observation, positive in eastern hemisphere.}
}

\details{The method is taken from equations 8.5 and 8.6 of [1], or,
    equivalently, from  equations 12.5 and 12.6 of [2].}

\value{A data frame containing columns \code{altitude} (angle above horizon, in
    degrees) and \code{azimuth} (angle anticlockwise from south, in degrees).}

\author{Dan Kelley, based on formulae in [1] and [2].}

\references{
1. Meeus, Jean, 1982. Astronomical formuae for Calculators. Willmann-Bell. Richmond VA, USA. 201 pages.

2. Meeus, Jean, 1991. Astronomical algorithms.  Willmann-Bell, Richmond VA, USA. 429 pages.
}

\keyword{misc}

