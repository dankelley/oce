\name{eclipticalToEquatorial}

\alias{eclipticalToEquatorial}

\title{Convert from ecliptical to equatorial coordinates}

\description{Convert from ecliptical to equatorial coordinates}

\usage{eclipticalToEquatorial(lambda, beta, epsilon)}

\arguments{
    \item{lambda}{longitude, in degrees, or a data frame containing
        \code{lambda}, \code{beta}, and \code{epsilon}, in which case the next
        to arguments are ignored.}
    \item{beta}{geocentric latitude, in degrees}
    \item{epsilon}{obliquity of the ecliptic, in degrees}
}

\details{The method is taken from equations 8.3 and 8.4 of [1], or,
    equivalently, from  equations 12.3 and 12.4 of [2].}

\value{A data frame containing columns \code{rightAscension} and
    \code{declination} both in degrees.}

\author{Dan Kelley, based on formulae in [1] and [2].}

\references{
1. Meeus, Jean, 1982. Astronomical formuae for Calculators. Willmann-Bell. Richmond VA, USA. 201 pages.

2. Meeus, Jean, 1991. Astronomical algorithms.  Willmann-Bell, Richmond VA, USA. 429 pages.
}

\keyword{misc}

