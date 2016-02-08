% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{swBeta}

\alias{swBeta}

\title{Seawater haline contraction coefficient}

\description{Compute \eqn{\beta}{beta}, the haline contraction coefficient for
    seawater.}

\usage{swBeta(salinity, temperature=NULL, pressure=0,
       longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))}

\arguments{
  \item{salinity}{either practial salinity (in which case \code{temperature} and
    \code{pressure} must be provided) \strong{or} an \code{oce} object (in which
    case \code{salinity}, etc. are inferred from the object).}
  \item{temperature}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C], defined on
    the ITS-90 scale; see \dQuote{Temperature units} in the documentation for
    \code{\link{swRho}}.}
  \item{pressure}{seawater pressure [dbar]}
  \item{longitude}{longitude of observation (only used if \code{eos="gsw"}; see
    \sQuote{Details}).}
  \item{latitude}{latitude of observation (only used if \code{eos="gsw"}; see
    \sQuote{Details}).}
  \item{eos}{equation of state, either \code{"unesco"} or \code{"gsw"}.}
}

\details{If the first argument is a \code{ctd} object, then salinity,
  temperature and pressure values are extracted from it, and used for the
  calculation.}

\value{Value in 1/psu.}

\references{The \code{eos="unesco"} formulae are based on the UNESCO equation of
  state, but are formulaed empirically by Trevor J. McDougall, 1987, Neutral
  Surfaces, Journal of Physical Oceanography, volume 17, pages 1950-1964. The
  \code{eos="gsw"} formulae come from GSW; see references in the
  \code{\link{swRho}} documentation.}

\author{Dan Kelley}

\keyword{misc}
