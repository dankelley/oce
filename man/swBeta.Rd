\name{swBeta}

\alias{swBeta}

\title{Seawater haline contraction coefficient}

\description{Compute \eqn{\beta}{beta} using McDougall's (1987)
  algorithm. NOTE: this conflicts with base::beta, so be sure to prefix
  as swBeta().}

\usage{swBeta(salinity, temperature, pressure,
       longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))}

\arguments{
  \item{salinity}{either practial salinity (in which case \code{temperature}
      and \code{pressure} must be provided) \strong{or} an \code{oce} object
      (in which case \code{salinity}, etc. are inferred from the object).}
  \item{temperature}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
  \item{pressure}{seawater pressure [dbar]}
  \item{longitude}{longitude of observation (only used if \code{eos="gsw"}; see
      \sQuote{Details}).}
  \item{latitude}{latitude of observation (only used if \code{eos="gsw"}; see
      \sQuote{Details}).}
  \item{eos}{equation of state, either \code{"unesco"} or \code{"gsw"}.}
}

\details{If the first argument is a \code{ctd} object, then salinity,
  temperature and pressure values are extracted from it, and used for
  the calculation.}

\value{Value in 1/psu.}

\examples{
swBeta(40, 10, 4000, eos="unesco")
}

\references{See references at \code{\link{swRho}}.}

\author{Dan Kelley}

\keyword{misc}
