% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{swSpecificHeat}

\alias{swSpecificHeat}

\title{Seawater specific heat}

\description{Compute specific heat of seawater.}

\usage{swSpecificHeat(salinity, temperature=NULL, pressure=0,
    longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))}

\arguments{
  \item{salinity}{either practial salinity (in which case \code{temperature} and
    \code{pressure} must be provided) \strong{or} an \code{oce} object (in which
    case \code{salinity}, etc. are inferred from the object).}
  \item{temperature}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C], defined on
    the ITS-90 scale.}
  \item{pressure}{seawater pressure [dbar]}
  \item{longitude}{longitude of observation (only used if \code{eos="gsw"}; see
    \sQuote{Details}).}
  \item{latitude}{latitude of observation (only used if \code{eos="gsw"}; see
    \sQuote{Details}).}
  \item{eos}{equation of state, either \code{"unesco"} or \code{"gsw"}.}
}

\details{If the first argument is a \code{ctd} object, then salinity, etc, are
  extracted from it, and used for the calculation.}

\value{Specific heat \eqn{J kg^{-1}\,^\circ C^{-1}}{J/(kg degC)}}

\examples{
swSpecificHeat(40, T90fromT68(40), 10000, eos="unesco") # 3949.499
}

\references{
Millero et. al., J. Geophys. Res. 78 (1973), 4499-4507

Millero et. al., UNESCO report 38 (1981), 99-188.
}

\author{Dan Kelley}

\keyword{misc}
