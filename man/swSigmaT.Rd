\name{swSigmaT}

\alias{swSigmaT}

\title{Seawater quasi-potential density anomaly}

\description{Compute \eqn{\sigma_t}{sigma-t}, a rough estimate of
  potential density of seawater, minus 1000 kg/m\eqn{^3}{^3}.}

\usage{swSigmaT(salinity, temperature=NULL, pressure=NULL,
    longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))}

\arguments{
  \item{salinity}{either salinity [PSU] (in which case
    \code{temperature} and \code{pressure} must be provided) \strong{or}
    a \code{ctd} object (in which case \code{salinity},
    \code{temperature} and \code{pressure} are determined from the object, and
    must not be provided in the argument list).}
  \item{temperature}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
  \item{pressure}{pressure [dbar]}
  \item{longitude}{longitude of observation (only used if \code{eos="gsw"}).}
  \item{latitude}{latitude of observation (only used if \code{eos="gsw"}).}
  \item{eos}{name of equation of state to be used, either \code{"unesco"} or
      \code{"gsw"}.}
}

\details{If the first argument is an \code{oce} object, then salinity, etc.,
    are extracted from it, and used for the calculation.}

\value{Quasi-potential density anomaly [kg/m\eqn{^3}{^3}], defined as the
    density calculated with pressure set to zero.}

\seealso{Related density routines include: \code{\link{swRho}},
    \code{\link{swSigma}}, \code{\link{swSigmaTheta}}, \code{\link{swSigma0}},
    etc.}

\examples{
swSigmaT(35, 13, 1000)                 # 26.39682 (gsw)
swSigmaT(35, 13, 1000, eos="unesco")   # 26.39354
}

\references{See citations provided in the \code{\link{swRho}} documentation.}

\author{Dan Kelley}

\keyword{misc}
