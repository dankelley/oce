\name{swSigma0}

\alias{swSigma0}
\alias{swSigma1}
\alias{swSigma2}
\alias{swSigma3}
\alias{swSigma4}

\title{Seawater potential density anomaly reference to standardized pressures}

\description{Compute \eqn{\sigma_\theta}{sigma}, the potential density of
    seawater (minus 1000 kg/m\eqn{^3}{^3}), referenced to various pressures,
    i.e \code{swSigma0} is referenced to the surface and is thus identical to
    \code{\link{swSigmaTheta}}, while \code{swSigma1} is referenced to
    1000dbar, \code{swSigma2} is referenced to 2000dbar, \code{swSigma2} is
    referenced to 3000dbar, and \code{swSigma4} is referenced to 4000dbar. }

\usage{
swSigma0(salinity, temperature, pressure,
    longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))
swSigma1(salinity, temperature, pressure,
    longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))
swSigma2(salinity, temperature, pressure,
    longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))
swSigma3(salinity, temperature, pressure,
    longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))
swSigma4(salinity, temperature, pressure,
    longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))
}

\arguments{
  \item{salinity}{either practial salinity (in which case \code{temperature}
      and \code{pressure} must be provided) \strong{or} an \code{oce} object
      (in which case \code{salinity}, etc. are inferred from the object).}
  \item{temperature}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C], defined on
    the ITS-90 scale; see \dQuote{Temperature units} in the documentation for
    \code{\link{swRho}}.}
  \item{temperature}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C], defined on
    the ITS-90 scale; see \dQuote{Temperature units} in the documentation for
    \code{\link{swRho}}.}
  \item{pressure}{pressure [dbar]}
  \item{longitude}{longitude of observation (only used if \code{eos="gsw"}).}
  \item{latitude}{latitude of observation (only used if \code{eos="gsw"}).}
  \item{eos}{equation of state, either \code{"unesco"} [1,2] or \code{"gsw"}.}
}

\details{If the first argument is a \code{ctd} object or \code{section} object,
    then salinity, etc., values are extracted from it, and used for the
    calculation.

  Definition:
  \eqn{\sigma_0=\sigma_\theta=\rho(S,\theta(S,t,p),0}{sigma_0=sigma_theta=rho(S,theta(S,t,p),0)}
  - 1000 kg/m\eqn{^3}{^3}.
}

\value{Potential density anomaly [kg/m\eqn{^3}{^3}].}

\seealso{Related density routines include: \code{\link{swSigmaTheta}}, which
    these functions employ, and also \code{\link{swRho}}, and (mostly for
    historical use) \code{\link{swSigmaT}}.  }

\examples{
swSigmaTheta(35, 13, 1000)
swSigma0(35, 13, 1000)
swSigma1(35, 13, 1000)
swSigma2(35, 13, 1000)
swSigma3(35, 13, 1000)
}

\references{See citations provided in the \code{\link{swRho}} documentation.}

\author{Dan Kelley}

\keyword{misc}
