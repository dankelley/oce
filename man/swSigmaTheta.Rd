% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{swSigmaTheta}

\alias{swSigmaTheta}

\title{Seawater potential density anomaly}

\description{Compute \eqn{\sigma_\theta}{sigma-theta}, the potential
  density of seawater, minus 1000 kg/m\eqn{^3}{^3}. }

\usage{swSigmaTheta(salinity, temperature=NULL, pressure=NULL, referencePressure=0,
    longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))}

\arguments{
  \item{salinity}{either practial salinity or an \code{oce} object; see
      \dQuote{Details}.}
  \item{temperature}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C], defined on
    the ITS-90 scale; see \dQuote{Temperature units} in the documentation for
    \code{\link{swRho}}.}
  \item{pressure}{pressure [dbar].}
  \item{referencePressure}{reference pressure [dbar], e.g. 0 for
      \eqn{\sigma_0}{sigma_0}) calculation.}
  \item{longitude}{longitude of observation (only used if \code{eos="gsw"}).}
  \item{latitude}{latitude of observation (only used if \code{eos="gsw"}).}
  \item{eos}{name of equation of state to be used, either \code{"unesco"} or
      \code{"gsw"}.}
}

\details{If the first argument is an \code{oce} object, then salinity, etc.,
    are extracted from it, and used for the calculation instead of any values
    provided in the other arguments.}

\value{Potential density anomaly [kg/m\eqn{^3}{^3}], defined as
    \eqn{\sigma_\theta=\rho(S,\theta(S,t,p),0}{sigma_theta=rho(S,theta(S,t,p),0)}
  - 1000 kg/m\eqn{^3}{^3}.}

\seealso{Related density routines include: \code{\link{swRho}},
    \code{\link{swSigma}}, \code{\link{swSigmaTheta}}, \code{\link{swSigma0}},
    etc.}

\examples{
swSigmaTheta(35, 13, 1000)             # 26.42514 (gsw)
swSigmaTheta(35, 13, 1000, eos="unesco") # 26.4219
}

\references{See citations provided in the \code{\link{swRho}} documentation.}

\author{Dan Kelley}

\keyword{misc}
