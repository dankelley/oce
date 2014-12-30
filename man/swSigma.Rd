\name{swSigma}

\alias{swSigma}

\title{Seawater potential density anomaly}

\description{Compute \eqn{\sigma_\theta}{sigma}, the potential density of
    seawater (minus 1000 kg/m\eqn{^3}{^3}), referenced to various pressures,
    i.e \code{swSigma0} is referenced to the surface and is thus identical to
    \code{\link{swSigmaTheta}}, while \code{swSigma1} is referenced to
    1000dbar, \code{swSigma2} is referenced to 2000dbar, \code{swSigma2} is
    referenced to 3000dbar, and \code{swSigma4} is referenced to 4000dbar. }

\usage{swSigma(salinity, temperature=NULL, pressure=NULL,
    longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))}

\arguments{
  \item{salinity}{either salinity [PSU] (in which case
    \code{temperature} and \code{pressure} must be provided) \strong{or}
    a \code{ctd} object (in which case \code{salinity},
    \code{temperature} and \code{pressure} are determined from the
    object, and must not be provided in the argument list).}
  \item{temperature}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
  \item{pressure}{pressure [dbar]}
  \item{longitude}{longitude of observation (only used if \code{eos="gsw"}).}
  \item{latitude}{latitude of observation (only used if \code{eos="gsw"}).}
  \item{eos}{name of equation of state to be used, either \code{"unesco"} or
      \code{"gsw"}.}
}

\details{If the first argument is a \code{ctd} object, then salinity,
  temperature and pressure values are extracted from it, and used for
  the calculation.

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
}

\references{Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for
 computation of fundamental properties of seawater. \emph{Unesco
 Technical Papers in Marine Science}, \bold{44}, 53 pp

 Gill, A.E., 1982. \emph{Atmosphere-ocean Dynamics}, 
 Academic Press, New York, 662 pp.

 FIXME-gsw: this needs to have an option to use gsw (which is going to require lat and lon args)

}

\author{Dan Kelley}

\keyword{misc}
