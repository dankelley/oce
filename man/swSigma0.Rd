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

\usage{swSigma0(salinity, temperature, pressure)
swSigma1(salinity, temperature, pressure)
swSigma2(salinity, temperature, pressure)
swSigma3(salinity, temperature, pressure)
swSigma4(salinity, temperature, pressure)
}

\arguments{
  \item{salinity}{either salinity [PSU] (in which case
    \code{temperature} and \code{pressure} must be provided) \strong{or}
    a \code{ctd} object (in which case \code{salinity},
    \code{temperature} and \code{pressure} are determined from the
    object, and must not be provided in the argument list).}
  \item{temperature}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
  \item{pressure}{pressure [dbar]}
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
swSigma0(35, 13, 1000)
swSigma1(35, 13, 1000)
swSigma2(35, 13, 1000)
swSigma3(35, 13, 1000)
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
