\name{swSigmaTheta}

\alias{swSigmaTheta}

\title{Seawater potential density anomaly}

\description{Compute \eqn{\sigma_\theta}{sigma-theta}, the potential
  density of seawater, minus 1000 kg/m\eqn{^3}{^3}. }

\usage{swSigmaTheta(salinity, temperature=NULL, pressure=NULL, referencePressure=0)}

\arguments{
  \item{salinity}{either salinity [PSU] (in which case
    \code{temperature} and \code{pressure} must be provided) \strong{or}
    a \code{ctd} object (in which case \code{salinity},
    \code{temperature} and \code{pressure} are determined from the
    object, and must not be provided in the argument list).}
  \item{temperature}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
  \item{pressure}{pressure [dbar]}
  \item{referencePressure}{reference pressure [dbar]}
}

\details{If the first argument is a \code{ctd} object, then salinity,
  temperature and pressure values are extracted from it, and used for
  the calculation.

  Definition:
  \eqn{\sigma_\theta=\rho(S,\theta(S,t,p),0}{sigma_theta=rho(S,theta(S,t,p),0)}
  - 1000 kg/m\eqn{^3}{^3}.
}

\value{Potential density anomaly [kg/m\eqn{^3}{^3}].}

\seealso{Related density routines include: \code{\link{swRho}}, 
  \code{\link{swSigmaT}}, and the family of functions for density
  referenced to various pressure values, namely
  \code{\link{swSigma0}},
  \code{\link{swSigma1}},
  \code{\link{swSigma2}},
  \code{\link{swSigma3}}, and
  \code{\link{swSigma0}}.
}

\examples{
sigmaTheta <- swSigmaTheta(35, 13, 1000) #
}

\references{Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for
 computation of fundamental properties of seawater. \emph{Unesco
 Technical Papers in Marine Science}, \bold{44}, 53 pp

 Gill, A.E., 1982. \emph{Atmosphere-ocean Dynamics}, 
 Academic Press, New York, 662 pp.}

\author{Dan Kelley}

\keyword{misc}
