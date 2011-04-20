\name{swRho}

\alias{swRho}

\title{Seawater density}

\description{Compute, \eqn{\rho}{rho}, the \emph{in-situ} density of seawater.}

\usage{swRho(salinity, temperature=NULL, pressure=NULL)}

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

  The density is calculated using the UNESCO equation of state for
  seawater, assuming that input variables are defined according to
  modern calibrations.  In conventional Oceanographic notation, the
  calculated quantity is \eqn{\rho(S,t,p)}{rho(S,t,p)} }

\value{\emph{In-situ} density [kg/m\eqn{^3}{^3}].}

\seealso{Related density routines include
\code{\link{swSigma}}, 
\code{\link{swSigmaT}}, and
\code{\link{swSigmaTheta}}. 
}

\examples{ rho <- swRho(35, 13, 1000) # 1030.818}

\references{Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for
 computation of fundamental properties of seawater. \emph{Unesco
 Technical Papers in Marine Science}, \bold{44}, 53 pp

 Gill, A.E., 1982. \emph{Atmosphere-ocean Dynamics}, Academic Press, New
 York, 662 pp.}

\author{Dan Kelley}

\keyword{misc}
