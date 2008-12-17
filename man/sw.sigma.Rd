\name{sw.sigma}

\alias{sw.sigma}

\title{Seawater density anomaly}

\description{Compute \eqn{\sigma}{sigma}, the \emph{in-situ} density of
  seawater, minus 1000 kg/m\eqn{^3}{^3}.} 

\usage{sw.sigma(S, t=NULL, p=NULL)}

\arguments{
  \item{S}{either salinity [PSU] (in which case \code{t} and \code{p}
    must be provided) \strong{or} a \code{ctd} object (in which case
    \code{S}, \code{t} and \code{p} are determined from the object, and
    must not be provided in the argument list).}
  \item{t}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
  \item{p}{pressure [dbar]}
}

\details{If the first argument is a \code{ctd} object, then salinity,
  temperature and pressure values are extracted from it, and used for
  the calculation.

  Definition:
  \eqn{\rho(S,t,p)}{rho(S,t,p)}
  - 1000 kg/m\eqn{^3}{^3}.
}

\value{\emph{In-situ} density anomaly [kg/m\eqn{^3}{^3}].}

\seealso{Related density routines include: \code{\link{sw.rho}} to
  calculate in-situ density, \code{\link{sw.sigma.t}} to calculate an
  archaic quantity found in old textbooks, and
  \code{\link{sw.sigma.theta}} to calculate potential density anomaly
  relative to the surface pressure.  If you are thinking of using
  \code{sw.sigma}, you probably want \code{\link{sw.sigma.theta}}.}

\examples{sig <- sw.sigma(35, 13, 1000)	# 30.818}

\references{Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for
 computation of fundamental properties of seawater. \emph{Unesco
 Technical Papers in Marine Science}, \bold{44}, 53 pp

 Gill, A.E., 1982. \emph{Atmosphere-ocean Dynamics}, Academic Press, New
 York, 662 pp.}

\author{Dan Kelley}

\keyword{misc}
