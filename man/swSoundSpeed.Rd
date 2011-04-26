\name{swSoundSpeed}

\alias{swSoundSpeed}

\title{Seawater sound speed}

\description{Compute the seawater speed of sound.}

\usage{swSoundSpeed(salinity, temperature=NULL, pressure=NULL)}

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

  The sound speed is calculated using the formulation in section 9 of
  Fofonoff and Millard (1983).}

\value{Sound speed [m/s].}

\examples{
s <- swSoundSpeed(40, 40, 10000) # 1731.995 (p48 of Fofonoff + Millard 1983)
}

\references{Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for
 computation of fundamental properties of seawater. \emph{Unesco
 Technical Papers in Marine Science}, \bold{44}, 53 pp.  (See section
 9.)  }

\author{Dan Kelley}

\keyword{misc}
