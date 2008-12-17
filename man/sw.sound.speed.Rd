\name{sw.sound.speed}

\alias{sw.sound.speed}

\title{Seawater sound speed}

\description{Compute the seawater speed of sound.}

\usage{sw.sound.speed(S, t=NULL, p=NULL)}

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

  The sound speed is calculated using the formulation in section 9 of
  Fofonoff and Millard (1983).}

\value{Sound speed [m/s].}

\examples{
s <- sw.sound.speed(40, 40, 10000) # 1731.995 (p48 of Fofonoff + Millard 1983)
}

\references{Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for
 computation of fundamental properties of seawater. \emph{Unesco
 Technical Papers in Marine Science}, \bold{44}, 53 pp.  (See section
 9.)  }

\author{Dan Kelley}

\keyword{misc}
