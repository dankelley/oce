\name{sw.specific.heat}

\alias{sw.specific.heat}

\title{Seawater specific heat}

\description{Compute specific heat of seawater.}

\usage{sw.specific.heat(salinity, temperature=NULL, pressure=NULL)}

\arguments{
  \item{salinity}{either salinity [PSU] (in which case
    \code{temperature} and \code{pressure} must be provided) \strong{or}
    a \code{ctd} object (in which case \code{salinity},
    \code{temperature} and \code{pressure} are determined from the
    object, and must not be provided in the argument list).}
  \item{temperature}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
  \item{pressure}{seawater pressure [dbar]}
}

\details{If the first argument is a \code{ctd} object, then salinity,
  temperature and pressure values are extracted from it, and used for
  the calculation.

  Based on matlab code on
  \url{ftp://acourstics.whoi.edu/pub/Matlab/oceans}, which was in turn
  based on Millero et al (1973, 1981).  }

\value{Specific heat \eqn{J kg^{-1}\,^\circ C^{-1}}{J/(kg degC)}}

\examples{
C.P <- sw.specific.heat(40, 40, 10000)	# 3949.500
}

\references{
Millero et. al., J. Geophys. Res. 78 (1973), 4499-4507

Millero et. al., UNESCO report 38 (1981), 99-188.
}

\author{Dan Kelley}

\keyword{misc}
