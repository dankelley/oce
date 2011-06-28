\name{swAlphaOverBeta}

\alias{swAlphaOverBeta}

\title{Ratio of seawater thermal expansion coefficient to haline contraction coefficient}

\description{Compute \eqn{\alpha/\beta}{alpha/beta} using McDougall's (1987) algorithm.}

\usage{swAlphaOverBeta(salinity, temperature=NULL, pressure=NULL,
                   isTheta=FALSE)}

\arguments{
  \item{salinity}{either salinity [PSU] (in which case
    \code{temperature} and \code{pressure} must be provided) \strong{or}
    a \code{ctd} object (in which case \code{salinity},
    \code{temperature} and \code{pressure} are determined from the
    object, and must not be provided in the argument list).}
  \item{temperature}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
  \item{pressure}{pressure [dbar]}
  \item{isTheta}{Set TRUE if the temperature a potential temperature, FALSE otherwise.}
}

\details{If the first argument is a \code{ctd} object, then salinity,
  temperature and pressure values are extracted from it, and used for
  the calculation.}

\value{Value in psu/\eqn{^\circ}{deg}C.}

\examples{
 # 0.3476 (from p1964 of McDougall 1987)
 ab <- swAlphaOverBeta(40, 10, 4000, isTheta=TRUE) 
}

\references{McDougall, T.J. 1987. "Neutral Surfaces" Journal of Physical
  Oceanography vol 17 pages 1950-1964}

\author{Dan Kelley}

\keyword{misc}
