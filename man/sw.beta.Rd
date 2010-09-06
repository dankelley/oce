\name{sw.beta}

\alias{sw.beta}

\title{Seawater haline contraction coefficient}

\description{Compute \eqn{\beta}{beta} using McDougall's (1987)
  algorithm. NOTE: this conflicts with base::beta, so be sure to prefix
  as sw.beta().}

\usage{sw.beta(salinity, temperature, pressure, is.theta=FALSE)}

\arguments{
  \item{salinity}{either salinity [PSU] (in which case
    \code{temperature} and \code{pressure} must be provided) \strong{or}
    a \code{ctd} object (in which case \code{salinity},
    \code{temperature} and \code{pressure} are determined from the
    object, and must not be provided in the argument list).}
  \item{temperature}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
  \item{pressure}{seawater pressure [dbar]}
  \item{is.theta}{Set TRUE if temperature is theta or FALSE if temperature is in-situ}
}

\details{If the first argument is a \code{ctd} object, then salinity,
  temperature and pressure values are extracted from it, and used for
  the calculation.}

\value{Value in 1/psu.}

\examples{
sw.beta(40, 10, 4000, is.theta=TRUE)  # 0.72088e-3 (from p1964 of McDougall 1987)
}

\references{McDougall, T.J. 1987. "Neutral Surfaces" Journal of Physical
  Oceanography vol 17 pages 1950-1964}

\author{Dan Kelley}

\keyword{misc}
