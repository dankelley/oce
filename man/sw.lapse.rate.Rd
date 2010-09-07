\name{sw.lapse.rate}

\alias{sw.lapse.rate}

\title{Seawater lapse rate}

\description{Compute adiabatic lapse rate}

\usage{sw.lapse.rate(salinity, temperature=NULL, pressure=NULL)}

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

  Compute lapse rate using Fofonoff and Millard's (1983) formula.
}

\value{Lapse rate [\eqn{deg}{deg}C/m].}

\examples{
 lr <- sw.lapse.rate(40, 40, 10000) # 3.255976e-4
}

\references{Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for
 computation of fundamental properties of seawater. \emph{Unesco
 Technical Papers in Marine Science}, \bold{44}, 53 pp.  (Section 7,
 pages 38-40) }
\author{Dan Kelley}

\keyword{misc}
