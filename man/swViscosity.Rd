% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{swViscosity}

\alias{swViscosity}

\title{Seawater viscosity}

\description{Compute viscosity of seawater, in \eqn{Pa\cdot s}{Pa*s}}

\usage{swViscosity(salinity, temperature=NULL)}

\arguments{
  \item{salinity}{either salinity [PSU] (in which case
    \code{temperature} and \code{pressure} must be provided) \strong{or}
    a \code{ctd} object (in which case \code{salinity},
    \code{temperature} and \code{pressure} are determined from the
    object, and must not be provided in the argument list).}
  \item{temperature}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C], defined on
    the ITS-90 scale; see \dQuote{Temperature units} in the documentation for
    \code{\link{swRho}}, and the examples below.}
}

\details{If the first argument is a \code{ctd} object, then salinity,
  temperature and pressure values are extracted from it, and used for the
  calculation.

  The result is determined from a regression of the data provided in Table 87 of
  Dorsey (1940). The fit matches the table to within 0.2 percent at worst, and
  with average absolute error of 0.07 percent. The maximum deviation from the
  table is one unit in the last decimal place.

  No pressure dependence was reported by Dorsey (1940).
}

\value{Viscosity of seawater in \eqn{Pa\cdot s}{Pa*s}.  Divide by density to get
  kinematic viscosity in \eqn{m^2/s}{m^2/s}.}

\examples{
swViscosity(30, 10) # 0.001383779
}

\references{N. Ernest Dorsey (1940), \emph{Properties of ordinary
 Water-substance}, American Chemical Society Monograph Series.  Reinhold
 Publishing.}

\author{Dan Kelley}

\keyword{misc}
