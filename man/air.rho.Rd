\name{air.rho}

\alias{air.rho}

\title{Air density}

\description{Compute, \eqn{\rho}{rho}, the \emph{in-situ} density of air.}

\usage{air.rho(temperature, pressure, humidity)}

\arguments{
  \item{temperature}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
  \item{pressure}{pressure in Pa (NOT kPa) -- ignored at present}
  \item{humidity}{ignored at present}
}

\details{This will eventually be a proper equation of state, but for now
  it just returns the sealevel density of a standard atmosphere, 1.225
  kg/m^3.}

\value{\emph{In-situ} air density [kg/m\eqn{^3}{^3}].}

\examples{rho <- air.rho() # 1.22}

\references{
  National Oceanographic and Atmospheric Agency, 1976.  U.S. Standard
  Atmosphere, 1976.  NOAA-S/T 76-1562.
  (Available as of 2010-09-30 at \url{http://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/19770009539_1977009539.pdf}).}

\author{Dan Kelley}

\keyword{misc}
