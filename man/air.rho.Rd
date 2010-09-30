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
  it just returns the sealevel density of a standard atmosphere, 1.22
  kg/m^3.}

\value{\emph{In-situ} air density [kg/m\eqn{^3}{^3}].}

\examples{rho <- air.rho() # 1.22}

\references{

  \url{http://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/19770009539_1977009539.pdf}

  \url{http://www.atmosculator.com/The\%20Standard\%20Atmosphere.html}
}

\author{Dan Kelley}

\keyword{misc}
