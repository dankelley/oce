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
  it's just returns something from wikipedia (i.e. not trustworthy), and
  not using humidity.}

\value{\emph{In-situ} air density [kg/m\eqn{^3}{^3}].}

\examples{
degC <- seq(0,30,length.out=100)
p <- seq(98,102,length.out=100) * 1e3
contour(x=degC, y=p, z=outer(degC,p,air.rho), labcex=1)
}

\references{
  National Oceanographic and Atmospheric Agency, 1976.  U.S. Standard
  Atmosphere, 1976.  NOAA-S/T 76-1562.
  (Available as of 2010-09-30 at \url{http://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/19770009539_1977009539.pdf}).}

\author{Dan Kelley}

\keyword{misc}
