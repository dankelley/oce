% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{swThermalConductivity}

\alias{swThermalConductivity}

\title{Seawater thermal conductivity}

\description{Compute seawater thermal conductivity, in \eqn{W m^{-1\circ}C^{-1}}{W/(m*degC)}}

\usage{swThermalConductivity(salinity, temperature=NULL, pressure=NULL)}

\arguments{

  \item{salinity}{salinity [PSU], or a \code{ctd} object, in which case
    \code{temperature} and \code{pressure} will be ignored.}

  \item{temperature}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C], defined
    on the ITS-90 scale; see \dQuote{Temperature units} in the documentation for
    \code{\link{swRho}}.}

  \item{pressure}{pressure [dbar]}
}

\details{Caldwell's (1974) detailed formulation is used.  To be specific, his
  equation 6 to calculate K, and his two sentences above that equation are used
  to infer this to be K(0,T,S) in his notation of equation 7. Then, application
  of his equations 7 and 8 is straightforward. He states an accuracy for this
  method of 0.3 percent.  (See the check against his Table 1 in the
  \dQuote{Examples}.)

  If the first argument is an appropriate \code{oce} object, then the salinity,
  temperature and pressure values are extracted from it and then used in the
  calculation, ignoring the values of the other arguments.

}

\value{Conductivity of seawater in \eqn{W m^{-1\,\circ}C^{-1}}{W/(m*degC)}.  

    To calculate thermal diffusivity in \eqn{m^2/s}{m^2/s}, divide by the
    product of density and specific heat, as in the example.
}

\examples{
library(oce)
# Values in m^2/s, a unit that is often used instead of W/(m*degC).
swThermalConductivity(35, 10, 100) / (swRho(35,10,100) * swSpecificHeat(35,10,100)) # ocean
swThermalConductivity(0, 20, 0) / (swRho(0, 20, 0) * swSpecificHeat(0, 20, 0)) # lab
# Caldwell Table 1 gives 1478e-6 cal/(cm*sec*degC) at 31.5 o/oo, 10degC, 1kbar
joulePerCalorie <- 4.18400
cmPerM <- 100
swThermalConductivity(31.5,10,1000) / joulePerCalorie / cmPerM
}

\references{Caldwell, Douglas R., 1974. Thermal conductivity of seawater,
    \emph{Deep-sea Research}, \bold{21}, 131-137.}

\author{Dan Kelley}

\keyword{misc}

