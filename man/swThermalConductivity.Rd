\name{swThermalConductivity}

\alias{swThermalConductivity}

\title{Seawater thermal conductivity}

\description{Compute seawater thermal conductivity, in \eqn{W m^{-1\circ}C^{-1}}{W/(m*degC)}}

\usage{swThermalConductivity(salinity, temperature=NULL, pressure=NULL)}

\arguments{
    \item{salinity}{salinity [PSU], or a \code{ctd} object, in which case
        \code{temperature} and \code{pressure} will be ignored.}
    \item{temperature}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
    \item{pressure}{pressure [dbar]}
}

\details{Caldwell's (1974 equation 9) formulation is used.  If the first
    argument is an appropriate \code{oce} object, then the salinity,
    temperature and pressure values are extracted from it and then used in the
    calculation, ignoring the values of the other arguments.

     Table 1 of Caldwell 1974 gives 1478e-6 cal/(cm*s*degC) at S=31.5, T=10,
     and p=1kbar=1e4dbar, based on another scheme that is somewhat difficult to
     understand.  Thus, the "table 1" value in the examples is not a check
     value, but rather a demonstration that Caldwell's two methods agree to
     about 0.1 percent, well within the 0.5 percent that he states as the
     accuracty for his equation.
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
# Comparison with Caldwell Table 1.
swThermalConductivity(31.5,10,1.0e4) / 418.4 * 1e6 # cf Caldwell table 1
}

\references{Caldwell, Douglas R., 1974. Thermal conductivity of seawater,
    \emph{Deep-sea Research}, \bold{21}, 131-137.}

\author{Dan Kelley}

\keyword{misc}

