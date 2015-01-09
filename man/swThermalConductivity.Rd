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

\details{Caldwell's (1974) formulation is used.  If the first item is
    an appropriate \code{oce} object, then the salinity, temperature and
    pressure values will be extracted from it, and used in the calculation.}

\value{Conductivity of seawater in \eqn{W m^{-1\,\circ}C^{-1}}{W/(m*degC)}.  

    To calculate thermal diffusivity in \eqn{m^2/s}{m^2/s}, divide by the
    product of density and specific heat, as in the example.
}

\examples{
library(oce)
cond <- swThermalConductivity(35, 10, 100)
diffusivity <- cond / (swRho(35,10,100) * swSpecificHeat(35,10,100))
}

\references{Caldwell, Douglas R., 1974. Thermal conductivity of seawater,
    \emph{Deep-sea Research}, \bold{21}, 131-137.}

\author{Dan Kelley}

\keyword{misc}

