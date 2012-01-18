\name{swConductivity}

\alias{swConductivity}

\title{Seawater thermal conductivity}

\description{Compute seawater thermal conductivity, in \eqn{W m^{-1\circ}C^{-1}}{W/(m*degC)}}

\usage{swConductivity(salinity, temperature=NULL, pressure=NULL)}

\arguments{
    \item{salinity}{salinity [PSU], or a \code{ctd} object, in which case
        \code{temperature} and \code{pressure} will be ignored.}
    \item{temperature}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
    \item{pressure}{pressure [dbar]}
}

\details{If the first item is \code{ctd} object, then the salinity, temperature
    and pressure values will be extracted from it, and used in the calculation.

    \strong{Note that this is provisional code only!} 

    \strong{To do:} Fill in a summary of Caldwell's technique.

    \strong{To do:} Compare this with Caldwell's stated uncertainty.

    \strong{Caution.} The results differ from Fofonoff's (1962)
    table 5 by 0.1 percent at 35PSU, and by under 1 percent 
    for fresh water.
}

\value{Conductivity of seawater in \eqn{W m^{-1\,\circ}C^{-1}}{W/(m*degC)}.  

    To calculate thermal diffusivity in \eqn{m^2/s}{m^2/s}, divide by the
    product of density and specific heat, as in the example.
}

\examples{
library(oce)
cond <- swConductivity(10,35,100); # 0.618569
diffusivity <- cond / (swRho(10,35,100) * swSpecificHeat(10,35,100))
}

\references{Caldwell, Douglas R., 1974. Thermal conductivity of seawater,
    \emph{Deep-sea Research}, \bold{21}, 131-137.

    Fofonoff, N. P., 1962.  Physical properties of sea-water, \emph{The Sea},
    \bold{1}, 3-30.}

\author{Dan Kelley}

\keyword{misc}

