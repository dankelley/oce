\name{swConservativeTemperature}

\alias{swConservativeTemperature}

\title{Seawater conservative temperature, in GSW formulation}

\description{Compute seawater conservative temperature, according to the
    GSW/TEOS-10 formulation.}

\usage{swConservativeTemperature(salinity, temperature=NULL, pressure=NULL,
    longitude=300, latitude=30)}

\arguments{
  \item{salinity}{either practial salinity (in which case \code{temperature}
      and \code{pressure} must be provided) \strong{or} an \code{oce} object
      (in which case \code{salinity}, etc. are inferred from the object).}
  \item{temperature}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
  \item{pressure}{pressure [dbar]}
  \item{longitude}{longitude of observation.}
  \item{latitude}{latitude of observation.}
}

\details{If the first argument is an \code{oce} object, then values for
    salinity, etc., are extracted from it, and used for the calculation, and
    the corresponding arguments to the present function are ignored.

    The conservative temperature is calculated using the TEOS-10 function
    \code{\link[gsw]{gsw_CT_from_t}} from the \CRANpkg{gsw} package.}

\value{Conservative temperature in degrees Celcius.}

\seealso{The related TEOS-10 quantity ``absolute salinity'' may be computed
    with \code{\link{swAbsoluteSalinity}}.  For a ctd object, conservative
    temperature may also be recovered by indexing as e.g.
    \code{ctd[["conservativeTemperature"]]} or \code{ctd[["CT"]]}.}

\examples{
swConservativeTemperature(35,10,1000,188,4) # 9.86883
}

\references{McDougall, T.J. and P.M. Barker, 2011: Getting started with
    TEOS-10 and the Gibbs Seawater (GSW) Oceanographic Toolbox, 28pp.,
    SCOR/IAPSO WG127, ISBN 978-0-646-55621-5.}

\author{Dan Kelley}

\keyword{misc}
