\name{swAbsoluteSalinity}

\alias{swAbsoluteSalinity}

\title{Seawater absolute salinity, in GSW formulation}

\description{Compute seawater absolute salinity, according to the GSW/TEOS-10
    formulation.}

\usage{swAbsoluteSalinity(salinity, pressure=NULL, longitude=300, latitude=30)}

\arguments{
  \item{salinity}{either practial salinity (in which case \code{temperature}
      and \code{pressure} must be provided) \strong{or} an \code{oce} object
      (in which case \code{salinity}, etc. are inferred from the object).}
  \item{pressure}{pressure in dbar.}
  \item{longitude}{longitude of observation.}
  \item{latitude}{latitude of observation.}
}

\details{If the first argument is an \code{oce} object, then values for
    salinity, etc., are extracted from it, and used for the calculation, and
    the corresponding arguments to the present function are ignored.

    The absolute salinity is calculated using the GSW function
    \code{\link[gsw]{gsw_SA_from_SP}}.  Typically, this is a fraction of a unit
    higher than practical salinity as defined in the UNESCO formulae.}


\value{Absolute Salinity in \eqn{g/kg}{g/kg}.}

\seealso{The related TEOS-10 quantity ``conservative temperature'' may be
    computed with \code{\link{swConservativeTemperature}}.  For a ctd object,
    absolute salinity may also be recovered by indexing as e.g.
    \code{ctd[["absoluteSalinity"]]} or \code{ctd[["SA"]]}.}

\examples{
\dontrun{
sa <- swAbsoluteSalinity(35.5, 300, 260, 16)
stopifnot(abs(35.671358392019094 - sa) < 00.000000000000010)
}
}

\references{McDougall, T.J. and P.M. Barker, 2011: Getting started with
    TEOS-10 and the Gibbs Seawater (GSW) Oceanographic Toolbox, 28pp.,
    SCOR/IAPSO WG127, ISBN 978-0-646-55621-5.}

\author{Dan Kelley}

\keyword{misc}
