\name{swAbsoluteSalinity}

\alias{swAbsoluteSalinity}

\title{Seawater absolute salinity, in TEOS-10}

\description{Compute seawater absolute salinity, according to TEOS-10.}

\usage{swAbsoluteSalinity(salinity, pressure, longitude, latitude)}

\arguments{
  \item{salinity}{either salinity (in which case \code{pressure}, etc.  must
      be provided) \strong{or} a \code{ctd} object (in which case
      \code{salinity}, etc.  are determined from the object, and must not be
      provided in the argument list).}
  \item{pressure}{pressure in dbar.}
  \item{longitude}{longitude of observation.}
  \item{latitude}{latitude of observation.}
}

\details{If the first argument is a \code{ctd} object, then values of salinity,
    etc, are extracted from it, and used for the calculation.

    The absolute salinity is calculated using the TEOS-10 function
    \code{gsw_sa_from_sp}.  Typically, this is a fraction of a unit higher than
    practical salinity as defined in the UNESCO formulae from the 1980s.}


\value{Absolute salinity in \eqn{g/kg}{g/kg}.}

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

\references{ [1] McDougall, T.J. and P.M. Barker, 2011: Getting started with
    TEOS-10 and the Gibbs Seawater (GSW) Oceanographic Toolbox, 28pp.,
    SCOR/IAPSO WG127, ISBN 978-0-646-55621-5.

    [2] The TEOS-10 library is provided at \url{www.teos-10.org}.
    
    [3] A programming interface to the matlab version of the TEOS library is
    provided at \url{http://www.teos-10.org/pubs/gsw/html/gsw_contents.html}.}

\author{Dan Kelley}

\keyword{misc}
