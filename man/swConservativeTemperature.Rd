\name{swConservativeTemperature}

\alias{swConservativeTemperature}

\title{Seawater conservative temperature, as defined in TEOS-10.}

\description{Compute seawater conservative temperature, according to TEOS-10.}

\usage{swConservativeTemperature(salinity, temperature, pressure)}

\arguments{
  \item{salinity}{either salinity (in which case \code{temperature} and
      \code{pressure} must be provided) \strong{or} a \code{ctd} object (in
      which case \code{salinity}, etc.  are determined from the object, and
      must not be provided in the argument list).}
  \item{temperature}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
  \item{pressure}{pressure [dbar]}
}

\details{If the first argument is a \code{ctd} object, then values of salinity,
    etc, are extracted from it, and used for the calculation.

    The conservative temperature is calculated using the TEOS-10 function
    \code{gsw_ct_from_t}.}

\value{Conservative temperature in degrees Celcius.}

\seealso{The related TEOS-10 quantity ``absolute salinity'' may be computed
    with \code{\link{swAbsoluteSalinity}}.} 

\examples{
\dontrun{
ct <- swConservativeTemperature(35.7, 15, 300)
stopifnot(abs(14.930280459895560 - ct) < 00.000000000000010)
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
