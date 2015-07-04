% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{swRho}

\alias{swRho}

\title{Seawater density}

\description{Compute \eqn{\rho}{rho}, the \emph{in-situ} density of seawater.}

\usage{swRho(salinity, temperature=NULL, pressure=NULL,
    longitude, latitude, eos=getOption("oceEOS", default="gsw"))}

\arguments{

  \item{salinity}{either practical salinity (in which case \code{temperature}
    and \code{pressure} must be provided) \strong{or} an \code{oce} object, in
    which case \code{salinity}, \code{temperature} (in the ITS-90 scale; see
    next item), etc. are inferred from the object.}

  \item{temperature}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C], as defined
    on the ITS-90 scale.  This scale is used by GSW-style calculation (as
    requested by setting \code{eos="gsw"}), and is the value contained within
    \code{ctd} objects (and probably most other objects created with data
    acquired in the past decade or two). Since the UNESCO-style calculation is
    based on IPTS-68, the temperature is converted within the present function,
    using \code{\link{T68fromT90}}.}

  \item{pressure}{pressure [dbar]}

  \item{longitude}{longitude of observation (only used if \code{eos="gsw"}; see
    \sQuote{Details}).}

  \item{latitude}{latitude of observation (only used if \code{eos="gsw"}; see
    \sQuote{Details}).}

  \item{eos}{equation of state, either \code{"unesco"} [1,2] or \code{"gsw"}
    [3,4].}

}


\details{If the first argument is an \code{oce} object, then values for
  salinity, etc., are extracted from it, and used for the calculation, and the
  corresponding arguments to the present function are ignored.

  If \code{eos="unesco"}, the density is calculated using the UNESCO equation of
  state for seawater [1,2], and if \code{eos="gsw"}, the GSW formulation [3,4]
  is used.

}
    
\value{\emph{In-situ} density [kg/m\eqn{^3}{^3}].}

\seealso{Related density routines include
\code{\link{swSigma0}} (and equivalents at other pressure horizons),
\code{\link{swSigmaT}}, and
\code{\link{swSigmaTheta}}. 
}

\examples{
library(oce)
rho <- swRho(35, 13, 1000)
}

\references{

  [1] Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for computation of
  fundamental properties of seawater. \emph{Unesco Technical Papers in Marine
    Science}, \bold{44}, 53 pp

  [2] Gill, A.E., 1982. \emph{Atmosphere-ocean Dynamics}, Academic Press, New
  York, 662 pp.

  [3] IOC, SCOR, and IAPSO (2010). The international thermodynamic equation of
  seawater-2010: Calculation and use of thermodynamic properties.  Technical
  Report 56, Intergovernmental Oceanographic Commission, Manuals and Guide.

  [4] McDougall, T.J. and P.M. Barker, 2011: Getting started with TEOS-10 and
  the Gibbs Seawater (GSW) Oceanographic Toolbox, 28pp., SCOR/IAPSO WG127, ISBN
  978-0-646-55621-5.

}

\author{Dan Kelley}

\keyword{misc}
