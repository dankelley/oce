\name{swSigmaTheta}

\alias{swSigmaTheta}

\title{Seawater potential density anomaly}

\description{Compute \eqn{\sigma_\theta}{sigma-theta}, the potential
  density of seawater, minus 1000 kg/m\eqn{^3}{^3}. }

\usage{swSigmaTheta(salinity, temperature=NULL, pressure=NULL, referencePressure=0,
    longitude, latitude, eos=getOption("oceEOS", default="gsw"))}

\arguments{
  \item{salinity}{either salinity [PSU] (in which case \code{temperature} and
      \code{pressure} must be provided) \strong{or} a \code{ctd} object or
      \code{section} object (in which case all the other arguments are ignored,
      with data stored within the object being used instead).}
  \item{temperature}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
  \item{pressure}{pressure [dbar]}
  \item{referencePressure}{reference pressure [dbar], e.g. 0 for
      \eqn{\sigma_\theta}{sigma_theta} (or \eqn{\sigma_0}{sigma_0})
      calculation.}
  \item{longitude}{longitude of observation (only used if \code{eos="gsw"}).}
  \item{latitude}{latitude of observation (only used if \code{eos="gsw"}).}
  \item{eos}{equation of state, either \code{"unesco"} [1,2] or \code{"gsw"}
      [3,4].}
}

\details{If the first argument is a \code{ctd} object, then salinity,
    temperature and pressure values are extracted from it, and used for the
    calculation.

    In the \code{gsw} case, if \code{longitude} and \code{latitude} are
    missing, default values of 300E and 30N will be used, and a warning will be
    generated if \code{getOption("oceEOSwarning")} is \code{TRUE}.

}

\value{Potential density anomaly [kg/m\eqn{^3}{^3}], defined as
    \eqn{\sigma_\theta=\rho(S,\theta(S,t,p),0}{sigma_theta=rho(S,theta(S,t,p),0)}
  - 1000 kg/m\eqn{^3}{^3}.}

\seealso{Related density routines include: \code{\link{swRho}},
    \code{\link{swSigmaT}}, and the family of functions for density referenced
    to various pressure values, namely \code{\link{swSigma0}},
    \code{\link{swSigma1}}, \code{\link{swSigma2}}, \code{\link{swSigma3}}, and
    \code{\link{swSigma0}}.}

\examples{
sigmaTheta <- swSigmaTheta(35, 13, 1000) #
}

\references{

    [1] Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for computation of
    fundamental properties of seawater. \emph{Unesco Technical Papers in Marine
        Science}, \bold{44}, 53 pp

    [2] Gill, A.E., 1982. \emph{Atmosphere-ocean Dynamics}, Academic Press, New
    York, 662 pp.

    [3] IOC, SCOR, and IAPSO (2010). The international thermodynamic equation
    of seawater-2010: Calculation and use of thermodynamic properties.
    Technical Report 56, Intergovernmental Oceanographic Commission, Manuals
    and Guide.

    [4] McDougall, T.J. and P.M. Barker, 2011: Getting started with TEOS-10 and
    the Gibbs Seawater (GSW) Oceanographic Toolbox, 28pp., SCOR/IAPSO WG127,
    ISBN 978-0-646-55621-5.

}

\author{Dan Kelley}

\keyword{misc}
