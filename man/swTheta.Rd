% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{swTheta}

\alias{swTheta}

\title{Seawater potential temperature}

\description{Compute \eqn{\theta}{theta}, the potential temperature of
  seawater.}

\usage{swTheta(salinity, temperature=NULL, pressure=NULL, referencePressure=0,
    longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))}

\arguments{
  \item{salinity}{either salinity [PSU] (in which case \code{temperature} and
      \code{pressure} must be provided) \strong{or} an \code{oce} object (in
      which case \code{salinity}, etc. are inferred from the object).}
  \item{temperature}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C], defined
      on the ITS-90 scale; see \dQuote{Temperature units} in the documentation for
      \code{\link{swRho}}, and the examples below.}
  \item{pressure}{pressure [dbar]}
  \item{referencePressure}{reference pressure [dbar]}
  \item{longitude}{longitude of observation (only used if \code{eos="gsw"}; see
      \sQuote{Details}).}
  \item{latitude}{latitude of observation (only used if \code{eos="gsw"}; see
      \sQuote{Details}).}
  \item{eos}{equation of state, either \code{"unesco"} [1,2] or \code{"gsw"}
      [3,4].}
}

\details{The potential temperature is defined to be the temperature that a
    water parcel of salinity \code{S}, \emph{in-situ} temperature \code{t} and
    pressure \code{p} would have if were to be moved adiabatically to a
    location with pressure \code{referencePressure}. This quantity is commonly
    denoted \eqn{\theta}{theta} in the oceanographic literature.

    If the first argument is a \code{ctd} or \code{section} object, then
    values for salinity, etc., are extracted from it, and used for the
    calculation, and the corresponding arguments to the present function are
    ignored.

    For \code{eos="unesco"} the method of Fofonoff \emph{et al.} (1983), is
    used [1,2].  For \code{eos="gsw"}, \code{\link[gsw]{gsw_pt_from_t}} is used
    [3,4].

}
  
\value{Potential temperature [\eqn{^\circ}{deg}C] of seawater.}

\seealso{The corresponding potential density anomaly
\eqn{\sigma_\theta}{sigma-theta} can be calculated with
\code{\link{swSigmaTheta}}.
}

\examples{
library(oce)

print(swTheta(40, T90fromT68(40), 10000, 0, eos="unesco")) # 36.89073 (Fofonoff et al., 1983)
	
# Demonstrate that the UNESCO and GSW methods agree to a about 0.1C over a
# typical span of values.
S <- c(30,35,30,35)
T <- c(-2,-2,30,30)
p <- 1000 * runif(n=4)
print(max(abs(swTheta(S,T90fromT68(T),p) - swTheta(S,T,p,0,eos="gsw"))))

# Example from a cross-Atlantic section
data(section)
stn <- section[['station', 70]]
plotProfile(stn, 'theta', ylim=c(6000, 1000))
lines(stn[['temperature']], stn[['pressure']], lty=2)
legend("topleft", lty=1:2,
       legend=c("potential", "in-situ"),
       bg='white', title="Station 70")
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
