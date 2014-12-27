\name{swTheta}

\alias{swTheta}

\title{Seawater potential temperature}

\description{Compute \eqn{\theta}{theta}, the potential temperature of
  seawater.}

\usage{swTheta(salinity, temperature=NULL, pressure=NULL, referencePressure=0,
         method=c("unesco","bryden","gsw","teos"))}

\arguments{
  \item{salinity}{either salinity [PSU] (in which case
    \code{temperature} and \code{pressure} must be provided) \strong{or}
    a \code{ctd} object (in which case \code{salinity},
    \code{temperature} and \code{pressure} are determined from the
    object, and must not be provided in the argument list).}
  \item{temperature}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
  \item{pressure}{pressure [dbar]}
  \item{referencePressure}{reference pressure [dbar]}
  \item{method}{algorithm to be used (see details)}
}

\details{If the first argument is a \code{ctd} object, then salinity,
  temperature and pressure values are extracted from it, and used for
  the calculation.

  The potential temperature is defined to be the temperature that a
  water parcel of salinity \code{S}, \emph{in-situ} temperature \code{t}
  and pressure \code{p} would have if were to be moved adiabatically to
  a location with pressure \code{referencePressure}. This quantity is
  commonly denoted \eqn{\theta}{theta} in the oceanographic literature.

  The \code{"bryden"} method, based on Bryden (1973), calculates
  potential temperature referenced to the surface, ignoring the
  reference pressure.  The \code{"unesco"} method, based on Fofonoff
  \emph{et al.} (1983), generalizes Bryden's method, permitting
  calculation for arbitrary reference pressure.  For normal use, the
  \code{"unesco"} method is preferred.  (The example given below
  illustrates that the two methods yield similar results for
  surface-referenced calculations.)}

\value{Potential temperature [\eqn{^\circ}{deg}C] of seawater.}

\seealso{The corresponding potential density anomaly
\eqn{\sigma_\theta}{sigma-theta} can be calculated with
\code{\link{swSigmaTheta}}.
}

\examples{
library(oce)
cat("unesco example:",swTheta(35, 13, 1000, 0),"\n")
cat("bryden example:",swTheta(35, 13, 1000, 0, "bryden"),"\n")

print(swTheta(40,40,10000,0,"unesco")) # 36.89073 (Fofonoff et al., 1983)
	
# Demonstrate that the methods agree to a couple of
# millidegrees over a typical span of values
S <- c(30,30,38,38)
T <- c(-2,-2,30,30)
p <- 1000 * runif(n=4)
print(max(abs(swTheta(S,T,p) - swTheta(S,T,p,0,"bryden"))))


# Example from a cross-Atlantic section
data(section)
stn <- section[['station', 70]]
plotProfile(stn, 'theta', ylim=c(6000, 1000))
lines(stn[['temperature']], stn[['pressure']], lty=2)
legend("topleft", lty=1:2,
       legend=c("potential", "in-situ"),
       bg='white', title="Station 70")
}

\references{Bryden, H. L., 1973. New polynomials for thermal expansion,
  adiabatic temperature gradient and potential temperature of seawater,
  \emph{Deep-Sea Res.}, \bold{20}, 401-408.

  Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for computation of
  fundamental properties of seawater. \emph{Unesco Technical Papers in
  Marine Science}, \bold{44}, 53 pp.}

\author{Dan Kelley}

\keyword{misc}
