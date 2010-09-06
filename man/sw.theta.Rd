\name{sw.theta}

\alias{sw.theta}

\title{Seawater potential temperature}

\description{Compute \eqn{\theta}{theta}, the potential temperature of seawater. }

\usage{sw.theta(salinity, temperature=NULL, pressure=NULL, pref=0, method=c("UNESCO1983","Bryden1973"))}

\arguments{
  \item{salinity}{either salinity [PSU] (in which case
    \code{temperature} and \code{pressure} must be provided) \strong{or}
    a \code{ctd} object (in which case \code{salinity},
    \code{temperature} and \code{pressure} are determined from the
    object, and must not be provided in the argument list).}
  \item{temperature}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
  \item{pressure}{pressure [dbar]}
  \item{pref}{reference pressure [dbar]}
  \item{method}{algorithm to be used (see details)}
}

\details{If the first argument is a \code{ctd} object, then salinity,
  temperature and pressure values are extracted from it, and used for
  the calculation.

  The potential temperature is defined to be the temperature that a
  water parcel of salinity \code{S}, \emph{in-situ} temperature \code{t}
  and pressure \code{p} would have if were to be moved adiabatically to
  a location with pressure \code{pref}. This quantity is commonly
  denoted \eqn{\theta(S,t,p,p_{ref})}{theta(S,t,p,pref)} in the
  oceanographic literature.

  The \code{"Bryden1973"} method does not use the reference pressure,
  since it is set up to approximate potential temperature referenced to
  the surface.

  For general use, the \code{"UNESCO1983"} method is preferable, since
  it permits calculation for arbitrary reference pressure. The UNESCO
  formula is derived from Bryden's earlier method, as Fofonoff \emph{et
  al.}  (1983) explain.

  This is not the place to discuss the two methods in detail, but users
  may note from the example that the two typically yield values that
  agree to a few millidegrees.}

\value{Potential temperature [\eqn{^\circ}{deg}C] of seawater.}

\seealso{The corresponding potential density anomaly
\eqn{\sigma_\theta}{sigma-theta} can be calculated with
\code{\link{sw.sigma.theta}}.
}

\examples{
library(oce)
print(sw.theta(35, 13, 1000)) # 12.858

print(sw.theta(40,40,10000,0,"UNESCO1983")) # 36.89073 (Fofonoff et al., 1983)
	
# Demonstrate that the methods agree to a couple of
# millidegrees over a typical span of values
S <- c(30,30,38,38)
T <- c(-2,-2,30,30)
p <- rep(1000,4)                                   
print(max(abs(sw.theta(S,T,p) - sw.theta(S,T,p,0,"UNESCO1983"))))
}

\references{Bryden, H. L., 1973. New polynomials for thermal expansion,
  adiabatic temperature gradient and potential temperature of seawater,
  \emph{Deep-Sea Res.}, \bold{20}, 401-408.

  Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for computation of
  fundamental properties of seawater. \emph{Unesco Technical Papers in
  Marine Science}, \bold{44}, 53 pp.}

\author{Dan Kelley}

\keyword{misc}
