\name{sw.rho}
\alias{sw.rho}
\title{Seawater density}
\description{
Compute, \eqn{\rho}{rho}, the \emph{in-situ} density of seawater.
}
\usage{
rho <- sw.rho(S, t, p);
}
\arguments{
  \item{S}{\emph{in-situ} salinity [PSU]}
  \item{t}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
  \item{p}{\emph{in-situ} pressure [dbar]}
}
\details{
  The density is calculated using the UNESCO equation of state
  for seawater, assuming that input variables are defined
  according to modern calibrations.
  In conventional Oceanographic notation, the calculated quantity is
  \eqn{\rho(S,t,p)}{rho(S,t,p)}
}
\value{
  \emph{In-situ} density [kg/m\eqn{^3}{^3}].
}

\seealso{
Related density routines include
\code{\link{sw.sigma}}, 
\code{\link{sw.sigma.t}}, and
\code{\link{sw.sigma.theta}}. 
}
\examples{
 rho <- sw.rho(35, 13, 1000) # 1030.818
}
\references{
 Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for computation of
 fundamental properties of seawater. \emph{Unesco Technical Papers in Marine
 Science}, \bold{44}, 53 pp

 Gill, A.E., 1982. \emph{Atmosphere-ocean Dynamics}, 
 Academic Press, New York, 662 pp.
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}
