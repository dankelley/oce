\name{sw.sigma.theta}
\alias{sw.sigma.theta}
\title{Seawater potential density anomaly}
\description{
Compute \eqn{\sigma_\theta}{sigma-theta}, the 
potential density of seawater, minus 1000 kg/m\eqn{^3}{^3}.
}
\usage{
  sigma.theta <- sw.sigma.theta(S, t, p);
}
\arguments{
  \item{S}{\emph{in-situ} salinity [PSU]}
  \item{t}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
  \item{p}{\emph{in-situ} pressure [dbar]}
}
\details{
  Definition:
  \eqn{\sigma_\theta=\rho(S,\theta(S,t,p),0}{sigma_theta=rho(S,theta(S,t,p),0)}
  - 1000 kg/m\eqn{^3}{^3}.
}
\value{
  Potential density anomaly [kg/m\eqn{^3}{^3}].
}
\seealso{
Related density routines include: \code{link{sw.rho}}, 
  \code{link{sw.sigma}}, and
  \code{link{sw.sigma.t}}.
}
\examples{
 sigma.theta <- sw.sigma.theta(35, 13, 1000) #
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
