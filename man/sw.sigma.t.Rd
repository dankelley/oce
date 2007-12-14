\name{sw.sigma.t}
\alias{sw.sigma.t}
\title{Seawater quasi-potential density anomaly}
\description{
Compute \eqn{\sigma_t}{sigma-t}, a quasi-potential density of seawater,
minus 1000 kg/m\eqn{^3}{^3}.
}
\usage{
sigma.t <- sw.sigma.t(ctd)
sigma.t <- sw.sigma.t(S, t=NULL, p=NULL)
}
\arguments{
  \item{ctd}{a \code{ctd} object}
  \item{S}{salinity [PSU]}
  \item{t}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
  \item{p}{pressure [dbar]}
}
\details{
In the first form, the argument is a \code{ctd} object, from which the
salinity, temperature and pressure values are extracted and used to
for the calculation.


  Definition: 
  \eqn{\sigma_t=\rho(S,t,0)}{sigma_t=rho(S,t,0)}
  - 1000 kg/m\eqn{^3}{^3}.
}
\value{
  Quasi-potential density anomaly [kg/m\eqn{^3}{^3}].
}
\seealso{
Related density routines include: \code{link{sw.rho}}, 
  \code{link{sw.sigma}}, and
  \code{link{sw.sigma.theta}}.
}
\examples{
 sigma.t <- sw.sigma.t(35, 13, 1000) # 
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
