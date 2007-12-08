\name{sw.sigma}
\alias{sw.sigma}
\title{Seawater density anomaly}
\description{
Compute \eqn{\sigma}{sigma}, the \emph{in-situ} density of seawater, minus 1000 kg/m\eqn{^3}{^3}.
}
\usage{
  sig <- sw.sigma(S, t, p);
}
\arguments{
  \item{S}{\emph{in-situ} salinity [PSU]}
  \item{t}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
  \item{p}{\emph{in-situ} pressure [dbar]}
}
\details{
  Definition:
  \eqn{\rho(S,t,p)}{rho(S,t,p)}
  - 1000 kg/m\eqn{^3}{^3}.
}
\value{
  \emph{In-situ} density anomaly [kg/m\eqn{^3}{^3}].
}
\seealso{
Related density routines include: \code{\link{sw.rho}} 
to calculate in-situ density,
\code{\link{sw.sigma.t}} to calculate an archaic quantity found in old
textbooks, and
\code{\link{sw.sigma.theta}} to calculate potential density anomaly
relative to the surface pressure.  If you are thinking of using
\code{sw.sigma}, you probably want \code{\link{sw.sigma.theta}}.
}
\examples{
 sig <- sw.sigma(35, 13, 1000)	# 30.818
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
