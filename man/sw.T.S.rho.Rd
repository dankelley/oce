\name{sw.T.S.rho}
\alias{sw.T.S.rho}
\title{Seawater temperature from salinity and density}
\description{
  Compute \emph{in-situ} temperature, given salinity, density, and pressure.
}
\usage{
  temperature <- sw.T.S.rho(S, sig, p);
}
\arguments{
  \item{S}{\emph{in-situ} salinity [PSU]}
  \item{sig}{\emph{in-situ} density or sigma value [\eqn{kg/m^3}{kg/m^3}]}
  \item{p}{\emph{in-situ} pressure [dbar]}
}
\details{
  Finds the temperature that yields the given density, with the given salinity and pressure.
  The method is a bisection search with temperature tolerance 0.001 \eqn{^\circ C}{degC}.
}
\value{
  \emph{In-situ} temperature [\eqn{^\circ C}{degC}].
}

\seealso{
  \code{\link{sw.S.T.rho}}
}
\examples{
 temperature <- sw.T.S.rho(35, 23, 0)
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
