\name{sw.viscosity}
\alias{sw.viscosity}
\title{Seawater viscosity}
\description{
  Compute viscosity of seawater, in \eqn{Pa\cdot s}{Pa*s}
}
\usage{
  v <- sw.viscosity(S, t);
}
\arguments{
  \item{S}{\emph{in-situ} salinity [PSU]}
  \item{t}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
}
\details{
  Derived from a curve fit to table 87 of Dorsey (1940).
  The fit matches the table to 
  within 0.2 percent at worst, and with average
  absolute error of 0.07 percent.  
  The maximum
  deviation from the table is one unit in the
  last decimal place.

  No pressure dependence was reported by Dorsey (1940).

  It would be a good idea to look for other values 
  to check those used here, and of course to
  get an idea of pressure dependence.  Therefore,
  this subroutine is provisional.
  
  -- Dan Kelley 2006-01-21.
}
\value{
  Viscosity of seawater in \eqn{Pa\cdot s}{Pa*s}.  Divide
  by density to get kinematic viscosity in \eqn{m^2/s}{m^2/s}.
}
\seealso{
  N/A.
}
\examples{
 v <- sw.viscosity(30,10); # 0.001383779
}
\references{
 N. Ernest Dorsey (1940),
 \emph{Properties of ordinary Water-substance},
 American Chemical Society Monograph Series.
 Reinhold Publishing.
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}
