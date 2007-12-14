\name{sw.viscosity}
\alias{sw.viscosity}
\title{Seawater viscosity}
\description{
  Compute viscosity of seawater, in \eqn{Pa\cdot s}{Pa*s}
}
\usage{
v <- sw.viscosity(ctd)
v <- sw.viscosity(S, t=NULL)
}
\arguments{
  \item{ctd}{a \code{ctd} object}
  \item{S}{salinity [PSU]}
  \item{t}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
}
\details{
In the first form, the argument is a \code{ctd} object, from which the
salinity, temperature and pressure values are extracted and used to
for the calculation.

Derived from a curve fit to table 87 of Dorsey (1940). The fit matches the table to within
0.2 percent at worst, and with average absolute error of 0.07 percent. The maximum deviation
from the table is one unit in the last decimal place.

No pressure dependence was reported by Dorsey (1940).

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
