\name{sw.sound.speed}
\alias{sw.sound.speed}
\title{Seawater sound speed}
\description{
Compute the seawater speed of sound.
}
\usage{
speed <- sw.sound.speed(ctd)
speed <- sw.sound.speed(S, t=NULL, p=NULL)
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

The sound speed is calculated using the formulation in section 9 of
Fofonoff and Millard (1983).
}
\value{
  Sound speed [m/s].
}
\seealso{
  N/A.
}
\examples{
s <- sw.sound.speed(40, 40, 10000) # 1731.995 (p48 of Fofonoff + Millard 1983)
}
\references{
 Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for computation of
 fundamental properties of seawater. \emph{Unesco Technical Papers in Marine
 Science}, \bold{44}, 53 pp.  (See section 9.)
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}
