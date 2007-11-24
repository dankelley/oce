\name{sw.sound.speed}
\alias{sw.sound.speed}
\title{Seawater sound speed}
\description{
Compute the seawater speed of sound.
}
\usage{
speed <- sw.sound.speed(S, t, p);
}
\arguments{
  \item{S}{\emph{in-situ} salinity [PSU]}
  \item{t}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
  \item{p}{\emph{in-situ} pressure [dbar]}
}
\details{
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
