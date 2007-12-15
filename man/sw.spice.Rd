\name{sw.spice}
\alias{sw.spice}
\title{Seawater spiciness}
\description{
  Compute seawater "spice" (a variable orthogonal to density in TS space)
}
\usage{
spice <- sw.spice(ctd)
spice <- sw.spice(S, t=NULL, p=NULL)
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


Roughly speaking, seawater with a high spiciness is relatively warm and salty compared with
less spicy water. Another interpretation is that spice is a variable measuring distance
orthogonal to isopycnal lines on TS diagrams (if the diagrams are scaled to make the
isopycnals run at 45 degres). The definition used here is that of Pierre Flament. (Other
formulations exist.) Note that pressure is ignored in the definition. Spiceness is sometimes
denoted \eqn{\pi(S,t,p)}{pi(S,t,p)}. } \value{ Spice [kg/m\eqn{^3}{^3}]. }

\seealso{
  N/A.
}
\examples{
 spice <- sw.spice(35, 10, 0) # 1.1 by eye, from Flament's fig2
}
\references{
P. Flament, 2002.
A state variable for characterizing water masses and their diffusive
stability: spiciness.   \emph{Progr. Oceanog.}, 
\bold{54}, 493-501.
}
\author{Dan Kelley}
\keyword{misc}
