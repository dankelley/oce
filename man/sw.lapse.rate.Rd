\name{sw.lapse.rate}
\alias{sw.lapse.rate}
\title{Seawater lapse rate}
\description{
  Compute adiabatic lapse rate
}
\usage{
lr <- sw.lapse.rate(ctd)
lr <- sw.lapse.rate(S, t=NULL, p=NULL)
}
\arguments{
  \item{ctd}{an object of \code{\link[base]{class}} "\code{ctd}".}
  \item{S}{salinity [PSU]}
  \item{t}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
  \item{p}{pressure [dbar]}
}
\details{
In the first form, the argument is a \code{ctd} object, from which the
salinity, temperature and pressure values are extracted and used to
for the calculation.

Compute lapse rate using Fofonoff and Millard's (1983) formula.
}
\value{
  Lapse rate [\eqn{deg}{deg}C/m].
}
\seealso{
  N/A.
}
\examples{
 lr <- sw.lapse.rate(40, 40, 10000) # 3.255976e-4
}
\references{
 Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for computation of
 fundamental properties of seawater. \emph{Unesco Technical Papers in Marine
 Science}, \bold{44}, 53 pp.  (Section 7, pages 38-40)
}
\author{Dan Kelley}
\keyword{misc}
