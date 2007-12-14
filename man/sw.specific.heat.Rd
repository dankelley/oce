\name{sw.specific.heat}
\alias{sw.specific.heat}
\title{Seawater specific heat}
\description{
  Compute specific heat of seawater.
}
\usage{
C.P <- sw.specific.heat(ctd)
C.P <- sw.specific.heat(S, t=NULL, p=NULL)
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

Based on matlab code on
\url{ftp://acourstics.whoi.edu/pub/Matlab/oceans}, which was in turn
based on Millero et al (1973, 1981).
}
\value{
  Specific heat \eqn{J kg^{-1}\,^\circ C^{-1}}{J/(kg degC)}
}
\seealso{
}
\examples{
 C.P <- sw.specific.heat(40, 40, 10000)	# 3949.500
}
\references{
Millero et. al., J. Geophys. Res. 78 (1973), 4499-4507

Millero et. al., UNESCO report 38 (1981), 99-188.
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}
