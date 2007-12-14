\name{sw.N2}
\alias{sw.N2}
\title{Seawater square of buoyancy frequency}
\description{
Compute 
\eqn{N^2}{N^2}, the square of the buoyancy frequency for a seawater profile.
}
\usage{
N2 <- sw.N2(ctd, \dots)
N2 <- sw.N2(p, sigma.theta=NULL, \dots)
}
\arguments{
  \item{ctd}{a \code{ctd} object}
  \item{p}{pressure [dbar]}
  \item{sigma.theta}{Surface-referenced potential density minus 1000 [kg/m\eqn{^3}{^3}]}
  \item{\dots}{Extra arguments that will be passed to 
    \code{smooth.spline} if supplied.  A common example is to set \code{df}, 
    the degrees of freedom for the spline fit; if not set, this will be set
    to the value \code{length(p)/4}.}
}
\details{
In the first form, the argument is a \code{ctd} object, from which the
salinity, temperature and pressure values are extracted, used to calculate
sigma.theta, and then used for the calculation of the square of the buoyancy
frequency.

The result is calculated from the derivative of a smoothing cubic
spline fitted to the density profile using \code{smooth.spline}.
Optional arguments in \dots are passed to this routine, and this gives
the user a great deal of control over the smoothing technique; see the
documentation on \code{smooth.spline} for details.  For example,
\code{\link{plot.profile}} uses \code{df=length(x$p)/4} as an optional
argument to \code{N2} to do some smoothing of the density profile.
}
\value{
  Square of buoyancy frequency [radian/s].
}
\seealso{
}
\examples{
library(oce)
data(ctd)
p <- ctd$data$pressure
sigthe <- sw.sigma.theta(ctd$data$temperature, ctd$data$salinity, ctd$data$pressure)
par(mfcol=c(3,1))
plot(sigthe, -p)
lines(sigthe, -p)
plot(sw.N2(p,sigthe), -p)
lines(sw.N2(p,sigthe), -p)
abline(v=0)
# Demonstrate the effect of the df parameter in smooth.spline()
lines(sw.N2(p,sigthe,df=length(p)/4), -p)
abline(v=0)
}
\references{}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}
