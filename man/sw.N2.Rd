\name{sw.N2}

\alias{sw.N2}

\title{Seawater square of buoyancy frequency}

\description{Compute 
\eqn{N^2}{N^2}, the square of the buoyancy frequency for a seawater profile.
}

\usage{sw.N2(p, sigma.theta=NULL, \dots)}

\arguments{
  \item{p}{either pressure [dbar] (in which case \code{sigma.theta} must
  be provided) \strong{or} an object of class \code{ctd} object (in which case
    \code{sigma.theta} is inferred from the object.}
  \item{sigma.theta}{Surface-referenced potential density minus 1000 [kg/m\eqn{^3}{^3}]}
  \item{\dots}{optional extra arguments to be passed to
    \code{smooth.spline}.  A common example is to set \code{df}, the
    degrees of freedom for the spline fit; if not set, this will be set
    to \code{min(length(p)/5, 10)}.}
}

\details{If the first argument is a \code{ctd} object, then
  \code{sigma.theta} is inferred from it.

  The result is calculated from the derivative of a smoothing cubic
  spline fitted to the density profile using \code{smooth.spline}.
  Optional arguments in \dots are passed to this routine, and this gives
  the user a great deal of control over the smoothing technique; see the
  documentation on \code{smooth.spline} for details.  For example,
  \code{\link{plot.profile}} uses \code{df=length(x$p)/5} as an optional
  argument to \code{N2} to do some smoothing of the density profile.}

\value{Square of buoyancy frequency [radian/s].}

\examples{
library(oce)
data(ctd)
# Illustrate effect of changing df
plot(sw.N2(ctd), ctd$data$pressure,
	ylim=rev(range(ctd$data$pressure)), xlab="N2",ylab="p", type='l')
lines(sw.N2(ctd, df=10), ctd$data$pressure, col="blue")
grid()
}

\author{Dan Kelley}

\keyword{misc}
