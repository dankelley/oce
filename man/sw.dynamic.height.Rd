\name{sw.dynamic.height}
\alias{sw.dynamic.height}
\title{Dynamic height of seawater profile}
\description{
Compute the dynamic height of a column of seawater.
}
\usage{
h <- sw.dynamic.height(ctd, pref=2000)
}
\arguments{
  \item{ctd}{a \code{ctd} object}
  \item{pref}{reference pressure [dbar]}
}
\details{

A piecewise-linear model of the density variation with pressure is
developed using \code{\link[stats]{approxfun}}.  (The option \code{rule=2} is
used to extrapolate the uppermost density up to the surface,
preventing a possible a bias for bottle data, in which the first depth
may be a few metres below the surface.)  A second function is
constructed as the density of water with salinity 35PSU, temperature
of 0\eqn{^\circ}{deg}C, and pressure as in the \code{ctd}.  The
reciprocal difference of these densities is then integrated using
\code{\link[stats]{integrate}} with pressure limits \code{0} to \code{pref},
and divided by the acceleration due to gravity, to calculate the
dynamic height.

A missing value is returned if the water column is too short to have
bottom pressure exceeding \code{pref}.
}
\value{
Dynamic height [m].
}
\examples{
library(oce)
data(a03)
x <- NULL
h <- NULL
for (i in 102:124) { # Gulf Stream
    x <- c(x, a03$stations[[i]]$longitude)
    h <- c(h, sw.dynamic.height(a03$stations[[i]]))
}
plot(x,h, type='b', xlab="Longitude [W]", ylab="Dynamic Height [m]")
}
\references{
Gill, A.E., 1982. \emph{Atmosphere-ocean Dynamics}, 
Academic Press, New York, 662 pp.
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}
