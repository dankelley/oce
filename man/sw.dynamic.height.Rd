\name{sw.dynamic.height}
\alias{sw.dynamic.height}
\title{Dynamic height of seawater profile}
\description{
Compute the dynamic height of a column of seawater.
}
\usage{
dh <- sw.dynamic.height(section, pref=2000)
h <- sw.dynamic.height(ctd, pref=2000)
}
\arguments{
  \item{section}{a \code{section} object}
  \item{ctd}{a \code{ctd} object}
  \item{pref}{reference pressure [dbar]}
}
\details{

The first form calculates dynamic height for each station within a section, and returns a
list containing distance along the section along with dynamic height.

The second form calculates dynamic height for a single station, and returns just a single
value, the dynamic height.

Stations are analysed in steps. First, a piecewise-linear model of the density variation with
pressure is developed using \code{\link[stats]{approxfun}}. (The option \code{rule=2} is used
to extrapolate the uppermost density up to the surface, preventing a possible a bias for
bottle data, in which the first depth may be a few metres below the surface.) A second
function is constructed as the density of water with salinity 35PSU, temperature of
0\eqn{^\circ}{deg}C, and pressure as in the \code{ctd}. The reciprocal difference of these
densities is then integrated using \code{\link[stats]{integrate}} with pressure limits
\code{0} to \code{pref}, and divided by the acceleration due to gravity, to calculate the
dynamic height.

If the water column is too short to have bottom pressure exceeding \code{pref}, a missing
value is reported.
}
\value{
In the first form, a list containing \code{distance}, the distance [km] from the first
station in the section and \code{height}, the dynamic height [m].

In the second form, dynamic height [m].
}
\examples{
library(oce)
data(a03)
Gulf.Stream <- section.subset(a03, 124:102)
dh <- sw.dynamic.height(Gulf.Stream)
plot(dh$distance, dh$height, type='b', xlab="Distance along track [km]", ylab="Dynamic Height [m]")
}
\references{
Gill, A.E., 1982. \emph{Atmosphere-ocean Dynamics}, 
Academic Press, New York, 662 pp.
}
\author{Dan Kelley}
\keyword{misc}
