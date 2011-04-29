\name{topoInterpolate}

\alias{topoInterpolate}

\title{Interpolate within a topography dataset}

\description{Interpolate within a topography dataset}


\usage{
topoInterpolate(latitude, longitude, topo)
}

\arguments{

  \item{latitude}{latitude, or vector of latitudes (in the same sign convention
      as used in \code{topo}).}

  \item{longitude}{longitude, or vector of longitudes (in the same sign
      convention as used in \code{topo}).}
  
  \item{topo}{topography object, as e.g. loaded with \code{data(topoWorld)}.}

}

\details{Bilinear interpolation is used so that values will vary smoothly
    within a latitude-longitude grid cell. Note that the sign convention for
    \code{latitude} and \code{longitude} must match that in \code{topo}.}

\value{A height (or vector of heights) giving the elevation of the earth above
    means sea level at the indicated location on the earth.}

\seealso{A \code{topo} object may be read with \code{\link{read.topo}}.}

\examples{
library(oce)
data(topoWorld)
data(topoMaritimes)
# "The Gully", approx. 400m deep, connects Gulf of St Lawrence with North Atlantic
topoInterpolate(45, 360 - 57, topoWorld)
topoInterpolate(45, -57, topoMaritimes)
}

\author{Dan Kelley}

\keyword{misc}
