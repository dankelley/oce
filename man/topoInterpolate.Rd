\name{topoInterpolate}

\alias{topoInterpolate}

\title{Interpolate within a topography dataset}

\description{Interpolate within a topography dataset}


\usage{
topoInterpolate(longitude, latitude, topo)
}

\arguments{

  \item{longitude}{longitude, or vector of longitudes (in the same sign
      convention as used in \code{topo}).}
  
  \item{latitude}{latitude, or vector of latitudes (in the same sign convention
      as used in \code{topo}).}
  
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
# "The Gully", approx. 400m deep, connects Gulf of St Lawrence with North Atlantic
topoInterpolate(45, -57, topoWorld)
}

\author{Dan Kelley}

\keyword{misc}
