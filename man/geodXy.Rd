\name{geodXy}
\alias{geodXy}
\title{Convert lat/lon to x/y on earth}
\description{Convert lat/lon to x/y on earth}

\usage{geodXy(lon, lat, lon.ref, lat.ref, rotate=0)}

\arguments{
  \item{lon}{vector of longitudes}
  \item{lat}{vector of latitudes}
  \item{lon.ref}{numeric, reference longitude}
  \item{lat.ref}{numeric, reference latitude}
  \item{rotate}{numeric, counterclockwise angle, in degrees, by which to
    rotate the (\code{x}, \code{y}) coordinates about the reference
    point.  This is useful in rotating the coordinate system to align
    with a coastline, a mean current, etc.}
}

\details{This is based on the same geodesic calculations used in
  \code{\link{geodDist}}, the documentation of which explains the
  method and provides literature citations.}

\value{Data frame of \code{x} and \code{y}, measured in metres.}

\seealso{\code{\link{geodDist}}}

\examples{
library(oce)
lat <- c(0, 1/60,    0, 1/60)
lon <- c(0,    0, 1/60, 1/60)
plot(geodXy(lat, lon, 0, 0, 0) / 1852)
points(geodXy(lat, lon, 0, 0, 1) / 1852, col='red')
}

\author{Dan Kelley}

\keyword{misc}
