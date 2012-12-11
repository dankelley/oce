\name{geodGc}

\alias{geodGc}

\title{Find great-circle segments between two points on earth}

\description{Find great-circle segments between two points on earth}

\usage{geodGc(lon1, lat1, lon2, lat2, dmax)}

\arguments{
  \item{lon1}{longitude of first point, in degrees east}
  \item{lat1}{latitude of first point, in degrees east}
  \item{lon2}{longitude of second point, in degrees north}
  \item{lat2}{latitude of second point, in degrees north}
  \item{dmax}{maximum angular separation to tolerate between sub-segments, in
      degrees.}
}

\details{Code contributed by Clark RichardsThis is based on the same geodesic calculations used in
  \code{\link{geodDist}}, the documentation of which explains the
  method and provides literature citations.}

\value{Data frame of \code{x} and \code{y}, measured in metres.}

\seealso{\code{\link{geodDist}} and \code{\link{geodXy}}}

\examples{
library(oce)
data(coastlineWorld)
mapPlot(coastlineWorld, type='l',
        latitudelim=c(0,120), longitudelim=c(-80,10),
        projection="orthographic", orientation=c(45,-30,0))
lon <- c(-63.5744, 0.1062) # Halifax CA and London UK
lat <- c(44.6479, 51.5171)
l <- geodGc(lon[1], lat[1], lon[2], lat[2], d=1)
mapLines(l$longitude, l$latitude, col='red')
}

\author{Dan Kelley, based on code from Clark Richards, in turn based on
    formulae provided by Ed Williams.}

\source{\url{http://williams.best.vwh.net/avform.htm#Intermediate}}

\keyword{misc}
