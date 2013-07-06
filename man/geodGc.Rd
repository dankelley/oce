\name{geodGc}

\alias{geodGc}

\title{Find great-circle segments between two points on earth}

\description{Find great-circle segments between two points on earth}

\usage{geodGc(longitude, latitude, dmax)}

\arguments{
  \item{longitude}{vector of longitudes, in degrees east}
  \item{latitude}{vector of latitudes, in degrees north}
  \item{dmax}{maximum angular separation to tolerate between sub-segments, in
      degrees.}
}

\details{Each pair in the \code{longitude} and \code{latitude} vectors is
    considered in turn.  For long vectors, this may be slow.}

\value{Data frame of \code{longitude} and \code{latitude}.}

\seealso{\code{\link{geodDist}} and \code{\link{geodXy}}}

\examples{
library(oce)
data(coastlineWorld)
mapPlot(coastlineWorld, type='l',
        latitudelim=c(35,80), longitudelim=c(-80,10),
        projection="orthographic", orientation=c(35, -35, 0))
## Airplanes travelling between New York and London 
## pass over Newfoundland and Nova Scotia.
lon <- c(-74.0064, 0.1062)
lat <- c(40.7142, 51.5171)
l <- geodGc(lon, lat, d=1)
mapLines(l$longitude, l$latitude, col='red', lwd=2)
}

\author{Dan Kelley, based on code from Clark Richards, in turn based on
    formulae provided by Ed Williams.}

\source{\url{http://williams.best.vwh.net/avform.htm#Intermediate}}

\keyword{misc}
