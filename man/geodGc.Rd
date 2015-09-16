\name{geodGc}

\alias{geodGc}

\title{Great-circle segments between points on earth}

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
\dontrun
library(oce)
data(coastlineWorld)
mapPlot(coastlineWorld, type='l',
        longitudelim=c(-80,10), latitudelim=c(35,80),
        projection="+proj=ortho", orientation=c(35, -35, 0))
## Great circle from New York to Paris (Lindberg's flight)
l <- geodGc(c(-73.94,2.35), c(40.67,48.86), 1) 
mapLines(l$longitude, l$latitude, col='red', lwd=2)
}
}

\author{Dan Kelley, based on code from Clark Richards, in turn based on
    formulae provided by Ed Williams [1].}

\references{1.  \url{http://williams.best.vwh.net/avform.htm#Intermediate}}

\keyword{misc}

