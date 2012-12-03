\name{mapPoints}

\alias{mapPoints}

\title{Plot points on a existing map}

\description{Plot points on an existing map}

\usage{mapPoints(latitude, longitude, \dots)
}

\arguments{
    \item{latitude}{latitudes of points to be plotted}
    \item{longitude}{longitudes of points to be plotted}
    \item{\dots}{optional arguments passed to \code{\link{points}}.}
}

\details{Adds points to an existing map, by analogy to \code{\link{points}}.}

\seealso{Maps are created with \code{\link{mapPlot}}, and lines may be added
    with \code{\link{mapLines}}.}

\examples{
library(oce)
data(coastlineWorld)
longitude <- coastlineWorld[['longitude']]
latitude <- coastlineWorld[['latitude']]
mapPlot(latitude, longitude, type='l',
        latitudelim=c(0,120), longitudelim=c(-80,10),
        projection="orthographic", orientation=c(45,-100,0))
lon <- -63.5744 # Halifax
lat <- 44.6479
mapPoints(lat, lon, pch=20, col='red')
}

\author{Dan Kelley}
\keyword{hplot}
