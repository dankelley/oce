\name{mapPoints}

\alias{mapPoints}

\title{Plot points on a existing map}

\description{Plot points on an existing map}

\usage{mapPoints(longitude, latitude, \dots)}

\arguments{
    \item{longitude}{longitudes of points to be plotted}
    \item{latitude}{latitudes of points to be plotted}
    \item{\dots}{optional arguments passed to \code{\link{points}}.}
}

\details{Adds points to an existing map, by analogy to \code{\link{points}}.}

\seealso{See \code{\link{mapPlot}} for general information on plotting maps,
    including other functions.}

\examples{
library(oce)
data(coastlineWorld)
longitude <- coastlineWorld[['longitude']]
latitude <- coastlineWorld[['latitude']]
mapPlot(longitude, latitude, type='l',
        longitudelim=c(-80,10), latitudelim=c(0,120),
        projection="orthographic", orientation=c(45,-100,0))
lon <- -63.5744 # Halifax
lat <- 44.6479
mapPoints(lon, lat, pch=20, col='red')
}

\author{Dan Kelley}
\keyword{hplot}
