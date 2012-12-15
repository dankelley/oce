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
\dontrun{
library(oce)
data(coastlineWorld)
mapPlot(coastlineWorld, type='l',
        longitudelim=c(-70,0), latitudelim=c(40,80),
        projection="orthographic", orientation=c(45,-35,0),
        grid=TRUE)
data(drifter)
lon <- drifter[['longitude']]
lat <- drifter[['latitude']]
mapPoints(lon, lat, pch=20, col='red')
}
}

\author{Dan Kelley}
\keyword{hplot}
