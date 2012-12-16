\name{mapImage}

\alias{mapImage}

\title{Plot an image on a existing map}

\description{Plot an image on an existing map}

\usage{mapImage(longitude, latitude, z)}

\arguments{
    \item{longitude}{longitudes of points to be plotted}
    \item{latitude}{latitudes of points to be plotted}
    \item{z}{matrix for image}
}

\details{Adds an image to an existing map, by analogy to \code{\link{image}}.}

\section{Bugs}{More arguments need to be added, e.g. for the colour mapping, etc.}

\seealso{See \code{\link{mapPlot}} for general information on plotting maps,
    including other functions.}

\examples{
\dontrun{
library(oce)
data(coastlineWorld)
mapPlot(coastlineWorld, type='l',
        latitudelim=c(40,50), longitudelim=c(-70,-50),
        proj="polyconic", orientation=c(90, -90,0), grid=TRUE)
data(topoMaritimes)
look <- seq(1, 600, 10)
tlon <- topoMaritimes[['longitude']][look]
tlat <- topoMaritimes[['latitude']][look]
z <- topoMaritimes[['z']][look, look]
mapImage(tlon, tlat, z)
mapMeridians(10, lty='dotted', col='darkgray')
mapZones(10, lty='dotted', col='darkgray')
mapLines(coastlineWorld)
}
}

\author{Dan Kelley}
\keyword{hplot}
