\name{mapPolygon}

\alias{mapPolygon}

\title{Plot a polygon on a existing map}

\description{Plot a polygon on an existing map}

\usage{mapPolygon(longitude, latitude, density=NULL, angle=45,
    border=NULL, col=NA, lty=par('lty'), \dots, fillOddEven=FALSE)}

\arguments{
    \item{longitude}{longitudes of points to be plotted}
    \item{latitude}{latitudes of points to be plotted}
    \item{density}{as for \code{\link{polygon}}}
    \item{angle}{as for \code{\link{polygon}}}
    \item{border}{as for \code{\link{polygon}}}
    \item{col}{as for \code{\link{polygon}}}
    \item{lty}{as for \code{\link{polygon}}}
    \item{\dots}{as for \code{\link{polygon}}}
    \item{fillOddEven}{as for \code{\link{polygon}}}
}

\details{Adds a polygon to an existing map, by analogy to
    \code{\link{polygon}}.  Used by \code{\link{mapImage}}.}

\seealso{See \code{\link{mapPlot}} for general information on plotting maps,
    including other functions.}

\examples{
library(oce)
data(coastlineWorld)
mapPlot(coastlineWorld, type='l',
        latitudelim=c(40,50), longitudelim=c(-70,-50),
        proj="polyconic", orientation=c(90, -90,0), grid=TRUE)
data(topoMaritimes)
tlon <- topoMaritimes[['longitude']][seq(1, 500, 10)]
tlat <- topoMaritimes[['latitude']][seq(1, 500, 10)]
z <- topoMaritimes[['z']][seq(1, 500, 10), seq(1, 500, 10)]
mapImage(tlon, tlat, z)
mapLines(coastlineWorld[['longitude']], coastlineWorld[['latitude']])
}

\author{Dan Kelley}
\keyword{hplot}
