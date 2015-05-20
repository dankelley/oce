\name{mapPolygon}

\alias{mapPolygon}

\title{Plot a polygon on a existing map}

\description{Plot a polygon on an existing map}

\usage{mapPolygon(longitude, latitude, density=NULL, angle=45,
    border=NULL, col=NA, lty=par('lty'), \dots, fillOddEven=FALSE)}

\arguments{
    \item{longitude}{longitudes of points to be plotted, or an object from
        which longitude and latitude can be inferred (e.g. a coastline file, or
        the return value from \code{\link{mapLocator}}), in which case the
        following two arguments are ignored.}
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
\dontrun{
library(oce)
data(coastlineWorld)
mapPlot(coastlineWorld, type='l',
longitudelim=c(-70,-50), latitudelim=c(40,50),
proj="polyconic", orientation=c(90, -90,0), grid=TRUE)
data(topoWorld)
tlon <- topoWorld[['longitude']][550:650]
tlat <- topoWorld[['latitude']][240:300]
z <- topoWorld[['z']][550:650, 240:300]
mapImage(tlon, tlat, z)
mapLines(coastlineWorld[['longitude']], coastlineWorld[['latitude']])
}
}

\author{Dan Kelley (see \code{\link{lonlat2map}} for a note on the authors of
    PROJ.4).}



\keyword{hplot}

