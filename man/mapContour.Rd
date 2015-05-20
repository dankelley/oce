\name{mapContour}

\alias{mapContour}

\title{Plot contours on a existing map}

\description{Plot contours on an existing map}

\usage{mapContour(longitude=seq(0, 1, length.out=nrow(z)),
latitude=seq(0, 1, length.out=ncol(z)),
z,
nlevels=10, levels=pretty(range(z, na.rm=TRUE), nlevels), 
col=par("fg"), lty=par("lty"), lwd=par("lwd"))}


\arguments{
    \item{longitude}{longitudes of points to be plotted, or an object of class
        \code{topo} (see \code{\link{topo-class}}), in which case
        \code{longitude}, \code{latitude} and \code{z} are inferred from that
        object.}
    \item{latitude}{latitudes of points to be plotted}
    \item{z}{matrix to be contoured}
    \item{nlevels}{number of contour levels, if and only if \code{levels} is not supplied}
    \item{levels}{list of contour levels} \item{col}{colour of lines}
    \item{lty}{type of lines} \item{lwd}{width of lines}
}

\details{Adds contour lines to an existing map, using \code{\link{mapLines}}.
    The arguments are based on those to \code{\link{contour}} and
    \code{\link{contourLines}}.}

\section{Bugs}{As with \code{\link{mapLines}}, long lines should be subdivided
    into multiple segments so that e.g. great circle lines will be curved.}

\seealso{See \code{\link{mapPlot}} for general information on plotting maps,
    including other functions.}

\examples{
\dontrun{
library(oce)
data(coastlineWorld)
par(mar=rep(1, 4))
## Arctic 100m, 2km, 3km isobaths, showing shelves and ridges.
mapPlot(coastlineWorld,
        latitudelim=c(60,120), longitudelim=c(-130,-50), 
        proj="stereographic", orientation=c(90, -90, 0), 
axes=FALSE, fill='lightgray')
data(topoWorld)
lon <- topoWorld[['longitude']]
lat <- topoWorld[['latitude']]
z <- topoWorld[['z']] 
mapContour(lon, lat, z, levels=c(-100, -2000, -3000),col=1:3,lwd=2)
}
}

\author{Dan Kelley (see \code{\link{lonlat2map}} for a note on the authors of
    PROJ.4).}

\keyword{hplot}

