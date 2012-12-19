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
    \item{nlevels}{number of contour levels}
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
longitude <- coastlineWorld[['longitude']]
latitude <- coastlineWorld[['latitude']]
## Canada and Arctic Ocean, with 500m isobath in blue
mapPlot(longitude, latitude, type='l',
        latitudelim=c(50,120), longitudelim=c(-80,10),
        grid=10,
        projection="orthographic", orientation=c(45,-100,0))
data(topoWorld)
lon <- topoWorld[['longitude']]
lat <- topoWorld[['latitude']]
z <- topoWorld[['z']] 
mapContour(lon, lat, z, levels=-1000, lty='dotted')
}
}

\author{Dan Kelley}
\keyword{hplot}
