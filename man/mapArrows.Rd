\name{mapArrows}

\alias{mapArrows}

\title{Plot arrows on a existing map}

\description{Plot arrows on an existing map, e.g. to indicate a place location.
    This is not well-suited for drawing direction fields, e.g. of velocities;
    for that, see \code{\link{mapDirectionField}}.}

\usage{
mapArrows(longitude0, latitude0,
                      longitude1=longitude0, latitude1=latitude0,
                      length=0.25, angle=30,
                      code=2, col=par("fg"), lty=par("lty"),
                      lwd=par("lwd"), ...)}

\arguments{
    \item{longitude0, latitude0}{starting points for arrows}
    \item{longitude1, latitude1}{ending points for arrows}
    \item{length}{length of the arrow heads, passed to \code{\link{arrows}}}
    \item{angle}{angle of the arrow heads, passed to \code{\link{arrows}}}
    \item{code}{numerical code indicating the type of arrows, passed to \code{\link{arrows}}}
    \item{col}{arrow colour, passed to \code{\link{arrows}}}
    \item{lty}{arrow line type, passed to \code{\link{arrows}}}
    \item{lwd}{arrow line width, passed to \code{\link{arrows}}}
    \item{\dots}{optional arguments passed to \code{\link{arrows}}}
}

\details{Adds arrows to an existing map, by analogy to \code{\link{arrows}}.}

\seealso{See \code{\link{mapPlot}} for general information on plotting maps,
    including other functions.}

\examples{
\dontrun{
library(oce)
data(coastlineWorld)
mapPlot(coastlineWorld, longitudelim=c(-130,-55), latitudelim=c(35,60),
        proj="lambert", parameters=c(lat0=40,lat1=60),
        orientation=c(90,-100,0))
lon <- seq(-120, -75, 15)
n <- length(lon)
lat <- 45 + rep(0, n)
# Draw meridional arrows in N America, from 45N to 60N.
mapArrows(lon, lat, lon, lat+15, length=0.05, col="blue")
}
}

\author{Dan Kelley (see \code{\link{lonlat2map}} for a note on the authors of
    PROJ.4).}

\keyword{hplot}

