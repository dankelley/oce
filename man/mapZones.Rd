\name{mapZones}

\alias{mapZones}

\title{Plot zones on an existing map}

\description{Plot zones (lines of constant longitude) on a existing map}

\usage{mapZones(lon, polarCircle=0, \dots)}

\arguments{
    \item{lon}{either a boolean indicating whether to draw a zonal grid, or a
        single number to indicate the increment in zone longitudes, or a vector 
        of longitudes at which to draw zones.}

    \item{polarCircle}{a number indicating the number of degrees of latitude
        extending from the poles, within which zones are not drawn.}

    \item{\dots}{optional arguments passed to \code{\link{lines}}.}
}

\details{Zones that will not fit in the plotting space are ignored.}

\section{Bugs}{This should use \code{\link{approx}} to fill in multiple
    segments within the line, so that e.g. great circle lines will be curved.}

\seealso{See \code{\link{mapPlot}} for general information on plotting maps,
    including other functions.}

\examples{
library(oce)
data(coastlineWorld)
longitude <- coastlineWorld[['longitude']]
latitude <- coastlineWorld[['latitude']]
mapPlot(longitude, latitude, type='l', grid=FALSE,
        longitudelim=c(-80,10), latitudelim=c(0,120),
        projection="orthographic", orientation=c(45,-100,0))
mapZones()
}

\author{Dan Kelley}
\keyword{hplot}
