\name{mapZones}

\alias{mapZones}

\title{Plot zones on an existing map [DEPRECATED]}

\description{Plot zones (lines of constant longitude) on a existing map.
    \emph{DEPRECATED--will be removed by end Sept 2014}}


\usage{mapZones(longitude, polarCircle=0, lty='solid', lwd=0.5*par('lwd'), col='darkgray', \dots)}

\arguments{

    \item{longitude}{either a boolean indicating whether to draw a zonal grid,
        or a vector of longitudes at which to draw zones.}

    \item{polarCircle}{a number indicating the number of degrees of latitude
        extending from the poles, within which zones are not drawn.}

    \item{lty}{line type}

    \item{lwd}{line width}

    \item{col}{colour}

    \item{\dots}{optional arguments passed to \code{\link{lines}}.}
}

\details{Zones that will not fit in the plotting space are ignored.}

\section{Bugs}{This should use \code{\link{approx}} to fill in multiple
    segments within the line, so that e.g. great circle lines will be curved.}

\seealso{See \code{\link{mapPlot}} for general information on plotting maps,
    including other functions.}

\examples{
\dontrun{
library(oce)
data(coastlineWorld)
longitude <- coastlineWorld[['longitude']]
latitude <- coastlineWorld[['latitude']]
mapPlot(longitude, latitude, type='l', grid=FALSE,
longitudelim=c(-80,10), latitudelim=c(0,120),
projection="orthographic", orientation=c(45,-100,0))
mapZones()
}
}

\author{Dan Kelley (see \code{\link{lonlat2map}} for a note on the authors of
    PROJ.4).}

\keyword{hplot}
