\name{mapPlot}

\alias{mapPlot}

\title{Plot a map}

\description{Plot a map}

\usage{mapPlot(longitude, latitude, longitudelim, latitudelim, grid,
       projection="mercator", parameters=NULL, orientation=NULL,
       \dots)
}

\arguments{
    \item{longitude}{longitudes of points to be plotted, or a coastline object
        from which longitude and latitude are to be taken (see e.g.
        \code{\link{read.coastline}}).}
    \item{latitude}{latitudes of points to be plotted}
    \item{longitudelim}{optional limit of longitudes to plot}
    \item{latitudelim}{optional limit of latitudes to plot}
    \item{grid}{if \code{TRUE}, draw an auto-scaled lat-lon grid; if a number,
        draw a grid with the indicated spacing, in degrees.}
    \item{projection}{projection; see \code{\link{mapproject}}.}
    \item{parameters}{parameters for projection; see \code{\link{mapproject}}.}
    \item{orientation}{orientation for projection; see \code{\link{mapproject}}.}
    \item{\dots}{optional arguments passed to plotting functions.}
}

\details{Creates a map using the indicated projection.}

\seealso{Points may be added to a map with \code{\link{mapPoints}}, lines may
    be added with \code{\link{mapLines}}, and text with \code{\link{mapText}}.}

\examples{
library(oce)
data(coastlineWorld)
longitude <- coastlineWorld[['longitude']]
latitude <- coastlineWorld[['latitude']]
mapPlot(longitude, latitude, type='l',
        latitudelim=c(0,120), longitudelim=c(-80,10),
        grid=10,
        projection="orthographic", orientation=c(45,-100,0))

## Illustrate the Lambert conformal conic projection,
## said by Snyder (page 34) to be suitable for regions
## of large east-west extent away from the equator.
## Here, show USA and Canada.
mapPlot(coastlineWorld, type='l',
        latitudelim=c(30,60), longitudelim=c(-130,-50),
        proj="lambert",
        parameters=c(lat0=40,lat1=60),
        orientation=c(90,-90,0), grid=TRUE)

## Similar looking map, with the polyconic projection
mapPlot(coastlineWorld, type='l',
        latitudelim=c(30,60), longitudelim=c(-130,-50),
        proj="polyconic", orientation=c(90, -90,0), grid=TRUE)
}

\author{Dan Kelley}

\references{Snyder, John P., 1987.  Map Projections: A Working Manual.  USGS
    Professional Paper: 1395 (available at
    \url{pubs.usgs.gov/pp/1395/report.pdf}). }

\keyword{hplot}
