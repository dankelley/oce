\name{mapPlot}

\alias{mapPlot}

\title{Plot a map}

\description{Plot a map}

\usage{mapPlot(longitude, latitude, longitudelim, latitudelim, grid,
       projection="mercator", parameters=NULL, orientation=NULL,
       \dots)
}

\arguments{
    \item{longitude}{longitudes of points to be plotted}
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
        projection="orthographic", orientation=c(45,-100,0))
}

\author{Dan Kelley}
\keyword{hplot}
