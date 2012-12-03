\name{mapPlot}

\alias{mapPlot}

\title{Plot a map}

\description{Plot a map}

\usage{mapPlot(latitude, longitude, latitudelim, longitudelim,
       projection="mercator", parameters=NULL, orientation=NULL,
       \dots)
}

\arguments{
    \item{latitude}{latitudes of points to be plotted}
    \item{longitude}{longitudes of points to be plotted}
    \item{latitudelim}{optional limit of latitudes to plot}
    \item{longitudelim}{optional limit of longitudes to plot}
    \item{projection}{projection; see \code{\link{mapproject}}.}
    \item{parameters}{parameters for projection; see \code{\link{mapproject}}.}
    \item{orientation}{orientation for projection; see \code{\link{mapproject}}.}
    \item{\dots}{optional arguments passed to plotting functions.}
}

\details{Creates a map using the indicated projection.}

\seealso{Points may be added to a map with \code{\link{mapPoints}}.}

\examples{
library(oce)
data(coastlineWorld)
longitude <- coastlineWorld[['longitude']]
latitude <- coastlineWorld[['latitude']]
mapPlot(latitude, longitude, type='l', latitudelim=c(0,120), longitudelim=c(-80,10), projection="orthographic", orientation=c(45,-100,0))
}

\author{Dan Kelley}
\keyword{hplot}
