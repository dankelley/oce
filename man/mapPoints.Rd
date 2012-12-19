\name{mapPoints}

\alias{mapPoints}

\title{Plot points on a existing map}

\description{Plot points on an existing map}

\usage{mapPoints(longitude, latitude, \dots)}

\arguments{

    \item{longitude}{longitudes of points to be plotted, or an object with a
        slot named \code{data} that contains items named \code{longitude} and
        \code{latitude} (e.g. of class \code{coastline}; see
        \code{\link{coastline-class}}), in which case position is inferred from
        the object and the second argument to this function is ignored.}

    \item{latitude}{latitudes of points to be plotted}

    \item{\dots}{optional arguments passed to \code{\link{points}}.}
}

\details{Adds points to an existing map, by analogy to \code{\link{points}}.}

\seealso{See \code{\link{mapPlot}} for general information on plotting maps,
    including other functions.}

\examples{
\dontrun{
library(oce)
data(coastlineWorld)
mapPlot(coastlineWorld, type='l', grid=TRUE,
        longitudelim=c(-80,0), latitudelim=c(20,50),
        projection="mercator", orientation=c(90,-40,0))
data(a03)
lon <- a03[["longitude", 'byStation']]
lat <- a03[["latitude", 'byStation']]
mapPoints(lon, lat, pch=20, col='red')
}
}

\author{Dan Kelley}
\keyword{hplot}
