\name{mapLines}

\alias{mapLines}

\title{Plot lines on a existing map}

\description{Plot lines on an existing map}

\usage{mapLines(longitude, latitude, \dots)}

\arguments{
    \item{longitude}{longitudes of points to be plotted}
    \item{latitude}{latitudes of points to be plotted}
    \item{\dots}{optional arguments passed to \code{\link{lines}}.}
}

\details{Adds lines to an existing map, by analogy to \code{\link{lines}}.}

\section{Bugs}{This should use \code{\link{approx}} to fill in multiple
    segments within the line, so that e.g. great circle lines will be curved.}

\seealso{See \code{\link{mapPlot}} for general information on plotting maps,
    including other functions.}

\examples{
library(oce)
data(coastlineWorld)
longitude <- coastlineWorld[['longitude']]
latitude <- coastlineWorld[['latitude']]
mapPlot(longitude, latitude, type='l',
        longitudelim=c(-80,10), latitudelim=c(0,120),
        projection="orthographic", orientation=c(45,-100,0))
lon <- c(-63.5744, 0.1062)             # Halifax CA to London UK
lat <- c(44.6479, 51.5171)
mapLines(lon, lat, col='red')
}

\author{Dan Kelley}
\keyword{hplot}
