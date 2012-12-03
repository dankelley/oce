\name{mapLines}

\alias{mapLines}

\title{Plot lines on a existing map}

\description{Plot lines on an existing map}

\usage{mapLines(latitude, longitude, \dots)
}

\arguments{
    \item{latitude}{latitudes of points to be plotted}
    \item{longitude}{longitudes of points to be plotted}
    \item{\dots}{optional arguments passed to \code{\link{lines}}.}
}

\details{Adds lines to an existing map, by analogy to \code{\link{lines}}.}

\section{Bugs}{This should use \code{\link{approx}} to fill in multiple
    segments within the line, so that e.g. great circle lines will be curved.}

\seealso{Maps are created with \code{\link{mapPlot}}, and points may be added
    to maps with \code{\link{mapPoints}}.}

\examples{
library(oce)
data(coastlineWorld)
longitude <- coastlineWorld[['longitude']]
latitude <- coastlineWorld[['latitude']]
mapPlot(latitude, longitude, type='l',
        latitudelim=c(0,120), longitudelim=c(-80,10),
        projection="orthographic", orientation=c(45,-100,0))
lon <- c(-63.5744, 0.1062)             # Halifax CA to London UK
lat <- c(44.6479, 51.5171)
mapLines(lat, lon, col='red')
}

\author{Dan Kelley}
\keyword{hplot}
