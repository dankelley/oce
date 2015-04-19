\name{mapLines}

\alias{mapLines}

\title{Plot lines on a existing map}

\description{Plot lines on an existing map}

\usage{mapLines(longitude, latitude, greatCircle=FALSE, \dots)}

\arguments{

    \item{longitude}{longitudes of points to be plotted, or an object from
        which longitude and latitude can be inferred (e.g. a coastline file, or
        the return value from \code{\link{mapLocator}}), in which case the
        following two arguments are ignored.}

    \item{latitude}{latitudes of points to be plotted}

    \item{greatCircle}{boolean indicating whether to render line segments as
        great circles.  (Ignored at present.)}

    \item{\dots}{optional arguments passed to \code{\link{lines}}.}
}

\details{Adds lines to an existing map, by analogy to \code{\link{lines}}.}

\seealso{See \code{\link{mapPlot}} for general information on plotting maps,
    including other functions.}

\examples{
\dontrun{
library(oce)
data(coastlineWorld)
mapPlot(coastlineWorld, type='l',
        longitudelim=c(-80,10), latitudelim=c(0,120),
        projection="+proj=ortho")
lon <- c(-63.5744, 0.1062)             # Halifax CA to London UK
lat <- c(44.6479, 51.5171)
mapLines(lon, lat, col='red')
}
}

\author{Dan Kelley}

\keyword{hplot}

