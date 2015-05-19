\name{mapText}

\alias{mapText}

\title{Plot text on a existing map}

\description{Plot text on an existing map}

\usage{mapText(longitude, latitude, labels, \dots)}

\arguments{
    \item{longitude}{longitudes of text to be plotted}
    \item{latitude}{latitudes of text to be plotted}
    \item{labels}{labels of text to be plotted}
    \item{\dots}{optional arguments passed to \code{\link{text}}, e.g. \code{adj},
        \code{pos}, etc.}
}

\details{Adds text to an existing map, by analogy to \code{\link{text}}.}

\seealso{See \code{\link{mapPlot}} for general information on plotting maps,
    including other functions.}

\examples{
library(oce)
data(coastlineWorld)
longitude <- coastlineWorld[['longitude']]
latitude <- coastlineWorld[['latitude']]
mapPlot(longitude, latitude, type='l', grid=5,
        longitudelim=c(-70,-50), latitudelim=c(45,50),
        projection="+proj=merc")
lon <- -63.5744 # Halifax
lat <- 44.6479
mapPoints(lon, lat, pch=20, col="red")
mapText(lon, lat, "Halifax", col="red", pos=1, offset=1)
}

\author{Dan Kelley (see \code{\link{lonlat2map}} for a note on the authors of
    PROJ.4).}

\keyword{hplot}

