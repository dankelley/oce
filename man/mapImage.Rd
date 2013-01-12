\name{mapImage}

\alias{mapImage}

\title{Plot an image on a existing map}

\description{Plot an image on an existing map}

\usage{mapImage(longitude, latitude, z, zlim, breaks, col)}

\arguments{

    \item{longitude}{longitudes of grid lines}

    \item{latitude}{latitudes of grid lines}

    \item{z}{matrix for image}
    
    \item{zlim}{limit for z (colour)}

    \item{breaks}{the z values for breaks in the colour scheme.  If this is of
        length 1, the value indicates the desired number of breaks, which is
        supplied to \code{\link{pretty}}, in determining clean break points.}

    \item{col}{either a vector of colours corresponding to the breaks, of length
        1 plus the number of breaks, or a function specifying colours,
        e.g. \code{\link{oceColorsJet}} for a rainbow.}
}

\details{Adds an image to an existing map, by analogy to \code{\link{image}}.}

\section{Bugs}{More arguments need to be added, e.g. for the colour mapping, etc.}

\seealso{See \code{\link{mapPlot}} for general information on plotting maps,
    including other functions.}

\examples{
\dontrun{
library(oce)
data(coastlineWorld)
mapPlot(coastlineWorld, type='l',
        latitudelim=c(40,50), longitudelim=c(-70,-50),
        proj="polyconic", orientation=c(90, -60,0), grid=TRUE)
data(topoWorld)
mapImage(topoWorld, col=oceColorsGebco)
mapMeridians(10, lty='dotted', col='darkgray')
mapZones(10, lty='dotted', col='darkgray')
mapLines(coastlineWorld)

## Northern polar region, with colour-coded bathymetry
mapPlot(coastlineWorld, type='l',
        latitudelim=c(60,120), longitudelim=c(-180,180),
        proj="stereographic", grid=TRUE)
data(topoWorld)
mapImage(topoWorld,
         col=rev(oceColorsGebco(50)), breaks=seq(-5000,0,length.out=50))
##mapImage(topoWorld,
##         col=oceColorsJet(50), breaks=seq(-4000,0,length.out=50))
mapMeridians()
mapZones()
mapLines(coastlineWorld[['longitude']], coastlineWorld[['latitude']])
}
}

\author{Dan Kelley}
\keyword{hplot}
