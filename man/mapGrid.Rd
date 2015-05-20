\name{mapGrid}

\alias{mapGrid}

\title{Plot longitude and latitude grid on an existing map}

\description{Plot longitude and latitude grid on an existing map}

\usage{mapGrid(dlongitude=15, dlatitude=15, longitude, latitude,
       col="darkgray", lty="solid", lwd=0.5*par("lwd"), polarCircle=0,
       debug=getOption("oceDebug"))}


\arguments{

    \item{dlongitude}{increment in longitude, ignored if \code{longitude}
        is supplied, but otherwise determines the longitude sequence.}

    \item{dlatitude}{increment in latitude, ignored if \code{latitude}
        is supplied, but otherwise determines the latitude sequence.}

    \item{longitude}{vector of longitudes, or \code{NULL} to prevent drawing
        longitude lines.}
    
    \item{latitude}{vector of latitudes, or \code{NULL} to prevent drawing
        latitude lines.}
    
    \item{col}{colour of lines}

    \item{lty}{line type}

    \item{lwd}{line width}

    \item{polarCircle}{a number indicating the number of degrees of latitude
        extending from the poles, within which zones are not drawn.}
    
    \item{debug}{a flag that turns on debugging.  Set to 1 to get a moderate
        amount of debugging information, or to 2 to get more.} 

}

\details{This is somewhat analogous to \code{\link{grid}}, except that the
    first two arguments of the latter supply the number of lines in the grid,
    whereas the present function has increments for the first two arguments.}

\section{Plans}{At the moment, the function cannot determine which lines might
    work with labels on axes, but this could perhaps be added later, making
    this more analogous with \code{\link{grid}}.}

\seealso{See \code{\link{mapPlot}} for general information on plotting maps,
    including other functions.}

\examples{
library(oce)
data(coastlineWorld)
mapPlot(coastlineWorld, type='l', grid=FALSE,
    longitudelim=c(-80,10), latitudelim=c(0,120),
    projection="+proj=ortho")
mapGrid(15, 15, polarCircle=15)
}

\author{Dan Kelley (see \code{\link{lonlat2map}} for a note on the authors of
    PROJ.4).}


\keyword{hplot}
