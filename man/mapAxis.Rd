\name{mapAxis}

\alias{mapAxis}

\title{Plot axis labels on an existing map}

\description{Plot axis labels on an existing map}

\usage{mapAxis(side=1:2, longitude, latitude, debug=getOption("oceDebug"))}

\arguments{

    \item{side}{the side at which labels are to be drawn.  If not provided,
        sides 1 and 2 will be used (i.e. bottom and left-hand sides).}

    \item{longitude}{vector of longitudes to indicate.  If not provided, and if
        a grid has already been drawn, then the labels will be at the
        interesections of the grid lines with the plotting box.}

    \item{latitude}{vector of latitudes to indicate.  If not provided, and if a
        grid has already been drawn, then the labels will be at the
        interesections of the grid lines with the plotting box.}
   
    \item{debug}{a flag that turns on debugging.  Set to 1 to get a moderate
        amount of debugging information, or to 2 to get more.} 

}

\details{This function is still in development, and the argument list as well
    as the action taken are both subject to change, hence the brevity of this
    help page.

Note that if a grid line crosses the axis twice, only one label will be drawn.

}

\seealso{See \code{\link{mapPlot}} for general information on plotting maps,
    including other functions.}

\examples{
library(oce)
data(coastlineWorld)
par(mar=c(2, 2, 3, 1))
lonlim <- c(-180, 180)
latlim <- c(60, 120)
p <- '+proj=stere +lat_0=90'
mapPlot(coastlineWorld, projection=p,
        longitudelim=lonlim, latitudelim=latlim,
        grid=FALSE) # grid=FALSE will soon become the default
mapGrid(15, 15, polarCircle=1/2)
mapAxis()
}

\author{Dan Kelley}

\keyword{hplot}
