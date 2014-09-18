\name{mapGrid}

\alias{mapGrid}

\title{Plot longitude and latitude grid on an existing map}

\description{Plot longitude and latitude grid on an existing map}

\usage{mapGrid(dlongitude, dlatitude, longitude, latitude,
       col="darkgray", lty="solid", lwd=0.5*par("lwd"), polarCircle=0)}


\arguments{
    \item{dlongitude}{increment in longitude, used in sequence from -180 to 180.}

    \item{dlatitude}{increment in latitude, used in sequence from -90 to 90.}

    \item{longitude}{list of longitudes}
    
    \item{latitude}{list of latitudes}

    \item{polarCircle}{a number indicating the number of degrees of latitude
        extending from the poles, within which zones are not drawn.}

    \item{col}{colour}

    \item{lty}{line type}

    \item{lwd}{line width}

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
\dontrun{
library(oce)
data(coastlineWorld)
mapPlot(coastlineWorld, type='l', grid=FALSE,
    longitudelim=c(-80,10), latitudelim=c(0,120),
    projection="orthographic", orientation=c(45,-100,0))
mapGrid(15, 15, polarCircle=15)
}
}

\author{Dan Kelley}
\keyword{hplot}
