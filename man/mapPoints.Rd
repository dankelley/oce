\name{mapPoints}

\alias{mapPoints}

\title{Plot points on a existing map}

\description{Plot points on an existing map}

\usage{mapPoints(longitude, latitude, debug=getOption("oceDebug"), \dots)}

\arguments{

    \item{longitude}{longitudes of points to be plotted, or an object from
        which longitude and latitude can be inferred in which case the
        following two arguments are ignored.  This objects that are possible
        include those of type \code{coastline}.}
    
    \item{latitude}{latitudes of points to be plotted}

    \item{debug}{a flag that turns on debugging.  Set to 1 to get a moderate
        amount of debugging information, or to 2 to get more.} 

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
        projection="+proj=merc", orientation=c(90,0,0))
data(section)
mapPoints(section)
}
}

\author{Dan Kelley}

\keyword{hplot}

