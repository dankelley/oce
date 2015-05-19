\name{mapLongitudeLatitudeXY}

\alias{mapLongitudeLatitudeXY}

\title{Convert from longitude and latitude to x and y}

\description{Find (x,y) values corresponding to (longitude, latitude) values,
    using the present projection.}

\usage{mapLongitudeLatitudeXY(longitude, latitude)}

\arguments{

    \item{longitude}{longitudes of points, or an object from which latitude and
        longitude can be inferred (e.g. a coastline file, or the return value
        from \code{\link{mapLocator}}), in which case the following two
        arguments are ignored.}
    
    \item{latitude}{latitudes of points, needed only if they cannot be inferred
        from from the first argument}
}


\details{This is mainly a wrapper around \code{mapproject}.}

\value{A list containing \code{x} and \code{y}.}

\seealso{See \code{\link{mapPlot}} for general information on plotting maps,
    including other functions.}

\examples{
\dontrun{
library(oce)
data(coastlineWorld)
par(mfrow=c(2,1), mar=rep(2, 4))
mapPlot(coastlineWorld, projection="+proj=moll") # sets a projection
xy <- mapLongitudeLatitudeXY(coastlineWorld)
plot(xy, type='l', asp=1)
}
}

\author{Dan Kelley (see \code{\link{lonlat2map}} for a note on the authors of
    PROJ.4).}

\keyword{hplot}

