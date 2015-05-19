\name{mapTissot}

\alias{mapTissot}

\title{Plot Tissot indicatrices}

\description{Plot ellipses at grid intersection points, as a method for
    indicating the distortion inherent in the projection [1].}

\usage{mapTissot(grid=rep(15, 2), scale=0.2, \dots)}


\arguments{

    \item{grid}{numeric vector of length 2, specifying the increment in
        longitude and latitude for the grid. Indicatrices are drawn at e.g.
        longitudes \code{seq(-180, 180, grid[1])}.}

    \item{scale}{scale factor for ellipses. This is multiplied by
        \code{min(grid)} and the result is the radius of the circle on the
        earth, in latitude degrees.}

    \item{\dots}{extra arguments passed to plotting functions, e.g.
        \code{col="red"} yields red indicatrices.}

}

\details{The purpose and interpretation are outline in [1], but should also be
    self-explanatory.}

\references{1. Snyder, John P., 1987.  Map Projections: A Working Manual.  USGS
    Professional Paper: 1395 (available at
    \url{pubs.usgs.gov/pp/1395/report.pdf}).}

\seealso{See \code{\link{mapPlot}} for general information on plotting maps,
    including other functions.}

\examples{
\dontrun{
library(oce)
data(coastlineWorld)
par(mfrow=c(1,1), mar=c(2, 2, 1, 1))
p  <- "+proj=aea +lat_1=10 +lat_2=60 +lon_0=-45"
mapPlot(coastlineWorld, projection=p, fill="gray",
        longitudelim=c(-90,0), latitudelim=c(0,50))
mapTissot(c(15, 15), col='red')
}
}

\author{Dan Kelley (see \code{\link{lonlat2map}} for a note on the authors of
    PROJ.4).}

\keyword{hplot}
