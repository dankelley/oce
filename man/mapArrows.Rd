% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map.R
\name{mapArrows}
\alias{mapArrows}
\title{Add Arrows to a Map}
\usage{
mapArrows(
  longitude0,
  latitude0,
  longitude1 = longitude0,
  latitude1 = latitude0,
  length = 0.25,
  angle = 30,
  code = 2,
  col = par("fg"),
  lty = par("lty"),
  lwd = par("lwd"),
  ...
)
}
\arguments{
\item{longitude0, latitude0}{starting points for arrows.}

\item{longitude1, latitude1}{ending points for arrows.}

\item{length}{length of the arrow heads, passed to \code{\link[=arrows]{arrows()}}.}

\item{angle}{angle of the arrow heads, passed to \code{\link[=arrows]{arrows()}}.}

\item{code}{numerical code indicating the type of arrows, passed to \code{\link[=arrows]{arrows()}}.}

\item{col}{arrow color, passed to \code{\link[=arrows]{arrows()}}.}

\item{lty}{arrow line type, passed to \code{\link[=arrows]{arrows()}}.}

\item{lwd}{arrow line width, passed to \code{\link[=arrows]{arrows()}}.}

\item{...}{optional arguments passed to \code{\link[=arrows]{arrows()}}.}
}
\description{
Plot arrows on an existing map, e.g. to indicate a place location.
This is not well-suited for drawing direction fields, e.g. of
velocities; for that, see \code{\link[=mapDirectionField]{mapDirectionField()}}.
Adds arrows to an existing map, by analogy to \code{\link[=arrows]{arrows()}}.
}
\examples{
\donttest{
library(oce)
data(coastlineWorld)
mapPlot(coastlineWorld,
    longitudelim = c(-120, -60), latitudelim = c(30, 60),
    col = "lightgray", projection = "+proj=lcc +lat_1=45 +lon_0=-100"
)
lon <- seq(-120, -75, 15)
n <- length(lon)
lat <- 45 + rep(0, n)
# Draw meridional arrows in N America, from 45N to 60N.
mapArrows(lon, lat, lon, lat + 15, length = 0.05, col = "blue")
}

}
\seealso{
A map must first have been created with \code{\link[=mapPlot]{mapPlot()}}.

Other functions related to maps: 
\code{\link{formatPosition}()},
\code{\link{lonlat2map}()},
\code{\link{lonlat2utm}()},
\code{\link{map2lonlat}()},
\code{\link{mapAxis}()},
\code{\link{mapContour}()},
\code{\link{mapCoordinateSystem}()},
\code{\link{mapDirectionField}()},
\code{\link{mapGrid}()},
\code{\link{mapImage}()},
\code{\link{mapLines}()},
\code{\link{mapLocator}()},
\code{\link{mapLongitudeLatitudeXY}()},
\code{\link{mapPlot}()},
\code{\link{mapPoints}()},
\code{\link{mapPolygon}()},
\code{\link{mapScalebar}()},
\code{\link{mapText}()},
\code{\link{mapTissot}()},
\code{\link{oceCRS}()},
\code{\link{oceProject}()},
\code{\link{shiftLongitude}()},
\code{\link{usrLonLat}()},
\code{\link{utm2lonlat}()}
}
\author{
Dan Kelley
}
\concept{functions related to maps}
