% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map.R
\name{lonlat2utm}
\alias{lonlat2utm}
\title{Convert Longitude and Latitude to UTM}
\usage{
lonlat2utm(longitude, latitude, zone, km = FALSE)
}
\arguments{
\item{longitude}{numeric vector of decimal longitude.  May also be
a list containing items named \code{longitude} and \code{latitude}, in which
case the indicated values are used, and next argument is ignored.}

\item{latitude}{numeric vector of decimal latitude (ignored if
\code{longitude} is a list containing both coordinates)}

\item{zone}{optional indication of UTM zone.  Normally this is inferred from
the longitude, but specifying it can be helpful in dealing with Landsat
images, which may cross zones and which therefore are described by a single
zone.}

\item{km}{logical value indicating whether \code{easting} and \code{northing}
are in kilometers or meters.}
}
\value{
\code{lonlat2utm} returns a list containing \code{easting},
\code{northing}, \code{zone} and \code{hemisphere}.
}
\description{
Convert Longitude and Latitude to UTM
}
\examples{
library(oce)
# Cape Split, in the Minas Basin of the Bay of Fundy
lonlat2utm(-64.496567, 45.334626)

}
\references{
\verb{https://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system},
downloaded May 31, 2014.
}
\seealso{
\code{\link[=utm2lonlat]{utm2lonlat()}} does the inverse operation.  For general
projections and their inverses, use \code{\link[=lonlat2map]{lonlat2map()}} and
\code{\link[=map2lonlat]{map2lonlat()}}.

Other functions related to maps: 
\code{\link{formatPosition}()},
\code{\link{lonlat2map}()},
\code{\link{map2lonlat}()},
\code{\link{mapArrows}()},
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
