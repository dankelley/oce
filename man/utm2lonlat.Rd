\name{utm}

\alias{utm2lonlat}
\alias{lonlat2utm}

\title{Convert between lat-lon and UTM}

\description{Convert between lat-lon and UTM coordinates}

\usage{
utm2lonlat(easting, northing, zone=1, hemisphere="N", km=TRUE)
lonlat2utm(longitude, latitude, km=TRUE)
}

\arguments{
  \item{easting}{easting coordinate (in km or m, depending on value of
      \code{km})}
  \item{northing}{northing coordinate (in km or m, depending on value of
      \code{km})}
  \item{zone}{UTM zone}
  \item{longitude}{decimal longitude}
  \item{latitude}{decimal latitude}
  \item{km}{logical value indicating whether \code{easting} and \code{northing}
      are in kilometers or meters.}
}

\value{For \code{utm2lonlat}, a list containing \code{longitude} and
    \code{latitude}. For \code{lonlat2utm}, a list containing \code{easting},
    \code{northing}, \code{zone} and \code{hemisphere}.}

\examples{
library(oce)
longitude <- -59.56594
latitude <- 45.62391
easting <- 767.700
northing <- 5058.000
zone <- 20
lonlat <- utm2lonlat(easting, northing, zone, "N")
lonlat2utm(lonlat$longitude, lonlat$latitude)
}

\references{\url{http://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system},
    downloaded May 31, 2014.}

\author{Dan Kelley}

\keyword{misc}

