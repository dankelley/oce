\name{utm2lonlat}

\alias{utm2lonlat}

\title{Convert from UTM to longitude and latitude}

\description{Convert from UTM to longitude and latitude}

\usage{utm2lonlat(easting, northing, zone=1, hemisphere="N", km=FALSE)}

\arguments{
  \item{easting}{easting coordinate (in km or m, depending on value of
      \code{km}).  Alternatively, a list containing items named \code{easting},
      \code{northing}, and \code{zone}, in which case these are taken from the
      list and the arguments named \code{northing}, \code{zone} and are
      ignored.}
  \item{northing}{northing coordinate (in km or m, depending on value of
      \code{km}).}
  \item{zone}{UTM zone}
  \item{hemisphere}{indication of hemisphere; \code{"N"} for North, anything
      else for South.}
  \item{km}{logical value indicating whether \code{easting} and \code{northing}
      are in kilometers or meters.}
}

\value{A list containing \code{longitude} and \code{latitude}.}

\examples{
library(oce)
## Cape Split, in the Minas Basin of the Bay of Fundy
utm2lonlat(852863, 5029997, 19)
}

\seealso{\code{\link{lonlat2utm}} does the inverse operation.  For general
    projections and their inverses, use \code{\link{lonlat2map}} and
    \code{\link{map2lonlat}}.}

\references{\url{http://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system},
    downloaded May 31, 2014.}

\author{Dan Kelley}

\keyword{misc}

