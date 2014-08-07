\name{lonlat2utm}

\alias{lonlat2utm}

\title{Convert from longitude and latitude to UTM}

\description{Convert from longitude and latitude to UTM coordinates}

\usage{lonlat2utm(longitude, latitude, zone, km=FALSE)}

\arguments{
  \item{longitude}{decimal longitude.  May also be a list containing items
      named \code{longitude} and \code{latitude}, in which case the indicated
      values are used, and next argument is ignored.}
  \item{latitude}{decimal latitude (ignored if \code{longitude} is a list
      containing both coordinates)}
  \item{zone}{optional indication of UTM zone.  Normally this is inferred from
      the longitude, but specifying it can be helpful in dealing with Landsat
      images, which may cross zones and which therefore are described
      by a single zone.}
  \item{km}{logical value indicating whether \code{easting} and \code{northing}
      are in kilometers or meters.}
}

\value{A list containing \code{easting}, \code{northing}, \code{zone} and
    \code{hemisphere}.}

\examples{
library(oce)
## Cape Split, in the Minas Basin of the Bay of Fundy
lonlat2utm(-64.496567, 45.334626)
}

\seealso{\code{\link{utm2lonlat}} does the inverse operation.  For general
    projections and their inverses, use \code{\link{lonlat2map}} and
    \code{\link{map2lonlat}}.}

\references{\url{http://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system},
    downloaded May 31, 2014.}

\author{Dan Kelley}

\keyword{misc}

