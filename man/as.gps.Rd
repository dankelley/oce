\name{as.gps}

\alias{as.gps}

\title{Coerce data into a GPS dataset}

\description{Coerces a sequence of longitudes and latitudes into a GPS dataset.}

\usage{as.gps(longitude, latitude, filename="")}

\arguments{
  \item{longitude}{the longitude in decimal degrees, positive east of
      Greenwich, or a data frame with columns named \code{latitude} and
      \code{longitude}, in which case these values are extracted from the data
      frame and the second argument is ignored.}
  \item{latitude}{the latitude in decimal degrees, positive north of the Equator.}
  \item{filename}{name of file containing data (if applicable).}
}

\details{This may be used when \code{\link{read.gps}} cannot read a file, or
    when the data have been manipulated. }

\value{An object of \code{\link[base]{class}} \code{"gps"} (for details, see
    \code{\link{read.gps}}).}

\seealso{The documentation for \code{\link{gps-class}} explains the
    structure of gps objects, and also outlines the other functions
    dealing with them.}

\examples{
    library(oce)
    data(coastlineHalifax)
    lat <- coastlineHalifax[["latitude"]]
    lon <- coastlineHalifax[["longitude"]]
    gps <- as.gps(lon, lat, "-")
    plot(coastlineHalifax)
    points(gps[["longitude"]], gps[["latitude"]])
}

\references{The GPX format is described at
    \url{http://www.topografix.com/GPX/1/1/}.}

\author{Dan Kelley}

\keyword{misc}
