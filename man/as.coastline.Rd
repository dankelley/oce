\name{as.coastline}

\alias{as.coastline}

\title{Coerce data into coastline dataset}

\description{Coerces a sequence of longitudes and latitudes into a coastline dataset.}

\usage{as.coastline(longitude, latitude, fillable)}

\arguments{
  \item{longitude}{the longitude in decimal degrees, positive east of
      Greenwich, or a data frame with columns named \code{latitude} and
      \code{longitude}, in which case these values are extracted from the data
      frame and the second argument is ignored.}
  \item{latitude}{the latitude in decimal degrees, positive north of the Equator.}
  \item{fillable}{boolean indicating whether the coastline can be drawn as a filled polygon.}
}

\details{This may be used when \code{\link{read.coastline}} cannot read a file, or when
    the data have been manipulated. }

\value{An object of \code{\link[base]{class}} \code{"coastline"} (for details, see \code{\link{read.coastline}}).}

\seealso{The documentation for \code{\link{coastline-class}} explains the
    structure of coastline objects, and also outlines the other functions
    dealing with them.}

\references{
The NOAA site \url{http://www.ngdc.noaa.gov/mgg/shorelines/shorelines.html}
is a good source for coastline data files.}

\author{Dan Kelley}

\keyword{misc}
