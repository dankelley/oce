\name{as.coastline}
\alias{as.coastline}
\title{Coerce data into coastline dataset}
\description{
  Coerces a sequence of latitudes and longitudes into a coastline dataset.
}
\usage{as.coastline(latitude, longitude, fillable)}
\arguments{
  \item{latitude}{the latitude in decimal degrees, positive north of the
      equator, or a data frame with columns named \code{latitude} and
      \code{longitude}, in which case these values are extracted from the data
      frame and the second argument is ignored.}
  \item{longitude}{the longitude in decimal degrees, positive north of Greenwich.}
  \item{fillable}{boolean indicating whether the coastline can be drawn as a filled polygon.}
}
\details{
This may be used when \code{\link{read.coastline}} cannot read a file, or when
the data have been manipulated.
}

\value{An object of \code{\link[base]{class}} \code{"coastline"} (for details, see \code{\link{read.coastline}}).}

\seealso{The documentation for \code{\link{coastline-class}} explains the
    structure of coastline objects, and also outlines the other functions
    dealing with them.}

\examples{
\dontrun{
d<-read.table("bbasin.dat",header=FALSE) # dataset not provided with oce
HH <-c(1645:2105) # main outline of Halifax Harbour
GI <-c(4866:4885) # George's Island
coastlineHalifax <- as.coastline(latitude[c(HH,GI)],longitude[c(HH,GI)]-360)
}
}
\references{
The NOAA site \url{http://www.ngdc.noaa.gov/mgg/shorelines/shorelines.html}
is a good source for coastline data files.
}
\author{Dan Kelley}
\keyword{misc}
