\name{as.coastline}
\alias{as.coastline}
\title{Coerce data into coastline dataset}
\description{
  Coerces a sequence of latitudes and longitudes into a coastline dataset.
}
\usage{
as.coastline(latitude, longitude)
}
\arguments{
  \item{latitude}{the latitude in decimal degrees, positive north of the equator.}
  \item{longitude}{the longitude in decimal degrees, positive north of Greenwich.}
}
\details{
This may be used when \code{\link{read.coastline}} cannot read a file, or when
the data have been manipulated.
}

\value{A \code{coastline} object as described at \code{\link{read.coastline}}.}

\seealso{
You may also use \code{\link{read.coastline}} to read a coastline file.
Plotting is typically done with \code{\link{plot.coastline}}, although
it is sufficient to just use \code{plot} for coastline objects.
}
\examples{
\dontrun{
d<-read.table("bbasin.dat",header=FALSE) # dataset not provided with oce
HH <-c(1645:2105) # main outline of Halifax Harbour
GI <-c(4866:4885) # George's Island
coastline.halifax<-as.coastline(latitude[c(HH,GI)],longitude[c(HH,GI)]-360)
}
}
\references{
The NOAA site \url{http://www.ngdc.noaa.gov/mgg/shorelines/shorelines.html}
is a good source for coastline data files.
}
\author{Dan Kelley}
\keyword{misc}
