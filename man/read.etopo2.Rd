\name{read.etopo2}
\alias{read.etopo2}
\title{Read an ETOPO2 topography file}
\description{Read a file that contains topographic data in the ETOPO2 dataset, 
as provided by the USGS website.}
\usage{read.etopo2(filename)}
\arguments{
  \item{filename}{name of a file containing an ETOPO2 dataset.}
}
\value{A list containing \code{lat}, latitude in degrees north of the equator,
\code{lon}, longitude in degrees east of Greenwich (negative for the western
hemisphere), and \code{z}, a matrix of height in metres (negative in the ocean).}

\examples{
\dontrun{
library(oce)
bof <- read.topo("dek02_3864.asc")
plot(etopo2)
}
}
\references{
\url{http://www.ngdc.noaa.gov/mgg/gdas/gd_designagrid.html}
}
\author{Dan Kelley}
\keyword{misc}
