\name{read.etopo2}
\alias{read.etopo2}
\title{Read an ETOPO2 topography file}
\description{Read a file that contains topographic data in the ETOPO2 dataset, 
  as provided by the NOAA website.}
\usage{read.etopo2(filename)}
\arguments{
  \item{filename}{name of a file containing an ETOPO2 dataset.}
}
\value{An object of type \code{\link[base]{class}} \code{"etopo2"},
  which is a list containing the following items.
  \item{data}{a data frame containing \code{lat},
    \code{lon}, and \code{z}}
  \item{metadata}{a list containing the source filename}
  \item{processing.log}{a processing log, in the standard \code{oce}
    format.}
}

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
