\name{read.topo}

\alias{read.topo}

\title{Read an topography file}

\description{Read a file that contains topographic data in the ETOPO2 dataset,
    as provided by the NOAA website [1].}

\usage{read.topo(file, \dots)}

\arguments{
  \item{file}{name of a file containing an ETOPO2 dataset.}
  \item{\dots}{additional arguments, passed to called routines.}
}

\value{An object of type \code{\link[base]{class}} \code{"topo"},
  which is a list containing the following items.
  \item{data}{a data frame containing \code{lat},
    \code{lon}, and \code{z}}
  \item{metadata}{a list containing the source filename}
  \item{processingLog}{a processingLog, in the standard \code{oce} format.}
}

\examples{
\dontrun{
library(oce)
topoMaritimes <- read.topo("topoMaritimes.asc")
plot(topographyMaritimes)
}
}

\references{[1] NOAA website that provides datasets:
    \url{http://maps.ngdc.noaa.gov/viewers/wcs-client} with the \code{ArcGIS
        ASCII Grid} menu item selected.}

\author{Dan Kelley}

\keyword{misc}
