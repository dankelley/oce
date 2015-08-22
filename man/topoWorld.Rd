% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{topoWorld}

\docType{data}

\alias{topoWorld}

\title{Global topographic dataset at half-degree resolution}

\description{Global topographic dataset at half-degree resolution, created by
  decimating the ETOPO5 dataset.  Its longitude ranges from -179.5 to 180, and
  its latitude ranges from -89.5 to 90.  Height is measured in metres above
  nominal sea level.}

\details{The coarse resolution can be a problem in plotting depth contours along
  with coastlines in regions of steep topography. For example, near the
  southeast corner of Newfoundland, a 200m contour will overlap a coastline
  drawn with \code{\link[ocedata]{coastlineWorldFine}}. The solution in such
  cases is to download a higher-resolution topography file and create a
  \code{topo} object with \code{\link{read.topo}} or \code{\link{as.topo}}.}

\usage{data(topoWorld)}

\source{The binary ETOPO5 dataset was downloaded in late 2009 from the NOAA
  website, \url{http://www.ngdc.noaa.gov/mgg/global/relief/ETOPO5/TOPO/ETOPO5/},
  decoded, decimated from 1/12th degree resolution to 1/2 degree resolution, and
  shifted so that longitude runs from negative to positive instead of from 0 to
  360.}

\examples{
\dontrun{
library(oce)
data(topoWorld) 
par(mfrow=c(2,1))
plot(topoWorld, location=NULL)
imagep(topoWorld)
}
}

\seealso{Created with \code{\link{as.topo}}.}

\author{Dan Kelley}

\keyword{datasets}

