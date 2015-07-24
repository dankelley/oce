% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{topoWorld}

\docType{data}

\alias{topoWorld}

\title{Global topographic dataset at half-degree resolution}

\description{Global topographic dataset at half-degree resolution, created by
  decimating the ETOPO5 dataset.}

\details{The coarse resolution can be a problem in plotting depth contours along
  with coastlines in regions of steep topography. For example, near the
  southeast corner of Newfoundland, a 200m contour will overlap a coastline
  drawn with \code{\link[ocedata]{coastlineWorldFine}}. The solution in such
  cases is to download a higher-resolution topography file and create a
  \code{topo} object with \code{\link{read.topo}} or \code{\link{as.topo}}.}

\usage{data(topoWorld)}

\source{The ETOPO5 dataset was downloaded in late 2009 from the NOAA website,
  and decimated from 1/12th degree resolution to 1/2 degree resolution.}

\references{\url{http://www.ngdc.noaa.gov/mgg/global/relief/ETOPO5/TOPO/ETOPO5/}}

\examples{
\dontrun{
library(oce)
data(topoWorld) 
plot(topoWorld, location=NULL)
}
}

\seealso{Created with \code{\link{as.topo}}.}

\author{Dan Kelley}

\keyword{datasets}

