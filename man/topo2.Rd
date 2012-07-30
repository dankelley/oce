\name{topo2}

\docType{data}

\alias{topo2}

\title{World topograph data, on a 2-degree grid.}

\description{World topograph data, on a 2-degree grid.}

\usage{data(topo2)}

\examples{
library(oce)
data(topo2)
# Image with default axes
imagep(topo2)
x <- seq(.25, 358.25, 2)
y <- seq(-89.75, 88.25, 2)
# Coastline with sensible axes
contour(x, y, topo2, level=0, drawlabels=FALSE)
}

\source{The data are calculated from the \code{z} entry in the
    \code{\link{topoWorld}} data, smoothed with \code{\link{matrixSmooth}} with
    \code{passes=2}, and then subsetted to 2 degrees of latitude and
    longitude.} 

\keyword{datasets}
