\name{topo.world}

\docType{data}

\alias{topo.world}

\title{Global topographic dataset at half-degree resolution}

\description{Global topographic dataset at half-degree resolution,
  created by decimating the ETOPO5 dataset.}

\usage{data(topo.world)}

\source{The dataset was downloaded in late 2009 from the NOAA website,
  and processed with the following code
  \preformatted{
library(oce)
f <- file("~/data/etopo5.dat", "rb")
nx <- 12 * 360
ny <- 12 * 180
d <- t(matrix(readBin(f, "integer", size=2, endian="big", n=nx*ny), byrow=TRUE, nrow=ny))
z <- d[seq(1, nx, by=6), seq(ny, 1, by=-6)]
topo.world  <- as.topo(seq(0.25,359.75,1/2),seq(-89.75,89.75,1/2),z)
plot(topo.world)
}
}

\references{\url{http://www.ngdc.noaa.gov/mgg/global/relief/ETOPO5/TOPO/ETOPO5/}}

\seealso{Created with \code{\link{as.topo}}.}

\author{Dan Kelley}

\keyword{misc}
