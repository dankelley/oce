\name{topoWorld}

\docType{data}

\alias{topoWorld}

\title{Global topographic dataset at half-degree resolution}

\description{Global topographic dataset at half-degree resolution,
  created by decimating the ETOPO5 dataset.}

\usage{data(topoWorld)}

\source{The ETOPO5 dataset was downloaded in late 2009 from the NOAA website,
    and decimated from 1/12th degree resolution to 1/2 degree resolution.}

\references{\url{http://www.ngdc.noaa.gov/mgg/global/relief/ETOPO5/TOPO/ETOPO5/}}

\examples{
\dontrun{
library(oce)
data(topoWorld) 
plot(topoWorld, legend=NULL)
}
}

\seealso{Created with \code{as.topo} in the Oce package.}

\author{Dan Kelley}

\keyword{datasets}

