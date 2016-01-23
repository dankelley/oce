\name{landsat}

\docType{data}

\alias{landsat}

\title{Landsat-8 dataset}

\description{Subset of the Landsat-8 image designated LC80080292014065LGN00, an
    image from March 2014 that covers Nova Scotia and portions of the Bay of
    Fundy and the Scotian Shelf. The image is decimated to reduce the memory
    requirements of this package, yielding a spatial resolution of about 2km.}

\examples{
\dontrun{
library(oce)
data(landsat, package="ocedata")
plot(landsat)
}
}

\usage{data(landsat)}

\source{The original data were downloaded from the USGS earthexplorer website,
    although other sites can also be used
    to uncover it by name.  The code below shows how the dataset was created.
    The decimation by 100 reduces file size from about 1GB to under 100Kb.
\preformatted{
library(oce)
landsat <- decimate(read.landsat("LC80080292014065LGN00"), by=100)
}
}
\concept{satellite data}
\keyword{datasets}

