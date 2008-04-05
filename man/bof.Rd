\name{bof}
\docType{data}
\alias{bof}
\title{Sample ETOPO2 dataset, showing the Bay of Fundy, near Nova Scotia,
Canada.}
\description{The result of reading an ETOPO2 with \code{\link{read.etopo2}}. 
The dataset was downloaded in early 2008 from the NOAA website
\url{http://www.ngdc.noaa.gov/mgg/gdas/gd_designagrid.html}, with 
a longitude box from 68W to 63W and a latitude box from 44N to 46N.
}
\usage{data(bof)}
\source{The dataset was downloaded in early 2008 from the NOAA website
\url{http://www.ngdc.noaa.gov/mgg/gdas/gd_designagrid.html}, with 
a longitude box from 68W to 63W and a latitude box from 44N to 46N.}
\references{\url{http://www.ngdc.noaa.gov/mgg/gdas/gd_designagrid.html}}
\example{
library(oce)
data(bof)
plot(bof)
}
\seealso{\code{\link{read.etopo2}}.}
\author{Dan Kelley}
\keyword{misc}
