\name{endeavour}

\docType{data}

\alias{endeavour}

\title{Cook's Endeavour cruise track}

\description{Time, longitude, and latitude for the H.M.S. Endeavour cruise,
    1768-1771}

\usage{data(endeavour)}

\examples{
library(oce)
data(coastlineWorld)
data(endeavour)
mapPlot(coastlineWorld, type='l', proj='mollweide', orientation=c(90, -100, 0))
mapPoints(endeavour$longitude, endeavour$latitude, pch=20, col='red')
}

\source{The data were extracted from an XML file contained within a zipfile
    provided at the Australian government website
    \url{http://data.gov.au/dataset/journal-of-the-h-m-s-endeavour-1768-1771/}.
    (The world would be a better place if other governments similarly archived
    data of this sort.)}

\keyword{datasets}
