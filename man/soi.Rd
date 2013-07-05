\name{soi}

\docType{data}

\alias{soi}

\title{Southern Oscillation Index}

\description{This is the Southern Oscillation Index, downloaded in June 2013.}

\examples{
library(oce)
library(ocedata)
data(soi)
plot(soi$year, soi$soi, type='l')
}

\usage{data(soi)}

\source{\url{http://www.cgd.ucar.edu/cas/catalog/climind/soi.html}}
    
\author{Dan Kelley}

\keyword{datasets}
