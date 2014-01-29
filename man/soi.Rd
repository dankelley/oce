\name{soi}

\docType{data}

\alias{soi}

\title{Southern Oscillation Index}

\description{This is the Southern Oscillation Index, downloaded in January
    2014.}

\examples{
\dontrun{
library(oce)
data(soi)
plot(soi$year, soi$soi, type='l')
}
}

\usage{data(soi)}

\source{\url{http://www.cgd.ucar.edu/cas/catalog/climind/soi.html}}
    
\author{Dan Kelley}

\keyword{datasets}
