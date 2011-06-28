\name{soi}

\docType{data}

\alias{soi}

\title{Southern Oscillation Index}

\description{This is the Southern Oscillation Index, downloaded in June 2011,
    and processed as in This dataset is used in Chapter 5 of Kelley (2011).}

\examples{
library(oce)
data(soi)
plot(soi$year, soi$soi, type='l')
}

\usage{data(soi)}

\source{\url{http://www.cgd.ucar.edu/cas/catalog/climind/soi.html}}
    
\author{Dan Kelley}

\references{Dan Kelley, in preparation. Oceanographic Analysis with R.  Springer Verlag.}

\keyword{datasets}
