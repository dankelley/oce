\name{soi}

\docType{data}

\alias{soi}

\title{Southern Oscillation Index}

\description{This is the Southern Oscillation Index, downloaded in May
    2014 and processed as follows.
\preformatted{
d <- scan("http://www.cgd.ucar.edu/cas/catalog/climind/SOI.signal.annstd.ascii")
isYear <- d > 1800
index <- d[!isYear]
year <- 1/24 + seq(d[isYear][1], by=1/12, length.out=length(index))
## Trim -99.99 values
missing <- index < -90
year <- year[!missing]
index <- index[!missing]
soi <- data.frame(year=year, index=index)
}
}

\examples{
\dontrun{
library(oce)
data(soi)
plot(soi$year, soi$index, type='l')
}
}

\usage{data(soi)}

\source{\url{http://www.cgd.ucar.edu/cas/catalog/climind/SOI.signal.annstd.ascii}}
    
\author{Dan Kelley}

\keyword{datasets}
