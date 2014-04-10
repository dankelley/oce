\name{nao}

\docType{data}

\alias{nao}

\title{North Atlantic Oscillation Index}

\description{This is the North Oscillation Index, downloaded in May 2014 and processed as follows.
\preformatted{
d <- scan("http://www.cpc.ncep.noaa.gov/products/precip/CWlink/pna/norm.nao.monthly.b5001.current.ascii.table")
isYear <- d > 1900
index <- d[!isYear]
year <- 1/24 + seq(d[isYear][1], by=1/12, length.out=length(index))
nao <- data.frame(year=year, index=index)
}
}

\examples{
\dontrun{
library(oce)
data(nao)
plot(nao$year, nao$index, type='l')
}
}

\usage{data(nao)}

\source{\url{http://www.cpc.ncep.noaa.gov/products/precip/CWlink/pna/norm.nao.monthly.b5001.current.ascii.table}}
    
\author{Dan Kelley}

\keyword{datasets}
