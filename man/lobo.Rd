\name{lobo}
\docType{data}
\alias{lobo}
\title{lobo dataset}
\description{This is sample lobo dataset obtained in the Northwest Arm
of Halifax by Satlantic.}
\usage{data(ctd)}
\examples{
library(oce)
data(lobo)
summary(lobo)
plot(lobo)
}

\source{The data were downloaded from a web interface at Satlantic
  as noted below (which may not work in the future):
\preformatted{
uri <- paste("http://lobo.satlantic.com/cgi-bin/nph-data.cgi?",
"min_date=20090301&max_date=20090401",
"&x=date&",
"y=current_across1,current_along1,nitrate,fluorescence,salinity,temperature&",
"data_format=text",sep="")
lobo <- read.lobo(uri)
}
}

\seealso{The documentation for \code{\link{lobo-class}} explains the structure
    of LOBO objects, and also outlines the other functions dealing with them.}

\author{Dan Kelley}
\keyword{datasets}
