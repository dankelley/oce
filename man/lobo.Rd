\name{lobo}
\docType{data}
\alias{lobo}
\title{Sample lobo dataset}
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
  \url{http://www.satlantic.com} that seems not to exist anymore.  This,
  and the fact that Satlantic sells software for visualizing lobo data,
  perhaps argues for removal of lobo support in a future version of oce.}

\author{Dan Kelley}
\keyword{misc}
