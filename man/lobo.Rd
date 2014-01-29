\name{lobo}

\docType{data}

\alias{lobo}

\title{lobo dataset}

\description{This is sample lobo dataset obtained in the Northwest Arm
of Halifax by Satlantic.}

\usage{data(ctd)}

\examples{
\dontrun{
library(oce)
data(lobo)
summary(lobo)
plot(lobo)
}
}

\source{The data were downloaded from a web interface at Satlantic LOBO web
    server and then read with \code{read.lobo} in the Oce package.}

\seealso{The documentation for \code{lobo-class} in the Oce package explains
    the structure of LOBO objects, and also outlines the other functions
    dealing with them.}

\author{Dan Kelley}
\keyword{datasets}
