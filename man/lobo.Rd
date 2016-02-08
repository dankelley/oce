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
    server and then read with \code{\link{read.lobo}}.}

\seealso{The documentation for \code{\link{lobo-class}} explains the structure
    of \code{lobo} objects, and also outlines the other functions dealing with
    them.}

\author{Dan Kelley}
\keyword{datasets}
