\name{logger}
\alias{logger}

\docType{data}

\title{A logger dataset, of the temperature-data recorder variety}

\description{A sample object of Oce class \code{\link{logger-class}},
    containing vectors for time, temperature and pressure, the last of which
    has not had atmospheric pressure subtracted (see \sQuote{Examples}).}

\usage{data(logger)}

\source{The \acronym{SLEIWEX} experiment.}

\references{\url{http://myweb.dal.ca/kelley/SLEIWEX/index.php}}

\seealso{The documentation for \code{\link{logger-class}} explains the
    structure of \code{logger} objects, and also outlines the other functions
    dealing with them.}

\examples{
library(oce)
data(logger)
## subtract standard atmospheric pressure
logger[["pressure"]] <- logger[["pressure"]] - 10.3
plot(logger)
}

\author{Dan Kelley}

\keyword{datasets}
