% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2

\name{rsk}
\alias{rsk}

\docType{data}

\title{A rsk dataset, of the temperature-data recorder variety}

\description{A sample object of Oce class \code{\link{rsk-class}}, containing
  vectors for time, temperature and pressure, the last of which has not had
  atmospheric pressure subtracted (see \sQuote{Examples}).}

\usage{data(rsk)}

\source{The \acronym{SLEIWEX} experiment.}

\references{\url{http://myweb.dal.ca/kelley/SLEIWEX/index.php}}

\seealso{The documentation for \code{\link{rsk-class}} explains the structure of
  \code{rsk} objects, and also outlines the other functions dealing with them.}

\examples{
library(oce)
data(rsk)
## subtract standard atmospheric pressure
rsk[["pressure"]] <- rsk[["pressure"]] - 10.3
plot(rsk)
}

\author{Dan Kelley}

\keyword{datasets}
