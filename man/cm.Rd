% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{cm}

\docType{data}

\alias{cm}

\title{Current-meter record}

\description{The result of using \code{\link{read.cm}} on a current meter file
  holding measurements made with an InterOcean S4 device.  See
  \code{\link{read.cm}} for some general cautionary notes on reading such files,
  and be aware that the salinities in this sample dataset are known to be
  incorrect, for unknown reasons perhaps related to lack of calibration of an
  old instrument that is seldom used.}

\usage{data(cm)}

\examples{
\dontrun{
library(oce)
data(cm)
summary(cm)
plot(cm)
}
}

\seealso{The documentation for \code{\link{cm-class}} explains the structure of
  \code{cm} objects, and also outlines the other functions dealing with them.}

\keyword{datasets}

