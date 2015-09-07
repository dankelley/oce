% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{cm}

\docType{data}

\alias{cm}

\title{Current-meter record}

\description{The result of using \code{\link{read.cm}} on a current meter file
  holding measurements made with an InterOcean S4 device.  See
  \code{\link{read.cm}} for cautionary notes, and also be aware that the
  salinities are suspiciously high.}

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

