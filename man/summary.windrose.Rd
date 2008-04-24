\name{summary.windrose}

\alias{summary.windrose}

\title{Summarize a windrose data object}

\description{Summarizes some of the data in a \code{windrose} object.}

\usage{
\method{summary}{windrose}(object, \dots)
}

\arguments{
  \item{object}{A \code{windrose} object, e.g. as created by \code{\link{as.windrose}}.}
  \item{\dots}{passed to children.}
}

\details{Pertinent summary information is presented.}

\value{NULL}

\seealso{A \code{windrose} object may be created with
  \code{\link{as.windrose}} or plotted with
  \code{\link{plot.windrose}}.}

\examples{
library(oce)
xcomp <- rnorm(360) + 1
ycomp <- rnorm(360)
wr <- as.windrose(xcomp, ycomp)
summary(wr)
}

\author{Dan Kelley}

\keyword{misc}
