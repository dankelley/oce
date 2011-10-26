\name{summary.windrose}

\alias{summary.windrose}

\title{Summarize a windrose data object}

\description{Summarizes some of the data in a \code{windrose} object.}

\usage{\method{summary}{windrose}(object, \dots)}

\arguments{
  \item{object}{an object of class \code{"windrose"}, usually, a result of a
    call to \code{\link{as.windrose}}.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented.}

\value{An object of class \code{"summary.windrose"}, which contains
  pertinent information about the windrose record and its processing.}

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
