\name{summary.topo}

\alias{summary.topo}

\title{Summarize a topography (ETOPO2) data object}

\description{Summarizes some of the data in an \code{topo} object.}

\usage{\method{summary}{topo}(object, \dots)}

\arguments{
  \item{object}{A \code{topo} object, e.g. as read by \code{\link{read.topo}}.}
  \item{\dots}{passed to children.}
}

\details{Pertinent summary information is presented, including the
  latitude and longitude range, and the range of elevation.}

\value{NULL}

\seealso{A \code{topo} object may be read with \code{\link{read.topo}}.}

\examples{
library(oce)
data(topo.maritimes)
summary(topo.maritimes)
}

\author{Dan Kelley}

\keyword{misc}
