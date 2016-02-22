\name{summary.topo}

\alias{summary.topo}
\alias{summary,topo,missing-method}
\alias{summary,topo-method}

\title{Summarize a topography data object}

\description{Summarizes some of the data in an \code{topo} object.}

\usage{\S4method{summary}{topo}(object, \dots)}

\arguments{
  \item{object}{an object of class \code{"topo"}, usually, a result of a
    call to \code{\link{read.topo}} or \code{\link{read.oce}}.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented, including the
  longitude and latitude range, and the range of elevation.}

\value{A matrix containing statistics of the elements of the \code{data} slot.}

\seealso{A \code{topo} object may be read with \code{\link{read.topo}}.}

\examples{
library(oce)
data(topoWorld)
summary(topoWorld)
}

\author{Dan Kelley}

\keyword{misc}
