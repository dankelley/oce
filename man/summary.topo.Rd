\name{summary.topo}

\alias{summary.topo}
\alias{print.summary.topo}

\title{Summarize a topography data object}

\description{Summarizes some of the data in an \code{topo} object.}

\usage{
\method{summary}{topo}(object, \dots)

\method{print}{summary.topo}(x, digits=max(6, getOption("digits")-1), \dots)
}

\arguments{
  \item{object}{an object of class \code{"topo"}, usually, a result of a
    call to \code{\link{read.topo}} or \code{\link{read.oce}}.}
  \item{x}{an object of class \code{"summary.topo"}, usually, a result
    of a call to \code{summary.topo}.}
  \item{digits}{the number of significant digits to use when printing.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented, including the
  latitude and longitude range, and the range of elevation.}

\value{An object of class \code{"summary.lobo"}, which contains
  pertinent information about the Lobo record and its processing.}

\seealso{A \code{topo} object may be read with \code{\link{read.topo}}.}

\examples{
library(oce)
data(topo.maritimes)
summary(topo.maritimes)
}

\author{Dan Kelley}

\keyword{misc}
