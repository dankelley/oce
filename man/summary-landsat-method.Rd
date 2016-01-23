\docType{methods}
\name{summary,landsat-method}
\alias{summary,landsat-method}
\alias{summary.landsat}

\title{Summarize a landsat object}

\description{Summarizes some of the data in a \code{landsat} object.}

\usage{\S4method{summary}{landsat}(object, \dots)}

\arguments{
  \item{object}{an object of class \code{"landsat"}, usually, a result of a
    call to \code{\link{read.landsat}}.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented, including the station
    name, sampling location, data ranges, etc.}

\value{A matrix containing statistics of the elements of the \code{data} slot.}

\seealso{The documentation for \code{\link{landsat-class}} explains the
    structure of landsat objects, and also outlines the other functions dealing
    with them.}

\author{Dan Kelley}

\concept{satellite data}

\keyword{misc}

