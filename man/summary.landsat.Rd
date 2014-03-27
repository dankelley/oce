\name{summary.landsat}

\alias{summary.landsat}
\alias{summary,landsat,missing-method}
\alias{summary,landsat-method}

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

\value{An object of class \code{"summary.landsat"}, which when printed shows
    pertinent information about the landsat record and its processing.}

\seealso{The documentation for \code{\link{landsat-class}} explains the
    structure of landsat objects, and also outlines the other functions dealing
    with them.}

\author{Dan Kelley}

\keyword{misc}

