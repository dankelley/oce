\name{summary.ladp}

\alias{summary.ladp}
\alias{summary,ladp,missing-method}
\alias{summary,ladp-method}

\title{Summarize an ladp object}

\description{Summarizes some of the data in an \code{ladp} object.}

\usage{\S4method{summary}{ladp}(object, \dots)}

\arguments{
  \item{object}{an object of class \code{"ladp"}, usually, a result of a
    call to \code{\link{as.ladp}}.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented, including the station
    name, sampling location, data ranges, etc.}

\value{A matrix containing statistics of the elements of the \code{data} slot.}

\seealso{The documentation for \code{\link{ladp-class}} explains the structure
    of \code{ladp} objects, and also outlines the other functions dealing with
    them.}

\author{Dan Kelley}

\keyword{misc}
