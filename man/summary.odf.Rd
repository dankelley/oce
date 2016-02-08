\name{summary.odf}

\alias{summary.odf}
\alias{summary,odf,missing-method}
\alias{summary,odf-method}

\title{Summarize an odf object}

\description{Summarizes some of the data in a \code{odf} object.}

\usage{\S4method{summary}{odf}(object, \dots)}

\arguments{
  \item{object}{an object of class \code{"odf"}, usually, a result of a
    call to \code{\link{read.odf}} or \code{\link{read.oce}}.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented, including the station
    name, sampling location, data ranges, etc.}

\value{A matrix containing statistics of the elements of the \code{data} slot.}

\seealso{The documentation for \code{\link{odf-class}} explains the structure
    of ODF objects, and also outlines the other functions dealing with them.}

\author{Dan Kelley}

\keyword{misc}
