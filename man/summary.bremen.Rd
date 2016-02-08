\name{summary.bremen}

\alias{summary.bremen}
\alias{summary,bremen,missing-method}
\alias{summary,bremen-method}

\title{Summarize a bremen object}

\description{Summarizes some of the data in a \code{bremen} object.}

\usage{\S4method{summary}{bremen}(object, \dots)}

\arguments{
  \item{object}{an object of class \code{"bremen"}, usually, a result of a
    call to \code{\link{read.bremen}}.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented, including the station
    name, sampling location, data ranges, etc.}

\value{A matrix containing statistics of the elements of the \code{data} slot.}

\seealso{The documentation for \code{\link{bremen-class}} explains the
    structure of these objects, and outlines some functions that deal with
    them.}

\author{Dan Kelley}

\keyword{misc}
