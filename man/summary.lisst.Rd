\name{summary.lisst}

\alias{summary.lisst}
\alias{summary,lisst,missing-method}
\alias{summary,lisst-method}

\title{Summarize a LISST object}

\description{Summarizes some of the data in a \code{lisst} object.}

\usage{\S4method{summary}{lisst}(object, \dots)}

\arguments{
  \item{object}{an object of class \code{"lisst"}, usually, a result of a
    call to \code{\link{read.lisst}}, \code{\link{read.oce}}, or
    \code{\link{as.lisst}}.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented, including the sampling
    times, data ranges, etc.}

\value{A matrix containing statistics of the elements of the \code{data} slot.}

\seealso{The documentation for \code{\link{lisst-class}} explains the structure
    of LISST objects, and also outlines the other functions dealing with them.}

\examples{
library(oce)
data(lisst)
summary(lisst)
}

\author{Dan Kelley}

\keyword{misc}
