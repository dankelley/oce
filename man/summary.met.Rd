\name{summary.met}

\alias{summary.met}
\alias{summary,met,missing-method}
\alias{summary,met-method}

\title{Summarize a met object}

\description{Summarizes some of the data in a \code{met} object.}

\usage{\S4method{summary}{met}(object, \dots)}

\arguments{
  \item{object}{an object of class \code{"met"}, usually, a result of a
    call to \code{\link{read.met}}, or \code{\link{as.met}}.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented, including the
  sampling location, data ranges, etc.}

\value{A matrix containing statistics of the elements of the \code{data} slot.}

\seealso{The documentation for \code{\link{met-class}} explains the structure
    of met objects, and also outlines the other functions dealing with them.}

\examples{
library(oce)
data(met)
summary(met)
}

\author{Dan Kelley}

\keyword{misc}
