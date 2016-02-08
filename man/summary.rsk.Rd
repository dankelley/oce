% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{summary.rsk}

\alias{summary.rsk}
\alias{summary,rsk,missing-method}
\alias{summary,rsk-method}

\title{Summarize a rsk object}

\description{Summarize a rsk object}

\usage{\S4method{summary}{rsk}(object, \dots)}

\arguments{
  \item{object}{an object of class \code{"rsk"}, usually, a result of a
    call to \code{read.rsk}.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented, including the start and
  stop times, as well as statistics of temperature and pressure.}

\value{A matrix containing statistics of the elements of the \code{data} slot.}

\seealso{The documentation for \code{\link{rsk-class}} explains the structure of
  \code{rsk} objects, and also outlines the other functions dealing with them.}

\examples{
library(oce)
data(rsk)
summary(rsk)
}

\author{Dan Kelley}

\keyword{misc}
