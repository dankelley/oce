\name{summary.echosounder}

\alias{summary.echosounder}
\alias{summary,echosounder,missing-method}
\alias{summary,echosounder-method}

\title{Summarize an echosounder object}

\description{Summarizes some of the data in an \code{echosounder} object.}

\usage{\S4method{summary}{echosounder}(object, \dots)}

\arguments{
  \item{object}{an object of class \code{"echosounder"}, usually, a result of a
    call to \code{\link{read.echosounder}}, \code{\link{read.oce}}, or
    \code{\link{as.echosounder}}.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented, including the
  sampling location, data ranges, etc.}

\value{A matrix containing statistics of the elements of the \code{data} slot.}

\seealso{The documentation for \code{\link{echosounder-class}} explains the
    structure of echosounder objects, and also outlines the other functions
    dealing with them.}

\examples{
library(oce)
data(echosounder)
summary(echosounder)
}

\author{Dan Kelley}

\keyword{misc}
