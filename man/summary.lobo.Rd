\name{summary.lobo}

\alias{summary.lobo}

\title{Summarize a lobo data object}

\description{Summarizes some of the data in a lobo object.}

\usage{
\method{summary}{lobo}(object, \dots)
}

\arguments{
  \item{object}{an object of class \code{"lobo"}, usually, a result of a
    call to \code{\link{read.lobo}} or \code{\link{read.oce}}.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented, including the
  sampling interval, data ranges, etc.}

\value{An object of class \code{"summary.lobo"}, which contains
  pertinent information about the Lobo record and its processing.}

\seealso{The documentation for \code{\link{lobo-class}} explains the structure
    of LOBO objects, and also outlines the other functions dealing with them.}

\examples{
library(oce)
data(lobo)
summary(lobo)
}

\references{
\url{http://lobo.satlantic.com}
\url{http://www.mbari.org/lobo/}
}

\author{Dan Kelley}

\keyword{misc}
