\name{summary.met}

\alias{summary.met}

\title{Summarize a met object}

\description{Summarizes some of the data in a \code{met} object.}

\usage{
\method{summary}{met}(object, \dots)
}

\arguments{
  \item{object}{an object of class \code{"met"}, usually, a result of a
    call to \code{\link{read.met}}, or \code{\link{as.met}}.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented, including the
  sampling location, data ranges, etc.}

\value{An object of class \code{"summary.met"}, which contains
  pertinent information about the met record and its processing.}

\seealso{The documentation for \code{\link{met-class}} explains the structure
    of met objects, and also outlines the other functions dealing with them.}

\examples{
library(oce)
data(met)
summary(met)
}

\author{Dan Kelley}

\keyword{misc}
