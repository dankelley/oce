\name{summary.section}

\alias{summary.section}
\alias{summary,section,missing-method}
\alias{summary,section-method}

\title{Summarize a CTD section}

\description{Summarize a CTD section.}

\usage{\S4method{summary}{section}(object, \dots)}

\arguments{
  \item{object}{an object of class \code{"section"}, usually, a result of a
    call to \code{\link{read.section}}, \code{\link{read.oce}}, or
    \code{\link{as.section}}.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented, including station
  locations, distance along track, etc.}

\seealso{The documentation for \code{\link{section-class}} explains the
    structure of section objects, and also outlines the other functions dealing
    with them.}

\examples{
library(oce)
data(section)
summary(section)
}

\author{Dan Kelley}

\keyword{misc}
