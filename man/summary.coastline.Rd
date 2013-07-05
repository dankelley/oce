\name{summary.coastline}

\alias{summary.coastline}

\title{Summarize a coastline data object}

\description{Summarizes coastline length, bounding box, etc.}

\usage{
\method{summary}{coastline}(object, \dots)
}

\arguments{
  \item{object}{an object of class \code{"coastline"}, usually, a result of a
    call to \code{\link{read.coastline}} or \code{\link{read.oce}}.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\value{An object of class \code{"summary.coastline"}, which contains
  pertinent information about the Lobo record and its processing.}

\seealso{The documentation for \code{\link{coastline-class}} explains the
    structure of coastline objects, and also outlines the other functions
    dealing with them.}

\examples{
library(oce)
library(ocedata)
data(coastlineWorld)
plot(coastlineWorld)
}

\references{http://www.ngdc.noaa.gov/mgg/shorelines/shorelines.html}

\author{Dan Kelley}

\keyword{misc}
