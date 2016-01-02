\name{summary.coastline}

\alias{summary.coastline}
\alias{summary,coastline,missing-method}
\alias{summary,coastline-method}

\title{Summarize a coastline data object}

\description{Summarizes coastline length, bounding box, etc.}

\usage{\S4method{summary}{coastline}(object, \dots)}

\arguments{
  \item{object}{an object of class \code{"coastline"}, usually, a result of a
    call to \code{\link{read.coastline}} or \code{\link{read.oce}}.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\value{A matrix containing statistics of the elements of the \code{data} slot.}

\seealso{The documentation for \code{\link{coastline-class}} explains the
    structure of coastline objects, and also outlines the other functions
    dealing with them.}

\examples{
library(oce)
data(coastlineWorld)
plot(coastlineWorld)
}

\references{http://www.ngdc.noaa.gov/mgg/shorelines/shorelines.html}

\author{Dan Kelley}

\keyword{misc}
