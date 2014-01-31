\name{subset.topo}

\alias{subset.topo}
\alias{subset,topo,missing-method}
\alias{subset,topo-method}

\title{Subset a topo object}

\description{Subset a topo (topography) object}

\usage{\S4method{subset}{topo}(x, subset, \dots)}

\arguments{
  \item{x}{a \code{topo} object.}
  \item{subset}{a condition to be applied to the \code{data} portion of
    \code{x}.  See \sQuote{Details}.}
  \item{\dots}{ignored.}
}

\details{This function is somewhat analogous to
    \code{\link{subset.data.frame}}.  Subsetting can be by \code{time} or
    \code{distance}, but these may not be combined; use a sequence of calls to
    subset by both.}

\value{A new \code{topo} object.}

\examples{
## northern hemisphere
library(oce)
data(topoWorld)
plot(subset(topoWorld, latitude > 0))
}

\author{Dan Kelley}

\keyword{misc}

