\name{subset.coastline}

\alias{subset.coastline}
\alias{subset,coastline,missing-method}
\alias{subset,coastline-method}

\title{Subset a coastline object}

\description{Subset a coastline object}

\usage{\S4method{subset}{coastline}(x, subset, \dots)}

\arguments{
  \item{x}{a \code{coastline} object.}
  \item{subset}{a condition to be applied to the \code{data} portion of
    \code{x}.  See \sQuote{Details}.}
  \item{\dots}{ignored.}
}

\details{This function is somewhat analogous to
    \code{\link{subset.data.frame}}.  Points on the coastline that are outside
    the subset region are set to \code{NA}.  If the results are to be plotted
    with \code{\link{plot.coastline}}, it will be necessary to set the argument
    \code{fill=NULL} to avoid filling land areas.}

\value{A new \code{coastline} object.}

\examples{
library(oce)
data(coastlineWorld)
plot(coastlineWorld)
a <- subset(coastlineWorld, longitude < 0)
points(a[['longitude']], a[['latitude']], col='red', pch=20, cex=1/2)
}

\author{Dan Kelley}

\keyword{misc}

