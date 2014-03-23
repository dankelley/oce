\name{plot.landsat}

\alias{plot.landsat}
\alias{plot,landsat,missing-method}
\alias{plot,landsat-method}

\title{Plot landsat data}

\description{Plot landsat data.}

\usage{\S4method{plot}{landsat}(x, which=1, debug=getOption("oceDebug"), \dots)
}
\arguments{
  \item{x}{A \code{landsat} object, e.g. as read by \code{\link{read.landsat}}.}
  \item{which}{desired plot type; 1=histogram, 2=image}
  \item{debug}{set to a positive value to get debugging information during processing.}
  \item{\dots}{optional arguments passed to plotting functions.}
}

\details{This is a very preliminary version of the function!}

\seealso{The documentation for \code{\link{landsat-class}} explains the
    structure of landsat objects, and also outlines the other functions dealing
    with them.}

\author{Dan Kelley}

\keyword{hplot}
