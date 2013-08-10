\name{subset.section}

\alias{subset.section}
\alias{subset,section,missing-method}
\alias{subset,section-method}

\title{Subset a section object}

\description{Subset a section object}

\usage{\S4method{subset}{section}(x, subset, \dots)}

\arguments{
  \item{x}{a \code{section} object.}
  \item{subset}{a condition to be applied to the \code{data} portion of
    \code{x}.  See \sQuote{Details}.}
  \item{\dots}{may include \code{indices}, to hold a vector of stations to be
      kept; if so, \code{subset} may not be supplied.}
}

\details{This function is somewhat analogous to
    \code{\link{subset.data.frame}}.}

\value{A new \code{section} object.}

\examples{
library(oce)
data(a03)
GS <- subset(a03, indices=124:102)
}

\author{Dan Kelley}

\keyword{misc}
