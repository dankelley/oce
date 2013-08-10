\name{subset.oce}

\alias{subset.oce}
\alias{subset,oce,missing-method}
\alias{subset,oce-method}

\title{Subset an oce object}

\description{Subset an oce object}

\usage{\S4method{subset}{oce}(x, subset, \dots)}

\arguments{
  \item{x}{a \code{oce} object.}
  \item{subset}{a condition to be applied to the \code{data} portion of
    \code{x}.  See \sQuote{Details}.}
  \item{\dots}{ignored.}
}

\details{This function is somewhat analogous to
    \code{\link{subset.data.frame}}.}

\value{A new \code{oce} object.}

\author{Dan Kelley}

\keyword{misc}
