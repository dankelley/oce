% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{subset.odf}

\alias{subset.odf}
\alias{subset,odf,missing-method}
\alias{subset,odf-method}

\title{Subset an odf object}

\description{Subset an odf object}

\usage{\S4method{subset}{odf}(x, subset, \dots)}

\arguments{
  \item{x}{a \code{odf} object.}
  \item{subset}{a condition to be applied to the \code{data} portion of
    \code{x}.  See \sQuote{Details}.}
  \item{\dots}{ignored.}
}

\details{This function is somewhat analogous to
  \code{\link{subset.data.frame}}.}

\value{A new \code{odf} object.}

\author{Dan Kelley}

\keyword{misc}

