\name{subset.adp}

\alias{subset.adp}
\alias{subset,adp,missing-method}
\alias{subset,adp-method}

\title{Subset an adp object}

\description{Subset an adp (acoustic Doppler profile) object}

\usage{\S4method{subset}{adp}(x, subset, \dots)}

\arguments{
  \item{x}{a \code{adp} object.}
  \item{subset}{a condition to be applied to the \code{data} portion of
    \code{x}.  See \sQuote{Details}.}
  \item{\dots}{ignored.}
}

\details{This function is somewhat analogous to
    \code{\link{subset.data.frame}}.  Subsetting can be by \code{time} or
    \code{distance}, but these may not be combined; use a sequence of calls to
    subset by both.}

\value{A new \code{adp} object.}

\examples{
library(oce)
data(adp)
# First part of time series
plot(subset(adp, time < mean(range(adp[['time']]))))
}

\author{Dan Kelley}

\keyword{misc}

