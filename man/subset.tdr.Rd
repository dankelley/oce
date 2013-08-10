\name{subset.tdr}

\alias{subset.tdr}

\title{Subset a tdr object}

\description{Subset a tdr object}

\usage{\method{subset}{tdr}(x, subset, \dots)}

\arguments{
  \item{x}{a \code{tdr} object.}
  \item{subset}{a condition to be applied to the \code{data} portion of
    \code{x}.  See \sQuote{Details}.}
  \item{\dots}{ignored.}
}

\details{This function is somewhat analogous to
    \code{\link{subset.data.frame}}, but subsetting is only permitted by time.}

\value{A new \code{tdr} object.}

\examples{
library(oce)
data(tdr)
plot(tdr)
plot(subset.tdr(tdr, time < mean(range(tdr[['time']]))))
}

\author{Dan Kelley}

\keyword{misc}
