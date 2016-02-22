\name{subset.lobo}

\alias{subset.lobo}
\alias{subset,lobo,missing-method}
\alias{subset,lobo-method}

\title{Subset an lobo object}

\description{Subset an lobo (acoustic Doppler profile) object}

\usage{\S4method{subset}{lobo}(x, subset, \dots)}

\arguments{
  \item{x}{a \code{lobo} object.}
  \item{subset}{a condition to be applied to the \code{data} portion of
    \code{x}.  See \sQuote{Details}.}
  \item{\dots}{ignored.}
}

\details{This function is somewhat analogous to
    \code{\link{subset.data.frame}}, except that subsets can only be specified
    in terms of \code{time}.}

\value{A new \code{lobo} object.}

\examples{
library(oce)
data(lobo)
plot(lobo)
plot(subset(lobo, time < mean(range(lobo[['time']]))))
}

\author{Dan Kelley}

\keyword{misc}

