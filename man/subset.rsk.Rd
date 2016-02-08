% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{subset.rsk}

\alias{subset.rsk}
\alias{subset,rsk,missing-method}
\alias{subset,rsk-method}

\title{Subset a rsk object}

\description{Subset a rsk object}

\usage{\S4method{subset}{rsk}(x, subset, \dots)}

\arguments{
  \item{x}{a \code{rsk} object.}
  \item{subset}{a condition to be applied to the \code{data} portion of
    \code{x}.  See \sQuote{Details}.}
  \item{\dots}{ignored.}
}

\details{This function is somewhat analogous to
    \code{\link{subset.data.frame}}, but subsetting is only permitted by time.}

\value{A new \code{rsk} object.}

\examples{
library(oce)
data(rsk)
plot(rsk)
plot(subset(rsk, time < mean(range(rsk[['time']]))))
}

\author{Dan Kelley}

\keyword{misc}

