\name{subset.sealevel}

\alias{subset.sealevel}
\alias{subset,sealevel,missing-method}
\alias{subset,sealevel-method}

\title{Subset a sealevel object}

\description{Subset a sealevel object}

\usage{\S4method{subset}{sealevel}(x, subset, \dots)}

\arguments{
  \item{x}{a \code{sealevel} object.}
  \item{subset}{a condition to be applied to the \code{data} portion of
    \code{x}.  See \sQuote{Details}.}
  \item{\dots}{ignored.}
}

\details{This function is somewhat analogous to
    \code{\link{subset.data.frame}}, but subsetting is only permitted by time.}

\value{A new \code{sealevel} object.}

\examples{
library(oce)
data(sealevel)
plot(sealevel)
plot(subset(sealevel, time < mean(range(sealevel[['time']]))))
}

\author{Dan Kelley}

\keyword{misc}
