\name{subset.lisst}

\alias{subset.lisst}

\title{Subset a lisst object}

\description{Subset a lisst object}

\usage{\method{subset}{lisst}(x, subset, \dots)}

\arguments{
  \item{x}{a \code{lisst} object.}
  \item{subset}{a condition to be applied to the \code{data} portion of
    \code{x}.  See \sQuote{Details}.}
  \item{\dots}{ignored.}
}

\details{This function is somewhat analogous to
    \code{\link{subset.data.frame}}, but subsetting is only permitted by time.}

\value{A new \code{lisst} object.}

\examples{
library(oce)
data(lisst)
plot(lisst)
plot(subset.lisst(lisst, time < mean(range(lisst[['time']]))))
}

\author{Dan Kelley}

\keyword{misc}
