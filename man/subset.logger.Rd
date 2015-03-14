\name{subset.logger}

\alias{subset.logger}
\alias{subset,logger,missing-method}
\alias{subset,logger-method}

\title{Subset a logger object}

\description{Subset a logger object}

\usage{\S4method{subset}{logger}(x, subset, \dots)}

\arguments{
  \item{x}{a \code{logger} object.}
  \item{subset}{a condition to be applied to the \code{data} portion of
    \code{x}.  See \sQuote{Details}.}
  \item{\dots}{ignored.}
}

\details{This function is somewhat analogous to
    \code{\link{subset.data.frame}}, but subsetting is only permitted by time.}

\value{A new \code{logger} object.}

\examples{
library(oce)
data(logger)
plot(logger)
plot(subset(logger, time < mean(range(logger[['time']]))))
}

\author{Dan Kelley}

\keyword{misc}

