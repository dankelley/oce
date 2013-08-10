\name{subset.echosounder}

\alias{subset.echosounder}
\alias{subset,echosounder,missing-method}
\alias{subset,echosounder-method}

\title{Subset an echosounder object}

\description{Subset an echosounder object}

\usage{\S4method{subset}{echosounder}(x, subset, \dots)}

\arguments{
  \item{x}{a \code{echosounder} object.}
  \item{subset}{a condition to be applied to the \code{data} portion of
    \code{x}.  See \sQuote{Details}.}
  \item{\dots}{ignored.}
}

\details{This function is somewhat analogous to
    \code{\link{subset.data.frame}}.  Subsetting can be by \code{time} or
    \code{depth}, but these may not be combined; use a sequence of calls to
    subset by both.}

\value{A new \code{echosounder} object.}

\examples{
library(oce)
data(echosounder)
plot(echosounder)
plot(subset(echosounder, depth < 10))
plot(subset(echosounder, time < mean(range(echosounder[['time']]))))
}

\author{Dan Kelley}

\keyword{misc}

