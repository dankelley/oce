\name{subset.met}

\alias{subset.met}
\alias{subset,met,missing-method}
\alias{subset,met-method}

\title{Subset a met object}

\description{Subset a met object}

\usage{\S4method{subset}{met}(x, subset, \dots)}

\arguments{
  \item{x}{a \code{met} object.}
  \item{subset}{a condition to be applied to the \code{data} portion of
    \code{x}.  See \sQuote{Details}.}
  \item{\dots}{ignored.}
}

\details{This function is somewhat analogous to
    \code{\link{subset.data.frame}}.  Subsetting can be by \code{time} or
    \code{distance}, but these may not be combined; use a sequence of calls to
    subset by both.}

\value{A new \code{met} object.}

\examples{
library(oce)
data(met)
# Few days surrounding Hurricane Juan
plot(subset(met, time > as.POSIXct("2003-09-27", tz="UTC")))
}

\author{Dan Kelley}

\keyword{misc}

