\name{subset.cm}

\alias{subset.cm}
\alias{subset,cm,missing-method}
\alias{subset,cm-method}

\title{Subset a cm object}

\description{Subset a cm (current meter) object}

\usage{\S4method{subset}{cm}(x, subset, \dots)}

\arguments{
  \item{x}{a \code{cm} object.}
  \item{subset}{a condition to be applied to the \code{data} portion of
    \code{x}.  See \sQuote{Details}.}
  \item{\dots}{ignored.}
}

\details{This function is somewhat analogous to \code{\link{subset.data.frame}}.}

\value{A new \code{cm} object.}

\examples{
library(oce)
data(cm)
plot(cm)
plot(subset(cm, time < mean(range(cm[['time']]))))
}

\author{Dan Kelley}

\keyword{misc}

