\name{subset.adv}

\alias{subset.adv}
\alias{subset,adv,missing-method}
\alias{subset,adv-method}

\title{Subset an adv object}

\description{Subset an adv (acoustic Doppler profile) object}

\usage{\S4method{subset}{adv}(x, subset, \dots)}

\arguments{
  \item{x}{a \code{adv} object.}
  \item{subset}{a condition to be applied to the \code{data} portion of
    \code{x}.  See \sQuote{Details}.}
  \item{\dots}{ignored.}
}

\details{This function is somewhat analogous to
    \code{\link{subset.data.frame}}, except that subsets can only be specified
    in terms of \code{time}.}

\value{A new \code{adv} object.}

\examples{
library(oce)
data(adv)
plot(adv)
plot(subset(adv, time < mean(range(adv[['time']]))))
}

\author{Dan Kelley}

\keyword{misc}

