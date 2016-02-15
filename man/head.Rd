\name{head}
\alias{head}
\alias{head.adp}
\alias{tail}
\alias{tail.adp}

\title{Ends of oce Objects}

\description{Returns the first or last parts of an \code{oce} object.}

\usage{\method{head}{adp}(x, n = 6L, \dots)
\method{tail}{adp}(x, n = 6L, \dots)
}

\arguments{
  \item{x}{an object}
  \item{n}{a single integer. If positive, size for the resulting
    object: number of elements for a vector (including lists), rows for
    a matrix or data frame or lines for a function. If negative, all but
    the \code{n} last/first number of elements of \code{x}.}
  \item{\dots}{arguments to be passed to or from other methods.}
}

\details{
  For \code{adp} objects, returns first or last profiles, as indicated.
}

\value{An object like \code{x} but generally smaller.}

\examples{
library(oce)
data(adp)
start <- head(adp)
plot(start)
}

\author{Dan Kelley}

\keyword{manip}

