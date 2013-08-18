\name{subset.ctd}

\alias{subset.ctd}
\alias{subset,ctd,missing-method}
\alias{subset,ctd-method}

\title{Subset a ctd object}

\description{Subset a ctd object}

\usage{\S4method{subset}{ctd}(x, subset, \dots)}

\arguments{
  \item{x}{a \code{ctd} object.}
  \item{subset}{a condition to be applied to the \code{data} portion of
    \code{x}.  See \sQuote{Details}.}
  \item{\dots}{ignored.}
}

\details{This function is somewhat analogous to
    \code{\link{subset.data.frame}}, but only one independent variable may be
    used in \code{subset} in any call to the function, which means that
    repeated calls will be necessary to, subset based on more than one
    independent variable (e.g. time and distance).  }

\value{A new \code{ctd} object.}

\examples{
library(oce)
data(ctd)
plot(ctd)
plot(subset(ctd, pressure<10))
}

\author{Dan Kelley}

\keyword{misc}
