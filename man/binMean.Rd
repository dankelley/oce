\name{binMean}

\alias{binMean}

\title{Bin-average a vector y, based on x values}

\description{Bin-average a vector y, based on x values}

\usage{binMean(x, y, breaks)}

\arguments{
  \item{x}{a vector of numerical values.}

  \item{y}{a vector of numerical values.}

  \item{breaks}{values of x at the boundaries between bins.  If not supplied,
      this will be calculated using \code{\link{pretty}}.}

}

\details{The \code{y} vector is averaged in bins defined for \code{x}.  Missing
    values in \code{y} are ignored.}

\value{A list with the following elements: \code{breaks}, \code{mids}, the
    midpoints of the bins, and \code{mean}, the average \code{y} value in the
    bins.  Note that the lengths of }

\examples{
library(oce)
## A. fake linear data
x <- seq(0, 100, 1)
y <- 1 + 2 * x
plot(x, y, pch=1)
m <- binMean(x, y)
points(m$mids, m$mean, pch=3, col='red', cex=3)

## B. fake quadratic data
y <- 1 + x ^2
plot(x, y, pch=1)
m <- binMean(x, y)
points(m$mids, m$mean, pch=3, col='red', cex=3)

## C. natural data
data(co2)
plot(co2, col='gray')
m <- binMean(time(co2), co2)
lines(m$mids, m$mean, type='o')
}

\author{Dan Kelley}

\keyword{misc}
