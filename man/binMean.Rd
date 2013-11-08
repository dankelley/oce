\name{binMean}

\alias{binMean}
\alias{binMean1D}
\alias{binMean2D}

\title{Bin-average f=f(x) or f=f(x,y), based on x or (x,y) values}

\description{Bin-average f=f(x) or f=f(x,y), based on x or (x,y) values}

\usage{
binMean1D(x, f, xbreaks)
binMean2D(x, y, f, xbreaks, ybreaks)}

\arguments{
  \item{x}{a vector of numerical values.}
  
  \item{y}{a vector of numerical values.}

  \item{f}{a vector of numerical values, f=f(x) for the 1D function and
      f=f(x,y) for the 2D function.}

  \item{xbreaks}{values of x at the boundaries between bins; calculated using
      \code{\link{pretty}} if not supplied.}

  \item{ybreaks}{values of y at the boundaries between bins; calculated using
      \code{\link{pretty}} if not supplied.}

}

\details{The \code{f} vector is averaged in bins defined for \code{x}.  Missing
    values in \code{f} are ignored.}

\value{A list with the following elements: the breaks (\code{xbreaks}, along
    with \code{ybreaks} for the 2D case), the break mid-points (\code{xmids}
    along with \code{ymids} for the 2D case), the number of data points in each
    bin, \code{number}, and the mean value of \code{f} value in the bins,
    \code{value}.  For the 1D case, \code{number} and \code{mean} are vectors,
    whereas they are matrices for the 2D case.  For plotting, the midpoints are
    more useful than the breaks, as shown in the examples.}

\examples{
library(oce)
## A. fake linear data
x <- seq(0, 100, 1)
f <- 1 + 2 * x
plot(x, f, pch=1)
m <- binMean1D(x, f)
points(m$xmids, m$result, pch=3, col='red', cex=3)

## B. fake quadratic data
f <- 1 + x ^2
plot(x, f, pch=1)
m <- binMean1D(x, f)
points(m$xmids, m$result, pch=3, col='red', cex=3)

## C. natural data
data(co2)
plot(co2, col='gray')
m <- binMean1D(time(co2), co2)
lines(m$xmids, m$result, type='o')

## D. 2D
x <- runif(500)
y <- runif(500)
f <- x - y
m <- binMean2D(x, y, f)
plot(x, y)
grid()
contour(m$xmids, m$ymids, m$result, add=TRUE)
}

\author{Dan Kelley}

\keyword{misc}
