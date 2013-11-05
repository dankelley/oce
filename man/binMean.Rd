\name{binMean}

\alias{binMean}
\alias{binMean1D}
\alias{binMean2D}

\title{Bin-average f=f(x) or f=f(x,y), based on x or (x,y) values}

\description{Bin-average f=f(x) or f=f(x,y), based on x or (x,y) values}

\usage{
binMean1D(x, f, breaks)
binMean2D(x, y, f, xbreaks, ybreaks)}

\arguments{
  \item{x}{a vector of numerical values.}
  
  \item{y}{a vector of numerical values.}

  \item{f}{a vector of numerical values, f=f(x) for the 1D function and
      f=f(x,y) for the 2D function.}

  \item{breaks}{values of x at the boundaries between bins.  If not supplied,
      this will be calculated using \code{\link{pretty}}.}
  
  \item{xbreaks,ybreaks}{values of x and y at the boundaries between bins.  If
      not supplied, this will be calculated using \code{\link{pretty}}.}

}

\details{The \code{f} vector is averaged in bins defined for \code{x}.  Missing
    values in \code{f} are ignored.}

\value{A list with the following elements: \code{breaks}, \code{mids}, the
    midpoints of the bins, and \code{mean}, the average \code{y} value in the
    bins.  Note that the lengths of }

\examples{
library(oce)
## A. fake linear data
x <- seq(0, 100, 1)
f <- 1 + 2 * x
plot(x, f, pch=1)
m <- binMean1D(x, f)
points(m$mids, m$mean, pch=3, col='red', cex=3)

## B. fake quadratic data
f <- 1 + x ^2
plot(x, f, pch=1)
m <- binMean1D(x, f)
points(m$mids, m$mean, pch=3, col='red', cex=3)

## C. natural data
data(co2)
plot(co2, col='gray')
m <- binMean1D(time(co2), co2)
lines(m$mids, m$mean, type='o')

## D. 2D
x <- runif(500)
y <- runif(500)
f <- x - y
m <- binMean2D(x, y, f)
plot(x, y)
grid()
contour(m$xmids, m$ymids, m$mean, add=TRUE)
}

\author{Dan Kelley}

\keyword{misc}
