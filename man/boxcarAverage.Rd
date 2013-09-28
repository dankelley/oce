\name{boxcarAverage}

\alias{boxcarAverage}
\alias{boxcarAverage2D}

\title{Boxcar averaging}

\description{Perform one-dimensional or two-dimensional boxcar averaging.}

\usage{
boxcarAverage(x, y, xout=pretty(x))
boxcarAverage2D(x1, x2, y, x1out=pretty(x1), x2out=pretty(x2))
}

\arguments{
    \item{x}{vector of x values}
    \item{x1}{vector of x1 values}
    \item{x2}{vector of x2 values}
    \item{y}{vector of y values, corresponding to the elements x (or x1 and x2)}
    \item{xout}{vector of gridded x values}
    \item{x1out}{vector of gridded x1 values}
    \item{x2out}{vector of gridded x2 values}
}

\details{In the first form, performs one-dimensional boxcar averaging of a
    function y=y(x), with the elements of \code{xout} being the boundaries of
    the averaging regions.  In the second form, the function is y=y(x1, x2),
    and the boundaries are specified by \code{x1out} and \code{x2out}. Any bin
    with no data is assigned the value \code{NA}.}

\value{In the 1-dimensional case, a list containing vectors \code{average} and
    \code{xout}.  In the 2-dimensional case, a list containing matrix
    \code{average} along with vectors \code{x1out} and \code{x2out}.}

\examples{
library(oce)

## 1. one-dimensional with a CTD profile
data(ctd)
ba <- boxcarAverage(ctd[['pressure']], ctd[['temperature']], seq(0, 50, 5))
plotProfile(ctd, "temperature")
lines(ba$average, ba$xout, col='red')

## 2. two-dimensional with fake data
n <- 1000
x <- runif(n)
y <- runif(n)
z <- x - y
plot(x, y, pch=20, col='gray')
ba <- boxcarAverage2D(x, y, z)
contour(ba$x1out, ba$x2out, ba$average, add=TRUE, labcex=1)
}

\author{Dan Kelley}

\keyword{misc}

