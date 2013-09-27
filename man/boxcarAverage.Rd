\name{boxcarAverage}

\alias{boxcarAverage}

\title{Boxcar averaging}

\description{Boxcar averaging}

\usage{boxcarAverage(x, y, xout=pretty(x))}

\arguments{
    \item{x}{vector of x values}
    \item{y}{vector of y values}
    \item{xout}{vector of grid x values}
}

\details{Performs boxcar averaging of vector data, with bins centred on the
    \code{xout} values.}

\value{A list containing \code{average} and \code{xout}.}

\examples{
library(oce)
x <- seq(0, 10, length.out=100)
y <- 1 + x + x^2
plot(x, y)
ba <- boxcarAverage(x, y)
## Note that right-most xout point is biased low
points(ba$xout, ba$average, col='red')

# data
data(ctd)
ba <- boxcarAverage(ctd[['pressure']], ctd[['temperature']], seq(0, 50, 5))
plotProfile(ctd, "temperature")
points(ba$average, ba$xout, col='red')
}

\author{Dan Kelley}

\keyword{misc}

