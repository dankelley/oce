\name{detrend}

\alias{detrend}

\title{Detrend a set of observations}

\description{Detrend a set of observations}

\usage{detrend(x, y)}

\arguments{
  \item{x}{a vector of numerical values.  If \code{y} is not given, then \code{x} is taken for
      \code{y}.}
  \item{y}{an optional vector}
}

\details{Detrends \code{y} by subtracting a linear trend in \code{x}, to make \code{y[1]=0} and
    \code{y[length(y)]=0}.  If \code{y} is not given, then y is taken from x, and x is set to the
    series of integers from 1 to \code{length{x}}.}
    
\value{A new vector that is is zero at the start and end.}

\examples{
x <- seq(0, 0.9 * pi, length.out=50)
y <- sin(x)
plot(x, y)
lines(x, detrend(y), col='gray')
abline(h=0, lty='dotted')
}

\author{Dan Kelley}

\keyword{misc}
