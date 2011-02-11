\name{detrend}

\alias{detrend}

\title{Detrend a set of observations}

\description{Detrend a set of observations}

\usage{detrend(x, y)}

\arguments{
  \item{x}{a vector}
  \item{y}{a vector}
}

\details{Detrends \code{y} by subtracting a linear trend in \code{x}, to make \code{y[1]=0} and
    \code{y[length(y)]=0}.}
    
\value{A new vector that is is zero at the start and end.}

\examples{
x <- seq(0, 0.9 * pi, length.out=50)
y <- sin(x)
plot(x, y)
lines(x, detrend(x, y), col='gray')
abline(h=0, lty='dotted')
}

\author{Dan Kelley}

\keyword{misc}
