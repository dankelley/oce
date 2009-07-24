\name{oce.filter}
\alias{oce.filter}
\title{Filter a time-series, possibly recursively}
\description{Filter a time-series, possibly recursively}
\usage{oce.filter(b, a=1, x)}
\arguments{
\item{b}{a vector of numeric values, giving the \eqn{b}{b} coefficients (see \dQuote{Details}).}
\item{a}{a vector of numeric values, giving the \eqn{a}{a} coefficients (see \dQuote{Details}).}
\item{x}{a vector of numeric values, to be filtered as a time series.}
}

\details{The filter is defined as e.g.
\eqn{y[i]=b[1]*x[i] + b[2]*x[i-1] + b[3]*x[i-2] + ... - a[2]*y[i-1] - a[3]*y[i-2] - a[4]*y[i-3] - ...}{y[i]=b[1]*x[i] + b[2]*x[i-1] + b[3]*x[i-2] + ... - a[2]*y[i-1] - a[3]*y[i-2] - a[4]*y[i-3] - ...}

(Of course, not all terms are used if the lengths of \code{a} and
\code{b} are too small for all the terms written above.)  Near the
start of the time series, terms in the sums are dropped as appropriate
to avoid looking before the start time.

Note that the first value in the \code{a} vector is ignored, and if
\code{length(a)} equals 1, the result is a non-recursive filter.

The filter works exactly as does Matlab's function of the same name;
the equality of results against the Matlab example are ensured at the
time of building the \code{oce} package.

By contrast with the \code{\link{filter}} function of R,
\code{oce.filter} lacks the option to do a circular filter.  As a consequence, 
\code{oce.filter} introduces a phase lag.  One way to remove this lag is to 
run the filter forwards and then backwards, as in the \dQuote{Examples}.
}

\value{A numeric vector of the filtered results, \eqn{y}{y} as denoted in \dQuote{Details}.}


\examples{
library(oce)
b <- rep(1,5)/5
a <- 1
x <- seq(1, 4, by=0.2)
y <- oce.filter(b, a, x)
plot(x, y)
points(x, x, pch="x", col="red") # note that y is offset

# remove the phase lag
y <- rev(oce.filter(b, a, rev(oce.filter(b, a, x))))
points(x, y, pch="+", col="blue")
}

\author{Dan Kelley}
\keyword{misc}
