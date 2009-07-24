\name{oce.filter}
\alias{oce.filter}
\title{Filter a time-series, possibly recursively}
\description{Filter a time-series, possibly recursively}
\usage{oce.filter(b, a, x)}
\arguments{
\item{b}{a vector of numeric values, giving the \eqn{b}{b} coefficients (see \dQuote{Details}).}
\item{a}{a vector of numeric values, giving the \eqn{a}{a} coefficients (see \dQuote{Details}).}
\item{x}{a vector of numeric values, to be filtered as a time series.}
}

\details{The filter is defined as
\eqn{y_i=\sum_{i=1}^n(b_ix_i) - \sum_{i=2}^n(a_iy_i)}{y_i=\sum_{i=1}^n(b_ix_i) - \sum_{i=2}^n(a_iy_i)}
where the sums are trimmed at the beginning to avoid looking at undefined offsets.

The filter works exactly as does Matlab's function of the same name.
}

\value{A numeric vector of the filtered results, \eqn{y}{y} as denoted in \dQuote{Details}.}


\examples{
library(oce)
b <- rep(1,5)/5
a <- 1
x <- seq(1, 4, by=0.2)
plot(x, oce.filter(b, a, x))
}

\author{Dan Kelley}
\keyword{misc}
