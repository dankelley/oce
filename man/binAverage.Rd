\name{binAverage}

\alias{binAverage}

\title{Bin-average a vector y, based on x values}

\description{Bin-average a vector y, based on x values}

\usage{binAverage(x, y, xmin, xmax, xinc)}

\arguments{
  \item{x}{a vector of numerical values.}
  \item{y}{a vector of numerical values.}
  \item{xmin}{x value at the lower limit of first bin.}
  \item{xmax}{x value at the upper limit of last bin.}
  \item{xinc}{width of bins, in terms of x value.}
}

\details{The \code{y} vector is averaged in bins defined for \code{x}.}

\value{A vector of \code{y} averages in the designated \code{x} bins.}

\examples{
library(oce)
x <- 1:60 + 0.5
y <- 2 * x
xmin <- 10
xmax <- 40
xinc <- 10
binned <- binAverage(x, y, xmin, xmax, xinc)
plot(x, y)
points(xinc/2 + seq(xmin, xmax-xinc, xinc), binned, col='red', cex=3)
lines(xinc/2 + seq(xmin, xmax-xinc, xinc), binned, col='red')
abline(v=seq(xmin, xmax, xinc))
}

\author{Dan Kelley}

\keyword{misc}
