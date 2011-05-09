\name{binAverage}

\alias{binAverage}

\title{Bin-average a vector y, based on x values}

\description{Bin-average a vector y, based on x values}

\usage{binAverage(x, y, xmin, xmax, xinc)}

\arguments{
  \item{x}{a vector of numerical values.}

  \item{y}{a vector of numerical values.}

  \item{xmin}{x value at the lower limit of first bin; the minimum \code{x}
      will be used if this is not provided.}
  
  \item{xmax}{x value at the upper limit of last bin; the maximum \code{x} will
      be used if this is not provided.}
 
  \item{xinc}{width of bins, in terms of x value; 1/10th of \code{xmax-xmin}
      will be used if this is not provided.}

}

\details{The \code{y} vector is averaged in bins defined for \code{x}.  Missing
    values in \code{y} are ignored.}

\value{A list with two elements: \code{x}, the mid-points of the bins, and
    \code{y}, the average \code{y} value in the bins.}

\examples{
library(oce)
data(co2)
plot(co2)
avg <- binAverage(time(co2), co2, 1950, 2000, 2)
lines(avg$x, avg$y, col='red')
}

\author{Dan Kelley}

\keyword{misc}
