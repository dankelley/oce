\name{despike}

\alias{despike}

\title{Remove spikes from a time series}

\description{Remove spikes from a time series}

\usage{despike(x, method=1, n=4, k=7, physical.range)}

\arguments{
  \item{x}{a vector}
  \item{method}{number indicating the method; see \sQuote{Details}.}
  \item{n}{number of standard-deviation increments to tolerate}
  \item{k}{length of running median used in algorithm}
  \item{physical.range}{optional two-element vector holding the smallest
    physically-realistic value and the highest one.  (For example, for
    water temperature, one might use \code{c(-3,101)}.)}
}

\details{The method identifies spikes by statistical deviation from a
  smoothed form of the series.  

  The first step is to construct gapless, physically-realistic series
  that has no missing values, and no values outside the physical range
  (if \code{physical.range} is given).  All such values are replaced
  with the overall \code{\link{median}}.

  The next step is to create a smoothed version of this series.  If
  \code{method=1}, a running median is used, calculated with
  \code{\link{runmed}}, with the running length given by \code{k}.  If
  \code{method=2}, \code{\link{smooth}} is used.

  Finally, the difference between this smoothed series and the gapless,
  physically-realistic series is calculated.  Any spots at which this
  difference exceeds its mean value by \code{n} standard deviations are
  flagged as spikes.}

\value{A new vector that is identical to the original one, except that
  spikes and unphysical values are replace with \code{NA}.}

\examples{
n <- 100
x <- 1:n
y <- rnorm(n=n)
y[n/2] <- 10                    # 10 standard deviations
yy <- despike(y)
plot(x, y, type='l')
spike <- is.na(yy)
points(x[spike], y[spike], col="red", cex=3)
}

\author{Dan Kelley}

\keyword{misc}
