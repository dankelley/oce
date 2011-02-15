\name{despike}

\alias{despike}

\title{Remove spikes from a time series}

\description{Remove spikes from a time series}

\usage{despike(x, method=c("median","smooth", "mean"), n=4, k=7, physical.range)}

\arguments{
  \item{x}{a vector}
  \item{method}{number indicating the method used to develop the
      smoothed estimate of \code{x}; see \sQuote{Details}.}
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

  If \code{method="median"}, then a running median is calculated with
  \code{\link{runmed}}, with the running length given by \code{k}.  This is
  taken as a reference series.  Then the absolute value of the difference
  between this reference series and the original is divided by the overall
  standard deviation of the original series, and if the result exceeds
  \code{n}, the point is flagged as suspect.  

  If \code{method="smooth"}, then \code{\link{smooth}} is used to create the
  reference series, and the other steps are as for \code{"median"}.

  if \code{method="mean"}, then the overall mean of the series is used as the
  reference series, and the other steps are as for \code{"median"}.

  Finally, in all methods, flagged points are replaced with code{NA} values.}

\value{A new vector that is identical to the original one, except that spikes
    and unphysical values are replaced with \code{NA}.}

\note{This is a preliminary verson of the function, and it may change.  In
    particular, either \code{mean} should refer to a running mean, or
    \code{median} should be renamed \code{runmed}.}

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
