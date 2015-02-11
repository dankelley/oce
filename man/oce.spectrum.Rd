\name{oce.spectrum}

\alias{oce.spectrum}
\alias{oceSpectrum}

\title{Wrapper to give normalized spectrum}
\description{This is a wrapper around the R \code{\link{spectrum}}
  function, which returns spectral values that are adjusted so that the
  integral of those values equals the variance of the input \code{x}.
}
\usage{oce.spectrum(x, ...)}
\arguments{
  \item{x}{As for \code{\link{spectrum}}, a univariate or multivariate
    time series.}
  \item{\dots}{extra arguments passed on to \code{\link{spectrum}}.}
}
\examples{
  x <- rnorm(1e3)
  s <- spectrum(x, plot=FALSE)
  ss <- oce.spectrum(x, plot=FALSE)
  cat("variance of x=", var(x), "\n")
  cat("integral of     spectrum=", sum(s$spec)*diff(s$freq[1:2]), "\n")
  cat("integral of oce.spectrum=", sum(ss$spec)*diff(ss$freq[1:2]), "\n")
}
\value{A spectrum that has values that integrate to the variance.}
\seealso{\code{\link{spectrum}}.}
\author{Dan Kelley}
\keyword{misc}
