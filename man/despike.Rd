\name{despike}

\alias{despike}

\title{Remove spikes from a time series}

\description{Remove spikes from a time series}

\usage{despike(x, n=3)}

\arguments{
  \item{x}{a vector}
  \item{n}{number of standard-deviation increments to tolerate}
}

\details{FIXME: fill in more information.  Some of this must be
  published somewhere, so quote the literature, too.}

\examples{
x <- rnorm(n=100)
x[50] <- 4
xx <- despike(x)
}

\author{Dan Kelley}

\keyword{misc}
