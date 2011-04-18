\name{secondsToCtime}

\alias{secondsToCtime}

\title{Convert a time interval to a colon-separated string}

\description{Convert a time interval to a colon-separated string}

\usage{secondsToCtime(sec)}

\arguments{
  \item{sec}{length of time interval in seconds.}
}

\details{Strings are }

\value{A string with a colon-separated time interval.}

\examples{
library(oce)
cat("   10 s = ", secondsToCtime(10), "\n", sep="")
cat("   61 s = ", secondsToCtime(61), "\n", sep="")
cat("86400 s = ", secondsToCtime(86400), "\n", sep="")
}

\seealso{See \code{\link{ctimeToSeconds}}, the inverse of this.}

\author{Dan Kelley}

\keyword{misc}
