\name{seconds.to.ctime}

\alias{seconds.to.ctime}

\title{Convert a time interval to a colon-separated string}

\description{Convert a time interval to a colon-separated string}

\usage{seconds.to.ctime(sec)}

\arguments{
  \item{sec}{length of time interval in seconds.}
}

\details{Strings are }

\value{A string with a colon-separated time interval.}

\examples{
library(oce)
cat("   10 s = ", seconds.to.ctime(10), "\n", sep="")
cat("   61 s = ", seconds.to.ctime(61), "\n", sep="")
cat("86400 s = ", seconds.to.ctime(86400), "\n", sep="")
}

\seealso{See \code{\link{ctime.to.seconds}}, the inverse of this.}

\author{Dan Kelley}

\keyword{misc}
