\name{undrift.time}

\alias{undrift.time}

\title{Correct for drift in instrument clock}

\description{Correct for drift in instrument clock}

\usage{d <- undrift.time(x, slow.end=0, tname="t")}

\arguments{
  \item{x}{an object of \code{\link[base]{class}} "\code{oce}".}
  \item{slow.end}{number of seconds to add to final instrument time, to
    get the correct time of the final sample.  This will be a positive
    number, for a "slow" instrument clock.}
  \item{tname}{name of time column.}
}

\details{It is assumed that the instrument clock matches the real time
  at the start of the sampling, and that the clock drifts linearly
  (i.e. is uniformly fast or slow) over the sampling interval.  Linear
  interpolation is used to infer the values of all variables in the
  \code{x$data}.  The data length is altered in this process, e.g. a
  slow instrument clock (positive \code{slow.end}) takes too few samples
  in a given time interval, so \code{undrift.time} will increase the
  number of data.}

\value{An object of the same class as \code{x}, with \code{x$data}
  adjusted appropriately.}

\examples{
\dontrun{
library(oce)
rbr011855 <- read.oce("rbr011855")
d <- subset(rbr011855, t < as.POSIXct("2008-06-25 10:05:00"))
x <- undrift.time(d, 1)	  # a slow instrument clock lost 1 second
summary(d)
summary(x)
}
}

\author{Dan Kelley}

\keyword{misc}
