% vim:textwidth=100:expandtab:shiftwidth=2:softtabstop=2
\name{pressure}
\alias{pressure}
\alias{salinity}
\alias{sigmaTheta}
\alias{temperature}
\alias{latitude}
\alias{longitude}
\alias{time}
\alias{elevation}

\concept{data extraction}

\title{Extract pressure, salinity, or temperature from an oce object}

\description{Extract pressure, salinity, or temperature from an oce object}


\usage{
  pressure(x, time)
  salinity(x, time)
  sigmaTheta(x, time)
  temperature(x, time)
  latitude(x, time)
  longitude(x, time)
  time(x, time)
  elevation(x, time)
}

\arguments{
  \item{x}{an \code{oce} object.}

  \item{time}{optional indication of times to which to interpolate.  This may be a vector of POSIXt
    times, or an \code{oce} object that contains a vector of POSIXt times.}

}

\details{This is a convenience function that isolates the user from the details of storage.}

\value{A vector containing the indicated variable.  For a \code{ctd} object, the returned value
  corresponds directly to the vector \code{x$data$pressure} (etc.), but for a \code{section} object,
  the returned value is built by stringing together the values of the indicate variable, station by
  station.}

\examples{
  library(oce)
  data(ctd)
  cat("Average ctd temperature:", mean(temperature(ctd), na.rm=TRUE), "\n")
  data(a03)
  plot.TS(as.ctd(salinity(a03),temperature(a03),pressure(a03)))
}

\seealso{Similar accessor functions are \code{\link{heading}}, \code{\link{pitch}},
  \code{\link{roll}}, \code{\link{velocity}} and \code{\link{time}}.}

\author{Dan Kelley}

\keyword{misc}
