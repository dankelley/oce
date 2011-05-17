\name{pressure}

\alias{pressure}
\alias{salinity}
\alias{temperature}

\title{Extract pressure, salinity, or temperature from an oce object}

\description{Extract pressure, salinity, or temperature from an oce object}


\usage{
pressure(x)
salinity(x)
temperature(x)
}

\arguments{
  \item{x}{an \code{oce} object.}
}

\details{This is a convenience function that isolates the user from the details
    of details of storage.}

\value{A vector containing the indicated variable.  For a \code{ctd} object, the returned value corresponds
    directly to the vector \code{x$data$pressure} (etc.), but for a
    \code{section} object, the returned value is built by stringing together
    the values of the indicate variable, station by station.}

\examples{
library(oce)
data(a03)
plot.TS(as.ctd(salinity(a03),temperature(a03),pressure(a03)))
}

\seealso{Similar accessor functions are \code{\link{heading}}, \code{\link{pitch}}, \code{\link{roll}}, 
    \code{\link{velocity}} and \code{\link{time}}.}

\author{Dan Kelley}

\keyword{misc}
