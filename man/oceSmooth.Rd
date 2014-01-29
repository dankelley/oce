\name{oceSmooth}
\alias{oceSmooth}
\title{Smooth an oce object}
\description{Smooth an oce object.}
\usage{oceSmooth(x, \dots)}

\arguments{
  \item{x}{an \code{oce} object.}
  \item{\dots}{parameters to be supplied to \code{\link{smooth}},
    which does the actual work.}
}

\details{Each data element is smoothed as a timeseries. For ADP data,
  this is done along time, not distance.  Time vectors, if any, are
  not smoothed.  A good use of \code{oceSmooth} is for despiking
  noisy data.}

\value{An object of \code{\link[base]{class}} \code{"oce"} that has been
smoothed appropriately.}

\seealso{The work is done with \code{\link{smooth}}, and the \code{...}
  arguments are handed to it directly by \code{oceSmooth}.}

\examples{
library(oce)
if (require(ocedata)) {
    data(ctd)
    d <- oceSmooth(ctd)
    plot(d)
}
}

\author{Dan Kelley}

\keyword{misc}
