\name{oce.smooth}

\alias{oce.smooth}
\alias{oceSmooth}

\title{Smooth an oce object}
\description{Smooth an oce object.}
\usage{oce.smooth(x, \dots)}

\arguments{
  \item{x}{an \code{oce} object.}
  \item{\dots}{parameters to be supplied to \code{\link{smooth}},
    which does the actual work.}
}

\details{Each data element is smoothed as a timeseries. For ADP data,
  this is done along time, not distance.  Time vectors, if any, are
  not smoothed.  A good use of \code{oce.smooth} is for despiking
  noisy data.}

\value{An object of \code{\link[base]{class}} \code{"oce"} that has been
smoothed appropriately.}

\seealso{The work is done with \code{\link{smooth}}, and the \code{...}
  arguments are handed to it directly by \code{oce.smooth}.}

\examples{
library(oce)
data(ctd)
d <- oce.smooth(ctd)
plot(d)
}

\author{Dan Kelley}

\keyword{misc}
