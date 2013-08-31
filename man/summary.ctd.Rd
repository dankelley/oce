\name{summary.ctd}

\alias{summary.ctd}

\title{Summarize a CTD object}

\description{Summarizes some of the data in a \code{ctd} object.}

\usage{
\method{summary}{ctd}(object, \dots)
}

\arguments{
  \item{object}{an object of class \code{"ctd"}, usually, a result of a
    call to \code{\link{read.ctd}}, \code{\link{read.oce}}, or
    \code{\link{as.ctd}}.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented, including the station
    name, sampling location, data ranges, etc.}

\value{An object of class \code{"summary.ctd"}, which when printed shows
  pertinent information about the CTD record and its processing.}

\seealso{The documentation for \code{\link{ctd-class}} explains the structure
    of CTD objects, and also outlines the other functions dealing with them.}

\examples{
library(oce)
data(ctd)
summary(ctd)
}


\author{Dan Kelley}

\keyword{misc}
