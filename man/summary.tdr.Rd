\name{summary.tdr}

\alias{summary.tdr}

\title{Summarize a tdr object}

\description{Summarize a tdr object}

\usage{
\method{summary}{tdr}(object, \dots)
}

\arguments{
  \item{object}{an object of class \code{"tdr"}, usually, a result of a
    call to \code{read.tdr}.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented, including the start
  and stop times, as well as statistics of temperature and pressure.}

\value{An object of class \code{"summary.tdr"}, which contains
  pertinent information about the TDR record and its processing.}

\seealso{The documentation for \code{\link{tdr-class}} explains the structure of
    PT objects, and also outlines the other functions dealing with them.}

\examples{
library(oce)
data(tdr)
summary(tdr)
}

\author{Dan Kelley}

\keyword{misc}
