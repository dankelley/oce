\name{summary.pt}

\alias{summary.pt}

\title{Summarize a pt object}

\description{Summarize a pt object}

\usage{
\method{summary}{pt}(object, \dots)
}

\arguments{
  \item{object}{an object of class \code{"pt"}, usually, a result of a
    call to \code{read.pt}.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented, including the start
  and stop times, as well as statistics of temperature and pressure.}

\value{An object of class \code{"summary.pt"}, which contains
  pertinent information about the TDR record and its processing.}

\seealso{The documentation for \code{\link{pt-class}} explains the structure of
    PT objects, and also outlines the other functions dealing with them.}

\examples{
library(oce)
data(ptr)
summary(ptr)
}

\author{Dan Kelley}

\keyword{misc}
