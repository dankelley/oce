\name{summary.logger}

\alias{summary.logger}
\alias{summary,logger,missing-method}
\alias{summary,logger-method}

\title{Summarize a logger object}

\description{Summarize a logger object}

\usage{\S4method{summary}{logger}(object, \dots)}

\arguments{
  \item{object}{an object of class \code{"logger"}, usually, a result of a
    call to \code{read.logger}.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented, including the start
  and stop times, as well as statistics of temperature and pressure.}

\value{\code{NULL}}

\seealso{The documentation for \code{\link{logger-class}} explains the structure of
    PT objects, and also outlines the other functions dealing with them.}

\examples{
library(oce)
data(logger)
summary(logger)
}

\author{Dan Kelley}

\keyword{misc}
