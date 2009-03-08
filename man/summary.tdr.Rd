\name{summary.tdr}

\alias{summary.tdr}
\alias{print.summary.tdr}

\title{Summarize an TDR temperature-depth data object}

\description{Summarize an TDR temperature-depth data object}

\usage{
\method{summary}{tdr}(object, \dots)

\method{print}{summary.tdr}(x, digits=max(6, getOption("digits")-1), \dots)
}

\arguments{
  \item{object}{an object of class \code{"tdr"}, usually, a result of a
    call to \code{read.tdr}.}
  \item{x}{an object of class \code{"summary.tdr"}, usually, a result
    of a call to \code{summary.tdr}.}
  \item{digits}{the number of significant digits to use when printing.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented, including the start
  and stop times, as well as statistics of temperature and pressure.}

\value{An object of class \code{"summary.tdr"}, which contains
  pertinent information about the TDR record and its processing.}

\seealso{See \code{\link{read.tdr}} for more information on dealing
  with the \code{tdr} data type, including examples.}

\examples{
library(oce)
data(tdr)
plot(tdr)
tdr.trimmed <- tdr.trim(tdr)
plot(tdr.trimmed)
}

\author{Dan Kelley}

\keyword{misc}
