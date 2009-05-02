\name{summary.pt}

\alias{summary.pt}
\alias{print.summary.pt}

\title{Summarize an TDR temperature-depth data object}

\description{Summarize an pT data object}

\usage{
\method{summary}{pt}(object, \dots)

\method{print}{summary.pt}(x, digits=max(6, getOption("digits")-1), \dots)
}

\arguments{
  \item{object}{an object of class \code{"pt"}, usually, a result of a
    call to \code{read.pt}.}
  \item{x}{an object of class \code{"summary.pt"}, usually, a result
    of a call to \code{summary.pt}.}
  \item{digits}{the number of significant digits to use when printing.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented, including the start
  and stop times, as well as statistics of temperature and pressure.}

\value{An object of class \code{"summary.pt"}, which contains
  pertinent information about the TDR record and its processing.}

\seealso{See \code{\link{read.pt}} for more information on dealing
  with the \code{pt} data type, including examples.}

\examples{
library(oce)
data(pt)
plot(pt)
pt.trimmed <- pt.trim(pt)
plot(pt.trimmed)
}

\author{Dan Kelley}

\keyword{misc}
