\name{summary.processingLog}

\alias{summary.processingLog}
\alias{print.summary.processingLog}

\title{Summarize the processingLog of an Oce object}

\description{Summarize the processingLog of an Oce object}

\usage{
\method{summary}{processingLog}(object, \dots)
\method{print}{summary.processingLog}(x, digits=max(6, getOption("digits")-1), \dots)
}

\arguments{
  \item{object}{a processingLog list}
  \item{x}{an object of class \code{"summary.processingLog"}, usually, a result
    of a call to \code{summary.processingLog}.}
  \item{digits}{the number of significant digits to use when printing.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented, including the start
  and stop times, as well as statistics of temperature and pressure.}

\value{An object of class \code{"summary.processingLog"}, which contains
  pertinent information about the TDR record and its processing.}

\seealso{See \code{\link{read.pt}} for more information on dealing
  with the \code{pt} data type, including examples.}

\author{Dan Kelley}

\keyword{misc}
