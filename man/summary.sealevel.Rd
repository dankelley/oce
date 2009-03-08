\name{summary.sealevel}

\alias{summary.sealevel}

\alias{print.summary.sealevel}

\title{Summarize a sealevel data object}

\description{Summarizes some of the data in a sealevel object.}

\usage{
\method{summary}{sealevel}(object, \dots)

\method{print}{summary.sealevel}(x, digits=max(6, getOption("digits")-1), \dots)

}

\arguments{
  \item{object}{A \code{sealevel} object, e.g. as read by
    \code{\link{read.sealevel}} or created with \code{\link{as.sealevel}}.}
  \item{x}{an object of class \code{"sealevel"}, usually, a result of a
    call to \code{summary.sealevel}.}
  \item{digits}{Number of digits to use in stats.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented.}

\value{An object of class \code{"summary.sealevel"}, which contains
  pertinent information about the sealevel record and its processing.}

\seealso{The data may be read with \code{\link{read.sealevel}}, compiled
  with \code{\link{as.sealevel}}, or plotted with.}

\examples{
library(oce)
data(sealevel.hal)
summary(sealevel.hal)
}

\author{Dan Kelley}

\keyword{misc}
