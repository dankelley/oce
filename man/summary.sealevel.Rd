\name{summary.sealevel}

\alias{summary.sealevel}

\title{Summarize a sealevel data object}

\description{Summarizes some of the data in a sealevel object.}

\usage{\method{summary}{sealevel}(object, digits=4, \dots)}

\arguments{
  \item{object}{A \code{sealevel} object, e.g. as read by \code{\link{read.sealevel}}.}
  \item{digits}{Number of digits to use in stats.}
  \item{\dots}{Passed to children.}
}

\details{Pertinent summary information is presented.}

\value{None.}

\seealso{
  \code{\link{read.sealevel}} reads the information, while
  \code{\link{plot.sealevel}} plots it.}

\examples{
library(oce)
data(sealevel.hal)
summary(sealevel.hal)
}

\references{\url{ftp://ilikai.soest.hawaii.edu/rqds/hourly.fmt}.}

\author{Dan Kelley}

\keyword{misc}
