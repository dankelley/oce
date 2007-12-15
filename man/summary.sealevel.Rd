\name{summary.sealevel}
\alias{summary.sealevel}
\title{Summarize a sealevel data object}
\description{
  Summarizes some of the data in a sealevel object.
}
\usage{
\method{summary}{sealevel}(object, \dots)
}
\arguments{
  \item{object}{A \code{sealevel} object, e.g. as read by \code{\link{read.sealevel}}.}
  \item{\dots}{Passed to children.}
}
\details{
  Pertinent summary information is presented.
}
\value{No value is returned.}
\seealso{
  \code{\link{read.sealevel}} reads the information, while
  \code{\link{plot.sealevel}} plots it.
}
\examples{
library(oce)
data(sealevel)
summary(sealevel)
}
\references{
\url{ftp://ilikai.soest.hawaii.edu/rqds/hourly.fmt}.
}
\author{Dan Kelley}
\keyword{misc}
