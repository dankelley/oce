\name{summary.etopo2}
\alias{summary.etopo2}
\title{Summarize a topography (ETOPO2) data object}
\description{
Summarizes some of the data in an \code{etopo2} object.
}
\usage{
\method{summary}{etopo2}(object, \dots)
}
\arguments{
  \item{object}{A \code{etopo2} object, e.g. as read by \code{\link{read.etopo2}}.}
  \item{\dots}{passed to children.}
}
\details{
Pertinent summary information is presented, including the latitude and longitude
range, and the range of elevation.
}
\value{
NULL
}

\seealso{
A \code{etopo2} object may be read with \code{\link{read.etopo2}}.
}
\examples{
library(oce)
data(bof)
summary(bof)
}
\references{
}
\author{Dan Kelley}
\keyword{misc}
