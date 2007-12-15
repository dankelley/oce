\name{summary.section}
\alias{summary.section}
\title{Summarize a CTD section}
\description{
  Summarize a CTD section.
}
\usage{
\method{summary}{section}(object, quiet, \dots)
}
\arguments{
  \item{object}{a \code{section} object, e.g. as read by \code{\link{make.section}}.}
  \item{quiet}{set \code{TRUE} to prevent this from printing anything, e.g. if the goal is just to get the return value.}
  \item{\dots}{passed to children.}
}
\details{
  Pertinent summary information is presented about the stations in the section.
}
\value{
Returns (invisibly) a data frame with elements \code{filename},
the original name of CTD file for a given station, and the station coordinates
stored in 
\code{latitude} (decimal degrees, positive north of the equator), and
\code{longitude} (in decimal degrees, positive east of Greenwich).
}

\seealso{
A \code{section} object may be created by using \code{\link{make.section}}
to assemble individual \code{ctd} objects, or it may be created by reading
a section data file with \code{\link{read.section}}.
}
\examples{
library(oce)
data(section)
summary(section)
}
\author{Dan Kelley}
\keyword{misc}
