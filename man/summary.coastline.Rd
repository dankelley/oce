\name{summary.coastline}

\alias{summary.coastline}

\title{Summarize a coastline data object}

\description{Summarizes coastline length, bounding box, etc.}

\usage{
\method{summary}{coastline}(object, \dots)
}

\arguments{
  \item{object}{A \code{coastline} object, e.g. as read by \code{\link{read.coastline}}.}
  \item{\dots}{Passed to children.}
}

\seealso{The \code{coastline} object may be read with
  \code{\link{read.coastline}} and plotted with
  \code{\link{plot.coastline}}.}

\examples{
\dontrun{
library(oce)
cl <- read.coastline("7404.dat")
plot(cl)
}
}

\references{http://www.ngdc.noaa.gov/mgg/shorelines/shorelines.html}

\author{Dan Kelley}

\keyword{misc}
