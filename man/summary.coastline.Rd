\name{summary.coastline}

\alias{summary.coastline}

\title{Summarize a coastline data object}

\description{Summarizes coastline length, bounding box, etc.}

\usage{
\method{summary}{coastline}(object, \dots)
}

\arguments{
  \item{object}{an object of class \code{"coastline"}, usually, a result of a
    call to \code{\link{read.coastline}} or \code{\link{read.oce}}.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\seealso{The \code{coastline} object may be read with
  \code{\link{read.coastline}} and plotted with
  \code{\link{plot.coastline}}.}

\value{An object of class \code{"summary.coastline"}, which contains
  pertinent information about the Lobo record and its processing.}

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
