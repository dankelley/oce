\name{summary.coastline}

\alias{summary.coastline}

\alias{print.summary.coastline}

\title{Summarize a coastline data object}

\description{Summarizes coastline length, bounding box, etc.}

\usage{
\method{summary}{coastline}(object, \dots)

\method{print}{summary.coastline}(x, digits=max(6, getOption("digits")-1), \dots)
}

\arguments{
  \item{object}{A \code{coastline} object, e.g. as read by \code{\link{read.coastline}}.}
  \item{x}{an object of class \code{"coastline"}, usually, a result of a
    call to \code{\link{read.coastline}} or \code{\link{read.oce}}.}
  \item{digits}{the number of significant digits to use when printing.}
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
