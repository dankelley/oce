\name{summary.windrose}

\alias{summary.windrose}
\alias{print.summary.windrose}

\title{Summarize a windrose data object}

\description{Summarizes some of the data in a \code{windrose} object.}

\usage{
\method{summary}{windrose}(object, \dots)

\method{print}{summary.windrose}(x, digits=max(6, getOption("digits")-1), \dots)
}

\arguments{
  \item{object}{an object of class \code{"windrose"}, usually, a result of a
    call to \code{\link{as.windrose}}.}
  \item{x}{an object of class \code{"summary.windrose"}, usually, a
    result of a call to \code{summary.windrose}.}
  \item{digits}{the number of significant digits to use when printing.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented.}

\value{An object of class \code{"summary.windrose"}, which contains
  pertinent information about the windrose record and its processing.}

\seealso{A \code{windrose} object may be created with
  \code{\link{as.windrose}} or plotted with
  \code{\link{plot.windrose}}.}

\examples{
library(oce)
xcomp <- rnorm(360) + 1
ycomp <- rnorm(360)
wr <- as.windrose(xcomp, ycomp)
summary(wr)
}

\author{Dan Kelley}

\keyword{misc}
