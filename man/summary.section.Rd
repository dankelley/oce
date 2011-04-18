\name{summary.section}

\alias{summary.section}
\alias{print.summary.section}

\title{Summarize a CTD section}

\description{Summarize a CTD section.}

\usage{
\method{summary}{section}(object, \dots)

\method{print}{summary.section}(x, digits=max(6, getOption("digits")-1), \dots)
}

\arguments{
  \item{object}{an object of class \code{"section"}, usually, a result of a
    call to \code{\link{read.section}}, \code{\link{read.oce}}, or
    \code{\link{makeSection}}.}
  \item{x}{an object of class \code{"summary.section"}, usually, a
    result of a call to \code{summary.section}.}
  \item{digits}{the number of significant digits to use when printing.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented, including station
  locations, distance along track, etc.}

\seealso{A \code{section} object may be created by using
  \code{\link{makeSection}} to assemble individual \code{ctd} objects,
  or it may be created by reading a section data file with
  \code{\link{read.section}}.}

\examples{
library(oce)
data(section)
summary(section)
}

\author{Dan Kelley}

\keyword{misc}
