\name{summary.adp}

\alias{summary.adp}
\alias{print.summary.adp}

\title{Summarize an adp object}

\description{Summarize data in an \code{adp} object.}

\usage{
\method{summary}{adp}(object, \dots)

\method{print}{summary.adp}(x, digits=max(6, getOption("digits")-1), \dots)
}

\arguments{
  \item{object}{an object of class \code{"adp"}, usually, a result of a
    call to \code{\link{read.oce}}, \code{\link{read.adp.rdi}},
    or \code{\link{read.adp.nortek}}.}
  \item{x}{an object of class \code{"summary.adp"}, usually, a result of a
    call to \code{summary.adp}.}
  \item{digits}{the number of significant digits to use when printing.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented.}

\value{An object of class \code{"summary.adp"}, which contains
  pertinent information about the ADP record and its processing.}

\seealso{A \code{adp} object may  be read with \code{\link{read.adp}} or
  \code{\link{read.oce}}.}

\examples{
library(oce)
data(adp)
summary(adp)
}

\author{Dan Kelley}

\keyword{misc}
