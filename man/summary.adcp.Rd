\name{summary.adcp}

\alias{summary.adcp}
\alias{print.summary.adcp}

\title{Summarize an adcp object}

\description{Summarize data in an \code{adcp} object.}

\usage{
\method{summary}{adcp}(object, \dots)

\method{print}{summary.adcp}(x, digits=max(6, getOption("digits")-1), \dots)
}

\arguments{
  \item{object}{an object of class \code{"adcp"}, usually, a result of a
    call to \code{\link{read.oce}}, \code{\link{read.adcp.rdi}},
    or \code{\link{read.adcp.nortek}}.}
  \item{x}{an object of class \code{"summary.adcp"}, usually, a result of a
    call to \code{summary.adcp}.}
  \item{digits}{the number of significant digits to use when printing.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented.}

\value{An object of class \code{"summary.adcp"}, which contains
  pertinent information about the ADCP record and its processing.}

\seealso{A \code{adcp} object may  be read with \code{\link{read.adcp}} or
  \code{\link{read.oce}}.}

\examples{
library(oce)
data(adcp)
summary(adcp)
}

\author{Dan Kelley}

\keyword{misc}
