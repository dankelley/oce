\name{summary.adp}

\alias{summary.adp}

\title{Summarize an ADP object}

\description{Summarize data in an \code{adp} object.}

\usage{
\method{summary}{adp}(object, \dots)
}

\arguments{
  \item{object}{an object of class \code{"adp"}, usually, a result of a
    call to \code{\link{read.oce}}, \code{\link{read.adp.rdi}},
    or \code{\link{read.adp.nortek}}.}
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
