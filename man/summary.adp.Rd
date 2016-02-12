\name{summary.adp}

\alias{summary.adp}
\alias{summary,adp,missing-method}
\alias{summary,adp-method}

\title{Summarize an ADP object}

\description{Summarize data in an \code{adp} object.}

\usage{\S4method{summary}{adp}(object, \dots)}

\arguments{
  \item{object}{an object of class \code{"adp"}, usually, a result of a
    call to \code{\link{read.oce}}, \code{\link{read.adp.rdi}},
    or \code{\link{read.adp.nortek}}.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented.}

\value{A matrix containing statistics of the elements of the \code{data} slot.}

\seealso{The documentation for \code{\link{adp-class}} explains the structure
    of ADP objects, and also outlines the other functions dealing with them.}

\examples{
library(oce)
data(adp)
summary(adp)
}

\author{Dan Kelley}

\keyword{misc}
