\name{summary.adv}

\alias{summary.adv}
\alias{summary,adv,missing-method}
\alias{summary,adv-method}

\title{Summarize an ADV object}

\description{Summarize data in an \code{adv} object.}

\usage{\S4method{summary}{adv}(object, \dots)}

\arguments{
  \item{object}{an object of class \code{"adv"}, usually, a result of a
    call to \code{\link{read.adv}}.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented.}

\value{A matrix containing statistics of the elements of the \code{data} slot.}

\seealso{The documentation for \code{\link{adv-class}} explains the structure
    of ADV objects, and also outlines the other functions dealing with them.}

\examples{
library(oce)
data(adv)
summary(adv)
}

\author{Dan Kelley}

\keyword{misc}
