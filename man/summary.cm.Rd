\name{summary.cm}

\alias{summary.cm}
\alias{summary,cm,missing-method}
\alias{summary,cm-method}

\title{Summarize a cm (current meter) object}

\description{Summarize a \code{cm} (current meter) object}

\usage{\S4method{summary}{cm}(object, \dots)}

\arguments{
  \item{object}{an object of class \code{"cm"}, as read by
  \code{\link{read.cm.s4}}, perhaps called automatically by
  \code{\link{read.cm}} or \code{\link{read.oce}}.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented.}

\value{A matrix containing statistics of the elements of the \code{data} slot.}

\seealso{The documentation for \code{\link{cm-class}} explains the structure of
    CM objects, and also outlines the other functions dealing with them.}

\examples{
library(oce)
data(cm)
summary(cm)
}

\author{Dan Kelley}

\keyword{misc}
