\name{summary.argo}

\alias{summary.argo}
\alias{summary,argo,missing-method}
\alias{summary,argo-method}

\title{Summarize an Argo object}

\description{Summarizes some of the data in an \code{argo} object.}

\usage{\S4method{summary}{argo}(object, \dots)}

\arguments{
  \item{object}{an object of class \code{"argo"}, usually, a result of a
    call to \code{\link{read.argo}}.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented.}

\value{A matrix containing statistics of the elements of the \code{data} slot.}

\seealso{The documentation for \code{\link{argo-class}} explains the
    structure of argo objects, and also outlines the other functions dealing
    with them.}

\examples{
library(oce)
data(argo)
summary(argo)
}

\references{\url{http://www.argo.ucsd.edu/}}

\author{Dan Kelley}

\keyword{misc}
