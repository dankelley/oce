\name{subset.oce}

\alias{subset.oce}

\title{Subset an oce object}

\description{Subset an oce object}

\usage{\method{subset}{oce}(x, subset, \dots)}

\arguments{
  \item{x}{an \code{oce} object.}
  \item{subset}{a condition to be applied to the \code{data} portion of \code{x}.}
  \item{\dots}{ignored.}
}

\details{This is analogous to \code{\link{subset.data.frame}}.  Note
that it only works for objects in which the \code{data} portion is a
data frame, e.g. \code{ctd}, \code{rbrtdr}.}

\value{A new \code{oce} object.}

\seealso{\code{\link{subset.data.frame}}, upon which this is based.}

\examples{
library(oce)
data(ctd)
plot(ctd)
plot(subset(ctd, pressure>10))
}

\author{Dan Kelley}

\keyword{misc}
