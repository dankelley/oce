\name{velocity}

\alias{velocity}

\title{Extract velocity from an oce object}

\description{Extract velocity from an oce object}

\usage{velocity(x)}

\arguments{
  \item{x}{an \code{oce} object.}
}

\details{This is a convenience function that isolates the user from the details
    of details of storage.}

\value{A matrix or array of the velocity data.}

\examples{
library(oce)
data(adp)
print(dim(velocity(adp)))
}

\seealso{Similar accessor functions are \code{\link{heading}}, \code{\link{pitch}}, \code{\link{roll}}, and
    \code{\link{time}}.}

\author{Dan Kelley}

\keyword{misc}
