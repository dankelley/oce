\name{findInOrdered}

\alias{findInOrdered}

\title{Find indices of tiems in an ordered vector}

\description{Find indices of tiems in an ordered vector}

\usage{findInOrdered(x, f)}

\arguments{
  \item{x}{a numeric vector, in increasing order by value.}
  \item{f}{a numeric vector of items whose indices are sought.}
}

\value{A numerical vector indicating the indices of left-sided neighbors.}

\details{The indices point to the largest items in \code{x} that are less than
    or equal the values in \code{f}.  The method uses a bisection search, so
    the time taken is proportional to \code{length(f) * log2(length(x))}.}

\examples{
findInOrdered(seq(0,10,1), c(1.2,7.3))
}

\author{Dan Kelley}

\keyword{misc}
