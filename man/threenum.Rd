\name{threenum}

\alias{threenum}

\title{Calculate min, mean, and max values}

\description{Calculate min, mean, and max values}

\usage{threenum(x)}

\arguments{
  \item{x}{a vector or matrix of numerical values.}
}

\details{This is a faster cousin of the standard \code{\link{fivenum}}
    function, because calculating a median rquires sorting, which is slow.
    Another advantage of this is that it returns also the dimensionality of the
    data.}
      
\value{A character vector of four values: the minimum, the mean, the maximum,
    and an indication of the number of data.}

\examples{
library(oce)
threenum(1:10)
}

\author{Dan Kelley}

\keyword{misc}
