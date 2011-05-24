\name{threenum}

\alias{threenum}

\title{Calculate min, mean, and max values}

\description{Calculate min, mean, and max values}

\usage{threenum(x)}

\arguments{
  \item{x}{a vector or matrix of numerical values.}
}

\details{This is a faster cousin of the standard \code{\link{fivenum}} function.}
      
\value{A vector of three values, the minimum, the mean, and the maximum.}

\examples{
library(oce)
threenum(1:10)
}

\author{Dan Kelley}

\keyword{misc}
