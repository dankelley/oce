\name{byte2binary}

\alias{byte2binary}

\title{Format bytes as binary}

\description{Format bytes as binary}

\usage{byte2binary(x)}

\arguments{
  \item{x}{an integer to be interpreted as a byte.}
}

\value{A character string representing the bit strings for the elements
  of \code{x}.}

\examples{
library(oce)
x <- 0:16
print(byte2binary(x))
}

\author{Dan Kelley}

\keyword{misc}
