\name{byte2binary}

\alias{byte2binary}

\title{Format bytes as binary}

\description{Format bytes as binary}

\usage{byte2binary(x, endian=c("little", "big"))}

\arguments{
  \item{x}{an integer to be interpreted as a byte.}
  \item{endian}{character string indicating the endian-ness ("big" or
   "little").  The PC/intel convention is to use "little", and so most
   data files are in that format.}
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
