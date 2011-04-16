\name{bcdToInteger}

\alias{bcdToInteger}

\title{Decode BCD to integer}

\description{Decode binary-coded-decimal to integer}

\usage{bcdToInteger(x, endian=c("little", "big"))}

\arguments{
  \item{x}{a raw value, or vector of raw values, coded in binary-coded
    decimal.}
  \item{endian}{character string indicating the endian-ness ("big" or
   "little").  The PC/intel convention is to use "little", and so most
   data files are in that format.}
}

\value{An integer, or list of integers.}

\examples{
library(oce)
twenty.five <- bcdToInteger(as.raw(0x25))
thirty.seven <- as.integer(as.raw(0x25))
}

\author{Dan Kelley}

\keyword{misc}
