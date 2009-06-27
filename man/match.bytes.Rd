\name{match.bytes}

\alias{match.bytes}

\title{Find spots in a raw vector that match a given byte sequence.}

\description{Find spots in a raw vector that match a given byte sequence.}

\usage{match.bytes(input, bytes)}

\arguments{
\item{input}{a vector of raw values.}
\item{bytes}{a vector of bytes to match (must be of length 2 or 3 at present; for 1-byte, use \code{\link{which}}).}
}

\value{List of the indices of \code{input} that match the start of the \code{bytes} sequence (see example).}

\examples{
buf <- as.raw(c(0xa5, 0x11, 0xaa, 0xa5, 0x11, 0x00))
match <- match.bytes(buf, as.raw(c(0xa5, 0x11)))
print(buf)
print(match)
}

\author{Dan Kelley}

\keyword{misc}
