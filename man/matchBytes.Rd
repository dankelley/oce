\name{matchBytes}

\alias{matchBytes}

\title{Locate byte sequences in a raw vector}

\description{Find spots in a raw vector that match a given byte sequence.}

\usage{matchBytes(input, b1, \dots)}

\arguments{
\item{input}{a vector of raw (byte) values.}
\item{b1}{a vector of bytes to match (must be of length 2 or 3 at present; for 1-byte, use \code{\link{which}}).}
\item{\dots}{additional bytes to match for (up to 2 permitted)}
}

\value{List of the indices of \code{input} that match the start of the \code{bytes} sequence (see example).}

\examples{
buf <- as.raw(c(0xa5, 0x11, 0xaa, 0xa5, 0x11, 0x00))
match <- matchBytes(buf, 0xa5, 0x11)
print(buf)
print(match)
}

\author{Dan Kelley}

\keyword{misc}
