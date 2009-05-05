\name{make.filter}
\alias{make.filter}
\title{Make a digital filter}
\description{make a digital filter}
\usage{make.filter(type=c("blackman-harris"), m)}
\arguments{
  \item{type}{a string indicating the type of filter.  Only one type is
    provided so far, the Blackman-Harris filter, a modified
    raised-cosine filter.  For a discussion
    of this and other filters, see Harris (1978).}
  \item{m}{sugggested length of filter.  If \code{m} is an even number,
    the filter length is set to \code{m+1}.}
}

\details{The filter is suitable for use by \code{\link{filter}} or
  \code{\link{convolve}}.  Note that \code{\link{convolve}} should be
  faster than \code{\link{filter}}, but it cannot be used if the time
  series has missing values.  The half-power frequency is \code{1/m} cycles
  per time unit (see \dQuote{Examples}).}

\value{A list of filter coefficients, symmetric about the midpoint and
  summing to 1.}

\examples{
library(oce)
x <- c(rep(0,30),rep(1,30),rep(0,30))
plot.ts(x)
x1 <- filter(x, make.filter("blackman-harris", 5))
lines(x1, col='red')
x2 <- filter(x, make.filter("blackman-harris", 10))
lines(x2, col='blue')
legend("topright", lwd=1, col=c("red", "blue"), legend=c("m=5", "m=10"))

# Spectral representation
r <- rnorm(1e4)
r.spec <- spectrum(r, spans=c(21,5,3), plot=FALSE)
length <- 10
f <- make.filter("blackman-harris", length)
r.lowpass <- filter(r, f, circular=TRUE)
r.lowpass.spec <- spectrum(r.lowpass, spans=c(21,5,3), plot=FALSE)

plot(r.spec$freq, r.spec$spec, ylim=c(0.01, 2), log="xy", type="l")
lines(r.lowpass.spec$freq, r.lowpass.spec$spec, col="red")
abline(v=1/length, col="blue")
abline(h=1, col="blue")
abline(h=1/2, col="blue")               # half power
}

\references{F. J. Harris, 1978.  On the use of windows for harmonic
  analysis with the discrete Fourier Transform.
  \emph{Proceedings of the IEEE}, 66(1), 51-83
  (\url{http://web.mit.edu/xiphmont/Public/windows.pdf}.)}

\author{Dan Kelley}

\keyword{misc}
