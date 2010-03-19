\name{make.filter}
\alias{make.filter}
\title{Make a digital filter}
\description{make a digital filter}
\usage{make.filter(type=c("blackman-harris","rectangular", "hamming", "hann"), m)}
\arguments{
  \item{type}{a string indicating the type of filter: \code{"blackman-harris"}
    for the Blackman-Harris filter (a modified raised-cosine filter), 
    \code{"rectangular"} for a flat filter, \code{"hamming"} for a Hamming
    filter (a raised-cosine that does not taper to zero at the ends), or
    \code{"hann"} (a raised cosine that tapers to zero at the ends).
    Many sources define these filters; see Harris (1978) for a comparison of
    these and other filters.}
  \item{m}{length of filter.  This should be an odd number, for any
    non-rectangular filter.}
}

\details{The filter is suitable for use by \code{\link{filter}} or
  \code{\link{convolve}}.  Note that \code{\link{convolve}} should be
  faster than \code{\link{filter}}, but it cannot be used if the time
  series has missing values.  For the Blackman-Harris filter,
  the half-power frequency is possibly \code{1/m} cycles per time unit 
  (see \dQuote{Examples}).  When using \code{\link{filter}} with these 
  filters, use \code{circular=TRUE}.}

\value{A list of filter coefficients, symmetric about the midpoint and
  summing to 1.  These may be used with \code{\link{filter}}, which
  should be provided with argument \code{circular=TRUE} to avoid phase offsets.}

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
