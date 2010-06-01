\name{make.filter}
\alias{make.filter}
\title{Make a digital filter}
\description{make a digital filter}
\usage{make.filter(type=c("blackman-harris","rectangular", "hamming", "hann"),
            m, asKernel=FALSE)}
\arguments{
  \item{type}{a string indicating the type of filter to
    use.
    \itemize{
      \item \code{"blackman-harris"} yields a modified raised-cosine
      filter designated as  "4-Term (-92 dB) Blackman-Harris" by Harris
      (1978; coefficients given in the table on page 65).  This is also
      called "minimum 4-sample Blackman Harris" by that author, in his
      Table 1, which lists figures of merit as follows: highest side
      lobe level -92dB; side lobe fall off -6 db/octave; coherent gain
      0.36; equivalent noise bandwidth 2.00 bins; 3.0-dB bandwidth 1.90
      bins; scallop loss 0.83 dB; worst case process loss 3.85 dB;
      6.0-db bandwidth 2.72 bins; overlap correlation 46 percent for
      75\% overlap and 3.8 for 50\% overlap.  Note that the equivalent
      noise bandwidth is the width of a spectral peak, so that a value of
      2 indicates a cutoff frequency of \code{1/m}, where \code{m} is as
      given below.
      \item \code{"rectangular"} for a flat filter.  (This is just for
      convenience; \code{\link{kernel}("daniell",....)} would give the
      same, in kernel form.)
      \code{"hamming"} for a Hamming filter (a raised-cosine that does
      not taper to zero at the ends)
      \item \code{"hann"} (a raised cosine that tapers to zero at the
      ends).
    }
    See Harris (1978) for a comparison of these and other filters.
    (This paper recommends for the Blackman-Harris filter.)
  }
  \item{m}{length of filter.  This should be an odd number, for any
    non-rectangular filter.}
  \item{asKernel}{boolean, set to \code{TRUE} to get a smoothing kernel
    for the return value.}
}

\details{The filter is suitable for use by \code{\link{filter}},
  \code{\link{convolve}} or (for the \code{asKernal=TRUE} case)
  with \code{\link{kernapply}}.  Note that \code{\link{convolve}} should
  be faster than \code{\link{filter}}, but it cannot be used if the time
  series has missing values.  For the Blackman-Harris filter,
  the half-power frequency is at \code{1/m} cycles per time unit,
  as shown in the \dQuote{Examples} section.  When using
  \code{\link{filter}} or \code{\link{kernapply}} with these filters,
  use \code{circular=TRUE}.}

\value{If \code{asKernel} is \code{FALSE}, this returns a list of filter
  coefficients, symmetric about the midpoint and summing to 1.  These
  may be used with \code{\link{filter}}, which should be provided with
  argument \code{circular=TRUE} to avoid phase offsets.  If
  \code{asKernel} is \code{TRUE}, the return value is a smoothing
  kernel, which can be applied to a timeseries with
  \code{\link{kernapply}}, whose bandwidth can be determined with
  \code{\link{bandwidth.kernel}}, and which has both print and plot
  methods.}

\examples{
library(oce)
x <- c(rep(0,30),rep(1,30),rep(0,30))
plot.ts(x)
x1 <- filter(x, make.filter("blackman-harris", 5))
lines(x1, col='red')
x2 <- filter(x, make.filter("blackman-harris", 11))
lines(x2, col='blue')
legend("topright", lwd=1, col=c("red", "blue"), legend=c("m=5", "m=11"))

# Power response
fr <- function(coef)
{
    n <- 1000
    omega <- seq(1e-5, pi, length.out=n)
    if (class(coef) == "tskernel")
        coef <- coef[-coef$m:coef$m]
    res <- rep(coef[1], n)
    ncoef <- length(coef)
    ejw <- exp(-1i * omega)
    ejw0 <- ejw
    for (i in 2:ncoef) {
        res <- res + coef[i] * ejw
        ejw <- ejw * ejw0
    }
    res <- list(f=omega/(2*pi), p=Mod(res)^2)
    class(res) <- "fr"
    res
}
plot.fr <- function(x, ...)
{
    plot(log10(x$f), 10*log10(x$p), xlab=expression(log[10](frequency)), ylab="dB", type='l', ...)
    abline(h=1, lty='dotted', col='lightgray')
}

# Compare Blackman-Harris with Daniell (with no attempt to match)
m <- 61                         # 1-minute half-power for sampling at 1Hz
par(mfcol=c(2,2))
# Left column: frequency response
par(mgp=getOption("oce.mgp"))
par(mgp=getOption("oce.mgp"))
par(mar=c(3,3,2,1))
bh <- make.filter(type="blackman-harris", m=m, asKernel=TRUE)
plot(fr(bh))
mtext(attr(bh,"name"), side=3, adj=0)
d <- kernel("daniell", c(9,3))
plot(fr(d))
mtext(attr(d,"name"), side=3, adj=0)
# Right column: coefficients and bandwidth
par(mgp=getOption("oce.mgp"))
par(mar=c(3,3,2,1))
plot(bh)
abline(v=bandwidth.kernel(bh)*c(-1,1), col="red", lwd=2)
plot(d)
abline(v=bandwidth.kernel(d)*c(-1,1), col="red", lwd=2)

## Test filter response, fr(), against spectal ratio
par(mfrow=c(2,1))
plot(fr(bh), xlim=c(-6,0), lwd=2)
mtext("Blackman-Harris calculated response", side=3, adj=0)
x <- rnorm(5e4)
y <- kernapply(x, bh, circular=TRUE)
sx <- spectrum(x, spans=c(11,5,3), plot=FALSE)
sy <- spectrum(y, spans=c(11,5,3), plot=FALSE)
plot(sx$freq, 10*log10(sy$spec/sx$spec), type='l', log="x", xlim=c(1e-6,1),
     xlab="Frequency", ylab="dB", lwd=2)
mtext("Blackman-Harris response from spectral ratio random time-series", side=3, adj=0)

## Near-cutoff behaviour
par(mfrow=c(1,1))
plot(fr(bh), xlim=log10(1/m*sqrt(c(0.5, 2))), ylim=c(-6,0), lwd=2)
abline(h=-3, col="red", lty="dotted") # half power
abline(v=log10(1/m), col="red", lty="dotted")
mtext(paste("Nominal cutoff\n1/", m, "Hz", sep=""), side=3, at=log10(1/m), col="red")
mtext("Half power", side=4, at=-3, col="red")
mtext(attr(bh,"name"), side=3, adj=0)
abline(-39, -6*log2(10), lwd=1, col="blue") # 6db/octave (Harris Table 1)
legend("topright", col=c("black", "blue"), lwd=c(2,1), legend=c("Response", "6db/octave"))
}

\references{F. J. Harris, 1978.  On the use of windows for harmonic
  analysis with the discrete Fourier Transform.
  \emph{Proceedings of the IEEE}, 66(1), 51-83
  (\url{http://web.mit.edu/xiphmont/Public/windows.pdf}.)}

\author{Dan Kelley}

\keyword{misc}
