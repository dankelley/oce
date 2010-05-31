\name{make.filter}
\alias{make.filter}
\title{Make a digital filter}
\description{make a digital filter}
\usage{make.filter(type=c("blackman-harris","rectangular", "hamming", "hann"),
            m, asKernel=FALSE)}
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
  \item{asKernel}{boolean, set to \code{TRUE} to get a smoothing kernel
    for the return value.}
}

\details{The filter is suitable for use by \code{\link{filter}} or
  \code{\link{convolve}}.  Note that \code{\link{convolve}} should be
  faster than \code{\link{filter}}, but it cannot be used if the time
  series has missing values.  For the Blackman-Harris filter,
  the half-power frequency is possibly \code{1/m} cycles per time unit 
  (see \dQuote{Examples}).  When using \code{\link{filter}} with these 
  filters, use \code{circular=TRUE}.}

\value{If \code{asKernel} is \code{FALSE}, this returns a list of filter
  coefficients, symmetric about the midpoint and summing to 1.  These
  may be used with \code{\link{filter}}, which should be provided with
  argument \code{circular=TRUE} to avoid phase offsets.  If
  \code{asKernel} is \code{TRUE}, the return value is a smoothing
  kernel, which can be applied to a timeseries with
  \code{\link{kernapply}}, whose bandwidth can be determined with
  \code{\link{bandwidth.kernel}}, and which has both print and plot methods.}

\examples{
library(oce)
x <- c(rep(0,30),rep(1,30),rep(0,30))
plot.ts(x)
x1 <- filter(x, make.filter("blackman-harris", 5))
lines(x1, col='red')
x2 <- filter(x, make.filter("blackman-harris", 11))
lines(x2, col='blue')
legend("topright", lwd=1, col=c("red", "blue"), legend=c("m=5", "m=11"))

# Frequency response
fr <- function(coef)
{
    n <- 200
    omega <- seq(pi/1e4, pi, length.out=n)
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
    res <- list(f=omega/pi/2, H=Mod(res))
    class(res) <- "fr"
    res
}
plot.fr <- function(x, log="xy", ...)
{
    plot(x$f, x$H, xlab="Frequency", ylab="Response", type='l', log=log, ...)
    abline(h=1, lty='dotted', col='lightgray')
}
par(mfrow=c(2,2))
par(mgp=getOption("oce.mgp"))
par(mar=c(3,3,2,1))
bh <- make.filter(m=41, asKernel=TRUE)
plot(fr(bh), log="")
mtext("Blackman-Harris m=51")
plot(fr(bh))
mtext("Blackman-Harris m=41")
d <- kernel('daniell', c(9,3))
plot(fr(d), log="")
mtext("Daniell m=c(9,3)")
plot(fr(d))
mtext("Daniell m=c(9,3)")

# Bandwidth
par(mfrow=c(2,1))
par(mgp=getOption("oce.mgp"))
par(mar=c(3,3,2,1))
plot(bh)
abline(v=bandwidth.kernel(bh)*c(-1,1), col="red", lwd=2)
plot(d)
abline(v=bandwidth.kernel(bh)*c(-1,1), col="red", lwd=2)

## If sampling at 1Hz, this filter has a 50-percent gain at 60s.
par(mfrow=c(1,1))
plot(fr(make.filter(m=79, asKernel=TRUE)))
abline(h=c(1/2, 1),col=c('red','blue'))
abline(v=c(1/60), col='red')
mtext("Half-amplitude\n60 Hz", side=3, at=1/60, col='red')
abline(v=1/2)
mtext("Nyquist\n0.5 Hz", side=3, at=1/2)
mtext("Blackman-Harris, m=79", side=3, adj=0)
}

\references{F. J. Harris, 1978.  On the use of windows for harmonic
  analysis with the discrete Fourier Transform.
  \emph{Proceedings of the IEEE}, 66(1), 51-83
  (\url{http://web.mit.edu/xiphmont/Public/windows.pdf}.)}

\author{Dan Kelley}

\keyword{misc}
