\name{make.filter}
\alias{make.filter}
\title{Make a digital filter}
\description{make a digital filter}
\usage{make.filter(type=c("blackman-harris","rectangular", "hamming", "hann"),
            m, asKernel=FALSE)}
\arguments{
  \item{type}{a string indicating the type of filter to
    use.    (See Harris (1978) for a comparison of these and similar
    filters.)
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
opar <- par(no.readonly = TRUE)
x <- c(rep(0,30),rep(1,30),rep(0,30))
plot.ts(x)
x1 <- stats::filter(x, make.filter("blackman-harris", 5))
lines(x1, col='red')
x2 <- stats::filter(x, make.filter("blackman-harris", 11))
lines(x2, col='blue')
legend("topright", lwd=1, col=c("red", "blue"), legend=c("m=5", "m=11"))

# Power response
fr <- function(coef, a, b)
{
    postpad <- function (x, n)
    {
        nx <- length(x)
        if (n > nx)
            c(x, rep(0, n - nx))
        else x[1:n]
    }
    n <- 10000
    omega <- seq(1e-5, pi, length.out=n)
    if (missing(coef)) {
        if (missing(a) || missing(b))
            stop("must give a and b, if coef not given")
        p <- Mod(fft(postpad(b, 2*n))[1:n] / fft(postpad(a, 2*n))[1:n])^2
        pp <- p
    } else {
        if (class(coef) == "tskernel")
            coef <- coef[(-coef$m):coef$m]
        p <- Mod(fft(postpad(coef, 2*n))[1:n])^2 # check if need to double coef
    }
    res <- list(f=omega/(2*pi), p=p)
    class(res) <- "fr"
    res
}

plot.fr <- function(x, add=FALSE, xtype="log", ...)
{
    if (add) {
        lines(log10(x$f), 10*log10(x$p), ...)
    } else {
        if (xtype=="log")
            plot(log10(x$f), 10*log10(x$p), xlab=expression(log[10](frequency)), ylab="dB", type='l', ...)
        else
            plot(x$f, 10*log10(x$p), xlab=expression(frequency), ylab="dB", type='l', ...)
        grid()
    }
}

make.butterworth <- function(n, W, type, sampling.frequency)
{
    butter <- butter(n=n, W=W/(0.5*sampling.frequency), type=type)
}


## 1. Compare Blackman-Harris with Daniell (with no attempt to match)
m <- 61                         # 1-minute half-power for sampling at 1Hz
par(mfcol=c(2,2))
# Left column: frequency response
par(mgp=getOption("oce.mgp"))
par(mgp=getOption("oce.mgp"))
par(mar=c(3,3,2,1))
bh <- make.filter(type="blackman-harris", m=m, asKernel=TRUE)
plot(fr(bh))
abline(v=log10(1/61),col='red') # design half-power frequency
abline(h=-3, col='red')         # half-power line
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

## 2. Verify fr() using direct spectral ratio
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

## 3. Demonstrate Blackman-Harris cutoff location and slope
par(mfrow=c(1,1))
plot(fr(bh), xlim=log10(1/m*sqrt(c(0.5, 2))), ylim=c(-6,0), lwd=2)
abline(h=-3, col="red", lty="dotted") # half power
abline(v=log10(1/m), col="red", lty="dotted")
mtext(paste("Nominal cutoff\n1/", m, "Hz", sep=""), side=3, at=log10(1/m), col="red")
mtext("Half power", side=4, at=-3, col="red")
mtext(attr(bh,"name"), side=3, adj=0)
abline(-39, -6*log2(10), lwd=1, col="blue") # 6db/octave (Harris Table 1)
legend("topright", col=c("black", "blue"), lwd=c(2,1), legend=c("Response", "6db/octave"))

## 4. Illustrate effect of Butterworth order on frequency response
par(mfrow=c(2,1))
# library(signal)
# ab <- make.butterworth(n=2, W=c(0.01,0.1), type="pass", sampling.frequency=1)
ab <- list(a=c(1,-3.1594633021,3.7926844229,-2.0825733172,0.45044543006),
           b=c(0.056448462261,0,-0.11289692452,0,0.056448462261))
par(mgp=getOption("oce.mgp"))
par(mar=c(3,3,1,1))
plot(fr(a=ab$a, b=ab$b), ylim=c(-20,0), xlim=c(-3, 0))
abline(v=log10(c(0.01, 0.1)), col='red', lty="dotted")
abline(h=c(0,-3), col='red', lty="dotted")
mtext("Butterworth, order=2", side=3, adj=0, cex=2/3)

# library(signal)
# ab <- make.butterworth(n=4, W=c(0.01,0.1), type="pass", sampling.frequency=1)
ab <- list(a=c( 1,          -6.3987950868, 18.056234988, -29.398420878, 30.245469352,
               -20.147981515, 8.4876967896, -2.0670139621, 0.22281157292),
           b=c(0.0033628151287, 0, -0.013451260515, -6.2896966359e-18,
               0.020176890772, 6.2896966359e-18, -0.013451260515, 0, 0.0033628151287))
par(mgp=getOption("oce.mgp"))
par(mar=c(3,3,1,1))
plot(fr(a=ab$a, b=ab$b), ylim=c(-20,0), xlim=c(-3, 0))
abline(v=log10(c(0.01, 0.1)), col='red', lty="dotted")
abline(h=c(0,-3), col='red', lty="dotted")
mtext("Butterworth, order=4", side=3, adj=0, cex=2/3)

## Compare Blackman-Harris with Butterworth
par(mfrow=c(1,1))
par(mgp=getOption("oce.mgp"))
par(mar=c(3,3,1,1))
bh <- make.filter(type="blackman-harris", m=61)
plot(fr(bh), xlim=c(-3,-0.5), ylim=c(-30, 0))
abline(v=log10(1/60), col="green")
# library(signal)
# ab <- make.butterworth(n=2, W=0.016667, type="low", sampling.frequency=1)
ab <- list(a = c(1.0             , -1.8521435375    , 0.862346071863),
           b = c(0.00255063358957,  0.00510126717914, 0.00255063358957))
plot(fr(a=ab$a, b=ab$b), add=TRUE, col="red")
# library(signal)
# ab <- make.butterworth(n=4, W=0.016668, type="low", sampling.frequency=1)
ab <- list(a = c(1.0, -3.72640902937, 5.21603324655, -3.25000460109, 0.760485648747),
           b = c(6.57905243486e-06, 2.63162097395e-05, 3.94743146092e-05, 2.63162097395e-05, 6.57905243486e-06))
plot(fr(a=ab$a, b=ab$b), add=TRUE, col="blue")
legend("topright", lwd=1, col=c("black", "red", "blue", "green"),
       legend=c("BH", "butter 2", "butter 4", "cutoff"))
mtext("NOTE: this is for ONE pass of butterworth", side=3, adj=0)
par(opar)
}


\section{Butterworth filters}{

Matlab:

\preformatted{
  >> [b,a]=butter(2,0.5)

  b =

  0.2929    0.5858    0.2929


  a =

  1.0000   -0.0000    0.1716

  >> [b,a]=butter(2,0.25)

  b =

  0.0976    0.1953    0.0976


  a =

  1.0000   -0.9428    0.3333

  >>
  
}

R package \code{signal}

\preformatted{
  library(signal)
  > butter(2, 0.5)
  $b
  [1] 0.2928932 0.5857864 0.2928932

  $a
  [1]  1.000000e+00 -1.625884e-16  1.715729e-01

  attr(,"class")
  [1] "Arma"
  > butter(2, 0.25)
  $b
  [1] 0.09763107 0.19526215 0.09763107

  $a
  [1]  1.0000000 -0.9428090  0.3333333

  attr(,"class")
  [1] "Arma"
}

}

\references{F. J. Harris, 1978.  On the use of windows for harmonic
  analysis with the discrete Fourier Transform.
  \emph{Proceedings of the IEEE}, 66(1), 51-83
  (\url{http://web.mit.edu/xiphmont/Public/windows.pdf}.)}

\author{Dan Kelley}

\keyword{misc}
