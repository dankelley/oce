\name{pwelch}

\alias{pwelch}

\title{Welch periodogram}

\description{Compute periodogram using the Welch (1967) method}

\usage{pwelch(x, window, noverlap, nfft, fs, spectrumtype, esttype,
     debug=getOption("oce.debug"), \dots)}

\arguments{
  \item{x}{a vector or timeseries to be analyzed.  If a timeseries,
    then there is no need to specify \code{fs}.}
  \item{window}{window specification, either a single value giving the
    number of windows to use, or a vector of values, specifying the
    window coefficients.  If not specified, then 8 windows are used,
    each with a Hanning (half-cosine) window.
    BUG: in the present version of the code, the only
    working possibility is to provide a single value, or no value.}
  \item{noverlap}{number of points to overlap between windows.  If not
    specified, this will be set to half the window length.}
  \item{nfft}{length of FFT.  This cannot be given if \code{window} is
    given, and the latter is a single integer.}
  \item{fs}{frequency of time-series.  If \code{x} is a time-series,
    and if \code{fs} is supplied, then time-series is altered to have
    frequency \code{fs}.}
  \item{spectrumtype}{not used (yet)}
  \item{esttype}{not used (yet)}
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a
    moderate amount of debugging information, or to 2 to get more.}
  \item{\dots}{optional extra arguments to be passed to
    \code{\link{spectrum}}. Unless specified in this list,
    \code{\link{spectrum}} is called with \code{plot=FALSE} to prevent
    plotting the separate spectra, and with \code{taper=0}, 
    which is not needed with the default Hanning window.  However, the
    other defaults of \code{\link{spectrum}} are used,
    e.g. \code{detrend=TRUE}.}
}
 
\details{The Welch (1967) method is used.}

\examples{
library(oce)
Fs <- 1000
t <- seq(0, 0.296, 1/Fs)
x <- cos(2 * pi * t * 200) + rnorm(n=length(t))
xts <- ts(x, frequency=Fs)
s <- spectrum(xts, spans=c(3,2), main="random + 200 Hz", log='no')
w <- pwelch(xts)
lines(w$freq, w$spec, col="red")
w2 <- pwelch(xts, nfft=75)
lines(w2$freq, w2$spec, col='green')
abline(v=200, col="blue", lty="dotted")
cat("var(x)", sum((x-mean(x))^2), "\n")
cat("sum(s$spec)", sum(s$spec), "\n")
cat("sum(w$spec)", sum(w$spec), "\n")
cat("sum(w2$spec)", sum(w2$spec), "\n")
}

\references{
Welch, P. D., 1967.
The Use of Fast Fourier Transform for the Estimation of Power
Spectra: A Method Based on Time Averaging Over Short, Modified
Periodograms.
\emph{IEEE Transactions on Audio Electroacoustics},
AU-15,
70--73.
}

\author{Dan Kelley}

\keyword{misc}
