#' Welch periodogram
#'
#' Compute periodogram using the Welch (1967) method.
#' First, `x` is broken up into chunks,
#' overlapping as specified by `noverlap`.  These chunks are then
#' detrended with [detrend()], multiplied by the window, and then
#' passed to [spectrum()].  The resulting spectra are then averaged,
#' with the results being stored in `spec` of the return value.  Other
#' entries of the return value mimic those returned by [spectrum()].
#'
#' @param x a vector or timeseries to be analyzed.  If a timeseries, then there
#' is no need to specify `fs`.
#'
#' @param window window specification, either a single value giving the number
#' of windows to use, or a vector of window coefficients.  If not specified,
#' then 8 windows are used, each with a Hamming (raised half-cosine) window.
#'
#' @param noverlap number of points to overlap between windows.  If not
#' specified, this will be set to half the window length.
#'
#' @param nfft length of FFT.  This cannot be given if `window` is given,
#' and the latter is a single integer.
#'
#' @param fs frequency of time-series.  If `x` is a time-series, and if
#' `fs` is supplied, then time-series is altered to have frequency
#' `fs`.
#'
#' @param spectrumtype not used (yet)
#'
#' @param esttype not used (yet)
#'
#' @param plot logical, set to `TRUE` to plot the spectrum.
#'
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, or to 2 to get more.
#'
#' @param \dots optional extra arguments to be passed to
#' [spectrum()]. Unless specified in this list,
#' [spectrum()] is called with `plot=FALSE` to prevent plotting
#' the separate spectra, and with `taper=0`, which is not needed with the
#' default Hanning window.  However, the other defaults of
#' [spectrum()] are used, e.g. `detrend=TRUE`.
#'
#' @return List mimicking the return value from [spectrum()],
#' containing frequency `freq`, spectral power `spec`, degrees of
#' freedom `df`, bandwidth `bandwidth`, etc.
#' @section Bugs: Both bandwidth and degrees of freedom are just copied from
#' the values for one of the chunk spectra, and are thus incorrect.  That means
#' the cross indicated on the graph is also incorrect.
#'
#' @author Dan Kelley
#'
#' @references Welch, P. D., 1967. The Use of Fast Fourier Transform for the
#' Estimation of Power Spectra: A Method Based on Time Averaging Over Short,
#' Modified Periodograms. \emph{IEEE Transactions on Audio Electroacoustics},
#' AU-15, 70--73.
#'
#' @examples
#' library(oce)
#' Fs <- 1000
#' t <- seq(0, 0.296, 1/Fs)
#' x <- cos(2 * pi * t * 200) + rnorm(n=length(t))
#' xts <- ts(x, frequency=Fs)
#' s <- spectrum(xts, spans=c(3,2), main="random + 200 Hz", log='no')
#' w <- pwelch(xts, plot=FALSE)
#' lines(w$freq, w$spec, col="red")
#' w2 <- pwelch(xts, nfft=75, plot=FALSE)
#' lines(w2$freq, w2$spec, col='green')
#' abline(v=200, col="blue", lty="dotted")
#' cat("Checking spectral levels with Parseval's theorem:\n")
#' cat("var(x)                              = ", var(x), "\n")
#' cat("2 * sum(s$spec) * diff(s$freq[1:2]) = ", 2 * sum(s$spec) * diff(s$freq[1:2]), "\n")
#' cat("sum(w$spec) * diff(s$freq[1:2])     = ", sum(w$spec) * diff(w$freq[1:2]), "\n")
#' cat("sum(w2$spec) * diff(s$freq[1:2])    = ", sum(w2$spec) * diff(w2$freq[1:2]), "\n")
#' ## co2
#' par(mar=c(3,3,2,1), mgp=c(2,0.7,0))
#' s <- spectrum(co2, plot=FALSE)
#' plot(log10(s$freq), s$spec * s$freq,
#'      xlab=expression(log[10]*Frequency), ylab="Power*Frequency", type='l')
#' title("Variance-preserving spectrum")
#' pw <- pwelch(co2, nfft=256, plot=FALSE)
#' lines(log10(pw$freq), pw$spec * pw$freq, col='red')
#'
#' @md
pwelch <- function(x, window, noverlap, nfft, fs, spectrumtype, esttype,
                   plot=TRUE,
                   debug=getOption("oceDebug"), ...)
{
    ##http://octave.svn.sourceforge.net/viewvc/octave/trunk/octave-forge/main/signal/inst/pwelch.m

    hamming.local <- function (n) # avoid having to pull in the signal library
    {
        n <- round(n)
        if (n < 0)
            stop("n must round to a positive integer")
        if (n == 1)
            c <- 1
        else {
            n <- n - 1
            pi <- 4 * atan2(1, 1) # avoid problems if user redefined this
            c <- 0.54 - 0.46 * cos(2 * pi * (0:n)/n)
        }
        c
    }
    ## hanning.local <- function(n) # avoid having to pull in the signal library
    ## {
    ##     if (!(length(n) == 1 && (n == round(n)) && (n > 0)))
    ##         stop("n must be a positive integer")
    ##     if (n == 1)
    ##         c <- 1
    ##     else {
    ##         pi <- 4 * atan2(1, 1)       # avoid problems if user redefined this
    ##         n <- n - 1
    ##         c <- 0.5 - 0.5 * cos(2 * pi * (0:n)/n)
    ##     }
    ##     c
    ## }
    gave.window <- !missing(window)
    gave.nfft <- !missing(nfft)
    gave.fs <- !missing(fs)
    gave.noverlap <- !missing(noverlap)
    oceDebug(debug, sprintf("pwelch(x, window=%s, nfft=%s, fs=%s, noverlap=%s, ...) {\n",
                             if (gave.window) window else "(not given)",
                             if (gave.nfft) nfft else "(not given)",
                             if (gave.noverlap) noverlap else "(not given)",
                             if (gave.fs) fs else "(not given)"), unindent=1)
    if (is.ts(x)) {
        if (missing(fs))
            fs <- frequency(x)
        else {
            if (fs != frequency(x)) {
                warning("fs does not match frequency(x); using the former")
                x <- ts(x, frequency=fs)
            }
        }
    }
    x.len <- length(x)
    if (x.len < 1)
        stop("need more than one data point")
    if (!missing(spectrumtype))
        warning("'spectrumtype' is ignored at present")
    if (!missing(esttype))
        warning("'esttype' is ignored at present")
    if (gave.window) {
        if (gave.nfft && (length(window) != nfft))
            stop("if both 'window' and 'nfft' are given, then length(window) must equal nfft")
        if (length(window) == 1) {
            window <- hamming.local(floor(x.len / window))
        } else if (!is.vector(window)) {
            stop("for now, 'window' may only be a list of numbers, or a single number")
        }
    } else {
        if (gave.nfft) {
            if (nfft < 1)
                stop("'nfft' must be a positive integer")
            if (nfft > 0.5 * x.len)
                nfft <- x.len
            window <- hamming.local(nfft)
        } else {
            if (gave.noverlap) {
                windowLength <- min(x.len, floor(x.len / 8))
            } else {
                windowLength <- min(x.len, floor(x.len / 8 / 0.5))
            }
            window <- hamming.local(windowLength)
        }
    }
    normalization <- mean(window^2)
    window.len <- length(window)
    if (missing(noverlap)) {
        noverlap <- floor(window.len / 2)
    }
    step <- floor(window.len - noverlap + 1)
    oceDebug(debug, "window.len=", window.len, "  step=", step, "  noverlap=", noverlap, "  x.len=", x.len, "\n")
    if (step < 1)
        stop("overlap cannot exceed segment length")
    ## i0 <- 1
    ## nwindows <- floor(x.len / window.len)
    psd <- NULL
    nrow <- 0
    start <- 1
    end <- window.len
    args <- list(...)
    names.args <- names(args)
    if (!("taper" %in% names.args))
        args$taper <- 0
    if (!("plot" %in% names.args))
        args$plot <- FALSE
    if (!("demean" %in% names.args))
        args$demean <- TRUE
    if (!("detrend" %in% names.args))
        args$detrend <- TRUE
    while (TRUE) {
        oceDebug(debug, "  calculating subspectrum at indices ", start, "to", end, "\n")
        xx <- ts(window * detrend(x[start:end])$Y, frequency=fs)
        args$x <- xx                   # before issue 242, wrapped RHS in as.vector()
        s <- do.call(spectrum, args=args)
        if (nrow == 0)
            freq <- s$freq
        psd <- c(psd, s$spec)
        start <- start + step
        end <- end + step
        nrow <- nrow + 1
        if (end > x.len)
            break
    }
    nrow <- max(1, nrow)
    psd <- matrix(psd, nrow=nrow, byrow=TRUE) / normalization
    oceDebug(debug, "resultant spectrum is average across matrix of dimension", dim(psd), "\n")
    oceDebug(debug, "} # pwelch()\n", unindent=1)
    res <- list(freq=freq, spec=apply(psd, 2, mean),
                method="Welch", series=deparse(substitute(x)),
                df=s$df * (x.len / length(window)),
                bandwidth=s$bandwidth, # FIXME: wrong formulae
                demean=FALSE, detrend=TRUE)
    class(res) <- "spec"
    if (plot) {
        plot(res, ...)
        return(invisible(res))
    } else return(res)
}
