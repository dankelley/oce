# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Welch Periodogram
#'
#' Compute periodogram using the Welch (1967) method. This function is somewhat
#' analogous to the Matlab function of the same name, but it is *not* intended
#' as a drop-in replacement. Please see the \sQuote{Details} and the description
#' of the parameters, to learn about the complex interactions of the controlling
#' parameters.
#'
#' The basic gist of the action is not too difficult to explain: `x` is broken
#' up into subdivisions, spectral analysis is done on each, and the results are
#' averaged to get a return value.
#'
#' However, things get complicated in practice. This is because there are
#' several interlocking parameters that control both the subdivision stage and
#' the spectral stage. The goal is to at least roughly mimic the Matlab
#' function, so the parameter names and how they are interpreted are dictated to
#' some extent by how things work in the Matlab code.
#'
#' The parameters `window`, `noverlap` and `nfft` control the subdivision
#' behaviour. The parameters `fs`, `spec`, `demean` and `detrend` control the
#' spectral-analysis behaviour. Users who find the documention on these things
#' to be confusing may want to examine the code to see what is actually being
#' done. If they see problems, they are asked to post issues on the oce github
#' website.
#'
#' @param x a vector or timeseries to be analyzed.  If `x` is a timeseries, then
#' it there is no need to `fs`, and doing so will result in an error if it does
#' not match the value inferred from `x`.
#'
#' @param window optional numeric vector specifying a window to be applied to
#' the timeseries subsamples.  If `spec` is provided, then it is an error to
#' provide `window`. This is because this function is set up to construct a
#' window automatically in this case. The window is of the Hamming variety, of
#' length given by `nfft`, if `nfft` is less than half the length of `x`, or of
#' length `x` otherwise. On the other hand, if `spec` is not provided but
#' `nfft`, then a similar window is constructed if `window` is not provided. If
#' neither `nfft` nor `window` is provided, `pwelch` will divide the timeseries
#' up into 8 subdivisions (equivalent, it will act as though `nfft` were set to
#' `length(x)/8`).
#'
#' @param noverlap number of points to overlap between windows.  If not
#' specified, this will be set to half the window length.
#'
#' @param nfft length of FFT. See `window` for how `nfft` interacts with that
#' argument.
#'
#' @param fs frequency of time-series.  If `x` is a time-series, and if `fs` is
#' supplied, then time-series is altered to have frequency `fs`.
#'
#' @param spec optional function to be used for the computation of the spectrum,
#' to allow finer-grained control of the processing. If provided, `spec` must
#' accept a time-series as its first argument, and must return a list containing
#' the spectrum in `spec` and the frequency in `freq`. Note that the `window`
#' parameter must not be supplied if `spec` and `nfft` are supplied, because in
#' that case a window is automatically constructed (see the documentation for
#' `window`). Note that the values of `demean`, `detrend` and `plot` are ignored
#' if `spec` is given. However, the \dots argument *is* passed to `spec`.
#' Frankly, users who wish to supply their own `spec` might be better
#' advised to just break up the time-series themselves, to gain full
#' control.
#'
#' @param demean,detrend logical values that can control the spectrum
#' calculation, in the default case of `spec`.  These are passed to [spectrum()]
#' and thence [spec.pgram()]; see the help pages for the latter for an
#' explanation.
#'
#' @param plot logical, set to `TRUE` to plot the spectrum.
#'
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, or to 2 to get more.
#'
#' @param \dots optional extra arguments to be passed to
#' [spectrum()], or to `spec`, if the latter is given.
#'
#' @return `pwelch` returns a list mimicking the return value from [spectrum()],
#' containing frequency `freq`, spectral power `spec`, degrees of freedom `df`,
#' bandwidth `bandwidth`, etc.
#'
#' @section Bugs:
#'
#' Both bandwidth and degrees of freedom are just copied from
#' the values for one of the chunk spectra, and are thus incorrect.  That means
#' the cross indicated on the graph is also incorrect.
#'
#' @section Historical notes:
#'
#' 1. *2021-06-26:* Until this date, [pwelch()] passed the subsampled timeseries
#'    portions through [detrend()] before applying the window. This practice was
#'    dropped because it could lead to over-estimates of low frequency energy
#'    (as noticed by Holger Foysi of the University of Siegen), perhaps because
#'    [detrend()] considers only endpoints and therefore can yield
#'    inaccurate trend estimates. In a related change, `demean` and `detrend`
#'    were added as formal arguments, to avoid users having to trace the
#'    documentation for [spectrum()] and then [spec.pgram()], to learn how to
#'    remove means and trends from data. For more control, the `spec` argument
#'    was added to let users sidestep [spectrum()] entirely, by providing their
#'    own spectral computation functions.

#' 2. *2025-07-04:* until this date, there was an error in supplying `nfft`
#'    together with `spec` (it is issue 2299 on the github website). This issued
#'    an error message that resulted from the fact that it was not permitted to
#'    supply `window` in that case. To address the problem, whilst retaining the
#'    requirement that `window` not be supplied, `pwelch()` was changed so that
#'    it constructs a window automatically. At this time, the documentation
#'    was rewritten in an attempt to provide more clarity.
#'
#' @references
#'
#' Welch, P. D., 1967. The Use of Fast Fourier Transform for the
#' Estimation of Power Spectra: A Method Based on Time Averaging Over Short,
#' Modified Periodograms. *IEEE Transactions on Audio Electroacoustics*,
#' AU-15, 70--73.
#'
#' @examples
#' library(oce)
#' Fs <- 1000
#' t <- seq(0, 0.296, 1 / Fs)
#' x <- cos(2 * pi * t * 200) + rnorm(n = length(t))
#' X <- ts(x, frequency = Fs)
#' s <- spectrum(X, spans = c(3, 2), main = "random + 200 Hz", log = "no")
#' w <- pwelch(X, plot = FALSE)
#' lines(w$freq, w$spec, col = "red")
#' w2 <- pwelch(X, nfft = 75, plot = FALSE)
#' lines(w2$freq, w2$spec, col = "green")
#' abline(v = 200, col = "blue", lty = "dotted")
#' cat("Checking spectral levels with Parseval's theorem:\n")
#' cat("var(x)                              = ", var(x), "\n")
#' cat("2 * sum(s$spec) * diff(s$freq[1:2]) = ", 2 * sum(s$spec) * diff(s$freq[1:2]), "\n")
#' cat("sum(w$spec) * diff(s$freq[1:2])     = ", sum(w$spec) * diff(w$freq[1:2]), "\n")
#' cat("sum(w2$spec) * diff(s$freq[1:2])    = ", sum(w2$spec) * diff(w2$freq[1:2]), "\n")
#' # co2
#' par(mar = c(3, 3, 2, 1), mgp = c(2, 0.7, 0))
#' s <- spectrum(co2, plot = FALSE)
#' plot(log10(s$freq), s$spec * s$freq,
#'     xlab = expression(log[10] * Frequency), ylab = "Power*Frequency", type = "l"
#' )
#' title("Variance-preserving spectrum")
#' pw <- pwelch(co2, nfft = 256, plot = FALSE)
#' lines(log10(pw$freq), pw$spec * pw$freq, col = "red")
#'
#' @author Dan Kelley
pwelch <- function(
    x, window, noverlap, nfft, fs, spec,
    demean = FALSE, detrend = TRUE,
    plot = TRUE, debug = getOption("oceDebug"), ...) {
    # http://octave.svn.sourceforge.net/viewvc/octave/trunk/octave-forge/main/signal/inst/pwelch.m
    # avoid having to pull in the signal library
    hamming.local <- function(n) {
        n <- round(n)
        if (n < 0) {
            stop("n must round to a positive integer")
        }
        if (n == 1) {
            c <- 1
        } else {
            n <- n - 1
            pi <- 4.0 * atan2(1.0, 1.0)
            c <- 0.54 - 0.46 * cos(2 * pi * (0:n) / n)
        }
    }
    # hanning.local <- function(n) # avoid having to pull in the signal library
    # {
    #     if (!(length(n) == 1 && (n == round(n)) && (n > 0)))
    #         stop("n must be a positive integer")
    #     if (n == 1)
    #         c <- 1
    #     else {
    #         pi <- 4 * atan2(1, 1)       # avoid problems if user redefined this
    #         n <- n - 1
    #         c <- 0.5 - 0.5 * cos(2 * pi * (0:n)/n)
    #     }
    #     c
    # }
    gave.window <- !missing(window)
    gave.nfft <- !missing(nfft)
    gave.noverlap <- !missing(noverlap)
    gave.spec <- !missing(spec)
    oceDebug(debug, "pwelch(x, ", argShow(window), argShow(nfft), argShow(noverlap), argShow(fs), "...) START\n", sep = "", unindent = 1)
    if (is.ts(x)) {
        if (missing(fs)) {
            fs <- frequency(x)
        } else if (fs != frequency(x)) {
            stop("fs, if provided, must equal frequency(x)")
        }
    }
    nx <- length(x)
    if (nx < 1) {
        stop("need more than one data point")
    }
    if (gave.spec) {
        if (!gave.nfft) {
            stop("give nfft if spec is given")
        }
        if (gave.window) {
            stop("window must not be given, if spec is given")
        }
        # User gave nfft, so we can use that
        if (nfft < 1) {
            stop("'nfft' must be a positive integer")
        }
        if (nfft > 0.5 * nx) {
            nfft <- nx
        }
        window <- hamming.local(nfft)
        oceDebug(debug, "spec and nfft both given; using hamming window of length ", nfft, "\n")
        # --END
    } else {
        if (gave.window) {
            if (gave.nfft && (length(window) != nfft)) {
                stop("if both 'window' and 'nfft' are given, then length(window) must equal nfft")
            }
            if (length(window) == 1) {
                window <- as.integer(window)
                if (window < 1L) {
                    stop("window must be a positive integer, if length(window)==1")
                }
                window <- hamming.local(floor(nx / window))
                oceDebug(debug, "window given, but spec not given,; using hamming window of length ", nx / window, "\n")
            } else if (!is.vector(window)) {
                stop("'window' must be a numeric vector")
            }
        } else {
            if (gave.nfft) {
                if (nfft < 1) {
                    stop("'nfft' must be a positive integer")
                }
                if (nfft > 0.5 * nx) {
                    nfft <- nx
                }
                window <- hamming.local(nfft)
                oceDebug(debug, "window given, but spec not given,; using hamming window of length ", nx / window, "\n")
            } else {
                # FIXME: should we use 'overlap' here?
                windowLength <- min(
                    nx,
                    if (gave.noverlap) {
                        floor(nx / 8)
                    } else {
                        floor(nx / 8 / 0.5)
                    }
                )
                window <- hamming.local(windowLength)
                oceDebug(debug, "window not given; using hamming window of length ", nx / windowLength, "\n")
            }
        }
    }
    normalization <- mean(window^2)
    window.len <- length(window)
    if (missing(noverlap)) {
        noverlap <- floor(window.len / 2)
    }
    step <- floor(window.len - noverlap + 1)
    oceDebug(debug, "using window.len=", window.len, "  step=", step, "  noverlap=", noverlap, "  nx=", nx, ", normalization=", normalization, "\n", sep = "")
    if (step < 1) {
        stop("overlap cannot exceed segment length")
    }
    # i0 <- 1
    # nwindows <- floor(nx / window.len)
    psd <- NULL
    nrow <- 0
    start <- 1
    if (gave.spec) {
        end <- nfft
        while (TRUE) {
            oceDebug(debug, "  calc. subspectrum w/ user's spec, at indices ", start, ":", end, "\n")
            ## 2299 xx <- ts(x[start:end], frequency = fs)
            xx <- ts(window * x[start:end], frequency = fs)
            s <- spec(xx, ...) # note the ...
            if (nrow == 0) {
                freq <- s$freq
            }
            psd <- c(psd, s$spec)
            start <- start + step
            end <- end + step
            nrow <- nrow + 1
            if (end > nx) {
                break
            }
        }
    } else {
        end <- window.len
        args <- list(...)
        names.args <- names(args)
        if (!("taper" %in% names.args)) {
            args$taper <- 0
        }
        args$plot <- plot
        args$demean <- demean
        args$detrend <- detrend
        while (TRUE) {
            oceDebug(debug, "  calc. subspectrum w/ spectrum(), at indices ", start, ":", end, "\n")
            xx <- ts(window * x[start:end], frequency = fs)
            args$x <- xx # before issue 242, wrapped RHS in as.vector()
            s <- do.call(spectrum, args = args)
            if (nrow == 0) {
                freq <- s$freq
            }
            psd <- c(psd, s$spec)
            start <- start + step
            end <- end + step
            nrow <- nrow + 1
            if (end > nx) {
                break
            }
        }
    }
    nrow <- max(1, nrow)
    psd <- matrix(psd, nrow = nrow, byrow = TRUE) / normalization
    oceDebug(debug, "spectrum averaged across ", paste(dim(psd), collapse = "x"), " matrix\n")
    res <- list(
        freq = freq, spec = apply(psd, 2, mean),
        method = "Welch", series = deparse(substitute(expr = x, env = environment())),
        df = s$df * (nx / length(window)),
        bandwidth = s$bandwidth, # FIXME: wrong formulae
        demean = FALSE, detrend = TRUE
    )
    class(res) <- "spec"
    if (plot) {
        plot(res, ...)
        oceDebug(debug, "END pwelch()\n", unindent = 1, sep = "")
        return(invisible(res))
    } else {
        oceDebug(debug, "END pwelch()\n", unindent = 1, sep = "")
        return(res)
    }
}
