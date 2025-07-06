# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Welch Periodogram
#'
#' Compute periodogram using the Welch (1967) method. This function is somewhat
#' analogous to the Matlab function of the same name, but it is *not* intended
#' as a drop-in replacement. Please see the \sQuote{Arguments} and
#' \sQuote{Details} to learn about the complex interactions of the controlling
#' parameters.
#'
#' The gist of the [pwelch()] behaviour is not too difficult to explain: `x` is
#' broken up into subdivisions, spectral analysis is done on each, and the
#' results are averaged to get a return value.
#'
#' However, things get complicated in practice. This is because there are
#' several interlocking parameters that control both the subdivision stage and
#' the spectral stage. The goal is to at least roughly mimic the Matlab
#' function, so the parameter names and how they are interpreted are dictated to
#' some extent by how things work in the Matlab code.
#'
#' The parameters `window`, `noverlap` and `nfft` control the subdivision
#' behaviour. The parameters `fs`, `spec`, `demean` and `detrend` control the
#' spectral-analysis behaviour. Users who find the documentation on these things
#' to be confusing may want to examine the code to see what is actually being
#' done. If they see problems, they are asked to post issues on the oce github
#' website.
#'
#' @param x a vector or timeseries to be analyzed.  If `x` is a timeseries, then
#' it there is no need to `fs`, and doing so will result in an error if it does
#' not match the value inferred from `x`.
#'
#' @param window optional value that can have several meanings. CASE 1: If
#' `window` is a single integer, then that is taken as the number of fragments
#' into which `x` is subdivided. In this case, a Hamming window, of length
#' `length(x)/window`, is constructed using [makeFilter()] with its `normalize`
#' and `asKernel` arguments both set to FALSE.  This filter is then multiplied
#' elementwise with the `x` values in the subdivision. CASE 2: if `window` is a
#' numeric vector of length exceeding 1, then the values are taken to be a
#' filter to be applied to the subset of `x`, and thus the length of `window`
#' and the value of `nfft` must be equal, if both are supplied.
#'
#' @param noverlap number of points to overlap between the subdivisions
#' of `x`. If this is not provided, a value equal to half the subset
#' length will be used.
#'
#' @param nfft length of the FFT, i.e. length of the desired subsets of `x`.
#' This argument works together with the `window` argument; see the documentation
#' on the latter to learn more.
#'
#' @param fs numeric value indicating the sampling frequency for `x`.  If
#' `x` is already a time-series object, then `fs` must match its frequency,
#' or an error is reported.
#'
#' @param spec optional function to be used, in conjunction with `nfft`, to
#' control the computation of the spectra in the subdivided time-series. The
#' purpose is to allow fine-grained control of the processing, mainly for use by
#' experts. If provided, `spec` must accept a time-series as its first argument,
#' along with optional other arguments that are passed through as the `...`
#' argument. The return value from `spec` must be a list or data frame
#' containing the spectrum in an element named `spec` and the frequency in an
#' element named `freq`. Note that an error will be reported if `window` is
#' provided in addition to `spec` and `nfft`. This is because [pwelch()]
#' automatically constructs a (Hamming) window and multiplies it into each
#' subset of `x`. Also, note that the values of `demean` and `detrend` are
#' ignored if `spec` is provided; it's up to the user to decide on these things
#' and to handle them within `spec()`.
#'
#' @param demean,detrend logical values that can control the spectrum
#' calculation, but only if `spec` is not provided. These are passed to
#' [spectrum()] and thence to [spec.pgram()]; see the help pages for the latter
#' for an explanation.
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
#'
#' 2. *2025-07-04:* until this date, there was an error in supplying `nfft`
#'    together with `spec` (it is issue 2299 on the github website). This issued
#'    an error message that resulted from the fact that it was not permitted to
#'    supply `window` in that case. To address the problem, whilst retaining the
#'    requirement that `window` not be supplied, `pwelch()` was changed so that
#'    it constructs a window automatically.  (In the future, [pwelch()] may be
#'    modified to accept `window=FALSE`, in which case no windowing will be done
#'    here, leaving it up to the user to decide whether to do windowing in the
#'    user-supplied `spec()` function.)
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
#' s <- spectrum(X, spans = c(3, 2), log = "no", plot = FALSE)
#' plot(s$freq, s$spec, type = "l", xlab = "Frequency", ylab = "Spectrum")
#' w <- pwelch(X, plot = FALSE)
#' lines(w$freq, w$spec, col = 2)
#' abline(v = 200, col = "lightgray")
#' legend("topright", bg = "white", lwd = 1, col = 1:2, legend = c("spectrum()", "pwelch()"))
#'
#' @author Dan Kelley and Clark Richards
pwelch <- function(
    x, window, noverlap, nfft, fs, spec,
    demean = FALSE, detrend = TRUE,
    plot = TRUE, debug = getOption("oceDebug"), ...) {
    # http://octave.svn.sourceforge.net/viewvc/octave/trunk/octave-forge/main/signal/inst/pwelch.m
    # avoid having to pull in the signal library
    # 2025-07-05 hamming.local <- function(n) {
    # 2025-07-05     n <- round(n)
    # 2025-07-05     if (n < 0) {
    # 2025-07-05         stop("n must round to a positive integer")
    # 2025-07-05     }
    # 2025-07-05     if (n == 1) {
    # 2025-07-05         1
    # 2025-07-05     } else {
    # 2025-07-05         n <- n - 1
    # 2025-07-05         pi <- 4.0 * atan2(1.0, 1.0)
    # 2025-07-05         0.54 - 0.46 * cos(2 * pi * (0:n) / n)
    # 2025-07-05     }
    # 2025-07-05 }
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
            stop("must provide nfft if spec is provided")
        }
        if (gave.window) {
            stop("window must not be provided, if spec is provided")
        }
        # User gave nfft, so we can use that
        if (nfft < 1) {
            stop("'nfft' must be a positive integer")
        }
        if (nfft > 0.5 * nx) {
            nfft <- nx
        }
        # 2025-07-05 window <- hamming.local(nfft)
        window <- makeFilter("hamming", nfft, normalize = FALSE, asKernel = FALSE)
        oceDebug(debug, "spec and nfft both provided; using hamming window of length ", nfft, "\n")
        # --END
    } else {
        # did not give spec
        if (gave.window) {
            if (gave.nfft && (length(window) != nfft)) {
                stop("if both 'window' and 'nfft' are provided, then length(window) must equal nfft")
            }
            if (length(window) == 1) {
                window <- as.integer(window)
                if (window < 1L) {
                    stop("window must be a positive integer, if length(window)==1")
                }
                # 2025-07-05 window <- hamming.local(floor(nx / window))
                window <- makeFilter("hamming", floor(nx / window), normalize = FALSE, asKernel = FALSE)
                oceDebug(debug, "window provided, but spec not provided,; using hamming window of length ", nx / window, "\n")
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
                # 2025-07-05 window <- hamming.local(nfft)
                window <- makeFilter("hamming", nfft, normalize = FALSE, asKernel = FALSE)
                oceDebug(debug, "window provided, but spec not provided,; using hamming window of length ", nx / window, "\n")
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
                # 2025-07-05 window <- hamming.local(windowLength)
                window <- makeFilter("hamming", windowLength, normalize = FALSE, asKernel = FALSE)
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
