pwelch <- function(x, window, noverlap, nfft, fs, spectrumtype, esttype,
                   debug=getOption("oceDebug"), ...)
{
    hamming.local <- function (n) # avoid having to pull in the signal library
    {
        if (!(n == round(n) && n > 0))
            stop("n must be a positive integer > 0")
        if (n == 1)
            c = 1
        else {
            n = n - 1
            pi <- 4 * atan2(1, 1) # avoid problems if user redefined this
            c = 0.54 - 0.46 * cos(2 * pi * (0:n)/n)
        }
        c
    }
    hanning.local <- function(n) # avoid having to pull in the signal library
    {
        if (!(length(n) == 1 && (n == round(n)) && (n > 0)))
            stop("n must be a positive integer")
        if (n == 1)
            c <- 1
        else {
            pi <- 4 * atan2(1, 1)       # avoid problems if user redefined this
            n <- n - 1
            c <- 0.5 - 0.5 * cos(2 * pi * (0:n)/n)
        }
        c
    }
    gave.window <- !missing(window)
    gave.nfft <- !missing(nfft)
    gave.fs <- !missing(fs)
    oceDebug(debug, sprintf("\b\bpwelch(x, window=%s, nfft=%s, fs=%s, ...) {\n",
                             if (gave.window) window else "(not given)",
                             if (gave.nfft) nfft else "(not given)",
                             if (gave.fs) fs else "(not given)"))
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
        } else {
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
            window <- hamming.local(floor(x.len / 8))
        }
    }
    normalization <- mean(window^2)
    window.len <- length(window)
    if (missing(noverlap)) {
        noverlap <- floor(window.len / 2)
    }
    step <- floor(window.len - noverlap + 1)
    oceDebug(debug, "window.len=",window.len,"  step=",step,"  noverlap=", noverlap, "  x.len=", x.len, "\n")
    if (step < 1)
        stop("overlap cannot exceed segment length")
    i0 <- 1
    nwindows <- floor(x.len / window.len)
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
        oceDebug(debug, "  subspectrum at indices ", start, "to", end, "\n")
        xx <- ts(window * x[start:end], frequency=fs)
        args$x <- xx
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
    oceDebug(debug, "calculating spectrum across matrix of dimension", dim(psd), "\n")
    oceDebug(debug, "\b\b} # pwelch()\n")
    list(freq=freq, spec=2*apply(psd, 2, mean), nwindow=nrow)
}
