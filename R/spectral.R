pwelch <- function(x, window, noverlap, nfft, fs, spectrumtype,
	esttype, debug=getOption("oce.debug"))
{
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
    if (!missing(nfft))
        warning("'nfft' is ignored at present")
    if (!missing(spectrumtype))
        warning("'spectrumtype' is ignored at present")
    if (!missing(esttype))
        warning("'esttype' is ignored at present")
    if (missing(window)) {
        window <- hanning.local(floor(x.len / 8))
    } else {
        if (length(window) == 1) {
            window <- hanning.local(floor(x.len / window))
        } else {
            stop("for now, 'window' may only be a list of numbers, or a single number")
        }
    }
    window.len <- length(window)
    if (missing(noverlap)) {
        noverlap <- floor(window.len / 2)
    }
    step <- floor(window.len - noverlap + 1)
    oce.debug(debug, "window.len=",window.len,"  step=",step,"  noverlap=", noverlap, "  x.len=", x.len, "\n")
    if (step < 1)
        stop("overlap cannot exceed segment length")
    i0 <- 1
    nwindows <- floor(x.len / window.len)
    psd <- NULL
    nrow <- 0
    start <- 1
    end <- window.len
    while (end < (x.len - window.len)) {
        oce.debug(debug, "start:end = ", start, ":", end, "\n")
        xx <- ts(window * x[start:end], frequency=fs)
        spectrum <- spectrum(xx, plot=FALSE) # FIXME: check the args!
        psd <- c(psd, spectrum$spec) # FIXME: this is slow; should pre-allocate
        start <- start + step
        end <- end + step
        nrow <- nrow + 1
    }
    psd <- matrix(psd, nrow=nrow, byrow=TRUE)
    list(freq=spectrum$freq, spec=apply(psd, 2, mean), nwindow=nrow)
}

