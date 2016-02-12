runlm <- function(x, y, xout, window=c("hanning", "boxcar"), L, deriv)
{
    if (missing(x)) stop("must supply 'x'")
    if (missing(y)) stop("must supply 'y'")
    x <- as.vector(x)
    y <- as.vector(y)
    nx <- length(x)
    ny <- length(y)
    if (nx != ny)
        stop("lengths of x and y must match, but they are ", nx, " and ", ny, ", respectively\n")
    if (!missing(deriv) && deriv != 0 && deriv != 1)
        stop("deriv must be 0 or 1\n")
    if (missing(xout))
        xout <- x
    window <- match.arg(window)
    if (missing(L)) {
        spacing <- median(abs(diff(x)), na.rm=TRUE)
        if (nx > 20)
            L <- spacing * floor(nx / 10)
        else if (nx > 10)
            L <- spacing * floor(nx / 3)
        else
            L <- spacing * floor(nx / 2)
        ## adjust for bandwidth.  Table 1 of harris1979otuo calls our
        ## "hanning" as "Hanning alpha=2", and this has equivalent
        ## noise bandwidth 1.5, 3.0-db bandwidth 1.44, both in bin
        ## units; we here multiply by L by 1.5.
        if (window == "hanning")
            L <- L * 1.5
        ##cat("L:", L, ", spacing:", spacing, "\n")
    }
    res <- .Call("run_lm", x, y, xout, switch(window, boxcar=0, hanning=1), L)
    if (!missing(deriv) && deriv == 0)
        res <- res$y
    else if (!missing(deriv) && deriv == 1)
        res <- res$dydx
    res
}

