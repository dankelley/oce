runderiv <- function(x, y, window=c("hanning", "boxcar"), L)
{
    if (missing(x)) stop("must supply 'x'")
    if (missing(y)) stop("must supply 'y'")
    nx <- length(x)
    ny <- length(y)
    window <- match.arg(window)
    if (nx != ny)
        stop("lengths of x and y must match, but they are ", nx, " and ", ny, ", respectively\n")
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
    .Call("run_deriv", x, y, L, switch(window, boxcar=0, hanning=1))
}

