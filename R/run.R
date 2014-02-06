runderiv <- function(x, y, L, window=c("boxcar", "hanning"))
{
    if (missing(x)) stop("must supply 'x'")
    if (missing(y)) stop("must supply 'y'")
    if (missing(L))
        L <- 5 * median(diff(x), na.rm=TRUE)
    window <- match.arg(window)
    .Call("run_deriv", x, y, L, switch(window, boxcar=0, hanning=1))
}

