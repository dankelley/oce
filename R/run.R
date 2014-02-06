runderiv <- function(x, y, L)
{
    if (missing(x)) stop("must supply 'x'")
    if (missing(y)) stop("must supply 'y'")
    if (missing(L))
        L <- 5 * median(diff(x), na.rm=TRUE)
    .Call("running_derivative", x, y, L)
}

