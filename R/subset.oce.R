subset.oce <- function (x, subset,...)
{
    if (inherits(x, "section")) stop("subset() does not work on section objects")
    r <- eval(substitute(subset), x$data, parent.frame())
    r <- r & !is.na(r)
    rval <- x
    rval$data <- x$data[r,]
    rval <- processing.log.append(rval, paste("modified by subset.oce(x, ", deparse(substitute(subset)), ")", sep=""))
    class(rval) <- class(x)
    rval
}
