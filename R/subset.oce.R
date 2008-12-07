subset.oce <- function (x, subset,...)
{
    r <- eval(substitute(subset), x$data, parent.frame())
    r <- r & !is.na(r)
    rval <- x
    rval$data <- x$data[r,]
    rval <- processing.log.append(rval, paste("modified by subset.oce(x, ", deparse(substitute(subset)), ")", sep=""))
    class(rval) <- class(x)
    rval
}
