subset.oce <- function (x, subset,...)
{
    r <- eval(substitute(subset), x$data, parent.frame())
    r <- r & !is.na(r)
    rval <- x
    rval$data <- x$data[r,]
    class(rval) <- class(x)
    rval
}
