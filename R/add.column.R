add.column <- function (x, data, name)
{
    if (!inherits(x, "oce")) stop("method is only for oce objects")
    if (missing(data)) stop("must supply data")
    if (missing(name)) stop("must supply name")
    n <- dim(x$data)[1]
    nd <- length(data)
    if (nd != n) stop("data length is ", nd, " but it must be ", n, " to match existing data")
    rval <- x
    rval$data <- data.frame(x$data, data)
    names(rval$data) <- c(names(x$data), name)
    rval <- processing.log.append(rval, paste(deparse(match.call()), sep="", collapse=""))
    rval
}
