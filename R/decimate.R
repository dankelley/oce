decimate <- function(x, by=10, method=c("direct"))
{
    if (!inherits(x, "oce")) stop("method is only for oce objects")
    method <- match.arg(method)
    res <- x
    if (method == "direct") {
        i <- seq(1, dim(x$data)[1], by=by)
        res$data <- x$data[i,]
        processing.log.append(res, paste(deparse(match.call()), sep="", collapse=""))
    }                                   # other methods cause error
}
