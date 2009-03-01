decimate <- function(x, by=10, method=c("direct", "filter"), filter)
{
    if (!inherits(x, "oce")) stop("method is only for oce objects")
    method <- match.arg(method)
    res <- x
    if (method == "direct") {
        i <- seq(1, dim(x$data)[1], by=by)
        res$data <- x$data[i,]
    } else if (method == "filter") {
        if (missing(filter)) stop("must supply a filter")
        cat("doing filter method\n")
        nvar <- dim(x$data)[2]
        for (var in 1:nvar) {
            res$data[,var] <- filter(x$data[,var], filter)
        }
        i <- seq(1, dim(x$data)[1], by=by)
        res$data <- res$data[i,]
    }
    processing.log.append(res, paste(deparse(match.call()), sep="", collapse=""))
}
