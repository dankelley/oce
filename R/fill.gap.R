fill.gap <- function(x, start, end, column=NULL)
{
    if (!inherits(x, "oce")) stop("method is only for oce objects")
    if (missing(start)) stop("must supply start")
    if (missing(end)) stop("must supply end")
    if (!is.null(column)) {
        start <- which(x$data[[column]] == start)[1]
        end <- which(x$data[[column]] == end)[1]
    }
    if (end - start < 1) stop("end must be at least 1+start")
    rval <- x
    i <- start:end
    for (name in names(x$data)) {
        filler <- approx(x=c(start, end), y=x$data[[name]][c(start,end)], xout=i)$y
        class(filler) <- class(x$data[[name]])
        rval$data[[name]][i] <- filler
    }
    rval <- processing.log.append(rval, paste(deparse(match.call()), sep="", collapse=""))
    rval
}
