processing.log.append <- function (x, action="")
{
    x$processing.log$time <- c(x$processing.log$time, as.POSIXct(Sys.time(),tz="GMT"))
    x$processing.log$action <- c(x$processing.log$action, action)
    x
}
processing.log.item <- function(log.action="")
{
    rval <- list(time=c(Sys.time()), action=log.action)
    class(rval) <- "processing.log"
    rval
}

processing.log.summary <- function(object)
{
    n <- length(object$processing.log$action)
    if (n > 0) {
        res <- NULL
        for (i in 1:n) {
            kludge <- gsub(", [   ]*", ", ", object$processing.log$action[i]) # FIXME: why are these spaces there?
            res <- c(res, paste("  ",
                                paste(as.character(as.POSIXlt(object$processing.log$time[i], tz="UTC")),
                                      " UTC\n     ``", kludge, "``\n",sep="")))
        }
    } else {
        res <- "  (none)"
    }
    class(res) <- "processing.log.summary"
    res
}

print.processing.log.summary <- function(x, ...)
{
    n <- length(x)
    cat("* Processing log\n\n", ...)
    if (n > 0) for (i in 1:n) cat(x[i]) else cat("  (none)\n", ...)
}
