processing.log.add <- function(pl, action="")
{
    res <- if (is.null(pl)) list(time=NULL, action=NULL) else pl
    res$time <- c(res$time, as.POSIXct(Sys.time(), tz="GMT"))
    res$action <- c(res$action, action)
    class(res) <- "processing.log"
    res
}

processing.log.append <- function(x, action="")
{
    res <- x
    print(str(res$processing.log))
    res$processing.log$time <- c(res$processing.log$time, as.POSIXct(Sys.time(), tz="GMT"))
    res$processing.log$action <- c(res$processing.log$action, action)
    res
}

processing.log.item <- function(log.action="")
{
    rval <- list(time=c(Sys.time()), action=log.action)
    class(rval) <- "processing.log"
    rval
}

summary.processing.log <- function(object)
{
    n <- length(object$processing.log$action)
    if (n > 0) {
        res <- NULL
        for (i in 1:n) {
            kludge <- gsub(", [   ]*", ", ", object$processing.log$action[i]) # FIXME: why are these spaces there?
            res <- c(res, paste("  *",
                                paste(as.character(as.POSIXlt(object$processing.log$time[i], tz="UTC")),
                                      " UTC: ``", kludge, "``\n",sep="")))
        }
    } else {
        res <- "  (none)"
    }
    class(res) <- "processing.log.summary"
    res
}

print.summary.processing.log <- function(x, ...)
{
    n <- length(x)
    cat("* Processing log\n\n", ...)
    if (n > 0) for (i in 1:n) cat(x[i]) else cat("  (none)\n", ...)
}
