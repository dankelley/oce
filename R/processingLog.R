processingLogAdd <- function(pl, action="")
{
    res <- if (is.null(pl)) list(time=NULL, action=NULL) else pl
    res$time <- c(res$time, as.POSIXct(Sys.time(), tz="GMT"))
    res$action <- c(res$action, action)
    class(res) <- "processingLog"
    res
}

processingLogAppend <- function(x, action="")
{
    res <- x
    res$processingLog$time <- c(res$processingLog$time, as.POSIXct(Sys.time(), tz="GMT"))
    res$processingLog$action <- c(res$processingLog$action, action)
    res
}

processingLogItem <- function(action="")
{
    rval <- list(time=c(Sys.time()), action=action)
    class(rval) <- "processingLog"
    rval
}

processingLogSummary <- function(object)
{
    n <- length(object$processingLog$action)
    if (n > 0) {
        res <- NULL
        for (i in 1:n) {
            kludge <- gsub(", [   ]*", ", ", object$processingLog$action[i]) # FIXME: why are these spaces there?
            res <- c(res, paste("  *", paste(format(number.as.POSIXct(object$processingLog$time[i])),
                                             " UTC: ``", kludge, "``\n",sep="")))
        }
    } else {
        res <- "  (none)"
    }
    class(res) <- "processingLogSummary"
    res
}

print.processingLogSummary <- function(x, ...)
{
    n <- length(x)
    cat("* processingLog\n\n", ...)
    if (n > 0)
        for (i in 1:n)
            cat(x[i]) else cat("  (none)\n", ...)
}
