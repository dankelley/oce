historyAdd <- function(h, action="")
{
    res <- if (is.null(h)) list(time=NULL, action=NULL) else h
    res$time <- c(res$time, as.POSIXct(Sys.time(), tz="GMT"))
    res$action <- c(res$action, action)
    class(res) <- "history"
    res
}

historyAppend <- function(x, action="")
{
    res <- x
    res$history$time <- c(res$history$time, as.POSIXct(Sys.time(), tz="GMT"))
    res$history$action <- c(res$history$action, action)
    res
}

historyItem <- function(action="")
{
    rval <- list(time=c(Sys.time()), action=action)
    class(rval) <- "history"
    rval
}

summary.history <- function(object, ...)
{
    n <- length(object$action)
    if (n > 0) {
        res <- NULL
        for (i in 1:n) {
            kludge <- gsub(", [   ]*", ", ", object$action[i]) # FIXME: why are these spaces there?
            res <- c(res, paste("  *", paste(format(number.as.POSIXct(object$time[i])),
                                             " UTC: ``", kludge, "``\n",sep="")))
        }
    } else {
        res <- "  (none)"
    }
    class(res) <- "summary.history"
    res
}

print.summary.history <- function(x, digits = max(6, getOption("digits") - 1), ...)
{
    n <- length(x)
    cat("* History::\n\n", ...)
    if (n > 0) {
        for (i in 1:n)
            cat(x[i])
    } else {
        cat("  (none)\n", ...)
    }
}
