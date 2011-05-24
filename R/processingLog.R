"processingLog<-" <- function(x, value)
{
    if (inherits(x, "oce")) {
        h <- list(time=x$processingLog[[1]], value=x$processingLog[[2]]) # using indices to allow renaming
        h$time <- c(h$time, as.POSIXct(Sys.time(), tz="UTC"))
        h$value <- c(h$value, value)
        class(h) <- "processingLog"
        x$processingLog <- h
    } else {
        stop("'x' is not an oce object")
    }
    x
}

processingLog <- function(h, value="")
{
    res <- if (is.null(h)) list(time=NULL, value=NULL) else h
    res$time <- c(res$time, as.POSIXct(Sys.time(), tz="UTC"))
    res$value <- c(res$value, value)
    class(res) <- "processingLog"
    res
}

processingLogItem <- function(value="")
{
    rval <- list(time=c(Sys.time()), value=value)
    class(rval) <- "processingLog"
    rval
}

summary.processingLog <- function(object, ...)
{
    n <- length(object[[2]])
    if (n > 0) {
        res <- NULL
        for (i in 1:n) {
            kludge <- gsub(", [   ]*", ", ", object[[2]][i]) # use [[i]] because oce<0.3 called it "action"
            res <- c(res, paste("  *", paste(format(numberAsPOSIXct(object$time[i])),
                                             " UTC: ``", kludge, "``\n",sep="")))
        }
    } else {
        res <- "  (none)"
    }
    class(res) <- "summary.processingLog"
    res
}

print.summary.processingLog <- function(x, digits = max(6, getOption("digits") - 1), ...)
{
    n <- length(x)
    cat("* Processing Log::\n\n", ...)
    if (n > 0) {
        for (i in 1:n)
            cat(x[i])
    } else {
        cat("  (none)\n", ...)
    }
}
