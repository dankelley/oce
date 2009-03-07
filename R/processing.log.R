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
#old.processing.log.summary <- function(object)
#{
#    if (!is.null(object$processing.log$action)) {
#        cat("Processing log:\n")
#        n <- length(object$processing.log$action)
#        for (i in 1:n) {
#            cat(paste("  ",
#                      paste(as.character(as.POSIXlt(object$processing.log$time[i], "UTC"),sep=""),
#                            "UTC\n   ", object$processing.log$action[i], "\n"),sep=""))
#        }
#    }
#    invisible(object)
#}
processing.log.summary <- function(object)
{
    n <- length(object$processing.log$action)
    if (n > 0) {
        res <- NULL
        cat("n=",n,"\n")
        for (i in 1:n) {
            cat("i=",i,"\n")
            print(object$processing.log$action[i])
            print(object$processing.log$time[i])
            print(as.character(as.POSIXlt(object$processing.log$time[i], "UTC")))
            res <- c(res, paste("  ",
                                paste(as.character(as.POSIXlt(object$processing.log$time[i], "UTC"),sep=""),
                                      "UTC\n   ", object$processing.log$action[i], "\n"),sep=""))
        }
    } else {
        res <- " (none)"
    }
    class(res) <- "processing.log.summary"
    res
}
print.processing.log.summary <- function(x)
{
    n <- length(x)
    cat("Processing Log:\n")
    if (n > 0) for (i in 1:n) cat(x[i]) else cat("  (none)\n")
}

