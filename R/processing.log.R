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
    if (!is.null(object$processing.log$action)) {
        cat("Processing log:\n")
        n <- length(object$processing.log$action)
        for (i in 1:n) {
            cat(paste("  ",
                      paste(as.character(as.POSIXlt(object$processing.log$time[i], "UTC"),sep=""),
                            "UTC\n   ", object$processing.log$action[i], "\n"),sep=""))
        }
    }
    invisible(object)
}

