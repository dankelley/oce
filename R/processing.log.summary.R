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

