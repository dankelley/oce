"processingLog<-" <- function(x, value)
{
    if (inherits(x, "oce")) {
        if (0 == length(x@processingLog)) {
            x@processingLog <- list(time=as.POSIXct(Sys.time(), tz="UTC"), value=value)
        } else {
            x@processingLog$time <- c(x@processingLog$time, as.POSIXct(Sys.time(), tz="UTC"))
            x@processingLog$value <- c(x@processingLog$value, value)
        }
    } else {
        stop("'x' is not an oce object")
    }
    x
}

processingLog <- function(h, value="")
{
    res <- if (is.null(h)) list(time=NULL, value=NULL) else h
    if (is.null(h$time[1])) {
        res$time <- as.POSIXct(Sys.time(), tz="UTC")
        res$value <- value
    } else {
        res$time <- c(res$time, as.POSIXct(Sys.time(), tz="UTC"))
        res$value <- c(res$value, value)
    }
    res
}

processingLogAppend <- function(h, value="")
{
    if (inherits(h, "oce"))
        h <- h@processingLog
    res <- if (is.null(h)) list(time=NULL, value=NULL) else h
    if (is.null(h$time[1])) {
        res$time <- as.POSIXct(Sys.time(), tz="UTC")
        res$value <- value
    } else {
        res$time <- c(res$time, as.POSIXct(Sys.time(), tz="UTC"))
        res$value <- c(res$value, value)
    }
    res
}


processingLogItem <- function(value="")
{
    list(time=c(Sys.time()), value=value)
}

processingLogShow <- function(x)
{
    cat("* Processing Log::\n")
    for (i in seq_along(x@processingLog$value)) {
        cat("  * ", format(x@processingLog$time[i]), " UTC: ``",
            x@processingLog$value[i], "``\n", sep="")
    }
}

