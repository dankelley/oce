"processingLog<-" <- function(x, value)
{
    if (inherits(x, "oce")) {
        h <- list(time=x@processingLog[[1]], value=x@processingLog[[2]]) # using indices to allow renaming
        h$time <- c(h$time, as.POSIXct(Sys.time(), tz="UTC"))
        h$value <- c(h$value, value)
        x@processingLog <- h
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

