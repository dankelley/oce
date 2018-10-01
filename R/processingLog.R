#' Add an item to a processing log (in place)
#'
#' @param x An \code{oce} object.
#' @param value A character string with the description of the logged activity.
#' @examples
#' data(ctd)
#' processingLogShow(ctd)
#' processingLog(ctd) <- "test"
#' processingLogShow(ctd)
#' @family things related to processing logs
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



#' Append an item to a processing log
#' @return An \code{\link{list}} containing items named
#' \code{time} and \code{value}, i.e. the times of entries
#' and the text notations of those entries..
#' @param h either the \code{processingLog} slot of an object, or
#' an \code{oce} object from which the processingLog will be extracted
#' @param value A string indicating the text of the log entry.
#' @family things related to processing logs
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

#' Create an item that can be inserted into a processing log
#'
#' A function is used internally to initialize processing logs.
#' Users will probably prefer to use \code{\link{processingLogAppend}}
#' instead.
#'
#' @param value A string that will be used for the item.k
#' @return A \code{\link{list}} containing \code{time}, which is
#' the \code{\link{Sys.time}} at the moment the function is called and
#' \code{value}, a string that is set to the argument of the same name.
#' @family things related to processing logs
processingLogItem <- function(value="")
{
    list(time=c(Sys.time()), value=value)
}

#' Show the processing log of an \code{oce} object
#' @param x An \code{oce} object.
#' @family things related to processing logs
processingLogShow <- function(x)
{
    cat("* Processing Log\n")
    for (i in seq_along(x@processingLog$value)) {
        cat("    - ", format(x@processingLog$time[i]), " UTC: `",
            x@processingLog$value[i], "`\n", sep="")
    }
}
