#' Add an Item to a Processing Log
#'
#' @param x an [oce-class] object.
#'
#' @param value A character string with the description of the logged activity.
#'
#' @examples
#' data(ctd)
#' processingLogShow(ctd)
#' processingLog(ctd) <- "test"
#' processingLogShow(ctd)
#'
#' @family things related to processing logs
"processingLog<-" <- function(x, value) {
    if (inherits(x, "oce")) {
        if (0 == length(x@processingLog)) {
            x@processingLog <- list(time = presentTime(), value = value)
        } else {
            x@processingLog$time <- c(x@processingLog$time, presentTime())
            x@processingLog$value <- c(x@processingLog$value, value)
        }
    } else {
        stop("'x' is not an oce object")
    }
    x
}


#' Append an Item to a Processing Log
#'
#' @return An [list()] containing items named
#' `time` and `value`, i.e. the times of entries
#' and the text notations of those entries..
#'
#' @param h either the `processingLog` slot of an object, or
#' an `oce` object from which the processingLog will be extracted
#'
#' @param value A string indicating the text of the log entry.
#'
#' @family things related to processing logs
processingLogAppend <- function(h, value = "") {
    if (inherits(h, "oce")) {
        h <- h@processingLog
    }
    res <- if (is.null(h)) list(time = NULL, value = NULL) else h
    if (is.null(h$time[1])) {
        res$time <- presentTime()
        res$value <- value
    } else {
        res$time <- c(res$time, presentTime())
        res$value <- c(res$value, value)
    }
    res
}

#' Create an Item That can be Inserted into a Processing Log
#'
#' A function is used internally to initialize processing logs.
#' Users will probably prefer to use [processingLogAppend()]
#' instead.
#'
#' @param value A string that will be used for the item.
#'
#' @return A [list()] containing `time`, which is
#' the time in UTC (calculated with [presentTime()])
#' at the moment the function is called and
#' `value`, a string that is set to the argument of the same name.
#'
#' @family things related to processing logs
processingLogItem <- function(value = "") {
    list(time = c(presentTime()), value = value)
}

#' Show the Processing Log of an oce Object
#'
#' @param x an [oce-class] object.
#'
#' @family things related to processing logs
processingLogShow <- function(x) {
    cat("* Processing Log\n\n")
    for (i in seq_along(x@processingLog$value)) {
        cat("    - ", format(x@processingLog$time[i], tz = "UTC"), " UTC: `",
            x@processingLog$value[i], "`\n",
            sep = ""
        )
    }
}
