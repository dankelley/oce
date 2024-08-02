# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Class to Store Satellite Data
#'
#' This class holds satellite data of various types, including
#' [amsr-class] and [g1sst-class].
#'
#' @author Dan Kelley and Chantelle Layton
#'
#' @family classes holding satellite data
setClass("satellite", contains = "oce")

setMethod(
    f = "initialize",
    signature = "satellite",
    definition = function(.Object, filename, subclass, ...) {
        .Object <- callNextMethod(.Object, ...)
        if (!missing(filename)) {
            .Object@metadata$filename <- filename
        }
        .Object@processingLog$time <- presentTime()
        .Object@processingLog$value <- if (missing(subclass)) {
            "create 'satellite' object"
        } else {
            paste("create '", subclass, "' object")
        }
        return(.Object)
    }
)

#' Summarize a satellite Object
#'
#' @param object a [satellite-class] object.
#'
#' @param ... Ignored.
#'
#' @author Dan Kelley
setMethod(
    f = "summary",
    signature = "satellite",
    definition = function(object, ...) {
        invisible(callNextMethod()) # summary
    }
)

#' Plot a satellite Object
#'
#' For an example using `g1sst` data, see [read.g1sst()].
#'
#' @param x a [satellite-class] object.
#'
#' @param y String indicating the quantity to be plotted.
#'
#' @param asp Optional aspect ratio for plot.
#'
#' @param debug A debugging flag, integer.
#'
#' @param ... extra arguments passed to [imagep()], e.g. set
#' `col` to control colors.
#'
#' @author Dan Kelley
#'
#' @family functions that plot oce data
#'
#' @aliases plot.satellite
setMethod(
    f = "plot",
    signature = signature("satellite"),
    definition = function(x, y, asp, debug = getOption("oceDebug"), ...) {
        oceDebug(debug, "plot.satellite(..., y=c(", if (missing(y)) "(missing)" else y, ", ...) START\n", sep = "", unindent = 1)
        if (missing(y)) {
            stop("must indicate what to plot")
        }
        lon <- x[["longitude"]]
        lat <- x[["latitude"]]
        if (missing(asp)) {
            asp <- 1 / cos(pi / 180 * abs(mean(lat, na.rm = TRUE)))
        }
        if ("zlab" %in% names(list(...))) {
            imagep(lon, lat, x[[y]], asp = asp, ...)
        } else {
            imagep(lon, lat, x[[y]], asp = asp, zlab = y, ...)
        }
        oceDebug(debug, "END plot.satellite()\n", unindent = 1)
    }
)
