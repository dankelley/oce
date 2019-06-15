## vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Class to Store Satellite Data
#'
#' This class holds satellite data of various types, including
#' \code{\link{amsr-class}} and \code{\link{g1sst-class}}.
#' @author Dan Kelley and Chantelle Layton
#' @concept satellite
#' @family things related to satellite data
setClass("satellite", contains="oce")

setMethod(f="initialize",
          signature="satellite",
          definition=function(.Object, filename, subclass) {
              if (!missing(filename))
                  .Object@metadata$filename <- filename
              .Object@processingLog$time <- presentTime()
              .Object@processingLog$value <- if (missing(subclass))
                  "create 'satellite' object" else paste("create '", subclass, "' object")
              return(.Object)
          })

#' Summarize a satellite object
#'
#' @param object The object to be summarized.
#' @param ... Ignored.
#' @author Dan Kelley
#' @concept satellite
#' @family things related to satellite data
setMethod(f="summary",
          signature="satellite",
          definition=function(object, ...) {
              ## message("JUNK BEGIN")
              ## spacecraft <- if ("spacecraft" %in% names(object@metadata))
              ##     object@metadata$spacecraft
              ## else if ("satellite" %in% names(object@metadata))
              ##     object@metadata$satellite
              ## else
              ##     ""
              ## cat("Satellite Summary\n-----------------\n\n")
              ## showMetadataItem(object, "filename",   "Data file:           ")
              ## showMetadataItem(object, "satellite",  "Satellite:           ")
              ## lon <- object@metadata$longitude
              ## lat <- object@metadata$latitude
              ## if (length(lon) > 2) cat("* Longitude:           ", lon[1], ", ", lon[2],  ", ..., ", tail(lon, 1), "\n", sep="")
              ## else cat("* Longitude:           ", paste(lon, collapse=", "), "\n", sep="")
              ## if (length(lat) > 2) cat("* Latitude:            ", lat[1], ", ", lat[2],  ", ..., ", tail(lat, 1), "\n", sep="")
              ## else cat("* Latitude:            ", paste(lat, collapse=", "), "\n", sep="")
              ## if ("LANDSAT_8" == spacecraft) {
              ##     cat("* Time:                ",
              ##         format(object@metadata$time, "%Y-%m-%d %H:%M:%S %z"), "\n", sep="")
              ## }
              ## message("JUNK END")
              invisible(callNextMethod()) # summary
          })

#' Plot a satellite object
#'
#' For an example using \code{g1sst} data, see \code{\link{read.g1sst}}.
#'
#' @param x An object inherting from \code{\link{satellite-class}}.
#' @param y String indicating the quantity to be plotted.
#' @param asp Optional aspect ratio for plot.
#' @param debug A debugging flag, integer.
#' @param ... extra arguments passed to \code{\link{imagep}}, e.g. set
#' \code{col} to control colors.
#'
#' @concept satellite
#' @author Dan Kelley
#' @family things related to satellite data
#' @family functions that plot oce data
#' @aliases plot.satellite
setMethod(f="plot",
          signature=signature("satellite"),
          definition=function(x, y, asp, debug=getOption("oceDebug"), ...)
          {
              oceDebug(debug, "plot.satellite(..., y=c(",
                       if (missing(y)) "(missing)" else y, ", ...) {\n", sep="", unindent=1)
              if (missing(y))
                  stop("must indicate what to plot")
              lon <- x[["longitude"]]
              lat <- x[["latitude"]]
              if (missing(asp)) asp <- 1/cos(pi/180*abs(mean(lat, na.rm=TRUE)))
              if ("zlab" %in% names(list(...))) {
                  imagep(lon, lat, x[[y]], asp=asp, ...)
              } else {
                  imagep(lon, lat, x[[y]], asp=asp, zlab=y, ...)
              }
              oceDebug(debug, "} # plot.satellite()\n", unindent=1)
          })
