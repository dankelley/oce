## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4


#' Class to store lowered-adp data
#'
#' Class to store data measured with a lowered ADP (also
#' known as ADCP) device.
#'
#' @section Accessing data:
#'
#'     Consider an object named \code{ladp}.
#'
#'     Metadata (contained in the S4 slot named \code{metadata}) may be retrieved
#'     or set by name, \code{ladp[["longitude"]] <- ladp[["longitude"]] + 1}
#'     corrects a one-degree error.  Use \code{names(ladp@@metadata)} to find the
#'     names of the metadata.
#'
#'     Column data may be accessed by name, e.g. \code{ladp[["u"]]},
#'     \code{ladp[["v"]]}, \code{ladp[["pressure"]]}, etc.  There may also be
#'     columns for \code{"temperature"} and \code{"salinity"}, and possibly other
#'     things. Use \code{names(ladp@@data)} to find the names of the data.
#'
#' @author Dan Kelley
#'
#' @family things related to \code{ladp} data
setClass("ladp", contains="oce")

setMethod(f="initialize",
          signature="ladp",
          definition=function(.Object, longitude, latitude, station, waterDepth, time,
                              pressure, u, v, salinity, temperature, ...) {
              ## Assign to some columns so they exist if needed later (even if they are NULL)
              .Object@metadata$longitude <- if (missing(longitude)) "?" else longitude
              .Object@metadata$latitude <- if (missing(latitude)) "?" else latitude
              .Object@metadata$station <- if (missing(station)) "?" else station
              .Object@metadata$waterDepth <- if (missing(waterDepth)) NA else waterDepth
              .Object@metadata$time <- if (missing(time)) NULL else time
              .Object@data$pressure <- if (missing(pressure)) NULL else pressure
              .Object@data$u <- if (missing(u)) NULL else u
              .Object@data$v <- if (missing(v)) NULL else v
              .Object@data$salinity <- if (missing(salinity)) NULL else salinity
              .Object@data$temperature <- if (missing(temperature)) NULL else temperature
              dots <- list(...)
              dotsNames <- names(dots)
              for (i in seq_along(dots)) {
                  ##message("extra column named: ", dotsNames[i])
                  .Object@data[dotsNames[i]] <- dots[i]
              }
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'ladp' object"
              return(.Object)
          })


#' Summarize an ladp object
#'
#' Pertinent summary information is presented, including the station name,
#' sampling location, data ranges, etc.
#'
#' @param object An object of class \code{"ladp"}, usually, a result of a call to
#' \code{\link{as.ladp}}.
#'
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A matrix containing statistics of the elements of the \code{data} slot.
#'
#' @author Dan Kelley
#'
#' @family things related to \code{ladp} data
setMethod(f="summary",
          signature="ladp",
          definition=function(object, ...) {
              cat("LADP Summary\n------------\n\n")
              showMetadataItem(object, "station", "Station:             ")
              invisible(callNextMethod()) # summary
          })

#' @title Extract Something From an ladp Object
#'
#' @param x A \code{ladp} object, i.e. one inheriting from \code{\link{ladp-class}}.
#' @template sub_subTemplate
#'
#' @examples
#' data(ctd)
#' head(ctd[["temperature"]])
#'
#' @author Dan Kelley
#'
#' @family things related to \code{ladp} data
setMethod(f="[[",
          signature(x="ladp", i="ANY", j="ANY"),
          ##definition=function(x, i, j=NULL, drop=NULL) {
          definition=function(x, i, j, ...) {
              if (i == "pressure" || i == "p") {
                  x@data$pressure
              } else if (i == "u") {
                  x@data$u
              } else if (i == "v") {
                  x@data$v
              } else if (i == "uz") {
                  x@data$uz
              } else if (i == "vz") {
                  x@data$vz
              } else if (i == "temperature" || i == "t") {
                  ## FIXME: document "t" part
                  x@data$temperature
              } else if (i == "salinity" || i == "S") {
                  x@data$salinity
              } else {
                  callNextMethod()     # [[
              }
          })


#' Plot an ladp object
#'
#' Uses \code{\link{plotProfile}} to create panels of depth variation of easterly
#' and northerly velocity components.
#'
#' @param x A \code{ladp} object, e.g. as read by \code{\link{as.ladp}}.
#' @param which Vector of names of items to be plotted.
#' @param ... Other arguments, passed to plotting functions.
#'
#' @author Dan Kelley
#'
#' @family things related to \code{ladp} data
#' @family functions that plot \code{oce} data
#' @aliases plot.ladp
setMethod(f="plot",
          signature=signature("ladp"),
          definition=function(x, which=c("u", "v"), ...) {
              par(mfrow=c(1, length(which)))
              for (w in which)
                  plotProfile(x, xtype=w, ...)
          })

fixColumn <- function(x) {
    x[!is.finite(x)] <- NA
    as.vector(x)
}



#' Coerce data into an ladp object
#'
#' This function assembles vectors of pressure and velocity, possibly also with
#' shears, salinity, temperature, etc.
#'
#' @param longitude longitude in degrees east, or an \code{oce} object that
#' contains the data otherwise given by \code{longitude} and the
#' other arguments.
#' @param latitude latitude in degrees east (use negative in southern hemisphere).
#' @param station number or string indicating station ID.
#' @param time time at the start of the profile, constructed by e.g. \code{\link{as.POSIXct}}.
#' @param pressure pressure in decibars, through the water column.
#' @param u eastward velocity (m/s).
#' @param v northward velocity (m/s).
#' @param uz vertical derivative of eastward velocity (1/s).
#' @param vz vertical derivative of northward velocity (1/s).
#' @param salinity salinity through the water column, in practical salinity units.
#' @param temperature temperature through the water column.
#' @param \dots optional additional data columns.
#'
#' @return An object of \code{\link{ladp-class}}.
#'
#' @author Dan Kelley
#'
#' @family things related to \code{ladp} data
as.ladp <- function(longitude, latitude, station, time, pressure, u, v, uz, vz, salinity, temperature, ...)
{
    if (inherits(longitude, "oce")) {
        x <- longitude
        longitude <- x[["longitude"]]
        latitude <- x[["latitude"]]
        station <- x[["station"]]
        time <- x[["time"]]
        ## try hard to get pressure
        pressure <- x[["pressure"]]
        if (is.null(pressure)) {
            z <- x[["z"]]
            if (is.null(z)) {
                depth <- x[["depth"]]
                if (is.null(depth)) {
                    stop("parent object lacks pressure, depth, and z")
                }
                pressure <- abs(depth)
            } else {
                pressure <- abs(z)
            }
        }
        u <- x[["u"]]
        if (is.null(u)) stop("parent object lacks u")
        v <- x[["v"]]
        if (is.null(v)) stop("parent object lacks v")
        uz <- x[["uz"]]
        vz <- x[["vz"]]
        salinity <- x[["salinity"]]
        temperature <- x[["temperature"]]
    } else if (is.data.frame(longitude) || is.list(longitude)) {
        x <- longitude
        names <- names(x)
        longitude <- x$longitude[1]
        latitude <- x$latitude[1]
        station <- x$station[1]
        time <- x$time
        pressure <- x$pressure
        if (is.null(pressure)) stop("parent object lacks pressure")
        u <- x$u
        if (is.null(u)) stop("parent object lacks u")
        v <- x$v
        if (is.null(v)) stop("parent object lacks v")
        uz <- if ("uz" %in% names) x$uz else NULL
        vz <- if ("vz" %in% names) x$vz else NULL
        salinity <- if ("salinity" %in% names) x$salinity else NULL
        temperature <- if ("temperature" %in% names) x$temperature else NULL
    } else {
        if (missing(longitude)) stop("must supply longitude")
        if (missing(latitude)) stop("must supply latitude")
        if (missing(station)) station <- "?"
        if (missing(time)) time <- NULL
        if (missing(pressure)) stop("must supply pressure")
        if (missing(u)) stop("must supply u") else u <- fixColumn(u)
        if (missing(v)) stop("must supply v") else v <- fixColumn(v)
        uz <- if (missing(uz)) NULL else fixColumn(uz)
        vz <- if (missing(vz)) NULL else fixColumn(vz)
        salinity <- if (missing(salinity)) NULL else fixColumn(salinity)
        temperature <- if (missing(temperature)) NULL else fixColumn(temperature)
    }
    res <- new("ladp", longitude=longitude, latitude=latitude, station=station, time=time,
                pressure=pressure, u=u, v=v, uz=uz, vz=vz, salinity=salinity, temperature=temperature, ...)
    res@metadata$waterDepth <- max(pressure, na.rm=TRUE)
    res
}
