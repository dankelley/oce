# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Class to Store AMSR-2 Satellite Data
#'
#' This class stores data from the AMSR-2 satellite.
#'
#' The Advanced Microwave Scanning Radiometer (AMSR-2) is in current operation on
#' the Japan Aerospace Exploration Agency (JAXA) GCOM-W1 space craft, launched in
#' May 2012. Data are processed by Remote Sensing Systems. The satellite
#' completes an ascending and descending pass during local daytime and nighttime
#' hours respectively. Each daily file contains 7 daytime and 7 nighttime
#' maps of variables named as follows within the `data`
#' slot of amsr objects: `timeDay`,
#' `SSTDay`, `LFwindDay` (wind at 10m sensed in
#' the 10.7GHz band), `MFwindDay` (wind at 10m sensed at 18.7GHz),
#' `vaporDay`, `cloudDay`, and `rainDay`, along with
#' similarly-named items that end in `Night`.
#' See reference 1 for additional information on the instrument, how
#' to cite the data source in a paper, etc.
#'
#' The bands are stored in [raw()] form, to save storage. The accessor
#' function \code{\link{[[,amsr-method}} can provide these values in `raw`
#' form or in physical units; [plot,amsr-method()], and
#' [summary,amsr-method()] work with physical units.
#'
#' @templateVar class amsr
#'
#' @templateVar dataExample {}
#'
#' @templateVar metadataExample Examples that are of common interest include  `longitude` and `latitude`, which define the grid.
#'
#' @template slot_summary
#'
#' @template slot_put
#'
#' @template slot_get
#'
#' @author Dan Kelley and Chantelle Layton
#'
#' @references
#' 1. Information on the satellite, how to cite the data, etc. is
#' provided at `http://www.remss.com/missions/amsr/`.
#'
#' 2. A simple interface for viewing and downloading data is at
#' `http://images.remss.com/amsr/amsr2_data_daily.html`.
#'
#' @family classes holding satellite data
#' @family things related to amsr data
setClass("amsr", contains = "satellite")

setMethod(
    f = "initialize",
    signature = "amsr",
    definition = function(.Object, filename, ...) {
        .Object <- callNextMethod(.Object, ...)
        if (!missing(filename)) {
            .Object@metadata$filename <- filename
        }
        .Object@processingLog$time <- presentTime()
        .Object@processingLog$value <- "create 'amsr' object"
        return(.Object)
    }
)

#<?> setMethod(f="show",
#<?>     signature="amsr",
#<?>     definition=function(object) {
#<?>         cat("Data (physical units):\n")
#<?>         dataNames <- names(object@data)
#<?>         for (b in seq_along(dataNames)) {
#<?>             dim <- if (is.list(object@data[[b]])) dim(object@data[[b]]$lsb) else dim(object@data[[b]])
#<?>             cat("  \"", dataNames[b], "\" has dimension c(", dim[1], ",", dim[2], ")\n", sep="")
#<?>         }
#<?>     })
#<?>

#' Sample amsr Data (Near Nova Scotia)
#'
#' This is a three-day composite satellite image for
#' July 27, 2023, trimmed to show waters south and east of
#' Nova Scotia, using code provide in the \dQuote{Details}
#' section.
#'
#' The following code was used to create this dataset.
#' \preformatted{
#' library(oce)
#' amsr <- read.amsr(download.amsr(2023, 7, 27, destdir="~/data/amsr"))
#' amsr <- subset(amsr, -71 < longitude & longitude < -60, debug=2)
#' amsr <- subset(amsr,  36 < latitude  &  latitude <  45, debug=2)
#' }
#'
#' @name amsr
#' @docType data
#'
#' @usage data(amsr)
#'
#' @examples
#' library(oce)
#' data(coastlineWorld)
#' data(amsr)
#' plot(amsr, "SST")
#' lines(coastlineWorld[["longitude"]], coastlineWorld[["latitude"]])
#'
#' @family satellite datasets provided with oce
#' @family datasets provided with oce
#' @family things related to amsr data
NULL

# Local function to determine amsr version.  An error results if x is not
# an amsr object.  If the object does not contain metadata$version,
# return 1L; otherwise, return metadata$version.
amsrType <- function(x) {
    if (!inherits(x, "amsr")) {
        stop("x is not an amsr object")
    }
    type <- x@metadata$type
    if (is.null(type)) 1L else type
}


#' Summarize an amsr Object
#'
#' Print a summary of key components of the object.
#'
#' @param object an [amsr-class] object.
#'
#' @param ... ignored.
#'
#' @author Dan Kelley
#'
#' @family things related to amsr data
setMethod(
    f = "summary",
    signature = "amsr",
    definition = function(object, ...) {
        cat("Amsr Summary\n------------\n\n")
        showMetadataItem(object, "filename", "Data file:       ")
        cat(sprintf("* Longitude range: %.4fE to %.4fE\n", object@metadata$longitude[1], tail(object@metadata$longitude, 1)))
        cat(sprintf("* Latitude range:  %.4fN to %.4fN\n", object@metadata$latitude[1], tail(object@metadata$latitude, 1)))
        cat(sprintf("* Format type:     %d\n", amsrType(object)))
        # Version 1 data are in raw format, so use [[ to get scientific units
        type <- amsrType(object)
        if (type == 1L) {
            for (name in names(object@data)) {
                object@data[[name]] <- object[[name]]
            }
        }
        invisible(callNextMethod()) # summary
    }
)

#' Extract Something From an amsr Object
#'
#' @param x an [amsr-class] object.
#'
#' @section Details of the Specialized Method:
#'
#' The `[[[` method handles both old-format and new-format [amsr-class]
#' objects. Old-format objects are read by [read.amsr()]
#' from from gzipped files holding data in raw format, from which
#' `[[` computes numeric results with linear
#' relationships provided at at `http://www.remss.com/missions/amsre`.
#' By contrast, new-format objects are read
#' from NetCDF files that hold the data as 4-byte
#' numeric values that are read directly, without applying a
#' scaling transformation.  The other
#' difference is that old-format objects contain day and night values,
#' e.g. `SSTDay` and `SSTNight`, whereas new-format objects contain
#' single values that combine these, e.g. `SST`.
#'
#' If `i` is `"?"`, then the return value is a list
#' containing four items, each of which is a character vector
#' holding the names of things that can be accessed with `[[`.
#' The `data` and `metadata` items hold the names of
#' entries in the object's data and metadata
#' slots, respectively. The `dataDerived`
#' and `metadataDerived` items are things that
#' `[[` can compute and then return.
#'
#' Data within the `data` slot may be found directly (for
#' both new-format and old-format objects) or indirectly (only
#' for old-style objects).  For example, `SST` works by direct
#' lookup for new-format objects, but it is computed using
#' `SSTNight` and `SSTDay` for old-format objects.  Use e.g.
#' `a[["?"]]` for any given object, to see what can be retrieved.
#'
## The conversion from raw to scientific units is done with formulae
## found at `http://www.remss.com/missions/amsre`, e.g. SST is
## computed by converting the raw value to an integer (between 0 and 255),
## multiplying by 0.15C, and subtracting 3C.
##
## The `"raw"` mode can be useful
## in decoding the various types of missing value that are used by `amsr`
## data, namely `as.raw(255)` for land, `as.raw(254)` for
## a missing observation, `as.raw(253)` for a bad observation,
## `as.raw(252)` for sea ice, or `as.raw(251)` for missing SST
## due to rain or missing water vapour due to heavy rain. Note that
## something special has to be done for e.g. `d[["SST","raw"]]`
## because the idea is that this syntax (as opposed to specifying
## `"SSTDay"`) is a request to try to find good
## data by looking at both the Day and Night measurements. The scheme
## employed is quite detailed. Denote by "A" the raw value of the desired field
## in the daytime pass, and by "B" the corresponding value in the
## nighttime pass. If either A or B is 255, the code for land, then the
## result will be 255. If A is 254 (i.e. there is no observation),
## then B is returned, and the reverse holds also. Similarly, if either
## A or B equals 253 (bad observation), then the other is returned.
## The same is done for code 252 (ice) and code 251 (rain).
#'
#' @return
#' `[[` returns numeric matrix data.
#'
#' @examples
#' # Histogram of SST values (for an old-format dataset)
#' library(oce)
#' data(amsr)
#' hist(amsr[["SST"]])
#'
#' @template sub_subTemplate
#'
#' @author Dan Kelley
#'
#' @family things related to amsr data
setMethod(
    f = "[[",
    signature(x = "amsr", i = "ANY", j = "ANY"),
    definition = function(x, i, j, ...) { # [[,amsr-method
        debug <- getOption("oceDebug")
        oceDebug(debug, "amsr [[ {\n", unindent = 1)
        if (missing(i)) {
            stop("Must name a amsr item to retrieve, e.g. '[[\"SST\"]]'", call. = FALSE)
        }
        i <- i[1] # drop extras if more than one given
        if (!is.character(i)) {
            stop("amsr item must be specified by name", call. = FALSE)
        }
        # The storage for the new (netcdf) format is much simpler than that
        # for the old format, since the latter required the use of scale factors
        # on raw numbers, etc.  We handle both new and old formats here.
        type <- amsrType(x)
        if (type == 1L) {
            dataDerived <- c("cloud", "LFwind", "MFwind", "rain", "SST", "time", "vapor")
            if (i == "?") {
                return(list(
                    metadata = sort(names(x@metadata)),
                    metadataDerived = NULL,
                    data = sort(names(x@data)),
                    dataDerived = sort(dataDerived)
                ))
            }
            if (is.character(i) && !is.na(pmatch(i, names(x@metadata)))) {
                oceDebug(debug, "} # amsr [[\n", unindent = 1)
                return(x@metadata[[i]])
            }
            namesAllowed <- c(names(x@data), dataDerived)
            if (!(i %in% namesAllowed)) {
                stop(
                    "band '", i, "' is not available in this object; try one of: ",
                    paste(namesAllowed, collapse = " ")
                )
            }
            # get numeric band, changing land, n-obs, bad-obs, sea-ice and windy to NA
            getBand <- function(b) {
                bad <- b == as.raw(0xff) | # land mass
                    b == as.raw(0xfe) | # no observations
                    b == as.raw(0xfd) | # bad observations
                    b == as.raw(0xfc) | # sea ice
                    b == as.raw(0xfb) # missing SST or wind due to rain, or missing water vapour due to heavy rain
                b <- as.numeric(b)
                b[bad] <- NA
                b
            }
            dim <- c(length(x@metadata$longitude), length(x@metadata$latitude))
            if (missing(j) || j != "raw") {
                # Apply units; see http://www.remss.com/missions/amsre
                # FIXME: the table at above link has two factors for time; I've no idea
                # what that means, and am extracting what seems to be seconds in the day.
                if (i == "timeDay") {
                    res <- 60 * 6 * getBand(x@data[[i]])
                } # FIXME: guessing on amsr time units
                else if (i == "timeNight") {
                    res <- 60 * 6 * getBand(x@data[[i]])
                } # FIXME: guessing on amsr time units
                else if (i == "time") {
                    res <- 60 * 6 * getBand(do_amsr_average(x@data[["timeDay"]], x@data[["timeNight"]]))
                } else if (i == "SSTDay") {
                    res <- -3 + 0.15 * getBand(x@data[[i]])
                } else if (i == "SSTNight") {
                    res <- -3 + 0.15 * getBand(x@data[[i]])
                } else if (i == "SST") {
                    res <- -3 + 0.15 * getBand(do_amsr_average(x@data[["SSTDay"]], x@data[["SSTNight"]]))
                } else if (i == "LFwindDay") {
                    res <- 0.2 * getBand(x@data[[i]])
                } else if (i == "LFwindNight") {
                    res <- 0.2 * getBand(x@data[[i]])
                } else if (i == "LFwind") {
                    res <- 0.2 * getBand(do_amsr_average(x@data[["LFwindDay"]], x@data[["LFwindNight"]]))
                } else if (i == "MFwindDay") {
                    res <- 0.2 * getBand(x@data[[i]])
                } else if (i == "MFwindNight") {
                    res <- 0.2 * getBand(x@data[[i]])
                } else if (i == "MFwind") {
                    res <- 0.2 * getBand(do_amsr_average(x@data[["MFwindDay"]], x@data[["MFwindNight"]]))
                } else if (i == "vaporDay") {
                    res <- 0.3 * getBand(x@data[[i]])
                } else if (i == "vaporNight") {
                    res <- 0.3 * getBand(x@data[[i]])
                } else if (i == "vapor") {
                    res <- 0.3 * getBand(do_amsr_average(x@data[["vaporDay"]], x@data[["vaporNight"]]))
                } else if (i == "cloudDay") {
                    res <- -0.05 + 0.01 * getBand(x@data[[i]])
                } else if (i == "cloudNight") {
                    res <- -0.05 + 0.01 * getBand(x@data[[i]])
                } else if (i == "cloud") {
                    res <- -0.05 + 0.01 * getBand(do_amsr_average(x@data[["cloudDay"]], x@data[["cloudNight"]]))
                } else if (i == "rainDay") {
                    res <- 0.01 * getBand(x@data[[i]])
                } else if (i == "rainNight") {
                    res <- 0.01 * getBand(x@data[[i]])
                } else if (i == "rain") {
                    res <- 0.01 * getBand(do_amsr_average(x@data[["rainDay"]], x@data[["rainNight"]]))
                } else if (i == "data") {
                    return(x@data)
                }
            } else {
                if (i == "timeDay") {
                    res <- x@data[[i]]
                } else if (i == "timeNight") {
                    res <- x@data[[i]]
                } else if (i == "time") {
                    res <- getBand(do_amsr_average(x@data[["timeDay"]], x@data[["timeNight"]]))
                } else if (i == "SSTDay") {
                    res <- x@data[[i]]
                } else if (i == "SSTNight") {
                    res <- x@data[[i]]
                } else if (i == "SST") {
                    res <- do_amsr_average(x@data[["SSTDay"]], x@data[["SSTNight"]])
                } else if (i == "LFwindDay") {
                    res <- x@data[[i]]
                } else if (i == "LFwindNight") {
                    res <- x@data[[i]]
                } else if (i == "LFwind") {
                    res <- do_amsr_average(x@data[["LFwindDay"]], x@data[["LFwindNight"]])
                } else if (i == "MFwindDay") {
                    res <- x@data[[i]]
                } else if (i == "MFwindNight") {
                    res <- x@data[[i]]
                } else if (i == "MFwind") {
                    res <- do_amsr_average(x@data[["MFwindDay"]], x@data[["MFwindNight"]])
                } else if (i == "vaporDay") {
                    res <- x@data[[i]]
                } else if (i == "vaporNight") {
                    res <- x@data[[i]]
                } else if (i == "vapor") {
                    res <- do_amsr_average(x@data[["vaporDay"]], x@data[["vaporNight"]])
                } else if (i == "cloudDay") {
                    res <- x@data[[i]]
                } else if (i == "cloudNight") {
                    res <- x@data[[i]]
                } else if (i == "cloud") {
                    res <- do_amsr_average(x@data[["cloudDay"]], x@data[["cloudNight"]])
                } else if (i == "rainDay") {
                    res <- x@data[[i]]
                } else if (i == "rainNight") {
                    res <- x@data[[i]]
                } else if (i == "rain") {
                    res <- do_amsr_average(x@data[["rainDay"]], x@data[["rainNight"]])
                } else if (i == "data") {
                    return(x@data)
                }
            }
        } else if (type == 2L) {
            if (i == "?") {
                return(list(
                    metadata = sort(names(x@metadata)),
                    metadataDerived = NULL,
                    data = sort(names(x@data)),
                    dataDerived = NULL
                ))
            }
            if (grepl("(Day|Night)$", i)) {
                iorig <- i
                i <- gsub("(Day|Night)$", "", i)
                oceDebug(debug, "returning \"", i, "\" for \"", iorig, "\"\n")
            }
            if (i %in% names(x@metadata)) {
                return(x@metadata[[i]])
            } else {
                return(x@data[[i]])
            }
        } else {
            stop("type ", type, " not understood; only types 1 and 2 are handled")
        }
        dim(res) <- dim
        res
    }
) # [[,amsr-method

#' Replace Parts of an amsr Object
#'
#' @param x an [amsr-class] object.
#'
#' @template sub_subsetTemplate
#'
#' @family things related to amsr data
setMethod(
    f = "[[<-",
    signature(x = "amsr", i = "ANY", j = "ANY"),
    definition = function(x, i, j, ..., value) {
        callNextMethod(x = x, i = i, j = j, ... = ..., value = value) # [[<-
    }
)

#' Subset an amsr Object
#'
#' Return a subset of a [amsr-class] object.
#'
#' This function is used to subset data within an [amsr-class]
#' object by `longitude` or by `latitude`.  These two methods cannot
#' be combined in a single call, so two calls are required, as shown
#' in the Example.
#'
#' @param x an [amsr-class] object.
#'
#' @param subset an expression indicating how to subset `x`.
#'
#' @param ... ignored.
#'
#' @return An [amsr-class] object.
#'
#' @examples
#' library(oce)
#' data(amsr) # see ?amsr for how to read and composite such objects
#' sub <- subset(amsr, -75 < longitude & longitude < -45)
#' sub <- subset(sub, 40 < latitude & latitude < 50)
#' plot(sub)
#' data(coastlineWorld)
#' lines(coastlineWorld[["longitude"]], coastlineWorld[["latitude"]])
#'
#' @author Dan Kelley
#'
#' @family things related to amsr data
#' @family functions that subset oce objects
setMethod(
    f = "subset",
    signature = "amsr",
    definition = function(x, subset, ...) { # subset,amsr-method
        dots <- list(...)
        debug <- if ("debug" %in% names(dots)) dots$debug else 0
        oceDebug(debug, "subset,amsr-method() {\n", style = "bold", sep = "", unindent = 1)
        res <- x
        subsetString <- paste(deparse(substitute(expr = subset, env = environment())), collapse = " ")
        type <- amsrType(x)
        if (length(grep("longitude", subsetString))) {
            if (length(grep("latitude", subsetString))) {
                stop("the subset must not contain both longitude and latitude. Call this twice, to combine these")
            }
            keep <- eval(
                expr = substitute(expr = subset, env = environment()),
                envir = data.frame(longitude = x@metadata$longitude), enclos = parent.frame(2)
            )
            oceDebug(debug, "keeping ", sum(keep), " of ", length(keep), " longitudes\n")
            if (type == 1L) {
                for (name in names(res@data)) {
                    oceDebug(debug, "processing ", name, " (type 1)\n")
                    res@data[[name]] <- res[[name, "raw"]][keep, ]
                }
            } else if (type == 2L) {
                for (name in names(res@data)) {
                    oceDebug(debug, "processing ", name, " (type 2)\n")
                    res@data[[name]] <- res[[name]][keep, ]
                }
            } else {
                stop("type ", type, " not understood; only types 1 and 2 are handled")
            }
            res@metadata$longitude <- x@metadata$longitude[keep]
        } else if (length(grep("latitude", subsetString))) {
            if (length(grep("longitude", subsetString))) {
                stop("the subset must not contain both longitude and latitude. Call this twice, to combine these")
            }
            keep <- eval(
                expr = substitute(expr = subset, env = environment()),
                envir = data.frame(latitude = x@metadata$latitude), enclos = parent.frame(2)
            )
            oceDebug(debug, "keeping ", sum(keep), " of ", length(keep), " latitudes\n")
            if (type == 1L) {
                for (name in names(res@data)) {
                    oceDebug(debug, "processing ", name, " (type 1)\n")
                    res@data[[name]] <- x[[name, "raw"]][, keep]
                }
            } else if (type == 2L) {
                for (name in names(res@data)) {
                    oceDebug(debug, "processing ", name, " (type 2)\n")
                    res@data[[name]] <- x[[name]][, keep]
                }
            } else {
                stop("type ", type, " not understood; only types 1 and 2 are handled")
            }

            res@metadata$latitude <- res@metadata$latitude[keep]
        } else {
            stop("may only subset by longitude or latitude")
        }
        res@processingLog <- processingLogAppend(res@processingLog, paste("subset(x, subset=", subsetString, ")", sep = ""))
        oceDebug(debug, "} # subset,amsr-method()\n", style = "bold", sep = "", unindent = 1)
        res
    }
)

#' Plot an amsr Object
#'
#' Plot an image of a component of an [amsr-class] object.
#'
#' In addition to fields named directly in the object, such as `SSTDay` and
#' `SSTNight`, it is also possible to plot computed fields, such as `SST`,
#' which combines the day and night fields.
#'
#' @param x an [amsr-class] object.
#'
#' @param y character value indicating the name of the band to plot; if not provided,
#' `SST` is used; see the documentation for the [amsr-class] class for a list of bands.
#'
#' @param asp optional numerical value giving the aspect ratio for plot.  The
#' default value, `NULL`, means to use an aspect ratio of 1 for world views,
#' and a value computed from `ylim`, if the latter is specified in the
#' `...` argument.
#'
#' @param breaks optional numeric vector of the z values for breaks in the color scheme.
#' If `colormap` is provided, it takes precedence over `breaks` and `col`.
#'
#' @param col optional argument, either a vector of colors corresponding to the breaks, of length
#' 1 less than the number of breaks, or a function specifying colors.
#' If neither `col` or `colormap` is provided, then `col` defaults to
#' [oceColorsTemperature()].
#' If `colormap` is provided, it takes precedence over `breaks` and `col`.
#'
#' @param colormap a specification of the colormap to use, as created
#' with [colormap()].  If `colormap` is NULL, which is the default, then
#' a colormap is created to cover the range of data values, using
#' [oceColorsTemperature] color scheme.
#' If `colormap` is provided, it takes precedence over `breaks` and `col`.
#' See \dQuote{Examples} for an example of using the "turbo" color scheme.
#'
#' @param zlim optional numeric vector of length 2, giving the limits
#' of the plotted quantity.  A reasonable default is computed, if this
#' is not given.
#'
#' @param missingColor optional list specifying colors to use for
#' non-data categories.  If not provided, a default is used.  For
#' type 1, that default is
#' `list(land="papayaWhip", none="lightGray", bad="gray", rain="plum",
#' ice="mediumVioletRed")`.  For type 2, it is
#' `list(coast="gray", land="papayaWhip", noObs="lightGray",
#' seaIce="mediumVioletRed")`.  Any colors may be used in place of these,
#' but the names must match, and all names must be present.
#'
#' @template debugTemplate
#'
#' @param ... extra arguments passed to [imagep()], e.g. to control
#' the view with `xlim` (for longitude) and `ylim` (for latitude).
#'
#' @examples
#' library(oce)
#' data(coastlineWorld)
#' data(amsr) # see ?amsr for how to read and composite such objects
#'
#' # Example 1: plot with default color scheme, oceColorsTemperature()
#' plot(amsr, "SST")
#' lines(coastlineWorld[["longitude"]], coastlineWorld[["latitude"]])
#'
#' # Example 2: 'turbo' color scheme
#' plot(amsr, "SST", col = oceColorsTurbo)
#' lines(coastlineWorld[["longitude"]], coastlineWorld[["latitude"]])
#'
#' @author Dan Kelley
#'
#' @family things related to amsr data
#' @family functions that plot oce data
#'
#' @aliases plot.amsr
setMethod(
    f = "plot",
    signature = signature("amsr"),
    # FIXME: how to let it default on band??
    definition = function(x, y, asp = NULL, # plot,amsr-method
                          breaks, col, colormap, zlim,
                          # FIXME: how do the old-format categories map to new ones?  (They don't
                          # seem to.)  For now, the next argument is just for new-format.
                          missingColor,
                          debug = getOption("oceDebug"), ...) {
        dots <- list(...)
        oceDebug(debug, "plot.amsr(..., y=c(",
            if (missing(y)) "(missing)" else y, ", ...) {\n",
            sep = "", style = "bold", unindent = 1
        )
        zlimGiven <- !missing(zlim)
        type <- amsrType(x)
        oceDebug(debug, "amsr type: ", type, "\n")
        if (missing(y)) {
            y <- "SST"
        }
        lon <- x[["longitude"]]
        lat <- x[["latitude"]]
        # Examine ylim (if asp is not NULL) and also at both xlim and ylim to
        # compute zrange.
        xlim <- dots$xlim
        ylim <- dots$ylim
        if (is.null(asp)) {
            if (!is.null(ylim)) {
                asp <- 1 / cos(pi / 180 * abs(mean(ylim, na.rm = TRUE)))
                oceDebug(debug, "inferred asp=", asp, " from ylim argument\n", sep = "")
            } else {
                asp <- 1 / cos(pi / 180 * abs(mean(lat, na.rm = TRUE)))
                oceDebug(debug, "inferred asp=", asp, " from ylim argument\n", sep = "")
            }
        } else {
            oceDebug(debug, "using supplied asp=", asp, "\n", sep = "")
        }
        z <- x[[y]]
        # Compute zrange for world data, or data narrowed to xlim and ylim.
        if (!is.null(xlim)) {
            if (!is.null(ylim)) {
                oceDebug(debug, "computing range based on z trimmed by xlim and ylim\n")
                zrange <- range(z[xlim[1] <= lon & lon <= xlim[2], ylim[1] <= lat & lat <= ylim[2]], na.rm = TRUE)
            } else {
                oceDebug(debug, "computing range based on z trimmed by xlim alone\n")
                zrange <- range(z[xlim[1] <= lon & lon <= xlim[2], ], na.rm = TRUE)
            }
        } else {
            if (!is.null(ylim)) {
                oceDebug(debug, "computing range based on z trimmed by ylim alone\n")
                zrange <- range(z[, ylim[1] <= lat & lat <= ylim[2]], na.rm = TRUE)
            } else {
                oceDebug(debug, "computing range based on whole-world data\n")
                zrange <- range(z, na.rm = TRUE)
            }
        }
        oceDebug(debug, "zrange: ", paste(zrange, collapse = " to "), "\n")
        # Determine colormap, if not given as an argument.
        if (missing(colormap)) {
            oceDebug(debug, "case 1: 'colormap' not given, so will be computed here\n")
            if (!missing(breaks)) {
                oceDebug(debug, "case 1.1: 'breaks' was specified\n")
                if (debug > 0) {
                    cat("FYI breaks are as follows:\n")
                    print(breaks)
                }
                if (!missing(col)) {
                    oceDebug(debug, "case 1.1.1: computing colormap from specified breaks and specified col\n")
                    colormap <- oce::colormap(zlim = if (zlimGiven) zlim else range(breaks), col = col)
                } else {
                    oceDebug(debug, "case 1.1.2: computing colormap from specified breaks and computed col\n")
                    colormap <- oce::colormap(zlim = if (zlimGiven) zlim else range(breaks), col = oceColorsTemperature)
                }
            } else {
                oceDebug(debug, "case 1.2: 'breaks' was not specified\n")
                if (!missing(col)) {
                    oceDebug(debug, "case 1.2.1: computing colormap from and computed breaks and specified col\n")
                    colormap <- oce::colormap(zlim = if (zlimGiven) zlim else zrange, col = col)
                } else {
                    oceDebug(debug, "case 1.2.2: computing colormap from computed breaks and computed col\n")
                    colormap <- oce::colormap(zlim = if (zlimGiven) zlim else zrange, col = oceColorsTemperature)
                }
            }
        } else {
            oceDebug(debug, "using specified colormap, ignoring breaks and col, whether they were supplied or not\n")
        }
        i <- if ("zlab" %in% names(dots)) {
            oceDebug(debug, "calling imagep() with asp=", asp, ", and zlab=\"", dots$zlab, "\"\n", sep = "")
            imagep(lon, lat, z, colormap = colormap, asp = asp, debug = debug - 1, ...)
        } else {
            oceDebug(debug, "calling imagep() with asp=", asp, ", and no zlab argument\n", sep = "")
            imagep(lon, lat, z, colormap = colormap, zlab = y, asp = asp, debug = debug - 1, ...)
        }
        # Handle missing-data codes by redrawing the (possibly decimated) image. Perhaps
        # imagep() should be able to do this, but imagep() is a long function
        # with a lot of interlocking arguments so I'll start by doing this
        # manually here, and, if I like it, I may extend imagep() later.
        type <- amsrType(x)
        if (missing(missingColor)) {
            if (type == 1L) {
                missingColor <- list(land = "papayaWhip", none = "lightGray", bad = "gray", rain = "plum", ice = "mediumVioletRed")
            } else if (type == 2L) {
                missingColor <- list(coast = "gray", land = "papayaWhip", noObs = "lightGray", seaIce = "mediumVioletRed")
            } else {
                stop("unrecognized amsr type, ", type, " (only 1 and 2 are allowed)")
            }
        } else {
            missingColorLength <- length(missingColor)
            if (type == 1L) {
                if (4 != missingColorLength) {
                    stop("must have 4 elements in the missingColor argument for new-format data")
                }
                if (!identical(sort(names(missingColor)), c("coast", "land", "noObs", "seaIce"))) {
                    stop("missingColor names must be: 'coast', 'land', 'noObs', 'seaIce'")
                }
            } else if (type == 2L) {
                if (5 != missingColorLength) {
                    stop("must have 5 elements in the missingColor argument for old-format data")
                }
                if (!all(sort(names(missingColor)) == sort(c("land", "none", "bad", "ice", "rain")))) {
                    stop("missingColor names must be: 'land', 'none', 'bad', 'ice' and 'rain'")
                }
            } else {
                stop("unrecognized amsr format, ", type, " (only 1 and 2 are allowed)")
            }
        }
        lonDecIndices <- seq(1L, length(lon), by = i$decimate[1])
        latDecIndices <- seq(1L, length(lat), by = i$decimate[2])
        lon <- lon[lonDecIndices]
        lat <- lat[latDecIndices]
        # Masks are stored very differently in type 1 and type 2.
        if (type == 1L) {
            missingColor <- list(
                land = "papayaWhip",
                none = "lightGray",
                bad = "gray",
                rain = "plum",
                ice = "mediumVioletRed"
            )
            codes <- list(
                land = as.raw(255), # land
                none = as.raw(254), # missing data
                bad = as.raw(253), # bad observation
                ice = as.raw(252), # sea ice
                rain = as.raw(251)
            ) # heavy rain
            for (codeName in names(codes)) {
                oceDebug(debug, "adding color for ", codeName, "\n")
                bad <- x[[y, "raw"]][lonDecIndices, latDecIndices] == as.raw(codes[[codeName]])
                image(lon, lat, bad,
                    col = c("transparent", missingColor[[codeName]]), add = TRUE
                )
            }
        } else if (type == 2L) {
            for (mask in c("coastMask", "landMask", "noObsMask", "seaIceMask")) {
                oceDebug(debug, "adding color for ", mask, "\n")
                image(lon, lat, x@data[[mask]][lonDecIndices, latDecIndices],
                    col = c("transparent", missingColor[[gsub("Mask$", "", mask)]]), add = TRUE
                )
            }
        } else {
            stop("type ", type, " not understood; only types 1 and 2 are handled")
        }
        box()
        oceDebug(debug, "} # plot.amsr()\n", sep = "", style = "bold", unindent = 1)
    }
) # plot,amsr-method


#' Download and Cache an amsr File
#'
#' If the file is already present in `destdir`, then it is not
#' downloaded again. The default `destdir` is the present directory,
#' but it probably makes more sense to use something like `"~/data/amsr"`
#' to make it easy for scripts in other directories to use the cached data.
#' The file is downloaded with [download.file()].  Please read the
#' \sQuote{History} section for important details on how [download.amsr()]
#' and also [read.amsr()] have had be altered over the years, to deal
#' with changes in the directory structure and file format on the
#' server from which files are downloaded.
#'
#' @param year,month,day a specification of the desired observation time.
#' There are 3 choices for this specification.  (a) If `year` is an object
#' created by [as.Date()], then that specifies the time, and so `month`
#' and `day` are ignored.  This scheme can be convenient for creating a
#' sequence of images, starting at a particular date, because adding 1
#' to an object of class `Date` increases the time by 1 day, saving
#' the user from having to know how many days are in any given month.
#' (b) If `year` is an integer, then it is taken to be the year, and
#' the user must also specify `month` and `day`, also integers. (c)
#' If `year` is NULL (which is the default), then the focus is set to
#' the most recent date, but this depends on the value of
#' `type` (see next).  If `type` is `"3day"`, `"daily"` or `"weekly"`,
#' or just the first two of them if `type` is `"monthly"`.  If these
#' things are provided, then they just match exactly the values in the
#' sought-after file on the remote server.  If `year` is NULL, then
#' [download.amsr()] constructs a URL that ought to be the most recent
#' available file: 3 days prior to the present date (if `type` is
#' `"3day"` or `"daily"`), the Saturday two weeks prior to the
#' present date (if `type` is `"weekly"`), or two months in the
#' past (if `type` is `"monthly"`).
#'
#' @param destdir A string naming the directory in which to cache the downloaded file.
#' The default is to store in the present directory, but many users find it more
#' helpful to use something like `"~/data/amsr"` for this, to collect all
#' downloaded amsr files in one place.
#'
#' @param server A string naming the server from which data
#' are to be acquired. See \dQuote{History}.
#'
#' @param type character value indicating where to get the data.  This may be
#' `"3day"` (the default), for a composite covering 3 days of observation, which
#' removes most viewing-path and cloud blanks, `"daily"` for a daily reading,
#' `"weekly"` for a composite covering a week, or `"monthly"` for a composite
#' covering a month.  In the `"daily"` case, the data arrays are 3D, with the
#' third dimension representing ascending and descending traces, but in all the
#' other cases, the arrays are 2D.
#'
#' @template debugTemplate
#'
#' @section History:
#' Until 25 March 2017, the default server was
#' `"ftp.ssmi.com/amsr2/bmaps_v07.2"`, but this was changed when the author
#' discovered that this FTP site had been changed to require users to create
#' accounts to register for downloads.  The default was changed to
#' `"http://data.remss.com/amsr2/bmaps_v07.2"` on the named date.
#' This site was found by a web search, but it seems to provide proper data.
#' It is assumed that users will do some checking on the best source.
#'
#' On 23 January 2018, it was noticed that the server-url naming convention
#' had changed, e.g.
#' `http://data.remss.com/amsr2/bmaps_v07.2/y2017/m01/f34_20170114v7.2.gz`
#' becoming
#' `http://data.remss.com/amsr2/bmaps_v08/y2017/m01/f34_20170114v8.gz`
#'
#' On 26 July 2023, it was noticed that the server-url naming convention
#' had changed again, requiring not only the alteration of the default
#' `server` value but also the addition of a new parameter named `type`.
#' Worse yet -- much worse -- the file format is now changed from a gzipped
#' format to a NetCDF format, and this will require a complete rewriting
#' of [read.amsr()].
#'
#' @return `download.amsr` returns a character value holding the full pathname
#' of the downloaded file.
#'
#' @section Sample of Usage:
#' \preformatted{
#' # The download may take up to about a minute.
#' f <- download.amsr(2023, 7, 27, destdir="~/data/amsr")
#' d <- read.amsr(f)
#' plot(d)
#' mtext(d[["filename"]], side=3, line=0, adj=0)
#' }
#'
#' @family functions that download files
#' @family functions that plot oce data
#' @family things related to amsr data
#'
#' @author Dan Kelley
download.amsr <- function(
    year = NULL, month, day, destdir = ".",
    server = "https://data.remss.com/amsr2/ocean/L3/v08.2", type = "3day",
    debug = 0) {
    oceDebug(debug, "download.amsr(type=\"", type, "\", ...) {\n", sep = "", unindent = 1)
    if (!type %in% c("3day", "daily", "weekly", "monthly")) {
        stop("type=\"", type, "\" not permitted; try \"3day\", \"daily\", \"weekly\" or \"monthly\"")
    }
    # If year, month, day not given, default to 3 days ago.
    today <- as.POSIXlt(Sys.Date())
    usingDefaultTime <- is.null(year)
    if (usingDefaultTime) {
        oceDebug(debug, "year is NULL, so a default time will be used\n")
    } else {
        if (inherits(year, "Date")) {
            tmp <- as.POSIXlt(year)
            year <- tmp$year + 1900
            month <- 1L + tmp$mon
            day <- tmp$mday
            oceDebug(debug, "computed year=", year, ", month=", month, ", day=", day, " from a Date object\n")
        } else {
            if (missing(month)) {
                stop("month must be provided, if year is provided")
            }
            year <- as.integer(year)
            month <- as.integer(month)
            if (type %in% c("3day", "daily")) {
                if (missing(day)) {
                    stop("day must be provided for type of '3day' or 'daily'")
                }
                day <- as.integer(day)
            } else {
                day <- 1L # not used later, because weekly and monthly data don't need this
            }
        }
    }
    if (type %in% c("3day", "daily")) {
        # https://data.remss.com/amsr2/ocean/L3/v08.2/3day/2023/RSS_AMSR2_ocean_L3_3day_2023-07-24_v08.2.nc
        # ^                                           ^    ^                       ^    ^    ^  ^
        # server                                      type year                  type year month day
        if (usingDefaultTime) {
            focus <- as.POSIXlt(Sys.Date() - 3L)
            year <- 1900L + focus$year
            month <- 1L + focus$mon
            day <- focus$mday
            oceDebug(debug, "defaulting to year=", year, ", month=", month, " and day=", day, "\n", sep = "")
        } else {
            oceDebug(debug, "user-supplied year=", year, ", month=", month, " and day=", day, "\n", sep = "")
        }
        url <- sprintf(
            "%s/%s/%d/RSS_AMSR2_ocean_L3_%s_%04d-%02d-%02d_v08.2.nc",
            server, type, year, type, year, month, day
        )
    } else if (identical(type, "weekly")) {
        if (usingDefaultTime) {
            # use the Saturday previous to the most recent Saturday
            today <- Sys.Date()
            dayName <- weekdays(today)
            offset <- switch(dayName,
                "Saturday" = 0,
                "Sunday" = 1,
                "Monday" = 2,
                "Tuesday" = 3,
                "Wednesday" = 4,
                "Thursday" = 5,
                "Friday" = 6
            )
            ymd <- format(today - offset - 7L)
            oceDebug(debug, "defaulting to ymd=\"", ymd, "\"\n")
        } else {
            ymd <- sprintf("%4d-%02d-%02d", year, month, day)
            oceDebug(debug, "user-provided ymd=\"", ymd, "\"\n")
        }
        # https://data.remss.com/amsr2/ocean/L3/v08.2/weekly/RSS_AMSR2_ocean_L3_weekly_2023-07-15_v08.2.nc
        # ^                                           ^                            ^    ^
        # server                                      type                       type   ymd
        url <- sprintf(
            "%s/%s/RSS_AMSR2_ocean_L3_%s_%s_v08.2.nc",
            server, type, type, ymd
        )
    } else if (identical(type, "monthly")) {
        # https://data.remss.com/amsr2/ocean/L3/v08.2/monthly/RSS_AMSR2_ocean_L3_monthly_2023-05_v08.2.nc
        # ^                                           ^                            ^    ^    ^
        # server                                      type                       type year month
        # use the month previous to the previous month
        if (usingDefaultTime) {
            year <- 1900L + today$year
            month <- 1L + today$mon
            if (month < 3L) {
                year <- year - 1L
                month <- 12L - 2L + month
            } else {
                month <- month - 2L
            }
            oceDebug(debug, "defaulting to year=", year, ", month=", month, "\n", sep = "")
        } else {
            oceDebug(debug, "user-supplied year=", year, ", month=", month, "\n", sep = "")
        }
        url <- sprintf(
            "%s/%s/RSS_AMSR2_ocean_L3_%s_%04d-%02d_v08.2.nc",
            server, type, type, year, month
        )
    } else {
        # check again (but should not be able to get here)
        stop("type=\"", type, "\" not permitted; try \"3day\", \"daily\", \"weekly\" or \"monthly\"")
    }
    file <- gsub(".*/", "", url)
    oceDebug(debug, "url=\"", url, "\"\n", sep = "")
    oceDebug(debug, "file=\"", file, "\"\n", sep = "")
    destfile <- paste(destdir, file, sep = "/")
    if (file.exists(destfile)) {
        oceDebug(debug, "using existing file \"", destfile, "\"\n", sep = "")
        oceDebug(debug, "} # download.amsr\n", sep = "", style = "bold", unindent = 1)
        return(destfile)
    }
    ok <- try(download.file(url, destfile))
    if (inherits(ok, "try-error")) {
        stop("could not download \"", url, "\" to local file \"", destfile, "\"")
    }
    oceDebug(debug, "} # download.amsr\n", sep = "", unindent = 1)
    destfile
}
#<2023-07-29> download.amsr <- function(year, month, day, destdir=".", server="http://data.remss.com/amsr2/bmaps_v08")
#<2023-07-29> {
#<2023-07-29>     # ftp ftp://ftp.ssmi.com/amsr2/bmaps_v07.2/y2016/m08/f34_20160804v7.2.gz
#<2023-07-29>     if (missing(year) && missing(month)) {
#<2023-07-29>         if (missing(day))
#<2023-07-29>             day <- 3
#<2023-07-29>         day <- abs(day)
#<2023-07-29>         today <- as.POSIXlt(Sys.Date() - day)
#<2023-07-29>         year <- 1900 + today$year
#<2023-07-29>         month <- 1 + today$mon
#<2023-07-29>         day <- today$mday
#<2023-07-29>     }
#<2023-07-29>     year <- as.integer(year)
#<2023-07-29>     month <- as.integer(month)
#<2023-07-29>     day <- as.integer(day)
#<2023-07-29>     destfile <- sprintf("f34_%4d%02d%02dv8.gz", year, month, day)
#<2023-07-29>     destpath <- paste(destdir, destfile, sep="/")
#<2023-07-29>     # example
#<2023-07-29>     # http://data.remss.com/amsr2/bmaps_v07.2/y2015/m11/f34_20151101v7.2.gz
#<2023-07-29>     if (tail(destpath, 1)=="/") { # remove trailing slash
#<2023-07-29>         destpath <- substr(destpath, 1, length(destpath)-1)
#<2023-07-29>     }
#<2023-07-29>     if (0 == length(list.files(path=destdir, pattern=paste("^", destfile, "$", sep="")))) {
#<2023-07-29>         source <- sprintf("%s/y%4d/m%02d/%s", server, year, month, destfile)
#<2023-07-29>         bad <- download.file(source, destfile)
#<2023-07-29>         if (!bad && destdir != ".")
#<2023-07-29>             system(paste("mv", destfile, destpath))
#<2023-07-29>     } else {
#<2023-07-29>         message("Not downloading ", destfile, " because it is already present in ", destdir)
#<2023-07-29>     }
#<2023-07-29>     if (destdir == ".") destfile else destpath
#<2023-07-29> }

#' Read an amsr File
#'
#' Read an amsr file, generating an [amsr-class] object.
#' Two file types are handled: type 1 is from gzipped files that were available
#' until perhaps the year 2022, and type 2 is from NetCDF files that
#' were available afterwards.
#' The type is stored in the `metadata` slot
#' as `type`, and this is detected in other functions relating to
#' [amsr-class] data.  The best way to locate amsr files is to use
#' [download.amsr()], but if this fails, it may be necessary to search
#' the web for a source.
#'
## AMSR files are provided at the FTP site, at
## \code{ftp.ssmi.com/amsr2/bmaps_v07.2} as of April 2021.
## To acquire such files,
## log in as "guest",
## then enter a year-based directory (e.g. `y2016` for the year 2016),
## then enter a month-based directory (e.g. `m08` for August, the 8th
## month), and then download a file for the present date, e.g.
## `f34_20160803v7.2.gz` for August 3rd, 2016. Do not uncompress
## this file, since `read.amsr` can only read the raw files from the server.
## If `read.amsr` reports an error on the number of chunks, try
## downloading a similarly-named file (e.g. in the present example,
## `read.amsr("f34_20160803v7.2_d3d.gz")` will report an error
## about inability to read a 6-chunk file, but
## `read.amsr("f34_20160803v7.2.gz")` will work properly.
#'
#' @param file String indicating the name of a compressed file. See
#' \dQuote{File sources}.
#'
#' @template encodingIgnoredTemplate
#'
#' @param debug A debugging flag, integer.
#'
#' @seealso [plot,amsr-method()] for an example.
#'
#' @author Dan Kelley and Chantelle Layton
#'
#' @family things related to amsr data
read.amsr <- function(file, encoding = NA, debug = getOption("oceDebug")) {
    if (missing(file)) {
        stop("must supply 'file'")
    }
    if (!is.character(file)) {
        stop("file must be a filename")
    }
    if (!file.exists(file)) {
        stop("cannot find file \"", file, "\"")
    }
    if (0L == file.info(file)$size) {
        stop("empty file \"", file, "\"")
    }
    oceDebug(debug, "read.amsr(file=\"", file, "\",", ", debug=", debug, ") {\n", sep = "", unindent = 1)
    isgz <- grepl(".gz$", file)
    isncdf <- grepl(".nc$", file)
    if (!any(c(isgz, isncdf))) { # also rechecked later
        stop("file must end in either \".gz\" or \".nc\"")
    }
    res <- new("amsr")
    filename <- file
    res@metadata$filename <- filename
    if (isgz) {
        oceDebug(debug, "old-style file, with name ending in \".gz\"\n")
        file <- if (length(grep(".*.gz$", filename))) gzfile(filename, "rb") else file(filename, "rb")
        on.exit(close(file))
        # we can hard-code a max size because the satellite data size is not variable
        buf <- readBin(file, what = "raw", n = 50e6, endian = "little")
        nbuf <- length(buf)
        dim <- c(1440, 720)
        nchunks <- nbuf / prod(dim)
        if (nchunks != round(nchunks)) {
            stop("error: the data length ", nbuf, " is not an integral multiple of ", dim[1], "*", dim[2])
        }
        # From an amsr webpage --
        # Each binary data file available from our ftp site consists of fourteen (daily) or
        # six (averaged) 0.25 x 0.25 degree grid (1440,720) byte maps. For daily files,
        # seven daytime, ascending maps in the following order, Time (UTC), Sea Surface
        # Temperature (SST), 10 meter Surface Wind Speed (WSPD-LF), 10 meter Surface
        # Wind Speed (WSPD-MF), Atmospheric Water Vapor (VAPOR), Cloud Liquid Water (CLOUD),
        # and Rain Rate (RAIN), are followed by seven nighttime maps in the same order.
        # Time-Averaged files contain just the geophysical layers in the same order
        # [SST, WSPD-LF, WSPD-MF,VAPOR, CLOUD, RAIN].
        select <- seq.int(1L, prod(dim))
        if (nchunks == 14) {
            oceDebug(debug, "14-chunk amsr file\n")
            timeDay <- buf[select]
            SSTDay <- buf[prod(dim) + select]
            LFwindDay <- buf[2 * prod(dim) + select]
            MFwindDay <- buf[3 * prod(dim) + select]
            vaporDay <- buf[4 * prod(dim) + select]
            cloudDay <- buf[5 * prod(dim) + select]
            rainDay <- buf[6 * prod(dim) + select]
            dim(timeDay) <- dim
            dim(SSTDay) <- dim
            dim(LFwindDay) <- dim
            dim(MFwindDay) <- dim
            dim(vaporDay) <- dim
            dim(cloudDay) <- dim
            dim(rainDay) <- dim

            timeNight <- buf[7 * prod(dim) + select]
            SSTNight <- buf[8 * prod(dim) + select]
            LFwindNight <- buf[9 * prod(dim) + select]
            MFwindNight <- buf[10 * prod(dim) + select]
            vaporNight <- buf[11 * prod(dim) + select]
            cloudNight <- buf[12 * prod(dim) + select]
            rainNight <- buf[13 * prod(dim) + select]
            dim(timeNight) <- dim
            dim(SSTNight) <- dim
            dim(LFwindNight) <- dim
            dim(MFwindNight) <- dim
            dim(vaporNight) <- dim
            dim(cloudNight) <- dim
            dim(rainNight) <- dim
            res@metadata$units$SSTDay <- list(unit = expression(degree * C), scale = "ITS-90")
            res@metadata$units$SSTNight <- list(unit = expression(degree * C), scale = "ITS-90")
            res@metadata$units$LFwindDay <- list(unit = expression(m / s), scale = "")
            res@metadata$units$LFwindNight <- list(unit = expression(m / s), scale = "")
            res@metadata$units$MFwindDay <- list(unit = expression(m / s), scale = "")
            res@metadata$units$MFwindNight <- list(unit = expression(m / s), scale = "")
            res@metadata$units$rainDay <- list(unit = expression(mm / h), scale = "")
            res@metadata$units$rainNight <- list(unit = expression(mm / h), scale = "")
            res@data <- list(
                timeDay = timeDay,
                SSTDay = SSTDay, LFwindDay = LFwindDay, MFwindDay = MFwindDay,
                vaporDay = vaporDay, cloudDay = cloudDay, rainDay = rainDay,
                timeNight = timeNight,
                SSTNight = SSTNight, LFwindNight = LFwindNight, MFwindNight = MFwindNight,
                vaporNight = vaporNight, cloudNight = cloudNight, rainNight = rainNight
            )
            res@metadata$longitude <- 0.25 * 1:dim[1] - 0.125
            res@metadata$latitude <- 0.25 * 1:dim[2] - 90.125
            # rearrange matrices so that Greenwich is near the centre
            for (name in names(res@data)) {
                t <- matrixShiftLongitude(res@data[[name]], res@metadata$longitude)
                res@data[[name]] <- t$m
            }
            res@metadata$longitude <- t$longitude
        } else if (nchunks == 6) {
            stop("Cannot (yet) read 6-chunk data. Please contact the developers if you need this file (and be sure to send the file to them).")
        } else {
            stop(
                "Can only handle 14-chunk data; this file has ",
                nchunks, " chunks. Please contact the developers if you need to read this file."
            )
        }
    } else if (isncdf) {
        if (!requireNamespace("ncdf4", quietly = TRUE)) {
            stop("must install.packages(\"ncdf4\") to read new-style amsr data")
        }
        # > sort(names(a1@data))
        #  [1] "cloudDay"    "cloudNight"  "LFwindDay"   "LFwindNight"
        #  [5] "MFwindDay"   "MFwindNight" "rainDay"     "rainNight"
        #  [9] "SSTDay"      "SSTNight"    "timeDay"     "timeNight"
        # [13] "vaporDay"    "vaporNight"
        oceDebug(debug, "new-style file, with name ending in \".nc\"\n")
        nc <- ncdf4::nc_open(file)
        SST <- ncdf4::ncvar_get(nc, "SST")
        dim <- dim(SST)
        # print(sort(names(nc$var)))
        #  [1] "cloud_liquid_water" "coast_mask"
        #  [3] "land_mask"          "noobs_mask"
        #  [5] "rain_rate"          "sea_ice_mask"
        #  [7] "SST"                "water_vapor"
        #  [9] "wind_speed_AW"      "wind_speed_LF"
        # [11] "wind_speed_MF"
        # Note that fixMatrix() uses the value of longitude, which is in 0-to-360
        # convention; we rewrite as -180-to-180 later, to match oce convention.
        res@metadata$longitude <- 0.25 * 1:dim[1] - 0.125
        res@metadata$latitude <- 0.25 * 1:dim[2] - 90.125
        fixMatrix <- function(name) {
            val <- ncdf4::ncvar_get(nc, name)
            matrixShiftLongitude(val, res@metadata$longitude)
        }
        tmp <- fixMatrix("cloud_liquid_water")
        res@data$cloud <- tmp$m
        longitudeNew <- tmp$longitude
        res@data$rain <- fixMatrix("rain_rate")$m
        res@data$SST <- fixMatrix("SST")$m
        res@data$vapor <- fixMatrix("water_vapor")$m
        res@data$AWwind <- fixMatrix("wind_speed_AW")$m
        res@data$LFwind <- fixMatrix("wind_speed_LF")$m
        res@data$MFwind <- fixMatrix("wind_speed_MF")$m
        res@data$coastMask <- fixMatrix("coast_mask")$m
        res@data$landMask <- fixMatrix("land_mask")$m
        res@data$noObsMask <- fixMatrix("noobs_mask")$m
        res@data$seaIceMask <- fixMatrix("sea_ice_mask")$m
        # finally, we can update longitude
        res@metadata$longitude <- longitudeNew
    } else {
        stop("file must end in either \".gz\" or \".nc\"")
    }
    res@metadata$spacecraft <- "amsr"
    res@metadata$type <- if ("SSTDay" %in% names(res@data)) 1L else 2L
    res@metadata$units <- list(
        longitude = list(unit = expression(degree * E), scale = ""),
        latitude = list(unit = expression(degree * N), scale = ""),
        SST = list(unit = expression(degree * C), scale = ""),
        AWwind = list(unit = expression(m / s), scale = ""), # all weater
        LFwind = list(unit = expression(m / s), scale = ""), # low-frequency 10.65 to 36.5 GHz
        MFwind = list(unit = expression(m / s), scale = ""), # medium-frequency 18.7 to 36.5 GHz
        vapor = list(unit = expression(kg / m^2), scale = ""),
        cloud = list(unit = expression(kg / m^2), scale = ""),
        rain = list(unit = expression(mm / hr), scale = "")
    )
    res@processingLog <- processingLogAppend(
        res@processingLog,
        paste0("read.amsr(file=\"", filename, "\")")
    )
    oceDebug(debug, "} # read.amsr()\n", sep = "", unindent = 1)
    res
}

#' Create a Composite of amsr Satellite Data
#'
#' Form averages for each item in the `data` slot of the supplied objects,
#' taking into account the bad-data codes.
#'
#' If none of the objects has good
#' data at any particular pixel (i.e. particular latitude and longitude),
#' the resultant will have the bad-data code of the last item in the argument
#' list.
#' The metadata in the result are taken directly from the metadata of the
#' final argument, except that the filename is set to a comma-separated list
#' of the component filenames.
#'
#' @param object An [amsr-class] object.
#'
#' @param ... Other amsr objects.
#'
#' @family things related to amsr data
#'
#' @template compositeTemplate
setMethod(
    "composite",
    c(object = "amsr"),
    function(object, ...) {
        dots <- list(...)
        ndots <- length(dots)
        if (ndots < 1) {
            stop("need more than one argument")
        }
        for (idot in 1:ndots) {
            if (!inherits(dots[[idot]], "amsr")) {
                stop("argument ", 1 + idot, " does not inherit from 'amsr'")
            }
        }
        # inherit most of the metadata from the last argument
        res <- dots[[ndots]]
        filenames <- object[["filename"]]
        for (idot in 1:ndots) {
            filenames <- paste(filenames, ",", dots[[idot]][["filename"]], sep = "")
        }
        dim <- dim(object@data[[1]])
        # 2023-09-08 code was rewritten because the file format has changed
        # significantly.  The new format permits the work to be done quickly
        # in R, without having to drop down to C++ to examine every pixel
        # at the byte level to see if it matches one of the bad-data codes.
        dataNames <- names(object@data)
        dataNames <- dataNames[!grepl("Mask$", dataNames)] # don't average masks
        for (name in dataNames) {
            bad <- with(object@data, landMask | coastMask | seaIceMask | noObsMask)
            sum <- array(0.0, dim = dim)
            count <- array(0L, dim = dim)
            # start filling arrays up
            tmp <- object@data[[name]]
            bad <- bad | is.na(tmp)
            tmp[bad] <- 0.0
            sum <- sum + tmp
            count <- count + !bad
            for (idot in seq_len(ndots)) {
                bad <- with(dots[[idot]]@data, landMask | coastMask | seaIceMask | noObsMask)
                tmp <- dots[[idot]]@data[[name]]
                bad <- bad | is.na(tmp)
                tmp[bad] <- 0.0
                sum <- sum + tmp
                count <- count + !bad
            }
            # finally, compute average
            sum <- sum / count
            sum[!is.finite(sum)] <- NA
            res@data[[name]] <- sum
        }
        # Now handle masks.  If *any* of the images has a non-TRUE mask,
        # then we have at least some valid data, so we turn the mask off.
        # I handle all masks this way, including landMask, which seems
        # (in my very limited tests) to be the same across images.  I would
        # have guessed that the coastMask would be identical also, but
        # that's not the case; perhaps clouds are being flagged as
        # coast data.
        landOK <- !object@data$landMask
        coastOK <- !object@data$coastMask
        noObsOK <- !object@data$noObsMask
        seaIceOK <- !object@data$seaIceMask
        for (dot in dots) {
            landOK <- landOK | !dot@data$landMask
            coastOK <- coastOK | !dot@data$coastMask
            noObsOK <- noObsOK | !dot@data$noObsMask
            seaIceOK <- seaIceOK | !dot@data$seaIceMask
        }
        res@data$landMask <- !landOK
        res@data$coastMask <- !coastOK
        res@data$noObsMask <- !noObsOK
        res@data$seaIceMask <- !seaIceOK
        # construct filename to indicate what the constituents were
        res@metadata$filename <- filenames
        res
    }
)
