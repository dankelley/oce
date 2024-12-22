# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Create a ctd Object from an rsk Object
#'
#' A new `ctd` object is assembled from the contents of the `rsk` object.
#' The data and metadata are mostly unchanged, with an important exception: the
#' `pressure` item in the `data` slot may altered, because `rsk`
#' instruments measure total pressure, not sea pressure; see \dQuote{Details}.
#'
#' The `pressureType` element of the
#' `metadata` of `rsk` objects defines the pressure type, and this controls
#' how pressure is set up in the returned object. If `object@@metadata$pressureType`
#' is `"absolute"` (or `NULL`) then the resultant pressure will be adjusted
#' to make it into `"sea"` pressure. To do this, the value of
#' `object@@metadata$pressureAtmospheric` is inspected. If this is present, then
#' it is subtracted from `pressure`. If this is missing, then
#' standard pressure (10.1325 dbar) will be subtracted. At this stage, the
#' pressure should be near zero at the ocean surface, but some additional adjustment
#' might be necessary, and this may be indicated by setting the argument `pressureAtmospheric` to
#' a non-zero value to be subtracted from pressure.
#'
#' @param x an [rsk-class] object.
#'
#' @param pressureAtmospheric A numerical value (a constant or a vector),
#' that is subtracted from the pressure in `object` before storing it in the return value.
#'
#' @param longitude numerical value of longitude, in degrees East.
#'
#' @param latitude numerical value of latitude, in degrees North.
#'
#' @param ship optional string containing the ship from which the observations were made.
#'
#' @param cruise optional string containing a cruise identifier.
#'
#' @param station optional string containing a station identifier.
#'
#' @param deploymentType character string indicating the type of deployment (see
#' [as.ctd()]).
#'
#' @template debugTemplate
rsk2ctd <- function(
    x, pressureAtmospheric = 0, longitude = NULL, latitude = NULL,
    ship = NULL, cruise = NULL, station = NULL, deploymentType = NULL,
    debug = getOption("oceDebug")) {
    oceDebug(debug, "rsk2ctd(...) START\n", sep = "", unindent = 1)
    res <- new("ctd")
    res@metadata <- x@metadata
    # The user may have already inserted some metadata, even if read.rsk() didn't, so
    # we have to take care of two cases in deciding on some things. The procedure is
    # to use the argument to rsk2ctd if one is given, otherwise to use the value already
    # in x@metadata, otherwise to set a default that matches as.ctd().
    res@metadata$longitude <- if (!is.null(longitude)) {
        longitude
    } else {
        if (is.null(res@metadata$longitude)) {
            NA
        } else {
            res@metadata$longitude
        }
    }
    res@metadata$latitude <- if (!is.null(latitude)) {
        latitude
    } else {
        if (is.null(res@metadata$latitude)) {
            NA
        } else {
            res@metadata$latitude
        }
    }
    res@metadata$ship <- if (!is.null(ship)) {
        ship
    } else {
        if (is.null(res@metadata$ship)) {
            ""
        } else {
            res@metadata$ship
        }
    }
    res@metadata$cruise <- if (!is.null(cruise)) {
        cruise
    } else {
        if (is.null(res@metadata$cruise)) {
            ""
        } else {
            res@metadata$cruise
        }
    }
    res@metadata$station <- if (!is.null(station)) {
        station
    } else {
        if (is.null(res@metadata$station)) {
            ""
        } else {
            res@metadata$station
        }
    }
    res@metadata$deploymentType <- if (!is.null(deploymentType)) {
        deploymentType
    } else {
        if (is.null(res@metadata$deploymentType)) {
            "unknown"
        } else {
            res@metadata$deploymentType
        }
    }
    # We start by copying the data, but we may need to do some fancy footwork
    # for pressure, because RBR devices store absolute pressure, not the sea
    # pressure that we have in CTD objects.
    res@data <- x@data
    if (!("pressure" %in% names(res@data))) {
        stop("there is no pressure in this rsk object, so it cannot be converted to a ctd object")
    }
    pressureAtmosphericStandard <- 10.1325
    if (is.null(x@metadata$pressureType)) {
        oceDebug(debug, "metadata$pressureType is NULL so guessing absolute pressure:\n")
        warning("rsk object lacks metadata$pressureType; assuming absolute and subtracting standard atm pressure to get sea pressure")
        res@data$pressure <- x@data$pressure - pressureAtmosphericStandard
        res@metadata$units$pressure$scale <- "sea"
        res@metadata$dataNamesOriginal[substr(res@metadata$dataNamesOriginal, 1, 4) == "pres"] <- ""
        res@processingLog <- processingLogAppend(
            res@processingLog,
            paste("subtracted 10.1325dbar (std atm) from pressure\n")
        )
    } else {
        # subtract atm pressure, if it has not already been subtracted
        oceDebug(debug, "metadata$pressureType is not NULL\n")
        if ("sea" != substr(x@metadata$pressureType, 1, 3)) {
            oceDebug(debug, "must convert from absolute pressure to sea pressure\n")
            if (!("pressureAtmospheric" %in% names(x@metadata))) {
                oceDebug(debug, "pressure is 'absolute'; subtracting std atm 10.1325 dbar\n")
                res@data$pressure <- x@data$pressure - pressureAtmosphericStandard
                res@metadata$units$pressure$scale <- "sea"
                res@metadata$dataNamesOriginal[substr(res@metadata$dataNamesOriginal, 1, 4) == "pres"] <- ""
                res@processingLog <- processingLogAppend(
                    res@processingLog,
                    paste("subtracted", pressureAtmosphericStandard, "dbar (std atm) from absolute pressure to get sea pressure")
                )
                oceDebug(debug, "subtracted std atm pressure from pressure\n")
            } else {
                res@data$pressure <- x@data$pressure - x@metadata$pressureAtmospheric
                res@metadata$units$pressure$scale <- "sea"
                res@metadata$dataNamesOriginal[substr(res@metadata$dataNamesOriginal, 1, 4) == "pres"] <- ""
                res@processingLog <- processingLogAppend(
                    res@processingLog,
                    paste(
                        "subtracted",
                        x@metadata$pressureAtmospheric,
                        "dbar from absolute pressure to get sea pressure"
                    )
                )
                oceDebug(debug, "subtracted", x@metadata$pressureAtmospheric, "dbar from pressure\n")
            }
        }
    }
    # Now we have sea pressure (if the rsk was set up correctly for the above to
    # work right), so we can adjust a second time, if the user changed from the
    # default of pressureAtmospheric=0.
    if (pressureAtmospheric[1] != 0) {
        res@data$pressure <- res@data$pressure - pressureAtmospheric
        oceDebug(debug, "subtracted", pressureAtmospheric, "dbar from pressure")
    }
    res@processingLog <- processingLogAppend(
        res@processingLog,
        paste("subtracted", pressureAtmospheric, "dbar from sea pressure")
    )
    if (!("salinity" %in% names(x@data))) {
        C <- x[["conductivity"]]
        if (is.null(C)) {
            stop("objects must have salinity or conductivity to be converted to CTD form")
        }
        unit <- as.character(x@metadata$units$conductivity$unit)
        if (0 == length(unit)) {
            S <- swSCTp(x[["conductivity"]], x[["temperature"]], res[["pressure"]])
            res@processingLog <- processingLogAppend(res@processingLog, "calculating salinity based on conductivity in (assumed) ratio units")
        } else if (unit == "uS/cm") {
            S <- swSCTp(x[["conductivity"]] / 42914.0, x[["temperature"]], res[["pressure"]])
            res@processingLog <- processingLogAppend(res@processingLog, "calculating salinity based on conductivity in uS/cm")
        } else if (unit == "mS/cm") {
            S <- swSCTp(x[["conductivity"]] / 42.914, x[["temperature"]], res[["pressure"]])
            res@processingLog <- processingLogAppend(res@processingLog, "calculating salinity based on conductivity in mS/cm")
        } else if (unit == "S/m") {
            S <- swSCTp(x[["conductivity"]] / 4.2914, x[["temperature"]], res[["pressure"]])
            res@processingLog <- processingLogAppend(res@processingLog, "calculating salinity based on conductivity in S/m")
        } else {
            stop("unrecognized conductivity unit '", unit, "'; only uS/cm, mS/cm and S/m are handled")
        }
        res <- oceSetData(res,
            name = "salinity", value = S,
            unit = list(unit = expression(), scale = "PSS-78")
        )
    }
    oceDebug(debug, "END rsk2ctd()\n", sep = "", unindent = 1)
    res@processingLog <- processingLogAppend(
        res@processingLog,
        paste("rsk2ctd(..., pressureAtmospheric=", pressureAtmospheric, ", debug)\n",
            sep = "", collapse = ""
        )
    )
    res
}
