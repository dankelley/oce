# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Coerce Data Into a ctd Object
#'
#' Assemble data into a [ctd-class] object.  This function is complicated
#' (spanning approximately 600 lines of code) because it tries to handle many
#' special cases, and tries to make sensible defaults for unspecified
#' parameters.  If odd results are found, users might find it helpful to call
#' this function with the first parameter being a simple vector of Practical
#' Salinity values, in which case the processing of the other arguments is
#' relatively straightforward.
#'
#' The following sections provide an some notes on how `as.ctd()` handles
#' certain object types given as the first parameter.
#'
#' **Converting argo objects**
#'
#' If the `salinity` argument is an object of [argo-class], then that
#' object is dismantled and reassembled as a [ctd-class] object in ways that
#' are mostly straightforward, although the handling of time depends
#' on the information in the original NetCDF data file that was used
#' by [read.argo()] to create the [argo-class] object.
#'
#' All Argo data files contain an item called `juld` from which the profile
#' time can be computed, and some also contain an additional item named `MTIME`,
#' from which the times of individual measurements can also be computed.  Both
#' cases are handled by `as.ctd()`, using a scheme outlined in
#' Note 4 of the Details section of the [read.argo()] documentation.
#'
#' **Converting rsk objects**
#'
#' If the `salinity` argument is an object of [rsk-class],
#' then `as.ctd` passes it,
#' `pressureAtmospheric`,
#' `longitude`,
#' and `latitude`
#' to [rsk2ctd()], which builds the ctd object that is
#' returned by `as.ctd`. The other arguments to `as.ctd`
#' are ignored in this instance, because `rsk` objects already
#' contain their information. If required, any data or metadata
#' element can be added to the value returned by `as.ctd`
#' using [oceSetData()] or [oceSetMetadata()],
#' respectively.
#'
#' The returned [rsk-class] object contains pressure in a form that
#' may need to be adjusted, because `rsk` objects may contain
#' either absolute pressure or sea pressure. This adjustment is handled
#' automatically by `as.ctd`, by examination of the metadata item
#' named `pressureType` (described in the documentation for
#' [read.rsk()]).  Once the sea pressure is determined,
#' adjustments may be made with the `pressureAtmospheric` argument,
#' although in that case it is better considered a pressure adjustment
#' than the atmospheric pressure.
#'
#' [rsk-class] objects may store sea pressure or absolute pressure (the
#' sum of sea pressure and atmospheric pressure), depending on how the object was
#' created with [as.rsk()] or [read.rsk()].  However,
#' [ctd-class] objects store sea pressure, which is needed for
#' plotting, calculating density, etc. This poses no difficulties, however,
#' because `as.ctd` automatically converts absolute pressure to sea pressure,
#' if the metadata in the [rsk-class] object indicates that this is
#' appropriate. Further alteration of the pressure can be accomplished with the
#' `pressureAtmospheric` argument, as noted above.
#'
#' @param salinity may be (1) a numeric vector holding Practical Salinity,
#' (2) a list or data frame holding `salinity` and other
#' hydrographic variables or (3) an `oce-class` object that holds
#' hydrographic information. If `salinity` is not provided,
#' then `conductivity` must be provided, so that [swSCTp()]
#' can be used to compute salinity.
#'
#' @param temperature a numeric vector containing *in-situ* temperature in
#' \eqn{^\circ}{deg}C on the ITS-90 scale; see \dQuote{Temperature units} in the
#' documentation for [swRho()].
#'
#' @param pressure a numeric vector containing sea pressure values, in decibars.
#' Typically, this vector has the same length as `salinity` and `temperature`,
#' but it also possible to supply just one value, which will be repeated
#' to get the right length. Note that `as.ctd()` stores the
#' sum of `pressure` and `pressureAtmospheric` in the returned object,
#' although the default value for `pressureAtmospheric` is zero, so
#' in the default case, `pressure` is stored directly.
#'
#' @param conductivity an optional numeric vector containing electrical
#' conductivity ratio through the water column. To convert from raw conductivity
#' in milliSeimens per centimeter divide by 42.914 to get conductivity ratio
#' (see Culkin and Smith, 1980).
#'
#' @param scan optional numeric vector holding scan number.  If not provided,
#' this is set to [seq_along]`(salinity)`.
#'
#' @param time optional vector of times of observation.
#'
#' @param units an optional list containing units.  If not supplied,
#' defaults are set for `pressure`, `temperature`, `salinity`,
#' and `conductivity`. Since these are simply guesses, users
#' are advised strongly to supply `units`. See \dQuote{Examples}.
#'
#' @param flags if supplied, this is a [list] containing data-quality
#' flags. The elements of this list must have names that match the data
#' provided to the object.
#'
#' @param missingValue optional missing value, indicating data that should be
#' taken as `NA`. Set to `NULL` to turn off this feature.
#'
#' @param type optional type of CTD, e.g. "SBE"
#'
#' @param serialNumber optional serial number of instrument
#'
#' @param ship optional string containing the ship from which the observations were made.
#'
#' @param cruise optional string containing a cruise identifier.
#'
#' @param station optional string containing a station identifier.
#'
#' @param startTime optional indication of the start time for the profile,
#' which is used in some several plotting functions.  This is best given as a
#' [POSIXt] time, but it may also be a character string
#' that can be converted to a time with [as.POSIXct()],
#' using `UTC` as the timezone.
#'
#' @param longitude optional numerical value containing longitude in decimal
#' degrees, positive in the eastern hemisphere. If this is a single number,
#' then it is stored in the `metadata` slot of the returned value; if it
#' is a vector of numbers, then they are stored in the `data` slot. If
#' `longitude' is not provided (i.e. if it is NULL, the default), then
#' `as.ctd()' tries to find it from the first parameter, if it is a list,
#' or an [oce-class] object.
#'
#' @param latitude similar to `longitude`.  Positive in the northern
#' hemisphere.
#'
#' @param deploymentType character string indicating the type of deployment. Use
#' `"unknown"` if this is not known, `"profile"` for a profile (in
#' which the data were acquired during a downcast, while the device was lowered
#' into the water column, perhaps also including an upcast; `"moored"` if
#' the device is installed on a fixed mooring, `"thermosalinograph"` (or
#' `"tsg"`) if the device is mounted on a moving vessel, to record
#' near-surface properties, or `"towyo"` if the device is repeatedly
#' lowered and raised.
#'
#' @param pressureAtmospheric A numerical value (a constant or a vector),
#' that is subtracted from pressure before storing it in the return value.
#' (This altered pressure is also used in calculating `salinity`, if
#' that is to be computed from `conductivity`, etc., using
#' [swSCTp()]; see `salinity` above.)
#'
#' @param sampleInterval optional numerical value indicating the time between
#' samples in the profile.
#'
#' @param profile optional positive integer specifying the number of the profile
#' to extract from an object that has data in matrices, such as for some
#' `argo` objects. Currently the `profile` argument is only utilized for
#' [argo-class] objects.
#'
# 1108 @param src optional string indicating data source.
#'
#' @template debugTemplate
#'
#' @return A [ctd-class] object.
#'
#' @examples
#' library(oce)
#' # 1. fake data, with default units
#' pressure <- 1:50
#' temperature <- 10 - tanh((pressure - 20) / 5) + 0.02 * rnorm(50)
#' salinity <- 34 + 0.5 * tanh((pressure - 20) / 5) + 0.01 * rnorm(50)
#' ctd <- as.ctd(salinity, temperature, pressure)
#' # Add a new column
#' fluo <- 5 * exp(-pressure / 20)
#' ctd <- oceSetData(ctd,
#'     name = "fluorescence", value = fluo,
#'     unit = list(unit = expression(mg / m^3), scale = "")
#' )
#' summary(ctd)
#'
#' # 2. fake data, with supplied units (which are the defaults, actually)
#' ctd <- as.ctd(salinity, temperature, pressure,
#'     units = list(
#'         salinity = list(unit = expression(), scale = "PSS-78"),
#'         temperature = list(unit = expression(degree * C), scale = "ITS-90"),
#'         pressure = list(unit = expression(dbar), scale = "")
#'     )
#' )
#'
#' @references
#'
#' 1. Culkin, F., and Norman D. Smith, 1980. Determination of the
#' concentration of potassium chloride solution having the same electrical
#' conductivity, at 15 C and infinite frequency, as standard seawater of salinity
#' 35.0000 ppt (Chlorinity 19.37394 ppt). *IEEE Journal of Oceanic
#' Engineering*, volume **5**, pages 22-23.
#'
#' @author Dan Kelley, with help from Clark Richards
#'
#' @family things related to ctd data
as.ctd <- function(
    salinity, temperature = NULL, pressure = NULL, conductivity = NULL,
    scan = NULL, time = NULL, units = NULL, flags = NULL, missingValue = NULL,
    type = "", serialNumber = NULL, ship = NULL, cruise = NULL, station = NULL,
    startTime = NULL, longitude = NULL, latitude = NULL, deploymentType = "unknown",
    pressureAtmospheric = 0, sampleInterval = NULL, profile = NULL,
    debug = getOption("oceDebug")) {
    oceDebug(debug, "as.ctd(...) START\n", sep = "", unindent = 1)
    unitsGiven <- !is.null(units)
    salinityGiven <- !missing(salinity)

    # Already a ctd-class object.
    if (salinityGiven && inherits(salinity, "ctd")) {
        oceDebug(debug, "first parameter is 'ctd-class', so returning as-is\n")
        oceDebug(debug, "END as.ctd()\n", sep = "", unindent = 1)
        return(salinity)
    }
    # An rsk-class object.
    if (salinityGiven && inherits(salinity, "rsk")) {
        oceDebug(debug, "first parameter is 'rsk-class', so converting with rsk2ctd()\n")
        res <- rsk2ctd(salinity,
            pressureAtmospheric = pressureAtmospheric,
            longitude = longitude,
            latitude = latitude,
            ship = ship,
            station = station,
            cruise = cruise,
            deploymentType = deploymentType,
            debug = debug - 1
        )
        oceDebug(debug, "END as.ctd() with rsk object as first parameter\n", sep = "", unindent = 1)
        return(res)
    } # end of rsk case
    # Not an rsk object.
    res <- new("ctd")
    if (!is.null(startTime) && is.character(startTime)) {
        startTime <- as.POSIXct(startTime, tz = "UTC")
    }
    if (!salinityGiven) {
        if (!missing(conductivity) && !missing(temperature) && !missing(pressure)) {
            salinity <- swSCTp(conductivity = conductivity, temperature = temperature, pressure = pressure)
            oceDebug(debug, "computed salinity using swSCTp() with stated conductivity, temperature and pressure\n")
        } else {
            stop("if salinity is not provided, conductivity, temperature and pressure must all be provided")
        }
    }
    filename <- ""
    waterDepth <- NA
    ounits <- NULL # replace with metadata$units if first parameter is an oce object
    # First parameter is an oce object
    if (inherits(salinity, "oce")) {
        oceDebug(debug, "first parameter is an oce object, so ignoring some other arguments\n")
        # dataNamesOriginal <- list()
        o <- salinity
        d <- o@data
        m <- o@metadata
        if ("longitude" %in% names(d) && is.null(longitude)) {
            longitude <- d$longitude
            oceDebug(debug, "inferred longitude from data slot of first parameter\n")
        } else if ("longitude" %in% names(m) && is.null(longitude)) {
            longitude <- m$longitude
            oceDebug(debug, "inferred longitude from metadata slot of first parameter\n")
        }
        if ("latitude" %in% names(d) && is.null(latitude)) {
            latitude <- d$latitude
            oceDebug(debug, "inferred latitude from data slot of first parameter\n")
        } else if ("latitude" %in% names(m) && is.null(latitude)) {
            latitude <- m$latitude
            oceDebug(debug, "inferred latitude from metadata slot of first parameter\n")
        }
        res@metadata$dataNamesOriginal <- m$dataNamesOriginal
        res@metadata$flagScheme <- m$flagScheme
        ounits <- o@metadata$units
        dnames <- names(d)
        mnames <- names(m)
        filename <- if ("filename" %in% mnames) m$filename else ""
        # Check whether various things have been specified in this call to
        # as.ctd().  If not, try looking for values in the metadata or data of
        # the first parameter.
        if (is.null(ship) && "ship" %in% mnames) {
            ship <- m$ship
        }
        if (is.null(cruise) && "cruise" %in% mnames) {
            cruise <- m$cruise
        }
        if (is.null(station) && "station" %in% mnames) {
            station <- m$station
        }
        if (is.null(startTime)) {
            if ("startTime" %in% mnames) {
                startTime <- as.POSIXct(m$startTime, tz = "UTC")
            } else if ("time" %in% mnames) {
                startTime <- as.POSIXct(m$time, tz = "UTC")
            }
        }
        # If longitude and latitude are not given as parameters, try to infer
        # them from the first parameter.  (Note that case is ignored.)
        if (is.null(longitude)) {
            if ("longitude" %in% dnames) {
                longitude <- d$longitude
                d$longitude <- NULL
                oceDebug(debug, "inferred longitude from first parameter's data slot\n")
            } else if ("LONGITUDE" %in% dnames) {
                longitude <- d$LONGITUDE
                d$LONGITUDE <- NULL
                oceDebug(debug, "inferred longitude from first parameter's data slot\n")
            } else if ("longitude" %in% mnames) {
                longitude <- m$longitude
                m$longitude <- NULL
                oceDebug(debug, "inferred longitude from first parameter's metadata slot\n")
            } else if ("LONGITUDE" %in% mnames) {
                longitude <- m$LONGITUDE
                m$LONGITUDE <- NULL
                oceDebug(debug, "inferred longitude from first parameter's metadata slot\n")
            }
        }
        if (is.null(latitude)) {
            if ("latitude" %in% dnames) {
                latitude <- d$latitude
                d$latitude <- NULL
                oceDebug(debug, "inferred latitude from first parameter's data slot\n")
            } else if ("LATITUDE" %in% dnames) {
                latitude <- d$LATITUDE
                d$LATITUDE <- NULL
                oceDebug(debug, "inferred latitude from first parameter's data slot\n")
            } else if ("latitude" %in% mnames) {
                latitude <- m$latitude
                m$latitude <- NULL
                oceDebug(debug, "inferred latitude from first parameter's metadata slot\n")
            } else if ("LATITUDE" %in% mnames) {
                latitude <- m$LATITUDE
                m$LATITUDE <- NULL
                oceDebug(debug, "inferred latitude from first parameter's metadata slot\n")
            }
        }
        if (is.null(serialNumber) && "serialNumber" %in% mnames) {
            serialNumber <- m$serialNumber
        }
        if (is.null(sampleInterval) && "sampleInterval" %in% mnames) {
            sampleInterval <- m$sampleInterval
        }
        if (is.na(waterDepth) && "waterDepth" %in% mnames) {
            waterDepth <- m$waterDepth
        }
        # Rename nicknames as oce names, updating dataNamesOriginal as required.
        if ("PSAL" %in% dnames && !("salinity" %in% dnames)) {
            names(d) <- gsub("PSAL", "salinity", names(d))
            res@metadata$dataNamesOriginal[["salinity"]] <- "PSAL"
        }
        if ("TEMP" %in% dnames && !("temperature" %in% dnames)) {
            names(d) <- gsub("TEMP", "temperature", names(d))
            res@metadata$dataNamesOriginal[["temperature"]] <- "TEMP"
        }
        if ("PRES" %in% dnames && !("pressure" %in% dnames)) {
            names(d) <- gsub("PRES", "pressure", names(d))
            res@metadata$dataNamesOriginal[["pressure"]] <- "PRES"
        }
        if (pressureAtmospheric != 0.0) {
            len <- length(pressureAtmospheric)
            if (1 != len && len != length(pressure)) {
                stop("length(pressureAtmospheric) must be 1 or length(pressure)")
            }
            d$pressure <- d$pressure - pressureAtmospheric
        }
        salinity <- d$salinity
        res@metadata$units <- o@metadata$units
        if (!is.null(flags)) {
            res@metadata$flags <- flags
        }
        if (!is.null(o@metadata$flags)) {
            res@metadata$flags <- o@metadata$flags
        }
        # Store QC items (as read.netcdf() can store) as flags.
        QCitems <- grep("QC$", names(d))
        # print(names(d))
        # print(names(d[QCitems]))
        for (QCitem in QCitems) {
            QCName <- names(d)[QCitem]
            # message(vectorShow(QCName))
            flagName <- gsub("[_]{0,1}QC", "", QCName)
            # message(vectorShow(flagName))
            res@metadata$flags[[flagName]] <- d[[QCName]]
            oceDebug(debug, "data$", QCName, " moved to metadata$flags$", flagName, "\n", sep = "")
        }
        d[QCitems] <- NULL
        # Handle other special variables
        # 1108 res@metadata$pressureType <- pressureType
        # copy relevant metadata.
        # 1108 if ("date" %in% mnames) res@metadata$date <- o@metadata$date
        # if any changes here, update oce.R @ ODF_CTD_LINK {
        res@metadata$startTime <- startTime
        if ("eventNumber" %in% mnames) {
            res@metadata$eventNumber <- o@metadata$eventNumber
        }
        if ("eventQualifier" %in% mnames) {
            res@metadata$eventQualifier <- o@metadata$eventQualifier
        }
        # } ODF_CTD_LINK
        if ("deploymentType" %in% mnames) {
            res@metadata$deploymentType <- o@metadata$deploymentType
        }
        if ("filename" %in% mnames) {
            res@metadata$filename <- o@metadata$filename
        }
        if ("serialNumber" %in% mnames) {
            res@metadata$serialNumber <- o@metadata$serialNumber
        }
        if ("ship" %in% mnames) {
            res@metadata$ship <- o@metadata$ship
        }
        if ("cruise" %in% mnames) {
            res@metadata$cruise <- o@metadata$cruise
        }
        if ("station" %in% mnames) {
            res@metadata$station <- o@metadata$station
        }
        if ("scientist" %in% mnames) {
            res@metadata$scientist <- o@metadata$scientist
        }
        if ("units" %in% mnames) {
            # the usual case
            # res@metadata$units$conductivity <- o@metadata$units$conductivity
            # res@metadata$units$temperature <- o@metadata$units$temperature
            res@metadata$units <- o@metadata$units
        } else {
            # permit a case that existed for a few months in 2015
            if ("conductivityUnit" %in% mnames) {
                res@metadata$units$conductivity <- o@metadata$conductivityUnit
            }
            if ("temperatureUnit" %in% mnames) {
                res@metadata$units$temperature <- o@metadata$temperatureUnit
            }
        }
        if ("pressureType" %in% mnames) {
            res@metadata$pressureType <- o@metadata$pressureType
        }
        # if ("scan" %in% dnames) res@data$scan <- d$scan
        # FIXME: time goes into metadata or data ... does that make sense?
        if ("time" %in% dnames) {
            if (length(d$time) > 1) {
                res@data$time <- d$time
            } else {
                res@metadata$time <- d$time
            }
        }
        # if ("quality" %in% dnames) res@data$quality <- d$quality
        # if ("oxygen" %in% dnames) res@data$oxygen <- d$oxygen
        # if ("nitrate" %in% dnames) res@data$nitrate <- d$nitrate
        # if ("nitrite" %in% dnames) res@data$nitrite <- d$nitrite
        # if ("phosphate" %in% dnames) res@data$phosphate <- d$phosphate
        # if ("silicate" %in% dnames) res@data$silicate <- d$silicate
        if (inherits(o, "argo")) {
            oceDebug(debug, "first parameter is an argo-class object\n")
            if (is.null(profile)) {
                profile <- 1
                if (dim(o[["pressure"]])[2] != 1) {
                    warning("using just column 1 of matrix data; use the \"profile\" argument to select a specific profile")
                }
            }
            if (!is.numeric(profile) || length(profile) != 1 || profile < 1) {
                stop("profile must be a positive integer")
            }
            # Pull out the startTime
            if (length(res@metadata$startTime) > 1) {
                res@metadata$startTime <- res@metadata$startTime[profile]
            }
            # Have to handle lon and lat before going into @data because it was
            # already pulled out above:
            if (length(longitude) > 1) {
                longitude <- longitude[profile]
            }
            if (length(latitude) > 1) {
                latitude <- latitude[profile]
            }
            # Extract metadata item, by profile. FIXME: do we capture all cases?
            getMetadataItem <- function(o, item, profile) {
                value <- o@metadata[[item]]
                if (is.vector(value)) {
                    value[profile]
                } else if (is.matrix(value)) {
                    value[, profile]
                } else {
                    value
                }
            }
            # FIXME: for issue 2270, add more items to copy
            for (item in c("id", "dataMode")) {
                res@metadata[item] <- getMetadataItem(o, item, profile)
                oceDebug(debug, "  copied metadata$", item, "\n", sep = "")
            }
            # Convert data items from array to vector
            for (field in names(d)) {
                dataInField <- d[[field]]
                oceDebug(debug, "  handling data$", field, "\n", sep = "")
                # in argo objects there are both matrix (temperature,
                # salinity, etc) and vector (time, latitude, etc)
                # data fields. For the former we want to extract the
                # single column. For the longitude and latitude we extract a
                # single value.  We do that also for time, storing the single
                # value in the `metadata` slot, *unless* MTIME is
                # defined, in which case we can construct a full time vector and
                # place it in the 'data` slot.
                #<old>if (field == "time") { # apparently POSIXct class things aren't vectors
                #<old>    message("TIME")
                #<old>    if ("MTIME" %in% names(d)) {
                #<old>        # FIXME: is profile correct in the next line?
                #<old>        res@data$time <- d[["time"]][profile] + d[["MTIME"]][profile] * 86400.0
                #<old>    } else {
                #<old>        res@metadata$time <- d[[field]][profile]
                #<old>    }
                #<old>}
                if (field == "mtime") {
                    if (is.matrix(dataInField)) {
                        ncol <- ncol(d[[field]])
                        if (profile > ncol) {
                            stop("profile cannot exceed ", ncol, " for a data matrix with ", ncol, " columns")
                        }
                        res@data[[field]] <- as.vector(d[[field]][, profile])
                    } else {
                        res@data[[field]] <- as.vector(d[[field]])
                    }
                    res@data$time <- res@metadata$startTime + 86400 * res@data$mtime
                } else if (is.vector(dataInField)) {
                    ncol <- length(d[[field]])
                    if (profile > ncol) {
                        stop("profile cannot exceed ", ncol, " for a data matrix with ", ncol, " columns")
                    }
                    ## I'm not sure the below for lon/lat will ever be
                    ## triggered, as lon and lat are pulled out separately above
                    ## and should be in the `d` variable at this point
                    if (field %in% c("longitude", "latitude")) {
                        res@metadata[[field]] <- d[[field]][profile]
                    } else {
                        res@data[[field]] <- d[[field]][profile]
                    }
                } else if (is.matrix(dataInField)) {
                    ncol <- ncol(d[[field]])
                    if (profile > ncol) {
                        stop("profile cannot exceed ", ncol, " for a data matrix with ", ncol, " columns")
                    }
                    res@data[[field]] <- d[[field]][, profile]
                } else if (is.array(dataInField)) { # argo can sometimes come out this (odd) way
                    warning("argo data \"", field, "\" converted from 1-D array to 1-col matrix")
                    if (1 == length(dim(d[[field]]))) {
                        d[[field]] <- as.vector(d[[field]])
                    }
                    res@data[[field]] <- d[[field]]
                } else {
                    if (1 == length(dim(d[[field]]))) {
                        d[[field]] <- as.vector(d[[field]])
                    }
                    res@data[[field]] <- d[[field]]
                }
            }
            # Convert flags from array to vector
            if ("flags" %in% names(res@metadata)) {
                for (iflag in seq_along(res@metadata$flags)) {
                    if (is.matrix(res@metadata$flags[[iflag]])) {
                        res@metadata$flags[[iflag]] <- res@metadata$flags[[iflag]][, profile]
                    }
                }
            }
            # end of argo case
        } else { # oce object, not argo
            oceDebug(debug, "x is a general oce object (not ctd, and not argo)\n")
            for (field in names(d)) {
                # print(field)
                if (field != "time") {
                    res@data[[field]] <- d[[field]]
                }
            }
            #<20240825> # whilst looking at issue 2237, I realized that we
            #<20240824> # already inferred longitude and latitude above, before looking
            #<20240824> # at special cases.
            #<20240825> if ("longitude" %in% dnames && "latitude" %in% dnames) {
            #<20240825>     oceDebug(debug, "longitude and latitude are in x@data\n")
            #<20240825>     longitude <- d$longitude
            #<20240825>     latitude <- d$latitude
            #<20240825>     if (length(longitude) != length(latitude)) {
            #<20240825>         stop("lengths of longitude and latitude must match")
            #<20240825>     }
            #<20240825>     if (length(longitude) == length(temperature)) {
            #<20240825>         res@data$longitude <- longitude
            #<20240825>         res@data$latitude <- latitude
            #<20240825>     } else {
            #<20240825>         res@metadata$longitude <- longitude[1]
            #<20240825>         res@metadata$latitude <- latitude[1]
            #<20240825>     }
            #<20240825> } else if ("longitude" %in% mnames && "latitude" %in% mnames) {
            #<20240825>     res@metadata$longitude <- m$longitude
            #<20240825>     res@metadata$latitude <- m$latitude
            #<20240825> }
        }
        res@metadata$deploymentType <- deploymentType
        # res@metadata$dataNamesOriginal <- m$dataNamesOriginal
        # move e.g. salinityFlag from data slot to metadata$flags
        dataNames <- names(res@data)
        flagNameIndices <- grep(".*Flag$", dataNames)
        if (length(flagNameIndices)) {
            for (iflag in flagNameIndices) {
                fname <- gsub("Flag$", "", dataNames[iflag])
                res@metadata$flags[[fname]] <- res@data[[dataNames[iflag]]]
                res@data[[dataNames[iflag]]] <- NULL
            }
        }
        # message("FIXME DAN L1552: longitude=", longitude)
    } else if (is.list(salinity) || is.data.frame(salinity)) {
        # 2. coerce a data-frame or list
        if (length(salinity) == 0) {
            stop("first parameter cannot be a zero-length list or data frame")
        }
        oceDebug(debug, "case 1: first parameter is a list or data frame\n")
        x <- salinity
        if (is.list(x) && inherits(x[[1]], "oce")) {
            oceDebug(debug, "case 1A: list holds oce objects\n")
            # Copy data over
            dataNames <- names(x[[1]]@data)
            oceDebug(debug, "copying data entries: \"", paste(dataNames, collapse = "\", \""), "\"\n", sep = "")
            for (name in dataNames) {
                res@data[[name]] <- unlist(lapply(x, function(xx) xx[[name]]))
            }
            # If longitude and latitude are not in data, the next will copy from metadata (if present there)
            if (!("longitude" %in% dataNames)) {
                res@data$longitude <- unlist(lapply(x, function(xx) rep(xx[["longitude"]], length.out = length(xx[["salinity"]]))))
            }
            if (!("latitude" %in% dataNames)) {
                res@data$latitude <- unlist(lapply(x, function(xx) rep(xx[["latitude"]], length.out = length(xx[["salinity"]]))))
            }
            # Flags
            if ("flags" %in% names(x[[1]]@metadata)) {
                flagNames <- names(x[[1]]@metadata$flags)
                oceDebug(debug, "copying flag entries: '", paste(flagNames, collapse = "', '"), "'\n", sep = "")
                for (name in flagNames) {
                    res@metadata$flags[[name]] <- unlist(lapply(x, function(xx) xx@metadata$flags[[name]]))
                }
                res@metadata$flagScheme <- x[[1]]@metadata$flagScheme
            }
            # Units
            res@metadata$units <- x[[1]]@metadata$units
        } else {
            oceDebug(debug, "case 1B: list made up of non-oce objects\n")
            names <- names(x)
            oceDebug(debug, "names: \"", paste(names, collapse = "\" \""), "\"\n", sep = "")
            # Permit oce-style names or WOCE-style names for the three key variables (FIXME: handle more)
            if (3 == sum(c("salinity", "temperature", "pressure") %in% names)) {
                oceDebug(debug, "found 'salinity', 'temperature' and 'pressure' in names\n")
                res@data$pressure <- x$pressure
                res@data$salinity <- x$salinity
                res@data$temperature <- x$temperature
                res@metadata$units <- units
                # 1108 res@metadata$pressureType <- pressureType
                res@metadata$pressureType <- "sea"
            } else if (3 == sum(c("PSAL", "TEMP", "PRES") %in% names)) {
                oceDebug(debug, "found 'PSAL', 'TEMP' and 'PRES' in names\n")
                res@data$pressure <- x$PRES
                res@data$salinity <- x$PSAL
                res@data$temperature <- x$TEMP
                res@metadata$units <- units
                # 1108 res@metadata$pressureType <- pressureType
                res@metadata$pressureType <- "sea"
            } else {
                stop("the first parameter must contain salinity, temperature, and pressure")
            }
            if (is.null(longitude)) {
                if ("longitude" %in% names) {
                    longitude <- x$longitude
                    oceDebug(debug, "retrieved longitude from first parameter\n")
                }
            }
            if (is.null(latitude)) {
                if ("latitude" %in% names) {
                    latitude <- x$latitude
                    oceDebug(debug, "retrieved latitude from first parameter\n")
                }
            }
            # if ("conductivity" %in% names) res@data$conductivity <- x$conductivity
            if ("COND" %in% names) {
                res@data$conductivity <- x$COND # FIXME accept other WOCE names
            }
            # if ("quality" %in% names) res@data$quality <- x$quality
            # if ("oxygen" %in% names) res@data$oxygen <- x$oxygen
            # if ("nitrate" %in% names) res@data$nitrate <- x$nitrate
            # if ("nitrite" %in% names) res@data$nitrite <- x$nitrite
            # if ("phosphate" %in% names) res@data$phosphate <- x$phosphate
            # if ("silicate" %in% names) res@data$silicate <- x$silicate
            if ("time" %in% names) {
                res@data$time <- x$time
            }
            haveAlready <- names(res@data)
            # add any remaining items
            for (field in names) {
                if (field != "time" && !(field %in% haveAlready)) {
                    res@data[[field]] <- x[[field]]
                }
            }
        }
    } else {
        oceDebug(debug, "case 2: salinity, temperature, pressure (etc) supplied\n")
        # 3. explicit mode
        # 1108 if (missing(temperature) && missing(CT)) stop("must give temperature or CT")
        if (missing(temperature)) {
            stop("must give temperature")
        }
        if (missing(pressure)) {
            stop("must give pressure")
        }
        if (!missing(units)) {
            res@metadata$units <- units
        }
        # 1108 res@metadata$pressureType <- pressureType
        res@metadata$pressureType <- "sea"
        salinity <- as.vector(salinity)
        temperature <- as.vector(temperature)
        pressure <- as.vector(pressure)
        if (!missing(pressureAtmospheric)) {
            pressure <- pressure - pressureAtmospheric
        }
        # 1108 haveSA <- !missing(SA)
        # 1108 haveCT <- !missing(CT)
        # 1108 if (haveSA != haveCT)
        # 1108     stop("SA and CT must both be supplied, if either is")
        # 1108 if (!missing(SA)) {
        # 1108     n <- length(SA)
        # 1108     if (length(CT) != n)
        # 1108         stop("lengths of SA and CT must match")
        # 1108     if (missing(longitude)) {
        # 1108         longitude <- rep(300, n)
        # 1108         latitude <- rep(0, n)
        # 1108         warning("longitude and latitude set to default values, since none given")
        # 1108     }
        # 1108     salinity <- gsw::gsw_SP_from_SA(SA, pressure, longitude, latitude)
        # 1108     temperature <- gsw::gsw_t_from_CT(SA, CT, pressure)
        # 1108 }
        # depths <- max(length(salinity), length(temperature), length(pressure))
        # 2015-01-24: now insist that lengths make sense; only pressure can be mismatched
        salinity <- as.vector(salinity)
        temperature <- as.vector(temperature)
        pressure <- as.vector(pressure)
        nS <- length(salinity)
        nT <- length(temperature)
        np <- length(pressure)
        if (nS != nT) {
            stop("lengths of salinity and temperature must match, but they are ", nS, " and ", nT)
        }
        if (np == 1) {
            pressure <- rep(pressure, nS)
        }
        np <- length(pressure)
        if (nS != np) {
            stop("lengths of salinity and pressure must match, but they are ", nS, " and ", np)
        }
        if (missing(scan)) {
            scan <- seq_along(salinity)
        }
        data <- list(
            scan = scan,
            salinity = salinity,
            temperature = temperature,
            pressure = pressure
        )
        if (!missing(conductivity)) {
            data$conductivity <- as.vector(conductivity)
        }
        # 1108 if (!missing(quality)) data$quality <- quality
        # 1108 if (!missing(oxygen)) data$oxygen <- oxygen
        # 1108 if (!missing(nitrate)) data$nitrate <- nitrate
        # 1108 if (!missing(nitrite)) data$nitrite <- nitrite
        # 1108 if (!missing(phosphate)) data$phosphate <- phosphate
        # 1108 if (!missing(silicate)) data$silicate <- silicate
        if (!missing(time)) {
            data$time <- time
        }
        # Handle missing value code (changes on July 24, 2016 fix issue 1028)
        if (!is.null(missingValue)) {
            for (dname in names(data)) {
                bad <- data[[dname]] == missingValue
                data[[dname]][bad] <- NA
            }
        }
        names <- names(data)
        # labels <- titleCase(names) # paste(toupper(substring(names, 1, 1)), substring(names, 2), sep="")
        if (length(longitude) != length(latitude)) {
            stop(
                "lengths of longitude and latitude must match, but they are ",
                length(longitude), " and ", length(latitude), ", respectively"
            )
        }
        if (1 < length(longitude) && length(longitude) != length(salinity)) {
            stop(
                "lengths of salinity and longitude must match but they are ",
                length(longitude), " and ", length(salinity), ", respectively"
            )
        }
        res@metadata$filename <- filename
        res@metadata$ship <- ship
        res@metadata$cruise <- cruise
        res@metadata$station <- station
        res@metadata$startTime <- startTime
        res@metadata$type <- type
        res@metadata$serialNumber <- serialNumber
        res@metadata$deploymentType <- deploymentType
        # If lon and lat are vectors, place in data, with averages in metadata.
        if (length(latitude) == 1) {
            res@metadata$latitude <- latitude
        } else if (length(latitude) > 1) {
            if (length(latitude) != length(temperature)) {
                stop("lengths of latitude and temperature must match")
            }
            data$latitude <- latitude
        }
        if (length(longitude) == 1) {
            res@metadata$longitude <- longitude
        } else if (length(longitude) > 1) {
            if (length(longitude) != length(temperature)) {
                stop("lengths of longitude and temperature must match")
            }
            data$longitude <- longitude
        }
        res@data <- data
    }
    # message("FIXME DAN L1767: longitude=", longitude)
    if (!is.null(ounits)) {
        oceDebug(debug, "copying units from first parameter\n")
        res@metadata$units <- ounits
    } else if (!unitsGiven) {
        oceDebug(debug, "assuming modern units, since none provided\n")
        # guess on units
        names <- names(res@data)
        if ("salinity" %in% names) {
            res@metadata$units$salinity <- list(unit = expression(), scale = "PSS-78")
        }
        if ("temperature" %in% names) {
            res@metadata$units$temperature <- list(unit = expression(degree * C), scale = "ITS-90")
        }
        if ("pressure" %in% names) {
            res@metadata$units$pressure <- list(unit = expression(dbar), scale = "")
        }
    }
    # the 'units' argument takes precedence over guesses
    dataNames <- names(res@data)
    unitsNames <- names(units)
    if (!is.null(flags)) {
        res@metadata$flags <- flags
    }
    # Default some units (FIXME: this may be a bad idea)
    if (is.null(res@metadata$units)) {
        if ("salinity" %in% dataNames && !("salinity" %in% unitsNames)) {
            res@metadata$units$salinity <- list(unit = expression(), scale = "PSS-78")
        }
        if ("temperature" %in% dataNames && !("temperature" %in% unitsNames)) {
            res@metadata$units$temperature <- list(unit = expression(degree * C), scale = "ITS-90")
        }
        if ("pressure" %in% dataNames && !("pressure" %in% unitsNames)) {
            res@metadata$units$pressure <- list(unit = expression(dbar), scale = "")
        }
    }
    # FIXME: setting waterDepth can have tricky results ... we've had issues with this
    if (is.na(res@metadata$waterDepth) && !is.na(waterDepth)) {
        res@metadata$waterDepth <- waterDepth
    }
    # message("FIXME DAN L1807: longitude=", longitude)
    # Remove lon and lat form metadata, if they are in data. This is so plot()
    # will show multiple stations, as can be the case in converting from
    # multi-station data.
    if (!"longitude" %in% names(res@metadata)) {
        res@metadata$longitude <- longitude
    }
    if (!"latitude" %in% names(res@metadata)) {
        res@metadata$latitude <- latitude
    }
    if ("longitude" %in% names(res@metadata) && "longitude" %in% names(res@data) &&
        "latitude" %in% names(res@metadata) && "latitude" %in% names(res@data)) {
        oceDebug(debug, "retaining longitude and latitude in data slot but not metadata slot\n")
        res@metadata$longitude <- NULL
        res@metadata$latitude <- NULL
    }
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep = "", collapse = ""))
    oceDebug(debug, "END as.ctd()\n", sep = "", unindent = 1)
    res
}
