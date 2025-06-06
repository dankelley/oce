# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Read a NetCDF File
#'
#' Read the contents of a NetCDF file, saving the information in a basic
#' [oce-class] object.  Since NetCDF files can hold any manner of data,
#' `read.netcdf()` might be used as a first step in the construction of another
#' object of a specialized class, perhaps [ctd-class], [topo-class], etc.  As
#' explained in \dQuote{Details}, the `renamer` argument can facilitate this
#' work.  More work is required to move flags from the `data` slot of the
#' result to the `metadata` slot, and this is illustrated in Example 3.
#'
## ease such later construction, `read.netcdf()` can rename variables, if the
## `renamer` parameter is set appropriately.
## and move data-quality entries to
## the `flags` element of the `metadata` slot (if the `qualityPrefix` is set
## appropriately).
#'
#' By default, the names of the data variables are not changed from those in the
#' data file.  This can be confusing to users who are unfamiliar with the naming
#' scheme used in a particular file, and so `read.netcdf()` has a parameter
#' named `renamer` with which the user can provide a translation key to go from
#' names in the NetCDF file to more standard oce names (like `salinity`).  See
#' \dQuote{Examples} to see how this works, for a particular file that follows
#' the NERC/BODC convention for naming variables.
#'
#' Unlike more specialized functions such as [read.ctd.sbe()], `read.netcdf()`
#' does not try to associate data-quality flags with the corresponding data
#' items. This is because, in the files examined by the author, there is no
#' clear pattern in the names.  For example, the test file referred to
#' in the \dQuote{Examples} section (which is not supplied with this package)
#' has three variables that relate to temperature, namely `"TEMPS901"`,
#' `"TEMPP901"`, and `"TEMPPR01"`.  Given common naming conventions, a quality
#' flag variable is normally constructed by prepending the letter `"Q"` to
#' the name of another variable.  Although there are no such names in this
#' dataset, it *does* have something called `"QTEMP_01"` that one might guess
#' to be a temperature-quality flag.  Based on this (and similar)
#' assumptions, Example 3 shows how to move data-quality variables from
#' the `data` slot of the returned object to the `metadata` slot,
#' which is where oce expects to find it, for further processing of
#' data-quality flags.
#'
#' In this same example file, there are some data fields that contain strings
#' that evidently provide calibration and other information on some of
#' the sensors.  Example 3 shows how to move these things from the `data`
#' slot to the `metadata` slot.
#'
#' Readers might wonder why the renaming and moving of material from the
#' `data` slot to the `metadata` slot is not done by `read.netcdf()` itself.
#' The answer is that these things apply only to files of the type being
#' dealt with in this example.  The NetCDF format can hold a very wide variety
#' of information, and so the basic behaviour of `read.netcdf()` is just to
#' read the data items (things called `var` by functions in the `ncdf4`
#' package, which `read.netcdf()` uses to read the file) and store them
#' in the `data` slot.  In most cases, it is simply up to the user to
#' decide what to do with the information.
#'
#' Finally, it should be noted that `read.netcdf()` tries to get some
#' common metadata elements from global attributes in the NetCDF file.
#' These include `Longitude`, `Latitude`, `Ship` and `Cruise`, all
#' of which are renamed in lower-case and stored in the `metadata` slot,
#' in accordance with oce convention.
#'
#' @param file character value specifying the name of a NetCDF file.
#'
#' @param ... ignored
#'
#' @template encodingIgnoredTemplate
#'
#' @param renamer function used to rename variables that are read from the file,
#' or NULL (which is the default) to avoid such renaming.  See \dQuote{Details}.
#'
## @param flagPrefix character value indicating the prefix character(s) that
## indicate that a variable is a quality flag for another variable. If this is
## NULL, no attempt is made to infer whether data items are measured values or
## quality flags. If `flagPrefix` is not NULL, then an attempt is made to infer
## data quality flags based on name.  See \dQuote{Details}.
#'
#' @template debugTemplate
#'
#' @return
#' An [oce-class] object with `var` elements from the NetCDF file stored in
#' the `data` slot. See the \dQuote{Examples} for hints on renaming the
#' elements, and moving some of them to the `metadata` slot.
#'
#' @examples
#' \dontrun{
#' # Download the file.  (This may break if the server changes.)
#' file <- tempfile(fileext = ".nc")
#' url <- paste0(
#'     "https://cioosatlantic.ca/erddap/files/",
#'     "bio_maritimes_region_ecosystem_survey_ctd/",
#'     "Maritimes%20Region%20Ecosystem%20Survey%20Summer/",
#'     "2023/CTD_CAR2023011_001_496780_DN.ODF.nc"
#' )
#' download.file(url, file)
#'
#' # Example 1: read without translating names
#' d <- read.netcdf(file)
#' summary(d)
#'
#' # Example 2: as Example 1, but translate (some) names
#' d <- read.netcdf(file, renamer = bodcNames2oceNames)
#' summary(d)
#'
#' # Example 3: as Example 2, but handle some flags that were
#' # noticed in this particular file.  See Details for more
#' # notes on variable names.  Note that the following code
#' # only deals with the first instance of a variable, e.g.
#' # temperature, and not temperature2 or temperature3.
#' # (This is of little consequence, since all 3 of the temperatures
#' # are identical.)
#' d <- read.netcdf(file, renamer = bodcNames2oceNames)
#' # Looking within the NetCDF file indicates that the built-in
#' # scheme for DFO files is appropriate here.
#' d <- initializeFlagScheme(d, name = "DFO")
#' # Move some data elements to the `metadata@flags` list,
#' # so they can be used for flag-handling operations. Some
#' # guesses had to be made on the name mapping (see Details).
#' flags <- list(
#'     QALTB_01 = "heightAboveBottom",
#'     QCPHLPR01 = "cholorophyll-a",
#'     QCNDC_01 = "conductivity",
#'     QDOXY_01 = "oxygen",
#'     QOXYV_01 = "oxygenVoltage",
#'     QPOTM_01 = "theta",
#'     QPRES_01 = "pressure",
#'     QPSAL_01 = "salinity",
#'     QPSAR_01 = "downwellingIrradiance",
#'     QSIGP_01 = "sigmaTheta",
#'     QTEMP_01 = "temperature"
#' )
#' for (i in seq_along(flags)) {
#'     varName <- flags[[i]]
#'     flagName <- names(flags)[i]
#'     # cat("fileName=", varName, ", flagName=", flagName, "\n", sep="")
#'     d@metadata$flags[[varName]] <- d[[flagName]] # move
#'     d@data[[flagName]] <- NULL # delete original
#' }
#' # For this group of files, it appears that sensor metadata are
#' # stored with particular names, e.g. "TemperatureSensor". The
#' # following moves these from the data slot to the metadata slot.
#' dataNames <- names(d@data)
#' for (sensorName in dataNames[grep("Sensor$", dataNames)]) {
#'     d@metadata[[sensorName]] <- d@data[[sensorName]]
#'     d@data[[sensorName]] <- NULL
#' }
#' summary(d)
#' # Display information about the temperator sensor
#' cat("Temperature Sensor\n")
#' if (require("jsonlite")) {
#'     str(jsonlite::fromJSON(d[["TemperatureSensor"]]))
#' }
#'
#' # Finally, remove the downloaded file, according to CRAN
#' # policy regarding downloads in documentation examples.
#' file.remove(file)
#' }
#'
#' @references
#' 1. Data variable vocabulary used by NERC/BODC.
#' <http://vocab.nerc.ac.uk/collection/P01/current/>
#'
#' 2. CIOOS Atlantic ERDDAP server entry for Bedford Institute measurements in
#' the waters of Maritime Canada.
#' <https://cioosatlantic.ca/erddap/files/bio_maritimes_region_ecosystem_survey_ctd/>
#' (Link tested 2024-09-21.)
#'
#' @author Dan Kelley
# read.netcdf <- function(file, ..., encoding = NA, renamer = NULL, flagPrefix = NULL, debug = getOption("oceDebug")) {
read.netcdf <- function(file, ..., encoding = NA, renamer = NULL, debug = getOption("oceDebug")) {
    if (missing(file)) {
        stop("must supply 'file'")
    }
    if (is.character(file)) {
        if (!file.exists(file)) {
            stop("cannot find file \"", file, "\"")
        }
        if (0L == file.info(file)$size) {
            stop("empty file \"", file, "\"")
        }
    }
    if (!requireNamespace("ncdf4", quietly = TRUE)) {
        stop("must install.packages(\"ncdf4\") to read netcdf data")
    }
    oceDebug(debug, "read.netcdf() START\n", unindent = 1)
    nc <- ncdf4::nc_open(file)
    res <- new("oce")
    varNames <- names(nc$var)
    if ("time" %in% names(nc$dim)) {
        varNames <- c(varNames, "time")
    }
    # set up dno (data names original)
    dno <- list()
    oceNames <- if (!is.null(renamer)) renamer(varNames) else varNames
    for (i in seq_along(varNames)) {
        dno[oceNames[i]] <- varNames[i]
    }
    res@metadata$dataNamesOriginal <- dno
    for (i in seq_along(varNames)) {
        # oceDebug(debug, "varNames[", i, "]=\"", varNames[i], "\"\n", sep = "")
        if (grepl("^history_", varNames[i], ignore.case = TRUE)) {
            next
        }
        units <- ncdf4::ncatt_get(nc, varNames[i], "units")
        scale <- ncdf4::ncatt_get(nc, varNames[i], "scale")
        isUnixTime <- FALSE
        if (units$hasatt) {
            if (grepl("^days since", units$value)) {
                res@metadata$units[[oceNames[i]]] <- list(
                    unit = expression(day),
                    scale = gsub("days ", "", units$value)
                )
            } else if (grepl("seconds since 1970-01-01", units$value)) {
                res@metadata$units[[oceNames[i]]] <- list(unit = expression(), scale = "")
                isUnixTime <- TRUE
            } else if (units$value == "1e-3" && scale$hasatt && scale$value == "PSS-78") {
                # CIOOS salinities have unit="1e-3" and e.g. scale="PSS-78"
                # FIXME: handle other salinity scales
                res@metadata$units[[oceNames[i]]] <- list(unit = expression(), scale = scale$value)
            } else if (units$value == "degree Celsius") {
                res@metadata$units[[oceNames[i]]] <- list(
                    unit = expression(degree * C),
                    scale = if (scale$hasatt) scale$value else ""
                )
            } else if (units$value == "dimensionless") {
                res@metadata$units[[oceNames[i]]] <- list(
                    unit = expression(),
                    scale = if (scale$hasatt) scale$value else ""
                )
            } else if (units$value == "uA") {
                res@metadata$units[[oceNames[i]]] <- list(
                    unit = expression(mu * A),
                    scale = if (scale$hasatt) scale$value else ""
                )
            } else if (units$value == "meter") {
                res@metadata$units[[oceNames[i]]] <- list(
                    unit = expression(m),
                    scale = ""
                )
            } else if (units$value == "meter second-1") {
                res@metadata$units[[oceNames[i]]] <- list(
                    unit = expression(m / s),
                    scale = ""
                )
            } else if (units$value == "MicroEinsteins m-2 s-1") {
                res@metadata$units[[oceNames[i]]] <- list(
                    unit = expression(mu * Einstein / m^2 / s),
                    scale = if (scale$hasatt) scale$value else ""
                )
            } else if (units$value == "mbar") {
                res@metadata$units[[oceNames[i]]] <- list(
                    unit = expression(mbar),
                    scale = ""
                )
            } else if (units$value == "hPa") {
                res@metadata$units[[oceNames[i]]] <- list(
                    unit = expression(hPa),
                    scale = ""
                )
            } else if (units$value == "NTU") {
                res@metadata$units[[oceNames[i]]] <- list(
                    unit = expression(NTU),
                    scale = if (scale$hasatt) scale$value else ""
                )
            } else if (units$value == "pH units") {
                res@metadata$units[[oceNames[i]]] <- list(
                    unit = expression(pH),
                    scale = if (scale$hasatt) scale$value else ""
                )
            } else {
                # Tell as.unit() to return NULL if unknown, so we can do
                # more logic here, where we know the scale, etc.
                oceunit <- as.unit(units$value, NULL)
                if (is.null(oceunit)) {
                    warning(
                        "\"", oceNames[i], "\" unit \"", units$value,
                        "\" is not recognized; please report an issue"
                    )
                    res@metadata$units[[oceNames[i]]] <- list(unit = units$value, scale = "")
                } else {
                    res@metadata$units[[oceNames[i]]] <- oceunit
                }
            }
        }
        # oceDebug(debug, "reading \"", varNames[i], "\"\n", sep = "")
        item <- ncdf4::ncvar_get(nc, varNames[i])
        if (is.array(item) && 1L == length(dim(item))) { # 1D array converted to vector
            item <- as.vector(item)
        }
        # message("DAN i=", i, ", oceNames[i]='", oceNames[i], "'")
        if (isUnixTime) {
            res@data[[oceNames[i]]] <- numberAsPOSIXct(item, tz = "UTC")
            oceDebug(debug, "converted ", varNames[i], " (of length ", length(item), ") to POSIX and saved in @data slot\n", sep = "")
        } else if (identical(tolower(oceNames[i]), "station")) {
            res@metadata[["station"]] <- trimws(item[1])
            oceDebug(debug, "saved ", varNames[i], " (of length ", length(item), ") in @metadata slot\n", sep = "")
        } else {
            res@data[[oceNames[i]]] <- item
            oceDebug(debug, "saved ", varNames[i], " (of length ", length(item), ") in @data slot\n", sep = "")
        }
    }

    # Try to get some global attributes.
    # Inelegantly permit first letter lower-case or upper-case
    if (ncdf4::ncatt_get(nc, 0, "Longitude")$hasatt) {
        res@metadata$longitude <- ncdf4::ncatt_get(nc, 0, "Longitude")$value
    }
    if (ncdf4::ncatt_get(nc, 0, "longitude")$hasatt) {
        res@metadata$longitude <- ncdf4::ncatt_get(nc, 0, "longitude")$value
    }
    if (ncdf4::ncatt_get(nc, 0, "Latitude")$hasatt) {
        res@metadata$latitude <- ncdf4::ncatt_get(nc, 0, "Latitude")$value
    }
    if (ncdf4::ncatt_get(nc, 0, "latitude")$hasatt) {
        res@metadata$latitude <- ncdf4::ncatt_get(nc, 0, "latitude")$value
    }
    if (ncdf4::ncatt_get(nc, 0, "Station")$hasatt) {
        res@metadata$station <- ncdf4::ncatt_get(nc, 0, "Station")$value
    }
    if (ncdf4::ncatt_get(nc, 0, "station")$hasatt) {
        res@metadata$station <- ncdf4::ncatt_get(nc, 0, "station")$value
    }
    if (ncdf4::ncatt_get(nc, 0, "Ship")$hasatt) {
        res@metadata$ship <- ncdf4::ncatt_get(nc, 0, "Ship")$value
    }
    if (ncdf4::ncatt_get(nc, 0, "ship")$hasatt) {
        res@metadata$ship <- ncdf4::ncatt_get(nc, 0, "ship")$value
    }
    if (ncdf4::ncatt_get(nc, 0, "Cruise")$hasatt) {
        res@metadata$cruise <- ncdf4::ncatt_get(nc, 0, "Cruise")$value
    }
    if (ncdf4::ncatt_get(nc, 0, "cruise")$hasatt) {
        res@metadata$cruise <- ncdf4::ncatt_get(nc, 0, "cruise")$value
    }
    if (ncdf4::ncatt_get(nc, 0, "time")$hasatt) {
        res@metadata$time <- ncdf4::ncatt_get(nc, 0, "time")$value
    }

    # This flag scheme is certainly broken, and so it is commented out.  It's
    # just some testing I was doing.  But then I realized the real problem:
    # there is no way to do this by prefix.  For example, my sample file has
    # 3 temperature variants ("TEMPS901", "TEMPP901", and "TEMPPR01")
    # but the flag is called "QTEMP_01" ... so I don't see any renaming
    # pattern that will tell me which temperature is being flagged. It's
    # the same problem with oxygen.  There is a flag "QOXYV_01" which
    # I am guessing (because of the V) is for "OXYOCPVL01", but that's just
    # a guess.  And later I see "QDOXY_01", and am guessing that might
    # refer to "DOXYZZ01", but that's just a guess.
    # <broken, not worth fixing> # Handle flags
    # <broken, not worth fixing> if (!is.null(flagPrefix)) {
    # <broken, not worth fixing>     message("FIXME: handle flagPrefix")
    # <broken, not worth fixing>     dno <- res@metadata$dataNamesOriginal # save some typing
    # <broken, not worth fixing>     cat(vectorShow(head(dno)))
    # <broken, not worth fixing>     wflag <- grep(paste0("^", flagPrefix), dno)
    # <broken, not worth fixing>     cat(vectorShow(head(wflag), n = -1))
    # <broken, not worth fixing>     cat(vectorShow(dno[head(wflag)], n = -1))
    # <broken, not worth fixing>     A <- gsub(paste0("^", flagPrefix), "", names(dno)[wflag])
    # <broken, not worth fixing>     for (a in A) {
    # <broken, not worth fixing>         cat("a=", a, "\n")
    # <broken, not worth fixing>         ww <- which(a == as.character(dno))
    # <broken, not worth fixing>         cat("ww=", ww, "\n")
    # <broken, not worth fixing>     }
    # <broken, not worth fixing>     browser()
    # <broken, not worth fixing>     #?print(gsub(paste0("^", flagPrefix), "", dno))
    # <broken, not worth fixing>     #wvar <- grep(gsub(paste0("^", flagPrefix), "", dno))
    # <broken, not worth fixing>     #cat(vectorShow(wvar))
    # <broken, not worth fixing>     #cat(vectorShow(dno[w]))
    # <broken, not worth fixing>     #cat(vectorShow(names(dno)[w]))
    # <broken, not worth fixing>     #cat(vectorShow(gsub(paste0("^", flagPrefix), "", dno[w])))
    # <broken, not worth fixing>     #cat(vectorShow(gsub(paste0("^", flagPrefix), "", names(dno)[w])))
    # <broken, not worth fixing> }
    res@processingLog <- processingLogAppend(res@processingLog, paste("read.netcdf(\"", file, "\")", sep = ""))
    oceDebug(debug, "END read.netcdf()\n", unindent = 1)
    res
}

#' Print an overview of the contents of a NetCDF file
#'
#' This prints an overview of the variables in a NetCDF file, along with time,
#' longitude and latitude, if the last three are stored as dimensions.
#'
#' An alternative to this is to examine what is printed by running
#' [ncdf4::nc_open()] with the given filename, but this output can be a bit
#' confusing to read, especially for files that have voluminous global
#' attributes.
#'
#' @param file character value naming a NetCDF file.
#'
#' @param level integer indicating the level of the overview.  If `level` is 1,
#' which is the default, then a list holding the names of variables and
#' dimensions is printed (and returned, silently).  If `level` is 2, then more
#' information is printed.
#'
#' @template debugTemplate
#'
#' @return
#' If `level` is 1, then the printed list of variables and dimensions
#' is returned.  Otherwise, more information is printed, but
#' the return value is the same as for `level` 1.
#'
#' @examples
#' library(oce)
#' # Use an Argo file that comes with the package.
#' if (requireNamespace("ncdf4") && requireNamespace("jsonlite")) {
#'     file <- system.file("extdata/D4902337_219.nc", package = "oce")
#'     netcdfTOC(file)
#' }
#'
#' @importFrom ncdf4 ncatt_get nc_close nc_open
#' @importFrom jsonlite fromJSON
#'
#' @author Dan Kelley
netcdfTOC <- function(file, level = 1L, debug = getOption("oceDebug")) {
    if (!requireNamespace("ncdf4", quietly = TRUE)) {
        stop("must install.packages(\"ncdf4\") to read netcdf data")
    }
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
        stop("must install.packages(\"jsonlite\") to read netcdf data")
    }
    nc <- ncdf4::nc_open(file)
    rval <- list(variables = names(nc$var), dimensions = names(nc$dim))
    if (level == 1L) {
        ncdf4::nc_close(nc)
        return(rval)
    } else if (level == 2L) {
        # print(sort(varnames))
        # get info on time and location, if those things are in the file
        varnames <- rval$variables
        if ("time" %in% rval$dimensions) {
            varnames <- c(varnames, "time")
        }
        if ("longitude" %in% rval$dimensions) {
            varnames <- c(varnames, "longitude")
        }
        if ("latitude" %in% rval$dimensions) {
            varnames <- c(varnames, "latitude")
        }
        # print(sort(varnames))
        for (varname in varnames) {
            cat("\n## ", varname, "\n\n", sep = "")
            # Capture (and discard) the output to avoid warnings
            # about 8-byte variables, which occur in some oceanographic
            # files for things like CTD scan counts.
            capture.output(att <- ncdf4::ncatt_get(nc, varname))
            for (item in names(att)) {
                if (item == "calibration") {
                    cat("* ", item, ":\n", sep = "")
                    cat(str(jsonlite::fromJSON(att[[item]])))
                } else {
                    cat(paste("* ", item, ": ",
                        paste(att[[item]], collapse = " "), "\n",
                        sep = ""
                    ))
                }
            }
            sample <- head(ncdf4::ncvar_get(nc, varname), 3L)
            if (is.numeric(sample)) {
                cat("* sample: ", paste(head(sample), collapse = " "), "\n")
            }
        }
        ncdf4::nc_close(nc)
        return(invisible(rval))
    } else {
        stop("level must be 1 or 2, not ", level, " as supplied")
    }
}
