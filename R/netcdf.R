# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Read a NetCDF File
#'
#' Read a netcdf file, trying to interpret its contents sensibly.
#'
#' It is important to note that this is a preliminary version of
#' this function, and much about it may change without notice.
#' Indeed, it may be removed entirely.
#'
#' Below are some features that may be changed.
#'
#' 1. The names of data items are not changed from those in the netcdf
#' file on the assumption that this will offer the least surprise to
#' the user.
#'
#' 2. An attempt is made to find some common metadata from global
#' attributes in the netcdf file. These attributes include
#' `Longitude`, `Latitude`, `Ship` and `Cruise`.
#' Before they are stored in the metadata, they are converted to
#' lower case, since that is the oce convention.
#'
#' @param file the name of a file
#'
#' @template encodingIgnoredTemplate
#'
#' @param ... ignored
#'
#' @template debugTemplate
#'
#' @return
#' An [oce-class] object.
read.netcdf <- function(file, ..., encoding = NA, debug = getOption("oceDebug")) {
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
    oceDebug(debug, "read.netcdf() {\n", unindent = 1)
    f <- ncdf4::nc_open(file)
    res <- new("oce")
    names <- names(f$var)
    for (name in names) {
        oceDebug(debug, "  name=\"", name, "\"\n")
        if (grepl("^history_", name, ignore.case = TRUE)) {
            next
        }
        units <- ncdf4::ncatt_get(f, name, "units")
        if (units$hasatt) {
            res@metadata$units[[name]] <- oce::as.unit(units$value)
        }
        item <- ncdf4::ncvar_get(f, name)
        if (is.array(item) && 1 == length(dim(item))) { # 1D array converted to 1 column matrix
            item <- as.vector(item)
        }
        if (tolower(name) == "time") {
            if (units$hasatt && units$value == "seconds since 1970-01-01 UTC") {
                res@metadata[["time"]] <- numberAsPOSIXct(item[1])
            } else {
                warning("time unit is not understood, so it remains simply numeric")
            }
        } else if (tolower(name) == "station") {
            res@metadata[["station"]] <- trimws(item[1])
        } else {
            res@data[[name]] <- item
        }
    }
    # Try to get some global attributes.
    # Inelegantly permit first letter lower-case or upper-case
    if (ncdf4::ncatt_get(f, 0, "Longitude")$hasatt) {
        res@metadata$longitude <- ncdf4::ncatt_get(f, 0, "Longitude")$value
    }
    if (ncdf4::ncatt_get(f, 0, "longitude")$hasatt) {
        res@metadata$longitude <- ncdf4::ncatt_get(f, 0, "longitude")$value
    }
    if (ncdf4::ncatt_get(f, 0, "Latitude")$hasatt) {
        res@metadata$latitude <- ncdf4::ncatt_get(f, 0, "Latitude")$value
    }
    if (ncdf4::ncatt_get(f, 0, "latitude")$hasatt) {
        res@metadata$latitude <- ncdf4::ncatt_get(f, 0, "latitude")$value
    }
    if (ncdf4::ncatt_get(f, 0, "Station")$hasatt) {
        res@metadata$station <- ncdf4::ncatt_get(f, 0, "Station")$value
    }
    if (ncdf4::ncatt_get(f, 0, "station")$hasatt) {
        res@metadata$station <- ncdf4::ncatt_get(f, 0, "station")$value
    }
    if (ncdf4::ncatt_get(f, 0, "Ship")$hasatt) {
        res@metadata$ship <- ncdf4::ncatt_get(f, 0, "Ship")$value
    }
    if (ncdf4::ncatt_get(f, 0, "ship")$hasatt) {
        res@metadata$ship <- ncdf4::ncatt_get(f, 0, "ship")$value
    }
    if (ncdf4::ncatt_get(f, 0, "Cruise")$hasatt) {
        res@metadata$cruise <- ncdf4::ncatt_get(f, 0, "Cruise")$value
    }
    if (ncdf4::ncatt_get(f, 0, "cruise")$hasatt) {
        res@metadata$cruise <- ncdf4::ncatt_get(f, 0, "cruise")$value
    }
    if (ncdf4::ncatt_get(f, 0, "time")$hasatt) {
        res@metadata$time <- ncdf4::ncatt_get(f, 0, "time")$value
    }
    res@processingLog <- processingLogAppend(res@processingLog, paste("read.netcdf(\"", file, "\")", sep = ""))
    oceDebug(debug, "} # read.netcdf()\n", unindent = 1)
    res
}
