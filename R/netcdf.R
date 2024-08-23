# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Read a NetCDF File
#'
#' Read a NetCDF file, trying to interpret its contents sensibly.
#'
#' It is important to note that this is a preliminary version of
#' this function, and much about it may change without notice,
#' perhaps until the summer of 2025.
#'
#' By default, the names of the data variables are not changed
#' from those in the data file.  This means that the user
#' has to be aware of the meanings of the names. Specifying
#' a function for the `renamer` parameter provides a way to
#' translate the data names, whilst retaining the original
#' names. For example, setting `renamer=bodcNames2oceNames`
#' will tell `read.netcdf()` to use [bodcNames2oceNames()]
#' to translate at least some of the names in the NERC/BODC
#' vocabulary (Reference 1) to standard oce names. The
#' documentation for [bodcNames2oceNames()] provides more detail
#' on this, including a suggestion for adding user extensions.
#'
#' An attempt is made to find some common metadata from global
#' attributes in the NetCDF file. These attributes include
#' `Longitude`, `Latitude`, `Ship` and `Cruise`.
#' Before they are stored in the metadata, they are converted to
#' lower case, since that is the oce convention.
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
#' @template debugTemplate
#'
#' @return
#' An [oce-class] object.
#'
#' @references
#' 1. Data variable vocabulary used by NERC/BODC.
#' <http://vocab.nerc.ac.uk/collection/P01/current/>
#'
#' 2. CIOOS Atlantic ERDDAP server entry for Bedford Institute measurements in
#' the waters of Maritime Canada.
#' <https://cioosatlantic.ca/erddap/files/bio_maritimes_region_ecosystem_survey_ctd/al>
#'
#' @author Dan Kelley
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
    f <- ncdf4::nc_open(file)
    res <- new("oce")
    fileNames <- names(f$var)
    # set up dno (data names original)
    dno <- list()
    oceNames <- if (!is.null(renamer)) renamer(fileNames) else fileNames
    for (i in seq_along(fileNames)) {
        dno[oceNames[i]] <- fileNames[i]
    }
    res@metadata$dataNamesOriginal <- dno
    for (i in seq_along(fileNames)) {
        oceDebug(debug, "fileNames[", i, "]=\"", fileNames[i], "\"\n", sep = "")
        if (grepl("^history_", fileNames[i], ignore.case = TRUE)) {
            next
        }
        units <- ncdf4::ncatt_get(f, fileNames[i], "units")
        scale <- ncdf4::ncatt_get(f, fileNames[i], "scale")
        isUnixTime <- FALSE
        if (units$hasatt) {
            oceDebug(debug, "  unit=\"", units$value, "\"\n")
            # seconds since 1970-01-01T00:00:00+00:00
            if (grepl("seconds since 1970-01-01", units$value)) {
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
            } else {
                # Tell as.unit() to return NULL if unknown, so we can do
                # more logic here, where we know the scale, etc.
                oceunit <- as.unit(units$value, NULL)
                if (is.null(oceunit)) {
                    warning(
                        "\"", oceNames[i], "\" unit \"", units$value,
                        "\" is not recognized; please report an issue"
                    )
                    res@metadata$units[[oceNames[i]]] <- units$value
                } else {
                    res@metadata$units[[oceNames[i]]] <- oceunit
                }
            }
        }
        item <- ncdf4::ncvar_get(f, fileNames[i])
        if (is.array(item) && 1L == length(dim(item))) { # 1D array converted to vector
            item <- as.vector(item)
        }
        if (isUnixTime) { # FALSE && tolower(name) == "time") {
            oceDebug(
                debug, "    interpreting this as a Unix time of length ", length(item), "\n"
            )
            res@data[[oceNames[i]]] <- numberAsPOSIXct(item, tz = "UTC")
        } else if (tolower(oceNames[i]) == "station") {
            res@metadata[["station"]] <- trimws(item[1])
        } else {
            res@data[[oceNames[i]]] <- item
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
    oceDebug(debug, "END read.netcdf()\n", unindent = 1)
    res
}
