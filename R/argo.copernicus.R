#' Read argo file in Copernicus format
#'
#' This function was added to read a particular file downloaded from the
#' Fleet Monitoring website (Reference 1).  The format was inferred
#' through examination of the file and a brief study of a document
#' (Reference 2) that describes the format.  Not all fields are read
#' by this function; see Reference 3 for a full list and note that
#' the author would be happy to add new entries (but not to spend hours
#' entering then all).
#'
#' @param file A character string giving the name of the file to load.
#'
#' @param debug A flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, or 0 (the default) for silent operation.
#'
#' @param processingLog ignored.
#'
#' @param ... ignored.
#'
#' @references
#' 1. `https://fleetmonitoring.euro-argo.eu/float/4902489`
#'
#' 2. Copernicus Marine In Situ Tac Data Management Team. Copernicus Marine In
#' Situ NetCDF Format Manual (version V1.43). Pdf. Copernicus in situ TAC, 2021.
#' `https://doi.org/10.13155/59938` (link checked 2022-04-11).
#'
#' 3. Variable names are provided in files at
#' `https://doi.org/10.13155/53381` (link checked 2022-04-12)
#'
#' @author Dan Kelley
read.argo.copernicus <- function(file,
    debug=getOption("oceDebug"),
    processingLog, ...)
{
    if (!missing(file) && is.character(file) && 0 == file.info(file)$size)
        stop("empty file")
    if (!requireNamespace("ncdf4", quietly=TRUE))
        stop("must install.packages(\"ncdf4\") to read argo data")
    if (missing(processingLog)) processingLog <- paste(deparse(match.call()), sep="", collapse="")
    ## ofile <- file
    filename <- ""
    ## NOTE: need to name ncdf4 package because otherwise R checks give warnings.
    if (is.character(file)) {
        filename <- fullFilename(file)
        file <- ncdf4::nc_open(file)
        on.exit(ncdf4::nc_close(file))
    } else {
        if (!inherits(file, "connection"))
            stop("argument `file' must be a character string or connection")
        if (!isOpen(file)) {
            file <- ncdf4::nc_open(file)
            on.exit(ncdf4::nc_close(file))
        }
    }
    oceDebug(debug, "read.argo.copernicus(file=\"", filename, "\", ...) {\n", sep="", unindent=1, style="bold")
    varNames <- names(file$var)
    oceDebug(debug, "varNames=c(\"", paste(varNames,collapse="\", \""), "\")\n")
    res <- new("argo")

    getGlobalAttribute <- function(file, attname)
    {
        a <- ncdf4::ncatt_get(nc=file, varid=0, attname=attname)
        if (a$hasatt) a$value else NULL
    }
    # FIXME: look in docs to see if there are other interesting things
    # that I've missed here.
    res@metadata$title <- getGlobalAttribute(file, "title")
    res@metadata$institution <- getGlobalAttribute(file, "institution")
    res@metadata$source <- getGlobalAttribute(file, "source")
    res@metadata$history <- getGlobalAttribute(file, "history")
    res@metadata$references <- getGlobalAttribute(file, "references")
    res@metadata$userManualVersion <- getGlobalAttribute(file, "user_manual_version")
    res@metadata$conventions <- getGlobalAttribute(file, "Conventions")
    res@metadata$formatVersion <- getGlobalAttribute(file, "format_version")
    res@metadata$conventions <- getGlobalAttribute(file, "Conventions")
    res@metadata$featureType <- getGlobalAttribute(file, "featureType")
    res@metadata$citation <- getGlobalAttribute(file, "citation")
    res@metadata$dateUpdate  <- getGlobalAttribute(file, "date_update")
    res@metadata$WMOInstType  <- getGlobalAttribute(file, "wmo_inst_type")
    res@metadata$DOI <- getGlobalAttribute(file, "doi")
    res@metadata$PI <- getGlobalAttribute(file, "pi_name")
    res@metadata$QCManual <- getGlobalAttribute(file, "qc_manual")
    res@metadata$id <- getGlobalAttribute(file, "platform_code") # or "id"???
    res@metadata$dataNamesOriginal <- list()
    res@metadata$flags <- list()
    for (name in varNames) {
        if ("CNDC" == name) {
            res@data$conductivity <- ncdf4::ncvar_get(file, name)
            res@metadata$dataNamesOriginal$salinity <- name
            oceDebug(debug, "inferring conductivity from ", name, "\n")
            res@metadata$units$conductivity <- list(unit=expression(S/m), scale="")
        } else if ("CNDC_QC" == name) {
            res@metadata$flags$conductivity <- ncdf4::ncvar_get(file, "CNDC_QC")
            oceDebug(debug, "inferring conductivity flag from CNDC_QC\n")
        } else if ("DOXY" == name) {
            res@data$oxygen <- ncdf4::ncvar_get(file, name)
            res@metadata$dataNamesOriginal$oxygen <- name
            oceDebug(debug, "inferring oxygen from ", name, "\n")
            res@metadata$units$oxygen <- list(unit=expression(mmol/m^3), scale="")
        } else if ("DOXY_QC" == name) {
            res@metadata$flags$oxygen <- ncdf4::ncvar_get(file, "DOXY_QC")
            oceDebug(debug, "inferring oxygen flag from DOXY_QC\n")
        } else if ("PRES" == name) {
            res@data$pressure <- ncdf4::ncvar_get(file, name)
            res@metadata$dataNamesOriginal$pressure <- name
            oceDebug(debug, "inferring pressure from ", name, "\n")
        } else if ("PRES_QC" == name) {
            res@metadata$flags$pressure <- ncdf4::ncvar_get(file, "PRES_QC")
            oceDebug(debug, "inferring pressure flag from PRES_QC\n")
        } else if ("PSAL" == name) {
            res@data$salinity <- ncdf4::ncvar_get(file, name)
            res@metadata$dataNamesOriginal$salinity <- name
            oceDebug(debug, "inferring salinity from ", name, "\n")
        } else if ("PSAL_QC" == name) {
            res@metadata$flags$salinity <- ncdf4::ncvar_get(file, "PSAL_QC")
            oceDebug(debug, "inferring salinity flag from PSAL_QC\n")
        } else if ("TEMP" == name) {
            res@data$temperature <- ncdf4::ncvar_get(file, name)
            res@metadata$dataNamesOriginal$temperature <- name
            oceDebug(debug, "inferring temperature from ", name, "\n")
        } else if ("TEMP_QC" == name) {
            res@metadata$flags$temperature <- ncdf4::ncvar_get(file, "TEMP_QC")
            oceDebug(debug, "inferring temperature flag from TEMP_QC\n")
        } else if (!(name %in% c("TIME", "POSITION_QC", "TIME_QC"))) { # some special cases skipped
            oceDebug(debug, "saving \"", name, "\" to data slot, without renaming\n", sep="")
            res@data[[name]] <- ncdf4::ncvar_get(file, name)
            res@metadata$dataNamesOriginal[[name]] <- name
        }
    }
    # Extract longitude, latitude and time, if they are present.
    lat <- try(ncdf4::ncvar_get(file, "LATITUDE"), silent=TRUE)
    if (!inherits(lat, "try-error")) {
        res@data$latitude <- lat
        res@metadata$dataNamesOriginal$latitude <- "LATITUDE"
    }
    lon <- try(ncdf4::ncvar_get(file, "LONGITUDE"), silent=TRUE)
    if (!inherits(lon, "try-error")) {
        res@data$longitude <- lon
        res@metadata$dataNamesOriginal$longitude <- "LONGITUDE"
    }
    # time is measured in years since start of 1950.
    time <- try(ncdf4::ncvar_get(file, "TIME"), silent=TRUE)
    if (!inherits(time, "try-error")) {
        time0 <- ISOdatetime(1950, 1, 1, 0.0, 0.0, 0.0, tz="UTC")
        res@data$time <- time0 + 86400.0 * time
    }
    timeQc <- try(ncdf4::ncvar_get(file, "TIME_QC"), silent=TRUE)
    if (!inherits(timeQc, "try-error"))
        res@metadata$flags$time <- timeQc
    positionQc <- try(ncdf4::ncvar_get(file, "POSITION_QC"), silent=TRUE)
    if (!inherits(positionQc, "try-error"))
        res@metadata$flags$position <- positionQc
    oceDebug(debug, "} # read.argo.copernicus()\n", sep="", unindent=1, style="bold")
    res
}

