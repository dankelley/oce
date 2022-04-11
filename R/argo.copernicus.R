#' Read argo file in Copernicus format
#'
#' This function was added to read a particular file downloaded from the
#' Fleet Monitoring website (Reference 1).  The format was inferred
#' through examination of the file and a brief study of a document
#' (Reference 2) that describes the format.  Not all fields are read
#' by this function.
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
#' `https://doi.org/10.13155/59938`.
#'
#' @author Dan Kelley
read.argo.copernicus <- function(file,
    debug=getOption("oceDebug"),
    processingLog, ...)
{
    if (!missing(file) && is.character(file) && 0 == file.info(file)$size)
        stop("empty file")
    if (!requireNamespace("ncdf4", quietly=TRUE))
        stop('must install.packages("ncdf4") to read argo data')
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
    res@metadata$dataNamesOriginal <- list()
    res@metadata$flags <- list()
    for (name in varNames) {
        if ("PRES" == name) {
            res@data$pressure <- ncdf4::ncvar_get(file, "PRES")
            res@metadata$dataNamesOriginal$pressure <- "PRES"
            oceDebug(debug, "inferring pressure from PRES\n")
        } else if ("PRES_QC" == name) {
            res@metadata$flags$pressure <- ncdf4::ncvar_get(file, "PRES_QC")
            oceDebug(debug, "inferring pressure flag from PRES_QC\n")
        } else if ("PSAL" == name) {
            res@data$salinity <- ncdf4::ncvar_get(file, "PSAL")
            res@metadata$dataNamesOriginal$salinity <- "PSAL"
            oceDebug(debug, "inferring salinity from PSAL\n")
        } else if ("PSAL_QC" == name) {
            res@metadata$flags$salinity <- ncdf4::ncvar_get(file, "PSAL_QC")
            oceDebug(debug, "inferring salinity flag from PSAL_QC\n")
        } else if ("TEMP" == name) {
            res@data$temperature <- ncdf4::ncvar_get(file, "TEMP")
            res@metadata$dataNamesOriginal$temperature <- "TEMP"
            oceDebug(debug, "inferring temperature from TEMP\n")
        } else if ("TEMP_QC" == name) {
            res@metadata$flags$temperature <- ncdf4::ncvar_get(file, "TEMP_QC")
            oceDebug(debug, "inferring temperature flag from TEMP_QC\n")
        } else {
            oceDebug(debug, "saving \"", name, "\" to data slot, without renaming\n", sep="")
            res@data[[name]] <- ncdf4::ncvar_get(file, name)
            res@metadata$dataNamesOriginal[[name]] <- name
        }
        # Extract time, longitude and latitude.  I'm not sure why these are not
        # appearing in varNames.
        res@data$latitude <- ncdf4::ncvar_get(file, "LATITUDE")
        res@data$longitude <- ncdf4::ncvar_get(file, "LONGITUDE")
        # time is in years since time0
        time0 <- ISOdatetime(1950, 1, 1, 0.0, 0.0, 0.0, tz="UTC")
        res@data$time <- 86400.0*ncdf4::ncvar_get(file, "TIME") + time0
    }
    oceDebug(debug, "} # read.argo.copernicus()\n", sep="", unindent=1, style="bold")
    res
}

