# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

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
#' @family things related to argo data
#'
#' @author Dan Kelley
read.argo.copernicus <- function(file,
    debug=getOption("oceDebug"),
    processingLog,
    ...)
{
    if (missing(file))
        stop("must supply 'file'")
    if (is.character(file)) {
        if (!file.exists(file))
            stop("cannot find file '", file, "'")
        if (0L == file.info(file)$size)
            stop("empty file '", file, "'")
    }
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
    #print(sort(varNames))
    nameMap <- list(
        "BBP700"="backscatter700", # not listed in official docs
        "BBP700_ADJUSTED"="backscatterA700djusted",
        "BBP700_ADJUSTED_ERROR"="backscatterA700djustedError",
        "CNDC"="conductivity",
        "CNDC_ADJUSTED"="conductivityAdjusted",
        "CNDC_ADJUSTED_ERROR"="conductivityAdjustedError",
        "CPHL"="chlorophyll",
        "CPHL_ADJUSTED"="chlorophyllAdjusted",
        "CPHL_ADJUSTED_ERROR"="chlorophyllAdjustedError",
        "DIRECTION"="direction",
        "DOX2"="oxygen",
        "DOX2_ADJUSTED"="oxygenAdjusted",
        "DOX2_ADJUSTED_ERROR"="oxygenAdjustedError",
        "DOXY"="oxygen",
        "DOXY_ADJUSTED"="oxygenAdjusted",
        "DOXY_ADJUSTED_ERROR"="oxygenAdjustedError",
        "NTAW"="nitrate",
        "NTAW_ADJUSTED"="nitrateAdjusted",
        "NTAW_ADJUSTED_ERROR"="nitrateAdjustedError",
        "PHPH"="pH",
        "PHPH_ADJUSTED"="pHAdjusted",
        "PHPH_ADJUSTED_ERROR"="pHAdjustedError",
        "PSAL"="salinity",
        "PSAL_ADJUSTED"="salinityAdjusted",
        "PSAL_ADJUSTED_ERROR"="salinityAdjustedError",
        "PRES"="pressure",
        "PRES_ADJUSTED"="pressureAdjusted",
        "PRES_ADJUSTED_ERROR"="pressureAdjustedError",
        "TEMP"="temperature",
        "TEMP_ADJUSTED"="temperatureAdjusted",
        "TEMP_ADJUSTED_ERROR"="temperatureAdjustedError",
        "VERTICAL_SAMPLING_SCHEME"="verticalSamplingScheme"
        )
    varNamesKnown <- names(nameMap)
    QCNamesKnown <- paste0(names(nameMap), "_QC")
    #print(QCNamesKnown)
    res@metadata$units <- list()
    for (name in varNames) {
        if (name %in% varNamesKnown) {
            oceName <- nameMap[[name]]
            res@data[[oceName]] <- ncdf4::ncvar_get(file, name)
            res@metadata$dataNamesOriginal[oceName] <- name
            oceDebug(debug, "inferring ", oceName, " from ", name, "\n")
            if (grepl("^BBP700", name))
                res@metadata$units[[oceName]] <- list(unit=expression(1/m), scale="")
            else if (grepl("^CNDC", name))
                res@metadata$units[[oceName]] <- list(unit=expression(S/m), scale="")
            else if (grepl("^CPHL", name))
                res@metadata$units[[oceName]] <- list(unit=expression(mg/m^3), scale="")
            else if (grepl("^DOX2", name))
                res@metadata$units[[oceName]] <- list(unit=expression(umol/kg), scale="")
            else if (grepl("^DOXY", name))
                res@metadata$units[[oceName]] <- list(unit=expression(mmol/m^3), scale="")
            else if (grepl("^NTAW", name))
                res@metadata$units[[oceName]] <- list(unit=expression(umol/kg), scale="")
            else if (grepl("^PHPH", name))
                res@metadata$units[[oceName]] <- list(unit=expression(), scale="")
            else if (grepl("^PRES", name))
                res@metadata$units[[oceName]] <- list(unit=expression(dbar), scale="")
            else if (grepl("^PSAL", name))
                res@metadata$units[[oceName]] <- list(unit=expression(), scale="")
            else if (grepl("^TEMP", name))
                res@metadata$units[[oceName]] <- list(unit=expression(degree*C), scale="")
        } else if (name %in% QCNamesKnown) {
            oceName <- nameMap[[gsub("_QC", "", name)]]
            res@metadata$flags[[oceName]] <- ncdf4::ncvar_get(file, name)
            oceDebug(debug, "inferring flags[", oceName, "] from ", name, "\n")
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

