# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Read a ctd File in SSDA Format
#'
#' [read.ctd.ssda()] reads CTD files in Sea & Sun Technology's Standard Data
#' Acquisition (SSDA) format. This function is somewhat preliminary, in the
#' sense that header information is not scanned fully, and some guesses have
#' been made about the meanings of variables and units.
#'
#' @return [read.ctd.ssda()] returns a [ctd-class] object.
#'
#' @param file a connection or a character string giving the name of the file to
#' load.
#'
#' @template encodingTemplate
#'
#' @param debug an integer specifying whether debugging information is
#' to be printed during the processing. If nonzero, some information
#' is printed.
#'
#' @param processingLog ignored.
#'
# @examples
# library(oce)
# f <- system.file("extdata", "ctd_ssda.csv", package="oce")
# d <- read.ctd(f)
#'
#' @family things related to ctd data
#' @family functions that read ctd data
#'
#' @author Dan Kelley, with help from Liam MacNeil
read.ctd.ssda <- function(
    file, encoding = "latin1",
    debug = getOption("oceDebug"), processingLog) {
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
    debug <- max(0L, as.integer(debug))
    oceDebug(debug, "read.ctd.ssda(file=\"", file, "\") START\n", sep = "", unindent = 1)
    if (is.character(file)) {
        filesize <- file.info(file)$size
        if (is.na(filesize) || 0L == filesize) {
            stop("empty file \"", file, "\"")
        }
    }
    if (is.character(file)) {
        file <- file(file, "r", encoding = encoding)
        on.exit(close(file))
    }
    lines <- readLines(file)
    dataStart <- grep("^Lines[ ]*:[ ]*[0-9]*$", lines)
    header <- lines[1L:dataStart]
    if (1 != length(dataStart)) {
        stop("cannot find 'Lines :' in the data file.")
    }
    # how many lines might there be in between?
    names <- strsplit(gsub("^;[ ]*", "", lines[dataStart + 2L]), "[ ]+")[[1]]
    # message("next are names:");print(names)
    namesOriginal <- names
    # Use standard oce names for some things.
    # (Thanks to Liam MacNeil for pointing these out.)
    names <- gsub("AO2_%", "oxygenSaturation", names)
    names <- gsub("AO2mg", "oxygenMg", names)
    names <- gsub("AO2ml", "oxygenMl", names)
    names <- gsub("Boden", "bottom", names)
    names <- gsub("Datasets", "scan", names)
    names <- gsub("Druck", "pressure", names)
    names <- gsub("Lat", "latitude", names)
    names <- gsub("Leitf", "conductivity", names)
    names <- gsub("Licor", "PAR", names)
    names <- gsub("Long", "longitude", names)
    names <- gsub("RawO2", "oxygenVoltage", names)
    names <- gsub("SALIN", "salinity", names)
    names <- gsub("SIGMA", "sigma", names)
    names <- gsub("Temp.", "temperature", names)
    d <- read.table(text = lines, skip = dataStart + 4, col.names = names, header = FALSE, encoding = encoding)
    # Store just the first longitude and latitude in the metadata. The
    # format is that e.g. 1559.9413E means 15.0 degrees + 49.9413 minutes.
    #
    # Also, as discussed at https://github.com/dankelley/oce/issues/2227, 
    # the test (local-only) suite had a problem with R 4.1.1 in the computation
    # of longitude. This was because of a trailing 'E' in the test file.
    # I think for previous versions of R, the as.numeric() call used on
    # that text entry must have ignored the trailing 'E'.
    lon1 <- d$longitude[1]
    lonSign <- if (grepl("[eE]", lon1)) 1.0 else if (grepl("wW", lon1)) -1.0 else 1.0
    lon <- as.numeric(gsub("[eEwW]", "", lon1))
    londeg <- floor(lon / 100)
    lonmin <- lon - londeg * 100
    longitude <- londeg + lonmin / 60.0
    longitude <- lonSign * longitude
    oceDebug(debug, "lon1=", lon1, ", lon=", lon, " deg=", londeg, " min=", lonmin, " -> longitude=", longitude, "\n")
    lat1 <- d$latitude[1]
    latSign <- if (grepl("[nN]", lat1)) 1.0 else if (grepl("eE", lat1)) -1.0 else 1.0
    lat <- as.numeric(gsub("[nNsS]", "", lat1))
    latdeg <- floor(lat / 100)
    latmin <- lat - latdeg * 100
    latitude <- latdeg + latmin / 60.0
    latitude <- latSign * latitude
    oceDebug(debug, "lat1=", lat1, ", lat=", lat, " deg=", latdeg, " min=", latmin, " -> latitude=", latitude, "\n")
    res <- as.ctd(
        salinity = d$salinity, temperature = d$temperature, pressure = d$pressure,
        longitude = longitude, latitude = latitude
    )
    # Save header and original names
    res@metadata$header <- header
    dno <- list()
    for (i in seq_along(names)) {
        dno[[names[i]]] <- namesOriginal[[i]]
    }
    res@metadata$dataNamesOriginal <- dno
    # Now add in non-standard data
    for (n in names(d)) {
        if (!n %in% c(c("salinity", "pressure", "temperature", "latitude", "longitude"))) {
            res <- oceSetData(res, n, d[[n]], note = NULL)
        }
    }
    # Add in time, removing the components (which serve no purpose)
    if (all(c("IntDT", "IntDT.1") %in% names(d))) {
        time <- as.POSIXct(paste(d$IntDT, d$IntDT.1), "%d.%m.%Y %H:%M:%S", tz = "UTC")
        res <- oceSetData(res, "time", time, note = NULL)
        res@data$IntDT <- NULL
        res@data$IntDT.1 <- NULL
    }
    # Handle some conversions and units
    if ("oxygenVoltage" %in% names(res@data)) {
        # file has in mV but oce uses V
        res@data$oxygenVoltage <- 0.001 * res@data$oxygenVoltage
        res@metadata$units$oxygenVoltage <- list(unit = expression(V), scale = "")
    }
    if ("oxygenSaturation" %in% names(res@data)) {
        res@metadata$units$oxygenSaturation <- list(unit = expression(percent), scale = "")
    }
    if ("oxygenMg" %in% names(res@data)) {
        res@metadata$units$oxygenMg <- list(unit = expression(mg / L), scale = "")
    }
    if ("oxygenMl" %in% names(res@data)) {
        res@metadata$units$oxygenMl <- list(unit = expression(mL / L), scale = "")
    }
    if ("conductivity" %in% names(res@data)) {
        res@metadata$units$conductivity <- list(unit = expression(mS / cm), scale = "")
    }
    if ("sigma" %in% names(res@data)) {
        res@metadata$units$sigma <- list(unit = expression(kg / m^3), scale = "")
    }
    if ("PAR" %in% names(res@data)) {
        res@metadata$units$PAR <- list(unit = expression(pffr), scale = "")
    }
    res@processingLog <- processingLogAppend(
        res@processingLog,
        paste(deparse(match.call()), sep = "", collapse = "")
    )
    oceDebug(debug, "END read.ctd.ssda()\n", sep = "", unindent = 1)
    res
}
