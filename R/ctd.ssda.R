#' Read CTD format in SSDA format
#'
#' [read.ctd.ssda()] reads CTD files in Sea & Sun Technology´s Standard Data
#' Acquisition (SSDA) format. This function is somewhat preliminary, in the
#' sense that header information is not scanned fully, and some guesses have
#' been made about the meanings of variables and units.
#'
#' @return [read.ctd.ssda()] returns a [ctd-class] object.
#'
#' @param file a connection or a character string giving the name of the file to
#' load.
#'
#' @param debug an integer specifying whether debugging information is
#' to be printed during the processing. If nonzero, some information
#' is printed.
#'
#' @family functions that read ctd data
#'
#' @author Dan Kelley, with help from (need permission to name)
read.ctd.ssda <- function(file, debug=getOption("oceDebug"))
{
    debug <- max(0L, as.integer(debug))
    oceDebug(debug, "read.ctd.ssda(file=\"", file, "\") {\n", sep="", style="bold", unindent=1)
    if (missing(file))
        stop("must provide 'file'")
    if (is.character(file)) {
        filesize <- file.info(file)$size
        if (is.na(filesize) || 0L == filesize)
            stop("empty file")
    }
    filename <- ""
    if (is.character(file)) {
        filename <- fullFilename(file)
        file <- file(file, "r")
        on.exit(close(file))
    }
    lines <- readLines(file)
    seek(file, 0L) # rewind so we can read from the source (faster than reading from text)
    dataStart <- grep("^Lines[ ]*:[ ]*[0-9]*$", lines)
    header <- lines[1L:dataStart]
    if (1 != length(dataStart))
        stop("cannot find 'Lines :' in the data file.")
    # how many lines might there be in between?
    names <- strsplit(gsub("^;[ ]*", "", lines[dataStart+2L]), "[ ]+")[[1]]
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
    names <- gsub("Licor", "fluorescence", names)
    names <- gsub("Long", "longitude", names)
    names <- gsub("RawO2", "oxygenVoltage", names)
    names <- gsub("SALIN", "salinity", names)
    names <- gsub("SIGMA", "sigma", names)
    names <- gsub("Temp.", "temperature", names)
    d <- read.table(file, skip=dataStart + 4, col.names=names, header=FALSE)
    # Lon and lat are in an odd system, with e.g. 12.34 meaning 12deg+34minutes.
    lon <- as.numeric(d$longitude[1])
    londeg <- floor(lon / 100)
    lonmin <- lon - londeg*100
    longitude <- londeg + lonmin / 60.0
    oceDebug(debug, "lon=", lon, " deg=", londeg, " min=", lonmin, " -> longitude=", longitude, "\n")
    lat <- as.numeric(gsub("N","",d$latitude[1]))
    latdeg <- floor(lat / 100)
    latmin <- lat - latdeg*100
    latitude <- latdeg + latmin / 60.0
    oceDebug(debug, "lat=", lat, " deg=", latdeg, " min=", latmin, " -> latitude=", latitude, "\n")
    ctd <- as.ctd(salinity=d$salinity, temperature=d$temperature, pressure=d$pressure,
        longitude=longitude, latitude=latitude)
    # Save header and original names
    ctd@metadata$header <- header
    dno <- list()
    for (i in seq_along(names))
        dno[[names[i]]] <- namesOriginal[[i]]
    ctd@metadata$dataNamesOriginal <- dno
    # Now add in non-standard data
    for (n in names(d)) {
        if (!n %in% c(c("salinity", "pressure", "temperature", "latitude", "longitude"))) {
            ctd <- oceSetData(ctd, n, d[[n]], note=NULL)
        }
    }
    # Add in time, removing the components (which serve no purpose)
    if (all(c("IntDT", "IntDT.1") %in% names(d))) {
        time <- as.POSIXct(paste(d$IntDT, d$IntDT.1), "%d.%m.%Y %H:%M:%S", tz="UTC")
        ctd <- oceSetData(ctd, "time", time, note=NULL)
        ctd@data$IntDT <- NULL
        ctd@data$IntDT.1 <- NULL
    }
    # Handle some conversions and units
    if ("oxygenVoltage" %in% names(ctd@data)) {
        # file has in mV but oce uses V
        ctd@data$oxygenVoltage <- 0.001 * ctd@data$oxygenVoltage
        ctd@metadata$units$oxygenVoltage <- list(unit=expression(V), scale="")
    }
    if ("oxygenSaturation" %in% names(ctd@data))
        ctd@metadata$units$oxygenSaturation<- list(unit=expression(percent), scale="")
    if ("conductivity" %in% names(ctd@data))
        ctd@metadata$units$conductivity <- list(unit=expression(mS/cm), scale="")
    if ("sigma" %in% names(ctd@data))
        ctd@metadata$units$sigma <- list(unit=expression(kg/m^3), scale="")
    oceDebug(debug, "} # read.ctd.ssda()\n", sep="", style="bold", unindent=1)
    ctd
}

