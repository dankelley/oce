# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Read an AML CTD file
#'
#' [read.ctd.aml()] reads files that hold data acquired with an AML
#' Oceanographic BaseX2 CTD instrument. The SeaCast software associated with
#' this device can output data in several formats, of which only two are
#' handled, and only one is recommended (see \sQuote{Details}).
#'
#' The handled formats match files available to the author, both of which
#' diverge slightly from the format described in the AML documentation (see
#' \dQuote{References}).
#'
#' Regardless of the format, files must contain columns named `Conductivity
#' (mS/cm)`, `Temperature (C)` and `Pressure (dBar)`, because [ctd-class]
#' objects need those quantities.  (Actually, if pressure is not found, but
#' `Depth (m)` is, then pressure is estimated with [swDepth()], as a
#' workaround.) Note that other columns will be also read and stored in the
#' returned value, but they will not have proper units.  Attempts are made to
#' infer the sampling location from the file, by searching for strings like
#' `Latitude=` in the header. Headers typically contain two values of the
#' location, and it is the second pair that is used by this function, with a
#' `NA` value being recorded if the value in the file is `no-lock`.  The
#' instrument serial number is also read, although the individual serial numbers
#' of the sensors are not read.  Position and serial number are stored in the
#' the `metadata` slot of the returned value.  The entire header is also stored
#' there, to let users glean more about dataset.
#'
#' Two formats are handled, as described below. Format 1 is greatly preferred,
#' because it is more robust (see below on `format=2`) and also because it can
#' be read later by the AML SeaCast software.
#'
#' 1. If `format` is `1` then the file is assumed to be in a format created by
#' selecting *Export As ... Seacast (.csv)* in AML's SeaCast software, with
#' settings to output pressure (or, as second-best, depth), temperature and
#' conductivity, and perhaps other things.  The delimiter must be comma.  If
#' date and time are output, their formats must be yyyy-mm-dd and UTC,
#' respectively.  Decoding the file proceeds as follows.  First, a check is done
#' to ensure that the first line consists of the string `[cast header]`. Then an
#' attempt is made to infer location and serial number from the header.  After
#' this, [read.ctd.aml()] searches down for a line containing the string
#' `[data]`. The first line thereafter is taken as a comma-separated list of
#' variable names, and lines following that are taken to hold the variable
#' values, separated by commas.
#'
#' 2. If `format` is `2` then the first line must be a comma-separated list of
#' column names.  This may be followed by header information, which is handled
#' similarly as for `format=1`. The data are read from all lines that have the
#' same number of commas as the first line, an admittedly brittle strategy
#' developed as a way to handle some files that lacked other information about
#' the end of the header.
#'
#' In both cases, the data columns, renamed to oce convention, are stored in the
#' `data` slot.  For the mandatory variables, units are also stored, as for
#' other [ctd-class] objects.
#'
#' @param file a connection or a character string giving the name of
#' the file to load.
#'
#' @param format an integer indicating the format type.  If not supplied, the
#' first line is examined to determine whether the file matches the `format=1` or
#' `format=2` style (see \sQuote{Details}).
#'
#' @template debugTemplate
#'
#' @param processingLog ignored.
#'
#' @param ... ignored.
#'
#' @return [read.ctd.aml()] returns a [ctd-class] object.
#'
#' @examples
#' library(oce)
#' f <- system.file("extdata", "ctd_aml.csv", package="oce")
#' d <- read.ctd.aml(f)
#' summary(d)
#'
#' @author Dan Kelley
#'
#' @references
#' AML Oceanographic. "SeaCast 4 User Manual (Version 2.06)." AML Oceanographic,
#' Mahy 2016.
#' `https://www.subseatechnologies.com/media/files/page/032e50ac/seacast-4-2-user-manual-sti.pdf`.
#'
#' @family functions that read ctd data
read.ctd.aml <- function(file, format,
    debug=getOption("oceDebug"), processingLog, ...)
{
    debug <- max(0L, as.integer(debug))
    if (missing(file))
        stop("must supply 'file'")
    oceDebug(debug, "read.ctd.aml(file=\"", file, "\", ...) {\n", unindent=1, style="bold")
    if (is.character(file) && 0 == file.info(file)$size)
        stop("empty file")
    filename <- ""
    if (is.character(file)) {
        filename <- fullFilename(file)
        file <- file(file, "r")
        on.exit(close(file))
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }
    getMetadataItem <- function(lines, name, numeric=TRUE, ignore.case=FALSE, debug=0)
    {
        oceDebug(debug, "getMetadataItem(lines, \"", name, "\", numeric=", numeric, ")\n", style="bold", unindent=1)
        l <- grep(paste0("^",name,"="), lines, ignore.case=ignore.case)
        res <- NA
        if (length(l) > 0L) {
            if (length(l) > 1L)
                oceDebug(debug, "using second of ", length(l), " values\n")
            # We take second definition, ignoring first (or any others).
            l <- l[2]
            res <- trimws(strsplit(lines[l], "=")[[1]][2])
            if (numeric) {
                res <- if (grepl("no-lock", res, ignore.case=TRUE)) NA else as.numeric(res)
            }
        }
        oceDebug(debug, "returning ", res, "\n")
        oceDebug(debug, "#} getMetadataItem()\n", style="bold", unindent=1)
        res
    }
    lines <- readLines(file, encoding="UTF-8-BOM", warn=FALSE)
    oceDebug(debug, "read ", length(lines), " lines in this file\n")
    if (missing(format)) {
        format <- if (grepl("^\\[cast header\\]", lines[1])) {
            1L
        } else if (grepl(",", lines[1])) {
            2L
        } else {
            stop("cannot determine file 'format' by examining first line (shown below)\n", lines[1])
        }
        oceDebug(debug, "inferred format=", format, " from file's first line\n")
    }
    format <- as.integer(format)
    if (format != 1L && format != 2L)
        stop("unrecognized format value, ", format, "; it must be 1 or 2")

    # FIXME: add other relevant metadata here.  This will require some
    # familiarity with the typical contents of the metadata.  For example,
    # I see 'SN' and 'BoardSN', and am inferring that we want to save
    # the first, but maybe it's the second...
    longitude <- getMetadataItem(lines, "longitude", ignore.case=TRUE, debug=debug-1L)
    if (is.na(longitude))
        longitude <- getMetadataItem(lines, "lon", ignore.case=TRUE, debug=debug-1L)
    latitude <- getMetadataItem(lines, "latitude", ignore.case=TRUE, debug=debug-1L)
    if (is.na(latitude))
        latitude <- getMetadataItem(lines, "lat", ignore.case=TRUE, debug=debug-1L)
    serialNumber <- getMetadataItem(lines, "sn", ignore.case=TRUE, numeric=FALSE, debug=debug-1L)
    oceDebug(debug, "inferred location ", longitude, "E, ", latitude, "N, ", " serialNumber ", serialNumber, "\n", sep="")
    header <- ""
    if (format == 1L) {
        endOfHeader <- grep("^\\[data\\]$", lines)
        header <- lines[seq(1L, endOfHeader-1L)]
        col.names <- strsplit(lines[endOfHeader+1L], ",")[[1]]
    } else if (format == 2L) {
        # find 'header' below
        col.names <- strsplit(lines[1], ",")[[1]]
    }
    oceDebug(debug, "step 1 col.names: c(\"", paste(col.names, collapse="\", \""), "\")\n")
    if (length(col.names) < 1L)
        stop("cannot determine column names")
    if (!("Temperature (C)" %in% col.names))
        stop("no 'Temperature (C)' column found")
    if (!("Conductivity (mS/cm)" %in% col.names))
        stop("no 'Conductivity (mS/cm)' column found")
    if (!("Pressure (dBar)" %in% col.names) && !("Depth (m)" %in% col.names))
        stop("No 'Pressure (dBar)' or 'Depth (m)' column found")
    col.names[col.names == "Temperature (C)"] <- "temperature"
    col.names[col.names == "Conductivity (mS/cm)"] <- "conductivity"
    col.names[col.names == "Pressure (dBar)"] <- "pressure"
    col.names[col.names == "Depth (m)"] <- "depth" # optional
    col.names[col.names == "Battery (V)"] <- "battery" # optional
    oceDebug(debug, "step 2 col.names: c(\"", paste(col.names, collapse="\", \""), "\")\n")
    if (format == 1L) {
        data <- read.csv(text=lines, skip=endOfHeader+1L, col.names=col.names)
    } else if (format == 2L) {
        nfields <- length(col.names)
        nfield <- unlist(lapply(lines, function(l) length(strsplit(l, ",")[[1]])))
        look <- nfield == nfield[1]
        header <- lines[seq(1L, which(look)[2]-1L)]
        look[1] <- FALSE
        data <- read.csv(text=lines[look], header=FALSE, col.names=col.names)
    } else {
        stop("unrecognized format value")
    }
    if (!("pressure" %in% names(data)) && "depth" %in% names(data)) {
        data$pressure <- swPressure(data$depth, latitude)
        oceDebug(debug, "inferred pressure from depth (assuming saltwater formula)\n")
    }
    S <- swSCTp(conductivity=data$conductivity,
        temperature=data$temperature, pressure=data$pressure,
        conductivityUnit="mS/cm", eos="gsw") # use gsw to get better results for S<2.
    res <- as.ctd(salinity=S, temperature=data$temperature,
        pressure=data$pressure, conductivity=data$conductivity,
        longitude=longitude, latitude=latitude,
        serialNumber=serialNumber, debug=debug-1L)
    oceDebug(debug, "created basic ctd object, with salinity, temperature, pressure, conductivity, longitude, latitude, and serial number\n")
    res@metadata$filename <- filename
    res@metadata$header <- header
    if (2L == sum(c("Date", "Time") %in% names(data))) {
        res@data$time <- as.POSIXct(paste(data$Date, data$Time), tz="UTC")
        oceDebug(debug, "added \"time\" to the data slot\n", sep="")
    }
    dno <- list(salinity="-", temperature="Temperature (C)",
        conductivity="Conductivity (mS/cm)", Date="Date", Time="Time")
    if ("depth" %in% names(data))
        dno$depth <- "Depth (m)"
    if ("battery" %in% names(data))
        dno$battery <- "Battery (V)"
    res@metadata$dataNamesOriginal <- dno
    for (name in names(data)) {
        if (name != "temperature" && name != "salinity" && name != "pressure") {
            res <- oceSetData(res, name, data[[name]], note=NULL)
            oceDebug(debug, "added \"", name, "\" to the data slot\n", sep="")
        }
    }
    # Add units for things not set up by as.ctd(). We know that conductivity
    # has no unit, and we know that it's present, but we check on the other
    # things.
    res@metadata$units$conductivity <- list(unit=expression(mS/cm), scale="")
    if ("battery" %in% names(res@data))
        res@metadata$units$battery <- list(unit=expression(V), scale="")
    if ("depth" %in% names(res@data))
        res@metadata$units$depth <- list(unit=expression(m), scale="")
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # read.ctd.aml() {\n", unindent=1, style="bold")
    res
}
