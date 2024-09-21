# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

# AML Base.X2 probe imported to my computer using the Sailfish 1.4.8.0 software

#' Read a ctd File in AML Format
#'
#' [read.ctd.aml()] reads files that hold data acquired with an AML
#' Oceanographic Base.X2 CTD instrument. The software associated with
#' this device can output data in multiple formats, of which [read.ctd.aml()]
#' can read only three, based on files provided to the author by users. If the
#' `format` parameter is not supplied, the function attempts to infer it from
#' the first line in the file; see \dQuote{Details}.
#'
#' If `format` is not supplied, the first line of the file is examined. If that
#' line contains `[cast header]` (case insensitive), then format 1 is inferred.
#' If it contains a comma (i.e. if no header is present) then format 2 is
#' inferred.  (The AML documentation cautions against saving in this format.)
#' And if it contains `[header]` (case insensitive) then format 3 is inferred.
#'
#' Support for types 1 and 2 were added in about the year 2017, whereas type 3
#' was added in 2024. Documentation was once available for formats 1 and 2, but
#' it was not an exact match to sample files provided to the author of
#' [read.ctd.aml()]. No documentation seemed to be available for format 3, so
#' the code was written after manual inspection of a sample file. Given these
#' things, users are advised to be on the lookout for problems.
#'
#' @param file a connection or a character string giving the name of
#' the file to load.
#'
#' @param format an integer indicating the format type.  If not supplied, the
#' first line is examined to make a guess as to the format (see \dQuote{Details}).
#'
#' @template encodingTemplate
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
#' # Show S,T and p for first 5 lines of a format=1 file
#' f1 <- system.file("extdata", "ctd_aml_type1.csv.gz", package = "oce")
#' d1 <- read.ctd.aml(f1)
#' data.frame(S = d1[["salinity"]], T = d1[["temperature"]], p = d1[["pressure"]])
#'
#' # Show S,T and p for first 5 lines of a format=3 file
#' f3 <- system.file("extdata", "ctd_aml_type3.csv.gz", package = "oce")
#' d3 <- read.ctd.aml(f3)
#' data.frame(S = d3[["salinity"]], T = d3[["temperature"]], p = d3[["pressure"]])
#'
#' @author Dan Kelley
#'
#' @references
#' 1. AML Oceanographic. "SeaCast 4 User Manual (Version 2.06)." AML Oceanographic,
#' May 2016.  This was once available at the <www.subsseateechnologies.com>
#' website, but neither it nor a new version could be located by the author's
#' search of the website in September 2024.
## https://www.subseatechnologies.com/media/files/page/032e50ac/seacast-4-2-user-manual-sti.pdf
#'
#' @family things related to ctd data
#' @family functions that read ctd data
read.ctd.aml <- function(file, format, encoding = "UTF-8-BOM", debug = getOption("oceDebug"), processingLog, ...) {
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
    oceDebug(debug, "read.ctd.aml(file=\"", file, "\", ...) START\n", sep = "", unindent = 1)
    if (is.character(file) && 0 == file.info(file)$size) {
        stop("empty file")
    }
    filename <- ""
    if (is.character(file)) {
        filename <- fullFilename(file)
        file <- file(file, "r", encoding = encoding)
        on.exit(close(file))
    }
    if (!inherits(file, "connection")) {
        stop("argument `file' must be a character string or connection")
    }
    if (!isOpen(file)) {
        open(file, "r", encoding = encoding)
        on.exit(close(file))
    }
    getMetadataItem <- function(lines, name, numeric = TRUE, ignore.case = FALSE, debug = 0) {
        oceDebug(debug, "getMetadataItem(lines, \"", name, "\", numeric=", numeric, ")\n", sep = "", unindent = 1)
        l <- grep(paste0("^", name, "="), lines, ignore.case = ignore.case)
        res <- NA
        if (length(l) > 0L) {
            if (length(l) > 1L) {
                oceDebug(debug, "using second of ", length(l), " values\n")
            }
            # We take second definition, ignoring first (or any others).
            l <- l[2]
            res <- trimws(strsplit(lines[l], "=")[[1]][2])
            if (numeric) {
                res <- if (grepl("no-lock", res, ignore.case = TRUE)) NA else as.numeric(res)
            }
        }
        oceDebug(debug, "returning ", res, "\n")
        oceDebug(debug, "END getMetadataItem()\n", unindent = 1)
        res
    }
    lines <- readLines(file, warn = FALSE)
    oceDebug(debug, "read ", length(lines), " lines in this file\n")
    if (missing(format)) {
        format <- if (grepl("^\\[cast header\\]", lines[1], ignore.case = TRUE)) {
            1L
        } else if (grepl(",", lines[1])) {
            2L
        } else if (grepl("^\\[header\\]", lines[1], ignore.case = TRUE)) {
            3L
        } else {
            stop("cannot determine file 'format' by examining first line (shown below)\n", lines[1])
        }
        oceDebug(debug, "inferred format=", format, " from file's first line\n")
    }
    format <- as.integer(format)
    if (!format %in% 1:3) {
        stop("unrecognized format value, ", format, "; it must be 1, 2 or 3")
    }
    if (format == 3L) {
        rval <- read.ctd.aml.type3(file = filename, encoding = encoding, debug = debug - 1L)
        oceDebug(debug, "END read.ctd.aml()\n", unindent = 1)
        return(rval)
    }
    # FIXME: add other relevant metadata here.  This will require some
    # familiarity with the typical contents of the metadata.  For example,
    # I see 'SN' and 'BoardSN', and am inferring that we want to save
    # the first, but maybe it's the second...
    longitude <- getMetadataItem(lines, "longitude", ignore.case = TRUE, debug = debug - 1L)
    if (is.na(longitude)) {
        longitude <- getMetadataItem(lines, "lon", ignore.case = TRUE, debug = debug - 1L)
    }
    latitude <- getMetadataItem(lines, "latitude", ignore.case = TRUE, debug = debug - 1L)
    if (is.na(latitude)) {
        latitude <- getMetadataItem(lines, "lat", ignore.case = TRUE, debug = debug - 1L)
    }
    serialNumber <- getMetadataItem(lines, "sn", ignore.case = TRUE, numeric = FALSE, debug = debug - 1L)
    oceDebug(debug, "inferred location ", longitude, "E, ", latitude, "N, ", " serialNumber ", serialNumber, "\n", sep = "")
    header <- ""
    if (format == 1L) {
        endOfHeader <- grep("^\\[data\\]$", lines)
        header <- lines[seq(1L, endOfHeader - 1L)]
        col.names <- strsplit(lines[endOfHeader + 1L], ",")[[1]]
    } else if (format == 2L) {
        # find 'header' below
        col.names <- strsplit(lines[1], ",")[[1]]
    }
    oceDebug(debug, "step 1 col.names: c(\"", paste(col.names, collapse = "\", \""), "\")\n")
    if (length(col.names) < 1L) {
        stop("cannot determine column names")
    }
    if (!("Temperature (C)" %in% col.names)) {
        stop("no 'Temperature (C)' column found")
    }
    if (!("Conductivity (mS/cm)" %in% col.names)) {
        stop("no 'Conductivity (mS/cm)' column found")
    }
    if (!("Pressure (dBar)" %in% col.names) && !("Depth (m)" %in% col.names)) {
        stop("No 'Pressure (dBar)' or 'Depth (m)' column found")
    }
    col.names[col.names == "Temperature (C)"] <- "temperature"
    col.names[col.names == "Conductivity (mS/cm)"] <- "conductivity"
    col.names[col.names == "Pressure (dBar)"] <- "pressure"
    col.names[col.names == "Depth (m)"] <- "depth" # optional
    col.names[col.names == "Battery (V)"] <- "battery" # optional
    oceDebug(debug, "step 2 col.names: c(\"", paste(col.names, collapse = "\", \""), "\")\n")
    if (format == 1L) {
        data <- read.csv(text = lines, skip = endOfHeader + 1L, col.names = col.names, encoding = encoding)
    } else if (format == 2L) {
        nfield <- unlist(lapply(lines, function(l) length(strsplit(l, ",")[[1]])))
        look <- nfield == nfield[1]
        header <- lines[seq(1L, which(look)[2] - 1L)]
        look[1] <- FALSE
        data <- read.csv(text = lines[look], header = FALSE, col.names = col.names, encoding = encoding)
    } else {
        stop("unrecognized format value")
    }
    if (!("pressure" %in% names(data)) && "depth" %in% names(data)) {
        data$pressure <- swPressure(data$depth, latitude)
        oceDebug(debug, "inferred pressure from depth (assuming saltwater formula)\n")
    }
    S <- swSCTp(
        conductivity = data$conductivity,
        temperature = data$temperature, pressure = data$pressure,
        conductivityUnit = "mS/cm", eos = "gsw"
    ) # use gsw to get better results for S<2.
    res <- as.ctd(
        salinity = S, temperature = data$temperature,
        pressure = data$pressure, conductivity = data$conductivity,
        longitude = longitude, latitude = latitude,
        serialNumber = serialNumber, debug = debug - 1L
    )
    oceDebug(debug, "created basic ctd object, with salinity, temperature, pressure, conductivity, longitude, latitude, and serial number\n")
    res@metadata$filename <- filename
    res@metadata$header <- header
    if (2L == sum(c("Date", "Time") %in% names(data))) {
        res@data$time <- as.POSIXct(paste(data$Date, data$Time), tz = "UTC")
        oceDebug(debug, "added \"time\" to the data slot\n", sep = "")
    }
    dno <- list(
        salinity = "-", temperature = "Temperature (C)",
        conductivity = "Conductivity (mS/cm)", Date = "Date", Time = "Time"
    )
    if ("depth" %in% names(data)) {
        dno$depth <- "Depth (m)"
    }
    if ("battery" %in% names(data)) {
        dno$battery <- "Battery (V)"
    }
    res@metadata$dataNamesOriginal <- dno
    for (name in names(data)) {
        if (name != "temperature" && name != "salinity" && name != "pressure") {
            res <- oceSetData(res, name, data[[name]], note = NULL)
            oceDebug(debug, "added \"", name, "\" to the data slot\n", sep = "")
        }
    }
    # Add units for things not set up by as.ctd(). We know that conductivity
    # has no unit, and we know that it's present, but we check on the other
    # things.
    res@metadata$units$conductivity <- list(unit = expression(mS / cm), scale = "")
    if ("battery" %in% names(res@data)) {
        res@metadata$units$battery <- list(unit = expression(V), scale = "")
    }
    if ("depth" %in% names(res@data)) {
        res@metadata$units$depth <- list(unit = expression(m), scale = "")
    }
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep = "", collapse = ""))
    oceDebug(debug, "END read.ctd.aml()\n", unindent = 1)
    res
}

# Read AML ctd format 3 (not exported)
#
# This is an ad-hoc attempt to read files provided by a user
# in late September, 2024.  See \dQuote{Details} for some
# provisos.
#
# This function was based on 4 sample files, evidently created
# with AML Sailfish 1.4.8.0 software. No documentation was
# made available, so the code was written by inspection
# of the files and some guessing on the format.  This means
# that the code is likely to be brittle against
# file variations.
#
# It is not envisioned that much support will be provided for
# this file format, given the lack of documentation.  This is the
# third format seen for AML files, and it seems likely that there
# are other formats in existence. Another factor mitigating against
# oce adding high support for this format is the
# fact that the files made available to the author contain
# startling errors in the stated units of for density and sound
# speed, which raises questions about the development
# state of the AML software.
#
# @param file character value naming a file.
#
# @param encoding ignored.
#
# @param debug ignored.
#
# @author Dan Kelley
read.ctd.aml.type3 <- function(file, encoding, debug = 0) {
    debug <- max(0L, as.integer(debug))
    oceDebug(debug, "read.ctd.aml.type3(...) START [unexported function]\n", sep = "", unindent = 1)
    if (is.character(file) && 0 == file.info(file)$size) {
        stop("empty file")
    }
    filename <- ""
    if (is.character(file)) {
        filename <- file
        file <- file(file, "r", encoding = encoding)
        on.exit(close(file))
    }
    if (!inherits(file, "connection")) {
        stop("argument `file' must be a character string or connection")
    }
    if (!isOpen(file)) {
        open(file, "r", encoding = encoding)
        on.exit(close(file))
    }
    getMetadata <- function(name, numeric = TRUE, default = NA) { # name is in title-case
        w <- grep(paste0("^", name, "="), lines)
        wlen <- length(w)
        if (wlen == 0) {
            default
        } else {
            if (numeric) as.numeric(gsub(".*=", "", lines[w])) else gsub(".*=", "", lines[w])
        }
    }
    lines <- readLines(file, warn = FALSE)
    # Get metadata (FIXME: possibly get other items)
    longitude <- getMetadata("Longitude")
    latitude <- getMetadata("Latitude")
    oceDebug(debug, sprintf("location: %.3fN %.3fE\n", latitude, longitude))
    # Get data
    w <- grep("^Columns=", lines)
    oceDebug(debug, "column names are in line ", w, "\n")
    col.names <- strsplit(gsub(".*=", "", lines[w]), ",")[[1]]
    w <- grep("\\[MeasurementData\\]", lines)
    oceDebug(debug, "data start at line ", w, "\n")
    #print(lines[w + seq(-1, 1)])
    data <- read.csv(text = lines, skip = w, header = FALSE,
        col.names = col.names, encoding = encoding
    )
    #message("data:")
    #print(data)
    rval <- as.ctd(
        salinity = data$Salinity, temperature = data$Temperature,
        pressure = data$Pressure, latitude = latitude, longitude = longitude
    )
    rval@metadata$filename <- filename
    rval@metadata$header <- lines[seq(1L, w - 1L)]
    for (name in col.names) {
        if (!name %in% c("Salinity", "Temperature", "Pressure", "Date", "Time")) {
            rval <- oceSetData(rval, tolower(name), data[[name]],
                note = paste("Add", tolower(name))
            )
        }
    }
    if ("Date" %in% col.names && "Time" %in% col.names) {
        time <- as.POSIXct(paste(data[["Date"]], data[["Time"]]), tz = "UTC")
        rval <- oceSetData(rval, "time", time, note = "Add time")
    }
    w <- grep("^Units=", lines)
    oceDebug(debug, "units start at line ", w, "\n")
    units <- strsplit(gsub("^Units=", "", lines[w]), ",")[[1]]
    # FIXME: detect incorrect units, and report if found
    for (i in seq_along(col.names)) {
        if (!col.names[i] %in% c("Salinity", "Temperature", "Pressure", "Date", "Time", "time")) {
            rval@metadata$units[[tolower(col.names[i])]] <- units[i]
        }
    }
    oceDebug(debug, "END read.ctd.aml.type3() [unexported function]\n", sep = "", unindent = 1)
    rval
}
