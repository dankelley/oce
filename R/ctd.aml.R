# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Read an AML CTD file
#'
#' [read.ctd.aml()] reads files that hold data acquired with an
#' AML Oceanographic BaseX2 CTD instrument. In its present form,
#' the function only works for .txt files that were created with
#' certain settings in the AML software, as explained in
#' \sQuote{Details} section.
#' See \sQuote{Alternatives to this function} for hints on dealing
#' with files that do not match the [read.ctd.aml()]
#' requirements.
#'
#' The processing depends on the `format` parameter.  There are *many*
#' varieties of formatting that are possible for AML files, and
#' [read.ctd.aml()] interprets only a few.  In each case, it is mandatory
#' that the file contain the columns named: `Date`, `Time`, `Conductivity
#' (mS/cm)`, `Temperature (C)` and `Pressure (dBar)`.  (Other quantities
#' may also be named, in addition.)  It must also contain
#' values for `Longitude` and `Latitude`.  An error results for
#' files that lack any of these quantities.  The permitted formats are as
#' follows.  The default, `format=1`, is *greatly* preferred, because it
#' is more robust (see below on `format=2`) and it can be read by the
#' AML Seacast software.
#'
#' 1. If `format` is `1` then the file is in a format created by selecting
#' **Export As ... Seacast (.csv)** in AML's Seacast software, with
#' pressure (not depth) selected as an output variable, and with
#' date, time, temperature and conductivity also selected for output.
#' This format is recognized in the following way.
#' The first line must be `[cast header]`.
#' Following that must be a header, of which the only elements that
#' are parsed individually are longitude, latitude and
#' instrument serial number. These values are in separate lines,
#' e.g. `longitude=-20` for a location at 20W.  Other header information
#' follows, and this is stored, as-is, as `header` in the `metadata` slot
#' of the returned value.
#' The actual data are stored in comma-separated format, and begin
#' just after a line containing the string `[data]`. The first
#' data line is a comma-separated list of variable names, and the
#' following lines hold the variable values, separated by commas.  In
#' other words, the material following the `[data]` line is in a conventional
#' format as used by spreadsheet software.
#'
#' 2. If `format` is `2` then the file is another of the many formats
#' that can be created with the AML Seacast software.  Since this format
#' is not recommended, the details of the Seacast settings will not be listed
#' here.  The file format is recognized by the first line
#' containing the names of the data columns, separated by commas. This
#' is followed by other information on things such as location,
#' expressed somewhat analogously with `format` 1, but using
#' lower-case variable names. Other background information is
#' stored, unparsed, in `header` in the `metadata` slot. After that
#' sequence, the file ends with the dataset itself, recognized by
#' having the same number of comma-separated
#' tokens as the naming line.  (Obviously, this matching by comma
#' counting is a risky procedure, for a user might have saved a comment with
#' that number of commas.  This is one reason why this data format
#' is not recommended.)
#'
#' In both cases, location information and serial number are stored
#' in the metadata (as is the case for general [ctd-class] objects),
#' and the data columns, renamed to oce convention, are stored in
#' the `data` slot.
#' Since the `header` item in the `metadata` slot contains the full
#' header, users are free to do further processing, as befits their
#' application.
#'
#'
#' @section Alternatives to this function:
#' If [read.ctd.aml()] fails to read a user's data file, all is
#' not lost, because a bit of work in a plain-text editor is likely
#' to reveal the data structure, so a user can extract elements
#' to be used with [as.ctd()] using procedures akin to those listed
#' in the \sQuote{Details} section.  Of course, the file must contain
#' conductivity, temperature and pressure (or depth, from which pressure
#' can be computed *if* an assumption is made about the density used
#' within the AML software to compute depth from pressure)
#' because these three things are needed for
#' [ctd-class] objects.  Adding longitude and latitude to the [as.ctd()]
#' call is recommended, if the desire is to use the modern TEOS-10
#' equation of state.
#'
#' @param file a connection or a character string giving the name of the file to
#' load.
#'
#' @param format an integer indicating the format type.  If not supplied,
#' [read.ctd.aml()] reads the first line, to see whether the file seems
#' to be of `format=1` or `format=2` style. \sQuote{Details}.
#'
#' @param debug an integer controlling whether to print debugging
#' information during processing. Use 0 for quiet processing, or
#' any positive number to see some output.
#'
#' @param processingLog ignored.
#'
#' @param ... ignored.
#'
#' @return [read.ctd.aml()] returns a [ctd-class] object.
#'
#' @author Dan Kelley
#'
#' @family functions that read ctd data
read.ctd.aml <- function(file, format,
    debug=getOption("oceDebug"), processingLog, ...)
{
    debug <- max(0L, as.integer(debug))
    oceDebug(debug, "read.ctd.aml() {\n", unindent=1, style="bold")
    if (missing(file))
        stop("must provide a file")
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
        res <- NULL
        if (length(l) > 0L) {
            if (length(l) > 1L)
                oceDebug(debug, "using first of ", length(l), " values\n")
            # NOTE: we take first definition, ignoring others
            item <- lines[l[1]]
            res <- strsplit(lines[l], "=")[[1]][2]
            if (numeric)
                res <- as.numeric(res)
            else
                res <- trimws(res)
        } else {
            NULL
        }
        oceDebug(debug, "returning ", if (is.null(res)) "NULL" else res, "\n")
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
    serialNumber <- NA
    longitude <- getMetadataItem(lines, "longitude", ignore.case=TRUE, debug=debug-1L)
    if (is.null(longitude))
        longitude <- getMetadataItem(lines, "lon", ignore.case=TRUE, debug=debug-1L)
    latitude <- getMetadataItem(lines, "latitude", ignore.case=TRUE, debug=debug-1L)
    if (is.null(latitude))
        latitude <- getMetadataItem(lines, "lat", ignore.case=TRUE, debug=debug-1L)
    serialNumber <- getMetadataItem(lines, "sn", ignore.case=TRUE, debug=debug-1L)
    if (is.null(longitude) || is.null(latitude)) {
        oceDebug(debug, "cannot determine location\n")
        warning("cannot determine location\n")
    } else {
        oceDebug(debug, "inferred location ", longitude, "E, ", latitude, "N, ", " serialNumber ", serialNumber, "\n", sep="")
    }
    #?Date <- getMetadataItem(lines, "Date", numeric=FALSE)
    #?Time <- getMetadataItem(lines, "Time", numeric=FALSE)
    #?time <- as.POSIXct(paste(Date, Time), tz="UTC")
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
    if (!("Date" %in% col.names))
        stop("no 'Date' column found")
    if (!("Time" %in% col.names))
        stop("no 'Time' column found")
    if (!("Temperature (C)" %in% col.names))
        stop("no 'Temperature (C)' column found")
    if (!("Conductivity (mS/cm)" %in% col.names))
        stop("no 'Conductivity (mS/cm)' column found")
    if (!("Pressure (dBar)" %in% col.names))
        stop("No 'Pressure (dBar)' column found")
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
    time <- as.POSIXct(paste(data$Date, data$Time), tz="UTC")
    #if (!("pressure" %in% names(data)) && "depth" %in% names(data)) {
    #    data$pressure <- swPressure(data$depth, latitude)
    #    oceDebug(debug, "computed pressure from depth (assuming saltwater formula)\n")
    #}
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
    res@data$time <- time
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
