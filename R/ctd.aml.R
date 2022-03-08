# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Read an AML CTD file
#'
#' [read.ctd.aml()] reads files that hold data acquired with an
#' AML Oceanographic BaseX2 CTD instrument. In its present form,
#' the fuction only works for .txt files that were created with
#' certain settings in the AML software, as explained in
#' \sQuote{Details} section.
#' See \sQuote{Alternatives to this function} for hints on dealing
#' with files that do not match the [read.ctd.aml()]
#' requirements.
#'
#' The first step in processing is to examine the first line of
#' the file, and to interpret it as a comma-separated list of column
#' names.  If this list does not contain each of `"Date"`,
#' `"Time"`, `"Conductivity (mS/cm)"` and `"Temperature (C)"`,
#' then an error message is printed.
#'
#' The next step is to break up each line in the file into
#' comma-separated elements, and to take note of the lines
#' that have the same number of elements as the first line.
#' Those lines are considered to be data, and are read with
#' [read.csv()].  Lines after the first, and before the first
#' data line are stored as `header` in the `metadata` slot
#' of the returned value, while the data (with columns renamed
#' to match oce conventions) are stored in the `data` slot.
#' If pressure is not found in the data, but depth is, then
#' pressure is computed with [swPressure()] and stored in the
#' `data` slot.  Salinity is then inferred with [swSCTp()]
#' and inserted into the `data` slot.
#'
#' Some extra processing is done to take account of units,
#' and to extract certain elements from the header, particularly
#' position (stored as `longitude` and `latitude` in the `metadata`
#' slot), and these are inserted into the return value, which
#' is constructed with [as.ctd()].
#'
#' @section Alternatives to this function:
#' If [read.ctd.aml()] fails to read a user's data file, all is
#' not lost, because a bit of work in a plain-text editor is likely
#' to reveal the data structure, so a user can extract elements
#' to be used with [as.ctd()] using procedures akin to those listed
#' in the \sQuote{Details} section.  Of course, the file must contain
#' conductivity, temperature and pressure (or depth, from which pressure
#' can be computed) because these three things are needed for
#' [ctd-class] objects.  Adding longitude and latitude to the [as.ctd()]
#' call is recommended, if the desire is to use the modern TEOS-10
#' equation of state.
#'
#' @param file a connection or a character string giving the name of the file to
#' load.
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
read.ctd.aml <- function(file, debug=getOption("oceDebug"),
    processingLog, ...)
{
    debug <- max(0L, as.integer(debug))
    oceDebug(debug, "read.ctd.aml() {\n", unindent=1, style="bold")
    if (!missing(file) && is.character(file) && 0 == file.info(file)$size)
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
    getMetadataItem <- function(lines, name, numeric=TRUE, debug=0)
    {
        oceDebug(debug, vectorShow(name))
        l <- grep(paste0("^",name,"="), lines)
        oceDebug(debug, vectorShow(l))
        res <- NULL
        if (length(l) > 0L) {
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
        res
    }
    lines <- readLines(file, encoding="UTF-8-BOM", warn=FALSE)
    oceDebug(debug, "read ", length(lines), " lines in this file\n")
    # FIXME: add other relevant metadata here.  This will require some
    # familiarity with the typical contents of the metadata.  For example,
    # I see 'SN' and 'BoardSN', and am inferring that we want to save
    # the first, but maybe it's the second...
    longitude <- getMetadataItem(lines, "Longitude", debug=debug)
    latitude <- getMetadataItem(lines, "Latitude", debug=debug)
    serialNumber <- getMetadataItem(lines, "SN", debug=debug)
    #?Date <- getMetadataItem(lines, "Date", numeric=FALSE)
    #?Time <- getMetadataItem(lines, "Time", numeric=FALSE)
    #?time <- as.POSIXct(paste(Date, Time), tz="UTC")
    col.names <- strsplit(lines[1], ",")[[1]]
    oceDebug(debug, "original column names: c(\"", paste(col.names, collapse="\", \""), "\")\n")
    if (length(col.names) < 3L) # need at least C, T and p/z
        stop("the first line, shown below, is not in the expected form.\n",lines[1])
    if (!("Date" %in% col.names))
        stop("No 'Date' column found")
    if (!("Time" %in% col.names))
        stop("No 'Time' column found")
    if (!("Temperature (C)" %in% col.names))
        stop("No 'Temperature (C)' column found")
    if (!("Conductivity (mS/cm)" %in% col.names))
        stop("No 'Conductivity (mS/cm)' column found")
    col.names[col.names == "Temperature (C)"] <- "temperature"
    col.names[col.names == "Conductivity (mS/cm)"] <- "conductivity"
    col.names[col.names == "Depth (m)"] <- "depth"
    col.names[col.names == "Pressure (dBar)"] <- "pressure"
    col.names[col.names == "Battery (V)"] <- "battery"
    oceDebug(debug, "transformed column names: c(\"", paste(col.names, collapse="\", \""), "\")\n")
    nfields <- length(col.names)
    nfield <- unlist(lapply(lines, function(l) length(strsplit(l, ",")[[1]])))
    look <- nfield == nfield[1]
    header <- lines[seq(1L, which(look)[2]-1L)]
    look[1] <- FALSE
    data <- read.csv(text=lines[look], header=FALSE, col.names=col.names)
    time <- as.POSIXct(paste(data$Date, data$Time), tz="UTC")
    if (!("pressure" %in% names(data)) && "depth" %in% names(data)) {
        data$pressure <- swPressure(data$depth, latitude)
        oceDebug(debug, "computed pressure from depth (assuming saltwater formula)\n")
    }
    S <- swSCTp(conductivity=data$conductivity,
        temperature=data$temperature, pressure=data$pressure,
        conductivityUnit="mS/cm", eos="unesco")
    res <- as.ctd(salinity=S, temperature=data$temperature,
        pressure=data$pressure, conductivity=data$conductivity,
        longitude=longitude, latitude=latitude,
        serialNumber=serialNumber)
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
