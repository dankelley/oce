#' Read an AML CTD file
#'
#' This function CTD "txt" formatted data from an AML Oceanographic BaseX2 instrument.
#' It is in a preliminary form, based on inferences made by inspecting
#' a user-supplied data file; see \dQuote{Details}.
#'
#' [read.ctd.aml()] makes assumptions about the data columns
#' that might not be true for all files. For example, it assumes that
#' depth is presented, not pressure, and that the unit is metres.
#' Indeed, it requires that the name of the depth column (read in as
#' the first line in the file), is `"Depth (m)"`; any other string
#' will result in an error.  Similarly, conductivity must be provided,
#' in a column named `"Conductivity (mS/cm)"`, in-situ
#' temperature must be provided in a column named `"Temperature (C)"`,
#' observation date (in yyyy-mm-dd format) must be provided in a column
#' named `"Date"`, and time (in hh-mm-ss format, assumed in UTC).
#' must be provided in a column named `"Time"`. If any of these requirements
#' is not met, an error is reported.
#'
#' Assumptions are also made about the metadata.  For example, the supplied
#' test file had two entries for Longitude, and two for Latitude, which
#' perhaps relate to starting and ending locations, but only the first location
#' is stored by [read.ctd.aml()].  Also, although files have serial numbers
#' for multiple items called "Slots", only the one named `"SN"` is used
#' here, and saved as `serialNumber` in the metadata.
#'
#' The header (contents up to and including the `Comments=` line in the file)
#' is stored as `header` in the `metadata` slot of the returned value.
#' The data are assumed to start 2 lines below this `Comments=` line.
#'
#' @param file a connection or a character string giving the name of the file to
#' load.  This must be in the "txt" format, not the "csv" format.
#'
#' @template debugTemplate
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
    getMetadataItem <- function(lines, name, numeric=TRUE)
    {
        l <- grep(paste0("^",name,"="), lines)
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
    # FIXME: add other relevant metadata here.  This will require some
    # familiarity with the typical contents of the metadata.  For example,
    # I see 'SN' and 'BoardSN', and am inferring that we want to save
    # the first, but maybe it's the second...
    longitude <- getMetadataItem(lines, "Longitude")
    latitude <- getMetadataItem(lines, "Latitude")
    serialNumber <- getMetadataItem(lines, "SN")
    Date <- getMetadataItem(lines, "Date", numeric=FALSE)
    Time <- getMetadataItem(lines, "Time", numeric=FALSE)
    time <- as.POSIXct(paste(Date, Time), tz="UTC")
    col.names <- strsplit(lines[1], ",")[[1]]
    CommentsLine <- grep("^Comments=", lines)
    oceDebug(debug, "CommentsLine=", CommentsLine, "\n")
    header <- lines[seq(1L, CommentsLine)]
    data <- read.csv(text=lines, skip=CommentsLine+1, header=FALSE, col.names=col.names)
    Date <- data[["Date"]]
    if (is.null(Date))
        stop("Date not found, because no column was named \"Date\"")
    Time <- data[["Time"]]
    if (is.null(Time))
        stop("Time not found, because no column was named \"Time\"")
    time <- as.POSIXct(paste(Date, Time), tz="UTC")
    T <- data[["Temperature..C."]]
    if (is.null(T))
        stop("temperature not found, because no column was named \"Temperature (C)\"")
    C <- data[["Conductivity..mS.cm."]]
    if (is.null(T))
        stop("conductivity not found, because no column was named \"Conductivity (mS/cm)\"")
    p <- swPressure(data[["Depth..m."]])
    if (is.null(T))
        stop("depth not found, because no column was named \"Depth (m)\"")
    S <- swSCTp(conductivity=C, temperature=T, pressure=p, conductivityUnit="mS/cm", eos="unesco")
    res <- as.ctd(salinity=S, temperature=T, pressure=p, conductivity=C,
        longitude=longitude, latitude=latitude,
        serialNumber=serialNumber)
    res@metadata$filename <- filename
    res@metadata$header <- header
    res@data$time <- time
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))

    oceDebug(debug, "} # read.ctd.aml() {\n", unindent=1, style="bold")
    res
}
