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
#' The sample file upon which [read.ctd.aml()] is based starts
#' with a line containing
#'```
#'Date,Time,Conductivity (mS/cm),Temperature (C),Depth (m),Battery (V)
#'```
#' and this must also be true of any file provided to the function.
#' The entries of this line are the names of columnar data that appear later
#' in the file, in comma-separated format.
#' Those columns are assumed to start two lines below
#' a line starting with
#'```
#'Comments:
#'```
#' and the lines above that are copied to an item named `header` in the
#' `metadata` slot of the returned value.  That slot also
#' holds `Longitude` and `Latitude`, inferred from the header, along
#' with some other items.
#'
#' @section Alternatives to this function:
#' It is not difficult to read files that fail to meet the requirements of
#' [read.ctd.aml()].  The first step is to examine the file (*without*
#' modifying it) using a text-based editor.  With this, it should be
#' easy to recognize a header at the start of the file. Take
#' note of the Longitude and Latitude, which you will need later,
#' along with any other information (e.g. serial numbers, etc.) that
#' you deem to be of interest.
#'
#' Next, find the line where the columnar data start.  This will be used as the
#' `skip` argument in a call to e.g. [read.csv()] or [read.delim()] that you
#' will use to read the data.  Use `header=FALSE` in that function call, so that
#' the resultant value will be an object with entries named `V1`, `V2`, etc.  You
#' must determine the meaning of those columns, from the header or from some
#' other information.
#'
#' You will be creating a [ctd-class] object with [as.ctd()]. This function
#' needs salinity, temperature, and pressure.  For modern computations phrased
#' in terms of the TEOS-10 equation of state, it also needs longitude and
#' latitude.  Pressure may be computed with [swPressure()], and
#' salinity with [swSCTp()]. Given all these values, the
#' final step is to use [as.ctd()] to create a [ctd-class] object.
#'
#' @param file a connection or a character string giving the name of the file to
#' load.
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
    if (length(col.names) < 3L) # need at least C, T and p/z
        stop("the first line, shown below, is not in the expected form.\n",lines[1])
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
    p <- swPressure(data[["Depth..m."]], latitude)
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
