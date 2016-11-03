## vim: tw=120 shiftwidth=4 softtabstop=4 expandtab:


#' Read observatory data file
#' 
#' This is a preliminary version of a function that may be extended or
#' deleted in a future version of Oce.  Part of the uncertainty is that the
#' format was inferred from inspection of files, and this is a dangerous
#' procedure, path because the data archiver might alter formats in future.
#' 
#' @param file a connection or a character string giving the name of the file
#' to load.
#' @param type type of data; must be \code{"ctd"}, the only type handled at
#' present.
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, or to 2 to get more.
#' @param processingLog if provided, the action item to be stored in the log.
#' (Typically only provided for internal calls; the default that it provides is
#' better for normal calls by a user.)
#' @param \dots additional arguments, passed to called routines.
#' @return An object of \code{\link[base]{class}} \code{"ctd"}.
#' @author Dan Kelley
#' @seealso The documentation for \code{\link{ctd-class}} explains the
#' structure of \code{ctd} objects, and also outlines the other functions
#' dealing with them.
#' @references \url{http://venus.uvic.ca/data/data-archive/}
read.observatory <- function(file, 
                    type=c("ctd"),
                    debug=getOption("oceDebug"), processingLog, ...)
{
    if (debug > 2) debug <- 2
    if (debug < 0) debug  <- 0
    oceDebug(debug, "read.observatory(file=\"",file, "\", ...) {\n", sep="", unindent=1)
    type <- match.arg(type)
    if (type == "ctd") {
        read.observatory.ctd(file=file, 
                   debug=debug-1, processingLog=processingLog, ...)
    } else {
        stop("unknown type of observatory data")
    }
}


#' Read observatory data CTD file
#' 
#' This is a preliminary version of a function that may be extended or
#' deleted in a future version of Oce.  Part of the uncertainty is that the
#' format was inferred from inspection of files, and this is a dangerous
#' procedure, path because the data archiver might alter formats in future.
#' 
#' @param file a connection or a character string giving the name of the file
#' to load.
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, or to 2 to get more.
#' @param processingLog if provided, the action item to be stored in the log.
#' (Typically only provided for internal calls; the default that it provides is
#' better for normal calls by a user.)
#' @param \dots additional arguments, passed to called routines.
#' @return An object of \code{\link[base]{class}} \code{"ctd"}.
#' @author Dan Kelley
#' @seealso The documentation for \code{\link{ctd-class}} explains the
#' structure of \code{ctd} objects, and also outlines the other functions
#' dealing with them.
#' @references \url{http://venus.uvic.ca/data/data-archive/}
#' @examples
#' library(oce)
#' \dontrun{
#' ctd <- read.oce("data.txt")
#' ## mooring data are not profiles, so show timeseries
#' par(mfrow=c(3,1))
#' oce.plot.ts(ctd[["time"]], ctd[["pressure"]])
#' oce.plot.ts(ctd[["time"]], ctd[["salinity"]])
#' oce.plot.ts(ctd[["time"]], ctd[["temperature"]])
#' }
read.observatory.ctd <- function(file, 
                                 debug=getOption("oceDebug"), processingLog, ...)
{
    if (debug > 1)
        debug <- 1
    oceDebug(debug, "read.observatory.ctd(file=\"",file,
              "\", ...) {\n", sep="", unindent=1)
    if (is.character(file)) {
        filename <- fullFilename(file)
        file <- file(file, "r")
        on.exit(close(file))
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        filename <- "(connection)"
        open(file, "rb")
        on.exit(close(file))
    }
    lines <- readLines(file)
    location <- lines[grep("Latitude", lines)]
    latitude <- as.numeric(gsub(" .*$", "", gsub("^.*Latitude\\(decdeg N\\): ", "", location)))
    longitude <- -as.numeric(gsub(" .*$", "", gsub("^.*Longitude\\(decdeg W\\): ", "", location)))
    waterDepth <- as.numeric(gsub("^.*Depth\\(m\\): ", "", location)) ## FIXME: is this actually on that line?
    check <- min(100, length(lines))
    headerStartEnd <- grep('^%', lines[1:check])
    d <- read.csv(text=lines[-(1:headerStartEnd[2])])
    colNames <- strsplit(lines[headerStartEnd[2]+1], "[ ]*,[ ]*")[[1]] 
    time <- strptime(d[,1], "%Y-%m-%dT%H:%M:%S",tz="UTC")
    Scol <- grep("^Practical Salinity \\(psu\\)", colNames)
    Tcol <- grep("^Temperature \\(C\\)", colNames)
    pcol <- grep("^Pressure \\(decibar\\)", colNames)
    salinity <- d[,Scol]
    temperature <- d[,Tcol]
    pressure  <- d[,pcol]
    res <- as.ctd(salinity=salinity, temperature=temperature, pressure=pressure, time=time,
                  latitude=latitude, longitude=longitude)
    res <- oceSetMetadata(res, name="waterDepth", value=waterDepth)
    res@metadata$filename <- filename
    oceDebug(debug, "} # read.observatory.ctd()\n", unindent=1)
    res
}

