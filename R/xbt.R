## vim:textwidth=150:expandtab:shiftwidth=4:softtabstop=4

#' Class to Store XBT Data
#'
#' This class stores expendable bathythermograph (XBT) data, e.g. from a Sippican
#' device.  Reference 1 gives some information on Sippican
#' devices, and reference 2 is a useful introduction to the
#' modern literature on XBTs in general.
#'
#' @references
#' 1. Sippican, Inc. "Bathythermograph Data Acquisition System: Installation, Operation and Maintenance
#' Manual (P/N 308195, Rev. A)," 2003.
#' https://pages.uoregon.edu/drt/MGL0910_Science_Report/attachments/MK21_ISA_Manual_Rev_A.pdf.
#'
#' 2. Cheng, Lijing, John Abraham, Gustavo Goni, Timothy Boyer, Susan Wijffels, Rebecca
#' Cowley, Viktor Gouretski, et al. "XBT Science: Assessment of Instrumental Biases and Errors."
#' Bulletin of the American Meteorological Society 97, no. 6 (June 2016): 924-33.
#' \code{10.1175/BAMS-D-15-00031.1}
#'
#' @templateVar class xbt
#'
#' @templateVar dataExample The key items stored in this slot are `depth` (or `z`) and `temperature`, although some datasets also have `soundSpeed`.  Note that `depth` and `z` are inferred from time in water, using an empirical formula for instrument descent rate, and that `soundSpeed` is #' calculated using a fixed  practical salinity of 35. Note that the `[[` accessor will compute any of `depth`, `z` or `pressure`, based on whatever is in the data object.  Similarly, `soundspeed` will compute sound speed (assuming a practical salinity of 35), if that that item is present in the `data` slot.
#'
#' @templateVar metadataExample {}
#'
#' @template slot_summary
#'
#' @template slot_put
#'
#' @template slot_get
#'
#' @family things related to xbt data
#' @family classes provided by oce
#'
#' @author Dan Kelley
setClass("xbt", contains="oce")

#' An XBT Object
#'
#' An [xbt-class] object created by using [read.xbt()] on a Sippican file created by extracting the near-surface
#' fraction of the sample provided in Section 5.5.6 of reference 1.
#'
#' @name xbt
#'
#' @docType data
#'
#' @usage data(xbt)
#'
#' @examples
#' library(oce)
#' data(xbt)
#' summary(xbt)
#' plot(xbt)
#'
#' @references
#' 1. Sippican, Inc. "Bathythermograph Data Acquisition System: Installation, Operation and Maintenance
#' Manual (P/N 308195, Rev. A)," 2003.
#' https://pages.uoregon.edu/drt/MGL0910_Science_Report/attachments/MK21_ISA_Manual_Rev_A.pdf.
#'
#' @family datasets provided with oce
#' @family things related to xbt data
NULL

#' Extract Something From an xbt Object
#'
#' @param x an [xbt-class] object.
#'
#' @section Details of the Specialized Method:
#'
#' * If `i` is `"?"`, then the return value is a list
#' containing four items, each of which is a character vector
#' holding the names of things that can be accessed with `[[`.
#' The `data` and `metadata` items hold the names of
#' entries in the object's data and metadata
#' slots, respectively. The `dataDerived`
#' and `metadataDerived` items are each NULL, because
#' no derived values are defined by `cm` objects.
#'
#' @template sub_subTemplate
#'
#' @author Dan Kelley
#'
#' @family things related to xbt data
setMethod(f="[[",
          signature(x="xbt", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              dataNames <- names(x@data)
              S0 <- 35
              metadataDerived <- NULL
              dataDerived <- c("z", "pressure")
              if (i == "?")
                  return(list(metadata=sort(names(x@metadata)),
                          metadataDerived=sort(metadataDerived),
                          data=sort(names(x@data)),
                          dataDerived=sort(dataDerived)))
              if (i == "depth") {
                  if ("depth" %in% dataNames)
                      x@data$depth
                  else if ("z" %in% dataNames)
                      -x@data$depth
                  else if ("pressure" %in% dataNames)
                      swDepth(x@data$pressure, latitude=x@metadata$latitude)
              } else if (i == "z") {
                  if ("depth" %in% dataNames)
                      -x@data$depth
                  else if ("z" %in% dataNames)
                      x@data$z
                  else if ("pressure" %in% dataNames)
                      -swDepth(x@data$pressure, latitude=x@metadata$latitude)
              } else if (i == "pressure") {
                  if ("depth" %in% dataNames)
                      swPressure(depth=x@data$depth, latitude=x[["latitude"]])
                  else if ("z" %in% dataNames)
                      swPressure(depth=-x@data$z, latitude=x[["latitude"]])
                  else if ("pressure" %in% dataNames)
                      x@data$pressure
              } else if (i == "soundSpeed") {
                  if ("soundSpeed" %in% dataNames)
                      x@data$soundSpeed
                  else
                      swSoundSpeed(S0, x[["temperature"]], x[["pressure"]])
              } else {
                  callNextMethod()
              }
          })

#' Replace Parts of an xbt Object
#'
#' @param x an [xbt-class] object.
#'
#' @template sub_subsetTemplate
#'
#' @family things related to xbt data
setMethod(f="[[<-",
          signature(x="xbt", i="ANY", j="ANY"),
          definition=function(x, i, j, ..., value) {
              callNextMethod(x=x, i=i, j=j, ...=..., value=value) # [[<-
          })

setMethod(f="initialize",
          signature="xbt",
          ## the only thing we know for sure is that temperature will be given
          definition=function(.Object, z=NULL, depth=NULL, temperature=NULL, units, ...) {
              .Object <- callNextMethod(.Object, ...)
              if (!is.null(depth) && !is.null(z))
                  stop("cannot initialize XBT with both depth and z")
              if (missing(units)) {
                  if (!is.null(temperature))
                      .Object@metadata$units$temperature <- list(unit=expression(degree*C), scale="ITS-90")
                  if (!is.null(z))
                      .Object@metadata$units$z <- list(unit=expression(m), scale="")
                  else if (!is.null(depth))
                      .Object@metadata$units$depth <- list(unit=expression(m), scale="")
              } else {
                  .Object@metadata$units <- units # CAUTION: we are being quite trusting here
              }
              if (!is.null(depth))
                  .Object@data$depth <- depth
              if (!is.null(z))
                  .Object@data$z <- z
              if (!is.null(temperature))
                  .Object@data$temperature <- temperature
              .Object@processingLog$time <- presentTime()
              .Object@processingLog$value <- "create 'xbt' object"
              return(.Object)
          })

#' Summarize an xbt Object
#'
#' Summarizes some of the data in a `xbt` object.
#'
#' @param object A [xbt-class] object.
#'
#' @param ... Further arguments passed to or from other methods.
#'
#' @seealso The documentation for the [xbt-class] class explains the structure
#' of `xbt` objects, and also outlines the other functions dealing with them.
#'
#' @family things related to xbt data
#'
#' @author Dan Kelley
setMethod(f="summary",
          signature="xbt",
          definition=function(object, ...) {
              cat("xbt summary\n-----------\n\n", ...)
              showMetadataItem(object, "filename",      "File source:        ", quote=TRUE)
              showMetadataItem(object, "type",          "Instrument type:    ")
              showMetadataItem(object, "model",         "Instrument model:   ")
              showMetadataItem(object, "serialNumber",  "Serial Number:      ")
              showMetadataItem(object, "longitude",     "Longitude:          ")
              showMetadataItem(object, "latitude",      "Latitude:           ")
              showMetadataItem(object, "time",          "Time:               ")
              invisible(callNextMethod()) # summary
          })


#' Subset an xbt Object
#'
#' This function is somewhat analogous to [subset.data.frame()].
#'
#' @param x an [xbt-class] object.
#'
#' @param subset a condition to be applied to the `data` portion of `x`.
#' See \sQuote{Details}.
#'
#' @param ... ignored.
#'
#' @return A new `xbt` object.
#'
#' @examples
#' library(oce)
#' data(xbt)
#' plot(xbt)
#' plot(subset(xbt, depth < mean(range(xbt[["depth"]]))))
#'
#' @family things related to xbt data
#' @family functions that subset oce objects
#'
#' @author Dan Kelley
setMethod(f="subset",
          signature="xbt",
          definition=function(x, subset, ...) {
              subsetString <- paste(deparse(substitute(expr=subset, env=environment())), collapse=" ")
              res <- x
              dots <- list(...)
              debug <- getOption("oceDebug")
              if (length(dots) && ("debug" %in% names(dots)))
                  debug <- dots$debug
              if (missing(subset))
                  stop("must give 'subset'")
              if (length(grep("depth", subsetString))) {
                  oceDebug(debug, "subsetting an xbt by depth\n")
                  keep <- eval(expr=substitute(expr=subset, env=environment()), envir=x@data, enclos=parent.frame(2))
                  names <- names(x@data)
                  oceDebug(debug, vectorShow(keep, "keeping bins:"))
                  res <- x
                  for (name in names(x@data)) {
                      res@data[[name]] <- x@data[[name]][keep]
                  }
              }
              res@processingLog <- processingLogAppend(res@processingLog, paste("subset.xbt(x, subset=", subsetString, ")", sep=""))
              res
          })

#' Create an xbt object
#'
#' @param z numeric vector giving vertical coordinates of measurements. This is the negative of
#' depth, i.e. `z` is 0 at the air-sea interface, and negative within the water column.
#'
#' @param temperature numeric vector giving in-situ temperatures at the `z` values.
#'
#' @param longitude,latitude location in degE and degN.
#'
#' @param filename character value naming source file.
#'
#' @param sequenceNumber numerical value of the sequence number of the XBT drop.
#'
#' @param serialNumber character value holding the serial number of the XBT.
#'
#' @return An [xbt-class] object.
#'
#' @family things related to xbt data
#'
#' @author Dan Kelley
as.xbt <- function(z, temperature, longitude=NA, latitude=NA, filename="", sequenceNumber=NA,
                   serialNumber="")
{
    if (missing(z))
        stop("must provide z")
    if (missing(temperature))
        stop("must provide temperture")
    if (length(z) != length(temperature))
        stop("length of z (", length(z), ") does not match length of temperature (", length(temperature), ")")
    res <- new("xbt", z=z, temperature=temperature)
    ## res@data$z <- z
    ## res@metadata$units$z <- list(unit=expression(m), scale="")
    ## res@metadata$units$temperature <- list(unit=expression(degree*C), scale="ITS-90")
    ## res@data$temperature <- temperature
    res@metadata$dataNamesOriginal <- list(z="", temperature="")
    res@metadata$longitude <- longitude
    res@metadata$latitude <- longitude
    res@metadata$header <- ""
    res@metadata$filename <- filename
    res@metadata$sequenceNumber <- sequenceNumber
    res@metadata$serialNumber <- serialNumber
    res
}

#' Read an xbt file
#'
#' Two file types are handled: (1) the `"sippican"` format, used
#' for Sippican XBT files, handled with [read.xbt.edf()], and (2)
#' the `"noaa1"` format, handled with [read.xbt.noaa1()]. The first of
#' these is recognized by [read.oce()], but the second must be called
#' directly with [read.xbt.noaa1()].
#'
#' @param file a connection or a character string giving the name of the file to
#' load.
#'
#' @param type character string indicating type of file, with valid choices being
#' `"sippican"` and `"noaa1"`.
#'
#' @param longitude,latitude optional signed numbers indicating the longitude in degrees
#' East and latitude in degrees North. These values are used if `type="sippican"`,
#' but ignored if `type="noaa1"`, because those files contain location information.
#'
#' @param debug a flag that turns on debugging.  The value indicates the depth
#' within the call stack to which debugging applies.
#'
#' @param processingLog if provided, the action item to be stored in the log.  This
#' parameter is typically only provided for internal calls; the default that it
#' provides is better for normal calls by a user.
#'
#' @return An [xbt-class] object.
#'
#' @examples
#' library(oce)
#' xbt <- read.oce(system.file("extdata", "xbt.edf", package="oce"))
#' summary(xbt)
#' plot(xbt)
#'
#' @family things related to xbt data
#'
#' @references
#' 1. Sippican, Inc. "Bathythermograph Data Acquisition System: Installation, Operation and Maintenance
#' Manual (P/N 308195, Rev. A)," 2003.
#' https://pages.uoregon.edu/drt/MGL0910_Science_Report/attachments/MK21_ISA_Manual_Rev_A.pdf.
#'
#' @author Dan Kelley
read.xbt <- function(file, type="sippican", longitude=NA, latitude=NA, debug=getOption("oceDebug"), processingLog)
{
    oceDebug(debug, "read.xbt(file=\"", file, "\", type=\"", type, "\", longitude=", longitude, ", latitude=", latitude, "...) {\n", sep="", unindent=1)
    if (missing(file))
        stop("must supply 'file'")
    if (is.character(file) && "http://" != substr(file, 1, 7) && 0 == file.info(file)$size)
        stop("empty file (read.xbt)")
    type <- match.arg(type)
    res <- if (type == "sippican") {
        read.xbt.edf(file=file, longitude=longitude, latitude=latitude,
                     debug=debug-1, processingLog=processingLog)
    } else if (type == "noaa1") {
        if (!is.null(longitude))
            warning("longitude argument is ignored for type=\"noaa1\"\n")
        if (!is.null(latitude))
            warning("latitude argument is ignored for type=\"noaa1\"\n")
        read.xbt.noaa1(file=file, debug=debug-1, processingLog=processingLog)
    } else {
        stop("unknown type of current meter")
    }
    oceDebug(debug, "} # read.xbt()\n", sep="", unindent=1)
    res
}

#' Read a Sippican '.edf' format xbt file
#'
#'
#' The function was written by inspection of a particular file, and might
#' be wrong for other files; see \dQuote{Details} for a note on character
#' translation.
#'
#' The header is converted to ASCII format prior to storage in
#' the `metadata` slot, so that e.g. a degree sign in the original file will
#' become a `?` character in the `header`.  This is to prevent problems
#' with submission of `oce` to the CRAN system, which produces NOTEs
#' about UTF-8 strings in data (on some build machines, evidently depending
#' on the locale on those machines).  This character substitution
#' is at odds with the `oce` philosophy of leaving data intact, so
#' it will be reverted, if CRAN policy changes or if the developers
#' can find a way to otherwise silence the NOTE.
#'
#' @param file a connection or a character string giving the name of the file to
#' load.
#'
#' @param longitude optional signed number indicating the longitude in degrees
#' East.
#'
#' @param latitude optional signed number indicating the latitude in degrees North.
#'
#' @param debug a flag that turns on debugging.  The value indicates the depth
#' within the call stack to which debugging applies.
#'
#' @param processingLog if provided, the action item to be stored in the log.  This
#' parameter is typically only provided for internal calls; the default that it
#' provides is better for normal calls by a user.
#'
#' @return An [xbt-class] object.
#'
#' @examples
#' library(oce)
#' xbt <- read.oce(system.file("extdata", "xbt.edf", package="oce"))
#' summary(xbt)
#' plot(xbt)
#'
#' @author Dan Kelley
read.xbt.edf <- function(file, longitude=NA, latitude=NA, debug=getOption("oceDebug"), processingLog)
{
    getHeaderItem <- function(l, name) {
        res <- l[grep(name, l)]
        gsub(paste(name, "[ ]*:[ ]*", sep=""), "", res)
    }
    if (missing(file))
        stop("must supply 'file'")
    if (is.character(file) && "http://" != substr(file, 1, 7) && 0 == file.info(file)$size)
        stop("empty file (read.xbt.edf)")
    oceDebug(debug, "read.xbt(file=\"", file, "\", longitude=", longitude, ", latitude=", latitude, "...) {\n",
             sep="", unindent=1)
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
    l <- readLines(file, 200, encoding="UTF-8") # don't read whole file
    pushBack(l, file)
    ## FIXME: is this detection of the end of the header robust?
    headerEnd <- grep("^Depth \\(", l)
    if (0 == length(headerEnd))
        stop("programming error: increase #lines read for header")
    res <- new("xbt")
    ## Convert from latin1 to UTF-8, so a degree sign does not cause problems
    ## res@metadata$header <- l[1:headerEnd]
    res@metadata$header <- iconv(l[1:headerEnd], from="UTF-8", to="ASCII", sub="?")

    date <- getHeaderItem(l, "Date of Launch")
    hms <- getHeaderItem(l, "Time of Launch")
    res@metadata$time <- as.POSIXct(paste(date, hms, sep=" "),
                                    format="%m/%d/%Y %H:%M:%S", tz="UTC")
    res@metadata$serialNumber <- getHeaderItem(l, "Serial #")
    res@metadata$sequenceNumber <- as.integer(getHeaderItem(l, "Sequence #"))
    res@metadata$dataNamesOriginal <- list(depth="Depth", temperature="Temperature", soundSpeed="Sound Velocity")
    ## Some steps needed for hemispheres.
    lat <- getHeaderItem(l, "Latitude")
    lats <- strsplit(lat, " ")[[1]]
    latdeg <- as.numeric(lats[1])
    latmin <- as.numeric(gsub("[NSns]", "", lats[2]))
    res@metadata$latitude <- (latdeg + latmin/60) * ifelse(length(grep("S", lats[2])), -1, 1)
    lon <- getHeaderItem(l, "Longitude")
    lons <- strsplit(lon, " ")[[1]]
    londeg <- as.numeric(lons[1])
    lonmin <- as.numeric(gsub("[EWew]", "", lons[2]))
    res@metadata$longitude <- (londeg + lonmin/60) * ifelse(length(grep("W", lons[2])), -1, 1)
    res@metadata$probeType <- getHeaderItem(l, "Probe Type")
    res@metadata$terminalDepth <- as.numeric(gsub("[ ]*m$", "", getHeaderItem(l, "Terminal Depth"))) # FIXME: assumes metric
    res@data <- as.list(read.table(file, skip=headerEnd+1,
                                   col.names=c("depth", "temperature", "soundSpeed"),
                                   encoding="UTF-8"))
    res@metadata$filename <- filename
    res@metadata$units$depth <- list(unit=expression(m), scale="")
    res@metadata$units$temperature <- list(unit=expression(degree*C), scale="ITS-90")
    res@metadata$units$soundSpeed <- list(unit=expression(m/s), scale="")
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # read.xbt.edf()\n", unindent=1)
    res
}

#' Read a NOAA format for AXBTs
#'
#'
#' This file format, described at \code{https://www.aoml.noaa.gov/phod/dhos/axbt.php}, contains a header
#' line, followed by data lines.  For example, a particular file at this site has first
#' three lines as follows.
#' ```
#' 181.589 20100709 140820  -85.336  25.290 N42RF GL10 14    2010-190-15:49:18
#'   -0.0 27.52 -9.99
#'   -1.5 27.52 -9.99
#'```
#' where the items on the header line are (1) a year-day (ignored here), (2)
#' YYYYMMDD, (3) HHMMSS, (4) longitude, (5) latitude, (6) aircraft wing
#' number, (7) project name, (8) AXBT channel and (9) AXBT ID.  The other lines hold vertical
#' coordinate in metres, temperature and temperature error; -9.99 is a missing-value
#' code.  (This formatting information is extracted from a file named `readme.axbt` that
#' is provided with the data.)
#'
#' @param file character value naming a file, or a file connection, containing the data.
#'
#' @param debug a flag that turns on debugging.  The value indicates the depth
#' within the call stack to which debugging applies.
#'
#' @param missingValue numerical value that is to be interpreted as `NA`
#'
#' @param processingLog if provided, the action item to be stored in the log.  This
#' parameter is typically only provided for internal calls; the default that it
#' provides is better for normal calls by a user.
#'
#' @return An [xbt-class] object.
#'
#' @family things related to xbt data
#'
#' @author Dan Kelley
read.xbt.noaa1 <- function(file, debug=getOption("oceDebug"), missingValue=-9.99, processingLog)
{
    oceDebug(debug, "read.xbt(file=\"", file, "\", type=\"", "...) {\n", sep="", unindent=1)
    filename <- "?"
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
    res <- new("xbt")
    header <- readLines(file, 1, encoding="UTF-8") # first line is a header
    res@metadata$header <- header
    res@metadata$filename <- filename
    headerTokens <- strsplit(header, "[ \t]+")[[1]]
    res@metadata$time <- ISOdatetime(substr(headerTokens[2], 1, 4), # year
                                     substr(headerTokens[2], 5, 6), # month
                                     substr(headerTokens[2], 7, 8), # day
                                     substr(headerTokens[3], 1, 2), # hour
                                     substr(headerTokens[3], 3, 4), # minute
                                     substr(headerTokens[3], 5, 6)) # second
    res@metadata$longitude <- as.numeric(headerTokens[4])
    res@metadata$latitude <- as.numeric(headerTokens[5])
    res@metadata$aircraft <- headerTokens[6]
    res@metadata$project <- headerTokens[7]
    res@metadata$axbtChannel <- headerTokens[8]
    res@metadata$axbtId <- headerTokens[9]
    data <- read.table(file, skip=1, header=FALSE, col.names=c("z", "temperature", "temperatureError"), encoding="UTF-8")
    res@metadata$header <- header
    res@metadata$units$z <- list(unit=expression(m), scale="")
    res@data <- as.list(data)
    if (!is.null(missingValue)) {
        for (name in names(res@data))
            res@data[[name]][res@data[[name]] == missingValue] <- NA
    }
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # read.xbt.noaa1()\n", sep="", unindent=1)
    res
}


#' Plot an xbt Object
#'
#' Plots data contained in an [xbt-class] object.
#'
#' The panels are controlled by the `which` argument, with choices as follows.
#' * `which=1` for a temperature profile as a function of depth.
#' * `which=2` for a soundSpeed profile as a function of depth.
#'
#' @param x an [xbt-class] object.
#'
#' @param which list of desired plot types; see \dQuote{Details} for the meanings of various
#' values of `which`.
#'
#' @param type type of plot, as for [plot()].
#'
#' @param mgp 3-element numerical vector to use for `par(mgp)`, and also for
#' `par(mar)`, computed from this.  The default is tighter than the R default,
#' in order to use more space for the data and less for the axes.
#'
#' @param mar value to be used with [`par`]`("mar")`.
#'
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate amount
#' of debugging information, or to 2 to get more.
#'
#' @param ... optional arguments passed to plotting functions.
#'
#' @examples
#' library(oce)
#' data(xbt)
#' summary(xbt)
#' plot(xbt)
#'
#' @family functions that plot oce data
#' @family things related to xbt data
#'
#' @aliases plot.xbt
#'
#' @author Dan Kelley
setMethod(f="plot",
          signature=signature("xbt"),
          definition=function(x,
                              which=1,
                              type="l",
                              mgp=getOption("oceMgp"),
                              mar,
                              debug=getOption("oceDebug"),
                              ...)
          {
              oceDebug(debug, "plot.xbt() {\n", unindent=1)
              ## dataNames <- names(x@data)
              ## if (3 != sum(c("depth", "temperature", "soundSpeed") %in% dataNames)) {
              ##     warning("In plot,xbt-method() :\n  cannot plot an xbt object unless its 'data' slot contains 'depth', 'temperature' and 'soundSpeed'", call.=FALSE)
              ##     return(invisible(NULL))
              ## }
              if (missing(mar)) {
                  mar <- c(1, mgp[1]+1.5, mgp[1]+1.5, mgp[1])
              }
              opar <- par(no.readonly = TRUE)
              lw <- length(which)
              oceDebug(debug, "length(which) =", lw, "\n")
              if (lw > 1)
                  on.exit(par(opar))
              par(mgp=mgp, mar=mar)
              oceDebug(debug, "which: c(", paste(which, collapse=", "), ")\n")
              if (lw > 1) {
                  par(mfrow=c(1, lw))
                  oceDebug(debug, "calling par(mfrow=c(", lw, ", 1)\n")
              }
              z <- x[["z"]]
              temperature <- x[["temperature"]]
              soundSpeed <- x[["soundSpeed"]]
              for (w in 1:lw) {
                  oceDebug(debug, "which[", w, "]=", which[w], "\n")
                  if (which[w] == 1) {
                      plot(temperature, z, xlab="", ylab=resizableLabel("z"), type=type, axes=FALSE, ...)
                      axis(2)
                      box()
                      axis(3)
                      mtext(resizableLabel("temperature"), side=3, line=mgp[1])
                  } else if (which[w] == 2) {
                      plot(soundSpeed, z, xlab="", ylab=resizableLabel("z"), type=type, axes=FALSE, ...)
                      axis(2)
                      box()
                      axis(3)
                      mtext(resizableLabel("sound speed"), side=3, line=mgp[1])
                  } else {
                      stop("which values are limited to 1 and 2")
                  }
              }
              oceDebug(debug, "} # plot.xbt()\n", unindent=1)
              invisible(NULL)
          })

