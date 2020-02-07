#' Class to Store LISST Data
#'
#' This class stores LISST (Laser in-situ scattering and transmissometry) data.
#'
#' @templateVar class lisst
#'
#' @templateVar dataExample {}
#'
#' @templateVar metadataExample {}
#'
#' @template slot_summary
#'
#' @template slot_put
#'
#' @template slot_get
#'
#' @author Dan Kelley
#'
#' @references A users's manual for the LISST-100 instrument is available at
#' the manufacturer's website \url{http://www.sequoiasci.com}.
#'
#' @family classes provided by oce
#' @family things related to lisst data
setClass("lisst", contains="oce")

#' Extract Something From a LISST Object
#'
#' @param x a [lisst-class] object.
#'
#' @template sub_subTemplate
#'
#' @family things related to lisst data
setMethod(f="[[",
          signature(x="lisst", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              callNextMethod()         # [[
          })

#' Replace Parts of a LISST Object
#'
#' @param x a [lisst-class] object.
#'
#' @template sub_subsetTemplate
#'
#' @family things related to lisst data
setMethod(f="[[<-",
          signature(x="lisst", i="ANY", j="ANY"),
          definition=function(x, i, j, ..., value) {
              callNextMethod(x=x, i=i, j=j, ...=..., value=value) # [[<-
          })


#' LISST Dataset
#'
#' LISST (Laser in-situ scattering and transmissometry) dataset, constructed
#' artificially.
#'
#' @name lisst
#'
#' @docType data
#'
#' @usage data(lisst)
#'
#' @author Dan Kelley
#'
#' @source This was constructed artificially using [as.lisst()],
#' to approximately match values that might be measured in the field.
#' @family datasets provided with oce
NULL

setMethod(f="initialize",
          signature="lisst",
          definition=function(.Object, filename="", longitude=NA, latitude=NA, ...) {
              .Object <- callNextMethod(.Object, ...)
              .Object@metadata$filename <- filename
              .Object@metadata$longitude <- longitude
              .Object@metadata$latitude <- latitude
              .Object@processingLog$time <- presentTime()
              .Object@processingLog$value <- paste("create 'lisst' object with",
                                                   " filename=\"", filename, "\"",
                                                   ", longitude=", longitude,
                                                   ", latitude=",
                                                   latitude, sep="")
              return(.Object)
          })

#' Summarize a LISST Object
#'
#' Summarizes some of the data in a `lisst` object, presenting such information
#' as the station name, sampling location, data ranges, etc.
#'
#' @param object a [lisst-class] object.
#'
#' @param ... Ignored.
#'
#' @examples
#' library(oce)
#' data(lisst)
#' summary(lisst)
#'
#' @author Dan Kelley
#'
#' @family things related to lisst data
setMethod(f="summary",
          signature="lisst",
          definition=function(object, ...) {
              cat("LISST Summary\n-------------\n\n")
              showMetadataItem(object, "filename", "File source:        ")
              invisible(callNextMethod()) # summary
          })



#' Plot LISST data
#'
#' Creates a multi-panel summary plot of data measured by LISST instrument.
#'
#' The panels are controlled by the `which` argument, as follows.
#'
#' * `which=1` to `32`, or `which="C1"` to `"C32"` for
#' a time-series graph of the named column (a size class).
#'
#' * `which=33` or `which="lts"` for a time-series plot of laser
#' transmission sensor.
#'
#' * `which=34` or `which="voltage"` for a time-series plot of
#' instrument voltage.
#'
#' * `which=35` or `which="aux"` for a time-series plot of the
#' external auxiliary input.
#'
#' * `which=36` or `which="lrs"` for a time-series plot of the
#' laser reference sensor.
#'
#' * `which=37` or `which="pressure"` for a time-series plot of
#' pressure.
#'
#' * `which=38` or `which="temperature"` for a time-series plot
#' of temperature.
#'
#' * `which=41` or `which="transmission"` for a time-series plot
#' of transmission, in percent.
#'
#' * `which=42` or `which="beam"` for a time-series plot of
#' beam-C, in 1/metre.
#'
#' @param x a [lisst-class] object.
#'
#' @param which list of desired plot types.  These are graphed in panels
#' running down from the top of the page.  See \dQuote{Details} for the
#' meanings of various values of `which`.
#'
#' @param tformat optional argument passed to [oce.plot.ts()], for
#' plot types that call that function.  (See [strptime()] for the
#' format used.)
#'
#' @param debug a flag that turns on debugging.  The value indicates the depth
#' within the call stack to which debugging applies.
#'
#' @param \dots optional arguments passed to plotting functions.
#'
#' @author Dan Kelley
#'
#' @seealso The documentation for [lisst-class] explains the
#' structure of lisst objects, and also outlines the other functions dealing
#' with them.
#'
#' @examples
#' library(oce)
#' data(lisst)
#' plot(lisst)
#'
#' @family functions that plot oce data
#' @family things related to lisst data
#'
#' @aliases plot.lisst
setMethod(f="plot",
          signature="lisst",
          definition=function(x, which = c(16, 37, 38), tformat, debug=getOption("oceDebug"), ...) {
              oceDebug(debug, "plot.lisst(..., which=c(", paste(which, collapse=","), "),...) {\n", sep="", unindent=1)
              nw <- length(which)
              oceDebug(debug, "which:", which, "\n")
              which <- oce.pmatch(which,
                                  list(C1=1, C2=2, C3=3, C4=4, C5=5, C6=6, C7=7, C8=8, C9=9, C10=10,
                                      C11=11, C12=12, C13=13, C14=14, C15=15, C16=16, C17=17, C18=18, C19=19, C20=20,
                                      C21=21, C22=22, C23=23, C24=24, C25=25, C26=26, C27=27, C28=28, C29=29, C30=30,
                                      C31=31, C32=32,
                                      lts=33, voltage=34, aux=35, lrs=36,
                                      pressure=37, temperature=38, transmission=41, beam=42))
              oceDebug(debug, "which:", which, "\n")
              opar <- par(no.readonly = TRUE)
              if (length(which) > 1) on.exit(par(opar))
              par(mfrow=c(nw, 1))
              time <- x[["time"]]
              for (w in 1:nw) {
                  ww <- which[w]
                  if      (ww <= 32) oce.plot.ts(time, x@data[[which[w]]],
                                                 ylab=paste(gettext("Size Class #", domain="R-oce"), ww, sep=""),
                                                 tformat=tformat, debug=debug-1, ...)
                  else if (ww == 33) oce.plot.ts(time, x[["lts"]],
                                                 ylab=gettext("Laser Trans. Sensor", domain="R-oce"),
                                                 tformat=tformat, debug=debug-1, ...)
                  else if (ww == 34) oce.plot.ts(time, x[["voltage"]],
                                                 ylab=gettext("Voltage", domain="R-oce"),
                                                 tformat=tformat, debug=debug-1, ...)
                  else if (ww == 35) oce.plot.ts(time, x[["aux"]],
                                                 ylab=gettext("Ext. Aux. Input", domain="R-oce"),
                                                 tformat=tformat, debug=debug-1, ...)
                  else if (ww == 36) oce.plot.ts(time, x[["lrs"]],
                                                 ylab=gettext("Laser Ref. Sensor", domain="R-oce"),
                                                 tformat=tformat, debug=debug-1, ...)
                  else if (ww == 37) oce.plot.ts(time, x[["pressure"]],
                                                 ylab=resizableLabel("p"),
                                                 tformat=tformat, debug=debug-1, ...)
                  else if (ww == 38) oce.plot.ts(time, x[["temperature"]],
                                                 ylab=resizableLabel("T"),
                                                 tformat=tformat, debug=debug-1, ...)
                  else if (ww == 41) oce.plot.ts(time, x[["transmission"]],
                                                 ylab=gettext("Transmission %", domain="R-oce"),
                                                 tformat=tformat, debug=debug-1, ...)
                  else if (ww == 42) oce.plot.ts(time, x[["beam"]],
                                                 ylab=gettext("Beam-C [1/m]", domain="R-oce"),
                                                 tformat=tformat, debug=debug-1, ...)
              }
          })



#' Coerce Data Into a LISST Object
#'
#' Coerce data into a lisst object
#' If `data` contains fewer than 42 columns, an error is reported.  If it
#' contains more than 42 columns, only the first 42 are used.  This is used by
#' [read.lisst()], the documentation on which explains the meanings
#' of the columns.
#'
#' @param data A table (or matrix) containing 42 columns, as in a LISST data
#' file.
#'
#' @param filename Name of file containing the data.
#'
#' @param year Year in which the first observation was made.  This is necessary
#' because LISST timestamps do not indicate the year of observation.  The
#' default value is odd enough to remind users to include this argument.
#'
#' @param tz Timezone of observations.  This is necessary because LISST
#' timestamps do not indicate the timezone.
#'
#' @param longitude Longitude of observation.
#'
#' @param latitude Latitude of observation.
#'
#' @return A [lisst-class] object.
#'
#' @author Dan Kelley
#'
#' @family things related to lisst data
as.lisst <- function(data, filename="", year=0, tz="UTC", longitude=NA, latitude=NA)
{
    res <- new("lisst", filename=filename, latitude=latitude, longitude=longitude)
    ncols <- ncol(data)
    if (ncols < 42)
        stop("data file must hold at least 42 space-separated columns")
    if (ncols > 42) {
        warning("data file has more than 42 columns; only first 42 are used")
        data <- data[, 1:42]
    }
    data <- data.frame(data)
    names <- rep("", length.out=42)
    names[1:32] <-  paste("C", 1:32, sep="")
    names[33] <- "lts"                  # laserTransmissionSensor
    names[34] <- "voltage"
    names[35] <- "aux"
    names[36] <- "lrs"                  # laserReferenceSensor
    names[37] <- "pressure"
    names[38] <- "temperature"
    names[39] <- "dayhour"
    names[40] <- "minutesecond"
    names[41] <- "transmission"
    names[42] <- "beam"
    names(data) <- names
    data <- as.list(data)
    day <- floor(data$dayhour/100)
    hour <- data$dayhour - 100 * day
    minute <- floor(data$minutesecond/100)
    second <- data$minutesecond - 100 * minute
    decimalday <- day + hour / 24 + minute / 60 / 24 + second / 24 / 60 / 60
    t0 <- as.POSIXct(paste(year, "-01-01 00:00:00", sep=""), tz=tz)
    data$time <- t0 + 86400 * decimalday / 365.25
    res@data <- data
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    names <- names(data)
    if ("pressure" %in% names) res@metadata$units$pressure <- list(unit=expression(dbar), scale="")
    if ("temperature" %in% names) res@metadata$units$temperature <- list(unit=expression(degree*C), scale="")
    res
}


#' Read a LISST File
#'
#' Read a LISST data file. 
#' The file should contain 42 columns, with no header.  If there are fewer than
#' 42 columns, an error results.  If there are more, only the first 42 are
#' used.  Note that [read.oce()] can recognize LISST files by their
#' having a name ending in `".asc"` and by having 42 elements on the first
#' line.  Even so, it is preferred to use the present function, because it
#' gives the opportunity to specify the year and timezone, so that times can be
#' calculated properly.
#'
#' @param file a connection or a character string giving the name of the file
#' to load.
#'
#' @param year year in which the measurement of the series was made.
#'
#' @param tz time zone.
#'
#' @param longitude longitude of observation (stored in metadata)
#'
#' @param latitude latitude of observation (stored in metadata)
#'
#' @return x A [lisst-class] object.
#'
#' @author Dan Kelley
#'
#' @family things related to lisst data
read.lisst <- function(file, year=0, tz="UTC", longitude=NA, latitude=NA)
{
    if (!missing(file) && is.character(file) && 0 == file.info(file)$size)
        stop("empty file")
    filename <- NULL
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
    data <- read.table(file, header=FALSE)
    res <- as.lisst(data, filename=filename, year=year, tz=tz, longitude=longitude, latitude=latitude)
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    names <- names(data)
    if ("pressure" %in% names) res@metadata$units$pressure <- list(unit=expression(dbar), scale="")
    if ("temperature" %in% names) res@metadata$units$temperature <- list(unit=expression(degree*C), scale="")
    res
}
