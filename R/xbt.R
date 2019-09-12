## vim:textwidth=100:expandtab:shiftwidth=4:softtabstop=4

#' Class to Store XBT Data
#'
#' This class stores expendible bathythermograph (XBT) data, e.g. from a Sippican
#' device.  Reference 1 gives some information on Sippican
#' devices, and reference 2 is a useful introduction to the
#' modern literature on XBTs in general.
#'
#' @references
#' 1. Sippican, Inc. “Bathythermograph Data Acquisition System: Installation, Operation and Maintenance
#' Manual (P/N 308195, Rev. A),” 2003.
#' https://pages.uoregon.edu/drt/MGL0910_Science_Report/attachments/MK21_ISA_Manual_Rev_A.pdf.
#'
#' 2. Cheng, Lijing, John Abraham, Gustavo Goni, Timothy Boyer, Susan Wijffels, Rebecca
#' Cowley, Viktor Gouretski, et al. “XBT Science: Assessment of Instrumental Biases and Errors.”
#' Bulletin of the American Meteorological Society 97, no. 6 (June 2016): 924–33.
#' https://doi.org/10.1175/BAMS-D-15-00031.1.
#'
#' @templateVar class xbt
#'
#' @templateVar dataExample The key items stored in this slot are `depth`, `temperature` and  `soundSpeed`.  Note that `depth` is inferred from time in  water, using an empirical formula for instrument descent  rate, and that `soundSpeed` is calculatd using a fixed  practical salinity of 35.
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
#' 1. Sippican, Inc. “Bathythermograph Data Acquisition System: Installation, Operation and Maintenance
#' Manual (P/N 308195, Rev. A),” 2003.
#' https://pages.uoregon.edu/drt/MGL0910_Science_Report/attachments/MK21_ISA_Manual_Rev_A.pdf.
#'
#' @family datasets provided with oce
#' @family things related to xbtdata
NULL

#' Extract Something From an xbt Object
#'
#' @param x an [xbt-class] object.
#'
#' @template sub_subTemplate
#'
#' @family things related to xbtdata
setMethod(f="[[",
          signature(x="xbt", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              callNextMethod()         # [[
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
          definition=function(.Object,
                              depth=NULL, temperature=NULL, soundSpeed=NULL, units) {
              if (missing(units)) {
                  .Object@metadata$units <- list(depth=list(unit=expression(m), scale=""),
                                                 temperature=list(unit=expression(degree*C), scale="ITS-90"),
                                                 soundSpeed=list(unit=expression(m/s), scale=""))
              } else {
                  .Object@metadata$units <- units # CAUTION: we are being quite trusting here
              }
              .Object@data$depth <- depth
              .Object@data$temperature <- temperature
              .Object@data$soundSpeed <- soundSpeed
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
              subsetString <- paste(deparse(substitute(subset)), collapse=" ")
              res <- x
              dots <- list(...)
              debug <- getOption("oceDebug")
              if (length(dots) && ("debug" %in% names(dots)))
                  debug <- dots$debug
              if (missing(subset))
                  stop("must give 'subset'")
              if (length(grep("depth", subsetString))) {
                  oceDebug(debug, "subsetting an xbt by depth\n")
                  keep <- eval(substitute(subset), x@data, parent.frame(2))
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


#' Read an xbt file
#'
#' In this version, only one type of file is handled, namely the `.edf` format, used
#' for Sippican XBT files.  This is handled by calling [read.xbt.edf()].
#'
#' @param file a connection or a character string giving the name of the file to
#' load.
#'
#' @param type character string indicating type of file (only `"sippican"` is permitted).
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
#' @family things related to xbt data
#'
#' @references
#' 1. Sippican, Inc. “Bathythermograph Data Acquisition System: Installation, Operation and Maintenance
#' Manual (P/N 308195, Rev. A),” 2003.
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
    res <- if (type == "sippican")
        read.xbt.edf(file=file, longitude=longitude, latitude=latitude,
                     debug=debug-1, processingLog=processingLog)
    else
        stop("unknown type of current meter")
    oceDebug(debug, "} # read.xbt()\n", sep="", unindent=1)
    res
}

#' Read a Sippican '.edf' format xbt file
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
    res@metadata$header <- l[1:headerEnd]
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
    res@data <- as.list(read.table(file, skip=headerEnd+1, col.names=c("depth", "temperature", "soundSpeed")))
    res@metadata$filename <- filename
    res@metadata$units$depth <- list(unit=expression(m), scale="")
    res@metadata$units$temperature <- list(unit=expression(degree*C), scale="ITS-90")
    res@metadata$units$soundSpeed <- list(unit=expression(m/s), scale="")
    oceDebug(debug, "} # read.xbt.edf()\n", unindent=1)
    res
}

#' Plot An xbt data
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
#' @param ... Optional arguments passed to plotting functions.
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
                              which=c(1, 2),
                              type="l",
                              mgp=getOption("oceMgp"),
                              mar,
                              debug=getOption("oceDebug"),
                              ...)
          {
              oceDebug(debug, "plot.xbt() {\n", unindent=1)
              if (3 != sum(c("depth", "temperature", "soundSpeed") %in% names(x@data))) {
                  warning("In plot,xbt-method() :\n  cannot plot an xbt object unless its 'data' slot contains 'depth', 'temperature' and 'soundSpeed'", call.=FALSE)
                  return(invisible(NULL))
              }
              if (missing(mar)) {
                  mar <- c(1, mgp[1]+1.5, mgp[1]+1.5, mgp[1])
              }
              opar <- par(no.readonly = TRUE)
              lw <- length(which)
              oceDebug(debug, "length(which) =", lw, "\n")
              if (lw > 1)
                  on.exit(par(opar))
              par(mgp=mgp, mar=mar)
              dots <- list(...)
              oceDebug(debug, "which: c(", paste(which, collapse=", "), ")\n")
              if (lw > 1) {
                  par(mfrow=c(1, lw))
                  oceDebug(debug, "calling par(mfrow=c(", lw, ", 1)\n")
              }
              len <- length(x@data$depth)
              for (w in 1:lw) {
                  oceDebug(debug, "which[", w, "]=", which[w], "\n")
                  if (which[w] == 1) {
                      plot(x@data$temperature, x@data$depth, ylim=rev(range(x@data$depth, na.rm=TRUE)),
                           xlab="", ylab=resizableLabel("depth"),
                           type=type, axes=FALSE)
                      axis(2)
                      box()
                      axis(3)
                      mtext(resizableLabel("temperature"), side=3, line=mgp[1])
                  } else if (which[w] == 2) {
                      plot(x@data$soundSpeed, x@data$depth, ylim=rev(range(x@data$depth, na.rm=TRUE)),
                           xlab="", ylab=resizableLabel("depth"),
                           type=type, axes=FALSE)
                      axis(2)
                      box()
                      axis(3)
                      mtext(resizableLabel("sound speed"), side=3, line=mgp[1])
                  } else {
                      stop("which values are limited to 1 and 2")
                  }
              }
              oceDebug(debug, "} # plot.xbt()\n", unindent=1)
              invisible()
          })

