## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

## REFERENCES:
##   [1] "DT4 Data File Format Specification" [July, 2010] DT4_format_2010.pdf


#' Class to Store Echosounder Data
#'
#' This class stores echosounder data. Echosounder objects may be
#' read with \code{\link{read.echosounder}},
#' summarized with \code{\link{summary,echosounder-method}},
#' and plotted with \code{\link{plot,echosounder-method}}.
#' The \code{\link{findBottom}}
#' function infers the ocean bottom from tracing the strongest reflector from
#' ping to ping.
#'
#' @templateVar class echosounder
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
#' @details
#'
#' \itemize{
#'
#' \item An infrequently updated record of the intrument position, in
#' \code{timeSlow}, \code{longitudeSlow} and \code{latitudeSlow}.  These are
#' used in plotting maps with \code{\link{plot,echosounder-method}}.
#'
#' \item An interpolated record of the instrument position, in \code{time},
#' \code{longitude}, and \code{latitude}.  Linear interpolation is used to
#' infer the longitude and latitude from the variables listed above.
#'
#' \item \code{depth}, vector of depths of echo samples (measured positive
#' downwards in the water column).  This is calculated from the inter-sample
#' time interval and the sound speed provided as the \code{soundSpeed} argument
#' to \code{\link{read.echosounder}}, so altering the value of the latter will
#' alter the echosounder plots provided by \code{\link{plot,echosounder-method}}.
#'
#' \item The echosounder signal amplitude \code{a}, a matrix whose number of
#' rows matches the length of \code{time}, etc., and number of columns equal to
#' the length of \code{depth}.  Thus, for example, \code{a[100,]} represents
#' the depth-dependent amplitude at the time of the 100th ping.
#'
#' \item A matrix named \code{b} exists for dual-beam and split-beam cases.
#' For dual-beam data, this is the wide-beam data, whereas \code{a} is the
#' narrow-beam data.  For split-beam data, this is the x-angle data.
#'
#' \item A matrix named \code{c} exists for split-beam data, containing the
#' y-angle data.
#'
#' \item In addition to these matrices, ad-hoc calculated matrices named
#' \code{Sv} and \code{TS} may be accessed as explained in the next section.
#'
#' }
#'
#' @name echosounder-class
#' @docType class
#'
#' @author Dan Kelley
#' @family things related to echosounder data
setClass("echosounder", contains="oce")


#' @title Echosounder Dataset
#'
#' @description
#' This is degraded subsample of measurements that were made with a Biosonics
#' scientific echosounder, as part of the St Lawrence Internal Wave Experiment
#' (SLEIWEX).
#'
#' @name echosounder
#' @docType data
#'
#' @author Dan Kelley
#' @source This file came from the SLEIWEX-2008 experiment, and was decimated
#' using \code{\link{decimate}} with \code{by=c()}.
#' @family datasets provided with oce
#' @family things related to echosounder data
NULL

setMethod(f="initialize",
          signature="echosounder",
          definition=function(.Object, filename="") {
              .Object@metadata$filename <- filename
              .Object@processingLog$time <- presentTime()
              .Object@processingLog$value <- "create 'echosounder' object"
              return(.Object)
          })




#' @title Summarize an Echosounder Object
#'
#' @description
#' Summarizes some of the data in an \code{echosounder} object.
#'
#' @param object an object of class \code{"echosounder"}, usually, a result of
#' a call to \code{\link{read.echosounder}}, \code{\link{read.oce}}, or
#' \code{\link{as.echosounder}}.
#' @param \dots further arguments passed to or from other methods.
#' @author Dan Kelley
#' @family things related to echosounder data
setMethod(f="summary",
          signature="echosounder",
          definition=function(object, ...) {
              cat("Echosounder Summary\n-------------------\n\n")
              showMetadataItem(object, "filename",               "File source:         ", quote=TRUE)
              showMetadataItem(object, "transducerSerialNumber", "Transducer serial #: ", quote=FALSE)
              metadataNames <- names(object@metadata)
              cat(sprintf("* File type:           %s\n", object[["fileType"]]))
              if ("beamType" %in% metadataNames)
                  cat(sprintf("* Beam type:           %s\n", object[["beamType"]]))
              time <- object[["time"]]
              tz <- attr(time[1], "tzone")
              nt <- length(time)
              cat(sprintf("* Channel:             %d\n", object[["channel"]]))
              cat(sprintf("* Measurements:        %s %s to %s %s\n", format(time[1]), tz, format(time[nt]), tz))
              cat(sprintf("* Sound speed:         %.2f m/s\n", object[["soundSpeed"]]))
              ##cat(sprintf("* Time between pings:  %.2e s\n", object[["samplingDeltat"]]))
              if ("pulseDuration" %in% metadataNames) cat(sprintf("* Pulse duration:      %g s\n", object[["pulseDuration"]]/1e6))
              cat(sprintf("* Frequency:           %f\n", object[["frequency"]]))
              cat(sprintf("* Blanked samples:     %d\n", object[["blankedSamples"]]))
              cat(sprintf("* Pings in file:       %d\n", object[["pingsInFile"]]))
              cat(sprintf("* Samples per ping:    %d\n", object[["samplesPerPing"]]))
              invisible(callNextMethod()) # summary
          })


#' Extract Something from an Echosounder Object
#' @param x A \code{echosounder} object, i.e. one inheriting from \code{\link{echosounder-class}}.
#'
#' @templateVar class echosounder
#'
#' @section Details of the specialized \code{echosounder} method:
#'
#' If \code{i} is the string \code{"Sv"}, the return value is calculated according to
#' \preformatted{
#' Sv <- 20*log10(a) -
#'   (x@@metadata$sourceLevel+x@@metadata$receiverSensitivity+x@@metadata$transmitPower) +
#'   20*log10(r) +
#'   2*absorption*r -
#'   x@@metadata$correction +
#'   10*log10(soundSpeed*x@@metadata$pulseDuration/1e6*psi/2)
#'}
#'
#' If \code{i} is the string \code{"TS"},
#' \preformatted{
#' TS <- 20*log10(a) -
#'   (x@@metadata$sourceLevel+x@@metadata$receiverSensitivity+x@@metadata$transmitPower) +
#'   40*log10(r) +
#'   2*absorption*r +
#'   x@@metadata$correction
#'}
#'
#' Otherwise, the generic \code{[[} is used.
#' @template sub_subTemplate
#' @family things related to echosounder data
setMethod(f="[[",
          signature(x="echosounder", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              if (i %in% c("Sv", "TS")) {
                  range <- rev(x@data$depth)
                  a <- x@data$a
                  ## biosonics has /20 because they have bwx in 0.1deg
                  psi <- x@metadata$beamwidthX / 2 * x@metadata$beamwidthY / 2 / 10^3.16
                  r <- matrix(rev(range), nrow=nrow(a), ncol=length(range), byrow=TRUE)
                  absorption <- swSoundAbsorption(x@metadata$frequency, 35, 10, mean(range))
                  soundSpeed <- x@metadata$soundSpeed
                  if (i == "Sv") {
                      Sv <- 20*log10(a) -
                      (x@metadata$sourceLevel+x@metadata$receiverSensitivity+x@metadata$transmitPower) +
                      20*log10(r) +
                      2*absorption*r -
                      x@metadata$correction +
                      10*log10(soundSpeed*x@metadata$pulseDuration/1e6*psi/2)
                      Sv[!is.finite(Sv)] <- NA
                      Sv
                  } else if (i == "TS") {
                      TS <- 20*log10(a) -
                      (x@metadata$sourceLevel+x@metadata$receiverSensitivity+x@metadata$transmitPower) +
                      40*log10(r) +
                      2*absorption*r +
                      x@metadata$correction
                      TS[!is.finite(TS)] <- NA
                      TS
                  }
              } else {
                  callNextMethod()     # [[
              }
          })

#' @title Replace Parts of an Echosounder Object
#' @param x An \code{echosounder} object, i.e. inheriting from \code{\link{echosounder-class}}
#' @template sub_subsetTemplate
#' @family things related to echosounder data
setMethod(f="[[<-",
          signature(x="echosounder", i="ANY", j="ANY"),
          definition=function(x, i, j, ..., value) {
              callNextMethod(x=x, i=i, j=j, ...=..., value=value) # [[<-
          })


#' @title Subset an Echosounder Object
#'
#' @description
#' This function is somewhat analogous to \code{\link{subset.data.frame}}.
#' Subsetting can be by \code{time} or \code{depth}, but these may not be
#' combined; use a sequence of calls to subset by both.
#'
#' @param x a \code{echosounder} object.
#' @param subset a condition to be applied to the \code{data} portion of
#' \code{x}.  See \sQuote{Details}.
#' @param \dots ignored.
#' @return A new \code{echosounder} object.
#' @author Dan Kelley
#' @examples
#' library(oce)
#' data(echosounder)
#' plot(echosounder)
#' plot(subset(echosounder, depth < 10))
#' plot(subset(echosounder, time < mean(range(echosounder[['time']]))))
#'
#' @family things related to echosounder data
#' @family functions that subset oce objects
setMethod(f="subset",
          signature="echosounder",
          definition=function(x, subset, ...) {
              subsetString <- paste(deparse(substitute(subset)), collapse=" ")
              res <- x
              dots <- list(...)
              debug <- if (length(dots) && ("debug" %in% names(dots))) dots$debug else getOption("oceDebug")
              if (missing(subset))
                  stop("must give 'subset'")
              if (length(grep("time", subsetString))) {
                  oceDebug(debug, "subsetting an echosounder object by time\n")
                  keep <- eval(substitute(subset), x@data, parent.frame(2))
                  oceDebug(debug, "keeping", 100 * sum(keep)/length(keep), "% of the fast-sampled data\n")
                  res <- x
                  ## trim fast variables, handling matrix 'a' differently, and skipping 'distance'
                  dataNames <- names(res@data)
                  res@data$a <- x@data$a[keep, ]
                  if ("b" %in% dataNames)
                      res@data$b <- x@data$b[keep, ]
                  if ("c" %in% dataNames)
                      res@data$c <- x@data$c[keep, ]
                  ## lots of debugging in here, in case other data types have other variable names
                  oceDebug(debug, "dataNames (orig):", dataNames, "\n")
                  if (length(grep('^a$', dataNames)))
                      dataNames <- dataNames[-grep('^a$', dataNames)]
                  if (length(grep('^b$', dataNames)))
                      dataNames <- dataNames[-grep('^b$', dataNames)]
                  if (length(grep('^c$', dataNames)))
                      dataNames <- dataNames[-grep('^c$', dataNames)]
                  oceDebug(debug, "dataNames (step 2):", dataNames, "\n")
                  if (length(grep('^depth$', dataNames)))
                      dataNames <- dataNames[-grep('^depth$', dataNames)]
                  oceDebug(debug, "dataNames (step 3):", dataNames, "\n")
                  if (length(grep('Slow', dataNames)))
                      dataNames <- dataNames[-grep('Slow', dataNames)]
                  oceDebug(debug, "dataNames (final), i.e. fast dataNames to be trimmed by time:", dataNames, "\n")
                  for (dataName in dataNames) {
                      oceDebug(debug, "fast variable:", dataName, "orig length", length(x@data[[dataName]]), "\n")
                      res@data[[dataName]] <- x@data[[dataName]][keep]
                      oceDebug(debug, "fast variable:", dataName, "new length", length(res@data[[dataName]]), "\n")
                  }
                  ## trim slow variables
                  subsetStringSlow <- gsub("time", "timeSlow", subsetString)
                  oceDebug(debug, "subsetting slow variables with string:", subsetStringSlow, "\n")
                  keepSlow <-eval(parse(text=subsetStringSlow), x@data, parent.frame(2))
                  oceDebug(debug, "keeping", 100 * sum(keepSlow)/length(keepSlow), "% of the slow-sampled data\n")
                  for (slowName in names(x@data)[grep("Slow", names(x@data))]) {
                      oceDebug(debug, "slow variable:", slowName, "orig length", length(x@data[[slowName]]), "\n")
                      res@data[[slowName]] <- x@data[[slowName]][keepSlow]
                      oceDebug(debug, "slow variable:", slowName, "new length", length(res@data[[slowName]]), "\n")
                  }
              } else if (length(grep("depth", subsetString))) {
                  oceDebug(debug, "subsetting an echosounder object by depth\n")
                  keep <- eval(substitute(subset), x@data, parent.frame(2))
                  res <- x
                  res[["depth"]] <- res[["depth"]][keep]
                  dataNames <- names(res@data)
                  res[["a"]] <- res[["a"]][, keep]
                  if ("b" %in% dataNames)
                      res@data$b <- x@data$b[, keep]
                  if ("c" %in% dataNames)
                      res@data$c <- x@data$c[, keep]
              } else {
                  stop("can only subset an echosounder object by 'time' or 'depth'")
              }
              res@processingLog <- processingLogAppend(res@processingLog, paste("subset.echosounder(x, subset=", subsetString, ")", sep=""))
              res
          })


#' Coerce Data into an Echosounder Object
#'
#' Coerces a dataset into a echosounder dataset.
#'
#' Creates an echosounder file.  The defaults for e.g.  \code{transmitPower}
#' are taken from the \code{echosounder} dataset, and they are unlikely to make
#' sense generally.
#'
#' @param time times of pings
#' @param depth depths of samples within pings
#' @param a matrix of amplitudes
#' @param src optional string indicating data source
#' @param sourceLevel source level, in dB (uPa at 1m), denoted \code{sl} in [1
#' p15], where it is in units 0.1dB (uPa at 1m)
#' @param receiverSensitivity receiver sensivitity of the main element, in
#' dB(counts/uPa), denoted \code{rs} in [1 p15], where it is in units of
#' 0.1dB(counts/uPa)
#' @param transmitPower transmit power reduction factor, in dB, denoted
#' \code{tpow} in [1 p10], where it is in units 0.1 dB.
#' @param pulseDuration duration of transmitted pulse in us
#' @param beamwidthX x-axis -3dB one-way beamwidth in deg, denoted \code{bwx}
#' in [1 p16], where the unit is 0.2 deg
#' @param beamwidthY y-axis -3dB one-way beamwidth in deg, denoted \code{bwx}
#' in [1 p16], where the unit is 0.2 deg
#' @param frequency transducer frequency in Hz, denoted \code{fq} in [1 p16]
#' @param correction user-defined calibration correction in dB, denoted
#' \code{corr} in [1 p14], where the unit is 0.01dB.
#' @return An object of \code{\link[base]{class}} \code{"echosounder"}; for
#' details of this data type, see \code{\link{echosounder-class}}).
#' @author Dan Kelley
#' @family things related to echosounder data
as.echosounder <- function(time, depth, a, src="",
                           sourceLevel=220,
                           receiverSensitivity=-55.4,
                           transmitPower=0,
                           pulseDuration=400,
                           beamwidthX=6.5, beamwidthY=6.5,
                           frequency=41800,
                           correction=0)
{
    res <- new('echosounder', filename=src)
    res@metadata$channel <- 1
    res@metadata$soundSpeed <- swSoundSpeed(35, 10, 1)
    res@metadata$samplingDeltat <- as.numeric(time[2]) - as.numeric(time[1])
    dim <- dim(a)
    res@metadata$pingsInFile <- dim[1]
    res@metadata$samplesPerPing <- dim[2]

    ## args
    res@metadata$sourceLevel <- sourceLevel
    res@metadata$receiverSensitivity <- receiverSensitivity
    res@metadata$transmitPower <- transmitPower
    res@metadata$pulseDuration <- pulseDuration
    res@metadata$beamwidthX <- beamwidthX
    res@metadata$beamwidthY <- beamwidthY
    res@metadata$frequency <- frequency
    res@metadata$correction <- correction

    ## FIXME: what about timeLocation, latitude, and longitude?
    res@data$time <- time
    res@data$depth <- depth
    res@data$a<- a
    names <- names(res@data)
    if ("latitude" %in% names) res@metadata$units$latitude <- list(unit=expression(degree*N), scale="")
    if ("longitude" %in% names) res@metadata$units$longitude <- list(unit=expression(degree*E), scale="")
    if ("latitudeSlow" %in% names) res@metadata$units$latitudeSlow <- list(unit=expression(degree*N), scale="")
    if ("longitudeSlow" %in% names) res@metadata$units$longitudeSlow <- list(unit=expression(degree*E), scale="")
    if ("depth" %in% names) res@metadata$units$depth <- list(unit=expression(m), scale="")
     res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}



#' @title Find the Ocean Bottom in an Echosounder Object
#'
#' @description
#' Finds the depth in a Biosonics echosounder file, by finding the strongest
#' reflector and smoothing its trace.
#'
#' @param x an object of class \code{echosounder}
#' @param ignore number of metres of data to ignore, near the surface
#' @param clean a function to clean the inferred depth of spikes
#' @return A list with elements: the \code{time} of a ping, the \code{depth} of
#' the inferred depth in metres, and the \code{index} of the inferred bottom
#' location, referenced to the object's \code{depth} vector.
#' @author Dan Kelley
#' @seealso The documentation for \code{\link{echosounder-class}} explains the
#' structure of \code{echosounder} objects, and also outlines the other
#' functions dealing with them.
#' @family things related to echosounder data
findBottom <- function(x, ignore=5, clean=despike)
{
    a <- x[["a"]]
    keep <- x[["depth"]] >= ignore
    wm <- clean(apply(a[, keep], 1, which.max))
    depth <- x[["depth"]][wm]
    list(time=x[["time"]], depth=depth, index=wm)
}


#' @title Plot Echosounder Data
#'
#' @description
#' Plot echosounder data.
#' Simple linear approximation is used when a \code{newx} value is specified
#' with the \code{which=2} method, but arguably a gridding method should be
#' used, and this may be added in the future.
#'
#' @param x An \code{echosounder} object, e.g. as read by
#' \code{\link{read.echosounder}}, or created by \code{\link{as.echosounder}}.
#' @param which list of desired plot types: \code{which=1} or \code{which="zt
#' image"} gives a z-time image, \code{which=2} or \code{which="zx image"}
#' gives a z-distance image, and \code{which=3} or \code{which="map"} gives a
#' map showing the cruise track.  In the image plots, the display is of
#' \code{\link{log10}} of amplitude, trimmed to zero for any amplitude values
#' less than 1 (including missing values, which equal 0).  Add 10 to the
#' numeric codes to get the secondary data (non-existent for single-beam files,
#' @param beam a more detailed specification of the data to be plotted.  For
#' single-beam data, this may only be \code{"a"}.  For dual-beam data, this may
#' be \code{"a"} for the narrow-beam signal, or \code{"b"} for the wide-beam
#' signal.  For split-beam data, this may be \code{"a"} for amplitude,
#' \code{"b"} for x-angle data, or \code{"c"} for y-angle data.
#' @param newx optional vector of values to appear on the horizontal axis if
#' \code{which=1}, instead of time.  This must be of the same length as the
#' time vector, because the image is remapped from time to \code{newx} using
#' \code{\link{approx}}.
#' @param xlab,ylab optional labels for the horizontal and vertical axes; if
#' not provided, the labels depend on the value of \code{which}.
#' @param xlim optional range for x axis.
#' @param ylim optional range for y axis.
#' @param zlim optional range for color scale.
#' @param type type of graph, \code{"l"} for line, \code{"p"} for points, or
#' \code{"b"} for both.
#' @param col color scale for image, a function
#' @param lwd line width (ignored if \code{type="p"})
#' @param atTop optional vector of time values, for labels at the top of the
#' plot produced with \code{which=2}.  If \code{labelsTop} is provided, then it
#' will hold the labels.  If \code{labelsTop} is not provided, the labels will
#' be constructed with the \code{\link{format}} function, and these may be
#' customized by supplying a \code{format} in the \dots{} arguments.
#' @param labelsTop optional vector of character strings to be plotted above
#' the \code{atTop} times.  Ignored unless \code{atTop} was provided.
#' @param tformat optional argument passed to \code{\link{imagep}}, for plot
#' types that call that function.  (See \code{\link{strptime}} for the format
#' used.)
#' @param despike remove vertical banding by using \code{\link{smooth}} to
#' smooth across image columns, row by row.
#' @param drawBottom optional flag used for section images.  If \code{TRUE},
#' then the bottom is inferred as a smoothed version of the ridge of highest
#' image value, and data below that are grayed out after the image is drawn.
#' If \code{drawBottom} is a color, then that color is used, instead of
#' white.  The bottom is detected with \code{\link{findBottom}}, using the
#' \code{ignore} value described next.
#' @param ignore optional flag specifying the thickness in metres of a surface
#' region to be ignored during the bottom-detection process.  This is ignored
#' unless \code{drawBottom=TRUE}.
#' @param drawTimeRange if \code{TRUE}, the time range will be drawn at the
#' top.  Ignored except for \code{which=2}, i.e. distance-depth plots.
#' @param drawPalette if \code{TRUE}, the palette will be drawn.
#' @param radius radius to use for maps; ignored unless \code{which=3} or
#' \code{which="map"}.
#' @param coastline coastline to use for maps; ignored unless \code{which=3} or
#' \code{which="map"}.
#'
#' @param mgp 3-element numerical vector to use for \code{par(mgp)}, and also
#' for \code{par(mar)}, computed from this.  The default is tighter than the R
#' default, in order to use more space for the data and less for the axes.
#' @param mar value to be used with \code{\link{par}("mar")}.
#' @param debug set to an integer exceeding zero, to get debugging information
#' during processing.
#' @param \dots optional arguments passed to plotting functions.  For example,
#' for maps, it is possible to specify the radius of the view in kilometres,
#' with \code{radius}.
#' @return A list is silently returned, containing \code{xat} and \code{yat},
#' values that can be used by \code{\link{oce.grid}} to add a grid to the plot.
#' @author Dan Kelley, with extensive help from Clark Richards
#' @examples
#'
#'\donttest{
#' library(oce)
#' data(echosounder)
#' plot(echosounder, which=c(1,2), drawBottom=TRUE)
#'}
#' @family things related to echosounder data
#' @aliases plot.echosounder
setMethod(f="plot",
          signature=signature("echosounder"),
          definition=function(x, which = 1, # 1=z-t section 2=dist-t section 3=map
                              beam="a",
                              newx,
                              xlab, ylab,
                              xlim, ylim, zlim,
                              type="l", col=oceColorsJet, lwd=2,
                              despike=FALSE,
                              drawBottom, ignore=5,
                              drawTimeRange=FALSE, drawPalette=TRUE,
                              radius, coastline,
                              mgp=getOption("oceMgp"),
                              mar=c(mgp[1]+1, mgp[1]+1, mgp[1]+1, mgp[1]+1),
                              atTop, labelsTop,
                              tformat,
                              debug=getOption("oceDebug"),
                              ...)
          {
              dots <- list(...)
              res <- list(xat=NULL, yat=NULL)
              dotsNames <- names(dots)
              oceDebug(debug, "plot() { # for echosounder\n", unindent=1)
              if ("adorn" %in% names(list(...)))
                  warning("In plot,echosounder-method() : the 'adorn' argument was removed in November 2017", call.=FALSE)
              opar <- par(no.readonly = TRUE)
              lw <- length(which)
              if (length(beam) < lw)
                  beam <- rep(beam, lw)
              opar <- par(no.readonly = TRUE)
              par(mgp=mgp, mar=mar)
              if (lw > 1) {
                  ##on.exit(par(opar))
                  if (lw > 2)
                      lay <- layout(matrix(1:4, nrow=2, byrow=TRUE))
                  else
                      lay <- layout(matrix(1:2, nrow=2, byrow=TRUE))
              }

              oceDebug(debug, "which:", which, "\n")
              which <- oce.pmatch(which, list("zt image"=1, "zx image"=2, map=3))
              oceDebug(debug, "which:", which, "\n")
              for (w in seq_along(which)) {
                  oceDebug(debug, "this which:", which[w], "\n")
                  if (which[w] == 1) {
                      time <- x[["time"]]
                      xInImage <- time
                      if (!length(time))
                          stop("plot.echosounder() cannot plot, because @data$time has zero length")
                      signal <- x[[beam[w]]]
                      newxGiven <- !missing(newx)
                      if (newxGiven) {
                          t <- as.numeric(time)
                          if (length(newx) != length(t))
                              stop("length of 'newx' must match that of time within the object")
                         xInImage <- newx
                      }
                      if (despike)
                          signal <- apply(signal, 2, smooth)
                      if (beam[w] == "Sv" || beam[w] == "TS") {
                          z <- signal
                      } else {
                          z <- log10(signal)
                      }
                      z[!is.finite(z)] <- NA # prevent problem in computing range
                      if (!missing(drawBottom)) {
                          if (is.logical(drawBottom) && drawBottom)
                              drawBottom <- "lightgray"
                          waterDepth <- findBottom(x, ignore=ignore)$depth
                          axisBottom <- par('usr')[3]
                          deepestWater <- max(abs(waterDepth))
                          ats <- imagep(xInImage, y=-x[["depth"]], z=z,
                                        xlab=if (missing(xlab)) "" else xlab, # time
                                        ylab=if (missing(ylab)) "z [m]" else ylab, # depth
                                        xlim=xlim,
                                        ylim=if (missing(ylim)) c(-deepestWater, 0) else ylim,
                                        zlim=if (missing(zlim)) c(if (beam[w] %in% c("Sv", "TS")) min(z, na.rm=TRUE) else 0, max(z, na.rm=TRUE)) else zlim,
                                        col=col,
                                        mgp=mgp, mar=mar,
                                        tformat=tformat,
                                        drawPalette=drawPalette,
                                        debug=debug-1, ...)
                          axisBottom <- par('usr')[3]
                          waterDepth <- c(axisBottom, -waterDepth, axisBottom)
                          time <-  x[["time"]]
                          if (newxGiven) {
                              newx2 <- approx(as.numeric(time), newx, as.numeric(time))$y
                              newx2 <- c(newx2[1], newx2, newx2[length(newx2)])
                              polygon(newx2, waterDepth, col=drawBottom)
                          } else {
                              time2 <- c(time[1], time, time[length(time)])
                              polygon(time2, waterDepth, col=drawBottom)
                          }
                      } else {
                          ats <- imagep(xInImage, y=-x[["depth"]], z=z,
                                        xlab=if (missing(xlab)) "" else xlab, # time
                                        ylab=if (missing(ylab)) "z [m]" else ylab, # depth
                                        xlim=xlim,
                                        ylim=if (missing(ylim)) c(-max(abs(x[["depth"]])), 0) else ylim,
                                        zlim=if (missing(zlim)) c(if (beam[w] %in% c("Sv", "TS")) min(z, na.rm=TRUE) else 0, max(z, na.rm=TRUE)) else zlim,
                                        col=col,
                                        mgp=mgp, mar=mar,
                                        tformat=tformat,
                                        drawPalette=drawPalette,
                                        debug=debug-1,
                                        zlab=beam[w],
                                        ...)
                      }
                      res$xat <- ats$xat
                      res$yat <- ats$yat
                      if (newxGiven) {
                          if (!missing(atTop)) {
                              at <- approx(as.numeric(x[["time"]]), newx, as.numeric(atTop))$y
                              if (missing(labelsTop))
                                  labelsTop <- format(atTop, format=if ("format" %in% dotsNames)  dots$format else "%H:%M:%S")
                              axis(side=3, at=at, labels=labelsTop, cex.axis=par('cex'))
                          } else {
                              pretty <- pretty(time)
                              labels <- format(pretty, format="%H:%M:%S")
                              at <- approx(as.numeric(time), newx, as.numeric(pretty))$y
                              axis(3, at=at, labels=labels, cex.axis=par('cex'))
                          }
                      }
                  } else if (which[w] == 2) {
                      latitude <- x[["latitude"]]
                      longitude <- x[["longitude"]]
                      jitter <- rnorm(length(latitude), sd=1e-8) # jitter lat by equiv 1mm to avoid colocation
                      distance <- geodDist(longitude, jitter+latitude, alongPath=TRUE) ## FIXME: jitter should be in imagep
                      depth <- x[["depth"]]
                      a <- x[["a"]]
                      if (despike)
                          a <- apply(a, 2, smooth)
                      z <- log10(ifelse(a > 1, a, 1)) # FIXME: make an argument for this '1'
                      deepestWater <- max(abs(depth))
                      if (!missing(drawBottom)) {
                          if (is.logical(drawBottom) && drawBottom)
                              drawBottom <- "lightgray"
                          waterDepth <- findBottom(x, ignore=ignore)
                          axisBottom <- par('usr')[3]
                          deepestWater <- max(abs(waterDepth$depth))
                      }
                      ats <- imagep(distance, -depth, z,
                                    xlab=if (missing(xlab)) "Distance [km]" else xlab,
                                    ylab=if (missing(ylab)) "z [m]" else ylab,
                                    ylim=if (missing(ylim)) c(-deepestWater, 0) else ylim,
                                    zlim=if (missing(zlim)) c(if (beam[w] %in% c("Sv", "TS")) min(z, na.rm=TRUE) else 0, max(z, na.rm=TRUE)) else zlim,
                                    mgp=mgp, mar=mar,
                                    tformat=tformat,
                                    col=col,
                                    drawPalette=drawPalette,
                                    debug=debug-1)
                      if (!missing(drawBottom)) {
                          if (is.logical(drawBottom) && drawBottom)
                              drawBottom <- "white"
                          ndistance <- length(distance)
                          distance2 <- c(distance[1], distance, distance[ndistance])
                          axisBottom <- par('usr')[3]
                          depth2 <- c(axisBottom, -depth[waterDepth$index], axisBottom)
                          polygon(distance2, depth2, col=drawBottom)
                      }
                      if (!missing(atTop)) {
                          at <- approx(as.numeric(x[["time"]]), distance, as.numeric(atTop))$y
                          if (missing(labelsTop))
                              labelsTop <- format(atTop, format=if ("format" %in% dotsNames)  dots$format else "%H:%M:%S")
                          axis(side=3, at=at, labels=labelsTop, cex.axis=par('cex'))
                      }
                      if (drawTimeRange) {
                          timeRange <- range(x[['time']])
                          label <- paste(timeRange[1], timeRange[2], sep=" to ")
                          mtext(label, side=3, cex=0.9*par('cex'), adj=0)
                      }
                      res$xat <- ats$xat
                      res$yat <- ats$yat
                  } else if (which[w] == 3) {
                      lat <- x[["latitude"]]
                      lon <- x[["longitude"]]
                      asp <- 1 / cos(mean(range(lat, na.rm=TRUE))*pi/180)
                      latm <- mean(lat, na.rm=TRUE)
                      lonm <- mean(lon, na.rm=TRUE)
                      if (missing(radius))
                          radius <- max(geodDist(lonm, latm, lon, lat))
                      else
                          radius <- max(radius, geodDist(lonm, latm, lon, lat))
                      km_per_lat_deg <- geodDist(lonm, latm, lonm, latm+1)
                      km_per_lon_deg <- geodDist(lonm, latm, lonm+1, latm)
                      lonr <- lonm + radius / km_per_lon_deg * c(-2, 2)
                      latr <- latm + radius / km_per_lat_deg * c(-2, 2)
                      plot(lonr, latr, asp=asp, type='n',
                           xlab=if (missing(xlab)) "Longitude" else xlab,
                           ylab=if (missing(ylab)) "Latitude" else ylab)
                      xaxp <- par("xaxp")
                      xat <- seq(xaxp[1], xaxp[2], length.out=1+xaxp[3])
                      yaxp <- par("yaxp")
                      yat <- seq(yaxp[1], yaxp[2], length.out=1+yaxp[3])
                      ats <- list(xat=xat, yat=yat)

                      if (!missing(coastline)) {
                          coastline <- coastline
                          if (!is.null(coastline@metadata$fillable) && coastline@metadata$fillable) {
                              polygon(coastline[["longitude"]], coastline[["latitude"]], col="lightgray", lwd=3/4)
                              polygon(coastline[["longitude"]]+360, coastline[["latitude"]], col="lightgray", lwd=3/4)
                              box()
                          } else {
                              lines(coastline[["longitude"]], coastline[["latitude"]], col="darkgray")
                              lines(coastline[["longitude"]]+360, coastline[["latitude"]], col="darkgray")
                          }
                      }
                      lines(lon, lat, col=if (!is.function(col)) col else "black", lwd=lwd)
                  }
              }
              oceDebug(debug, "} # plot.echosounder()\n", unindent=1)
              invisible(res)
          })


#' @title Read an Echosounder File
#'
#' @description
#' Reads a biosonics echosounder file.  This function was written for and
#' tested with single-beam, dual-beam, and split-beam Biosonics files of type
#' V3, and may not work properly with other file formats.
#'
#' @param file a connection or a character string giving the name of the file
#' to load.
#' @param channel sequence number of channel to extract, for multi-channel
#' files.
#' @param soundSpeed sound speed, in m/s. If not provided, this is calculated
#' using \code{\link{swSoundSpeed}(35, 15, 30, eos="unesco")}.  (In theory,
#' it could be calculated using the temperature and salinity that are stored
#' in the data file, but these will just be nominal values, anyway.
#' @param tz character string indicating time zone to be assumed in the data.
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, or to 2 to get more.
#' @param processingLog if provided, the action item to be stored in the log,
#' typically only provided for internal calls.
#' @return An object of \code{\link[base]{class}} \code{"echosounder"} with
#' standard slots \code{metadata}, \code{data} and \code{processingLog} that
#' are described in the documentation for the object
#' \code{\link{echosounder-class}}.
#' @section Bugs: Only the amplitude information (in counts) is determined.  A
#' future version of this function may provide conversion to dB, etc.  The
#' handling of dual-beam and split-beam files is limited.  In the dual-beam
#' cse, only the wide beam signal is processed (I think ... it could be the
#' narrow beam, actually, given the confusing endian tricks being played).  In
#' the split-beam case, only amplitude is read, with the x-axis and y-axis
#' angle data being ignored.
#' @author Dan Kelley, with help from Clark Richards
#' @seealso The documentation for \code{\link{echosounder-class}} explains the
#' structure of \code{ctd} objects, and also outlines the other functions
#' dealing with them.
#' @references Various echosounder instruments provided by BioSonics are
#' described at the company website, \url{https://www.biosonicsinc.com/}.  The
#' document listed as [1] below was provided to the author of this function in
#' November 2011, which suggests that the data format was not changed since
#' July 2010.
#'
#' [1] Biosonics, 2010.  DT4 Data File Format Specification.  BioSonics
#' Advanced Digital Hydroacoustics. July, 2010.  SOFTWARE AND ENGINEERING
#' LIBRARY REPORT BS&E-2004-07-0009-2.0.
#' @family things related to echosounder data
read.echosounder <- function(file, channel=1, soundSpeed,
                             tz=getOption("oceTz"), debug=getOption("oceDebug"),
                             processingLog)
{
    oceDebug(debug, "read.echosounder(file=\"", file, "\", tz=\"", tz, "\", debug=", debug, ") {\n", sep="", unindent=1)
    ##ofile <- file
    filename <- NULL
    if (is.character(file)) {
        filename <- fullFilename(file)
        file <- file(file, "rb")
        on.exit(close(file))
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        open(file, "rb")
        on.exit(close(file))
    }
    res <- new("echosounder", filename=filename)
    seek(file, 0, "start")
    seek(file, where=0, origin="end")
    fileSize <- seek(file, where=0)
    oceDebug(debug, "fileSize=", fileSize, "\n")
    buf <- readBin(file, what="raw", n=fileSize, endian="little")

    ## [1 p9]: "After this comes the main data section of the file.
    ## This will typically contain some or all of the following tuples:
    ## ping tuples (type 0x0015, 0x001C or 0x001D),
    ## time tuples (type 0x000F or 0x0020),
    ## position tuples (type 0x000E),
    ## navigation string tuples (type 0x0011 or 0x0030),
    ## transducer orientation tuples (type 0x0031),
    ## bottom pick tuples (type 0x0032),
    ## single echoes tuples (type 0x0033), and
    ## comment tuples (type 0x0034).

    ## [1 sec 3.3 ] describes the file format.  NB: files are little endian.
    ##
    ## Data are organized as a sequence of tuples, in the following format:
    ##   N = 2-byte unsigned int that indicates the size of the tuple.
    ##   code = 2-byte code (see table below)
    ##   data = N bytes (depends on code)
    ##   N6 = 2 bytes that must equal N+6, or the data are corrupted
    ##
    ## The codes, from the table in [1 sec 3.5] are as follows.
    ## The first tuple in a file must have code 0xFFFF, and the
    ## second must have code 001E, 0018, or 0001.
    ##
    ##   0xFFFF Signature (for start of file)
    ##   0x001E V3 File Header
    ##   0x0018 V2 File Header
    ##   0x0001 V1 File Header
    ##   0x0012 Channel Descriptor
    ##   0x0036 Extended Channel Descriptor
    ##   0x0015 Single-Beam Ping
    ##   0x001C Dual-Beam Ping
    ##   0x001D Split-Beam Ping
    ##   0x000F or 0x0020 Time
    ##   0x000E Position
    ##   0x0011 Navigation String
    ##   0x0030 Timestamped Navigation String
    ##   0x0031 Transducer Orientation
    ##   0x0032 Bottom Pick
    ##   0x0033 Single Echoes
    ##   0x0034 Comment
    ##   0xFFFE End of File
    tuple <- 1
    offset <- 0
    timeSlow <- latitudeSlow <- longitudeSlow <- NULL # accumulate using c() because length unknown
    timeLast <- 0
    ##first <- TRUE
    scan <- 1
    ##intensity <- list()
    time <- list()
    ##samplingDeltat <- 2.4e-05 # a guess, to avoid being unknown if the header cannot be read
    channelNumber <- NULL
    ##channelID <- NULL
    channelDeltat <- NULL
    blankedSamples <- 0
    fileType <- "unknown"
    range <- NULL
    beamType <- "unknown"
    ## The next three lines are just to prevent code-diagnostic warnings;
    ## These matrices are redefined later, when we know the geometry
    a <- matrix(NA_real_, nrow=1, ncol=1)
    b <- matrix(NA_real_, nrow=1, ncol=1)
    c <- matrix(NA_real_, nrow=1, ncol=1)
    ## FIXME: find out whether samplesPerPing is always defined prior to use in the code1==0x15 blocks.
    ## The Rstudio code-diagnostic complains that this variable is used before being defined,
    ## but when I run test code there is no problem, because the variable has been defined. What I
    ## do *not* know is whether files will always have these byte groupsing in this order, but at
    ## least setting to a zero value is likely to cause an error, if that ever occurs. (I may just
    ## need to reorder some code, if problems arise.)
    samplesPerPing <- 0 ## overriddent later; here just to prevent code-diagnostic warning
    while (offset < fileSize) {
        ##print <- debug && tuple < 200
        N <- .C("uint16_le", buf[offset+1:2], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res
        code1 <- buf[offset+3]
        code2 <- buf[offset+4]
        code <- readBin(buf[offset+3:4], "integer", size=2, n=1, endian="small", signed=FALSE)
        if (debug > 3) cat("buf[", 3+offset, "] = code1 = 0x", code1, sep="")
        ## The ordering of the code1 tests is not too systematic here; frequently-encountered
        ## codes are placed first, but then it's a bit random.
        if (code1 == 0x15 || code1 == 0x1c || code1 == 0x1d) {
            ## single-beam, dual-beam, or split-beam tuple
            thisChannel <- .C("uint16_le", buf[offset+4+1:2], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res
            pingNumber <- readBin(buf[offset+6+1:4], "integer", size=4L, n=1L, endian="little")
            pingElapsedTime <- 0.001 * readBin(buf[offset+10+1:4], "integer", size=4L, n=1L, endian="little")
            ##message("samplersPerPing=", samplesPerPing)
            ns <- .C("uint16_le", buf[offset+14+1:2], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res # number of samples
            if (thisChannel == channelNumber[channel]) {
                if (debug > 3) {
                    cat("buf[", 1+offset, ", ...] (0x", code1, " single-beam ping)",
                        " scan=", scan,
                        " ping=", pingNumber,
                        " ns=", ns,
                        " channel=", thisChannel,
                        " elapsedTime=", pingElapsedTime,
                        "\n", sep="")
                }
                ## Note the time reversal in the assignment to the data matrix 'a'
                ## FIXME: is it faster to flip the data matrix later?
                if (code1 == 0x15) {
                    tmp <- do_biosonics_ping(buf[offset+16+1:(2*ns)], samplesPerPing, ns, 0)
                    beamType <- "single-beam"
                } else if (code1 == 0x1c) {
                    tmp <- do_biosonics_ping(buf[offset+16+1:(4*ns)], samplesPerPing, ns, 1)
                    beamType <- "dual-beam"
                } else if (code1 == 0x1d) {
                    ## e.g. 01-Fish.dt4 sample file from Biosonics
                    tmp <- do_biosonics_ping(buf[offset+16+1:(4*ns)], samplesPerPing, ns, 2)
                    beamType <- "split-beam"
                } else {
                    stop("unknown 'tuple' 0x", code1, sep="")
                }
                a[scan, ] <- rev(tmp$a)
                b[scan, ] <- rev(tmp$b)
                c[scan, ] <- rev(tmp$c)
                time[[scan]] <- timeLast # FIXME many pings between times, so this is wrong
                scan <- scan + 1
                if (debug > 3) cat("channel:", thisChannel, "ping:", pingNumber, "pingElapsedTime:", pingElapsedTime, "\n")
           } else {
               if (debug > 0) {
                   cat("buf[", 1+offset, ", ...] = 0x", code1,
                       " ping=", pingNumber, " ns=", ns, " channel=", thisChannel, " IGNORED since wrong channel)\n", sep="")
                }
            }
        } else if (code1 == 0x0f || code == 0x20) {
            ## time
            timeSec <- readBin(buf[offset+4 + 1:4], what="integer", endian="little", size=4, n=1)
            timeSubSec <- .C("biosonics_ss", buf[offset+10], res=numeric(1), NAOK=TRUE, PACKAGE="oce")$res
            timeFull <- timeSec + timeSubSec
            timeElapsedSec <- readBin(buf[offset+10+1:4], what="integer", endian="little", size=4, n=1)/1e3
            ## centisecond = ss & 0x7F [1 sec 4.7]
            timeLast <- timeSec + timeSubSec # used for positioning
            if (debug > 3) cat(sprintf(" time calendar: %s   elapsed %.2f\n", timeFull+as.POSIXct("1970-01-01 00:00:00", tz="UTC"), timeElapsedSec))
        } else if (code1 == 0x0e) {
            ## position
            lat <- readBin(buf[offset + 4 + 1:4], "integer", endian="little", size=4, n=1) / 6e6
            lon <- readBin(buf[offset + 8 + 1:4], "integer", endian="little", size=4, n=1) / 6e6
            latitudeSlow <- c(latitudeSlow, lat)
            longitudeSlow <- c(longitudeSlow, lon)
            timeSlow <- c(timeSlow, timeLast)
            if (debug > 3) cat(" position", lat, "N", lon, "E\n")
        } else if (code2 == 0xff) {
            if (tuple == 1) {
                if (debug > 1) cat(" file start code\n")
            } else {
                break
            }
        } else if (code1 == 0x11) {
            if (debug > 3) cat(" navigation string IGNORED\n")
        } else if (code1 == 0x12) {
            channelNumber <- c(channelNumber, .C("uint16_le", buf[offset+4+1:2], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res)
            ib <- .C("uint16_le", buf[offset+20+1:2], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res
            blankedSamples <- ib
            channelDeltat <- c(channelDeltat, 1e-9*.C("uint16_le", buf[offset+12+1:2], 1L, res=integer(1), NAOK=TRUE,
                                                      PACKAGE="oce")$res)
            pingsInFile <- readBin(buf[offset+6+1:4], "integer", n=1, size=4, endian="little")
            samplesPerPing <- .C("uint16_le", buf[offset+10+1:2], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res # ssp [p13 1]
            sp <- 1e-9 * readBin(buf[offset+12+1:2], "integer", n=1L, size=2L, endian="little") # [p13 1] time between samples (ns)
            if (debug > 1) cat('sp: ', sp, ' sample period in ns\n', sep='')
            pud <- readBin(buf[offset+16+1:2], "integer", n=1L, size=2L, endian="little")
            if (debug > 1) cat('pud: ', pud, ' pulse duration in us (expect 400 for 01-Fish.dt4)\n', sep='')
            pp <- .C("uint16_le", buf[offset+18+1:2], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res # [p13 1] ping period (ms)
            if (debug > 1) cat('pp: ', pp, '\n', sep='')
            ib <- .C("uint16_le", buf[offset+20+1:2], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res # [p13 1]
            if (debug > 1) cat('ib: ', ib, '\n', sep='')
            ## next 2 bytes contain u2, ignored
            th <- 0.01 * readBin(buf[offset+24+1:2], "integer", n=1L, size=2L, endian="little") # [p13 1]
            if (debug > 1) cat('th: ', th, ' data collection threshold in dB (expect -65 for 01-Fish.dt4)\n', sep='')
            rxee <- buf[offset+26+1:128] # [1 p13] # receiver EEPROM image (FIXME: why is serialnum out by 1? INDEX question)
            ##corr <- .C("int16_le", buf[offset+282+1:2], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res # [p13 1]
            corr <- 0.01 * readBin(buf[offset+282+1:2], "integer", n=1L, size=2L, endian="little") # [p13 1]
            if (debug > 1) cat('corr: ', corr, ' user-defined calibration correction in dB (expect 0 for 01-Fish.dt4)\n', sep='')

            if (1 == length(channelNumber)) {
                ## get space
                a <- matrix(NA_real_, nrow=pingsInFile, ncol=samplesPerPing)
                b <- matrix(NA_real_, nrow=pingsInFile, ncol=samplesPerPing)
                c <- matrix(NA_real_, nrow=pingsInFile, ncol=samplesPerPing)
            }
            if (debug > 3) cat(" channel descriptor ",
                           " number=", tail(channelNumber, 1),
                           " blankedSamples=", blankedSamples,
                           " dt=", tail(channelDeltat, 1),
                           " pingsInFile=", pingsInFile,
                           " samplesPerPing=", samplesPerPing,
                           "\n")
        } else if (code1 == 0x30) {
            if (debug > 3) cat(" time-stamped navigation string IGNORED\n")
        } else if (code1 == 0xff && tuple > 1) {
            if (debug > 3) cat("  EOF\n")
        } else if (code1 == 0x1e) {
            if (debug > 1) cat(" V3 file header\n")
            fileType <- if (buf[offset + 1] == 0x10 & buf[offset + 2] == 0x00) "DT4 v2.3" else "DT4 pre v2.3"
            res@metadata$temperature <- 0.01*.C("uint16_le", buf[offset+8+1:2], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res
            if (debug > 1) cat("temperature=", res@metadata$temperature, "degC (expect 14 for 1-Fish.dt4)\n")
            res@metadata$salinity <- 0.01*.C("uint16_le", buf[offset+10+1:2], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res
            if (debug > 1) cat("salinity=", res@metadata$salinity, "PSU (expect 30 for 1-Fish.dt4)\n")
            ## res@metadata$soundSpeed <- swSoundSpeed(res@metadata$salinity, res@metadata$temperature, 30,
            ##                                        eos="unesco")
            res@metadata$transmitPower <- 0.01*.C("uint16_le", buf[offset+12+1:2], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res
            if (debug > 1) cat("transmitPower=", res@metadata$transmitPower, "(expect 0 for 1-Fish.dt4; called mTransmitPower)\n")
            res@metadata$tz <- readBin(buf[offset+16+1:2], "integer", size=2)
            if (debug > 1) cat("tz=", res@metadata$tz, "(expect -420 for 1-Fish.dt4)\n")
            dst <- readBin(buf[offset+18+1:2], "integer", size=2)
            res@metadata$dst <- dst != 0
            if (debug > 1) cat("dst=", res@metadata$dst, "(daylight saings time) expect TRUE for 1-Fish.dt4)\n")
        } else if (code1 == 0x18) {
            warning("Biosonics file of type 'V2' detected ... errors may crop up")
            fileType <- "V2"
        } else if (code1 == 0x01) {
            warning("Biosonics file of type 'V1' detected ... errors may crop up")
            fileType <- "V1"
        } else if (code1 == 0x32) {
            if (debug > 3) cat(" bottom pick tuple [1 sec 4.12] ")
            ##thisChannel <- .C("uint16_le", buf[offset+4+1:2], 1L, res=integer(1))$res
            ##thisPing <- .C("uint16_le", buf[offset+4+1:2], 1L, res=integer(1))$res
            foundBottom <- .C("uint16_le", buf[offset+14+1:2], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res
            if (foundBottom) {
                thisRange <- readBin(buf[offset+20+1:4], "numeric", size=4, n=1, signed=TRUE, endian="little")
            } else {
                thisRange <- NA
            }
            range <- c(range, thisRange)
            if (debug > 3) cat(" thisRange:", thisRange)
        } else if (code1 == 0x36) {
            if (debug > 3) cat(" extended channel descriptor IGNORED\n")
        } else if (code1 == 0x33) {
            if (debug > 3) cat(" single echo tuple IGNORED ... see p26 of DT4_format_2010.pdf\n")
        } else if (code1 == 0x34) {
            if (debug > 1) cat(" comment tuple [1 sec 4.14 p28]\n")
            ## FIXME: other info could be gleaned from the comment, if needed
            numbytes <- .C("uint16_le", buf[offset+34:35], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res
            if (debug > 1) cat('numbytes:', numbytes, ' ... NOTHING ELSE DECODED in this verion of oce.\n')
        } else {
            if (debug > 3) cat(" unknown code IGNORED\n")
        }
        N6 <- .C("uint16_le", buf[offset+N+5:6], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res
        if (N6 != N + 6)
            stop("error reading tuple number ", tuple, " (mismatch in redundant header-length flags)")
        offset <- offset + N + 6
        tuple <- tuple + 1
    }
    res@metadata$beamType <- beamType
    res@metadata$channel <- channel
    res@metadata$fileType <- fileType
    res@metadata$blankedSamples <- blankedSamples
    if (missing(soundSpeed)) {
        res@metadata$soundSpeed <- swSoundSpeed(35, 10, 30, eos="unesco")
    } else {
        res@metadata$soundSpeed <- soundSpeed
    }
    res@metadata$soundSpeed <- if (missing(soundSpeed)) swSoundSpeed(35, 10, 30, eos="unesco") else soundSpeed
    res@metadata$samplingDeltat <- channelDeltat[1] # nanoseconds
    res@metadata$pingsInFile <- pingsInFile
    res@metadata$samplesPerPing <- samplesPerPing
    ## channel info, with names matching [1 p13]
    ##res@metadata$samplingDeltat <- sp
    res@metadata$pulseDuration <- pud
    res@metadata$pp <- pp
    res@metadata$ib <- ib
    res@metadata$th <- th
    res@metadata$rxee <- rxee
    res@metadata$correction <- corr

    depth <- rev(blankedSamples + seq(1, dim(a)[2])) * res@metadata$soundSpeed * res@metadata$samplingDeltat / 2
    ## test: for 01-Fish.dt4, have as follows:
    ##     <mPingBeginRange_m>0.988429069519043</mPingBeginRange_m>
    ##     <mPingEndRange_m>64.787033081054688</mPingEndRange_m>
    ## but we get as follows:
    ##     range(d[['depth']])
    ##     [1]  1.001714 64.485323
    ## i.e. a 12cm error at top and a 20cm error at bottom.  Note that
    ##    > diff(d[['depth']][2:1])
    ##    [1] 0.01788775
    ## FIXME: check depth mismatch relates to (a) sound speed or (b) geometry.  (Small error; low priority.)

    time <- as.numeric(time)

    ## interpolate to "fast" latitude and longitude, after extending to ensure spans
    ## enclose the ping times.
    n <- length(latitudeSlow)
    ##t <- c(2*timeSlow[1]-timeSlow[2], timeSlow, 2*timeSlow[n] - timeSlow[n-1])
    approx2 <- function(x, y, xout)
    {
        nxout <- length(xout)
        before <- y[1] + (y[2] - y[1]) * (xout[1] - x[1]) / (x[2] - x[1])
        after <- y[n-1] + (y[n] - y[n-1]) * (xout[nxout] - x[n-1]) / (x[n] - x[n-1])
        approx(c(xout[1], x, xout[nxout]), c(before, y, after), xout)$y
    }
    latitude <- approx2(timeSlow, latitudeSlow, time)
    longitude <- approx2(timeSlow, longitudeSlow, time)

    res@metadata$transducerSerialNumber <- readBin(rxee[2+1:8], "character") # [1 p16] offset=2 length 8
    if (debug > 1) cat("transducerSerialNumber '", res@metadata$transducerSerialNumber, "' (expect DT600085 for 01-Fish.dt4)\n", sep="")
    res@metadata$calibrationTime <- numberAsPOSIXct(readBin(rxee[36+1:4], 'integer'), tz="UTC") # [1 p16] offset=36
    if (debug > 1) cat("calibrationTime: ", format(res@metadata$calibrationTime), " (expect Thu Apr 13 02:36:38 2000 for 01-Fish.dt4)\n", sep="")
    ##res@metadata$sl <- 0.1 * .C("int16_le", rxee[58+1:2], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res # [p16 1]
    res@metadata$sourceLevel <- 0.1 * readBin(rxee[58+1:2], "integer", n=1L, size=2L, endian="little") # [p16 1]
    if (debug > 1) cat('sl=', res@metadata$sourceLevel, ' (expect 220 for 01-Fish.dt4 source level SourceLevel_dBuPa)\n', sep='')
    #res@metadata$rs <- 0.1 * .C("int16_le", rxee[1+64+1:2], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res # [p16 1]
    res@metadata$receiverSensitivity <- 0.1 * readBin(rxee[64+1:2], "integer", n=1L, size=2) # [p16 1]
    if (debug > 1) cat('rs=', res@metadata$receiverSensitivity, ' receiver sensitivity in dB (counts/micro Pa) (expect -58.8 for 01-Fish.dt4)\n', sep='')

    res@metadata$trtype <- .C("uint16_le", rxee[84+1:2], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res # [p16 1]
    if (debug > 1) cat('trtype=', res@metadata$trtype, ' hardware [not deployment] transducer type (0 single, 3 dual, 4 split) expect 1 for 01-Fish.dt4\n', sep='')
    res@metadata$frequency <- readBin(rxee[86+1:4], "integer", n=1L, size=4) # [p16 1]
    if (debug > 1) cat('fq=', res@metadata$frequency, ' transducer frequency, Hz (expect 208000 for 01-Fish.dt4)\n', sep='')


    res@metadata$beamwidthX <- 0.1 * as.numeric(rxee[1+100]) # [1 p16] offset=100
    res@metadata$beamwidthY <- 0.1 * as.numeric(rxee[1+101]) # [1 p16] offset=101
    if (debug > 1) cat("beamwidthX=", res@metadata$beamwidthX, " (expect 6.5 for 01-Fish.dt4; called BeamWidthX_deg)\n", sep="")
    if (debug > 1) cat("beamwidthY=", res@metadata$beamwidthY, " (expect 6.5 for 01-Fish.dt4; called BeamWidthY_deg)\n", sep="")
    ##old: psi <- res@metadata$bwy / 2 * res@metadata$bwx / 2 * 10^(-3.16) # biosonics has /20 because they have bwx in 0.1deg
    ##old: if (debug > 1) cat("psi=", psi, "\n", sep="")
    ##old: ## c = sound speed (inferred from sal and tem FIXME could use pressure I suppose)
    ##old: ## r = range
    ##old: ##Sv <- 20*log10(a) -(sl+rs+tpow)/10.0 +20*log10(r) +2*a*r -10*log10(c*pud/1000000.0*psi/2.0) +corr/100.0
    ##old: range <- rev(depth)

    ##old: ## NB: In the calculations of Sv and TS, the terms with sl, rs and tpow
    ##old: ## are not not divided by 10, as in [1 p34 and 35], because here those
    ##old: ## quantities  are stored in dB, not 0.1 dB.  Similarly, corr is
    ##old: ## not divided by 100 because it is in dB, not 0.01 dB.

    ##old: ## backscattering strength (Sv) in dB [1 p34]
    ##old: absorption <- swSoundAbsorption(res@metadata$fq, 35, 10, mean(range))
    ##old: if (debug > 1) cat("sound absorption:", absorption, "dB/m\n")
    ##old: r <- matrix(rev(range), nrow=dim(a)[1], ncol=length(range), byrow=TRUE)
    ##old: Sv <- 20*log10(a) - (res@metadata$sl+res@metadata$rs+res@metadata$tpow) + 20*log10(r) + 2*absorption*r- 10*log10(soundSpeed*res@metadata$pulseDuration/1e6*psi/2) + corr
    ##old: Sv[!is.finite(Sv)] <- NA
    ##old: ## target strength (TS) in dB [1 p35]
    ##old: TS <- 20*log10(a) - (res@metadata$sl+res@metadata$rs+res@metadata$tpow) + 40*log10(r) + 2*absorption*r+ corr
    ##old: TS[!is.finite(TS)] <- NA

    res@data <- list(timeSlow=timeSlow+as.POSIXct("1970-01-01 00:00:00", tz="UTC"),
                     latitudeSlow=latitudeSlow,
                     longitudeSlow=longitudeSlow,
                     depth=depth,
                     ##range=range,
                     time=time+as.POSIXct("1970-01-01 00:00:00", tz="UTC"),
                     latitude=latitude,
                     longitude=longitude,
                     a=a, b=b, c=c)
    if (res@metadata$beamType == "single-beam") {
        res@data$b <- NULL
        res@data$c <- NULL
    }
    names <- names(res@data)
    if ("latitude" %in% names) res@metadata$units$latitude <- list(unit=expression(degree*N), scale="")
    if ("longitude" %in% names) res@metadata$units$longitude <- list(unit=expression(degree*E), scale="")
    if ("latitudeSlow" %in% names) res@metadata$units$latitudeSlow <- list(unit=expression(degree*N), scale="")
    if ("longitudeSlow" %in% names) res@metadata$units$longitudeSlow <- list(unit=expression(degree*E), scale="")
    if ("depth" %in% names) res@metadata$units$depth <- list(unit=expression(m), scale="")

    if (!missing(processingLog))
        res@processingLog <- processingLogItem(processingLog)
    res@processingLog <- processingLogAppend(res@processingLog,
                                             paste("read.echosounder(\"", filename, "\", channel=", channel, ", soundSpeed=",
                                                   if (missing(soundSpeed)) "(missing)" else soundSpeed, ", tz=\"", tz, "\", debug=", debug, ", processingLog)", sep=""))
    .C("biosonics_free_storage", PACKAGE="oce") # clear temporary storage space
    res
}
