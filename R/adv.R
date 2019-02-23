#' Class to Store adv Data
#'
#' This class holds data from acoustic-Doppler velocimeters.
#'
#' A file containing ADV data is usually recognized by Oce, and so
#' \code{\link{read.oce}} will usually read the data.  If not, one may use the
#' general ADV function \code{\link{read.adv}} or specialized variants
#' \code{\link{read.adv.nortek}}, \code{\link{read.adv.sontek.adr}} or
#' \code{\link{read.adv.sontek.text}}.
#'
#' ADV data may be plotted with \code{\link{plot,adv-method}} function, which is a
#' generic function so it may be called simply as \code{plot(x)}, where
#' \code{x} is an object inheriting from \code{\link{adv-class}}.
#'
#' Statistical summaries of ADV data are provided by the generic function
#' \code{\link{summary,adv-method}}.
#'
#' Conversion from beam to xyz coordinates may be done with
#' \code{\link{beamToXyzAdv}}, and from xyz to enu (east north up) may be done
#' with \code{\link{xyzToEnuAdv}}.  \code{\link{toEnuAdv}} may be used to
#' transfer either beam or xyz to enu.  Enu may be converted to other
#' coordinates (e.g. aligned with a coastline) with
#' \code{\link{enuToOtherAdv}}.
#'
#' @templateVar class adv
#'
#' @templateVar dataExample The key items stored in this slot include \code{time} and \code{v}.
#'
#' @templateVar metadataExample Examples that are of common interest include \code{frequency}, \code{oceCordinate}, and \code{frequency}.
#'
#' @template slot_summary
#'
#' @template slot_put
#'
#' @template slot_get
#'
#' @examples
#' data(adv)
#' adv[["v"]] <- 0.001 + adv[["v"]] # add 1mm/s to all velocity components
#'
#' @family classes provided by \code{oce}
#' @family things related to \code{adv} data
setClass("adv", contains="oce")

#' ADV (acoustic-doppler velocimeter) dataset
#'
#' This \code{\link{adv-class}} object is a sampling of measurements made with a
#' Nortek Vector acoustic Doppler velocimeter deployed as part of the St Lawrence
#' Internal Wave Experiment (SLEIWEX).  Various identifying features have been
#' redacted.
#'
#' @name adv
#'
#' @docType data
#'
#' @usage data(adv)
#'
#' @examples
#'\donttest{
#' library(oce)
#' data(adv)
#'
#' # Velocity time-series
#' plot(adv)
#'
#' # Spectrum of upward component of velocity, with ``turbulent'' reference line
#' s <- spectrum(adv[["v"]][,3],plot=FALSE)
#' plot(log10(s$freq), log10(s$spec), type='l')
#' for (a in seq(-20, 20, by=1))
#'     abline(a=a, b=-5/3, col='gray', lty='dotted')
#'}
#'
#' @source This file came from the SLEIWEX-2008 experiment.
#'
#' @family datasets provided with \code{oce}
#' @family things related to \code{adv} data
NULL


setMethod(f="initialize",
          signature="adv",
          definition=function(.Object, time, v, a, q, filename) {
              if (!missing(time)) .Object@data$time <- time
              if (!missing(v)) .Object@data$v <- v
              if (!missing(a)) .Object@data$a <- a
              if (!missing(q)) .Object@data$q <- q
              .Object@metadata$filename <- if (missing(filename)) "" else filename
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'adv' object"
              return(.Object)
          })



#' Summarize an ADV object
#'
#' Summarize data in an \code{adv} object.
#'
#' @param object an object of class \code{"adv"}, usually, a result of a call to
#' \code{\link{read.adv}}.
#'
#' @param ... further arguments passed to or from other methods.
#'
#' @examples
#' library(oce)
#' data(adv)
#' summary(adv)
#'
#' @author Dan Kelley
#'
#' @family things related to \code{adv} data
setMethod(f="summary",
          signature="adv",
          definition=function(object, ...) {
              cat("ADV Summary\n-----------\n\n", ...)
              cat(paste("* Instrument:             ", object@metadata$instrumentType,
                        ", serial number ``", object@metadata$serialNumber, "``\n", sep=""))
              cat(paste("* Source filename:        ``", object@metadata$filename, "``\n", sep=""))
              if ("latitude" %in% names(object@metadata)) {
                  cat(paste("* Location:              ",
                            if (is.na(object@metadata$latitude)) "unknown latitude" else sprintf("%.5f N", object@metadata$latitude), ", ",
                            if (is.na(object@metadata$longitude)) "unknown longitude" else sprintf("%.5f E", object@metadata$longitude), "\n"))
              }
              invisible(callNextMethod()) # summary
          })

#' @title Extract Something from an adv Object
#'
#' @param x An \code{adv} object, i.e. one inheriting from \code{\link{adv-class}}.
#'
#' @examples
#' data(adv)
#' head(adv[["q"]])            # in raw form
#' head(adv[["q", "numeric"]]) # in numeric form
#'
#' @template sub_subTemplate
#'
#' @section Details of the specialized \code{adv} method:
#'
#' In addition to the usual extraction of elements by name, some shortcuts
#' are also provided, e.g. \code{u1} retrieves \code{v[,1]}, and similarly
#' for the other velocity components. The \code{a} and \code{q}
#' data can be retrieved in \code{\link{raw}} form
#' or numeric form; see \dQuote{Examples}.
#'
#' It is also worth noting that heading, pitch, etc. may be stored in
#' "slow" form in the object (e.g. in \code{headingSlow} within
#' the \code{data} slot). In that case, accessing by full name, e.g.
#' \code{x[["headingSlow"]]} retrieves the item as expected, but
#' \code{x[["heading"]]} interpolates to the faster timescale, using
#' \code{\link{approx}(timeSlow, headingSlow, time)}.
#'
#' @author Dan Kelley
#'
#' @family things related to \code{adv} data
setMethod(f="[[",
          signature(x="adv", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              haveSlow <- "timeSlow" %in% names(x@data)
              if (i == "u1") {
                  return(x@data$v[, 1])
              } else if (i == "u2") {
                  return(x@data$v[, 2])
              } else if (i == "u3") {
                  return(x@data$v[, 3])
              } else if (i == "a") {
                  if (!missing(j) && j == "numeric") {
                      res <- x@data$a
                      dim <- dim(res)
                      res <- as.numeric(res)
                      dim(res) <- dim
                  } else {
                      res <- x@data$a
                  }
                  return(res)
              } else if (i == "q") {
                  if (!missing(j) && j == "numeric") {
                      res <- x@data$q
                      dim <- dim(res)
                      res <- as.numeric(res)
                      dim(res) <- dim
                  } else {
                      res <- x@data$q
                  }
                  return(res)
              } else if (i %in% c("heading", "pitch", "roll")) {
                  if (haveSlow) {
                      ## offset the time so approx doesn't yield all NAs
                      tSlow <- as.numeric(x@data$timeSlow)
                      t <- as.numeric(x@data$time)
                      t0 <- t[1]
                      res <- approx(tSlow-t0, x@data[[i]], t-t0)$y
                  } else {
                      res <- x@data[[i]]
                  }
              } else {
                  callNextMethod() # [[
              }
          })

#' Replace Parts of an ADV Object
#'
#' @details
#' If the \code{adv} object holds slow variables (i.e. if \code{timeSlow} is
#' in the \code{data} slot), then assigning to .e.g. \code{heading} will not
#' actually assign to a variable of that name, but instead assigns to
#' \code{headingSlow}. To catch misapplication of this rule, an error
#' message will be issued if the assigned value is not of the same length
#' as \code{timeSlow}.
#'
#' @param x An \code{adv} object, i.e. one inheriting from \code{\link{adv-class}}.
#' @param value The value to be inserted into \code{x}.
#'
#' @author Dan Kelley
#'
#' @template sub_subTemplate
#' @family things related to \code{adv} data
setMethod(f="[[<-",
          signature="adv",
          definition=function(x, i, j, ..., value) {
              ## FIXME: use j for e.g. times
              haveSlow <- "timeSlow" %in% names(x@data)
              if (i %in% names(x@metadata)) {
                  x@metadata[[i]] <- value
              } else if (i %in% names(x@data)) {
                 x@data[[i]] <- value
              } else if (i %in% c("heading", "pitch", "roll")) {
                  ## do not store as indicated; interpolate to the Slow variant
                  if (haveSlow) {
                      name <- paste(i, "Slow", sep="")
                      if (!(name %in% names(x@data)))
                          stop("no variable named '", name, "' in object's data slot")
                      if (length(value) != length(x@data[[name]]))
                          stop("length mismatch; value has ", length(value), " but ", name, " has ", length(x@data[[name]]), " elements")
                      x@data[[name]] <- value
                  } else {
                      x@data[[i]] <- value
                  }
              } else {
                  x <- callNextMethod(i=i, j=j, ...=..., value=value) # [[<-
              }
              ## Not checking validity because user may want to shorten items one by one, and check validity later.
              ## validObject(x)
              invisible(x)
          })



#' Subset an ADV Object
#'
#' Subset an adv (acoustic Doppler profile) object.  This function is somewhat
#' analogous to \code{\link{subset.data.frame}}, except that subsets can only be
#' specified in terms of \code{time}.
#'
#' @param x An \code{adv} object, i.e. one inheriting from \code{\link{adv-class}}.
#'
#' @param subset a condition to be applied to the \code{data} portion of \code{x}.
#' See \sQuote{Details}.
#'
#' @param \dots ignored.
#'
#' @return
#' A new \code{\link{adv-class}} object.
#'
#' @examples
#' library(oce)
#' data(adv)
#' plot(adv)
#' plot(subset(adv, time < mean(range(adv[['time']]))))
#'
#' @author Dan Kelley
#' @family things related to \code{adv} data
#' @family functions that subset \code{oce} objects
setMethod(f="subset",
          signature="adv",
          definition=function(x, subset, ...) {
              subsetString <- paste(deparse(substitute(subset)), collapse=" ")
              res <- x
              dots <- list(...)
              debug <- if (length(dots) && ("debug" %in% names(dots))) dots$debug else getOption("oceDebug")
              if (missing(subset))
                  stop("must give 'subset'")

              if (missing(subset))
                  stop("must specify a 'subset'")
              if (length(grep("time", subsetString))) {
                  oceDebug(debug, "subsetting an adv object by time\n")
                  keep <- eval(substitute(subset), x@data, parent.frame(2)) # used for $ts and $ma, but $tsSlow gets another
                  sum.keep <- sum(keep)
                  if (sum.keep < 2)
                      stop("must keep at least 2 profiles")
                  oceDebug(debug, "keeping", sum.keep, "of the", length(keep), "time slots\n")
                  oceDebug(debug, vectorShow(keep, "keeping bins:"))
                  res <- x
                  names <- names(x@data)
                  haveSlow <- "timeSlow" %in% names
                  keep <- eval(substitute(subset), x@data, parent.frame(2)) # used for $ts and $ma, but $tsSlow gets another
                  if (haveSlow) {
                      subsetStringSlow <- gsub("time", "timeSlow", subsetString)
                      keepSlow <-eval(parse(text=subsetStringSlow), x@data, parent.frame(2))
                  }
                  if ("timeBurst" %in% names) {
                      subsetStringBurst <- gsub("time", "timeBurst", subsetString)
                      keepBurst <-eval(parse(text=subsetStringBurst), x@data, parent.frame(2))
                  }
                  for (name in names(x@data)) {
                      if ("distance" == name)
                          next
                      if (length(grep("Burst$", name))) {
                          res@data[[name]] <- x@data[[name]][keepBurst]
                      } else if (length(grep("^time", name)) || is.vector(res@data[[name]])) {
                          if (1 == length(agrep("Slow$", name))) {
                              oceDebug(debug, "subsetting data$", name, " (using an interpolated subset)\n", sep="")
                              res@data[[name]] <- x@data[[name]][keepSlow]
                          } else {
                              oceDebug(debug, "subsetting data$", name, "\n", sep="")
                              res@data[[name]] <- x@data[[name]][keep]
                          }
                      } else if (is.matrix(res@data[[name]])) {
                          oceDebug(debug, "subsetting data$", name, ", which is a matrix\n", sep="")
                          res@data[[name]] <- x@data[[name]][keep, ]
                      } else if (is.array(res@data[[name]])) {
                          oceDebug(debug, "subsetting data$", name, ", which is an array\n", sep="")
                          res@data[[name]] <- x@data[[name]][keep, , ]
                      }
                  }
              } else {
                  stop("only 'time' is permitted for subsetting")
              }
              res@metadata$numberOfSamples <- dim(res@data$v)[1]
              res@processingLog <- processingLogAppend(res@processingLog, paste("subset(x, subset=", subsetString, ")", sep=""))
              res
          })


#' @template readAdvTemplate
#' @param type character string indicating type of file, and used by
#' \code{read.adv} to dispatch to one of the speciality functions.
#' @param start the time of the first sample, typically created with
#' \code{\link{as.POSIXct}}.  This may be a vector of times,
#' if \code{filename} is a vector of file names.
#' @param deltat the time between samples. (This is mandatory if
#' \code{header=FALSE}.)
#' @param header A logical value indicating whether the file starts with a header.
#' (This will not be the case for files that are created by data loggers that
#' chop the raw data up into a series of sub-files, e.g. once per hour.)
read.adv <- function(file, from=1, to, by=1, tz=getOption("oceTz"),
                     type=c("nortek", "sontek", "sontek.adr", "sontek.text"),
                     header=TRUE,
                     longitude=NA, latitude=NA,
                     start=NULL, deltat=NA,
                     debug=getOption("oceDebug"), monitor=FALSE, processingLog=NULL)
{
    type <- match.arg(type)
    ## FIXME: all these read.adv variants should have the same argument list
    if (type == "nortek")
        read.adv.nortek(file=file, from=from, to=to, by=by, tz=tz,
                        header=header,
                        longitude=longitude, latitude=latitude,
                        debug=debug, monitor=monitor, processingLog=processingLog)
    else if (type == "sontek") # guess
        read.adv.sontek.serial(file=file, from=from, to=to, by=by, tz=tz,
                               longitude=longitude, latitude=latitude,
                               start=start, deltat=deltat,
                               debug=debug, monitor=monitor, processingLog=processingLog)
    else if (type == "sontek.adr")
        read.adv.sontek.adr(file=file, from=from, to=to, by=by, tz=tz,
                            longitude=longitude, latitude=latitude,
                            debug=debug, processingLog=processingLog)
    else if (type == "sontek.text")
        read.adv.sontek.text(file=file, from=from, to=to, by=by, tz=tz,
                             longitude=longitude, latitude=latitude,
                             debug=debug, processingLog=processingLog)
    else
        stop("read.adv() cannot understand type = \"", type, "\"")
}



#' Plot ADV data
#'
#' Plot ADV data, i.e. stored in an \code{\link{adv-class}} object.
#'
#' @details
#' Creates a multi-panel summary plot of data measured by an ADV.
#' The panels are controlled by the \code{which} argument.  (Note the
#' gaps in the sequence, e.g. 4 and 8 are not used.)
#'
#' \itemize{
#'     \item \code{which=1} to \code{3} (or \code{"u1"} to \code{"u3"})
#'
#'     yield timeseries of the first, second, and third components of
#'     velocity (in beam, xyz or enu coordinates).
#'
#'     \item \code{which=4} is not permitted (since ADV are 3-beam devices)
#'
#'     \item \code{which=5} to \code{7} (or \code{"a1"} to \code{"a3"})
#'     yield timeseries of the amplitudes of beams 1 to 3.  (Note that
#'     the data are called \code{data$a[,1]}, \code{data$a[,2]} and
#'     \code{data$a[,3]}, for these three timeseries.)
#'
#'     \item \code{which=8} is not permitted (since ADV are 3-beam devices)
#'
#'     \item \code{which=9} to \code{11} (or \code{"q1"} to \code{"q3"})
#'     yield timeseries of correlation for beams 1 to 3.  (Note that the
#'     data are called \code{data$c[,1]}, \code{data$c[,2]} and
#'     \code{data$c[,3]}, for these three timeseries.)
#'
#'     \item \code{which=12} is not permitted (since ADVs are 3-beam devices)
#'
#'     \item \code{which=13} is not permitted (since ADVs do not measure salinity)
#'
#'     \item \code{which=14} or \code{which="temperature"} yields a timeseries of temperature.
#'
#'     \item \code{which=15} or \code{which="pressure"} yields a timeseries of pressure.
#'
#'     \item \code{which=16} or \code{which="heading"} yields a timeseries of heading.
#'
#'     \item \code{which=17} or \code{which="pitch"}yields a timeseries of pitch.
#'
#'     \item \code{which=18} or \code{which="roll"}yields a timeseries of roll.
#'
#'     \item \code{which=19} to \code{21} yields plots of correlation versus
#'     amplitude, for beams 1 through 3, using \code{\link{smoothScatter}}.
#'
#'     \item \code{which=22} is not permitted (since ADVs are 3-beam devices)
#'
#'     \item \code{which=23} or \code{"progressive vector"} yields a
#'     progressive-vector diagram in the horizontal plane, plotted with
#'     \code{asp=1}, and taking beam1 and beam2 as the eastward and
#'     northward components of velocity, respectively.
#'
#'     \item \code{which=28} or \code{"uv"} yields velocity plot in the
#'     horizontal plane, i.e. u[2] versus u[1].  If the number of data
#'     points is small, a scattergraph is used, but if it is large,
#'     \code{\link{smoothScatter}} is used.
#'
#'     \item \code{which=29} or \code{"uv+ellipse"} as the \code{"uv"}
#'     case, but with an added indication of the tidal ellipse,
#'     calculated from the eigen vectors of the covariance matrix.
#'
#'     \item \code{which=30} or \code{"uv+ellipse+arrow"} as the
#'     \code{"uv+ellipse"} case, but with an added arrow indicating the
#'     mean current.
#'
#'     \item \code{which=50} or \code{"analog1"} plots a time series of the
#'     analog1 signal, if there is one.
#'
#'     \item \code{which=51} or \code{"analog2"} plots a time series of the
#'     analog2 signal, if there is one.
#'
#'     \item \code{which=100} or \code{"voltage"} plots the voltage as a
#'     timeseries, if voltage exists in the dataset.
#' }
#' In addition to the above, there are some groupings defined:
#' \itemize{
#'     \item \code{which="velocity"} equivalent to \code{which=1:3} (three velocity components)
#'     \item \code{which="amplitude"} equivalent to \code{which=5:7} (three amplitude components)
#'     \item \code{which="backscatter"} equivalent to \code{which=9:11} (three backscatter components)
#'     \item \code{which="hydrography"} equivalent to \code{which=14:15} (temperature and pressure)
#'     \item \code{which="angles"} equivalent to \code{which=16:18} (heading, pitch and roll)
#' }
#'
#'
#' @param x An \code{adv} object, i.e. one inheriting from \code{\link{adv-class}}.
#'
#' @param which List of desired plot types.  These are graphed in panels running
#' down from the top of the page.  See \dQuote{Details} for the meanings of
#' various values of \code{which}.
#'
#' @param col Optional indication of color(s) to use.  If not provided, the
#' default for images is \code{oce.colorsPalette(128,1)}, and for lines and points
#' is black.
#'
#' @param titles Optional vector of character strings to be used as labels for the
#' plot panels.  For images, these strings will be placed in the right hand side
#' of the top margin.  For timeseries, these strings are ignored.  If this is
#' provided, its length must equal that of \code{which}.
#'
#' @param lwd If the plot is of a time-series or scattergraph format with lines,
#' this is used in the usual way; otherwise, e.g. for image formats, this is
#' ignored.
#'
#' @param type Type of plot, as for \code{\link{plot}}.
#'
#' @param drawTimeRange Boolean that applies to panels with time as the horizontal
#' axis, indicating whether to draw the time range in the top-left margin of the
#' plot.
#'
#' @param drawZeroLine Logical value indicating whether to draw zero lines on
#' velocities.
#'
#' @param useSmoothScatter Logical value indicating whether to use
#' \code{\link{smoothScatter}} in various plots, such as \code{which="uv"}.  If
#' not provided a default is used, with \code{\link{smoothScatter}} being used if
#' there are more than 2000 points to plot.
#'
#' @param mgp 3-element numerical
#' vector to use for \code{par(mgp)}, and also for \code{par(mar)}, computed from
#' this.  The default is tighter than the R default, in order to use more space
#' for the data and less for the axes.
#'
#' @param mar Value to be used with \code{\link{par}("mar")}.
#'
#' @param tformat Optional argument passed to \code{\link{oce.plot.ts}}, for plot
#' types that call that function.  (See \code{\link{strptime}} for the format
#' used.)
#'
#' @param marginsAsImage Logical value indicating whether to put a wide margin to
#' the right of time-series plots, matching the space used up by a palette in an
#' \code{\link{imagep}} plot.
#'
#' @param cex Size of labels on axes; see \code{\link[graphics]{par}}("cex").
#'
#' @param cex.axis See \code{\link[graphics]{par}}("cex.axis").
#'
#' @param cex.main See \code{\link[graphics]{par}}("cex.main").
#'
#' @param xlim Optional 2-element list for \code{xlim}, or 2-column matrix, in
#' which case the rows are used, in order, for the panels of the graph.
#'
#' @param ylim Optional 2-element list for \code{ylim}, or 2-column matrix, in
#' which case the rows are used, in order, for the panels of the graph.
#'
#' @param brushCorrelation Optional number between 0 and 100, indicating a
#' per-beam correlation threshold below which data are to be considered suspect.
#' If the plot type is \code{p}, the suspect points (velocity, backscatter
#' amplitude, or correlation) will be colored red; otherwise, this argument is
#' ignored.
#'
#' @param colBrush Color to use for brushed (bad) data, if
#' \code{brushCorrelation} is active.
#'
#' @param main Main title for plot, used just on the top panel, if there are
#' several panels.
#'
#' @param debug A flag that turns on debugging.  Set to 1 to get a moderate amount
#' of debugging information, or to 2 to get more.
#'
#' @param ... Optional arguments passed to plotting functions.
#'
#' @seealso The documentation for \code{\link{adv-class}} explains the structure
#' of ADV objects, and also outlines the other functions dealing with them.
#'
#' @examples
#' library(oce)
#' data(adv)
#' plot(adv)
#'
#' @author Dan Kelley
#'
#' @family functions that plot \code{oce} data
#' @family things related to \code{adv} data
#' @aliases plot.adv
setMethod(f="plot",
          signature=signature("adv"),
          definition=function(x, which=c(1:3, 14, 15),
                              col,
                              titles,
                              type="l",
                              lwd=par('lwd'),
                              drawTimeRange=getOption("oceDrawTimeRange"),
                              drawZeroLine=FALSE,
                              useSmoothScatter,
                              mgp=getOption("oceMgp"),
                              mar=c(mgp[1]+1.5, mgp[1]+1.5, 1.5, 1.5),
                              tformat,
                              marginsAsImage=FALSE,
                              cex=par("cex"), cex.axis=par("cex.axis"), cex.main=par("cex.main"),
                              xlim, ylim,
                              brushCorrelation, colBrush="red",
                              main="",
                              debug=getOption("oceDebug"),
                              ...)
          {
              debug <- min(4, max(0, round(debug)))
              if ("adorn" %in% names(list(...)))
                  warning("In plot,adv-method() : the 'adorn' argument was removed in November 2017", call.=FALSE)
              oceDebug(debug, "plot.adv(x, which=c(", paste(which, collapse=","), "), type=\"", type, "\", ...) {\n", sep="", unindent=1)
              have.brushCorrelation <- !missing(brushCorrelation)
              oceDebug(debug, "brushCorrelation", if (have.brushCorrelation) brushCorrelation else "not given", "\n")
              oceDebug(debug, "cex=", cex, " cex.axis=", cex.axis, " cex.main=", cex.main, "\n")
              oceDebug(debug, "mar=c(", paste(mar, collapse=","), ")\n")
              opar <- par(no.readonly = TRUE)
              dots <- names(list(...))
              ##if (!all(which %in% c(1:3,5:7,9:11,14:21,23)))
              ##   stop("\"which\" must be in the range c(1:3,5:7,9:11,14:21,23) but it is ", which)
              nw <- length(which)
              if (nw == 1 && is.character(which)) {
                  pm <- pmatch(which, c("velocity", "amplitude", "quality", "hydrography", "angles"))
                  if (!is.na(pm)) {
                      nbeams <- 3
                      if (pm == 1)
                          which <- 0 + seq(1, nbeams)
                      else if (pm == 2)
                          which <- 4 + seq(1, nbeams)
                      else if (pm == 3)
                          which <- 8 + seq(1, nbeams)
                      else if (pm == 4)
                          which <- 14:15
                      else if (pm == 5)
                          which <- 16:18
                      nw <- length(which)
                  }
              }
              colPerPoint <- FALSE
              if (missing(col)) {
                  col <- rep("black", length.out=nw)
              } else {
                  colPerPoint <- length(col) == length(x@data$time) # FIXME slow timescale here?
                  if (!colPerPoint)
                      col <- rep(col, length.out=nw)
              }
              if (!missing(titles) && length(titles) != nw)
                  stop("length of 'titles' must equal length of 'which'")
              if (nw > 1)
                  on.exit(par(opar))
              par(mgp=mgp, mar=mar)
              dots <- list(...)

              ## user may specify a matrix for xlim and ylim
              gave.ylim <- !missing(ylim)
              if (gave.ylim) {
                  if (is.matrix(ylim)) {
                      if (dim(ylim)[2] != nw) {
                          ylim2 <- matrix(ylim, ncol=2, nrow=nw) # FIXME: is this what I want?
                      }
                  } else {
                      ylim2 <- matrix(ylim, ncol=2, nrow=nw) # FIXME: is this what I want?
                  }
                  class(ylim2) <- class(ylim)
                  ylim <- ylim2
              }
              gave.xlim <- !missing(xlim)
              if (gave.xlim) {
                  if (is.matrix(xlim)) {
                      if (dim(xlim)[2] != nw) {
                          xlim2 <- matrix(xlim, ncol=2, nrow=nw) # FIXME: is this what I want?
                      }
                  } else {
                      if (length(xlim) != 2)
                          stop("xlim must be a vector of length 2, or a 2-column matrix")
                      xlim2 <- matrix(xlim[1:2], ncol=2, nrow=nw, byrow=TRUE)
                  }
                  xlim <- xlim2
              }
              oceDebug(debug, "before layout, cex=", par('cex'), "\n")
              if (nw > 1) {
                  if (marginsAsImage) {
                      w <- 1.5
                      lay <- layout(matrix(1:(2*nw), nrow=nw, byrow=TRUE), widths=rep(c(1, lcm(w)), nw))
                  } else {
                      lay <- layout(cbind(1:nw))
                  }
              }
              ## Translate word-style (FIXME: ugly coding)
              oceDebug(debug, "before nickname-substitution, which=c(", paste(which, collapse=","), ")\n")
              which2 <- vector("numeric", nw)
              if (nw == 1 && is.character(which)) {
                  wtmp <- char.expand(which,
                                      c("velocity", "amplitude", "backscatter", "hydrography", "angles"), nomatch=NULL)
                  if (!is.na(wtmp)) {
                      if (     wtmp == "velocity"   ) which <- 1:3
                      else if (wtmp == "amplitude"  ) which <- 5:7
                      else if (wtmp == "backscatter") which <- 9:11
                      else if (wtmp == "hydrography") which <- 14:15
                      else if (wtmp == "angles"     ) which <- 16:18
                      nw <- length(which)
                  }
              }
              for (w in 1:nw) {
                  ww <- which[w]
                  if (is.numeric(ww)) {
                      which2[w] <- ww
                  } else {
                      if (     ww == "u1") which2[w] <- 1
                      else if (ww == "u2") which2[w] <- 2
                      else if (ww == "u3") which2[w] <- 3
                      ## 4 not allowed since ADV is 3-beam
                      else if (ww == "a1") which2[w] <- 5
                      else if (ww == "a2") which2[w] <- 6
                      else if (ww == "a3") which2[w] <- 7
                      ## 4 not allowed since ADV is 3-beam
                      else if (ww == "q1") which2[w] <- 9
                      else if (ww == "q2") which2[w] <- 10
                      else if (ww == "q3") which2[w] <- 11
                      ## 4 not allowed since ADV is 3-beam
                      else if (ww == "salinity") which2[w] <- 13
                      else if (ww == "temperature") which2[w] <- 14
                      else if (ww == "pressure") which2[w] <- 15
                      else if (ww == "heading") which2[w] <- 16
                      else if (ww == "pitch") which2[w] <- 17
                      else if (ww == "roll") which2[w] <- 18
                      ## 19 beam-1 correlation-amplitude diagnostic plot
                      ## 20 beam-2 correlation-amplitude diagnostic plot
                      ## 21 beam-3 correlation-amplitude diagnostic plot
                      ## 22 not allowed, since ADVs have only 3 beams
                      else if (ww == "progressive vector") which2[w] <- 23
                      else if (ww == "uv") which2[w] <- 28
                      else if (ww == "uv+ellipse") which2[w] <- 29
                      else if (ww == "uv+ellipse+arrow") which2[w] <- 30
                      else if (ww == "analog1") which2[w] <- 50
                      else if (ww == "analog2") which2[w] <- 51
                      else if (ww == "voltage") which2[w] <- 100
                      else stop("unknown 'which':", ww)
                  }
              }
              which <- which2
              oceDebug(debug, "after nickname-substitution, which=c(", paste(which, collapse=","), ")\n")
              oceDebug(debug, "after layout, cex=", par('cex'), "\n")
              ## FIXME below here, was using tsSlow
              tlim <- range(x@data$time, na.rm=TRUE)
              for (w in 1:nw) {
                  if (w > 1)
                      main <- ""
                  oceDebug(debug, "plotting which[", w, "]=", which[w], "\n")
                  par(mgp=mgp, mar=mar)
                  if (which[w] %in% 1:3) {
                      ## u1, u2, u3
                      y <- as.numeric(x@data$v[, which[w]])
                      if (have.brushCorrelation && type == "p") {
                          good <- as.numeric(x@data$q[, which[w]]) >= brushCorrelation
                          oce.plot.ts(x@data$time[good], y[good], ylab=beamName(x, which[w]),
                                      drawTimeRange=drawTimeRange,
                                      xlim=if (gave.xlim) xlim[w, ] else tlim,
                                      ylim=if (gave.ylim) ylim[w, ] else range(y, na.rm=TRUE),
                                      type=type,
                                      cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                      mgp=mgp,
                                      mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                      lwd=lwd[w], col=col[w],
                                      main=main,
                                      tformat=tformat,
                                      debug=debug-1,
                                      ...)
                          points(x@data$time[!good], x@data$v[!good, which[w]], col=colBrush, ...)
                      } else {
                          oce.plot.ts(x@data$time, y, ylab=beamName(x, which[w]),
                                      drawTimeRange=drawTimeRange,
                                      xlim=if (gave.xlim) xlim[w, ] else tlim,
                                      ylim=if (gave.ylim) ylim[w, ] else range(y, na.rm=TRUE),
                                      type=type,
                                      cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                      mgp=mgp,
                                      mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                      lwd=lwd[w], col=col[w],
                                      main=main,
                                      tformat=tformat,
                                      debug=debug-1)
                      }
                      if (drawZeroLine)
                          abline(h=0)
                      rm(y)                       # space may be tight
                  } else if (which[w] %in% 5:7) {
                      ## a1, a2, a3
                      ## FIXME/DRY: alter a1,a2,a3 if alter q1,q2,q3, since both almost the same
                      oceDebug(debug, "plotting a1, a2, or a3 since which[w] == ", which[w], "\n")
                      y <- as.numeric(x@data$a[, which[w]-4])
                      oceDebug(debug, "range(y):", paste(range(y, na.rm=TRUE), sep="-"), "\n")
                      if (have.brushCorrelation && type == "p") {
                          good <- as.numeric(x@data$q[, which[w]-4]) >= brushCorrelation
                          oce.plot.ts(x@data$time[good], y[good],
                                      ylab=c(expression(a[1]), expression(a[2]), expression(a[3]), expression(a[4]))[which[w]-4],
                                      drawTimeRange=drawTimeRange,
                                      xlim=if (gave.xlim) xlim[w, ] else tlim,
                                      ylim=if (gave.ylim) ylim[w, ] else range(y, na.rm=TRUE),
                                      type=type,
                                      cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                      mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                      lwd=lwd[w], col=col[w],
                                      main=main,
                                      tformat=tformat,
                                      debug=debug-1)
                          points(x@data$time[!good], y[!good], col=colBrush)
                      } else {
                          oce.plot.ts(x@data$time, y,
                                      ylab=c(expression(a[1]), expression(a[2]), expression(a[3]), expression(a[4]))[which[w]-4],
                                      drawTimeRange=drawTimeRange,
                                      xlim=if (gave.xlim) xlim[w, ] else tlim,
                                      ylim=if (gave.ylim) ylim[w, ] else range(y, na.rm=TRUE),
                                      type=type,
                                      cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                      mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                      lwd=lwd[w], col=col[w],
                                      main=main,
                                      tformat=tformat,
                                      debug=debug-1)
                      }
                      rm(y)                       # space may be tight
                  } else if (which[w] %in% 9:11) {
                      ## q1, q2, q3 (named c1, c2, and c3 in the object)
                      y <- as.numeric(x@data$q[, which[w]-8])
                      if (have.brushCorrelation && type == "p") {
                          good <- as.numeric(x@data$q[, which[w]-8]) >= brushCorrelation
                          oce.plot.ts(x@data$time[good], y[good],
                                      ylab=c(expression(q[1]), expression(q[2]), expression(q[3]), expression(q[4]))[which[w]-8],
                                      drawTimeRange=drawTimeRange,
                                      xlim=if (gave.xlim) xlim[w, ] else tlim,
                                      ylim=if (gave.ylim) ylim[w, ] else range(y, na.rm=TRUE),
                                      type=type,
                                      cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                      mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                      lwd=lwd[w], col=col[w],
                                      main=main,
                                      tformat=tformat,
                                      debug=debug-1)
                          points(x@data$time[!good], y[!good], col=colBrush)
                      } else {
                          oce.plot.ts(x@data$time, y,
                                      ylab=c(expression(q[1]), expression(q[2]), expression(q[3]), expression(q[4]))[which[w]-8],
                                      drawTimeRange=drawTimeRange,
                                      xlim=if (gave.xlim) xlim[w, ] else tlim,
                                      ylim=if (gave.ylim) ylim[w, ] else range(y, na.rm=TRUE),
                                      type=type,
                                      cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                      mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                      lwd=lwd[w], col=col[w],
                                      tformat=tformat,
                                      debug=debug-1)
                      }
                      rm(y)                       # space may be tight
                  } else if (which[w] == 13 || which[w] == "salinity") {
                      if ("salinity" %in% names(x@metadata)) {
                          if ("timeSlow" %in% names(x@data)) {
                              salinity <- rep(x@metadata$salinity, length(x@data$temperatureSlow))
                              oce.plot.ts(x@data$timeSlow, salinity, ylab=resizableLabel("S", "y"),
                                          drawTimeRange=drawTimeRange,
                                          xlim=if (gave.xlim) xlim[w, ] else tlim,
                                          ylim=if (gave.ylim) ylim[w, ] else x@metadata$salinity+c(0.5, -0.5),
                                          type=type,
                                          cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                          mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                          lwd=lwd[w], col=if (colPerPoint) col else col[w],
                                          main=main,
                                          tformat=tformat,
                                          debug=debug-1)
                          } else {
                              salinity <- rep(x@metadata$salinity, length(x@data$temperature))
                              oce.plot.ts(x@data$time, salinity, ylab=resizableLabel("S", "y"),
                                          drawTimeRange=drawTimeRange,
                                          xlim=if (gave.xlim) xlim[w, ] else tlim,
                                          ylim=if (gave.ylim) ylim[w, ] else x@metadata$salinity+c(0.5, -0.5),
                                          type=type,
                                          cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                          mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                          lwd=lwd[w], col=if (colPerPoint) col else col[w],
                                          main=main,
                                          tformat=tformat,
                                          debug=debug-1)
                          }
                      } else {
                          warning("no salinity in this ADV object")
                      }
                  } else if (which[w] == 14 || which[w] == "temperature") {
                      if ("timeSlow" %in% names(x@data) && "temperatureSlow" %in% names(x@data)) {
                          oce.plot.ts(x@data$timeSlow, x@data$temperatureSlow, ylab=resizableLabel("T", "y"),
                                      drawTimeRange=drawTimeRange,
                                      xlim=if (gave.xlim) xlim[w, ] else tlim,
                                      ylim=if (gave.ylim) ylim[w, ] else range(x@data$temperature, na.rm=TRUE),
                                      type=type,
                                      cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                      mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                      lwd=lwd[w], col=if (colPerPoint) col else col[w],
                                      main=main,
                                      tformat=tformat,
                                      debug=debug-1)
                      } else {
                          oce.plot.ts(x@data$time, x@data$temperature, ylab=resizableLabel("T", "y"),
                                      drawTimeRange=drawTimeRange,
                                      xlim=if (gave.xlim) xlim[w, ] else tlim,
                                      ylim=if (gave.ylim) ylim[w, ] else range(x@data$temperature, na.rm=TRUE),
                                      type=type,
                                      cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                      mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                      lwd=lwd[w], col=if (colPerPoint) col else col[w],
                                      main=main,
                                      tformat=tformat,
                                      debug=debug-1)
                      }
                  } else if (which[w] == 15 || which[w] == "pressure") {
                      oce.plot.ts(x@data$time, x@data$pressure, ylab=resizableLabel("p", "y"),
                                  drawTimeRange=drawTimeRange,
                                  xlim=if (gave.xlim) xlim[w, ] else tlim,
                                  ylim=if (gave.ylim) ylim[w, ] else range(x@data$pressure, na.rm=TRUE),
                                  type=type,
                                  cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                  mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                  lwd=lwd[w], col=if (colPerPoint) col else col[w],
                                  main=main,
                                  tformat=tformat,
                                  debug=debug-1)
                  } else if (which[w] == 16 || which[w] == "heading") {
                      if ("timeSlow" %in% names(x@data) && "headingSlow" %in% names(x@data)) {
                          oce.plot.ts(x@data$timeSlow, x@data$headingSlow, ylab="heading",
                                      drawTimeRange=drawTimeRange,
                                      xlim=if (gave.xlim) xlim[w, ] else tlim,
                                      ylim=if (gave.ylim) ylim[w, ] else range(x@data$heading, na.rm=TRUE),
                                      type=type,
                                      cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                      mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                      lwd=lwd[w], col=if (colPerPoint) col else col[w],
                                      main=main,
                                      tformat=tformat,
                                      debug=debug-1)
                      } else {
                          oce.plot.ts(x@data$time, x@data$heading, ylab="heading",
                                      drawTimeRange=drawTimeRange,
                                      xlim=if (gave.xlim) xlim[w, ] else tlim,
                                      ylim=if (gave.ylim) ylim[w, ] else range(x@data$heading, na.rm=TRUE),
                                      type=type,
                                      cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                      mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                      lwd=lwd[w], col=if (colPerPoint) col else col[w],
                                      main=main,
                                      tformat=tformat,
                                      debug=debug-1)
                      }
                  } else if (which[w] == 17 || which[w] == "pitch") {
                      ## pitch
                      if ("timeSlow" %in% names(x@data) && "pitchSlow" %in% names(x@data)) {
                          oce.plot.ts(x@data$timeSlow, x@data$pitchSlow, ylab="pitch",
                                      drawTimeRange=drawTimeRange,
                                      xlim=if (gave.xlim) xlim[w, ] else tlim,
                                      ylim=if (gave.ylim) ylim[w, ] else range(x@data$pitch, na.rm=TRUE),
                                      type=type,
                                      cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                      mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                      lwd=lwd[w], col=if (colPerPoint) col else col[w],
                                      main=main,
                                      tformat=tformat,
                                      debug=debug-1)
                      } else {
                          oce.plot.ts(x@data$time, x@data$pitch, ylab="pitch",
                                      drawTimeRange=drawTimeRange,
                                      xlim=if (gave.xlim) xlim[w, ] else tlim,
                                      ylim=if (gave.ylim) ylim[w, ] else range(x@data$pitch, na.rm=TRUE),
                                      type=type,
                                      cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                      mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                      lwd=lwd[w], col=if (colPerPoint) col else col[w],
                                      main=main,
                                      tformat=tformat,
                                      debug=debug-1)
                      }
                  } else if (which[w] == 18 || which[w] == "roll") {
                      if ("timeSlow" %in% names(x@data) && "rollSlow" %in% names(x@data)) {
                          oce.plot.ts(x@data$timeSlow, x@data$rollSlow, ylab="roll",
                                      drawTimeRange=drawTimeRange,
                                      xlim=if (gave.xlim) xlim[w, ] else tlim,
                                      ylim=if (gave.ylim) ylim[w, ] else range(x@data$roll, na.rm=TRUE),
                                      type=type,
                                      cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                      mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                      lwd=lwd[w], col=col[w],
                                      main=main,
                                      tformat=tformat,
                                      debug=debug-1)
                      } else {
                          oce.plot.ts(x@data$time, x@data$roll, ylab="roll",
                                      drawTimeRange=drawTimeRange,
                                      xlim=if (gave.xlim) xlim[w, ] else tlim,
                                      ylim=if (gave.ylim) ylim[w, ] else range(x@data$roll, na.rm=TRUE),
                                      type=type,
                                      cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                      mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                      lwd=lwd[w], col=col[w],
                                      main=main,
                                      tformat=tformat,
                                      debug=debug-1)
                      }
                      ## FIXME: should plot.adv() be passing mar, cex, etc to smoothScatter?
                  } else if (which[w] == 19) {
                      ## beam 1 correlation-amplitude diagnostic plot
                      a <- as.numeric(x@data$a[, 1])
                      q <- as.numeric(x@data$q[, 1])
                      n <- length(a)
                      if (n < 2000 || (!missing(useSmoothScatter) && !useSmoothScatter)) {
                          plot(a, c,
                               xlab=gettext("Amplitude", domain="R-oce"),
                               ylab=gettext("Correlation", domain="R-oce"),
                               xlim=if (gave.xlim) xlim[w, ] else range(a),
                               ylim=if (gave.ylim) ylim[w, ] else range(c),
                               main=main,
                               debug=debug-1)
                      } else {
                          smoothScatter(a, c, nbin=64,
                                        xlab=gettext("Amplitude", domain="R-oce"),
                                        ylab=gettext("Correlation", domain="R-oce"),
                                        xlim=if (gave.xlim) xlim[w, ] else range(a),
                                        ylim=if (gave.ylim) ylim[w, ] else range(c),
                                        main=main,
                                        debug=debug-1)
                      }
                      mtext("beam 1")
                  } else if (which[w] == 20) {
                      ## beam 2 correlation-amplitude diagnostic plot
                      a <- as.numeric(x@data$a[, 2])
                      q <- as.numeric(x@data$q[, 2])
                      n <- length(a)
                      if (n < 2000 || (!missing(useSmoothScatter) && !useSmoothScatter)) {
                          plot(a, c,
                               xlab=gettext("Amplitude", domain="R-oce"),
                               ylab=gettext("Correlation", domain="R-oce"),
                               xlim=if (gave.xlim) xlim[w, ] else range(a),
                               ylim=if (gave.ylim) ylim[w, ] else range(c),
                               main=main,
                               debug=debug-1)
                      } else {
                          smoothScatter(a, c, nbin=64,
                                        xlab=gettext("Amplitude", domain="R-oce"),
                                        ylab=gettext("Correlation", domain="R-oce"),
                                        xlim=if (gave.xlim) xlim[w, ] else range(a),
                                        ylim=if (gave.ylim) ylim[w, ] else range(c),
                                        main=main,
                                        debug=debug-1)
                      }
                      mtext("beam 2")
                  } else if (which[w] == 21) {
                      ## beam 3 correlation-amplitude diagnostic plot
                      a <- as.numeric(x@data$a[, 3])
                      q <- as.numeric(x@data$q[, 3])
                      n <- length(a)
                      if (n < 2000 || (!missing(useSmoothScatter) && !useSmoothScatter)) {
                          plot(a, c,
                               xlab=gettext("Amplitude", domain="R-oce"),
                               ylab=gettext("Correlation", domain="R-oce"),
                               xlim=if (gave.xlim) xlim[w, ] else range(a),
                               ylim=if (gave.ylim) ylim[w, ] else range(c),
                               main=main)
                      } else {
                          smoothScatter(a, c, nbin=64,
                                        xlab=gettext("Amplitude", domain="R-oce"),
                                        ylab=gettext("Correlation", domain="R-oce"),
                                        xlim=if (gave.xlim) xlim[w, ] else range(a),
                                        ylim=if (gave.ylim) ylim[w, ] else range(c),
                                        main=main)
                      }
                      mtext("beam 3")
                  } else if (which[w] == 23 || which[w] == "progressive vector") {
                      ## progressive vector
                      par(mar=c(mgp[1]+1, mgp[1]+1, 1, 1))
                      mPerKm <- 1000
                      u <- x@data$v[, 1]
                      v <- x@data$v[, 2]
                      u[is.na(u)] <- 0        # zero out missing
                      v[is.na(v)] <- 0
                      xDist <- integrateTrapezoid(x@data$time, u, 'cA') / mPerKm
                      yDist<- integrateTrapezoid(x@data$time, v, 'cA') / mPerKm
                      plot(xDist, yDist, xlab="km", ylab="km", type=type,
                           cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                           asp=1, lwd=lwd[w], col=col[w], ...)
                      if (main[w] != "")
                          mtext(main[w], adj=1)
                  } else if (which[w] %in% 28:31) {
                      oceDebug(debug, "doing horizontal-velocity diagram\n")
                      par(mar=c(mgp[1]+1, mgp[1]+1, 1, 1))
                      n <- length(x@data$time)
                      if (n < 2000 || (!missing(useSmoothScatter) && !useSmoothScatter)) {
                          plot(x@data$v[, 1], x@data$v[, 2],
                               xlab=resizableLabel("u"),
                               ylab=resizableLabel("v"),
                               type=type,
                               cex=cex, cex.axis=cex.axis, cex.main=cex.main, asp=1,
                               xlim=if (gave.xlim)xlim, ylim=if (gave.ylim) ylim,
                               lwd=lwd[w], col=col[w], main=main, ...)
                      } else {
                          smoothScatter(x@data$v[, 1], x@data$v[, 2],
                                        xlab=resizableLabel("u"),
                                        ylab=resizableLabel("v"),
                                        cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                        asp=1, xlim=xlim, ylim=ylim, ...)
                      }
                      if (which[w] >= 29) {
                          ok <- !is.na(x@data$v[, 1]) & !is.na(x@data$v[, 2])
                          e <- eigen(cov(data.frame(u=x@data$v[ok, 1], v=x@data$v[ok, 2])))
                          major <- sqrt(e$values[1])
                          minor <- sqrt(e$values[2])
                          theta <- seq(0, 2*pi, length.out=360/5)
                          xx <- major * cos(theta)
                          yy <- minor * sin(theta)
                          theta0 <- atan2(e$vectors[2, 1], e$vectors[1, 1])
                          rotate <- matrix(c(cos(theta0), -sin(theta0), sin(theta0), cos(theta0)), nrow=2, byrow=TRUE)
                          xxyy <- rotate %*% rbind(xx, yy)
                          lines(xxyy[1, ], xxyy[2, ], lwd=5, col="yellow")
                          lines(xxyy[1, ], xxyy[2, ], lwd=2, col="darkblue")
                          if (which[w] >= 30) {
                              umean <- mean(x@data$v[, 1], na.rm=TRUE)
                              vmean <- mean(x@data$v[, 2], na.rm=TRUE)
                              arrows(0, 0, umean, vmean, lwd=5, length=1/10, col="yellow")
                              arrows(0, 0, umean, vmean, lwd=2, length=1/10, col=col)
                          }
                          if (main[w] != "")
                              mtext(main[w], adj=1)
                      }
                  } else if (which[w] == 50 || which[w] == "analog1") {
                      if ("analog1" %in% names(x@data)) {
                          oce.plot.ts(x@data$time, x@data$analog1, ylab="Analog 1",
                                      tformat=tformat,
                                      debug=debug-1)
                      } else {
                          warning("there is no analog1 signal in this ADV object")
                      }
                  } else if (which[w] == 51 || which[w] == "analog2") {
                      if ("analog2" %in% names(x@data)) {
                          oce.plot.ts(x@data$time, x@data$analog2, ylab="Analog 2",
                                      tformat=tformat,
                                      debug=debug-1)
                      } else {
                          warning("there is no analog2 signal in this ADV object")
                      }
                  } else if (which[w] == 100 || which[w] == "voltage") {
                      if ("voltageSlow" %in% names(x@data))
                          oce.plot.ts(x@data$timeSlow, x@data$voltageSlow, ylab="Voltage",
                                      tformat=tformat,
                                      debug=debug-1)
                      else
                          warning("no voltage signal to plot")
                  } else {
                      stop("unknown value of \"which\":", which[w])
                  }
                  drawTimeRange <- FALSE
                  if (marginsAsImage)  {
                      ## blank plot, to get axis length same as for images
                      omar <- par("mar")
                      par(mar=c(mar[1], 1/4, mgp[2]+1/2, mgp[2]+1))
                      plot(1:2, 1:2, type='n', axes=FALSE, xlab="", ylab="")
                      par(mar=omar)
                  }
              }
              oceDebug(debug, "} # plot.adv()\n", unindent=1)
              invisible()
          })



#' Convert an ADV Object to ENU Coordinates
#'
#' @param x An \code{adv} object, i.e. one inheriting from \code{\link{adv-class}}.
#' @param declination magnetic declination to be added to the heading, to get
#' ENU with N as "true" north.
#' @template debugTemplate
#' @author Dan Kelley
#' @seealso See \code{\link{read.adv}} for notes on functions relating to
#' \code{"adv"} objects.  Also, see \code{\link{beamToXyzAdv}} and
#' \code{\link{xyzToEnuAdv}}.
#' @references
#' \url{https://www.nortekgroup.com/faq/how-is-a-coordinate-transformation-done}
#' @family things related to \code{adv} data
toEnuAdv <- function(x, declination=0, debug=getOption("oceDebug"))
{
    oceDebug(debug, "adv.2enu() {\n", unindent=1)
    coord <- x@metadata$oceCoordinate
    if (coord == "beam") {
        x <- xyzToEnuAdv(beamToXyzAdv(x, debug=debug-1), declination=declination, debug=debug-1)
    } else if (coord == "xyz") {
        x <- xyzToEnuAdv(x, declination=declination, debug=debug-1)
    } else if (coord != "enu") {
        warning("toEnuAdv cannot convert from coordinate system ", coord, " to ENU, so returning argument as-is")
    }
    oceDebug(debug, "} # adv.2enu()\n", unindent=1)
    x
}


#' Convert ADV from Beam to XYZ Coordinates
#'
#' Convert ADV velocity components from a beam-based coordinate system to a
#' xyz-based coordinate system.
#'
#' The coordinate transformation is done using the transformation matrix
#' contained in \code{transformation.matrix} in the
#' \code{metadata} slot, which is normally
#' inferred from the header in the binary file.  If there is no such matrix
#' (e.g. if the data were streamed through a data logger that did not capture
#' the header), \code{beamToXyzAdv} the user will need to store one in
#' \code{x}, e.g. by doing something like the following:
#' \preformatted{
#' x[["transformation.matrix"]] <- rbind(c(11100, -5771, -5321),
#'                                       c( #' 291, 9716, -10002),
#'                                       c( 1409, 1409, 1409)) / 4096
#' }
#'
#' @param x an object of class \code{"adv"}.
#' @param debug a flag that, if non-zero, turns on debugging.  Higher values
#' yield more extensive debugging.
#' @author Dan Kelley
#' @seealso See \code{\link{read.adv}} for notes on functions relating to
#' \code{"adv"} objects.
#' @references
#' \url{https://www.nortekgroup.com/faq/how-is-a-coordinate-transformation-done}
#' @family things related to \code{adp} data
beamToXyzAdv <- function(x, debug=getOption("oceDebug"))
{
    oceDebug(debug, "beamToXyzAdv() {\n", unindent=1)
    if (!inherits(x, "adv"))
        stop("method is only for objects of class '", "adv", "'")
    if (x@metadata$oceCoordinate != "beam")
        stop("input must be in beam coordinates, but it is in ", x@metadata$oceCoordinate, " coordinates")
    if (is.null(x@metadata$transformationMatrix)) {
        cat("How to add a transformation matrix to a velocimeter record named 'x':
            x@metadata$transformationMatrix <- rbind(c(11100, -5771,  -5321),
                                                     c(  291,  9716, -10002),
                                                     c( 1409,  1409,   1409)) / 4096")
        stop("cannot convert coordinates because metadata$transformationMatrix is NULL (see above).")
    }
    tm <- x@metadata$transformationMatrix
    oceDebug(debug, "Transformation matrix:\n")
    oceDebug(debug, sprintf("%.10f %.10f %.10f\n", tm[1, 1], tm[1, 2], tm[1, 3]))
    oceDebug(debug, sprintf("%.10f %.10f %.10f\n", tm[2, 1], tm[2, 2], tm[2, 3]))
    oceDebug(debug, sprintf("%.10f %.10f %.10f\n", tm[3, 1], tm[3, 2], tm[3, 3]))
    ## Not using the matrix method because it might consume more memory, and
    ## measures no faster xyz <- tm %*% rbind(x@data$v[,1], x@data$v[,2],
    ## x@data$v[,3])
    u <- tm[1, 1] * x@data$v[, 1] + tm[1, 2] * x@data$v[, 2] + tm[1, 3] * x@data$v[, 3]
    v <- tm[2, 1] * x@data$v[, 1] + tm[2, 2] * x@data$v[, 2] + tm[2, 3] * x@data$v[, 3]
    w <- tm[3, 1] * x@data$v[, 1] + tm[3, 2] * x@data$v[, 2] + tm[3, 3] * x@data$v[, 3]
    x@data$v[, 1] <- u
    x@data$v[, 2] <- v
    x@data$v[, 3] <- w
    x@metadata$oceCoordinate <- "xyz"
    x@processingLog <- processingLogAppend(x@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # beamToXyzAdv()\n", unindent=1)
    x
}


#' Convert an ADP from XYZ to ENU Coordinates
#'
#' Convert ADV velocity components from a xyz-based coordinate system to
#' an enu-based coordinate system.
#'
#' The coordinate transformation is done using the heading, pitch, and roll
#' information contained within \code{x}.  The algorithm is similar to that
#' used for Teledyne/RDI ADCP units, taking into account the different
#' definitions of heading, pitch, and roll as they are defined for the
#' velocimeters.
#'
#' Generally, the transformation must be done on a time-by-time basis, which is
#' a slow operation.  However, this function checks whether the vectors for
#' heading, pitch and roll, are all of unit length, and in that case, the
#' calculation is altered, resulting in shorter execution times.  Note that
#' the angles are held in (\code{data$timeSlow}, \code{data$headingSlow}, ...)
#' for Nortek instruments and (\code{data$time}, \code{data$heading}, ...) for
#' Sontek instruments.
#'
#' Since the documentation provided by instrument manufacturers can be vague on
#' the coordinate transformations, the method used here had to be developed
#' indirectly.  (This is in contrast to the RDI ADCP instruments, for which
#' there are clear instructions.)  documents that manufacturers provide.  If
#' results seem incorrect (e.g. if currents go east instead of west), users
#' should examine the code in detail for the case at hand.  The first step is
#' to set \code{debug} to 1, so that the processing will print a trail of
#' processing steps.  The next step should be to consult the table below, to
#' see if it matches the understanding (or empirical tests) of the user.  It
#' should not be difficult to tailor the code, if needed.
#'
#' The code handles every case individually, based on the table given below.
#' The table comes from Clark Richards, a former PhD student at Dalhousie
#' University [2], who developed it based on instrument documentation,
#' discussion on user groups, and analysis of measurements acquired with Nortek
#' and Sontek velocimeters in the SLEIWEX experiment.
#'
#' The column labelled ``Cabled'' indicates whether the sensor and the pressure
#' case are connected with a flexible cable, and the column labelled ``H.case''
#' indicates whether the pressure case is oriented horizontally.  These two
#' properties are not discoverable in the headers of the data files, and so
#' they must be supplied with the arguments \code{cabled} and
#' \code{horizontalCase}.  The source code refers to the information in this
#' table by case numbers.  (Cases 5 and 6 are not handled.)  Angles are
#' abbreviated as follows:: heading ``H,'' pitch ``P,'' and roll ``R''.
#' Entries X, Y and Z refer to instrument coordinates of the same names.
#' Entries S, F and M refer to so-called ship coordinates starboard, forward,
#' and mast; it is these that are used together with a rotation matrix to get
#' velocity components in the east, north, and upward directions.
#'
#' \tabular{rrrrrrrrrrrr}{ \strong{Case} \tab \strong{Mfr.} \tab
#' \strong{Instr.} \tab \strong{Cabled} \tab \strong{H. case} \tab
#' \strong{Orient.} \tab \strong{H} \tab \strong{P} \tab \strong{R} \tab
#' \strong{S} \tab \strong{F} \tab \strong{M}\cr 1 \tab Nortek \tab vector \tab
#' no \tab - \tab up \tab H-90 \tab R \tab -P \tab X \tab -Y \tab -Z\cr 2 \tab
#' Nortek \tab vector \tab no \tab - \tab down \tab H-90 \tab R \tab -P \tab X
#' \tab Y \tab Z\cr 3 \tab Nortek \tab vector \tab yes \tab yes \tab up \tab
#' H-90 \tab R \tab -P \tab X \tab Y \tab Z\cr 4 \tab Nortek \tab vector \tab
#' yes \tab yes \tab down \tab H-90 \tab R \tab P \tab X \tab -Y \tab -Z\cr 5
#' \tab Nortek \tab vector \tab yes \tab no \tab up \tab - \tab - \tab - \tab -
#' \tab - \tab -\cr 6 \tab Nortek \tab vector \tab yes \tab no \tab down \tab -
#' \tab - \tab - \tab - \tab - \tab -\cr 7 \tab Sontek \tab adv \tab - \tab -
#' \tab up \tab H-90 \tab R \tab -P \tab X \tab -Y \tab -Z\cr 8 \tab Sontek
#' \tab adv \tab - \tab - \tab down \tab H-90 \tab R \tab -P \tab X \tab Y \tab
#' Z\cr }
#'
#' @param x An \code{adv} object, i.e. one inheriting from \code{\link{adv-class}}.
#' @param declination magnetic declination to be added to the heading, to get
#' ENU with N as "true" north.
#' @param cabled boolean value indicating whether the sensor head is connected
#' to the pressure case with a cable.  If \code{cabled=FALSE}, then
#' \code{horizontalCase} is ignored.  See \dQuote{Details}.
#' @param horizontalCase optional boolean value indicating whether the sensor
#' case is oriented horizontally.  Ignored unless \code{cabled} is \code{TRUE}.
#' See \dQuote{Details}.
#' @param sensorOrientation optional string indicating the direction in which
#' the sensor points.  The value, which must be \code{"upward"} or
#' \code{"downward"}, over-rides the value of \code{orientation},
#' in the \code{metadata} slot,
#' which will have been set by \code{\link{read.adv}}, \emph{provided} that the
#' data file contained the full header.  See \dQuote{Details}.
#' @param debug a flag that, if non-zero, turns on debugging.  Higher values
#' yield more extensive debugging.
#' @author Dan Kelley, in collaboration with Clark Richards
#' @seealso See \code{\link{read.adv}} for notes on functions relating to
#' \code{adv} objects.
#' @references
#' 1. \url{https://www.nortekgroup.com/faq/how-is-a-coordinate-transformation-done}
#'
#' 2. Clark Richards, 2012, PhD Dalhousie University Department of
#' Oceanography.
#'
#' @family things related to \code{adv} data
xyzToEnuAdv <- function(x, declination=0,
                        cabled=FALSE, horizontalCase, sensorOrientation,
                        debug=getOption("oceDebug"))
{
    oceDebug(debug, "xyzToEnuAdv(x, declination[1]=", declination[1],
              ",cabled=", cabled,
              ",horizontalCase=", if (missing(horizontalCase)) "(not provided)" else horizontalCase,
              ",sensorOrientation=", if (missing(sensorOrientation)) "(not provided)" else sensorOrientation,
              ",debug) {\n", unindent=1)
    if (!inherits(x, "adv"))
        stop("method is only for objects of class '", "adv", "'")
    if (x@metadata$oceCoordinate != "xyz")
        stop("input must be in xyz coordinates, but it is in ", x@metadata$oceCoordinate, " coordinates")
    if ("ts" %in% names(x@data) || "ma" %in% names(x@data))
        stop("cannot handle ADV objects that were created with oce version < 0.3")
    haveSlow <- "timeSlow" %in% names(x@data)
    if (haveSlow) {
        oceDebug(debug, "interpolating slowly-varying heading, pitch, and roll to the quickly-varying velocity times\n")
        t0 <- as.numeric(x@data$timeSlow[1])    # arbitrary; done in case approx hates large x values
        tFast <- as.numeric(x@data$time) - t0
        tSlow <- as.numeric(x@data$timeSlow) - t0
        heading <- approx(tSlow, x@data$headingSlow, xout=tFast)$y
        pitch <- approx(tSlow, x@data$pitchSlow, xout=tFast)$y
        roll <- approx(tSlow, x@data$rollSlow, xout=tFast)$y
    } else {
        heading <- x@data$heading
        pitch <- x@data$pitch
        roll <- x@data$roll
    }
    haveSteadyAngles <- length(heading) == 1 && length(pitch) == 1 && length(roll) == 1
    oceDebug(debug, "haveSteadyAngles=", haveSteadyAngles, "\n")
    # FIXME: haveTsSlow necessary here
    oceDebug(debug, "adv data does not have data ts slow; time-series data are data\n")
    ## Adjust various things, so that the xyz-to-enu formulae (based on RDI) will work
    ##
    ## The various cases are defined by help(xyzToEnuAdv).
    if (missing(sensorOrientation))
        sensorOrientation  <- x@metadata$orientation
    if (1 == length(agrep("nortek", x@metadata$manufacturer))) {
        if (!cabled) {
            if (sensorOrientation == "upward") {
                oceDebug(debug, "Case 1: Nortek vector velocimeter with upward-pointing sensor attached directly to pressure case.\n")
                oceDebug(debug, "        Using heading=heading=90, pitch=roll, roll=-pitch, S=X, F=-Y, and M=-Z.\n")
                heading <- heading - 90
                tmp <- pitch
                pitch <- roll
                roll <- -tmp
                starboard <- x@data$v[, 1]
                forward <- -x@data$v[, 2]
                mast <- -x@data$v[, 3]
            } else if (sensorOrientation == "downward") {
                oceDebug(debug, "Case 2: Nortek vector velocimeter with downward-pointing sensor attached directly to pressure case.\n")
                oceDebug(debug, "        Using heading=heading=90, pitch=roll, roll=-pitch, S=X, F=Y, and M=Z.\n")
                heading <- heading - 90
                tmp <- pitch
                pitch <- roll
                roll <- -tmp
                starboard <- x@data$v[, 1]
                forward <- x@data$v[, 2]
                mast <- x@data$v[, 3]
            } else {
                stop("need sensor orientation to be 'upward' or 'downward', not '", sensorOrientation, "'")
            }
        } else {
            ## vector cabelled: cases 3 to 6
            if (missing(horizontalCase))
                stop("must give horizontalCase for cabled Nortek Vector (cases 3 to 6)")
            if (!is.logical(horizontalCase))
                stop("must give horizontalCase as TRUE or FALSE")
            if (horizontalCase) {
                if (sensorOrientation == "upward") {
                    oceDebug(debug, "Case 3: Nortek vector velocimeter with upward-pointing sensor, cabled to a horizontal pressure case.\n")
                    oceDebug(debug, "        Using heading=heading=90, pitch=roll, roll=-pitch, S=X, F=Y, and M=Z.\n")
                    heading <- heading - 90
                    tmp <- pitch
                    pitch <- roll
                    roll <- -tmp
                    starboard <- x@data$v[, 1]
                    forward <- x@data$v[, 2]
                    mast <- x@data$v[, 3]
                } else if (sensorOrientation == "downward") {
                    oceDebug(debug, "Case 4: Nortek vector velocimeter with downward-pointing sensor, cabled to a horizontal pressure case.\n")
                    oceDebug(debug, "        Using heading=heading=90, pitch=roll, roll=pitch, S=X, F=-Y, and M=-Z.\n")
                    heading <- heading - 90
                    tmp <- pitch
                    pitch <- roll
                    roll <- tmp
                    starboard <- x@data$v[, 1]
                    forward <- x@data$v[, 2]
                    mast <- x@data$v[, 3]
                } else {
                    stop("need sensor orientation to be 'upward' or 'downward', not '", sensorOrientation, "'")
                }
            } else {
                stop("cannot handle cases 5 and 6 (vector velocimeter cabled to a vertical case)")
            }
        }
    } else if (1 == length(agrep("sontek", x@metadata$manufacturer))) {
        if (cabled)
            stop("cannot handle the case of a cabled Sontek unit (does it even exist?)")
        if (sensorOrientation == "upward") {
            oceDebug(debug, "Case 7: Sontek ADV velocimeter with upward-pointing sensor.\n")
            oceDebug(debug, "        Using heading=heading=90, pitch=roll, roll=-pitch, S=X, F=-Y, and M=-Z.\n")
            heading <- heading - 90
            tmp <- pitch
            pitch <- roll
            roll <- -tmp
            starboard <- x@data$v[, 1]
            forward <- -x@data$v[, 2]
            mast <- -x@data$v[, 3]
        } else if (sensorOrientation == "downward") {
            oceDebug(debug, "Case 8: Sontek ADV velocimeter with downward-pointing sensor.\n")
            oceDebug(debug, "        Using heading=heading=90, pitch=roll, roll=-pitch, S=X, F=Y, and M=Z.\n")
            heading <- heading - 90
            tmp <- pitch
            pitch <- roll
            roll <- -tmp
            starboard <- x@data$v[, 1]
            forward <- x@data$v[, 2]
            mast <- x@data$v[, 3]
        } else {
            stop("need metadata$orientation='upward' or 'downward', not '", x@metadata$orientation, "'")
        }
    } else {
        stop("unknown type of instrument; x@metadata$manufacturer must contain either \"sontek\" or \"nortek\"")
    }
    np <- dim(x@data$v)[1]           # number of profiles
    if (length(heading) < np)
        heading <- rep(heading, length.out=np)
    if (length(pitch) < np)
        pitch <- rep(pitch, length.out=np)
    if (length(roll) < np)
        roll <- rep(roll, length.out=np)
    enu <- do_sfm_enu(heading + declination, pitch, roll, starboard, forward, mast)
    x@data$v[, 1] <- enu$east
    x@data$v[, 2] <- enu$north
    x@data$v[, 3] <- enu$up
    x@metadata$oceCoordinate <- "enu"
    x@processingLog <- processingLogAppend(x@processingLog,
                                           paste("xyzToEnu(x",
                                                 ", declination=", declination,
                                                 ", horizontalCase=", if (missing(horizontalCase)) "(missing)" else horizontalCase,
                                                 ", sensorOrientiation=", if (missing(sensorOrientation)) "(missing)" else sensorOrientation,
                                                 ", debug=", debug, ")", sep=""))
    oceDebug(debug, "} # xyzToEnuAdv()\n", unindent=1)
    x
}



#' Convert ENU to Other Coordinate
#'
#' Convert ADV velocity components from an enu-based coordinate system to
#' another system, perhaps to align axes with the coastline.
#'
#' The supplied angles specify rotations to be made around the axes for which
#' heading, pitch, and roll are defined.  For example, an eastward current will
#' point southeast if \code{heading=45} is used.
#'
#' The returned value has heading, pitch, and roll matching those of \code{x},
#' so these angles retain their meaning as the instrument orientation.
#'
#' NOTE: this function works similarly to \code{\link{xyzToEnuAdv}}, except
#' that in the present function, it makes no difference whether the instrument
#' points up or down, etc.
#'
#' @param x An \code{adv} object, i.e. one inheriting from \code{\link{adv-class}}.
#' @param heading number or vector of numbers, giving the angle, in degrees, to
#' be added to the heading. If this has length less than the number of velocity
#' sampling times, then it will be extended using \code{\link{rep}}.
#' @param pitch as \code{heading} but for pitch.
#' @param roll as \code{heading} but for roll.
#' @template debugTemplate
#' @author Dan Kelley
#' @family things related to \code{adv} data
enuToOtherAdv <- function(x, heading=0, pitch=0, roll=0, debug=getOption("oceDebug"))
{
    if (!inherits(x, "adv"))
        stop("method is only for objects of class '", "adv", "'")
    if (x@metadata$oceCoordinate != "enu")
        stop("input must be in \"enu\" coordinates, but it is in ", x@metadata$oceCoordinate, " coordinates")
    oceDebug(debug, "enuToOtherAdv(x, heading=", heading, ", pitch=",
             pitch, ", roll=", roll, ", debug=", debug, ")", unindent=1)
    np <- dim(x@data$v)[1]           # number of profiles
    if (length(heading) < np)
        heading <- rep(heading, length.out=np)
    if (length(pitch) < np)
        pitch <- rep(pitch, length.out=np)
    if (length(roll) < np)
        roll <- rep(roll, length.out=np)
    other <- do_sfm_enu(heading, pitch, roll, x@data$v[, 1], x@data$v[, 2], x@data$v[, 3])
    x@data$v[, 1] <- other$east
    x@data$v[, 2] <- other$north
    x@data$v[, 3] <- other$up
    x@metadata$oceCoordinate <- "other"
    x@processingLog <- processingLogAppend(x@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # enuToOtherAdv()\n", unindent=1)
    x
}
