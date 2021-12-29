## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Class to Store Rsk Data
#'
#' This class stores Ruskin data, from RBR (see reference 1).
#'
#' A [rsk-class] object may be read with [read.rsk()] or created with
#' [as.rsk()].  Plots can be made with [plot,rsk-method()], while
#' [summary,rsk-method()] produces statistical summaries and `show`
#' produces overviews.   If atmospheric pressure has not been removed from the
#' data, the functions [rskPatm()] may provide guidance as to its value;
#' however, this last function is no equal to decent record-keeping at sea.
#'
#' @templateVar class rsk
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
#' @references
#' 1. RBR website (https://www.rbr-global.com/products)
#'
#' @author Dan Kelley and Clark Richards
#'
#' @family classes provided by oce
#' @family things related to rsk data
setClass("rsk", contains="oce")

#' Sample Rsk Dataset
#'
#' A sample `rsk` object derived from a Concerto CTD manufactured by RBR Ltd.
#'
#' The data were obtained September 2015, off the west coast
#' of Greenland, by Matt Rutherford and Nicole Trenholm of the
#' Ocean Research Project, in collaboration with RBR and with the
#' NASA Oceans Melting Greenland project.
#'
#' @name rsk
#'
#' @docType data
#'
#' @references \url{https://rbr-global.com/}
#'
#' @examples
#' library(oce)
#' data(rsk)
#' ## The object doesn't "know" it is CTD until told so
#' plot(rsk)
#' plot(as.ctd(rsk))
#'
#' @family datasets provided with oce
#' @family things related to rsk data
NULL


setMethod(f="initialize",
          signature="rsk",
          definition=function(.Object, time, pressure, temperature, filename="", ...) {
              .Object <- callNextMethod(.Object, ...)
              if (!missing(time)) .Object@data$time <- time
              if (!missing(pressure)) .Object@data$pressure <- pressure
              if (!missing(temperature)) .Object@data$temperature <- temperature
              .Object@metadata$filename <- filename
              .Object@metadata$model <- NA
              ## 20160515 ## Define some things that will likely get redefined by read.rsk(), if
              ## 20160515 ## that is used to create an rsk object.  It does no harm to define
              ## 20160515 ## them, anyway, since they are only used in summaries, and then only
              ## 20160515 ## if the object happens to have these quantities.
              ## 20160515 ## .Object@metadata$units$conductivity <- list(unit=expression(mS/cm), scale="")
              ## 20160515 ## .Object@metadata$units$temperature <- list(unit=expression(degree*C), scale="ITS-90")
              ## 20160515 ## .Object@metadata$units$pressure <- list(unit=expression(dbar), scale="")
              .Object@metadata$pressureType <- "absolute"
              .Object@metadata$pressureAtmospheric <- 10.1325
              .Object@processingLog$time <- presentTime()
              .Object@processingLog$value <- "create 'rsk' object"
              return(.Object)
          })

#' Summarize a Rsk Object
#'
#' Summarizes some of the data in a [rsk-class] object, presenting such information
#' as the station name, sampling location, data ranges, etc.
#'
#' @param object An [rsk-class] object.
#'
#' @param ... Further arguments passed to or from other methods.
#'
#' @seealso The documentation for [rsk-class] explains the structure
#' of CTD objects, and also outlines the other functions dealing with them.
#'
#' @examples
#' library(oce)
#' data(rsk)
#' summary(rsk)
#'
#' @author Dan Kelley
#'
#' @family things related to rsk data
setMethod(f="summary",
          signature="rsk",
          definition=function(object, ...) {
              m <- object@metadata
              mnames <- names(m)
              cat("rsk summary\n-----------\n", ...)
              cat("* Instrument:         model ", m$model,
                  " serial number ", m$serialNumber, "\n", sep="")
              if ("pressureAtmospheric" %in% mnames)
                  cat(paste("* Atmosph. pressure:  ", m$pressureAtmospheric, "\n", sep=""))
              if ("pressureType" %in% mnames)
                  cat(paste("* Pressure type:      ", m$pressureType, "\n", sep=""))
              cat(paste("* Source:             ``", m$filename, "``\n", sep=""))
              invisible(callNextMethod()) # summary
          })

#' Extract Something From a Rsk Object
#'
#' @param x an [rsk-class] object.
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
#' no derived values are defined by [rsk-class] objects.
#'
#' @template sub_subTemplate
#'
#' @author Dan Kelley
#'
#' @family things related to rsk data
setMethod(f="[[",
          signature(x="rsk", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              if (i == "?")
                  return(list(metadata=sort(names(x@metadata)),
                          metadataDerived=NULL,
                          data=sort(names(x@data)),
                          dataDerived=NULL))
              callNextMethod()         # [[
          })

#' Replace Parts of a Rsk Object
#'
#' @param x an [rsk-class] object.
#'
#' @template sub_subsetTemplate
#'
#' @family things related to rsk data
setMethod(f="[[<-",
          signature(x="rsk", i="ANY", j="ANY"),
          definition=function(x, i, j, ..., value) {
              callNextMethod(x=x, i=i, j=j, ...=..., value=value) # [[<-
          })



#' Subset a Rsk Object
#'
#' Subset a rsk object.  This function is somewhat analogous to
#' [subset.data.frame()], but subsetting is only permitted by time.
#'
#' @param x an [rsk-class] object.
#'
#' @param subset a condition to be applied to the `data` portion of `x`.
#' See \sQuote{Details}.
#'
#' @param \dots ignored.
#'
#' @return
#' An [rsk-class] object.
#'
#' @examples
#' library(oce)
#' data(rsk)
#' plot(rsk)
#' plot(subset(rsk, time < mean(range(rsk[['time']]))))
#'
#' @author Dan Kelley
#'
#' @family things related to rsk data
#' @family functions that subset oce objects
setMethod(f="subset",
          signature="rsk",
          definition=function(x, subset, ...) {
              res <- new("rsk") # start afresh in case x@data is a data.frame
              res@metadata <- x@metadata
              res@processingLog <- x@processingLog
              ## message("NOTE: debugging output coming up!")
              for (i in seq_along(x@data)) {
                  ####  message("i: ", i)
                  ####  str(x@data)
                  ####  str(x@data$time[1])
                  ####  print(x@data$time[1])
                  ####  print(x@data$time[2])
                  ####  print(is.language(substitute(subset)))
                  ####  str(substitute(subset))
                  ####  Prior to 2015-01-15 the next line was
                  ##    r <- eval(substitute(subset), x@data)#, parent.frame())
                  ## But that failed when calling subset from within other functions; see
                  ## github (FIXME: fill in issue link, when issue is submitted).
                  ##     http://r.789695.n4.nabble.com/getting-environment-from-quot-top-quot-promise-td4685138.html
                  ## for a question regarding environments. I used to have parent.frame() here, and
                  ## in other "subset" definitions, but my tests are suggesting parent.frame(2)
                  ## will work more generally: (a) within flat code and (b) within a function
                  ## that is passed items to go in the subset.
                  r <- eval(substitute(expr=subset, env=environment()), envir=x@data, enclos=parent.frame(2))
                  ####  str(r)
                  r <- r & !is.na(r)
                  res@data[[i]] <- x@data[[i]][r]
              }
              names(res@data) <- names(x@data)
              subsetString <- paste(deparse(substitute(expr=subset, env=environment())), collapse=" ")
              res@processingLog <- processingLogAppend(res@processingLog, paste("subset.rsk(x, subset=", subsetString, ")", sep=""))
              res
          })

#' Infer Rsk units from a vector of strings
#'
#' This is used by [read.rsk()] to infer the units of data, based
#' on strings stored in `.rsk` files. Lacking a definitive guide
#' to the format of these file, this function was based on visual inspection
#' of the data contained within a few sample files; unusual sensors may
#' not be handled properly.
#'
#' @param s Vector of character strings, holding the `units` entry in the
#' `channels` table of the `.rsk` database.
#'
#' @return List of unit lists.
#'
#' @family functions that interpret variable names and units from headers
unitFromStringRsk <- function(s)
{
    ## Note: some of these use e.g. [lL] because the Ruskin GUI uses upper case,
    ## whereas at least one file has used lower case. Another odd case is
    ## "dbar" vs "dBar", both of which have been seen in files. Still, it
    ## was decided not to use ignore.case=TRUE in the grep() commands,
    ## because that seems to overly blunt the tool.
    ##
    ## Here's how to figure out special characters:
    ## print(s)
    ## [1] "µMol/m²/s"
    ## Browse[1]> Encoding(s)<-"bytes"
    ## Browse[2]> print(s)
    ## [1] "\\xc2\\xb5Mol/m\\xc2\\xb2/s"
    if (1 == length(grep("mg/[lL]", s, useBytes=TRUE)))
        list(unit=expression(mg/l), scale="")
    else if (1 == length(grep("m[lL]/[lL]", s, useBytes=TRUE)))
        list(unit=expression(ml/l), scale="")
    else if (1 == length(grep("((u)|(\xc2\xb5))[mM]ol/[lL]", s, useBytes=TRUE)))
        list(unit=expression(mu*mol/l), scale="")
    else if (1 == length(grep("((u)|(\xc2\xb5))g/[lL]", s, useBytes=TRUE)))
        list(unit=expression(mu*g/l), scale="")
    else if (1 == length(grep("mS/cm", s, useBytes=TRUE)))
        list(unit=expression(mS/cm), scale="")
    else if (1 == length(grep("((u)|(\xc2\xb5))S/cm", s, useBytes=TRUE)))
        list(unit=expression(mu*S/cm), scale="")
    else if (1 == length(grep("d[bB]ar", s, useBytes=TRUE)))
        list(unit=expression(dbar), scale="")
    else if (1 == length(grep("%", s, useBytes=TRUE)))
        list(unit=expression(percent), scale="")
    else if (1 == length(grep("pH_units", s, useBytes=TRUE)))
        list(unit=expression(), scale="")
    else if (1 == length(grep("NTU", s, useBytes=TRUE)))
        list(unit=expression(NTU), scale="")
    else if (1 == length(grep("\xB0", s, useBytes=TRUE)))
        list(unit=expression(degree*C), scale="ITS-90") # guessing on scale
    else if (1 == length(grep("\\xc2\\xb5Mol/m\\xc2\\xb2/s", s, useBytes=TRUE))) # µMol/m²/s
        list(unit=expression(mu*mol/m^2/s), scale="")
    else if (is.na(s))
        list(unit=expression(), scale="?")
    else {
        warning("'", s, "' is not in the list of known .rsk units", sep="")
        list(unit=as.expression(s), scale="")
    }
}

#' Coerce Data Into a Rsk Object
#'
#' Create a rsk object.
#'
#' The contents of `columns` are be copied into the `data` slot
#' of the returned object directly, so it is critical that the names and units
#' correspond to those expected by other code dealing with
#' [rsk-class] objects. If there is a conductivity, it must be called
#' `conductivity`, and it must be in units of mS/cm. If there is a
#' temperature, it must be called `temperature`, and it must be an in-situ
#' value recorded in ITS-90 units.  And if there is a pressure, it must be
#' *absolute* pressure (sea pressure plus atmospheric pressure) and it must
#' be named `pressure`. No checks are made within `as.rsk` on any of
#' these rules, but if they are broken, you may expect problems with any further
#' processing.
#'
#' @param time a vector of times for the data.
#'
#' @param columns a list or data frame containing the measurements at the indicated
#' times; see \dQuote{Details}.
#'
#' @param filename optional name of file containing the data.
#'
#' @param instrumentType type of instrument.
#'
#' @param serialNumber serial number for instrument.
#'
#' @param model instrument model type, e.g. `"RBRduo"`.
#'
#' @param sampleInterval sampling interval. If given as `NA`, then this is
#' estimated as the median difference in times.
#'
#' @param debug a flag that can be set to `TRUE` to turn on debugging.
#'
#' @return An [rsk-class] object.
#'
#' @author Dan Kelley
#'
#' @family things related to rsk data
as.rsk <- function(time, columns,
                   filename="", instrumentType="rbr", serialNumber="", model="",
                   sampleInterval=NA,
                   debug=getOption("oceDebug"))
{
    debug <- min(debug, 1)
    oceDebug(debug, "as.rsk(..., filename=\"", filename, "\", serialNumber=\"", serialNumber, "\")\n", sep="", unindent=1)
    if (inherits(time, "oce")) {
        stop("cannot coerce from general oce object to rsk; submit an issue if you need this")
    }
    if (missing(time))
        stop("must give time")
    if (!inherits(time, "POSIXt"))
        stop("'time' must be POSIXt")
    time <- as.POSIXct(time)
    res <- new("rsk")
    res@metadata$instrumentType <- instrumentType
    if (nchar(model))
        res@metadata$model <-model
    res@metadata$serialNumber <- serialNumber
    res@metadata$filename <- filename
    res@metadata$sampleInterval <- if (is.na(sampleInterval))
        median(diff(as.numeric(time)), na.rm=TRUE) else sampleInterval
    processingLog <- paste(deparse(match.call()), sep="", collapse="")
    res@processingLog <- processingLogAppend(res@processingLog, processingLog)
    res@data <- list(time=time)
    if (!missing(columns)) {
        for (item in names(columns)) {
            res@data[[item]] <- columns[[item]]
        }
    }
    oceDebug(debug, "} # as.rsk()\n", sep="", unindent=1)
    res
}


#' Plot a rsk Object
#'
#' Rsk data may be in many forms, and it is not easy to devise a general plotting
#' strategy for all of them. The present function is quite crude, on the
#' assumption that users will understand their own datasets, and that they can
#' devise plots that are best-suited to their applications.  Sometimes, the
#' sensible scheme is to coerce the object into another form, e.g. using
#' `plot(as.ctd(rsk))` if the object contains CTD-like data.  Other times,
#' users should extract data from the `rsk` object and construct plots
#' themselves. The idea is to use the present function mainly to get an overview,
#' and for that reason, the default plot type (set by `which`) is a set of
#' time-series plots, because the one thing that is definitely known about
#' `rsk` objects is that they contain a `time` vector in their
#' `data` slot.
#'
#' @details Plots produced are time series plots of the data in the
#'     object. The default, `which="timeseries"` plots all data
#'     fields, and over-rides any other specification. Specific fields
#'     can be plotted by naming the field,
#'     e.g. `which="temperature"` to plot a time series of just
#'     the temperature field.
#'
#' @param x an [rsk-class] object.
#'
#' @param which character indicating desired plot types.  These are
#'     graphed in panels running down from the top of the page.  See
#'     \dQuote{Details} for the meanings of various values of
#'     `which`.
#'
#' @param tlim optional limits for time axis.  If not provided, the value will be
#' inferred from the data.
#'
#' @param ylim optional limits for the y axis.  If not provided, the
#'     value will be inferred from the data.  (It is helpful to
#'     specify this, if the auto-scaled value will be inappropriate,
#'     e.g. if more lines are to be added later). Note that this is
#'     ignored, unless `length(which) == 1` and `which`
#'     corresponds to one of the data fields. If a multipanel plot of
#'     a specific subset of the data fields is desired with
#'     `ylim` control, it should be done panel by panel (see
#'     Examples).
#'
#' @param xlab optional label for x axis.
#'
#' @param ylab optional label for y axis.
#'
#' @param tformat optional argument passed to [oce.plot.ts()], for plot
#' types that call that function.  (See [strptime()] for the format
#' used.)
#'
#' @param drawTimeRange boolean that applies to panels with time as the horizontal
#' axis, indicating whether to draw the time range in the top-left margin of the
#' plot.
#'
#' @param abbreviateTimeRange boolean that applies to panels with time as the
#' horizontal axis, indicating whether to abbreviate the second time in the time
#' range (e.g. skipping the year, month, day, etc. if it's the same as the start
#' time).
#'
#' @param useSmoothScatter a boolean to cause [smoothScatter()] to be
#' used for profile plots, instead of [plot()].
#'
#' @param mgp 3-element numerical vector to use for [`par`]`("mgp")`, and
#' also for `par(mar)`, computed from this.  The default is tighter than the
#' R default, in order to use more space for the data and less for the axes.
#'
#' @param mar value to be used with [`par`]`("mar")`.
#'
#' @param main main title for plot, used just on the top panel, if there are several panels.
#'
#' @param debug a flag that turns on debugging, if it exceeds 0.
#'
#' @param ... optional arguments passed to plotting functions.
#'
#' @examples
#' library(oce)
#' data(rsk)
#' plot(rsk) # default timeseries plot of all data fields
#'
#' ## A multipanel plot of just pressure and temperature with ylim
#' par(mfrow=c(2, 1))
#' plot(rsk, which="pressure", ylim=c(10, 30))
#' plot(rsk, which="temperature", ylim=c(2, 4))
#'
#' @seealso
#' The documentation for [rsk-class] explains the structure of
#' `rsk` objects, and also outlines the other functions dealing with them.
#'
#' @author Dan Kelley and Clark Richards
#'
#' @family functions that plot oce data
#' @family things related to rsk data
#'
#' @aliases plot.rsk
setMethod(f="plot",
          signature=signature("rsk"),
          definition=function(x, which="timeseries",
                              tlim, ylim,
                              xlab, ylab,
                              tformat,
                              drawTimeRange=getOption("oceDrawTimeRange"),
                              abbreviateTimeRange=getOption("oceAbbreviateTimeRange"),
                              useSmoothScatter=FALSE,
                              mgp=getOption("oceMgp"),
                              mar=c(mgp[1]+1.5, mgp[1]+1.5, 1.5, 1.5),
                              main="",
                              debug=getOption("oceDebug"),
                              ...)
          {
              oceDebug(debug, "plot.rsk(..., which=", which, ", ...) {\n", unindent=1)
              dotsNames <- names(list(...))
              ## FIXME: In the below, we could be more clever for single-panel plots
              ## but it may be better to get users out of the habit of supplying xlim
              ## etc (which will yield errors in plot.lm(), for example).
              if ("xlim" %in% dotsNames)
                  stop("in plot.rsk() : 'xlim' not allowed; use tlim", call.=FALSE)
              if (any(which=="timeseries"))
                  which <- "timeseries" # "timeseries" overrides any others
              lw <- length(which)
              if (lw == 1 && which=="timeseries") {
                  opar <- par(no.readonly = TRUE)
                  on.exit(par(opar))
                  names <- names(x@data)
                  if (!"time" %in% names) stop("plot.rsk() cannot plot timeseries, since no \"time\" data", call.=FALSE)
                  names <- names[names != "time"]
                  par(mfrow=c(length(names), 1))
                  for (name in names) {
                      if (!is.null(x[['units']])) {
                          unit <- x[['units']][[name]]$unit
                          tmp <- c(name, '~"["*', as.character(unit), '*"]"')
                          label <- bquote(.(parse(text=paste0(tmp, collapse=''))))
                      } else {
                          label <- name
                      }
                      oce.plot.ts(x[["time"]], x[[name]], ylab=label, ...)
                  }
              } else {
                  ## individual panels
                  ## Trim out plots that we cannot do.
                  names <- names(x@data)
                  names <- names[- (names=="time")]
                  nw <- length(which)
                  ## opar <- par(no.readonly = TRUE)
                  ## if (nw > 1) {
                  ##     par(mfrow=c(nw, 1))
                  ##     on.exit(par(opar))
                  ## }
                  if (missing(main))
                      main <- rep('', length.out=nw)
                  else
                      main <- rep(main, length.out=nw)
                  oceDebug(debug, "after nickname-substitution, which=c(", paste(which, collapse=","), ")\n")
                  for (w in 1:nw) {
                      oceDebug(debug, "which[", w, "]=", which[w], "\n")
                      haveField <- (which[w] %in% names) && any(is.finite(x[[which[w]]]))
                      if (haveField) {
                          if (!is.null(x[['units']])) {
                              unit <- x[['units']][[which[w]]]$unit
                              tmp <- c(which[w], '~"["*', as.character(unit), '*"]"')
                              label <- bquote(.(parse(text=paste0(tmp, collapse=''))))
                          } else {
                              label <- which[w]
                          }
                          oce.plot.ts(x@data$time, x[[which[w]]],
                                      xlab=if (!missing(xlab)) xlab else "",
                                      ylab=if (missing(ylab)) label else  ylab,
                                      type='l',
                                      xlim=if (missing(tlim)) range(x@data$time, na.rm=TRUE) else tlim,
                                      ylim=if (missing(ylim)) range(x[[which[w]]], na.rm=TRUE) else ylim,
                                      tformat=tformat,
                                      drawTimeRange=drawTimeRange,
                                      mgp=mgp, mar=mar, main=main[w], ...)
                          drawTimeRange <- FALSE    # only the first time panel gets the time indication
                          axis(2)
                      } else {
                          stop("Unrecognized value for \"which\". Must be \"timeseries\" or the name of any field from the data slot.")
                      }
                  }
              }
              oceDebug(debug, "} # plot.rsk()\n", unindent=1)
              invisible(NULL)
          })



#' Read a Rsk file
#'
#' Read an RBR rsk or txt file, e.g. as produced by an RBR logger, producing an
#' object of class `rsk`.
#'
#' This can read files produced by several RBR instruments.  At the moment, five
#' styles are understood: (1) text file produced as an export of an RBR `hex`
#' or `rsk` file; (2) text file with columns for temperature and pressure
#' (with sampling times indicated in the header); (3) text file with four columns,
#' in which the date the time of day are given in the first two columns, followed
#' by the temperature, and pressure; (4) text file with five columns, in which
#' depth in the water column is given after the pressure; (5) an SQLite-based
#' database format. The first four options are provided mainly for historical
#' reasons, since RBR instruments at the date of writing commonly use the SQLite
#' format, though the first option is common for all instruments that produce a
#' `hex` file that can be read using Ruskin.
#'
#' Options 2-4 are mostly obsolete, and will be removed from future versions.
#'
#' *A note on conductivity.* RBR devices record conductivity in mS/cm, and it
#' is this value that is stored in the object returned by `read.rsk`. This can
#' be converted to conductivity ratio (which is what many other instruments report)
#' by dividing by 42.914 (see Culkin and Smith, 1980) which will be necessary in
#' any seawater-related function that takes conductivity ratio as an argument (see
#' \dQuote{Examples}).
#'
#'   *A note on pressure.* RBR devices tend to record absolute pressure (i.e.
#'   sea pressure plus atmospheric pressure), unlike most oceanographic instruments
#'   that record sea pressure (or an estimate thereof).  The handling of pressure
#'   is controlled with the `patm` argument, for which there are three
#'   possibilities.  (1) If `patm` is `FALSE` (the default), then
#'   pressure read from the data file is stored in the `data` slot of return
#'   value, and the `metadata` item `pressureType` is set to the string
#'   `"absolute"`.  (2) If `patm` is `TRUE`, then an estimate of
#'   atmospheric pressure is subtracted from the raw data. For data files in the
#'   SQLite format (i.e.  `*.rsk` files), this estimate will be the value read
#'   from the file, or the ``standard atmosphere'' value 10.1325 dbar, if the file
#'   lacks this information.  (3) If `patm` is a numerical value (or list of
#'   values, one for each sampling time), then `patm` is subtracted from the
#'   raw data.  In cases 2 and 3, an additional column named
#'   `pressureOriginal` is added to the `data` slot to store the value
#'   contained in the data file, and `pressureType` is set to a string
#'   starting with `"sea"`.  See [as.ctd()] for details of how this
#'   setup facilitates the conversion of [rsk-class] objects to
#'   [ctd-class] objects.
#'
#' @param file a connection or a character string giving the name of the file to
#' load. Note that `file` must be a character string, because connections are
#' not used in that case, which is instead handled with database calls.
#'
#' @param from indication of the first datum to read.  This can a positive integer
#' to indicate sequence number, the POSIX time of the first datum, or a character
#' string that can be converted to a POSIX time.  (For POSIX times, be careful
#' about the `tz` argument.)
#'
#' @param to an indication of the last datum to be read, in the same format as
#' `from`.  If `to` is missing, data will be read to the end of the file.
#'
#' @param by an indication of the stride length to use while walking through the
#' file.  If this is an integer, then `by-1` samples are skipped between each
#' pair of samples that is read.  If this is a string representing a time interval,
#' in colon-separated format (HH:MM:SS or MM:SS), then this interval is divided by
#' the sampling interval, to get the stride length.
#'
#' @param type optional file type, presently can be `rsk` or `txt` (for a
#' text export of an RBR rsk or hex file). If this argument is not provided, an
#' attempt will be made to infer the type from the file name and contents.
#'
#' @param tz time zone.  The value `oceTz` is set at package setup.
#'
#' @param patm controls the handling of atmospheric pressure, an important issue
#' for RBR instruments that record absolute pressure; see \dQuote{Details}.
#'
#' @param processingLog if provided, the action item to be stored in the log.
#' This is typically only provided for internal calls; the default that it provides
#' is better for normal calls by a user.
#'
#' @param debug a flag that can be set to `TRUE` to turn on debugging.
#'
#' @return An [rsk-class] object.
#'
#' @seealso
#' The documentation for [rsk-class] explains the structure of
#' `rsk` objects, and also outlines other functions dealing with them.  Since
#' RBR has a wide variety of instruments, `rsk` datasets can be quite general,
#' and it is common to coerce `rsk` objects to other forms for specialized
#' work, e.g. [as.ctd()] can be used to create CTD object, so that the
#' generic plot obeys the CTD format.
#'
#' @references
#' Culkin, F., and Norman D. Smith, 1980. Determination of the concentration of
#' potassium chloride solution having the same electrical conductivity, at 15 C and
#' infinite frequency, as standard seawater of salinity 35.0000 ppt (Chlorinity
#' 19.37394 ppt). *IEEE Journal of Oceanic Engineering*, **5**, pp 22-23.
#'
#' @author Dan Kelley and Clark Richards
#'
#' @family things related to rsk data
read.rsk <- function(file, from=1, to, by=1, type, tz=getOption("oceTz", default="UTC"),
                        patm=FALSE, processingLog, debug=getOption("oceDebug"))
{
    if (!missing(file) && is.character(file) && 0 == file.info(file)$size)
        stop("empty file")
    debug <- max(0, min(debug, 2))
    oceDebug(debug, "read.rsk(file=\"", file, "\", from=", format(from),
             ", to=", if (missing(to))"(not given)" else format(to),
             ", by=", by,
             ", type=", if (missing(type)) "(missing)" else type,
             ", tz=\"", tz, "\", ...) {\n", sep="", unindent=1)
    filename <- file
    if (is.character(file)) {
        if (length(grep(".rsk$", file, ignore.case=TRUE, useBytes=TRUE)))
            type <- "rsk"
        else if (length(grep(".txt$", file, ignore.case=TRUE)))
            type <- "txt"
        file <- file(file, "r")
        on.exit(close(file))
    }
    if (!inherits(file, "connection"))
        stop("'file' must be a character string or connection")
    if (!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }
    from.keep <- from
    ##measurement.deltat <- 0
    if (is.numeric(from) && from < 1)
        stop("from cannot be an integer less than 1")

    if (!missing(to)) {
        if (is.numeric(to) && to < 1)
            stop("to cannot be an integer less than 1")
    }

    ##from.keep <- from
    if (!missing(to))
        to.keep <- to
    ##by.keep <- by
    ##host.time <- 0
    ##rsk.time <- 0
    ##subsampleStart <- 0
    ##subsampleEnd <- 0
    ##subsamplePeriod <- 0
    ##number.channels <- 0
    ## Q: what ends the header? a blank line?  Line 21?
    ## calibration 1
    ## calibration 2
    ## correction.to.conductivity
    ## memory type
    ## Timestamp
    ## columns time, Temperature, p
    ##header <- scan(file, what='char', sep="\n", n=19, quiet=TRUE)
    header <- c()
    measurementStart <-measurementEnd <- measurementDeltat <- NULL
    pressureAtmospheric <- NA
    if (!missing(type) && type == 'rsk') {
        if (!requireNamespace("RSQLite", quietly=TRUE))
            stop('must install.packages("RSQLite") to read rsk data')
        if (!requireNamespace("DBI", quietly=TRUE))
            stop('must install.packages("DBI") to read rsk data')
        con <- DBI::dbConnect(RSQLite::SQLite(), dbname=filename)

        ## Some, but not all, RSK files have "deployments", but we don't use it anyway.
        ##  deployments <- RSQLite::dbReadTable(con, "deployments")

        ## code based on test files and personal communication with RBR:
        ##   2011-10-11 RBR-DEK send test file and schema documentation [preliminary]
        ##   2011-10-12 DEK-RBR query on ordering of time in 'datasets'
        ## if (!inherits(from, 'POSIXt') & from != 1) {
        ##     warning("cannot (yet) handle numeric argument 'from' for a ruskin file; using from=1 instead (or use POSIXt)")
        ##     from <- 1
        ## }
        ## if (by != 1)
        ##     warning("cannot (yet) handle argument 'by' for a ruskin file; using by=1 instead")
        ## if (!missing(to))
        ##     warning("cannot (yet) handle numeric argument 'to' for a ruskin file; using the whole file (or use POSIXt)")

        ## Some, but not all, RSK files have "datasets". However, I've commented this code
        ## out because the result, ndatasets, is not used anywhere else.
        ##
        ##   ndatasets <- 1
        ##   try({
        ##       datasets <- RSQLite::dbReadTable(con, "datasets")
        ##       ndatasets <- dim(datasets)[1]
        ##       if (1 != ndatasets)
        ##           stop("read.rsk() cannot handle multi-dataset files; this file has ", ndatasets)
        ##   }, silent=TRUE)
        ##

        ## rsk database-schema version number
        dbInfo <- RSQLite::dbReadTable(con, "dbInfo")
        rskv <- dbInfo[1, 1]
        rskVersion <- as.numeric(strsplit(gsub(".[a-z].*$", "", gsub("^.*- *", "", rskv)), "\\.")[[1]])
        ## Ruskin software version number
        if (RSQLite::dbExistsTable(con, "appSettings")) {
            appSettings <- RSQLite::dbReadTable(con, "appSettings")
            rv <- appSettings[1, 2]
            ##OLD rv <- read.table(pipe(cmd), sep="|")[1, 2]
            ruskinVersion <- as.numeric(strsplit(gsub(".[a-z].*$", "", gsub("^.*- *", "", rv)), "\\.")[[1]])
        } else {
            ruskinVersion <- "mobile"
        }
        ##message("NEW: ruskinVersion: ", paste(ruskinVersion, collapse="."))
        ## Next block got triggered with too many files, and it seems more sensible
        ## to just go ahead and try to get something from the file as best we can.
        ## if (length(ruskinVersion == 3)) {
        ##     if (!(ruskinVersion[1] == 1 && ruskinVersion[2] == 5 && ruskinVersion[3] == 24))
        ##         warning("making some (untested) assumptions, since the ruskin Version (",
        ##                 paste(ruskinVersion, collapse="."),
        ##                 ") is outside the range for which tests have been done")
        ## }
        ## atmospheric pressure
        pressureAtmospheric <- 10.1325 # FIXME: what is best default?
        oceDebug(debug, "first, guess pressureAtmospheric=", pressureAtmospheric, "\n")
        warn <- FALSE
        try({
            ## need to wrap in try() because this can fail
            deriveDepth <- RSQLite::dbReadTable(con, "deriveDepth")
            pressureAtmospheric <- deriveDepth$atmosphericPressure
            warn <- TRUE
        }, silent=TRUE)
        if (warn)
            warning("non-standard pressureAtmospheric value: ", pressureAtmospheric)
        ## some cases can have an empty pressureAtmospheric
        if (length(pressureAtmospheric) == 0) {
            warning("empty pressureAtmospheric value in rsk file. Setting to default value of 10.1325")
            pressureAtmospheric <- 10.1325
        }
        ##message("NEW: pressureAtmospheric:", pressureAtmospheric)
        oceDebug(debug, "after studying the RSK file, now have pressureAtmospheric=", pressureAtmospheric, "\n")

        ## From notes in comments above, it seems necessary to order by
        ## timestamp (tstamp). Ordering does not seem to be an option for
        ## dbReadTable(), so we use dbFetch().

        ## Get time stamp. Note the trick of making it floating-point
        ## to avoid the problem that R lacks 64 bit integers.
        fields <- DBI::dbListFields(con, "data")
        fields <- fields[!grepl('tstamp', fields)]
        sql_fields <- if (packageVersion("RSQLite") < "2.0") "1.0*tstamp AS tstamp" else "tstamp"

        sql_fields <- paste(c(sql_fields, fields), collapse=',')
        sql_fields <- paste("SELECT", sql_fields, "FROM data")


        # When to and from are numeric and not equal to 1 we have to query the table
        # and then sort the times so that the limits are meaningful.  This code
        # does that only when required and will be slower than when from and to
        # are dates or character.
        time <- NA

        if (!missing(to)) {
            if (inherits(to, 'POSIXt')) {
                to <- as.character(as.numeric(to)*1000)
            } else if (inherits(to, 'character')) {
                to <- as.character(as.numeric(as.POSIXct(to, tz=tz))*1000)
            } else if (is.numeric(to)) {
                qres <- DBI::dbSendQuery(con,
                                         if (packageVersion("RSQLite") < "2.0")
                                             "select 1.0*tstamp from data order by tstamp;"
                                         else
                                             "select tstamp from data order by tstamp;")
                t1000 <- DBI::dbFetch(qres, n=-1)[[1]]
                RSQLite::dbClearResult(qres)
                time <- numberAsPOSIXct(as.numeric(t1000) / 1000, type='unix')
            }
        }

        if (is.numeric(from) & from != 1 & all(is.na(time))) {
            qres <- DBI::dbSendQuery(con,
                                     if (packageVersion("RSQLite") < "2.0")
                                         "select 1.0*tstamp from data order by tstamp;"
                                     else
                                         "select tstamp from data order by tstamp;")
            t1000 <- DBI::dbFetch(qres, n=-1)[[1]]
            RSQLite::dbClearResult(qres)
            time <- numberAsPOSIXct(as.numeric(t1000) / 1000, type='unix')
        }

        ## format to and from that match tstamp from the rsk file
        if (inherits(from, 'POSIXt')) {
            from <- as.character(as.numeric(from)*1000)
        } else if (inherits(from, 'character')) {
            from <- as.character(as.numeric(as.POSIXct(from, tz=tz))*1000)
        }

        if (!all(is.na(time))) {
            if (is.numeric(from)) {
                from <- t1000[from]
            }
            if (missing(to)) {
                to <- tail(t1000, 1)
            } else if (is.numeric(to)) {
                to <- t1000[to]
            }
        }
        ## Generate the sql that contains the time filters
        if (missing(to)) {
            if (is.numeric(from)) {
                qres <- DBI::dbSendQuery(con, paste(sql_fields, ";"))
            } else {
                qres <- DBI::dbSendQuery(con, paste(sql_fields, "where tstamp >=",  from, ";"))
            }
        } else {
            if (missing(to)) {
                qres <- DBI::dbSendQuery(con, paste(sql_fields, "where tstamp >=",  from, ";"))
            } else if (from==1) {
                qres <- DBI::dbSendQuery(con, paste(sql_fields, "where tstamp <=",  to, ";"))
            } else {
                qres <- DBI::dbSendQuery(con, paste(sql_fields, "where tstamp between",  from, "and", to, ";"))
            }
        }

        ## Now, get only the specified time range
        data <- DBI::dbFetch(qres, n=-1)
        data <- data[order(data$tstamp),]
        time <- numberAsPOSIXct(as.numeric(data[,1])/1000, type='unix')

        ## Need to check if there is a datasetID column (for rskVersion >= 1.12.2)
        ## If so, for now just extract it from the data matrix
        hasDatasetID <- sum(grep('datasetID', names(data))) > 0
        if (hasDatasetID) {
            datasetID <- data[, grep('datasetID', names(data))]
            data <- data[, -grep('datasetID', names(data)), drop=FALSE]
        }
        data <- data[,c(-1), drop=FALSE] # drop the corrupted time column
        DBI::dbClearResult(qres)
        ## Get column names from the 'channels' table.
        names <- tolower(RSQLite::dbReadTable(con, "channels")$longName)
        ## FIXME: some longnames have UTF-8 characters, and/or
        ## spaces. Should coerce to ascii with no spaces, or at least
        ## recognize fields and rename (e.g. `dissolved O2` should
        ## just be `oxygen`)
        names <- gsub(" ", "", names, fixed = TRUE) # remove spaces
        Encoding(names) <- 'latin1'
        names <- iconv(names, 'latin1', 'ASCII', sub='')
        ## if dissolvedo is a name call it oxygen
        names[which(match(names, 'dissolvedo') == 1)] <- 'oxygen'
        channelsTable <- RSQLite::dbReadTable(con, "channels")
        if ("isMeasured" %in% names(channelsTable)) {
            isMeasured <- channelsTable$isMeasured == 1
        } else {
            isMeasured <- channelsTable$isDerived == 0
            ##warning("old Ruskin file detected; if problems arise, update file with Ruskin software")
        }
        dataNamesOriginal <- c("-", channelsTable$shortName[isMeasured])
        ##1491> message("below is dataNamesOriginal: (at start)");print(dataNamesOriginal)
        ##[issue 1483] print(cbind(channelsTable,isMeasured))
        names <- names[isMeasured] # only take names of things that are in the data table
        unitsRsk <- channelsTable$units[isMeasured]
        ## Check for duplicated names, and append digits to make unique
        if (sum(duplicated(names)) > 0) {
            for (n in names) {
                dup <- match(names, n, nomatch=0)
                if (sum(dup) > 1) {
                    ## more than one
                    names[which(dup==1)] <- paste0(n, c('', seq(2, sum(dup))))
                }
            }
        }
        names(data) <- names
        data <- as.list(data)
        instruments <- RSQLite::dbReadTable(con, "instruments")
        serialNumber <- instruments$serialID
        model <- instruments$model
        schedules <- RSQLite::dbReadTable(con, "schedules")
        sampleInterval <- schedules$samplingPeriod/1000 # stored as milliseconds in rsk
        RSQLite::dbDisconnect(con)
        res <- new("rsk", time=time, filename=filename)
        res@metadata$dataNamesOriginal <- dataNamesOriginal
        for (iname in seq_along(names)) {
            res@data[[names[iname]]] <- data[[names[iname]]]
            res@metadata$units[[names[iname]]] <- unitFromStringRsk(unitsRsk[iname])
            if (debug > 1) {
                ## FIXME: developer sets this for temporary (and undocumented) debugging
                cat("\n***\nUNIT CHECK. The rsk string", unitsRsk[iname], "yielded as follows:\n")
                print(res@metadata$units[[names[iname]]])
                cat("***\n")
            }
        }
        res@metadata$units$pressure$scale <- "absolute"
        ##1491> message("res@metadata$dataNamesOriginal L909:");print(res@metadata$dataNamesOriginal)
        ##?browser()
        if ("pressure" %in% names) {
            ## possibly compute sea pressure
            if (is.logical(patm)) {
                if (patm) {
                    ## This code is a bit tricky because we modify existing pressure in-place
                    dataNames <- names(res@data)
                    dataNames[dataNames=="pressure"] <- "pressureOriginal"
                    names(res@data) <- dataNames
                    res@metadata$units$pressureOriginal <- list(unit=expression(dbar), scale="absolute")
                    res@data$pressure <- res@data$pressureOriginal - 10.1325
                    res@metadata$units$pressure <- list(unit=expression(dbar), scale="sea")
                    res@metadata$dataNamesOriginal <- c(res@metadata$dataNamesOriginal, "-")
                    res@metadata$pressureType <- "sea"
                    oceDebug(debug, "patm=TRUE, so removing std atmospheric pressure, 10.1325 dbar\n")
                }
            } else if (is.numeric(patm)) {
                npatm <- length(patm)
                if (1 < npatm && npatm != length(pressure))
                    stop("if patm is numeric, its length must equal 1, or the length(pressure).")
                oceDebug(debug, "patm is numeric, so subtracting it from pressure\n")
                ## This code is a bit tricky because we modify existing pressure in-place
                dataNames <- names(res@data)
                dataNames[dataNames=="pressure"] <- "pressureOriginal"
                names(res@data) <- dataNames
                res@metadata$units$pressureOriginal <- list(unit=expression(dbar), scale="absolute")
                res@data$pressure <- res@data$pressureOriginal - patm
                res@metadata$units$pressure <- list(unit=expression(dbar), scale="sea")
                res@metadata$dataNamesOriginal <- c(res@metadata$dataNamesOriginal, "-")
                res@metadata$pressureType <- "sea"
            } else {
                stop("patm must be logical or numeric")
            }
        }
        ##1491> message("res@metadata$dataNamesOriginal L944:");print(res@metadata$dataNamesOriginal)
        res@metadata$model <- model
        res@metadata$serialNumber <- serialNumber
        res@metadata$sampleInterval <- sampleInterval
        res@metadata$rskVersion <- rskVersion
        res@metadata$ruskinVersion <- ruskinVersion
        ##1491> message("res@metadata$dataNamesOriginal L951:");print(res@metadata$dataNamesOriginal)
        ##1491> message("names(res@data) L952:");print(names(res@data))
        ## HERE
        names(res@metadata$dataNamesOriginal) <- names(res@data)
        if (hasDatasetID) res@metadata$datasetID <- datasetID
        ## There is actually no need to set the conductivity unit since new()
        ## sets it, but do it anyway, as a placeholder to show where to do
        ## this, in case some RBR devices use different units.
        if ("cond12" %in% names(res@data)) {
            ## [issue 1483] Change the name, and possibly unit, of 'cond12'
            ##
            ## For a sample file, channelsTable gives the unit for cond12 as
            ## NA.  Rather than make unitFromStringRsk() give a conductivity
            ## unit whenever it gets an NA value (which might occur for other
            ## fields -- who knows?), we will switch NA to uS/cm because
            ## that seems to be the usual unit for RBR instruments. However,
            ## if the cond12 unit is not NA, we will leave it as it is, on the
            ## assumption that unitFromStringRsk() has already interpreted
            ## it correctly
            w <- which("cond12" == names)[1]
            if (is.na(unitsRsk[w])) {
                res@metadata$units$cond12 <- NULL # remove existing
                res@metadata$units$conductivity <- list(unit=expression(mS/cm), scale="")
            }
            newnames <- gsub("cond12", "conductivity", names(res@data))
            names(res@data) <- newnames
            names(res@metadata$dataNamesOriginal) <- newnames
        } else {
            ## FIXME: will this work for all RBR rsks that don't contain cond12?
            res@metadata$units$conductivity <- list(unit=expression(mS/cm), scale="")
        }
        ##1491> message("str(dataNamesOriginal) L984:");print(res@metadata$dataNamesOriginal)
        res@metadata$pressureAtmospheric <- pressureAtmospheric
        res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
        oceDebug(debug, "} # read.rsk()\n", sep="", unindent=1)
        return(res)
    } else if (!(missing(type)) && type=='txt') {
        oceDebug('RBR txt format\n')
        oceDebug(debug, "Format is Rtext Ruskin txt export", "\n")
        l <- readLines(file, n=50000)         # FIXME: need to read a
                                              # lot if there are lots
                                              # of "Events". Is there
                                              # a better way to do
                                              # this?
        pushBack(l, file)
        model <- unlist(strsplit(l[grep('Model', l, useBytes=TRUE)], '='))[2]
        serialNumber <- as.numeric(unlist(strsplit(l[grep('Serial', l, useBytes=TRUE)], '='))[2])
        sampleInterval <- 1/as.numeric(gsub('Hz', '', unlist(strsplit(l[grep('SamplingPeriod', l, useBytes=TRUE)], '='))[2]))
        numberOfChannels <- as.numeric(unlist(strsplit(l[grep('NumberOfChannels', l, useBytes=TRUE)], '='))[2])
        oceDebug(debug, "Model: ", model, "\n")
        oceDebug(debug, "serialNumber: ", serialNumber, "\n")
        oceDebug(debug, "sampleInterval: ", sampleInterval, "\n")
        oceDebug(debug, "File has ", numberOfChannels, "channels", "\n")
        channelNames <- NULL
        for (iChannel in 1:numberOfChannels) {
            channelNames <- c(channelNames,
                              tolower(unlist(strsplit(l[grep(paste0('Channel\\[', iChannel, '\\]'), l, useBytes=TRUE)], '=', useBytes=TRUE))[2]))
        }
        oceDebug(debug, "Channel names are:", channelNames, "\n")
        skip <- grep('Date & Time', l, useBytes=TRUE)      # Where should I start reading the data?
        oceDebug(debug, "Data starts on line", skip, "\n")
        d <- read.table(file, skip=skip, stringsAsFactors = FALSE)
        oceDebug(debug, "First time=", d$V1[1], d$V2[1], "\n")
        ## Assume date and time are first two columns
        time <- as.POSIXct(paste(d$V1, d$V2), format='%d-%b-%Y %H:%M:%OS', tz=tz)
        n <- length(time)
        channels <- list()
        for (iChannel in 1:numberOfChannels) {
            channels[[iChannel]] <- d[, iChannel+2]
        }
        names(channels) <- channelNames
        ## Now do subsetting
        if (inherits(from, "POSIXt") || inherits(from, "character")) {
            if (!inherits(to, "POSIXt") && !inherits(to, "character"))
                stop("if 'from' is POSIXt or character, then 'to' must be, also")
            if (to <= from)
                stop("cannot have 'to' <= 'from'")
            from <- as.numeric(difftime(as.POSIXct(from, tz=tz), measurementStart, units="secs")) / measurementDeltat
            oceDebug(debug, "inferred from =", format(from, width=7), " based on 'from' arg", from.keep, "\n")
            to <- as.numeric(difftime(as.POSIXct(to, tz=tz), measurementStart, units="secs")) / measurementDeltat
            oceDebug(debug, "inferred   to =",   format(to, width=7), " based on   'to' arg", to.keep, "\n")
        } else {
            if (from < 1)
                stop("cannot have 'from' < 1")
            if (!missing(to) && to < from)
                stop("cannot have 'to' < 'from'")
        }
        oceDebug(debug, "from=", from, "\n")
        oceDebug(debug, "to=", if (missing(to))"(not given)" else format(to), "\n")
        oceDebug(debug, "by=", by, "\n")
        if (inherits(by, "character")) by <- ctimeToSeconds(by)/sampleInterval # FIXME: Is this right?
        oceDebug(debug, "inferred by=", by, "samples\n")
        ## subset times
        if (inherits(from, "POSIXt") || inherits(from, "character")) {
            keep <- from <= time & time <= to # FIXME: from may be int or time
        } else {
            if (missing(to))
                keep <- seq.int(from, n, by)
            else
                keep <- seq.int(from, to, by)
        }
        oceDebug(debug, "will be skipping time with seq(..., by=", by, ")\n")
        time <- time[keep]
        channelsSub <- list()
        for (iChannel in 1:numberOfChannels) {
            channelsSub[[iChannel]] <- channels[[iChannel]][keep]
        }
        names(channelsSub) <- channelNames
        res <- as.rsk(time, columns=channelsSub,
                       instrumentType="rbr",
                       serialNumber=serialNumber, model=model,
                       sampleInterval=sampleInterval,
                       filename=filename,
                       debug=debug-1)
    } else {
        ## to read the "old" TDR files
        while (TRUE) {
            line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
            if (0 < (r<-regexpr("Temp[ \t]*Pres", line))) # nolint (variable not used)
                break
            header <- c(header, line)
            if (0 < (r<-regexpr("Logging[ \t]*start", line))) {
                ## nolint (variable not used)
                l <- sub("[ ]*Logging[ \t]*start[ ]*", "", line)
                measurementStart <- as.POSIXct(strptime(l, "%y/%m/%d %H:%M:%S", tz=tz))
            }
            ## "Logging end" would seem to be the sensible thing to examine,
            ## but "Logger time" seems correct in SLEIWEX 2008 data.  I think
            ## the issue is that the devices were turned off manually, and
            ## that time (the relevant one) is in "Logger time".
            ##OLD if (0 < (r<-regexpr("Logging[ \t]*end", line))) {
            ##OLD    l <- sub("[ ]*Logging[ \t]*end[ ]*", "", line)
            ##OLD    measurementEnd <- as.POSIXct(strptime(l,"%y/%m/%d %H:%M:%S", tz=tz))
            ##OLD }
            if (0 < (r<-regexpr("Logger[ \t]*time", line))) {
                l <- sub("[ ]*Logger[ \t]*time[ ]*", "", line)
                measurementEnd <- as.POSIXct(strptime(l, "%y/%m/%d %H:%M:%S", tz=tz))
            }
            if (0 < (r<-regexpr("Sample[ \t]*period", line))) {
                l <- sub("[ ]*Sample[ \t]*period[ ]*", "", line)
                sp <- as.numeric(strsplit(l, ":")[[1]])
                measurementDeltat <- (sp[3] + 60 * (sp[2] + 60*sp[1]))
            }
        }
        oceDebug(debug, "measurementStart =", format(measurementStart), "\n")
        oceDebug(debug, "measurementEnd =", format(measurementEnd), "\n")
        oceDebug(debug, "measurementDeltat  =", measurementDeltat, "\n")
        serialNumber <- strsplit(header[1], "[\t ]+")[[1]][4]
        oceDebug(debug, "serialNumber=", serialNumber, "\n")
        ## Now that we know the logging times, we can work with 'from 'and 'to'
        if (inherits(from, "POSIXt") || inherits(from, "character")) {
            if (!inherits(to, "POSIXt") && !inherits(to, "character"))
                stop("if 'from' is POSIXt or character, then 'to' must be, also")
            if (to <= from)
                stop("cannot have 'to' <= 'from'")
            from <- as.numeric(difftime(as.POSIXct(from, tz=tz), measurementStart, units="secs")) / measurementDeltat
            oceDebug(debug, "inferred from =", format(from, width=7), " based on 'from' arg", from.keep, "\n")
            to <- as.numeric(difftime(as.POSIXct(to, tz=tz), measurementStart, units="secs")) / measurementDeltat
            oceDebug(debug, "inferred   to =",   format(to, width=7), " based on   'to' arg", to.keep, "\n")
        } else {
            if (from < 1)
                stop("cannot have 'from' < 1")
            if (!missing(to) && to < from)
                stop("cannot have 'to' < 'from'")
        }
        oceDebug(debug, "by=", by, "in argument list\n")
        by <- ctimeToSeconds(by)
        oceDebug(debug, "inferred by=", by, "s\n")
        ##col.names <- strsplit(gsub("[ ]+"," ", gsub("[ ]*$","",gsub("^[ ]+","",line))), " ")[[1]]
        ## Read a line to determine if there is a pair of columns for time
        line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
        pushBack(line, file)
        line <- gsub("[ ]+$", "", gsub("^[ ]+", "", line))
        nvar <- length(strsplit(line, "[ ]+")[[1]])
        oceDebug(debug, " data line '", line, "' reveals ", nvar, " data per line\n", sep="")
        d <- scan(file, character(), quiet=TRUE) # read whole file (it's too tricky to bisect times with text data)
        n <- length(d) / nvar
        oceDebug(debug, "file has", length(d), "items; assuming", nvar, "items per line, based on first line\n")
        dim(d) <- c(nvar, n)
        if (nvar == 2) {
            time <- measurementStart + seq(from=0, to=n-1) * measurementDeltat
            Tcol <- 1
            pcol <- 2
        } else if (nvar == 4) {
            ## This time conversion is the slowest part of this function.  With R 2.13.0a working on
            ## a 620524-long vector: strptime() took 24s on a particular machine, and
            ## as.POSIXct() took 104s.  So, use strptime(), if the first time seems
            ## to be in a stanadard format.
            if (1 == length(grep("[0-9]{4}/[0-3][0-9]/[0-3][0-9]", d[1, 1])))
                time <- strptime(paste(d[1, ], d[2, ]), format="%Y/%m/%d %H:%M:%S", tz=tz)
            else
                time <- as.POSIXct(paste(d[1, ], d[2, ]), tz=tz)
            Tcol <- 3
            pcol <- 4
        } else if (nvar == 5) {
            ## 2008/06/25 10:00:00   18.5260   10.2225    0.0917
            if (1 == length(grep("[0-9]{4}/[0-3][0-9]/[0-3][0-9]", d[1, 1])))
                time <- strptime(paste(d[1, ], d[2, ]), format="%Y/%m/%d %H:%M:%S", tz=tz)
            else
                time <- as.POSIXct(paste(d[1, ], d[2, ]), tz=tz)
            Tcol <- 3
            pcol <- 4
        } else
            stop("wrong number of variables; need 2, 4, or 5, but got ", nvar)    ## subset

        ## subset times
        if (inherits(from, "POSIXt") || inherits(from, "character")) {
            keep <- from <= time & time <= to # FIXME: from may be int or time
        } else {
            if (missing(to))
                look <- from:n
            else
                look <- from:to
        }
        oceDebug(debug, "will be skipping time with seq(..., by=", by, ")\n")
        look <- seq.int(1, dim(d)[2], by=by)
        time <- time[look]
        temperature <- as.numeric(d[Tcol, look])
        pressure <- as.numeric(d[pcol, look])
        model <- ""
        res <- as.rsk(time, columns=list(temperature=temperature, pressure=pressure),
                       instrumentType="rbr",
                       serialNumber=serialNumber, model=model,
                       filename=filename,
                       debug=debug-1)
    }
    if (is.logical(patm)) {
        if (patm) {
            res@data$pressureOriginal <- res@data$pressure
            res@data$pressure <- res@data$pressure - 10.1325
            ## No need to check patm=FALSE case because object default is "absolute"
            res@metadata$pressureType <- "sea"
        }
    } else if (is.numeric(patm)) {
        res@data$pressureOriginal <- res@data$pressure
        res@data$pressure <- res@data$pressure - patm[1]
        res@metadata$pressureType <- "sea"
    } else {
        stop("patm must be logical or numeric")
    }
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    res@processingLog <- processingLogAppend(res@processingLog, processingLog)
    oceDebug(debug, "} # read.rsk()\n", sep="", unindent=1)
    res
}


#' Create a ctd Object from an rsk Object
#'
#' A new `ctd` object is assembled from the contents of the `rsk` object.
#' The data and metadata are mostly unchanged, with an important exception: the
#' `pressure` item in the `data` slot may altered, because `rsk`
#' instruments measure total pressure, not sea pressure; see \dQuote{Details}.
#'
#' The `pressureType` element of the
#' `metadata` of `rsk` objects defines the pressure type, and this controls
#' how pressure is set up in the returned object. If `object@@metadata$pressureType`
#' is `"absolute"` (or `NULL`) then the resultant pressure will be adjusted
#' to make it into `"sea"` pressure. To do this, the value of
#' `object@@metadata$pressureAtmospheric` is inspected. If this is present, then
#' it is subtracted from `pressure`. If this is missing, then
#' standard pressure (10.1325 dbar) will be subtracted. At this stage, the
#' pressure should be near zero at the ocean surface, but some additional adjustment
#' might be necessary, and this may be indicated by setting the argument `pressureAtmospheric` to
#' a non-zero value to be subtracted from pressure.
#'
#' @param x an [rsk-class] object.
#'
#' @param pressureAtmospheric A numerical value (a constant or a vector),
#' that is subtracted from the pressure in `object` before storing it in the return value.
#'
#' @param longitude numerical value of longitude, in degrees East.
#'
#' @param latitude numerical value of latitude, in degrees North.
#'
#' @param ship optional string containing the ship from which the observations were made.
#'
#' @param cruise optional string containing a cruise identifier.
#'
#' @param station optional string containing a station identifier.
#'
#' @param deploymentType character string indicating the type of deployment (see
#' [as.ctd()]).
#'
#' @template debugTemplate
rsk2ctd <- function(x, pressureAtmospheric=0, longitude=NULL, latitude=NULL,
                    ship=NULL, cruise=NULL, station=NULL, deploymentType=NULL,
                    debug=getOption("oceDebug"))
{
    oceDebug(debug, "rsk2ctd(...) {\n", sep="", unindent=1)
    res <- new("ctd")
    res@metadata <- x@metadata
    ## The user may have already inserted some metadata, even if read.rsk() didn't, so
    ## we have to take care of two cases in deciding on some things. The procedure is
    ## to use the argument to rsk2ctd if one is given, otherwise to use the value already
    ## in x@metadata, otherwise to set a default that matches as.ctd().
    res@metadata$longitude <- if (!is.null(longitude)) longitude else
        if (is.null(res@metadata$longitude)) NA else res@metadata$longitude
    res@metadata$latitude <- if (!is.null(latitude)) latitude else
        if (is.null(res@metadata$latitude)) NA else res@metadata$latitude
    res@metadata$ship <- if (!is.null(ship)) ship else
        if (is.null(res@metadata$ship)) "" else res@metadata$ship
    res@metadata$cruise <- if (!is.null(cruise)) cruise else
        if (is.null(res@metadata$cruise)) "" else res@metadata$cruise
    res@metadata$station <- if (!is.null(station)) station else
        if (is.null(res@metadata$station)) "" else res@metadata$station
    res@metadata$deploymentType <- if (!is.null(deploymentType)) deploymentType else
        if (is.null(res@metadata$deploymentType)) "unknown" else res@metadata$deploymentType

    ## We start by copying the data, but we may need to do some fancy footwork for pressure, because
    ## RBR devices store absolute pressure, not the sea pressure that we have in CTD objects.
    res@data <- x@data
    if (!("pressure" %in% names(res@data)))
        stop("there is no pressure in this rsk object, so it cannot be converted to a ctd object")
    pressureAtmosphericStandard <- 10.1325
    if (is.null(x@metadata$pressureType)) {
        oceDebug(debug, "metadata$pressureType is NULL so guessing absolute pressure: be on the lookout for problems, if not\n")
        warning("rsk object lacks metadata$pressureType; assuming absolute and subtracting standard atm pressure to get sea pressure")
        res@data$pressure <- x@data$pressure - pressureAtmosphericStandard
        res@metadata$units$pressure$scale <- "sea"
        res@metadata$dataNamesOriginal[substr(res@metadata$dataNamesOriginal, 1, 4) == "pres"] <- ""
        res@processingLog <- processingLogAppend(res@processingLog,
                                                 paste("subtracted 10.1325dbar (std atm) from pressure\n"))
    } else {
        ## subtract atm pressure, if it has not already been subtracted
        oceDebug(debug, "metadata$pressureType is not NULL\n")
        if ("sea" != substr(x@metadata$pressureType, 1, 3)) {
            oceDebug(debug, "must convert from absolute pressure to sea pressure\n")
            if (!("pressureAtmospheric" %in% names(x@metadata))) {
                oceDebug(debug, "pressure is 'absolute'; subtracting std atm 10.1325 dbar\n")
                res@data$pressure <- x@data$pressure - pressureAtmosphericStandard
                res@metadata$units$pressure$scale <- "sea"
                res@metadata$dataNamesOriginal[substr(res@metadata$dataNamesOriginal, 1, 4) == "pres"] <- ""
                res@processingLog <- processingLogAppend(res@processingLog,
                                                         paste("subtracted", pressureAtmosphericStandard, "dbar (std atm) from absolute pressure to get sea pressure"))
                oceDebug(debug, "subtracted std atm pressure from pressure\n")
            } else {
                res@data$pressure <- x@data$pressure - x@metadata$pressureAtmospheric
                res@metadata$units$pressure$scale <- "sea"
                res@metadata$dataNamesOriginal[substr(res@metadata$dataNamesOriginal, 1, 4) == "pres"] <- ""
                res@processingLog <- processingLogAppend(res@processingLog,
                                                         paste("subtracted",
                                                               x@metadata$pressureAtmospheric,
                                                               "dbar from absolute pressure to get sea pressure"))
                oceDebug(debug, "subtracted", x@metadata$pressureAtmospheric, "dbar from pressure\n")
            }
        }
    }
    ## Now we have sea pressure (if the rsk was set up correctly for the above to work right),
    ## so we can adjust a second time, if the user changed from the default of pressureAtmospheric=0.
    if (pressureAtmospheric[1] != 0) {
        res@data$pressure <- res@data$pressure - pressureAtmospheric
        oceDebug(debug, "subtracted", pressureAtmospheric, "dbar from pressure")
    }
    res@processingLog <- processingLogAppend(res@processingLog,
                                             paste("subtracted",
                                                   pressureAtmospheric, "dbar from sea pressure"))
    if (!("salinity" %in% names(x@data))) {
        C <- x[["conductivity"]]
        if (is.null(C))
            stop("objects must have salinity or conductivity to be converted to CTD form")
        unit <- as.character(x@metadata$units$conductivity$unit)
        if (0 == length(unit)) {
            S <- swSCTp(x[["conductivity"]], x[["temperature"]], res[["pressure"]])
            res@processingLog <- processingLogAppend(res@processingLog, "calculating salinity based on conductivity in (assumed) ratio units")
        } else if (unit == "uS/cm") {
            S <- swSCTp(x[["conductivity"]]/42914.0, x[["temperature"]], res[["pressure"]])
            res@processingLog <- processingLogAppend(res@processingLog, "calculating salinity based on conductivity in uS/cm")
        } else if (unit == "mS/cm") {
            S <- swSCTp(x[["conductivity"]]/42.914, x[["temperature"]], res[["pressure"]])
            res@processingLog <- processingLogAppend(res@processingLog, "calculating salinity based on conductivity in mS/cm")
        } else if (unit == "S/m") {
            S <- swSCTp(x[["conductivity"]]/4.2914, x[["temperature"]], res[["pressure"]])
            res@processingLog <- processingLogAppend(res@processingLog, "calculating salinity based on conductivity in S/m")
        } else {
            stop("unrecognized conductivity unit '", unit, "'; only uS/cm, mS/cm and S/m are handled")
        }
        ## res <- ctdAddColumn(res, column=S, name="salinity", label="Salinity",
        ##                     unit=list(unit=expression(), scale="PSS-78"))
        res <- oceSetData(res, name="salinity", value=S,
                          unit=list(unit=expression(), scale="PSS-78"))

    }
    oceDebug(debug, "} # rsk2ctd()\n", sep="", unindent=1)
    res@processingLog <- processingLogAppend(res@processingLog,
                                             paste("rsk2ctd(..., pressureAtmospheric=", pressureAtmospheric, ", debug)\n",
                                                   sep="", collapse=""))
    res
}


#' Estimate Atmospheric Pressure in Rsk Object
#'
#' Estimate atmospheric pressure in rsk record.
#'
#' Pressures must be in decibars for this to work.  First, a subset of pressures is
#' created, in which the range is `sap-dp` to `sap+dp`.  Here,
#' `sap`=10.1325 dbar is standard sealevel atmospheric pressure.  Within this
#' window, three measures of central tendency are calculated: the median, the mean,
#' and a weighted mean that has weight given by \eqn{exp(-2*((p
#'     - sap) / dp)^2)}{exp(-2*((p - sap) / dp)^2)}.
#'
#' @param x an [rsk-class] object.
#'
#' @param dp Half-width of pressure window to be examined (in decibars).
#'
#' @return A list of four estimates: `sap`, the median, the mean, and the weighted mean.
#'
#' @seealso
#' The documentation for [rsk-class] explains the structure of
#' `rsk` objects, and also outlines the other functions dealing with them.
#'
#' @examples
#' library(oce)
#' data(rsk)
#' print(rskPatm(rsk))
#'
#' @author Dan Kelley
#'
#' @family things related to rsk data
rskPatm <- function(x, dp=0.5)
{
    p <- if (inherits(x, "rsk")) x@data$pressure else x
    sap <- 10.1325                      # standard atm pressure
    if (length(p) < 1)
        return(rep(sap, 4))
    p <- p[(sap - dp) <= p & p <= (sap + dp)] # window near sap
    w <- exp(-2 * ( (p - sap) / dp)^2)
    if (length(p) < 4)
        rep(sap, 4)
    else
        c(sap, median(p), mean(p), weighted.mean(p, w))
}


#' Decode table-of-contents File from a Rsk File
#'
#' Decode table-of-contents file from a rsk file, of the sort used by some
#' researchers at Dalhousie University.
#'
#' It is assumed that the `.TBL` file contains lines of the form \code{"File
#'   \\day179\\SL08A179.023 started at Fri Jun 27 22:00:00 2008"} The first step is
#' to parse these lines to get day and hour information, i.e.  179 and 023 in the
#' line above.  Then, recognizing that it is common to change the names of such
#' files, the rest of the file-name information in the line is ignored, and instead
#' a new file name is constructed based on the data files that are found in the
#' directory.  (In other words, the `"\\day179\\SL08A"` portion of the line is
#' replaced.)  Once the file list is complete, with all times put into R format,
#' then (optionally) the list is trimmed to the time interval indicated by
#' `from` and `to`.  It is important that `from` and `to` be in
#' the `UTC` time zone, because that time zone is used in decoding the lines
#' in the `.TBL` file.
#'
#' @param dir name of a directory containing a single table-of-contents file, with
#' `.TBL` at the end of its file name.
#'
#' @param from optional [POSIXct()] time, indicating the beginning of a
#' data interval of interest.  This must have timezone `"UTC"`.
#'
#' @param to optional [POSIXct()] time, indicating the end of a data
#' interval of interest.  This must have timezone `"UTC"`.
#'
#' @param debug optional integer to control debugging, with positive values
#' indicating to print information about the processing.
#'
#' @examples
#'\dontrun{
#' table <- rskToc("/data/archive/sleiwex/2008/moorings/m05/adv/sontek_202h/raw",
#' from=as.POSIXct("2008-07-01 00:00:00", tz="UTC"),
#'     to=as.POSIXct("2008-07-01 12:00:00", tz="UTC"))
#' print(table)
#'}
#'
#' @return
#' A list with two elements: `filename`, a vector of file names, and
#' `startTime`, a vector of [POSIXct()] times indicating the (real)
#' times of the first datum in the corresponding files.
#'
#' @author Dan Kelley
#'
#' @family things related to rsk data
rskToc <- function(dir, from, to, debug=getOption("oceDebug"))
{
    if (missing(dir))
        stop("need a 'dir', naming a directory containing a file with suffix .TBL, and also data files named in that file")
    tbl.files <- list.files(path=dir, pattern="*.TBL$")
    if (length(tbl.files) < 1)
        stop("could not locate a .TBL file in direcory ", dir)
    tref <- as.POSIXct("2010-01-01", tz="UTC") # arbitrary time, to make integers
    file.code <- NULL
    startTime <- NULL
    for (tbl.file in tbl.files) {
        oceDebug(debug, tbl.file)
        lines <- readLines(paste(dir, tbl.file, sep="/"))
        if (length(lines) < 1)
            stop("found no data in file ", paste(dir, tbl.file, sep="/"))
        ## "File \\day179\\SL08A179.023 started at Fri Jun 27 22:00:00 2008"
        for (line in lines) {
            s <- strsplit(line, "[ \t]+")[[1]]
            if (length(s) > 2) {
                filename <- s[2]
                month <- s[6]
                day <- s[7]
                hms <- s[8]
                year <- s[9]
                t <- as.POSIXct(strptime(paste(year, month, day, hms), "%Y %b %d %H:%M:%S", tz="UTC"))
                len <- nchar(filename)
                code <- substr(filename, len-6, len)
                oceDebug(debug, s, "(", code, format(t), ")\n")
                file.code <- c(file.code, code)
                startTime <- c(startTime, as.numeric(t) - as.numeric(tref))
            }
        }
    }
    prefix <- list.files(dir, pattern=".*[0-9]$")[1]
    lprefix <- nchar(prefix)
    prefix <- substr(prefix, 1, lprefix-7)
    filename <- paste(dir, paste(prefix, file.code, sep=""), sep="/")
    startTime <- as.POSIXct(startTime + tref)
    oceDebug(debug, "from=", format(from), "\n")
    oceDebug(debug, "to=", format(to), "\n")
    if (!missing(from) && !missing(to)) {
        oceDebug(debug, "got", length(file.code), "candidate files")
        ok <- from <= startTime & startTime <= to
        oceDebug(debug, "ok=", ok, "\n")
        filename <- filename[ok]
        startTime <- startTime[ok]
        oceDebug(debug, "taking into account the times, ended up with", length(file.code), "files\n")
    }
    list(filename=filename, startTime=startTime)
}


## remove 2015-07-09
## rskTrim <- function(x, method="water", parameters=NULL, debug=getOption("oceDebug"))
## {
##     oceDebug(debug, "rskTrim() {\n", unindent=1)
##     if (!inherits(x, "rsk"))
##         stop("method is only for objects of class '", "rsk", "'")
##     res <- x
##     n <- length(x@data$temperature)
##     oceDebug(debug, "dataset has", n, "points\n")
##     if (n < 2) {
##         warning("too few data to trim rsk record")
##     } else {
##         which.method <- pmatch(method, c("water", "time", "index"), nomatch=0)
##         oceDebug(debug, "using method", which.method, "\n")
##         if (which.method == 1) {        # "water"
##             keep <- rep(FALSE, n)
##             air <- x@data$pressure < 10.5 # NB. standard pressure is 10.1325
##             waterIndices <- which(!air)
##             b <- 2                      # trim a few descending points
##             i.start <- waterIndices[1] + b
##             i.stop <- waterIndices[-b + length(waterIndices)]
##             keep[i.start:i.stop] <- TRUE
##         } else if (which.method == 2) { # "time"
##             oceDebug(debug, "trimming to time range ",as.character(parameters[1])," to ", as.character(parameters[2]), "\n")
##             keep <- rep(TRUE, n)
##             keep[x@data$time < as.POSIXlt(parameters[1])] <- FALSE
##             keep[x@data$time > as.POSIXlt(parameters[2])] <- FALSE
##         } else if (which.method == 3) { # "index"
##             oceDebug(debug, "parameters:",parameters,"\n")
##             if (min(parameters) < 1)
##                 stop("Cannot select indices < 1");
##             if (max(parameters) > n)
##                 stop(paste("Cannot select past end of array, i.e. past ", n))
##             keep <- rep(FALSE, n)
##             keep[parameters[1]:parameters[2]] <- TRUE
##         } else {
##             stop("Unknown method")
##         }
##     }
##     for (name in names(x@data))
##         res@data[[name]] <- subset(x@data[[name]], keep)
##     res@data$pressure <- res@data$pressure - 10.1325 # remove avg sealevel pressure
##     res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
##     oceDebug(debug, "} # rskTrim()\n", unindent=1)
##     res
## }
