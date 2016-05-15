## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' @title Class to Store Rsk Data
#' 
#' @description
#' Class for data stored in the ``Ruskin'' format used by RBR [1], including both
#' \code{rsk} SQLite files and the ASCII \code{txt} exported files.
#' 
#' A \code{rsk} object may be read with \code{\link{read.rsk}} or created with
#' \code{\link{as.rsk}}.  Plots can be made with \code{\link{plot,rsk-method}}, while
#' \code{\link{summary,rsk-method}} produces statistical summaries and \code{show}
#' produces overviews.   If atmospheric pressure has not been removed from the
#' data, the functions \code{\link{rskPatm}} may provide guidance as to its value;
#' however, this last function is no equal to decent record-keeping at sea.  Data
#' may be retrieved with \code{\link{[[,rsk-method}} or replaced with
#' \code{\link{[[<-,rsk-method}}.
#' 
#' @references
#' 1. \href{http://www.rbr-global.com/products}{RBR website: www.rbr-global.com/products}
#' 
#' @author Dan Kelley and Clark Richards
#' 
#' @family classes provided by \code{oce}
#' @family things related to \code{rsk} data
setClass("rsk", contains="oce")

#' @title Sample Rsk Dataset
#'
#' @description
#' A sample \code{rsk} object derived from a Concerto CTD manufactured by RBR Ltd.
#'
#' @details The data were obtained September 2015, off the west coast
#'     of Greenland, by Matt Rutherford and Nicole Trenholm of the
#'     Ocean Research Project, in collaboration with RBR and with the
#'     NASA Oceans Melting Greenland project.
#'
#' @name rsk
#' @docType data
#' @references \url{https://rbr-global.com/}
#' @examples
#' library(oce)
#' data(rsk)
#' plot(rsk)
#' plot(as.ctd(rsk))
#' plot(subset(as.ctd(rsk),pressure<10))
#'
#' @family datasets provided with \code{oce}
#' @family things related to \code{rsk} data
NULL


setMethod(f="initialize",
          signature="rsk",
          definition=function(.Object,time,pressure,temperature,filename="") {
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
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'rsk' object"
              return(.Object)
          })

#' @title Summarize a Rsk Object
#' 
#' @description
#' Summarizes some of the data in a \code{rsk} object, presenting such information
#' as the station name, sampling location, data ranges, etc.
#'
#' @param object An object of class \code{"rsk"}, usually, a result of a call to
#' \code{\link{read.rsk}}, \code{\link{read.oce}}, or \code{\link{as.rsk}}.
#' 
#' @param ... Further arguments passed to or from other methods.
#' 
#' @seealso The documentation for \code{\link{rsk-class}} explains the structure
#' of CTD objects, and also outlines the other functions dealing with them.
#' 
#' @examples
#' library(oce)
#' data(rsk)
#' summary(rsk)
#' 
#' @author Dan Kelley
#' 
#' @family things related to \code{rsk} data
setMethod(f="summary",
          signature="rsk",
          definition=function(object, ...) {
              m <- object@metadata
              mnames <- names(m)
              cat("rsk summary\n-----------\n", ...)
              cat("* Instrument:         model ", m$model,
                  " serial number " , m$serialNumber, "\n", sep='')
              if ("pressureAtmospheric" %in% mnames)
                  cat(paste("* Atmosph. pressure:  ", m$pressureAtmospheric, "\n", sep=""))
              if ("pressureType" %in% mnames)
                  cat(paste("* Pressure type:      ", m$pressureType, "\n", sep=""))
              cat(paste("* Source:             ``", m$filename, "``\n", sep=""))
              callNextMethod()
          })

#' @title Extract Something From a Rsk Object
#' @param x A rsk object, i.e. one inheriting from \code{\link{rsk-class}}.
#' @template sub_subTemplate
#' @family things related to \code{rsk} data
setMethod(f="[[",
          signature(x="rsk", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              callNextMethod()
          })

#' @title Replace Parts of a Rsk Object
#' @param x An \code{rsk} object, i.e. inheriting from \code{\link{rsk-class}}
#' @template sub_subsetTemplate
#' @family things related to \code{rsk} data
setMethod(f="[[<-",
          signature(x="rsk", i="ANY", j="ANY"),
          definition=function(x, i, j, value) {
              callNextMethod(x=x, i=i, j=j, value=value)
          })



#' @title Subset a Rsk Object
#' 
#' @description
#' Subset a rsk object.  This function is somewhat analogous to
#' \code{\link{subset.data.frame}}, but subsetting is only permitted by time.
#' 
#' @param x a \code{rsk} object, i.e. inheriting from \code{\link{rsk-class}}.
#' 
#' @param subset a condition to be applied to the \code{data} portion of \code{x}.
#' See \sQuote{Details}.
#' 
#' @param \dots ignored.
#' 
#' 
#' @return
#' A new \code{rsk} object.
#' 
#' @examples
#' library(oce)
#' data(rsk)
#' plot(rsk)
#' plot(subset(rsk, time < mean(range(rsk[['time']]))))
#' 
#' @author Dan Kelley
#' @family things related to \code{rsk} data
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
                  r <- eval(substitute(subset), x@data, parent.frame(2))
                  ####  str(r)
                  r <- r & !is.na(r)
                  res@data[[i]] <- x@data[[i]][r]
              }
              names(res@data) <- names(x@data)
              subsetString <- paste(deparse(substitute(subset)), collapse=" ")
              res@processingLog <- processingLogAppend(res@processingLog, paste("subset.rsk(x, subset=", subsetString, ")", sep=""))
              res
          })

#' Infer Rsk units from a vector of strings
#'
#' This is used by \code{\link{read.rsk}} to infer the units of data, based
#' on strings stored in \code{.rsk} files. Lacking a definitive guide
#' to the format of these file, this function was based on visual inspection
#' of the data contained within a few sample files; unusual sensors may
#' not be handled properly.
#'
#' @param s Vector of character strings, holding the `units` entry in the
#' \code{channels} table of the \code{.rsk} database.
#'
#' @return List of unit lists.
#' @family functions that interpret variable names and units from headers
unitFromStringRsk <- function(s)
{
    if (1 == length(grep("mg/L", s, useBytes=TRUE))) # guessing, from RBR docs
        list(unit=expression(mg/l), scale="")
    else if (1 == length(grep("mL/L", s, useBytes=TRUE))) # guessing, from RBR docs
        list(unit=expression(ml/l), scale="")
    else if (1 == length(grep("\xc2\xb5Mol/L", s, useBytes=TRUE))) # guessing, from RBR docs
        list(unit=expression(mu*mol/l), scale="")
    else if (1 == length(grep("mS/cm", s, useBytes=TRUE)))
        list(unit=expression(mS/cm), scale="")
    else if (1 == length(grep("uS/cm", s, useBytes=TRUE)))
        list(unit=expression(mu*S/cm), scale="")
    else if (1 == length(grep("d[bB]ar", s, useBytes=TRUE)))
        list(unit=expression(dbar), scale="")
    else if (1 == length(grep("NTU", s, useBytes=TRUE)))
        list(unit=expression(NTU), scale="")
    else if (1 == length(grep("\xB0", s, useBytes=TRUE)))
        list(unit=expression(degree), scale="ITS-90") # guessing on scale
    else {
        warning("'", s, "' is not in the list of known .rsk units", sep="")
        list(unit=as.expression(s), scale="")
    }
}

#' @title Coerce Data Into a Rsk Object
#' 
#' @description
#' Create a rsk object.
#' 
#' @details
#' The contents of \code{columns} are be copied into the \code{data} slot
#' of the returned object directly, so it is critical that the names and units
#' correspond to those expected by other code dealing with
#' \code{\link{rsk-class}} objects. If there is a conductivity, it must be called
#' \code{conductivity}, and it must be in units of mS/cm. If there is a
#' temperature, it must be called \code{temperature}, and it must be an in-situ
#' value recorded in ITS-90 units.  And if there is a pressure, it must be
#' \emph{absolute} pressure (sea pressure plus atmospheric pressure) and it must
#' be named \code{pressure}. No checks are made within \code{as.rsk} on any of
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
#' @param model instrument model type, e.g. \code{"RBRduo"}.
#' 
#' @param sampleInterval sampling interval. If given as \code{NA}, then this is
#' estimated as the median difference in times.
#' 
#' @param debug a flag that can be set to \code{TRUE} to turn on debugging.
#' 
#' @return
#' An object of \code{\link{rsk-class}} \code{"rsk"}.
#' 
#' @author Dan Kelley
#' @family things related to \code{rsk} data
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


#' @title Plot Rsk Data
#' 
#' @description
#' Rsk data may be in many forms, and it is not easy to devise a general plotting
#' strategy for all of them. The present function is quite crude, on the
#' assumption that users will understand their own datasets, and that they can
#' devise plots that are best-suited to their applications.  Sometimes, the
#' sensible scheme is to coerce the object into another form, e.g. using
#' \code{plot(as.ctd(rsk))} if the object contains CTD-like data.  Other times,
#' users should extract data from the \code{rsk} object and construct plots
#' themselves. The idea is to use the present function mainly to get an overview,
#' and for that reason, the default plot type (set by \code{which}) is a set of
#' time-series plots, because the one thing that is definitely known about
#' \code{rsk} objects is that they contain a \code{time} vector in their
#' \code{data} slot.
#' 
#' @details
#' Several plots are available.  \itemize{
#'     \item \code{which=0} or \code{"timeseries"} for time-series plots of each variable;
#'     this over-rides any other specification
#'     \item \code{which=1} or \code{"temperature"} for a time-series plot of temperature
#'     \item \code{which=2} or \code{"text"} for textual information about the dataset
#'     \item \code{which=3} or \code{"pressure"} for a time-series plot of pressure
#'     \item \code{which=4} or \code{"profile"} for a temperature profile
#' }
#' 
#' @param x \code{rsk} object, typically result of \code{\link{read.rsk}}.
#' 
#' @param which list of desired plot types.  These are graphed in panels running
#' down from the top of the page.  See \dQuote{Details} for the meanings of
#' various values of \code{which}.
#' 
#' @param title character string to be used in the text-summary panel
#' (\code{which}=2).
#' 
#' @param adorn list of expressions to be executed for the panels in turn, e.g. to
#' adorn the plots.  If the number matches the number of panels, then the strings
#' are applied to the appropriate panels, as they are drawn from top-left to
#' bottom-right.   If only a single expression is provided, it is used for all
#' panels.  (See \dQuote{Examples}).
#' 
#' @param tlim optional limits for time axis.  If not provided, the value will be
#' inferred from the data.
#' 
#' @param plim optional limits for pressure axis.  If not provided, the value will
#' be inferred from the data.  (It is helpful to specify this, if the auto-scaled
#' value will be inappropriate, e.g. if more lines are to be added later.)
#' 
#' @param Tlim optional limits for temperature axis.  If not provided, the value
#' will be inferred from the data.  (It is helpful to specify this, if the
#' auto-scaled value will be inappropriate, e.g. if more lines are to be added
#' later.)
#' 
#' @param xlab optional label for x axis.
#' 
#' @param ylab optional label for y axis.
#' 
#' @param tformat optional argument passed to \code{\link{oce.plot.ts}}, for plot
#' types that call that function.  (See \code{\link{strptime}} for the format
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
#' @param useSmoothScatter a boolean to cause \code{\link{smoothScatter}} to be
#' used for profile plots, instead of \code{\link{plot}}.
#' 
#' @param mgp 3-element numerical vector to use for \code{par(mgp)}, and
#' also for \code{par(mar)}, computed from this.  The default is tighter than the
#' R default, in order to use more space for the data and less for the axes.
#' 
#' @param mar value to be used with \code{\link{par}("mar")}.
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
#' plot(rsk, which=c(1,3))
#' 
#' 
#' @seealso
#' The documentation for \code{\link{rsk-class}} explains the structure of
#' \code{rsk} objects, and also outlines the other functions dealing with them.
#' 
#' @author Dan Kelley
#'
#' @family functions that plot \code{oce} data
#' @family things related to \code{rsk} data
setMethod(f="plot",
          signature=signature("rsk"),
          ##definition=function(x, which=c(1, 3, 4), title="", adorn=NULL,
          definition=function(x, which="timeseries", title="", adorn=NULL,
                              tlim, plim, Tlim,
                              xlab, ylab,
                              tformat,
                              drawTimeRange=getOption("oceDrawTimeRange"),
                              abbreviateTimeRange=getOption("oceAbbreviateTimeRange"),
                              useSmoothScatter=FALSE,
                              mgp=getOption("oceMgp"),
                              mar=c(mgp[1]+1.5,mgp[1]+1.5,1.5,1.5),
                              main="",
                              debug=getOption("oceDebug"),
                              ...)
          {
              oceDebug(debug, "plot.rsk(..., which=", which, ", ...) {\n", unindent=1)
              if (!inherits(x, "rsk"))
                  stop("method is only for objects of class '", "rsk", "'")
              dotsNames <- names(list(...))
              ## FIXME: In the below, we could be more clever for single-panel plots
              ## but it may be better to get users out of the habit of supplying xlim
              ## etc (which will yield errors in plot.lm(), for example).
              if ("xlim" %in% dotsNames)
                  stop("in plot.rsk() : 'xlim' not allowed; use tlim (for type=1 or 3) or Tlim (for type=4) ", call.=FALSE)
              if ("ylim" %in% dotsNames)
                  stop("in plot.rsk() : 'ylim' not allowed; use Tlim (for type=1 or 4) or plim (for type=3) ", call.=FALSE)
              whichOk <- c("timeseries", "temperature", "text", "pressure", "profile")
              whichNew <- oce.pmatch(which, list(timeseries=0, temperature=1, text=2, pressure=3, profile=4))
              if (any(is.na(whichNew))) stop("plot.rsk(..., which=\"", which, "\") not understood; try one of: ", paste(whichOk, collapse=" "), call.=FALSE)
              which <- whichNew # now it's numeric
              if (any(which==0))
                  which <- 0 # "timeseries" overrides any others
              opar <- par(no.readonly = TRUE)
              on.exit(par(opar))
              lw <- length(which)
              if (lw == 1 && which==0) {
                  names <- names(x@data)
                  if (!"time" %in% names) stop("plot.rsk() cannot plot timeseries, since no \"time\" data", call.=FALSE)
                  names <- names[names != "time"]
                  par(mfrow=c(length(names), 1))
                  for (name in names) {
                      oce.plot.ts(x[["time"]], x[[name]], ylab=name, ...)
                  }
              } else {
                  ## individual panels
                  ## Trim out plots that we cannot do.
                  names <- names(x@data)
                  haveTemperature <- ("temperature" %in% names) && any(is.finite(x@data$temperature))
                  havePressure <- ("pressure" %in% names) && any(is.finite(x@data$pressure))
                  if (!haveTemperature) 
                      which <- which[which != 1 & which != 4]
                  if (!havePressure) 
                      which <- which[which != 3 & which != 4]
                  nw <- length(which)
                  opar <- par(no.readonly = TRUE)
                  if (nw > 1)
                      on.exit(par(opar))
                  adorn.length <- length(adorn)
                  if (adorn.length == 1) {
                      adorn <- rep(adorn, nw)
                      adorn.length <- nw
                  }
                  ## Old-style for pT sensors; others, just 
                  if (3 == length(which) && 1 %in% which && 3 %in% which && 4 %in% which)
                      layout(rbind(c(1,2), c(3,4)), widths=c(2,1))
                  else
                      layout(matrix(1:nw))
                  par(mgp=mgp, mar=mar)
                  if (missing(main))
                      main <- rep('', length.out=nw)
                  else
                      main <- rep(main, length.out=nw)
                  oceDebug(debug, "after nickname-substitution, which=c(", paste(which, collapse=","), ")\n")
                  for (w in 1:nw) {
                      oceDebug(debug, "which[", w, "]=", which[w], "\n")
                      if (which[w] == 1) {           # temperature timeseries
                          if (haveTemperature) {
                              oce.plot.ts(x@data$time, x@data$temperature,
                                          xlab=if (!missing(xlab))xlab else "",
                                          ylab=if (missing(ylab)) resizableLabel("T", "y") else ylab,
                                          type='l',
                                          xlim=if (missing(tlim)) range(x@data$time, na.rm=TRUE) else tlim,
                                          ylim=if (missing(Tlim)) range(x@data$temperature, na.rm=TRUE) else Tlim,
                                          tformat=tformat,
                                          drawTimeRange=drawTimeRange,
                                          mgp=mgp, mar=mar, main=main[w], ...)
                              drawTimeRange <- FALSE    # only the first time panel gets the time indication
                              axis(2)
                          }
                      } else if (which[w] == 3) {    # pressure timeseries
                          if (havePressure) {
                              oce.plot.ts(x@data$time, x@data$pressure,
                                          xlab=if (!missing(xlab))xlab else "",
                                          ylab=if (missing(ylab)) resizableLabel("p", "y") else ylab,
                                          type='l',
                                          xlim=if (missing(tlim)) range(x@data$time, na.rm=TRUE) else tlim,
                                          ylim=if (missing(plim)) range(x@data$pressure, na.rm=TRUE) else plim,
                                          tformat=tformat,
                                          drawTimeRange=drawTimeRange,
                                          mgp=mgp, mar=mar, main=main[w], ...)
                              drawTimeRange <- FALSE
                          }
                      } else if (which[w] == 2) {
                          textItem<-function(xloc, yloc, item, cex=4/5*par("cex"), d.yloc=0.8) {
                              if (!is.null(item) && !is.na(item))
                                  text(xloc, yloc, item, adj = c(0, 0), cex=cex);
                              yloc - d.yloc
                          }
                          xfake <- seq(0:10)
                          yfake <- seq(0:10)
                          mar <- par("mar")
                          par(mar=c(0,0,0,0))

                          plot(xfake, yfake, type = "n", xlab = "", ylab = "", axes = FALSE)
                          xloc <- 1
                          yloc <- 10
                          cex <- par("cex")
                          yloc <- textItem(xloc, yloc, title, cex=1.25*cex)
                          ##if (!is.null(object@metadata$filename))
                          ##    textItem(object@metadata$filename, cex=cex)
                          if (!is.null(x@metadata$serialNumber)) {
                              yloc <- textItem(xloc, yloc, paste(gettext("Serial Number", domain="R-oce"), x@metadata$serialNumber),cex=cex)
                          }
                          if (!(1 %in% which || 2 %in% which)) { # don't bother with these if already on a time-series panel
                              yloc <- textItem(xloc, yloc, paste("Start:", x@data$time[1], attr(x@data$time, "tzone")), cex=cex)
                              yloc <- textItem(xloc, yloc, paste("End:", x@data$time[length(x@data$time)], attr(x@data$time, "tzone")), cex=cex)
                              yloc <- textItem(xloc, yloc, paste("Sampled interval:", difftime(x@data$time[2], x@data$time[1], units="secs"), "s"),cex=cex)
                          }
                          par(mar=mar)
                      } else if (which[w] == 4) {     # "profile"
                          if (haveTemperature && havePressure) {
                              args <- list(x=x@data$temperature, y=x@data$pressure,
                                           xlab="",
                                           ylab=resizableLabel("p"),
                                           xlim=if (missing(Tlim)) range(x@data$temperature, na.rm=TRUE) else Tlim,
                                           ylim=if (missing(plim)) rev(range(x@data$pressure, na.rm=TRUE)) else plim,
                                           ...)
                              a <- names(list(...))
                              if (!("type" %in% a))
                                  args <- c(args, type="p")
                              if (!("cex"  %in% a))
                                  args <- c(args, cex=1/2)
                              if (!("axes" %in% a))
                                  args <- c(args, axes=FALSE)
                              ##np <- length(x@data$pressure)
                              if (nw == 1)
                                  par(mar=c(1, 3.5, 4, 1))
                              if (useSmoothScatter) {
                                  args <- args[names(args) != "type"]
                                  do.call(smoothScatter, args)
                              } else {
                                  do.call(plot, args)
                              }
                              box()
                              axis(2)
                              axis(3)
                              mtext(resizableLabel("T", "x"), side = 3, line = 2)
                          }
                      }
                      if (w <= adorn.length) {
                          t <- try(eval(adorn[w]), silent=TRUE)
                          if (class(t) == "try-error")
                              warning("cannot evaluate adorn[", w, "]\n")
                      }
                  }
              }
              oceDebug(debug, "} # plot.rsk()\n", unindent=1)
              invisible()
          })



#' @title Read a Rsk file
#' 
#' @description
#' Read an RBR rsk or txt file, e.g. as produced by an RBR logger, producing an
#' object of class \code{rsk}.
#' 
#' @param file a connection or a character string giving the name of the file to
#' load. Note that \code{file} must be a character string, because connections are
#' not used in that case, which is instead handled with database calls.
#' 
#' @param from indication of the first datum to read.  This can a positive integer
#' to indicate sequence number, the POSIX time of the first datum, or a character
#' string that can be converted to a POSIX time.  (For POSIX times, be careful
#' about the \code{tz} argument.)
#' 
#' @param to an indication of the last datum to be read, in the same format as
#' \code{from}.  If \code{to} is missing, data will be read to the end of the file.
#' 
#' @param by an indication of the stride length to use while walking through the
#' file.  If this is an integer, then \code{by-1} samples are skipped between each
#' pair of samples that is read.  If this is a string representing a time interval,
#' in colon-separated format (HH:MM:SS or MM:SS), then this interval is divided by
#' the sampling interval, to get the stride length.
#' 
#' @param type optional file type, presently can be \code{rsk} or \code{txt} (for a
#' text export of an RBR rsk or hex file). If this argument is not provided, an
#' attempt will be made to infer the type from the file name and contents.
#' 
#' @param tz time zone.  The value \code{oceTz} is set at package setup.
#' 
#' @param patm controls the handling of atmospheric pressure, an important issue
#' for RBR instruments that record absolute pressure; see \dQuote{Details}.
#' 
#' @param processingLog if provided, the action item to be stored in the log.
#' This is typically only provided for internal calls; the default that it provides
#' is better for normal calls by a user.
#' 
#' @param debug a flag that can be set to \code{TRUE} to turn on debugging.
#' 
#' 
#' @details
#' This can read files produced by several RBR instruments.  At the moment, five
#' styles are understood: (1) text file produced as an export of an RBR \code{hex}
#' or \code{rsk} file; (2) text file with columns for temperature and pressure
#' (with sampling times indicated in the header); (3) text file with four columns,
#' in which the date the time of day are given in the first two columns, followed
#' by the temperature, and pressure; (4) text file with five columns, in which
#' depth in the water column is given after the pressure; (5) an SQLite-based
#' database format. The first four options are provided mainly for historical
#' reasons, since RBR instruments at the date of writing commonly use the SQLite
#' format, though the first option is common for all instruments that produce a
#' \code{hex} file that can be read using Ruskin.
#' 
#' Options 2-4 are mostly obsolete, and will be removed from future versions.
#' 
#' \emph{A note on conductivity.} RBR devices record conductivity in mS/cm, and it
#' is this value that is stored in the object returned by \code{read.rsk}. This can
#' be converted to conductivity ratio (which is what many other instruments report)
#' by dividing by 42.914 (see Culkin and Smith, 1980) which will be necessary in
#' any seawater-related function that takes conductivity ratio as an argument (see
#' \dQuote{Examples}).
#'     
#'   \emph{A note on pressure.} RBR devices tend to record absolute pressure (i.e.
#'   sea pressure plus atmospheric pressure), unlike most oceanographic instruments
#'   that record sea pressure (or an estimate thereof).  The handling of pressure
#'   is controlled with the \code{patm} argument, for which there are three
#'   possibilities.  (1) If \code{patm} is \code{FALSE} (the default), then
#'   pressure read from the data file is stored in the \code{data} slot of return
#'   value, and the \code{metadata} item \code{pressureType} is set to the string
#'   \code{"absolute"}.  (2) If \code{patm} is \code{TRUE}, then an estimate of
#'   atmospheric pressure is subtracted from the raw data. For data files in the
#'   SQLite format (i.e.  \code{*.rsk} files), this estimate will be the value read
#'   from the file, or the ``standard atmosphere'' value 10.1325 dbar, if the file
#'   lacks this information.  (3) If \code{patm} is a numerical value (or list of
#'   values, one for each sampling time), then \code{patm} is subtracted from the
#'   raw data.  In cases 2 and 3, an additional column named
#'   \code{pressureOriginal} is added to the \code{data} slot to store the value
#'   contained in the data file, and \code{pressureType} is set to a string
#'   starting with \code{"sea"}.  See \code{\link{as.ctd}} for details of how this
#'   setup facilitates the conversion of \code{\link{rsk-class}} objects to
#'   \code{\link{ctd-class}} objects.
#' 
#' @return An object of \code{\link{rsk-class}}.
#' 
#' @seealso
#' The documentation for \code{\link{rsk-class}} explains the structure of
#' \code{rsk} objects, and also outlines other functions dealing with them.  Since
#' RBR has a wide variety of instruments, \code{rsk} datasets can be quite general,
#' and it is common to coerce \code{rsk} objects to other forms for specialized
#' work, e.g. \code{\link{as.ctd}} can be used to create CTD object, so that the
#' generic plot obeys the CTD format.
#' 
#' @references
#' Culkin, F., and Norman D. Smith, 1980. Determination of the concentration of
#' potassium chloride solution having the same electrical conductivity, at 15 C and
#' infinite frequency, as standard seawater of salinity 35.0000 ppt (Chlorinity
#' 19.37394 ppt). \emph{IEEE Journal of Oceanic Engineering}, \bold{5}, pp 22-23.
#' 
#' @author Dan Kelley and Clark Richards
#'
#' @family things related to \code{rsk} data
read.rsk <- function(file, from=1, to, by=1, type, tz=getOption("oceTz", default="UTC"),
                        patm=FALSE, processingLog, debug=getOption("oceDebug"))
{
    debug <- max(0, min(debug, 2))
    oceDebug(debug, "read.rsk(file=\"", file, "\", from=", format(from),
             ", to=", if(missing(to))"(not given)" else format(to),
             ", by=", by,
             ", type=", if(missing(type)) "(missing)" else type,
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
        rskv <- dbInfo[1,1]
        rskVersion <- as.numeric(strsplit(gsub(".[a-z].*$","",gsub("^.*- *", "", rskv)),"\\.")[[1]])
        ## Ruskin software version number
        appSettings <- RSQLite::dbReadTable(con, "appSettings")
        rv <- appSettings[1,2]
        ##OLD rv <- read.table(pipe(cmd), sep="|")[1,2]
        ruskinVersion <- as.numeric(strsplit(gsub(".[a-z].*$","",gsub("^.*- *", "", rv)),"\\.")[[1]])
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
        try({ # need to wrap in try() because this can fail
            deriveDepth <- RSQLite::dbReadTable(con, "deriveDepth")
            pressureAtmospheric <- deriveDepth$atmosphericPressure 
            warn <- TRUE
        }, silent=TRUE)
        if (warn)
            warning("non-standard pressureAtmospheric value: ", pressureAtmospheric, "\n")
        ##message("NEW: pressureAtmospheric:", pressureAtmospheric)
        oceDebug(debug, "after studying the RSK file, now have pressureAtmospheric=", pressureAtmospheric, "\n")

        ## From notes in comments above, it seems necessary to order by
        ## timestamp (tstamp). Ordering does not seem to be an option for
        ## dbReadTable(), so we use dbFetch().

        ## Get time stamp. Note the trick of making it floating-point
        ## to avoid the problem that R lacks 64 bit integers.
        res <- DBI::dbSendQuery(con, "select 1.0*tstamp from data order by tstamp;")
        t1000 <- DBI::dbFetch(res, n=-1)[[1]]
        RSQLite::dbClearResult(res)
        time <- numberAsPOSIXct(as.numeric(t1000) / 1000, type='unix')
        if (missing(to)) {
            if (inherits(from, 'POSIXt')) {
                to <- tail(time, 1)
            } else if (inherits(from, 'character')) {
                to <- format(tail(time, 1))
            } else if (is.numeric(from)) {
                to <- length(time)
            } else {
                stop("Unknown format for to= argument")
            }
        }
        if (is.numeric(from) & is.numeric(to)) {
            from <- t1000[from]
            to <- t1000[to]
        } else if (inherits(from, 'POSIXt') & inherits(to, 'POSIXt')) {
            from <- as.character(as.numeric(from)*1000)
            to <- as.character(as.numeric(to)*1000)
        } else if (inherits(from, 'character') & inherits(to, 'character')) {
            from <- as.character(as.numeric(as.POSIXct(from, tz=tz))*1000)
            to <- as.character(as.numeric(as.POSIXct(to, tz=tz))*1000)
        } else {
            warning('from= and to= have to be of the same class (either index, POSIXt, or character)')
        }
        if ((as.numeric(to)-as.numeric(from)) <= 0)
            stop("'to' must be greater than 'from'")

        ## Now, get only the specified time range
        res <- DBI::dbSendQuery(con, paste("select 1.0*tstamp as tstamp, * from data where tstamp between",  from, "and", to, "order by tstamp;"))
        data <- DBI::dbFetch(res, n=-1)
        time <- numberAsPOSIXct(as.numeric(data[,1])/1000, type='unix')
        ## Need to check if there is a datasetID column (for rskVersion >= 1.12.2)
        ## If so, for now just extract it from the data matrix
        hasDatasetID <- sum(grep('datasetID', names(data))) > 0
        if (hasDatasetID) {
            datasetID <- data[,grep('datasetID', names(data))]
            data <- data[,-grep('datasetID', names(data)), drop=FALSE]
        }
        data <- data[,c(-1, -2), drop=FALSE] # drop the corrupted time column
        DBI::dbClearResult(res)
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
            warning("old Ruskin file detected; if problems arise, update file with Ruskin software")
        }
        dataNamesOriginal <- c("-", channelsTable$shortName[isMeasured])
        names <- names[isMeasured] # only take names of things that are in the data table
        unitsRsk <- channelsTable$units[isMeasured]
        ## Check for duplicated names, and append digits to make unique
        if (sum(duplicated(names)) > 0) {
            for (n in names) {
                dup <- match(names, n, nomatch=0)
                if (sum(dup) > 1) { # more than one
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
        for (iname in seq_along(names)) {
            res@data[[names[iname]]] <- data[[names[iname]]]
            res@metadata$units[[names[iname]]] <- unitFromStringRsk(unitsRsk[iname])
        }
        res@metadata$units$pressure$scale <- "absolute"
        if ("pressure" %in% names) { # possibly compute sea pressure
            if (is.logical(patm)) {
                if (patm) {
                    ## This code is a bit tricky because we modify existing pressure in-place
                    dataNames <- names(res@data)
                    dataNames[dataNames=="pressure"] <- "pressureOriginal"
                    names(res@data) <- dataNames
                    res@metadata$units$pressureOriginal <- list(unit=expression(dbar), scale="absolute")
                    res@data$pressure <- res@data$pressureOriginal - 10.1325
                    res@metadata$units$pressure <- list(unit=expression(dbar), scale="sea")
                    res@metadata$dataNamesOriginal <- c(res@metadata$dataNamesOriginal, "")
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
                res@metadata$dataNamesOriginal <- c(res@metadata$dataNamesOriginal, "")
                res@metadata$pressureType <- "sea"
            } else {
                stop("patm must be logical or numeric")
            }
        }
        res@metadata$model <- model
        res@metadata$serialNumber <- serialNumber
        res@metadata$sampleInterval <- sampleInterval
        res@metadata$rskVersion <- rskVersion
        res@metadata$ruskinVersion <- ruskinVersion
        res@metadata$dataNamesOriginal <- dataNamesOriginal
        if (hasDatasetID) res@metadata$datasetID <- datasetID
        ## There is actually no need to set the conductivity unit since new()
        ## sets it, but do it anyway, as a placeholder to show where to do
        ## this, in case some RBR devices use different units
        res@metadata$units$conductivity <- list(unit=expression(mS/cm), scale="") # FIXME: will this work for all RBR rsks?
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
                              tolower(unlist(strsplit(l[grep(paste0('Channel\\[', iChannel,'\\]'), l, useBytes=TRUE)], '=', useBytes=TRUE))[2]))
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
            channels[[iChannel]] <- d[,iChannel+2]
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
        oceDebug(debug, "to=", if(missing(to))"(not given)" else format(to), "\n")
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
    } else { # to read the "old" TDR files
        while (TRUE) {
            line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
            if (0 < (r<-regexpr("Temp[ \t]*Pres", line))) break
            header <- c(header, line)
            if (0 < (r<-regexpr("Logging[ \t]*start", line))) {
                l <- sub("[ ]*Logging[ \t]*start[ ]*", "", line)
                measurementStart <- as.POSIXct(strptime(l,"%y/%m/%d %H:%M:%S", tz=tz))
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
                measurementEnd <- as.POSIXct(strptime(l,"%y/%m/%d %H:%M:%S", tz=tz))
            }
            if (0 < (r<-regexpr("Sample[ \t]*period", line))) {
                l <- sub("[ ]*Sample[ \t]*period[ ]*", "", line)
                sp <- as.numeric(strsplit(l, ":")[[1]])
                measurementDeltat <- (sp[3] + 60*(sp[2] + 60*sp[1]))
            }
        }
        oceDebug(debug, "measurementStart =", format(measurementStart), "\n")
        oceDebug(debug, "measurementEnd =", format(measurementEnd), "\n")
        oceDebug(debug, "measurementDeltat  =", measurementDeltat, "\n")
        serialNumber <- strsplit(header[1],"[\t ]+")[[1]][4]
        oceDebug(debug, "serialNumber=", serialNumber,"\n")
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
        line <- gsub("[ ]+$", "", gsub("^[ ]+","", line))
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
            if (1 == length(grep("[0-9]{4}/[0-3][0-9]/[0-3][0-9]", d[1,1])))
                time <- strptime(paste(d[1,], d[2,]), format="%Y/%m/%d %H:%M:%S", tz=tz)
            else
                time <- as.POSIXct(paste(d[1,], d[2,]), tz=tz)
            Tcol <- 3
            pcol <- 4
        } else if (nvar == 5) {
            ## 2008/06/25 10:00:00   18.5260   10.2225    0.0917
            if (1 == length(grep("[0-9]{4}/[0-3][0-9]/[0-3][0-9]", d[1,1])))
                time <- strptime(paste(d[1,], d[2,]), format="%Y/%m/%d %H:%M:%S", tz=tz)
            else
                time <- as.POSIXct(paste(d[1,], d[2,]), tz=tz)
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
#' A new \code{ctd} object is assembled from the contents of the \code{rsk} object.
#' The data and metadata are mostly unchanged, with an important exception: the
#' \code{pressure} item in the \code{data} slot may altered, because \code{rsk}
#' instruments measure total pressure, not sea pressure; see \dQuote{Details}.
#'
#' @details
#' The \code{pressureType} element of the 
#' \code{metadata} of \code{rsk} objects defines the pressure type, and this controls
#' how pressure is set up in the returned object. If \code{object@@metadata$pressureType}
#' is \code{"absolute"} (or \code{NULL}) then the resultant pressure will be adjusted
#' to make it into \code{"sea"} pressure. To do this, the value of
#' \code{object@@metadata$pressureAtmospheric} is inspected. If this is present, then
#' it is subtracted from \code{pressure}. If this is missing, then 
#' standard pressure (10.1325 dbar) will be subtracted. At this stage, the
#' pressure should be near zero at the ocean surface, but some additional adjustment 
#' might be necessary, and this may be indicated by setting the argument \code{pressureAtmospheric} to 
#' a non-zero value to be subtracted from pressure.
#'
#' @param x An \code{rsk} object, i.e. one inheriting from \code{\link{rsk-class}}.
#' @param pressureAtmospheric A numerical value (a constant or a vector),
#' that is subtracted from the pressure in \code{object} before storing it in the return value.
#' @template debugTemplate
rsk2ctd <- function(x, pressureAtmospheric=0, debug=getOption("oceDebug"))
{
    oceDebug(debug, "rsk2ctd(...) {\n", sep="", unindent=1)
    res <- new("ctd")
    res@metadata <- x@metadata
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
        res <- ctdAddColumn(res, column=S, name="salinity", label="Salinity",
                            unit=list(unit=expression(), scale="PSS-78"))
    }
    oceDebug(debug, "} # rsk2ctd()\n", sep="", unindent=1)
    res@processingLog <- processingLogAppend(res@processingLog,
                                             paste("rsk2ctd(..., pressureAtmospheric=", pressureAtmospheric, ", debug)\n",
                                                   sep="", collapse=""))
    res
}


#' @title Estimate Atmospheric Pressure in Rsk Object
#' 
#' @description
#' Estimate atmospheric pressure in rsk record.
#'
#' @details
#' Pressures must be in decibars for this to work.  First, a subset of pressures is
#' created, in which the range is \code{sap-dp} to \code{sap+dp}.  Here,
#' \code{sap}=10.1325 dbar is standard sealevel atmospheric pressure.  Within this
#' window, three measures of central tendency are calculated: the median, the mean,
#' and a weighted mean that has weight given by \eqn{exp(-2*((p
#'     - sap) / dp)^2)}{exp(-2*((p - sap) / dp)^2)}.
##' 
#' @param x A \code{rsk} object, or a list of pressures (in decibars).
#' 
#' @param dp Half-width of pressure window to be examined (in decibars).
#' 
#' @return
#' A list of four estimates: \code{sap}, the median, the mean, and the weighted
#' mean.
#' 
#' @seealso
#' The documentation for \code{\link{rsk-class}} explains the structure of
#' \code{rsk} objects, and also outlines the other functions dealing with them.
#' 
#' @examples
#' library(oce)
#' data(rsk)
#' print(rskPatm(rsk))
#' 
#' @author Dan Kelley
#' 
#' @family things related to \code{rsk} data
rskPatm <- function(x, dp=0.5)
{
    p <- if (inherits(x, "rsk")) x@data$pressure else x
    sap <- 10.1325                      # standard atm pressure
    if (length(p) < 1)
        return(rep(sap, 4))
    p <- p[(sap - dp) <= p & p <= (sap + dp)] # window near sap
    w <- exp(-2*((p - sap) / dp)^2)
    if (length(p) < 4)
        rep(sap, 4)
    else
        c(sap, median(p), mean(p), weighted.mean(p, w))
}


#' @title Decode table-of-contents File from a Rsk File
#' 
#' @description
#' Decode table-of-contents file from a rsk file, of the sort used by some
#' researchers at Dalhousie University.
#' 
#' @details
#' It is assumed that the \code{.TBL} file contains lines of the form \code{"File
#'   \\day179\\SL08A179.023 started at Fri Jun 27 22:00:00 2008"} The first step is
#' to parse these lines to get day and hour information, i.e.  179 and 023 in the
#' line above.  Then, recognizing that it is common to change the names of such
#' files, the rest of the file-name information in the line is ignored, and instead
#' a new file name is constructed based on the data files that are found in the
#' directory.  (In other words, the \code{"\\day179\\SL08A"} portion of the line is
#' replaced.)  Once the file list is complete, with all times put into R format,
#' then (optionally) the list is trimmed to the time interval indicated by
#' \code{from} and \code{to}.  It is important that \code{from} and \code{to} be in
#' the \code{UTC} time zone, because that time zone is used in decoding the lines
#' in the \code{.TBL} file.
#' 
#' @param dir name of a directory containing a single table-of-contents file, with
#' \code{.TBL} at the end of its file name.
#' 
#' @param from optional \code{\link{POSIXct}} time, indicating the beginning of a
#' data interval of interest.  This must have timezone \code{"UTC"}.
#'     
#' @param to optional \code{\link{POSIXct}} time, indicating the end of a data
#' interval of interest.  This must have timezone \code{"UTC"}.
#' 
#' @param debug optional integer to control debugging, with positive values
#' indicating to print information about the processing.
#'   
#' @examples
#' \dontrun{
#' table <- rskToc("/data/archive/sleiwex/2008/moorings/m05/adv/sontek_202h/raw",
#' from=as.POSIXct("2008-07-01 00:00:00", tz="UTC"),
#'     to=as.POSIXct("2008-07-01 12:00:00", tz="UTC"))
#' print(table)
#' }
#' 
#' @return
#' A list with two elements: \code{filename}, a vector of file names, and
#' \code{startTime}, a vector of \code{\link{POSIXct}} times indicating the (real)
#' times of the first datum in the corresponding files.
#' 
#' @author Dan Kelley
#' 
#' @family things related to \code{rsk} data
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
