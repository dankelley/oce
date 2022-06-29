# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Class to Store LOBO Data
#'
#' This class stores LOBO data.
#'
#' @templateVar class lobo
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
#' @family classes provided by oce
#' @family things related to lobo data
setClass("lobo", contains="oce")

setMethod(f="initialize",
          signature="lobo",
          definition=function(.Object, time, u, v, salinity, temperature, airtemperature, pressure, nitrate, fluorescence, filename, ...) {
              .Object <- callNextMethod(.Object, ...)
              if (!missing(time)) .Object@data$time <- time
              if (!missing(u)) {
                  .Object@data$u <- u
                  .Object@metadata$units$u <- list(unit=expression(m/s), scale="")
              }
              if (!missing(v)) {
                  .Object@data$v <- v
                  .Object@metadata$units$v <- list(unit=expression(m/s), scale="")
              }
              if (!missing(salinity)) {
                  .Object@data$salinity <- salinity
                  .Object@metadata$units$salinity <- list(unit=expression(), scale="PSS-78")
              }
              if (!missing(temperature)) {
                  .Object@data$temperature <- temperature
                  .Object@metadata$units$temperature <- list(unit=expression(degree*C), scale="ITS-90")
              }
              if (!missing(airtemperature)) {
                  .Object@data$airtemperature <- airtemperature
                  .Object@metadata$units$airtemperature <- list(unit=expression(degree*C), scale="ITS-90")
              }
              if (!missing(pressure)) {
                  .Object@data$pressure <- pressure
                  .Object@metadata$units$pressure <- list(unit=expression(dbar), scale="")
              }
              if (!missing(nitrate)) {
                  .Object@data$nitrate <- nitrate
                  .Object@metadata$units$nitrate <- list(unit=expression(mu * M), scale="")
              }
              if (!missing(fluorescence)) {
                  .Object@data$fluorescence <- fluorescence
                  .Object@metadata$units$fluorescence <- list(unit=expression(mu * g / l), scale="")
              }
              .Object@metadata$filename <- if (missing(filename)) "" else filename
              .Object@processingLog$time <- presentTime()
              .Object@processingLog$value <- "create 'lobo' object"
              return(.Object)
          })


#' Sample LOBO Dataset
#'
#' This is sample lobo dataset obtained in the Northwest Arm of Halifax by
#' Satlantic.
#'
#' @name lobo
#'
#' @docType data
#'
#' @author Dan Kelley
#'
#' @source The data were downloaded from a web interface at Satlantic LOBO web
#' server and then read with [read.lobo()].
#'
#' @examples
#' library(oce)
#' data(lobo)
#' summary(lobo)
#' plot(lobo)
#'
#' @family datasets provided with oce
#' @family things related to lobo data
NULL

#' @title Extract Something From a LOBO Object
#'
#' @param x a [lobo-class] object.
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
#' @family things related to lobo data
setMethod(f="[[",
          signature(x="lobo", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              if (i == "?")
                  return(list(metadata=sort(names(x@metadata)),
                          metadataDerived=NULL,
                          data=sort(names(x@data)),
                          dataDerived=NULL))
              callNextMethod() # [[
          })

#' Replace Parts of a LOBO Object
#'
#' @param x a [lobo-class] object.
#'
#' @template sub_subsetTemplate
#'
#' @family things related to lobo data
setMethod(f="[[<-",
          signature(x="lobo", i="ANY", j="ANY"),
          definition=function(x, i, j, ..., value) {
              callNextMethod(x=x, i=i, j=j, ...=..., value=value) # [[<-
          })

#' Summarize a LOBO Object
#'
#' Pertinent summary information is presented, including the sampling interval,
#' data ranges, etc.
#'
#' @param object a [lobo-class] object.
#'
#' @param \dots further arguments passed to or from other methods.
#'
#' @return A matrix containing statistics of the elements of the `data`
#' slot.
#'
#' @author Dan Kelley
#'
#' @seealso The documentation for [lobo-class] explains the
#' structure of LOBO objects, and also outlines the other functions dealing
#' with them.
#'
#' @examples
#'
#' library(oce)
#' data(lobo)
#' summary(lobo)
#' @family things related to lobo data
setMethod(f="summary",
          signature="lobo",
          definition=function(object, ...) {
              cat("Lobo Summary\n------------\n\n")
              cat("* source: \"", object@metadata$filename, "\"\n", sep="")
              invisible(callNextMethod()) # summary
          })



#' Subset a LOBO Object
#'
#' Subset an lobo object, in a way that is somewhat
#' analogous to [subset.data.frame()].
#'
#' @param x a [lobo-class] object.
#'
#' @param subset a condition to be applied to the `data` portion of
#' `x`.  See \sQuote{Details}.
#'
#' @param \dots ignored.
#'
#' @return A [lobo-class] object.
#'
#' @author Dan Kelley
#'
#' @family things related to lobo data
#' @family functions that subset oce objects
setMethod(f="subset",
          signature="lobo",
          definition=function(x, subset, ...) {
              res <- new("lobo") # start afresh in case x@data is a data.frame
              res@metadata <- x@metadata
              res@processingLog <- x@processingLog
              for (i in seq_along(x@data)) {
                  ##r <- eval(substitute(subset), x@data, parent.frame(2))
                  r <- eval(expr=substitute(expr=subset, env=environment()), envir=x@data, enclos=parent.frame(2))
                  r <- r & !is.na(r)
                  res@data[[i]] <- x@data[[i]][r]
              }
              names(res@data) <- names(x@data)
              ## subsetString <- paste(deparse(substitute(subset)), collapse=" ")
              subsetString <- paste(deparse(substitute(expr=subset, env=environment())), collapse=" ")
              res@processingLog <- processingLogAppend(res@processingLog, paste("subset.lobo(x, subset=", subsetString, ")", sep=""))
              res
          })


#' @family things related to lobo data
plot.lobo.timeseries.TS <- function(lobo,
                                    S.col = "blue", T.col = "darkgreen", draw.legend=FALSE, ...)
{
    plot(lobo@data$time, lobo[["salinity"]], type='l', ylab="", axes=FALSE, ...)
    mgp <- par("mgp")
    ##cat("mgp=",paste(par("mgp"), collapse=" "), "\n")
    ##cat("mar=",paste(par("mar"), collapse=" "), "\n")
    axis(2, col.lab=S.col)
    axis.POSIXct(1, lobo@data$time)
    mtext("S [PSU]", side=2, line=mgp[1], col=S.col, cex=par("cex"))
    box()
    lines(lobo@data$time, lobo[["salinity"]], col=S.col, ...)
    ## Set up scale for temperature
    usr <- par("usr")
    range <- range(lobo[["temperature"]], na.rm=TRUE)
    usr[3:4] <- range + c(-1, 1) * 0.04 * diff(range)
    par(usr=usr)
    ##
    lines(lobo@data$time, lobo[["temperature"]], col=T.col, ...)
    axis(4, col=T.col)
    mtext(expression(paste("T [", degree, "C]")), side=4, line=mgp[1], col=T.col, cex=par("cex"))
    if (draw.legend)
        legend("topright", c("S", "T"), col=c(S.col, T.col), lwd=2)
    mtext(paste(paste(format(range(lobo@data$time, na.rm=TRUE)), collapse=" to "),
                attr(lobo@data$time[1], "tzone")),
          side=3, cex=3/4*par("cex.axis"), adj=0)
    invisible(lobo)
}

#' @family things related to lobo data
plot.lobo.timeseries.uv <- function(lobo, col.u = "blue", col.v = "darkgreen", draw.legend=FALSE, ...)
{
    peak <- max(range(c(lobo@data$u, lobo@data$v), na.rm=TRUE))
    ylim <- c(-peak, peak)
    plot(lobo@data$time, lobo@data$u, ylim=ylim, type='l', axes=FALSE, col=col.u, ylab="", ...)
    box()
    lines(lobo@data$time, lobo@data$v, col=col.v, ...)
    axis.POSIXct(1, lobo@data$time)
    axis(2, col=col.u)
    axis(4, col=col.v)
    mgp <- par("mgp")
    mtext("U [m/s]", side=2, line=mgp[1], col=col.u, cex=par("cex"))
    mtext("V [m/s]", side=4, line=mgp[1], col=col.v, cex=par("cex"))
    if (draw.legend)
        legend("topright", c("U", "V"), col=c(col.u, col.v), lwd=2)
    invisible(lobo)
}

#' @family things related to lobo data
plot.lobo.timeseries.biology <- function(lobo, col.fluorescence = "blue", col.nitrate = "darkgreen", draw.legend=FALSE, ...)
{
    plot(lobo@data$time, lobo@data$fluorescence, type='l', ylab="", axes=FALSE, ...)
    axis(2, col.lab=col.fluorescence)
    axis.POSIXct(1, lobo@data$time)
    mgp <- par("mgp")
    mtext("Fluorescence", side=2, line=mgp[1], col=col.fluorescence, cex=par("cex"))
    box()
    lines(lobo@data$time, lobo@data$fluorescence, col=col.fluorescence, ...)
    ## Set up scale for temperature
    usr <- par("usr")
    range <- range(lobo@data$nitrate, na.rm=TRUE)
    usr[3:4] <- range + c(-1, 1) * 0.04 * diff(range)
    par(usr=usr)
    ##
    lines(lobo@data$time, lobo@data$nitrate, col=col.nitrate)
    axis(4, col=col.nitrate)
    mtext("Nitrate", side=4, line=mgp[1], col=col.nitrate, cex=par("cex"))
    if (draw.legend)
        legend("top", c("nitrate", "fluorescence"), col=c(col.nitrate, col.fluorescence), lwd=2, ...)
}

#' @family things related to lobo data
plot.lobo.TS <- function(lobo, ...)
{
    plotTS(as.ctd(lobo[["salinity"]], lobo[["temperature"]], 0), ...)
}


#' Plot a lobo object
#'
#' Plot a summary diagram for lobo data.
#'
#' @param x a [lobo-class] object.
#'
#' @param which A vector of numbers or character strings, indicating the
#' quantities to plot.  These are stacked in a single column.  The possible
#' values for `which` are as follows: `1` or `"temperature"` for
#' a time series of temperature; `2` or `"salinity"` for salinity;
#' `3` or `"TS"` for a TS diagram (which uses `eos="unesco"`),
#' `4` or `"u"` for a
#' timeseries of the u component of velocity; `5` or `"v"` for a
#' timeseries of the v component of velocity; `6` or `"nitrate"` for
#' a timeseries of nitrate concentration; `7` or `"fluorescence"` for
#' a timeseries of fluorescence value.
#'
#' @param mgp 3-element numerical vector to use for `par(mgp)`, and also
#' for `par(mar)`, computed from this.  The default is tighter than the R
#' default, in order to use more space for the data and less for the axes.
#'
#' @param mar value to be used with [`par`]`("mar")`.
#' @template debugTemplate
#'
#' @param \dots optional arguments passed to plotting functions.
#'
#' @author Dan Kelley
#'
#' @family functions that plot oce data
#' @family things related to lobo data
#'
#' @aliases plot.lobo
setMethod(f="plot",
          signature=signature("lobo"),
          definition=function(x,
                              which=c(1, 2, 3),
                              mgp=getOption("oceMgp"),
                              mar=c(mgp[2]+1, mgp[1]+1, 1, mgp[1]+1.25),
                              debug=getOption("oceDebug"),
                              ...)
          {
              oceDebug(debug, "plot.lobo(...)\n", sep="")
              opar <- par(no.readonly = TRUE)
              nw <- length(which)
              oceDebug(debug, "which:", which, "\n")
              which2 <- oce.pmatch(which,
                                   list(temperature=1, salinity=2, TS=3, u=4, v=5, nitrate=6, fluoresence=7))
              oceDebug(debug, "which2:", which2, "\n")
              if (length(which) > 1) on.exit(par(opar))
              par(mgp=mgp, mar=mar)
              par(mar=c(mgp[2]+1, mgp[1]+1, 1.25, mgp[1]+1.25))
              par(mfrow=c(nw, 1))
              for (w in which2) {
                  if (w == 1) {
                      oce.plot.ts(x[["time"]], x[["temperature"]], ylab=resizableLabel("T"), debug=debug-1, ...)
                  } else if (w == 2) {
                      oce.plot.ts(x[["time"]], x[["salinity"]], ylab=resizableLabel("S"), debug=debug-1, ...)
                  } else if (w == 3) {
                      if (any(!is.na(x[['pressure']]))) {
                          plotTS(as.ctd(x[["salinity"]], x[["temperature"]], x[["pressure"]]), eos="unesco", debug=debug-1, ...)
                      } else {
                          plotTS(as.ctd(x[["salinity"]], x[["temperature"]], 0), eos="unesco", debug=debug-1, ...)
                      }
                  } else if (w == 4) {
                      oce.plot.ts(x[["time"]], x[["u"]], ylab=resizableLabel("u"), debug=debug-1, ...)
                  } else if (w == 5) {
                      oce.plot.ts(x[["time"]], x[["v"]], ylab=resizableLabel("v"), debug=debug-1, ...)
                  } else if (w == 6) {
                      oce.plot.ts(x[["time"]], x[["nitrate"]], ylab=resizableLabel("nitrate", axis="y"), debug=debug-1, ...)
                  } else if (w == 7) {
                      oce.plot.ts(x[["time"]], x[["fluorescence"]], ylab=resizableLabel("fluorescence", axis="y"), debug=debug-1, ...)
                  }
              }
          })


#' Read a LOBO File
#'
#' Read a data file created by a LOBO instrument.
#'
#' This version of `read.lobo` is really quite crude, having been
#' developed mainly for a ``predict the Spring bloom'' contest at Dalhousie
#' University.  In particular, the function assumes that the data columns are
#' exactly as specified in the Examples section; if you reorder the columns or
#' add new ones, this function is unlikely to work correctly. Furthermore, it
#' should be noted that the file format was inferred simply by downloading
#' files; the supplier makes no claims that the format will be fixed in time.
#' It is also worth noting that there is no [read.oce()] equivalent
#' to `read.lobo`, because the file format has no recognizable header.
#'
#' @param file a connection or a character string giving the name of the file
#' to load.
#'
#' @param cols number of columns in dataset.
#'
#' @template encodingTemplate
#'
#' @param processingLog if provided, the action item to be stored in the log.
#' (Typically only provided for internal calls; the default that it provides is
#' better for normal calls by a user.)
#'
#' @return A [lobo-class] object.
#'
#' @author Dan Kelley
#'
#' @examples
#'\dontrun{
#' library(oce)
#' uri <- paste("http://lobo.satlantic.com/cgi-bin/nph-data.cgi?",
#'   "min_date=20070220&max_date=20070305",
#'   "&x=date&",
#'   "y=current_across1,current_along1,nitrate,fluorescence,salinity,temperature&",
#'   "data_format=text",sep="")
#' lobo <- read.lobo(uri)
#'}
#' @family things related to lobo data
read.lobo <- function(file,
    cols=7,
    encoding="latin1",
    processingLog)
{
    if (missing(file))
        stop("must supply 'file'")
    if (is.character(file)) {
        if (!file.exists(file))
            stop("cannot find file '", file, "'")
        if (0L == file.info(file)$size)
            stop("empty file '", file, "'")
    }
    ## header <- scan(file, what=character(), sep="\t", nlines=1, quiet=TRUE)
    ## d <- scan(file, what=character(), sep="\t", skip=1,  quiet=TRUE)
    filename <- ""
    if (is.character(file)) {
        filename <- fullFilename(file)
        file <- file(file, "r", encoding=encoding)
        on.exit(close(file))
    } else {
        if (!inherits(file, "connection"))
            stop("argument `file' must be a character string or connection")
        if (!isOpen(file)) {
            open(file, "r")
            on.exit(close(file))
        }
    }
    d <- read.table(file, sep='\t', header=TRUE, stringsAsFactors=FALSE)
    names <- names(d)
    tCol <- grep("date", names)
    uCol <- grep("current across", names)
    vCol <- grep("current along", names)
    nitrateCol <- grep("nitrate", names)
    fluorescenceCol <- grep("fluorescence", names)
    SCol            <- grep("salinity", names)
    TCol            <- grep("^temperature", names, ignore.case=TRUE)
    TaCol           <- grep("^Air.*temperature", names, ignore.case=TRUE)
    pressureCol     <- grep("pressure", names)
    if (!length(tCol))
        stop("no time column in data file.  The column names are: ", paste(names, collapse=" "))
    ## until issue 808, used as.POSIXct() here
    time <- strptime(d[, tCol], "%Y-%m-%d %H:%M:%S", tz="UTC") # tz is likely wrong
    n <- dim(d)[1]
    u <- if (length(uCol)) as.numeric(d[, uCol]) else rep(NA, n)
    v <- if (length(vCol)) as.numeric(d[, vCol]) else rep(NA, n)
    salinity <- if (length(SCol)) as.numeric(d[, SCol]) else rep(NA, n)
    temperature <- if (length(TCol)) as.numeric(d[, TCol]) else rep(NA, n)
    airtemperature <- if (length(TaCol)) as.numeric(d[, TaCol]) else rep(NA, n)
    nitrate <- if (length(nitrateCol)) as.numeric(d[, nitrateCol]) else rep(NA, n)
    fluorescence <- if (length(fluorescenceCol)) as.numeric(d[, fluorescenceCol]) else rep(NA, n)
    pressure <- if (length(pressureCol)) as.numeric(d[, pressureCol]) else rep(NA, n)
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    ##hitem <- processingLogItem(processingLog)
    res <- new("lobo", time=time, u=u, v=v, salinity=salinity, temperature=temperature,
               airtemperature=airtemperature, pressure=pressure,
               nitrate=nitrate, fluorescence=fluorescence, filename=filename)
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}



#' Coerce Data into a Lobo Object
#'
#' Coerce a dataset into a lobo dataset.
#'
#' @param time vector of times of observation
#'
#' @param u vector of x velocity component observations
#'
#' @param v vector of y velocity component observations
#'
#' @param salinity vector of salinity observations
#'
#' @param temperature vector of temperature observations
#'
#' @param pressure vector of pressure observations
#'
#' @param nitrate vector of nitrate observations
#'
#' @param fluorescence vector of fluorescence observations
#'
#' @param filename source filename
#'
#' @return A [lobo-class] object.
#'
#' @author Dan Kelley
#'
#' @family things related to lobo data
as.lobo <- function(time, u, v, salinity, temperature, pressure, nitrate, fluorescence, filename="")
{
    if (missing(u) || missing(v) || missing(salinity) || missing(temperature) || missing(pressure))
        stop("must give u, v, salinity, temperature, and pressure")
    new("lobo", u=u, v=v, salinity=salinity, temperature=temperature, pressure=pressure,
        nitrate=nitrate, fluorescence=fluorescence, filename=filename)
}
