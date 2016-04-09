#' Class to store LOBO data
#' 
#' Class to store LOBO data.
#' A \code{lobo} object may be read with \code{\link{read.lobo}} or
#' constructed with \code{\link{as.lobo}}.  Plots can be made with
#' \code{\link{plot,lobo-method}}, while \code{\link{summary,lobo-method}} produces
#' statistical summaries. Data within a \code{lobo} object may be retrieved with
#' \code{\link{[[,lobo-method}} and altered with \code{\link{[[,lobo-method}}.
#' 
#' @author Dan Kelley
#' @family classes provided by \code{oce}
#' @family things related to \code{lobo} data
setClass("lobo", contains="oce")

setMethod(f="initialize",
          signature="lobo",
          definition=function(.Object,time,u,v,salinity,temperature,airtemperature,pressure,nitrate,fluorescence,filename) {
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
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'lobo' object"
              return(.Object)
          })


#' lobo dataset
#' 
#' This is sample lobo dataset obtained in the Northwest Arm of Halifax by
#' Satlantic.
#' 
#' @name lobo
#' @docType data
#'
#' @author Dan Kelley
#' @source The data were downloaded from a web interface at Satlantic LOBO web
#' server and then read with \code{\link{read.lobo}}.
#' @examples
#' \dontrun{
#' library(oce)
#' data(lobo)
#' summary(lobo)
#' plot(lobo)
#' }
#' 
#' @family datasets provided with \code{oce}
#' @family things related to \code{lobo} data
NULL

#' @title Extract Something From a \code{lobo} Object
#' @param x A lobo object, i.e. one inheriting from \code{\link{lobo-class}}.
#' @template sub_subTemplate
#' @family things related to \code{lobo} data
setMethod(f="[[",
          signature(x="lobo", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              callNextMethod()
          })

#' @title Replace Parts of a \code{lobo} Object
#' @param x An \code{lobo} object, i.e. inheriting from \code{\link{lobo-class}}
#' @template sub_subsetTemplate
#' @family things related to \code{lobo} data
setMethod(f="[[<-",
          signature(x="lobo", i="ANY", j="ANY"),
          definition=function(x, i, j, value) {
              callNextMethod(x=x, i=i, j=j, value=value)
          })

#' Summarize a lobo data object
#' 
#' Summarizes some of the data in a lobo object.
#' 
#' Pertinent summary information is presented, including the sampling interval,
#' data ranges, etc.
#' 
#' @param object an object of class \code{"lobo"}, usually, a result of a call
#' to \code{\link{read.lobo}} or \code{\link{read.oce}}.
#' @param \dots further arguments passed to or from other methods.
#' @return A matrix containing statistics of the elements of the \code{data}
#' slot.
#' @author Dan Kelley
#' @seealso The documentation for \code{\link{lobo-class}} explains the
#' structure of LOBO objects, and also outlines the other functions dealing
#' with them.
#' @references \url{http://lobo.satlantic.com} \url{http://www.mbari.org/lobo/}
#' @keywords misc
#' @examples
#' 
#' library(oce)
#' data(lobo)
#' summary(lobo)
#' @family things related to \code{lobo} data
setMethod(f="summary",
          signature="lobo",
          definition=function(object, ...) {
              cat("Lobo Summary\n------------\n\n")
              cat("* source: \"", object@metadata$filename, "\"\n", sep="")
              callNextMethod()
          })



#' Subset an lobo object
#' 
#' Subset an lobo object, in a way that is somewhat
#' analogous to \code{\link{subset.data.frame}}.
#' 
#' @param x a \code{lobo} object.
#' @param subset a condition to be applied to the \code{data} portion of
#' \code{x}.  See \sQuote{Details}.
#' @param \dots ignored.
#' @return A new \code{lobo} object.
#' @author Dan Kelley
#' @family things related to \code{lobo} data
setMethod(f="subset",
          signature="lobo",
          definition=function(x, subset, ...) {
              res <- new("lobo") # start afresh in case x@data is a data.frame
              res@metadata <- x@metadata
              res@processingLog <- x@processingLog
              for (i in seq_along(x@data)) {
                  r <- eval(substitute(subset), x@data, parent.frame(2))
                  r <- r & !is.na(r)
                  res@data[[i]] <- x@data[[i]][r]
              }
              names(res@data) <- names(x@data)
              subsetString <- paste(deparse(substitute(subset)), collapse=" ")
              res@processingLog <- processingLogAppend(res@processingLog, paste("subset.lobo(x, subset=", subsetString, ")", sep=""))
              res
          })
 

plot.lobo.timeseries.TS <- function(lobo,
                                    S.col = "blue", T.col = "darkgreen", draw.legend=FALSE, ...)
{
    plot(lobo@data$time, lobo@data$salinity, type='l', ylab="", axes=FALSE, ...)
    mgp <- par("mgp")
    ##cat("mgp=",paste(par("mgp"), collapse=" "), "\n")
    ##cat("mar=",paste(par("mar"), collapse=" "), "\n")
    axis(2, col.lab=S.col)
    axis.POSIXct(1, lobo@data$time)
    mtext("S [PSU]", side=2, line=mgp[1], col=S.col, cex=par("cex"))
    box()
    lines(lobo@data$time, lobo@data$salinity, col=S.col, ...)
    ## Set up scale for temperature
    usr <- par("usr")
    range <- range(lobo@data$temperature, na.rm=TRUE)
    usr[3:4] <- range + c(-1, 1) * 0.04 * diff(range)
    par(usr=usr)
    ##
    lines(lobo@data$time, lobo@data$temperature, col=T.col, ...)
    axis(4, col=T.col)
    mtext(expression(paste("T [", degree, "C]")), side=4, line=mgp[1], col=T.col, cex=par("cex"))
    if (draw.legend)
        legend("topright",c("S","T"),col=c(S.col,T.col),lwd=2)
    mtext(paste(paste(format(range(lobo@data$time, na.rm=TRUE)), collapse=" to "),
                attr(lobo@data$time[1], "tzone")),
          side=3, cex=3/4*par("cex.axis"), adj=0)
    invisible(lobo)
}

plot.lobo.timeseries.uv <- function(lobo, col.u = "blue", col.v = "darkgreen", draw.legend=FALSE, ...)
{
    peak <- max(range(c(lobo@data$u,lobo@data$v),na.rm=TRUE))
    ylim <- c(-peak,peak)
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
        legend("topright",c("U","V"),col=c(col.u,col.v),lwd=2)
    invisible(lobo)
}

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
        legend("top",c("nitrate","fluorescence"),col=c(col.nitrate,col.fluorescence),lwd=2, ...)
}

plot.lobo.TS <- function(lobo, ...)
{
    plotTS(as.ctd(lobo@data$salinity, lobo@data$temperature, 0), ...)
}


#' Plot lobo data
#' 
#' Plot a summary diagram for lobo data.
#' 
#' @param x A \code{lobo} object, e.g. as read by \code{\link{read.lobo}}.
#' @param which A vector of numbers or character strings, indicating the
#' quantities to plot.  These are stacked in a single column.  The possible
#' values for \code{which} are as follows: \code{1} or \code{"temperature"} for
#' a time series of temperature; \code{2} or \code{"salinity"} for salinity;
#' \code{3} or \code{"TS"} for a TS diagram; \code{4} or \code{"u"} for a
#' timeseries of the u component of velocity; \code{5} or \code{"v"} for a
#' timeseries of the v component of velocity; \code{6} or \code{"nitrate"} for
#' a timeseries of nitrate concentration; \code{7} or \code{"fluorescence"} for
#' a timeseries of fluorescence value.
#' @param adorn list of expressions to be executed for the panels in turn, e.g.
#' to adorn the plots.  If the number matches the number of panels, then the
#' strings are applied to the appropriate panels, as they are drawn from
#' top-left to bottom-right.  If only a single expression is provided, it is
#' used for all panels. (See \dQuote{Examples}.)
#' @param mgp 3-element numerical vector to use for \code{par(mgp)}, and also
#' for \code{par(mar)}, computed from this.  The default is tighter than the R
#' default, in order to use more space for the data and less for the axes.
#' @param mar value to be used with \code{\link{par}("mar")}.
#' @template debugTemplate
#' @param \dots optional arguments passed to plotting functions.
#' @author Dan Kelley
#' 
#' @family functions that plot \code{oce} data
#' @family things related to \code{lobo} data
setMethod(f="plot",
          signature=signature("lobo"),
          definition=function(x,
                              which=c(1,2,3), 
                              adorn=NULL,
                              mgp=getOption("oceMgp"),
                              mar=c(mgp[2]+1, mgp[1]+1, 1, mgp[1]+1.25),
                              debug=getOption("oceDebug"),
                              ...)
          {
              if (!inherits(x, "lobo"))
                  stop("method is only for objects of class '", "lobo", "'")
              oceDebug(debug, "plot.lobo(...)\n", sep="")
              opar <- par(no.readonly = TRUE)
              nw <- length(which)
              oceDebug(debug, "which:", which, "\n")
              which2 <- oce.pmatch(which,
                                   list(temperature=1, salinity=2, TS=3, u=4, v=5, nitrate=6, fluoresence=7))
              oceDebug(debug, "which2:", which2, "\n")
              if (length(which) > 1) on.exit(par(opar))
              par(mgp=mgp, mar=mar)
              adornLength <- length(adorn)
              if (adornLength < nw) {
                  adorn <- rep(adorn, nw)
                  adornLength <- nw
              }
              par(mar=c(mgp[2]+1, mgp[1]+1, 1.25, mgp[1]+1.25))
              par(mfrow=c(nw, 1))
              for (w in which2) {
                  if (w == 1) {
                      oce.plot.ts(x[["time"]], x[["temperature"]], ylab=resizableLabel("T"), ...)
                  } else if (w == 2) {
                      oce.plot.ts(x[["time"]], x[["salinity"]], ylab=resizableLabel("S"), ...)
                  } else if (w == 3) {
                      if (any(!is.na(x[['pressure']])))
                          plotTS(as.ctd(x[["salinity"]], x[["temperature"]], x[["pressure"]]), ...) else
                              plotTS(as.ctd(x[["salinity"]], x[["temperature"]], 0), ...)
                  } else if (w == 4) {
                      oce.plot.ts(x[["time"]], x[["u"]], ylab=resizableLabel("u"), ...)
                  } else if (w == 5) {
                      oce.plot.ts(x[["time"]], x[["v"]], ylab=resizableLabel("v"), ...)
                  } else if (w == 6) {
                      oce.plot.ts(x[["time"]], x[["nitrate"]], ylab=resizableLabel("nitrate", axis="y"), ...)
                  } else if (w == 7) {
                      oce.plot.ts(x[["time"]], x[["fluorescence"]], ylab=resizableLabel("fluorescence", axis="y"), ...)
                  }
                  if (adornLength > 0) {
                      t <- try(eval(adorn[1]), silent=TRUE)
                      if (class(t) == "try-error") warning("cannot evaluate adorn[", 1, "]\n")
                  }
              }

#              if (any(!is.na(x@data$u) & !is.na(x@data$v))) {
#                  par(mar=c(mgp[2]+1, mgp[1]+1, 1.25, mgp[1]+1.25))
#                  plot.lobo.timeseries.uv(x, ...)
#                  if (adornLength > 0) {
#                      t <- try(eval(adorn[2]), silent=TRUE)
#                      if (class(t) == "try-error") warning("cannot evaluate adorn[", 2, "]\n")
#                  }
#              }
#
#              par(mar=c(mgp[2]+1, mgp[1]+1, 1.25, mgp[1]+1.25))
#              plot.lobo.timeseries.biology(x, ...)
#              if (adornLength > 0) {
#                  t <- try(eval(adorn[3]), silent=TRUE)
#                  if (class(t) == "try-error") warning("cannot evaluate adorn[", 3, "]\n")
#              }
#
#              par(mar=c(mgp[1]+1, mgp[1]+1, 1.25, mgp[1]+1.25))
#              plot.lobo.TS(x, ...)
#              if (adornLength > 0) {
#                  t <- try(eval(adorn[4]), silent=TRUE)
#                  if (class(t) == "try-error") warning("cannot evaluate adorn[", 4, "]\n")
#              }
          })




#' Read a lobo data file
#' 
#' Read a data file created by a LOBO instrument.
#' 
#' This version of \code{read.lobo} is really quite crude, having been
#' developed mainly for a ``predict the Spring bloom'' contest at Dalhousie
#' University.  In particular, the function assumes that the data columns are
#' exactly as specified in the Examples section; if you reorder the columns or
#' add new ones, this function is unlikely to work correctly. Furthermore, it
#' should be noted that the file format was inferred simply by downloading
#' files; the supplier makes no claims that the format will be fixed in time.
#' 
#' It is also worth noting that there is no \code{\link{read.oce}} equivalent
#' to \code{read.lobo}, because the file format has no recognizable header.
#' 
#' @param file a connection or a character string giving the name of the file
#' to load.
#' @param cols number of columns in dataset.
#' @param processingLog if provided, the action item to be stored in the log.
#' (Typically only provided for internal calls; the default that it provides is
#' better for normal calls by a user.)
#' @return An object of \code{\link{lobo-class}}.
#' @author Dan Kelley
#' @examples
#' \dontrun{
#' library(oce)
#' uri <- paste("http://lobo.satlantic.com/cgi-bin/nph-data.cgi?",
#'   "min_date=20070220&max_date=20070305",
#'   "&x=date&",
#'   "y=current_across1,current_along1,nitrate,fluorescence,salinity,temperature&",
#'   "data_format=text",sep="")
#' lobo <- read.lobo(uri)
#' }
read.lobo <- function(file, cols=7, processingLog)
{
    ## header <- scan(file, what=character(), sep="\t", nlines=1, quiet=TRUE)
    ## d <- scan(file, what=character(), sep="\t", skip=1,  quiet=TRUE)
    filename <- ""
    if (is.character(file)) {
        filename <- fullFilename(file)
        file <- file(file, "r")
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
    time <- strptime(d[,tCol], "%Y-%m-%d %H:%M:%S", tz="UTC") # tz is likely wrong 
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



#' Coerce data into lobo dataset
#' 
#' Coerce a dataset into a lobo dataset.
#' 
#' This function assembles vectors into a \code{lobo} object.
#' 
#' @param time vector of times of observation
#' @param u vector of x velocity component observations
#' @param v vector of y velocity component observations
#' @param salinity vector of salinity observations
#' @param temperature vector of temperature observations
#' @param pressure vector of pressure observationss
#' @param nitrate vector of nitrate observationss
#' @param fluorescence vector of fluoresence observations
#' @param filename source filename
#' @return An object of \code{\link{lobo-class}}.
#' @author Dan Kelley
#' @family things related to \code{lobo} data
as.lobo <- function(time, u, v, salinity, temperature, pressure, nitrate, fluorescence, filename="")
{
    if (missing(u) || missing(v) || missing(salinity) || missing(temperature) || missing(pressure))
        stop("must give u, v, salinity, temperature, and pressure")
    new("lobo", u=u, v=v, salinity=salinity, temperature=temperature, pressure=pressure,
        nitrate=nitrate, fluorescence=fluorescence, filename=filename)
}


