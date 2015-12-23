setMethod(f="initialize",
          signature="lobo",
          definition=function(.Object,time,u,v,salinity,temperature,airtemperature,pressure,nitrate,fluorescence,filename) {
              if (!missing(time)) .Object@data$time <- time
              if (!missing(u)) .Object@data$u <- u
              if (!missing(v)) .Object@data$v <- v
              if (!missing(salinity)) .Object@data$salinity <- salinity
              if (!missing(temperature)) .Object@data$temperature <- temperature
              if (!missing(airtemperature)) .Object@data$airtemperature <- airtemperature
              if (!missing(pressure)) .Object@data$pressure <- pressure
              if (!missing(nitrate)) .Object@data$nitrate <- nitrate
              if (!missing(fluorescence)) .Object@data$fluorescence <- fluorescence
              .Object@metadata$filename <- if (missing(filename)) "" else filename
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'lobo' object"
              return(.Object)
          })


setMethod(f="summary",
          signature="lobo",
          definition=function(object, ...) {
              cat("Lobo Summary\n------------\n\n")
              cat("* source: \"", object@metadata$filename, "\"\n", sep="")
              timeRange <- range(object@data$time, na.rm=TRUE)
              cat("* time range:", format(timeRange[1], format="%Y-%m-%d %H:%M:%S %Z"),
                  "to", format(timeRange[2], format="%Y-%m-%d %H:%M:%S %Z"), "\n")
              ndata <- length(object@data)
              threes <- matrix(nrow=ndata-1, ncol=3) # skipping time
              for (i in 2:ndata) 
                  threes[i-1,] <- threenum(object@data[[i]])
              colnames(threes) <- c("Min.", "Mean", "Max.")
              rownames(threes) <- names(object@data)[-1] #skip time, the first column
              cat("* Statistics::\n\n", ...)
              print(threes)
              cat("\n")
              processingLogShow(object)
              invisible(NULL)
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
                      plotTS(as.ctd(x[["salinity"]], x[["temperature"]], x[["pressure"]]), ...)
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
    d <- read.table(file, sep='\t', header=TRUE)
    names <- names(d)
    tCol            <- grep("date", names)
    uCol            <- grep("current across", names)
    vCol            <- grep("current along", names)
    nitrateCol      <- grep("nitrate", names)
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
    metadata <- list(filename=file)
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    hitem <- processingLogItem(processingLog)
    res <- new("lobo", time=time, u=u, v=v, salinity=salinity, temperature=temperature,
               airtemperature=airtemperature, pressure=pressure,
               nitrate=nitrate, fluorescence=fluorescence, filename=filename)
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}

as.lobo <- function(time, u, v, salinity, temperature, pressure, nitrate, fluorescence, filename="")
{
    if (missing(u) || missing(v) || missing(salinity) || missing(temperature) || missing(pressure))
        stop("must give u, v, salinity, temperature, and pressure")
    res <- new("lobo", u=u, v=v, salinity=salinity, temperature=temperature, pressure=pressure, filename=filename)
    if (!missing(nitrate))
        res@data$nitrate <- nitrate
    if (!missing(fluorescence))
        res@data$fluorescence <- fluorescence
    res
}


