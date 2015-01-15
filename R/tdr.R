setMethod(f="initialize",
          signature="tdr",
          definition=function(.Object,time,pressure,temperature,filename="") {
              if (!missing(time)) .Object@data$time <- time
              if (!missing(pressure)) .Object@data$pressure <- pressure
              if (!missing(temperature)) .Object@data$temperature <- temperature
              .Object@metadata$filename <- filename
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'tdr' object"
              return(.Object)
          })


setMethod(f="summary",
          signature="tdr",
          definition=function(object, ...) {
              cat("TDR Summary\n----------\n", ...)
              cat(paste("* Instrument:         RBR, serial number ``", object@metadata$serialNumber,
                        "``, model ``", object@metadata$model, "``\n", sep=""))
              if ("pressureAtmospheric" %in% names(object@metadata)) {
                  cat(paste("* Atmospheric pressure: ", object@metadata$pressureAtmospheric, "\n", sep=""))
              }
              cat(paste("* Source:             ``", object@metadata$filename, "``\n", sep=""), ...)
              cat(sprintf("* Measurements:       %s %s to %s %s sampled at %.4g Hz\n",
                          format(object@metadata$tstart), attr(object@metadata$tstart, "tzone"),
                          format(object@metadata$tend), attr(object@metadata$tend, "tzone"),
                          1 / object@metadata$deltat))
              cat("* Statistics of subsample::\n\n")
              time.range <- range(object@data$time, na.rm=TRUE)
              threes <- matrix(nrow=2, ncol=3)
              threes[1,] <- threenum(object@data$temperature)
              threes[2,] <- threenum(object@data$pressure)
              colnames(threes) <- c("Min.", "Mean", "Max.")
              rownames(threes) <- c("Temperature", "Pressure")
              print(threes)
              cat('\n')
              processingLogShow(object)
              invisible(NULL)
          })


setMethod(f="subset",
          signature="tdr",
          definition=function(x, subset, ...) {
              rval <- new("tdr") # start afresh in case x@data is a data.frame
              rval@metadata <- x@metadata
              rval@processingLog <- x@processingLog
              message("NOTE: debugging output coming up!")
              for (i in seq_along(x@data)) {
                  message("i: ", i)
                  str(x@data)
                  str(x@data$time[1])
                  print(x@data$time[1])
                  print(x@data$time[2])
                  print(is.language(substitute(subset)))
                  str(substitute(subset))
                  r <- eval(substitute(subset), x@data, parent.frame())
                  str(r)
                  r <- r & !is.na(r)
                  rval@data[[i]] <- x@data[[i]][r]
              }
              subsetString <- paste(deparse(substitute(subset)), collapse=" ")
              rval@processingLog <- processingLog(rval@processingLog, paste("subset.tdr(x, subset=", subsetString, ")", sep=""))
              rval
          })
 
as.tdr <- function(time, temperature, pressure,
                   filename="",
                   instrumentType="rbr",
                   serialNumber="", model="",
                   pressureAtmospheric=NA,
                   processingLog, debug=getOption("oceDebug"))
{
    debug <- min(debug, 1)
    oceDebug(debug, "as.tdr(..., filename=\"", filename, "\", serialNumber=\"", serialNumber, "\")\n", sep="", unindent=1)
    if (missing(time) || missing(temperature) || missing(pressure))
        stop("must give (at least) time, temperature, and pressure")
    if (!inherits(time, "POSIXt"))
        stop("'time' must be POSIXt")
    time <- as.POSIXct(time)
    if (length(time) != length(temperature))
        stop("lengths of 'time' and 'temperature' must match")
    if (length(time) != length(pressure))
        stop("lengths of 'time' and 'pressure' must match")
    ##res <- new("tdr")# , time, pressure, temperature, filename)
    res <- new("tdr", time, pressure, temperature, filename)
    res@metadata$instrumentType <- instrumentType
    res@metadata$model <- model
    res@metadata$serialNumber <- serialNumber
    res@metadata$filename <- filename
    res@metadata$pressureAtmospheric <- pressureAtmospheric
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    res@processingLog <- processingLog(res@processingLog, processingLog)
    ## data <- list(time=time, pressure=pressure, temperature=temperature)
    ## res@data <- data
    ## message("NEW as.tdr() ... no help")
    oceDebug(debug, "} # as.tdr()\n", sep="", unindent=1)
    res
}

setMethod(f="plot",
          signature=signature("tdr"),
          definition=function(x, which=1:4, title="", adorn=NULL,
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
              oceDebug(debug, "plot.tdr(..., which=", which, ", ...) {\n", unindent=1)
              if (!inherits(x, "tdr"))
                  stop("method is only for objects of class '", "tdr", "'")
              if (0 == sum(!is.na(x@data$temperature)))
                  stop("no good temperatures to plot")
              if (0 == sum(!is.na(x@data$pressure)))
                  stop("no good pressures to plot")
              dotsNames <- names(list(...))
              ## FIXME: In the below, we could be more clever for single-panel plots
              ## but it may be better to get users out of the habit of supplying xlim
              ## etc (which will yield errors in plot.lm(), for example).
              if ("xlim" %in% dotsNames)
                  stop("in plot.tdr() : 'xlim' not allowed; use tlim (for type=1 or 3) or Tlim (for type=4) ", call.=FALSE)
              if ("ylim" %in% dotsNames)
                  stop("in plot.tdr() : 'ylim' not allowed; use Tlim (for type=1 or 4) or plim (for type=3) ", call.=FALSE)
              nw <- length(which)
              opar <- par(no.readonly = TRUE)
              if (nw > 1)
                  on.exit(par(opar))
              adorn.length <- length(adorn)
              if (adorn.length == 1) {
                  adorn <- rep(adorn, nw)
                  adorn.length <- nw
              }
              if (nw == 2) {
                  layout(cbind(c(1,2)))
              } else if (nw==3 || nw==4) {
                  layout(rbind(c(1,2), c(3,4)), widths=c(2,1))
              }
              par(mgp=mgp, mar=mar)
              which <- ocePmatch(which,
                                 list(temperature=1, text=2, pressure=3, profile=4))
              if (missing(main))
                  main <- rep('', length.out=nw)
              else
                  main <- rep(main, length.out=nw)
              oceDebug(debug, "after nickname-substitution, which=c(", paste(which, collapse=","), ")\n")
              for (w in 1:nw) {
                  oceDebug(debug, "which[", w, "]=", which[w], "\n")
                  if (which[w] == 1) {           # temperature timeseries
                      oce.plot.ts(x@data$time, x@data$temperature,
                                  xlab=if (!missing(xlab))xlab else "",
                                  ylab=if (missing(ylab)) resizableLabel("T", "y") else ylab,
                                  type='l',
                                  xlim=if (missing(tlim)) range(x@data$time, na.rm=TRUE) else tlim,
                                  ylim=if (missing(Tlim)) range(x@data$temperature, na.rm=TRUE) else Tlim,
                                  tformat=tformat,
                                  drawTimeRange=drawTimeRange,
                                  mgp=mgp, mar=mar, main=main[w], ...)
                      ##box()
                      ##oce.axis.POSIXct(1, x=object@data$time, drawTimeRange=drawTimeRange, abbreviateTimeRange=abbreviateTimeRange)
                      drawTimeRange <- FALSE    # only the first time panel gets the time indication
                      axis(2)
                  } else if (which[w] == 3) {    # pressure timeseries
                      oce.plot.ts(x@data$time, x@data$pressure,
                                  xlab=if (!missing(xlab))xlab else "",
                                  ylab=if (missing(ylab)) resizableLabel("p", "y") else ylab,
                                  type='l',
                                  xlim=if (missing(tlim)) range(x@data$time, na.rm=TRUE) else tlim,
                                  ylim=if (missing(plim)) range(x@data$pressure, na.rm=TRUE) else plim,
                                  tformat=tformat,
                                  drawTimeRange=drawTimeRange,
                                  mgp=mgp, mar=mar, main=main[w], ...)
                      ##box()
                      ##oce.axis.POSIXct(1, x=object@data$time, drawTimeRange=drawTimeRange)
                      drawTimeRange <- FALSE
                      ##axis(2)
                  } else if (which[w] == 2) {
                      text.item <- function(item, cex=4/5*par("cex")) {
                          if (!is.null(item) && !is.na(item)) {
                              text(xloc, yloc, item, adj = c(0, 0), cex=cex);
                          }
                      }
                      xfake <- seq(0:10)
                      yfake <- seq(0:10)
                      mar <- par("mar")
                      par(mar=c(0,0,0,0))

                      plot(xfake, yfake, type = "n", xlab = "", ylab = "", axes = FALSE)
                      xloc <- 1
                      yloc <- 10
                      d.yloc <- 0.7
                      cex <- par("cex")
                      text.item(title, cex=1.25*cex)
                      yloc <- yloc - d.yloc
                      ##if (!is.null(object@metadata$filename))
                      ##    text.item(object@metadata$filename, cex=cex)
                      if (!is.null(x@metadata$serialNumber)) {
                          text.item(paste(gettext("Serial Number", domain="R-oce"), x@metadata$serialNumber),cex=cex)
                          yloc <- yloc - d.yloc
                      }
                      if (!(1 %in% which || 2 %in% which)) { # don't bother with these if already on a time-series panel
                          text.item(paste("Start:", x@data$time[1], attr(x@data$time, "tzone")), cex=cex)
                          yloc <- yloc - d.yloc
                          text.item(paste("End:", x@data$time[length(x@data$time)], attr(x@data$time, "tzone")), cex=cex)
                          yloc <- yloc - d.yloc
                          text.item(paste("Sampled interval:", difftime(x@data$time[2], x@data$time[1], units="secs"), "s"),cex=cex)
                          yloc <- yloc - d.yloc
                      }
                      par(mar=mar)
                  } else if (which[w] == 4) {     # "profile"
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
                      np <- length(x@data$pressure)
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
                  if (w <= adorn.length) {
                      t <- try(eval(adorn[w]), silent=TRUE)
                      if (class(t) == "try-error")
                          warning("cannot evaluate adorn[", w, "]\n")
                  }
              }
              oceDebug(debug, "} # plot.tdr()\n", unindent=1)
              invisible()
          })

read.tdr <- function(file, from=1, to, by=1, type, tz=getOption("oceTz"),
                     processingLog, debug=getOption("oceDebug"))
{
    debug <- max(0, min(debug, 2))
    oceDebug(debug, "read.tdr(file=\"", file, "\", from=", format(from), ", to=", if(missing(to))"(not given)" else format(to), ", by=", by, ", tz=\"", tz, "\", ...) {\n", sep="", unindent=1)
    file <- fullFilename(file)
    filename <- file
    if (is.character(file)) {
        file <- file(file, "r")
        on.exit(close(file))
    }
    if (!inherits(file, "connection"))
        stop("'file' must be a character string or connection")
    if (!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }
    oceDebug(debug, "from=", from, "\n")
    from.keep <- from
    measurement.deltat <- 0
    if (is.numeric(from) && from < 1)
        stop("from cannot be an integer less than 1")
    ##from.keep <- from
    if (!missing(to))
        to.keep <- to
    by.keep <- by
    host.time <- 0
    logger.time <- 0
    subsampleStart <- 0
    subsampleEnd <- 0
    subsamplePeriod <- 0
    number.channels <- 0
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
        ## code based on test files and personal communication with RBR:
        ##   2011-10-11 RBR-DEK send test file and schema documentation [preliminary]
        ##   2011-10-12 DEK-RBR query on ordering of time in 'datasets'
        if (from != 1)
            warning("cannot (yet) handle argument 'from' for a ruskin file; using from=1 instead")
        if (by != 1)
            warning("cannot (yet) handle argument 'by' for a ruskin file; using by=1 instead")
        if (!missing(to))
            warning("cannot (yet) handle argument 'to' for a ruskin file; using the whole file")
        cmd <- paste("sqlite3", filename,  "'select * from datasets'")
        d <- read.table(pipe(cmd), sep="|")
        ndatasets <- dim(d)[1]
        if (1 != ndatasets) {
            stop("read.tdr(..., type=\"rbr/rsk\" cannot handle multi-dataset files; this file has ", ndatasets)
        }
        ## ruskin database-schema serial number: hard to decode, so I'll just give up on it
        cmd <- paste("sqlite3", filename,  "'select * from appSettings'")
        ##browser()
        ruskinVersion <- read.table(pipe(cmd), sep="|")[1,2]
        ##print(ruskinVersion)
        ruskinVersion <- as.numeric(strsplit(gsub(".[a-z].*$","",gsub("^.*- *", "",ruskinVersion)),"\\.")[[1]])
        ##print(ruskinVersion)
        if (length(ruskinVersion == 3)) {
            if (!(ruskinVersion[1] == 1 && ruskinVersion[2] == 5 && ruskinVersion[3] == 24))
                warning("making some (untested) assumptions, since the ruskin Version (",
                        paste(ruskinVersion, collapse="."),
                        ") is outside the range for which tests have been done")
        }
        ## atmospheric pressure
        cmd <- paste("sqlite3", filename,  "'select * from deriveDepth'")
        pressureAtmospheric <- read.table(pipe(cmd), sep="|")[1,3]
        ## get the actual data
        cmd <- paste("sqlite3", filename,  "'select * from data order by tstamp'")
        d <- read.table(pipe(cmd), sep="|")
        ## assign column names (probably not needed now, but this may be helpful later)
        cmd <- paste("sqlite3", filename,  "'PRAGMA table_info(data)'")
        names <- read.table(pipe(cmd), sep='|')[,2]
        names(d) <- names
        time <- numberAsPOSIXct(floor(d[,1]/1000), type='unix')
        cmd <- paste("sqlite3", filename,  "'select * from channels order by channelID'")
        channelNames <- read.table(pipe(cmd), sep='|')[,2]
        temperatureColumn <- grep(pattern='temp', x=channelNames, ignore.case=TRUE)
        pressureColumn <- grep(pattern='pres', x=channelNames, ignore.case=TRUE)
        if (length(temperatureColumn)) {
            temperatureColumn <- temperatureColumn + 1
        } else {
            temperatureColumn <- 2
            warning("cannot locate temperature column in 'channels' table of database; assuming column 2")
        }
        if (length(pressureColumn)) {
            pressureColumn <- pressureColumn + 1
        } else {
            pressureColumn <- 3
            warning("cannot locate pressure column in 'channels' table of database; assuming column 3")
        }
        temperature <- d[,temperatureColumn]
        pressure <- d[,pressureColumn]
        cmd <- paste("sqlite3", filename,  "'select * from instruments'")
        serialNumber <- read.table(pipe(cmd), sep='|')[1,1]
        model <- read.table(pipe(cmd), sep='|')[1,2]
    } else {
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
        col.names <- strsplit(gsub("[ ]+"," ", gsub("[ ]*$","",gsub("^[ ]+","",line))), " ")[[1]]
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
    }
    rval <- as.tdr(time, temperature, pressure, instrumentType="rbr",
                  serialNumber=serialNumber, model=model,
                  pressureAtmospheric=pressureAtmospheric,
                  filename=filename,
                  processingLog=paste(deparse(match.call()), sep="", collapse=""),
                  debug=debug-1)
    oceDebug(debug, "} # read.tdr()\n", sep="", unindent=1)
    rval
}


tdrPatm <- function(x, dp=0.5)
{
    p <- if (inherits(x, "tdr")) x@data$pressure else x
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

tdrTrim <- function(x, method="water", parameters=NULL, debug=getOption("oceDebug"))
{
    oceDebug(debug, "tdrTrim() {\n", unindent=1)
    if (!inherits(x, "tdr"))
        stop("method is only for objects of class '", "tdr", "'")
    res <- x
    n <- length(x@data$temperature)
    oceDebug(debug, "dataset has", n, "points\n")
    if (n < 2) {
        warning("too few data to trim tdr record")
    } else {
        which.method <- pmatch(method, c("water", "time", "index"), nomatch=0)
        oceDebug(debug, "using method", which.method, "\n")
        if (which.method == 1) {        # "water"
            keep <- rep(FALSE, n)
            air <- x@data$pressure < 10.5 # NB. standard pressure is 10.1325
            waterIndices <- which(!air)
            b <- 2                      # trim a few descending points
            i.start <- waterIndices[1] + b
            i.stop <- waterIndices[-b + length(waterIndices)]
            keep[i.start:i.stop] <- TRUE
        } else if (which.method == 2) { # "time"
            oceDebug(debug, "trimming to time range ",as.character(parameters[1])," to ", as.character(parameters[2]), "\n")
            keep <- rep(TRUE, n)
            keep[x@data$time < as.POSIXlt(parameters[1])] <- FALSE
            keep[x@data$time > as.POSIXlt(parameters[2])] <- FALSE
        } else if (which.method == 3) { # "index"
            oceDebug(debug, "parameters:",parameters,"\n")
            if (min(parameters) < 1)
                stop("Cannot select indices < 1");
            if (max(parameters) > n)
                stop(paste("Cannot select past end of array, i.e. past ", n))
            keep <- rep(FALSE, n)
            keep[parameters[1]:parameters[2]] <- TRUE
        } else {
            stop("Unknown method")
        }
    }
    for (name in names(x@data))
        res@data[[name]] <- subset(x@data[[name]], keep)
    res@data$pressure <- res@data$pressure - 10.1325 # remove avg sealevel pressure
    res@processingLog <- processingLog(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # tdrTrim()\n", unindent=1)
    res
}
