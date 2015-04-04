setMethod(f="initialize",
          signature="logger",
          definition=function(.Object,time,pressure,temperature,filename="") {
              if (!missing(time)) .Object@data$time <- time
              if (!missing(pressure)) .Object@data$pressure <- pressure
              if (!missing(temperature)) .Object@data$temperature <- temperature
              .Object@metadata$filename <- filename
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'logger' object"
              return(.Object)
          })


setMethod(f="summary",
          signature="logger",
          definition=function(object, ...) {
              cat("Logger Summary\n----------\n", ...)
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
          signature="logger",
          definition=function(x, subset, ...) {
              rval <- new("logger") # start afresh in case x@data is a data.frame
              rval@metadata <- x@metadata
              rval@processingLog <- x@processingLog
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
                  rval@data[[i]] <- x@data[[i]][r]
              }
              names(rval@data) <- names(x@data)
              subsetString <- paste(deparse(substitute(subset)), collapse=" ")
              rval@processingLog <- processingLog(rval@processingLog, paste("subset.logger(x, subset=", subsetString, ")", sep=""))
              rval
          })
 
as.logger <- function(time, temperature, pressure,
                      filename="",
                      instrumentType="rbr",
                      serialNumber="", model="",
                      pressureAtmospheric=NA,
                      processingLog, debug=getOption("oceDebug"))
{
    debug <- min(debug, 1)
    oceDebug(debug, "as.logger(..., filename=\"", filename, "\", serialNumber=\"", serialNumber, "\")\n", sep="", unindent=1)
    if (inherits(time, "ctd")) {
        class(time) <- "logger"
        return(time)
    }
    if (missing(time) || missing(temperature) || missing(pressure))
        stop("must give (at least) time, temperature, and pressure")
    if (!inherits(time, "POSIXt"))
        stop("'time' must be POSIXt")
    time <- as.POSIXct(time)
    if (length(time) != length(temperature))
        stop("lengths of 'time' and 'temperature' must match")
    if (length(time) != length(pressure))
        stop("lengths of 'time' and 'pressure' must match")
    res <- new("logger")
    res@metadata$instrumentType <- instrumentType
    res@metadata$model <- model
    res@metadata$serialNumber <- serialNumber
    res@metadata$filename <- filename
    res@metadata$pressureAtmospheric <- pressureAtmospheric
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    res@processingLog <- processingLog(res@processingLog, processingLog)
    res@data <- list(time=time, pressure=pressure, temperature=temperature)
    oceDebug(debug, "} # as.logger()\n", sep="", unindent=1)
    res
}

setMethod(f="plot",
          signature=signature("logger"),
          definition=function(x, which=c(1, 3, 4), title="", adorn=NULL,
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
              oceDebug(debug, "plot.logger(..., which=", which, ", ...) {\n", unindent=1)
              if (!inherits(x, "logger"))
                  stop("method is only for objects of class '", "logger", "'")
              dotsNames <- names(list(...))
              ## FIXME: In the below, we could be more clever for single-panel plots
              ## but it may be better to get users out of the habit of supplying xlim
              ## etc (which will yield errors in plot.lm(), for example).
              if ("xlim" %in% dotsNames)
                  stop("in plot.logger() : 'xlim' not allowed; use tlim (for type=1 or 3) or Tlim (for type=4) ", call.=FALSE)
              if ("ylim" %in% dotsNames)
                  stop("in plot.logger() : 'ylim' not allowed; use Tlim (for type=1 or 4) or plim (for type=3) ", call.=FALSE)

              ## Trim out plots that we cannot do.
              names <- names(x@data)
              haveTemperature <- ("temperature" %in% names) && any(is.finite(x@data$temperature))
              havePressure <- ("pressure" %in% names) && any(is.finite(x@data$pressure))
              which <- oce.pmatch(which,
                                  list(temperature=1, text=2, pressure=3, profile=4))
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
              if (nw == 2) {
                  layout(cbind(c(1,2)))
              } else if (nw==3 || nw==4) {
                  layout(rbind(c(1,2), c(3,4)), widths=c(2,1))
              }
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
                  }
                  if (w <= adorn.length) {
                      t <- try(eval(adorn[w]), silent=TRUE)
                      if (class(t) == "try-error")
                          warning("cannot evaluate adorn[", w, "]\n")
                  }
              }
              oceDebug(debug, "} # plot.logger()\n", unindent=1)
              invisible()
          })

read.logger <- function(file, from=1, to, by=1, type, tz=getOption("oceTz", default="UTC"),
                        patm=TRUE, processingLog, debug=getOption("oceDebug"))
{
    debug <- max(0, min(debug, 2))
    oceDebug(debug, "read.logger(file=\"", file, "\", from=", format(from), ", to=", if(missing(to))"(not given)" else format(to), ", by=", by, ", tz=\"", tz, "\", ...) {\n", sep="", unindent=1)
    file <- fullFilename(file)
    filename <- file
    if (is.character(file)) {
        if (length(grep(".rsk$", file, ignore.case=TRUE)) && missing(type)) 
            type <- "rsk"
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
        if (!requireNamespace("RSQLite", quietly=TRUE))
            stop('must install.packages("RSQLite") to read logger data')
        if (!requireNamespace("DBI", quietly=TRUE))
            stop('must install.packages("DBI") to read logger data')
        con <- DBI::dbConnect(RSQLite::SQLite(), dbname=filename)

        ## Some, but not all, RSK files have "deployments", but we don't use it anyway.
        ##  deployments <- RSQLite::dbReadTable(con, "deployments")
        
        ## code based on test files and personal communication with RBR:
        ##   2011-10-11 RBR-DEK send test file and schema documentation [preliminary]
        ##   2011-10-12 DEK-RBR query on ordering of time in 'datasets'
        if (from != 1)
            warning("cannot (yet) handle argument 'from' for a ruskin file; using from=1 instead")
        if (by != 1)
            warning("cannot (yet) handle argument 'by' for a ruskin file; using by=1 instead")
        if (!missing(to))
            warning("cannot (yet) handle argument 'to' for a ruskin file; using the whole file")

        ## Some, but not all, RSK files have "datasets"
        #try({
            datasets <- RSQLite::dbReadTable(con, "datasets")
            ndatasets <- dim(datasets)[1]
            if (1 != ndatasets)
                stop("read.logger() cannot handle multi-dataset files; this file has ", ndatasets)
        #}, silent=TRUE)

        ## ruskin database-schema serial number: hard to decode, so I'll just give up on it
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
        warn <- FALSE
        try({ # need to wrap in try() because this can fail
            deriveDepth <- RSQLite::dbReadTable(con, "deriveDepth")
            pressureAtmospheric <- deriveDepth$atmosphericPressure 
            warn <- TRUE
        }, silent=TRUE)
        if (warn)
            warning("non-standard pressureAtmospheric value: ", pressureAtmospheric, "\n")
        ##message("NEW: pressureAtmospheric:", pressureAtmospheric)

        ## From notes in comments above, it seems necessary to order by
        ## timestamp (tstamp). Ordering does not seem to be an option for
        ## dbReadTable(), so we use dbFetch().

        ## First, get time stamp. Note the trick of making it floating-point
        ## to avoid the problem that R lacks 64 bit integers.
        res <- DBI::dbSendQuery(con, "select 1.0*tstamp from data order by tstamp;")
        t1000 <- DBI::dbFetch(res, n=-1)[[1]]
        RSQLite::dbClearResult(res)
        time <- numberAsPOSIXct(as.numeric(t1000) / 1000, type='unix')

        ## Second, get the data; we drop the first column beause we have
        ## time already.
        res <- DBI::dbSendQuery(con, "select * from data order by tstamp;")
        data <- DBI::dbFetch(res, n=-1)[,-1, drop=FALSE]
        DBI::dbClearResult(res)
        ## Get column names from the 'channels' table.
        names <- tolower(RSQLite::dbReadTable(con, "channels")$longName)
        names <- names[1:dim(data)[2]] # sometimes there are more names than data channels
        names(data) <- names
        data <- as.list(data)

        ## message("dim of data: ", paste(dim(data), collapse="x"))
        instruments <- RSQLite::dbReadTable(con, "instruments")
        serialNumber <- instruments$serialID
        model <- instruments$model
        RSQLite::dbDisconnect(con)
        if (is.logical(patm)) {
            if (patm)
                data$pressure <- data$pressure - pressureAtmospheric
        } else {
            data$pressure <- data$pressure - patm
        }

        if (3 == sum(c("conductivity", "temperature", "pressure") %in% names)) {
            conductivity.standard <- 42.914 ## mS/cm conversion factor
            ## warning("assuming conductivity is in mS/cm")
            ## Use an estimate of at-sea pressure; otherwise can get an error of 0.005 in salinity,
            ## as estimated in a real profile extending to 200m.
            p <- data$pressure
            if (is.logical(patm) && !patm) {
                p <- data$pressure - 10.1325 # use a reasonable estimate of in-water pressure
            }
            oceDebug(debug, "head(p):", paste(p[1:3], collapse=" "), "\n")
            if ("salinity" %in% names) {
                S <- data$salinity
            } else {
                warning("computing salinity from conductivity (assumed mS/cm), temperature, and pressure")
                S <- swSCTp(data$conductivity / conductivity.standard, data$temperature, p)
            }
            ctd <- new("ctd", pressure=data$pressure, salinity=S, temperature=data$temperature, filename=filename)
            ctd@data[["time"]] <- time
            ctd@data[["scan"]] <- seq_along(data$pressure)
            ## ctd@processingLog <- processingLog(ctd@processingLog, paste("subtract pressureAtmospheric (", pressureAtmospheric, " dbar) from logger pressure", sep=""))

            ctd@metadata$pressureAtmospheric <- pressureAtmospheric
            ## CR suggests to read "sampleInterval" but I cannot find it from the following
            ##   echo ".dump"|sqlite3 050107_20130620_2245cast4.rsk | grep -i sample
            ## so I just infer it from the data
            ctd@metadata$sampleInterval <- median(diff(as.numeric(ctd@data$time))) 
            ctd@metadata$latitude <- NaN
            ctd@metadata$longitude <- NaN
            ctd@metadata$waterDepth <- max(data$pressure, na.rm=TRUE)
            ## The device may have other channels; add them.  (This includes conductivity.)
            for (name in names) {
                oceDebug(debug, "copying '", name, "' from Ruskin file into ctd object\n", sep="")
                if (!(name %in% c("temperature", "pressure"))) {
                    ctd@data[[name]] <- data[[name]]
                }
            }
            ## Add some metadata directly (FIXME: this is brittle to changes in names of the metadata)
            ctd@metadata$serialNumber <- serialNumber
            ctd@metadata$type <- "RBR"
            ctd@metadata$model <- model
            ctd@metadata$filename <- filename
            oceDebug(debug, "} # read.logger() -- returning a CTD object\n", sep="", unindent=1)
            return(ctd)
        } else {
            rval <- new("logger", time=time, filename=filename)
            for (name in names)
                rval@data[[name]] <- data[[name]]
            oceDebug(debug, "} # read.logger()\n", sep="", unindent=1)
            return(rval)
        }
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
    rval <- as.logger(time, temperature, pressure, instrumentType="rbr",
                      serialNumber=serialNumber, model=model,
                      pressureAtmospheric=pressureAtmospheric,
                      filename=filename,
                      processingLog=paste(deparse(match.call()), sep="", collapse=""),
                      debug=debug-1)
    oceDebug(debug, "} # read.logger()\n", sep="", unindent=1)
    rval
}


loggerPatm <- function(x, dp=0.5)
{
    p <- if (inherits(x, "logger")) x@data$pressure else x
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

loggerTrim <- function(x, method="water", parameters=NULL, debug=getOption("oceDebug"))
{
    oceDebug(debug, "loggerTrim() {\n", unindent=1)
    if (!inherits(x, "logger"))
        stop("method is only for objects of class '", "logger", "'")
    res <- x
    n <- length(x@data$temperature)
    oceDebug(debug, "dataset has", n, "points\n")
    if (n < 2) {
        warning("too few data to trim logger record")
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
    oceDebug(debug, "} # loggerTrim()\n", unindent=1)
    res
}
