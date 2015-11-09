setMethod(f="initialize",
          signature="rsk",
          definition=function(.Object,time,pressure,temperature,filename="") {
              if (!missing(time)) .Object@data$time <- time
              if (!missing(pressure)) .Object@data$pressure <- pressure
              if (!missing(temperature)) .Object@data$temperature <- temperature
              .Object@metadata$filename <- filename
              .Object@metadata$model <- NA
              .Object@metadata$conductivityUnit <- "mS/cm"
              .Object@metadata$temperatureUnit <- "ITS-90"
              .Object@metadata$pressureType <- "absolute"
              .Object@metadata$pressureAtmospheric <- 10.1325
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'rsk' object"
              return(.Object)
          })


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
              cat(sprintf("* Measurements:       %s %s to %s %s sampled at %.4g Hz\n",
                          format(m$tstart), attr(m$tstart, "tzone"),
                          format(m$tend), attr(m$tend, "tzone"),
                          1 / m$deltat))
              d <- object@data
              names <- names(d)
              ndata <- length(names)
              isTime <- names == "time"
              threes <- matrix(nrow=sum(!isTime), ncol=3)
              ii <- 1
              for (i in 1:ndata) {
                  if (isTime[i])
                      next
                  threes[ii,] <- threenum(d[[i]])
                  ii <- ii + 1
              }
              rownames(threes) <- paste("   ", names[!isTime])
              colnames(threes) <- c("Min.", "Mean", "Max.")
              cat("* Statistics of data:\n")
              print(threes, indent='  ')
              processingLogShow(object)
              invisible(NULL)
          })


setMethod(f="subset",
          signature="rsk",
          definition=function(x, subset, ...) {
              rval <- new("rsk") # start afresh in case x@data is a data.frame
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
              rval@processingLog <- processingLogAppend(rval@processingLog, paste("subset.rsk(x, subset=", subsetString, ")", sep=""))
              rval
          })
 
as.rsk <- function(time, columns,
                   filename="", instrumentType="rbr", serialNumber="", model="",
                   sampleInterval,
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
    if (missing(sampleInterval) || is.na(sampleInterval))
        sampleInterval <- median(diff(as.numeric(time)))
        oceDebug(debug, "sampleInterval not provided (or NA), estimating as:", sampleInterval, "s \n")
    res <- new("rsk")
    res@metadata$instrumentType <- instrumentType
    if (nchar(model)) 
        res@metadata$model <-model
    res@metadata$serialNumber <- serialNumber
    res@metadata$filename <- filename
    res@metadata$sampleInterval <- sampleInterval
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
              }
              oceDebug(debug, "} # plot.rsk()\n", unindent=1)
              invisible()
          })

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
    measurement.deltat <- 0
    if (is.numeric(from) && from < 1)
        stop("from cannot be an integer less than 1")
    ##from.keep <- from
    if (!missing(to))
        to.keep <- to
    by.keep <- by
    host.time <- 0
    rsk.time <- 0
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
            stop('must install.packages("RSQLite") to read rsk data')
        if (!requireNamespace("DBI", quietly=TRUE))
            stop('must install.packages("DBI") to read rsk data')
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

        ## Second, get the data; we drop the first column beause we have
        ## time already.
        res <- DBI::dbSendQuery(con, "select * from data order by tstamp;")
        data <- DBI::dbFetch(res, n=-1)[,-1, drop=FALSE]
        DBI::dbClearResult(res)
        ## Get column names from the 'channels' table.
        names <- tolower(RSQLite::dbReadTable(con, "channels")$longName)
        ## FIXME: some longnames have UTF-8 characters, and/or
        ## spaces. Should coerce to ascii with no spaces, or at least
        ## recognize fields and rename (e.g. `dissolved O2` should
        ## just be `oxygen`)
        isMeasured <- RSQLite::dbReadTable(con, "channels")$isMeasured == 1
        names <- names[isMeasured] # only take names of things that are in the data table
        ## Check for duplicated names, and append digits to make unique
        if (sum(duplicated(names)) > 0) {
            for (n in names) {
                dup <- grep(n, names)
                if (dup > 1) { # more than one
                    names[dup] <- paste0(n, c('', seq(2, length(dup))))
                }
            }
        }
        names(data) <- names
        data <- as.list(data)
        instruments <- RSQLite::dbReadTable(con, "instruments")
        serialNumber <- instruments$serialID
        model <- instruments$model
        schedules <- RSQLite::dbReadTable(con, "schedules")
        sampleInterval <- schedules$samplingPeriod
        RSQLite::dbDisconnect(con)
        rval <- new("rsk", time=time, filename=filename)
        for (name in names)
            rval@data[[name]] <- data[[name]]
        if ("pressure" %in% names) { # possibly compute sea pressure
            if (is.logical(patm)) {
                if (patm) {
                    rval@data$pressureOriginal <- rval@data$pressure
                    rval@data$pressure <- rval@data$pressure - 10.1325
                    ## No need to check patm=FALSE case because object default is "absolute"
                    rval@metadata$pressureType <- "sea, assuming standard atmospheric pressure 10.1325 dbar"
                    oceDebug(debug, "patm=TRUE, so removing std atmospheric pressure, 10.1325 dbar\n")
                }
            } else if (is.numeric(patm)) {
                npatm <- length(patm)
                if (1 < npatm && npatm != length(pressure))
                    stop("if patm is numeric, its length must equal 1, or the length(pressure).")
                oceDebug(debug, "patm is numeric, so removing std atmospheric pressure, 10.1325 dbar\n")
                rval@data$pressureOriginal <- rval@data$pressure
                rval@data$pressure <- rval@data$pressure - patm
                if (npatm == 1)
                    rval@metadata$pressureType <- sprintf("sea, assuming provided atmospheric pressure %f", patm)
                else 
                    rval@metadata$pressureType <- sprintf("sea, assuming provided atmospheric pressure %f, %f, ...", patm[1], patm[2])
            } else {
                stop("patm must be logical or numeric")
            }
        }
        rval@metadata$model <- model
        rval@metadata$serialNumber <- serialNumber
        ## CR suggests to read "sampleInterval" but I cannot find it from the following
        ##   echo ".dump" | sqlite3 cast4.rsk | grep -i sample
        ## so I just infer it from the data
        ## rval@metadata$sampleInterval <- median(diff(as.numeric(rval@data$time))) 
        rval@metadata$sampleInterval <- sampleInterval
        rval@metadata[["conductivityUnit"]] <- "mS/cm" # FIXME: will this work for all RBR rsks?
        rval@metadata$pressureAtmospheric <- pressureAtmospheric
        rval@processingLog <- processingLogAppend(rval@processingLog, paste(deparse(match.call()), sep="", collapse=""))
        oceDebug(debug, "} # read.rsk()\n", sep="", unindent=1)
        return(rval)
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
        rval <- as.rsk(time, columns=channelsSub,
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
        rval <- as.rsk(time, columns=list(temperature=temperature, pressure=pressure),
                       instrumentType="rbr",
                       serialNumber=serialNumber, model=model,
                       filename=filename,
                       debug=debug-1)
    }
    if (is.logical(patm)) {
        if (patm) {
            rval@data$pressureOriginal <- rval@data$pressure
            rval@data$pressure <- rval@data$pressure - 10.1325
            ## No need to check patm=FALSE case because object default is "absolute"
            rval@metadata$pressureType <- "sea, assuming standard atmospheric pressure 10.1325"
        }
    } else if (is.numeric(patm)) {
        rval@data$pressureOriginal <- rval@data$pressure
        rval@data$pressure <- rval@data$pressure - patm[1]
        rval@metadata$pressureType <- sprintf("sea, assuming provided atmospheric pressure %f", patm[1])
    } else {
        stop("patm must be logical or numeric")
    }
    rval@processingLog <- processingLogAppend(rval@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # read.rsk()\n", sep="", unindent=1)
    rval
}


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
