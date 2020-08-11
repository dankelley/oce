## vim:textwidth=100:expandtab:shiftwidth=4:softtabstop=4


#' Class to Store Current Meter Data
#'
#' This class stores current meter data, e.g. from an Interocean/S4 device
#' or an Aanderaa/RCM device.
#'
#' @templateVar class cm
#'
#' @templateVar dataExample The key items stored in this slot are `time`, `u` and `v`.
#'
#' @templateVar metadataExample {}
#'
#' @template slot_summary
#'
#' @template slot_put
#'
#' @template slot_get
#'
#' @family things related to cm data
#' @family classes provided by oce
#'
#' @author Dan Kelley
setClass("cm", contains="oce")

#' A Current Meter (cm) Object
#'
#' The result of using [read.cm()] on a current meter file holding measurements made with an
#' Interocean S4 device.  See [read.cm()] for some general cautionary notes on reading such
#' files. Note that the salinities in this sample dataset are known to be incorrect, perhaps
#' owing to a lack of calibration of an old instrument that had not been used in a long time.
#'
#' @name cm
#'
#' @docType data
#'
#' @usage data(cm)
#'
#' @examples
#'\dontrun{
#' library(oce)
#' data(cm)
#' summary(cm)
#' plot(cm)
#'}
#' @family datasets provided with oce
#' @family things related to cm data
NULL

#' Extract Something From a CM Object
#'
#' @param x a [cm-class] object.
#'
#' @template sub_subTemplate
#'
#' @family things related to cm data
setMethod(f="[[",
          signature(x="cm", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              callNextMethod()         # [[
          })

#' Replace Parts of a CM Object
#'
#' @param x a [cm-class] object.
#'
#' @template sub_subsetTemplate
#'
#' @family things related to cm data
setMethod(f="[[<-",
          signature(x="cm", i="ANY", j="ANY"),
          definition=function(x, i, j, ..., value) {
              callNextMethod(x=x, i=i, j=j, ...=..., value=value) # [[<-
          })

setMethod(f="initialize",
          signature="cm",
          definition=function(.Object, time=NULL, u=NULL, v=NULL, units, ...) {
              .Object <- callNextMethod(.Object, ...)
              ## sample, direction, conductivity, salinity, temperature, pressure) {
              if (missing(units)) {
                  .Object@metadata$units <- list(u=list(unit=expression(m/s), scale=""),
                                                 v=list(unit=expression(m/s), scale=""))
                  ## salinity=list(unit=expression(), scale="PSS-78"),
                  ##                                conductivity=list(unit=expression(), scale=""),
                  ##                                pressure=list(unit=expression(dbar), scale=""),
                  ##                                depth=list(unit=expression(m), scale=""))
              } else {
                  .Object@metadata$units <- units # CAUTION: we are being quite trusting here
              }
              ##.Object@metadata$filename <- filename
              ##.Object@metadata$units$temperature <- list(unit=expression(degree*C), scale="ITS-90") # guess on the unit
              ##.Object@metadata$units$conductivity <- list(unit=expression(mS/cm), scale="")
              ## .Object@data$sample <- if (missing(sample)) NULL else sample
              .Object@data$time <- time
              .Object@data$u <- u
              .Object@data$v <- v
              ## .Object@data$direction <- if (missing(direction)) NULL else direction
              ## .Object@data$conductivity <- if (missing(conductivity)) NULL else conductivity
              ## .Object@data$salinity <- if (missing(salinity)) NULL else salinity
              ## .Object@data$temperature <- if (missing(temperature)) NULL else temperature
              ## .Object@data$pressure <- if (missing(pressure)) NULL else pressure
              .Object@processingLog$time <- presentTime()
              .Object@processingLog$value <- "create 'cm' object"
              return(.Object)
          })

#' Summarize a CM Object
#'
#' Summarizes some of the data in a `cm` object, presenting such information
#' as the station name, sampling location, data ranges, etc.
#'
#' @param object A [cm-class] object.
#'
#' @param ... Further arguments passed to or from other methods.
#'
#' @seealso The documentation for the [cm-class] class explains the structure
#' of `cm` objects, and also outlines the other functions dealing with them.
#'
#' @family things related to cm data
#'
#' @author Dan Kelley
setMethod(f="summary",
          signature="cm",
          definition=function(object, ...) {
              cat("Cm summary\n----------\n\n", ...)
              showMetadataItem(object, "filename",      "File source:        ", quote=TRUE)
              showMetadataItem(object, "type",          "Instrument type:    ")
              showMetadataItem(object, "model",         "Instrument model:   ")
              showMetadataItem(object, "serialNumber",  "Serial Number:      ")
              showMetadataItem(object, "version",       "Version:            ")
              invisible(callNextMethod()) # summary
          })


#' Subset a CM Object
#'
#' This function is somewhat analogous to [subset.data.frame()].
#'
#' @param x a [cm-class] object.
#'
#' @param subset a condition to be applied to the `data` portion of `x`.
#' See \sQuote{Details}.
#'
#' @param ... ignored.
#'
#' @return A new `cm` object.
#'
#' @examples
#' library(oce)
#' data(cm)
#' plot(cm)
#' plot(subset(cm, time < mean(range(cm[['time']]))))
#'
#' @family things related to cm data
#' @family functions that subset oce objects
#'
#' @author Dan Kelley
setMethod(f="subset",
          signature="cm",
          definition=function(x, subset, ...) {
              subsetString <- paste(deparse(substitute(expr=subset, env=environment())), collapse=" ")
              res <- x
              dots <- list(...)
              debug <- getOption("oceDebug")
              if (length(dots) && ("debug" %in% names(dots)))
                  debug <- dots$debug
              if (missing(subset))
                  stop("must give 'subset'")
              if (length(grep("time", subsetString))) {
                  oceDebug(debug, "subsetting a cm by time\n")
                  keep <- eval(expr=substitute(expr=subset, env=environment()), envir=x@data, enclos=parent.frame(2))
                  names <- names(x@data)
                  oceDebug(debug, vectorShow(keep, "keeping bins:"))
                  oceDebug(debug, "number of kept bins:", sum(keep), "\n")
                  if (sum(keep) < 2)
                      stop("must keep at least 2 profiles")
                  res <- x
                  ## FIXME: are we handling slow timescale data?
                  for (name in names(x@data)) {
                      if (name == "time" || is.vector(x@data[[name]])) {
                          oceDebug(debug, "subsetting x@data$", name, ", which is a vector\n", sep="")
                          res@data[[name]] <- x@data[[name]][keep] # FIXME: what about fast/slow
                      } else if (is.matrix(x@data[[name]])) {
                          oceDebug(debug, "subsetting x@data$", name, ", which is a matrix\n", sep="")
                          res@data[[name]] <- x@data[[name]][keep, ]
                      } else if (is.array(x@data[[name]])) {
                          oceDebug(debug, "subsetting x@data$", name, ", which is an array\n", sep="")
                          res@data[[name]] <- x@data[[name]][keep, , , drop=FALSE]
                      }
                  }
              }
              res@processingLog <- processingLogAppend(res@processingLog, paste("subset.cm(x, subset=", subsetString, ")", sep=""))
              res
          })


#' Coerce data into a CM object
#'
#' @param time A vector of times of observation, or an `oce` object that holds `time`,
#' in addition to either both `u` and `v`, or both `directionTrue`
#' and `speedHorizontal`.
#'
#' @param u either a numerical vector containing the eastward component of velocity, in m/s,
#' or an `oce` object that can can be coerced into a `cm` object. In the second
#' case, the other arguments to the present function are ignored.
#'
#' @param v vector containing the northward component of velocity in m/s.
#'
#' @param pressure vector containing pressure in dbar. Ignored if the first argument
#' contains an `oce` object holding pressure.
#'
#' @param conductivity Optional vector of conductivity.
#' Ignored if the first argument contains an `oce` object holding pressure.
#'
#' @param salinity Optional vector of salinity, assumed to be Practical Salinity.
#' Ignored if the first argument contains an `oce` object holding salinity
#'
#' @param temperature Optional vector of temperature.
#' Ignored if the first argument contains an `oce` object holding temperature
#'
#' @param longitude Optional longitude in degrees East.
#' Ignored if the first argument contains an `oce` object holding longitude.
#'
#' @param latitude Latitude in degrees North.
#' Ignored if the first argument contains an `oce` object holding latitude.
#'
#' @param filename Optional source file name
#'
#' @template debugTemplate
#'
#' @family things related to cm data
as.cm <- function(time, u=NULL, v=NULL,
                  pressure=NULL, conductivity=NULL, temperature=NULL, salinity=NULL,
                  longitude=NA, latitude=NA, filename="", debug=getOption("oceDebug"))
{
    oceDebug(debug, "as.ctd() {\n", unindent=1)
    rpd <- atan2(1, 1) / 45            # radians per degree (avoid 'pi')
    ## Catch special cases
    firstArgIsOce <- inherits(time, "oce")
    if (firstArgIsOce) {
        x <- time
        mnames <- names(x@metadata)
        dnames <- names(x@data)
        if (!("time" %in% dnames))
            stop("first argument is an oce object, but it does not hold time")
        time <- x@data$time
        if ("pressure" %in% dnames)
            pressure <- x@data$pressure
        if ("conductivity" %in% dnames)
            conductivity <- x@data$conductivity
        if ("temperature" %in% dnames)
            temperature <- x@data$temperature
        if ("salinity" %in% dnames)
            salinity <- x@data$salinity
        if ("longitude" %in% mnames)
            longitude <- x@metadata$longitude
        if ("latitude" %in% mnames)
            latitude <- x@metadata$latitude
        if ("filename" %in% mnames)
            filename <- x@metadata$filename
        if ("u" %in% dnames && "v" %in% dnames) {
            u <- x@data$u
            v <- x@data$v
        } else if ("speedHorizontal" %in% dnames && "directionTrue" %in% dnames) {
            ## NOTE: this can be generalized later to take e.g. 'speed', if some objects have that
            u <- x@data$speedHorizontal * cos(rpd * (90-x@data$directionTrue))
            v <- x@data$speedHorizontal * sin(rpd * (90-x@data$directionTrue))
        } else if ("speedHorizontal" %in% dnames && "direction" %in% dnames) {
            u <- x@data$speedHorizontal * cos(rpd * (90-x@data$direction))
            v <- x@data$speedHorizontal * sin(rpd * (90-x@data$direction))
        } else {
            stop("first argument must hold either 'u' plus 'v' or 'speed' plus 'directionTrue' or 'direction'")
        }
    }
    direction <- 90 - atan2(v, u) / rpd
    direction <- ifelse(direction < 0, 360+direction, direction) # put in range 0 to 360
    res <- new("cm", time=time, u=u, v=v)
    res@metadata$filename <- filename
    res@metadata$longitude <- longitude
    res@metadata$latitude <- latitude
    if (!is.null(conductivity)) {
        res@data$conductivity <- conductivity
        res@metadata$units$conductivity <- list(unit=expression(mS/cm), scale="") # guessing on unit
    }
    if (!is.null(salinity)) {
        res@data$salinity <- salinity
        res@metadata$units$salinity <- list(unit=expression(), scale="PSS-78")
    }
    if (!is.null(temperature)) {
        res@data$temperature <- temperature
        res@metadata$units$temperature <- list(unit=expression(degree*C), scale="ITS-90")
    }
    if (!is.null(pressure)) {
        res@data$pressure <- pressure
        res@metadata$units$pressure <- list(unit=expression(dbar), scale="")
    }

    if (firstArgIsOce) {
        ## Copy some metadata that are used sometimes (esp. ODF files)
        if ("type" %in% mnames)
            res@metadata$type <- x@metadata$type
        if ("model" %in% mnames)
            res@metadata$model <- x@metadata$model
        if ("serialNumber" %in% mnames)
            res@metadata$serialNumber <- x@metadata$serialNumber
        if ("ship" %in% mnames)
            res@metadata$ship <- x@metadata$ship
        if ("scientist" %in% mnames)
            res@metadata$scientist <- x@metadata$scientist
        if ("cruise" %in% mnames)
            res@metadata$cruise <- x@metadata$cruise
        if ("station" %in% mnames)
            res@metadata$station <- x@metadata$station
        if ("countryInstituteCode" %in% mnames)
            res@metadata$countryInstituteCode <- x@metadata$countryInstituteCode
        if ("cruiseNumber" %in% mnames)
            res@metadata$cruiseNumber <- x@metadata$cruiseNumber
        if ("waterDepth" %in% mnames)
            res@metadata$waterDepth <- x@metadata$waterDepth
        ## determine original names, where known
        if ("dataNamesOriginal" %in% mnames) {
            if (is.list(x@metadata$dataNamesOriginal)) {
                res@metadata$dataNamesOriginal <- x@metadata$dataNamesOriginal
            } else {
                nameMapping <- as.list(x@metadata$dataNamesOriginal)
                names(nameMapping) <- names(x@data)
                res@metadata$dataNamesOriginal <- lapply(names(res@data), function(name) if (is.null(nameMapping[[name]])) "-" else nameMapping[[name]])
            }
        } else {
            res@metadata$dataNamesOriginal <- NULL
        }
    }
    oceDebug(debug, "} # as.cm()\n", unindent=1)
    res
}


#' Read a CM file
#'
#' Read a current-meter data file, producing a [cm-class] object.
#'
#' There function has been tested on only a single file, and the data-scanning
#' algorithm was based on visual inspection of that file.  Whether it will work
#' generally is an open question. It should be noted that the sample file had
#' several odd characteristics, some of which are listed below.
#'
#' * file contained two columns named `"Cond"`, which was guessed
#' to stand for conductivity. Since only the first contained data, the second was
#' ignored, but this may not be the case for all files.
#'
#' * The unit for `"Cond"` was stated in the file to be `"mS"`,
#' which makes no sense, so the unit was assumed to be mS/cm.
#'
#' * The file contained a column named `"T-Temp"`, which is not
#' something the author has seen in his career. It was assumed to stand for
#' in-situ temperature.
#'
#' * The file contained a column named `"Depth"`, which is not something
#' an instrument can measure. Presumably it was calculated from pressure (with
#' what atmospheric offset, though?) and so pressure was inferred from it using
#' [swPressure()].
#'
#' * The file contained several columns that lacked names. These were ignored.
#'
#' * The file contained several columns that seem to be derived from the
#' actual measured data, such as `"Speed"`, `"Dir"`, `"N-S Dist"`,
#' etc. These are ignored.
#'
#' * The file contained several columns that were basically a mystery to the
#' author, e.g. `"Hx"`, `"Hy"`, `"Vref"`, etc. These were ignored.
#'
#' Based on such considerations, [read.cm()] reads only the columns that
#' were reasonably well-understood based on the sample file. Users who need more
#' columns should contact the author. And a user who could produce a document
#' explaining the data format would be especially appreciated!
#'
#' @param file a connection or a character string giving the name of the file to
#' load.
#'
#' @param from index number of the first measurement to be read, or the time of
#' that measurement, as created with [as.POSIXct()] (hint: use
#' `tz="UTC"`).
#'
#' @param to indication of the last measurement to read, in a format matching that
#' of `from`.
#'
#' @param by an indication of the stride length to use while walking through the
#' file. If this is an integer, then `by-1` measurements are skipped between
#' each pair of profiles that is read. This may not make much sense, if the data
#' are not equi-spaced in time.  If `by` is a string representing a time
#' interval, in colon-separated format, then this interval is divided by the
#' sampling interval, to get the stride length. *BUG:* if the data are not
#' equi-spaced, then odd results will occur.
#'
#' @param longitude optional signed number indicating the longitude in degrees
#' East.
#'
#' @param latitude optional signed number indicating the latitude in degrees North.
#'
#' @param type character string indicating type of file (ignored at present).
#'
#' @param tz character string indicating time zone to be assumed in the data.
#'
#' @param debug a flag that turns on debugging.  The value indicates the depth
#' within the call stack to which debugging applies.
#'
#' @param monitor ignored.
#'
#' @param processingLog if provided, the action item to be stored in the log.  This
#' parameter is typically only provided for internal calls; the default that it
#' provides is better for normal calls by a user.
#'
#' @return An [cm-class] object.
#'
#' The `data` slot will contain all the data in the file, with names
#' determined from the tokens in line 3 in that file, passed through
#' [make.names()], except that
#' `Vnorth` is renamed `v` (after conversion from cm/s to m/s),
#' `Veast` is renamed `u` (after conversion from cm/s to m/s),
#' `Cond` is renamed `conductivity`,
#' `T.Temp` is renamed `temperature`
#' and
#' `Sal` is renamed `salinity`, and a new
#' column named `time` (a POSIX time) is constructed
#' from the information in the file header, and another named
#' `pressure` is constructed from the column named `Depth`.
#' At least in the single file studied in the creation of this function,
#' there are some columns that are unnamed in line 3 of the header;
#' these yield data items with names `X`, `X.1`, etc.
#'
#' @section Historical note:
#' Prior to late July, 2016, the direction of current flow was stored in the
#' return value, but it is no longer stored, since it can be derived from the
#' `u` and `v` values.
#'
#' @examples
#'\dontrun{
#'   library(oce)
#'   cm <- read.oce("cm_interocean_0811786.s4a.tab")
#'   summary(cm)
#'   plot(cm)
#'}
#'
#' @family things related to cm data
#'
#' @author Dan Kelley
read.cm <- function(file, from=1, to, by=1, tz=getOption("oceTz"),
                    type=c("s4"),
                    longitude=NA, latitude=NA,
                    debug=getOption("oceDebug"), monitor=FALSE, processingLog)
{
    if (!missing(file) && is.character(file) && 0 == file.info(file)$size)
        stop("empty file")
    oceDebug(debug, "read.cm(file=\"", file,
              "\", from=", format(from),
              ", to=", if (missing(to)) "(missing)" else format(to), ", by=", by, "type=", type, ", ...) {\n", sep="", unindent=1)
    type <- match.arg(type)
    if (type == "s4")
        read.cm.s4(file=file, from=from, to=to, by=by, tz=tz,
                   longitude=longitude, latitude=latitude,
                   debug=debug-1, monitor=monitor, processingLog=processingLog)
    else
        stop("unknown type of current meter")
}

read.cm.s4 <- function(file, from=1, to, by=1, tz=getOption("oceTz"),
                       longitude=NA, latitude=NA,
                       debug=getOption("oceDebug"), monitor=FALSE, processingLog)
{
    if (!missing(file) && is.character(file) && 0 == file.info(file)$size)
        stop("empty file")
    if (debug > 1)
        debug <- 1
    oceDebug(debug, "read.cm.s4(file=\"", file,
              "\", from=", format(from),
              ", to=", if (missing(to)) "(missing)" else format(to), ", by=", by, ", ...) {\n", sep="", unindent=1)
    if (is.character(file)) {
        filename <- fullFilename(file)
        file <- file(file, "r")
        on.exit(close(file))
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        filename <- "(connection)"
        open(file, "rb")
        on.exit(close(file))
    }
    ## Examine the first line of the file, to get serial number, etc.
    items <- scan(file, "character", nlines=1, sep="\t", quiet=TRUE) # slow, but just one line
    oceDebug(debug, "line 1 contains: ", paste(items, collapse=" "), "\n")
    serialNumber <- "unknown"
    version <- "unknown"
    type <- "unknown"
    for (i in 1:(-1 + length(items))) {
        if (length(grep("Serial", items[i])))
            serialNumber <- items[i+1]
        else if (length(grep("Version", items[i])))
            version <- items[i+1]
        else if (length(grep("Type", items[i])))
            type <- items[i+1]
    }
    ## Skip through the rest of the header, and start paying attention when
    ## row number is 1, 2, and then 3.  These first rows give us the time
    ## sequence.
    lines <- readLines(file, n=20)
    for (i in 2:20) {
        items <- strsplit(lines[i], "\t")[[1]]
        oceDebug(debug, "line", i, "contains: ", paste(items, collapse=" "), "\n")
        if (items[1] == "Sample #") {
            ## names <- sub('[ ]+$', '', sub('^[ ]+','', items))
            ## names <- ifelse(0 == nchar(names), paste("column", seq_along(names), sep=""), names)
        } else if (items[1] == "1") {
            start.day <- items[2]
        } else if (items[1] == "2") {
            start.hms <- items[3]
        } else if (items[1] == "3") {
            t0 <- strptime(paste(start.day, start.hms), format="%m/%d/%Y %H:%M:%S", tz=tz)
            t1 <- strptime(paste(start.day, items[3]), format="%m/%d/%Y %H:%M:%S", tz=tz)
            deltat <- as.numeric(t1) - as.numeric(t0)
            break
        }
    }
    ## Change names of known items to oce-style names, but leave others
    ## as they are, in case the user knows what they mean
    dnames <- strsplit(lines[3], '\t')[[1]]
    dnames <- gsub(" *$", "", gsub("^ *", "", dnames)) # remove leading/trailing blanks
    namesOriginal <- dnames
    dnames <- make.names(dnames, unique=TRUE)       # handle duplicated names
    ##dnames[dnames=="Sample.."] <- "sample"
    ##dnames[dnames=="Date"] <- "date"
    ##dnames[dnames=="Time"] <- "time"
    dnames[dnames=="decS"] <- "decS"
    dnames[dnames=="Vnorth"] <- "v"
    dnames[dnames=="Veast"] <- "u"
    ##dnames[dnames=="Speed"] <- "speed"
    ##dnames[dnames=="Dir"] <- "direction"
    ##dnames[dnames=="Vref"] <- "Vref"
    ##dnames[dnames=="Hx"] <- "hx"
    ##dnames[dnames=="Hy"] <- "hy"
    dnames[dnames=="Cond"] <- "conductivity"
    dnames[dnames=="T.Temp"] <- "temperature"
    ##dnames[dnames=="Depth"] <- "depth"
    ##dnames[dnames=="Hdg"] <- "heading"
    dnames[dnames=="Sal"] <- "salinity"
    ##dnames[dnames=="Dens"] <- "density"
    ##dnames[dnames=="SV"] <- "soundVelocity"
    ##dnames[dnames=="N.S.Dist"] <- "NSDist"
    ##dnames[dnames=="E.W.Dist"] <- "EWDist"
    ##dnames[dnames=="SRB.Date"] <- "SRBDate"
    ##dnames[dnames=="SRB.Time"] <- "SRBTime"
    ##dnames[dnames=="Vref.1"] <- "Vref2"
    ##dnames[dnames=="Hx.1"] <- "Hx2"
    ##dnames[dnames=="Hy.1"] <- "Hy2"
    ##dnames[dnames=="Cond.1"] <- "conductivity2"
    ##dnames[dnames=="T-Temp.1"] <- "temperature2"
    ##dnames[dnames=="Depth.1"] <- "depth2"
    ##dnames[dnames=="Sal.1"] <- "salinity2"
    ##dnames[dnames=="Dens.1"] <- "density2"
    ##dnames[dnames=="SV.1"] <- "soundVelocity2"

    # Finally, read the data and chop last 2 lines (which contain footer info
    pushBack(lines, file)
    d <- read.delim(file, skip=5, sep='\t', col.names=dnames, stringsAsFactors=FALSE)
    d <- d[seq.int(1L, dim(d)[1]-2), ]

    res <- new("cm") # will fill in later
    ## namesOriginal[namesOriginal==""] <- "-"
    dataNamesOriginal <- as.list(namesOriginal)
    names(dataNamesOriginal) <- dnames
    res@metadata$dataNamesOriginal <- dataNamesOriginal
    d$u <- d$u / 100
    d$v <- d$v / 100
    d$pressure <- swPressure(d$Depth, eos="gsw") # gsw is faster than unesco with essentially same results
    res@metadata$dataNamesOriginal$pressure <- "-"

    n <- length(d$u)
    time <- seq(t0, by=deltat, length.out=n)
    if (inherits(from, "POSIXt")) {
        if (!inherits(to, "POSIXt"))
            stop("if 'from' is POSIXt, then 'to' must be, also")
        if (!is.numeric(by) || by != 1)
            stop("sorry, 'by' must equal 1, in this version of read.cm.s4()")
        ##from.to.POSIX <- TRUE
        from.index <- which(time >= from)[1]
        if (is.na(from.index))
            from.index <- 1
        to.index <- which(to <= time)[1]
        if (is.na(to.index))
            to.index <- n
        oceDebug(debug, "Time-based trimming: from=", format(from), "to=", format(to), "yield from.index=", from.index, "and to.index=", to.index, "\n")
        keep <- seq(from.index, to.index)
    } else {
        if (!is.numeric(from))
            stop("'from' must be either POSIXt or numeric")
        to <- n
        if (!is.numeric(to))
            stop("'to' must be either POSIXt or numeric")
        keep <- seq(from, to)
    }
    keep <- keep[1 <= keep]
    keep <- keep[keep <= n]
    d$time <- time
    d <- d[keep, ]
    d <- as.list(d)
    for (dname in names(d)) {
        ##message("dname: ", dname)
        if (dname != "Date" && dname != "Time" && dname != "time" && dname != "SRB.Time")
            d[[dname]] <- as.numeric(d[[dname]])
        if (length(grep("^X[.0-9]*$", dname)))
            res@metadata$dataNamesOriginal[[dname]] <- "-"
    }
    res@data <- d
    ## res <- new("cm", sample=as.numeric(sample[keep]), time=time[keep],
    ##            u=u[keep], v=v[keep], direction=direction[keep],
    ##            conductivity=conductivity[keep],
    ##            salinity=salinity[keep], temperature=temperature[keep], pressure=pressure[keep])
    res@metadata$filename <- filename
    res@metadata$serialNumber <- serialNumber
    res@metadata$version <- version
    res@metadata$type <- type
    res@metadata$longitude <- longitude
    res@metadata$latitude <- latitude
    ## res@metadata$units$u <- list(unit=expression(m/s), scale="")
    ## res@metadata$units$v <- list(unit=expression(m/s), scale="")
    res@metadata$units$conductivity <- list(unit=expression(mS/cm), scale="")
    res@metadata$units$salinity <- list(unit=expression(), scale="PSS-78")
    res@metadata$units$temperature <- list(unit=expression(degree*C), scale="ITS-90")
    res@metadata$units$pressure <- list(unit=expression(dbar), scale="")
    ## res@metadata$units$pressure <- list(unit=expression(dbar), scale="")
    ## res@metadata$units$direction <- list(unit=expression(degree), scale="")
    if (missing(processingLog)) processingLog <- paste(deparse(match.call()), sep="", collapse="")
    res@processingLog <- processingLogAppend(res@processingLog, processingLog)
    oceDebug(debug, "} # read.cm()\n", unindent=1)
    res
}


#' Plot a cm Object
#'
#' Creates a multi-panel summary plot of data measured by a current meter.
#'
#' The panels are controlled by the `which` argument, as follows.
#'
#' * `which=1` or `which="u"` for a time-series graph of eastward
#' velocity, `u`, as a function of time.
#'
#' * `which=2` or `which="v"` for a time-series graph of
#' northward velocity, `u`, as a function of time.
#'
#' * `which=3` or `"progressive vector"` for progressive-vector
#' plot
#'
#' * `which=4` or `"uv"` for a plot of `v` versus `u`.
#' (Dots are used for small datasets, and smoothScatter for large ones.)
#'
#' * `which=5` or `"uv+ellipse"` as the `"uv"` case, but
#' with an added indication of the tidal ellipse, calculated from the eigen
#' vectors of the covariance matrix.
#'
#' * `which=6` or `"uv+ellipse+arrow"` as the `"uv+ellipse"`
#' case, but with an added arrow indicating the mean current.
#'
#' * `which=7` or `"pressure"` for pressure
#'
#' * `which=8` or `"salinity"` for salinity
#'
#' * `which=9` or `"temperature"` for temperature
#'
#' * `which=10` or `"TS"` for a TS diagram
#'
#' * `which=11` or `"conductivity"` for conductivity
#'
#' * `which=20` or `"direction"` for the direction of flow
#'
#' @param x a [cm-class] object.
#'
#' @param which list of desired plot types.  These are graphed in panels running
#' down from the top of the page.  See \dQuote{Details} for the meanings of various
#' values of `which`.
#'
#' @param type type of plot, as for [plot()].
#'
#' @param drawTimeRange boolean that applies to panels with time as the horizontal
#' axis, indicating whether to draw the time range in the top-left margin of the
#' plot.
#'
#' @param drawZeroLine boolean that indicates whether to draw zero lines on
#' velocities.
#'
#' @param mgp 3-element numerical vector to use for `par(mgp)`, and also for
#' `par(mar)`, computed from this.  The default is tighter than the R default,
#' in order to use more space for the data and less for the axes.
#'
#' @param mar value to be used with [`par`]`("mar")`.
#'
#' @param small an integer indicating the size of data set to be considered
#' "small", to be plotted with points or lines using the standard
#' [plot()] function.  Data sets with more than `small` points will
#' be plotted with [smoothScatter()] instead.
#'
#' @param main main title for plot, used just on the top panel, if there are
#' several panels.
#'
#' @param tformat optional argument passed to [oce.plot.ts()], for plot
#' types that call that function.  (See [strptime()] for the format
#' used.)
#'
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate amount
#' of debugging information, or to 2 to get more.
#'
#' @param ... Optional arguments passed to plotting functions.
#'
#' @examples
#'   library(oce)
#'   data(cm)
#'   summary(cm)
#'   plot(cm)
#'
#' @family functions that plot oce data
#' @family things related to cm data
#'
#' @aliases plot.cm
#'
#' @author Dan Kelley
setMethod(f="plot",
          signature=signature("cm"),
          definition=function(x,
                              which=c(1:2, 7:9),
                              type="l",
                              drawTimeRange=getOption("oceDrawTimeRange"),
                              drawZeroLine=FALSE,
                              mgp=getOption("oceMgp"),
                              mar=c(mgp[1]+1.5, mgp[1]+1.5, 1.5, 1.5),
                              small=2000,
                              main="",
                              tformat,
                              debug=getOption("oceDebug"),
                              ...)
          {
              oceDebug(debug, "plot.cm() {\n", unindent=1)
              oceDebug(debug, "  par(mar)=", paste(par('mar'), collapse=" "), "\n")
              oceDebug(debug, "  par(mai)=", paste(par('mai'), collapse=" "), "\n")
              if (3 != sum(c("time", "u", "v") %in% names(x@data))) {
                  warning("In plot,cm-method() :\n  cannot plot a cm object unless its 'data' slot contains 'time', 'u' and 'v'", call.=FALSE)
                  return(invisible(NULL))
              }
              opar <- par(no.readonly = TRUE)
              lw <- length(which)
              oceDebug(debug, "length(which) =", lw, "\n")
              if (lw > 1)
                  on.exit(par(opar))
              par(mgp=mgp, mar=mar)
              dots <- list(...)
              ##gave.ylim <- "ylim" %in% names(dots)
              ##ylim.given <- if (gave.ylim) dots[["ylim"]] else NULL

              oceDebug(debug, "later on in plot,cm-method:\n")
              oceDebug(debug, "  par(mar)=", paste(par('mar'), collapse=" "), "\n")
              oceDebug(debug, "  par(mai)=", paste(par('mai'), collapse=" "), "\n")
              oceDebug(debug, "which:", which, "\n")
              which <- oce.pmatch(which,
                                  list(u=1, v=2, "progressive vector"=3,
                                       "uv"=4, "uv+ellipse"=5, "uv+ellipse+arrow"=6,
                                       pressure=7, salinity=8, temperature=9, TS=10, conductivity=11,
                                       direction=20))
              oceDebug(debug, "which:", which, "\n")

              tt <- x@data$time
              class(tt) <- "POSIXct"              # otherwise image() gives warnings
              if (lw > 1) {
                  par(mfrow=c(lw, 1))
                  oceDebug(debug, "calling par(mfrow=c(", lw, ", 1)\n")
              }
              len <- length(x@data$u)
              for (w in 1:lw) {
                  oceDebug(debug, "which[", w, "]=", which[w], "; drawTimeRange=", drawTimeRange, "\n")
                  if (which[w] == 1) {
                      oce.plot.ts(x@data$time, x@data$u,
                                  type=type,
                                  xlab="", ylab=resizableLabel("u"),
                                  main=main, mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                  tformat=tformat)
                  } else if (which[w] == 2) {
                      oce.plot.ts(x@data$time, x@data$v,
                                  type=type,
                                  xlab="", ylab=resizableLabel("v"),
                                  main=main, mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                  tformat=tformat)
                  } else if (which[w] == 3) {
                      ## or "progressive vector"
                      oceDebug(debug, "progressive vector plot\n")
                      dt <- as.numeric(difftime(x@data$time[2], x@data$time[1], units="sec")) # FIXME: assuming equal dt
                      mPerKm <- 1000
                      u <- x@data$u
                      v <- x@data$v
                      ## message("head(u): ", paste(head(u), collapse=" "))
                      ## message("head(v): ", paste(head(v), collapse=" "))
                      ## message("dt: ", dt)
                      u[is.na(u)] <- 0        # zero out missing
                      v[is.na(v)] <- 0
                      x.dist <- cumsum(u) * dt / mPerKm
                      y.dist <- cumsum(v) * dt / mPerKm
                      ## message("head(x.dist): ", paste(head(x.dist), collapse=" "))
                      ## message("head(y.dist): ", paste(head(y.dist), collapse=" "))
                      plot(x.dist, y.dist,
                           xlab=resizableLabel("distance km"), ylab=resizableLabel("distance km"),
                           type='l', asp=1, ...)
                  } else if (which[w] %in% 4:6) {
                      ## "uv" (if 4), "uv+ellipse" (if 5), or "uv+ellipse+arrow" (if 6)
                      oceDebug(debug, "\"uv\", \"uv+ellipse\", or \"uv+ellipse+arrow\" plot\n")
                      if (len <= small)
                          plot(x@data$u, x@data$v, type=type,
                               xlab=resizableLabel("u"), ylab=resizableLabel("v"),
                               asp=1, ...)
                      else
                          smoothScatter(x@data$u, x@data$v,
                                        xlab=resizableLabel("u"), ylab=resizableLabel("v"),
                                        asp=1, ...)
                      if (which[w] >= 5) {
                          oceDebug(debug, "\"uv+ellipse\", or \"uv+ellipse+arrow\" plot\n")
                          ok <- !is.na(x@data$u) & !is.na(x@data$v)
                          e <- eigen(cov(data.frame(u=x@data$u[ok], v=x@data$v[ok])))
                          major <- sqrt(e$values[1])
                          minor <- sqrt(e$values[2])
                          theta <- seq(0, 2*pi, length.out=360/5)
                          xx <- major * cos(theta)
                          yy <- minor * sin(theta)
                          theta0 <- atan2(e$vectors[2, 1], e$vectors[1, 1])
                          rotate <- matrix(c(cos(theta0), -sin(theta0), sin(theta0), cos(theta0)), nrow=2, byrow=TRUE)
                          xxyy <- rotate %*% rbind(xx, yy)
                          col <- if ("col" %in% names(dots)) col else "darkblue"
                          lines(xxyy[1, ], xxyy[2, ], lwd=5, col="yellow")
                          lines(xxyy[1, ], xxyy[2, ], lwd=2, col=col)
                          if (which[w] >= 6) {
                              oceDebug(debug, "\"uv+ellipse+arrow\" plot\n")
                              umean <- mean(x@data$u, na.rm=TRUE)
                              vmean <- mean(x@data$v, na.rm=TRUE)
                              arrows(0, 0, umean, vmean, lwd=5, length=1/10, col="yellow")
                              arrows(0, 0, umean, vmean, lwd=2, length=1/10, col=col)
                          }
                      }
                  } else if (which[w] == 7) {
                      ## an older version had depth stored
                      p <- if ("pressure" %in% names(x@data)) x@data$pressure else
                          swPressure(x@data$depth, eos="gsw")
                      oce.plot.ts(x@data$time, p,
                                  type=type,
                                  xlab="", ylab=resizableLabel("p", "y"),
                                  main=main, mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                  tformat=tformat)
                  } else if (which[w] == 8) {
                      oce.plot.ts(x@data$time, x[["salinity"]],
                                  type=type,
                                  xlab="", ylab=resizableLabel("S", "y"),
                                  main=main, mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                  tformat=tformat)
                  } else if (which[w] == 9) {
                      oce.plot.ts(x@data$time, x[["temperature"]],
                                  type=type,
                                  xlab="", ylab=resizableLabel("T", "y"),
                                  main=main, mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                  tformat=tformat)
                  } else if (which[w] == 10) {
                      plotTS(as.ctd(x[["salinity"]], x[["temperature"]], x[["pressure"]]), main=main, ...)
                  } else if (which[w] == 11) {
                      cu <- x[["conductivityUnit"]]
                      if (is.list(cu))
                          cu <- as.character(cu$unit)
                      oce.plot.ts(x@data$time, x@data$conductivity,
                                  type=type,
                                  xlab="",
                                  ylab=if (0 == length(cu)) resizableLabel("C", "y") else
                                      if (cu=="mS/cm") resizableLabel("conductivity mS/cm", "y") else
                                          if (cu=="S/m") resizableLabel("conductivity S/m", "y") else
                                              "conductivity (unknown unit",
                                              main=main, mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                      tformat=tformat)
                  } else if (which[w] == 20) {
                      oce.plot.ts(x@data$time, x@data$direction,
                                  type=type,
                                  xlab="", ylab=resizableLabel("direction"),
                                  main=main, mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                  tformat=tformat)
                  } else {
                      stop("unknown value of which (", which[w], ")")
                  }
              }
              oceDebug(debug, "} # plot.cm()\n", unindent=1)
              invisible(NULL)
          })

