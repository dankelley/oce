## cm.R current-meter support (interocean S4)

read.cm <- function(file, from=1, to, by=1, tz=getOption("oce.tz"),
                    type=c("s4"),
                    latitude=NA, longitude=NA,
                    debug=getOption("oce.debug"), monitor=TRUE, log.action, ...)
{
    oce.debug(debug, "read.cm(...,from=",from,",to=",if (missing(to)) "(missing)" else to,",by=",by,"type=",type,",...)\n")
    type <- match.arg(type)
    if (monitor) cat(file, "\n", ...)
    if (type == "s4")
        read.cm.s4(file=file, from=from, to=to, by=by, tz=tz,
                   latitude=latitude, longitude=longitude,
                   debug=debug-1, monitor=monitor, log.action=log.action, ...)
    else
        stop("unknown type of current meter")
}

read.cm.s4 <- function(file, from=1, to, by=1, tz=getOption("oce.tz"),
                       latitude=NA, longitude=NA,
                       debug=getOption("oce.debug"), monitor=TRUE, log.action, ...)
{
    oce.debug(debug, "\b\bread.cm(...,from=",from,",to=",if (missing(to)) "(missing)" else to,",by=",by,",...) {\n")
    if (is.character(file)) {
        filename <- full.filename(file)
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
    metadata <- list(filename=filename)
    items <- scan(file, "character", nlines=1, sep="\t", quiet=TRUE)
    oce.debug(debug, "line 1 contains: ", paste(items, collapse=" "), "\n")
    metadata$manufacturer <- "interocean"
    metadata$instrument.type <- "s4"
    for (i in 1:(-1 + length(items))) {
        if (length(grep("Serial", items[i]))) metadata$serial.number <- items[i+1]
        else if (length(grep("Version", items[i]))) metadata$version <- items[i+1]
        else if (length(grep("Type", items[i]))) metadata$type <- items[i+1]
    }
    ## find first line of data
    for (skip in 2:20) {
        items <- scan(file, "character",nlines=1,sep="\t", quiet=TRUE)
        oce.debug(debug, "line", skip, "contains: ", paste(items, collapse=" "), "\n")
        if (items[1] == "1") {
            #start.day <- strptime("6/25/2008","%m/%d/%Y", tz=tz)
            start.day <- items[2]
        } else if (items[1] == "2") {
            start.hms <- items[3]
        } else if (items[1] == "3") {
            metadata$measurement.start <- strptime(paste(start.day, start.hms), "%m/%d/%Y %H:%M:%s", tz=tz)
            t1 <- strptime(paste(start.day, items[3]), "%m/%d/%Y %H:%M:%s", tz=tz)
            metadata$measurement.deltat <- as.numeric(difftime(t1, metadata$measurement.start, units="secs"))
            break
        }
    }

    d <- read.table(file, skip=skip, sep='\t',stringsAsFactors=FALSE) # FIXME: 16 just happens to work, for a file
    u <- d[,6] / 100                    # eastward, in cm/s
    v <- d[,5] / 100                    # northward, in cm/s
    n <- length(u)
    ts <- list(time=metadata$measurement.start+seq(0,n-1)*metadata$measurement.deltat, u=u, v=v)
    metadata$measurement.end <- ts$time[n]
    metadata$frequency <- 1 / metadata$deltat
    data <- list(ts=ts)
    log.item <- processing.log.item(log.action)
    rval <- list(metadata=metadata, data=data, processing.log=log.item)
    class(rval) <- c("cm", "s4", "oce")
    oce.debug(debug, "\b\b} # read.cm()\n")
    rval
}


summary.cm <- function(object, ...)
{
    if (!inherits(object, "cm")) stop("method is only for cm objects")
    if (inherits(object, "s4")) {
        res.specific <- list(foo="bar")
    } else {
        stop("can only summarize cm objects of sub-type \"s4\", not class ", paste(class(object),collapse=","))
    }
    res <- res.specific
    res$latitude <- object$metadata$latitude
    res$longitude <- object$metadata$longitude
    res$filename <- object$metadata$filename
    res$instrument.type <- object$metadata$instrument.type
    res$serial.number <- object$metadata$serial.number
    res$measurement.start <- object$metadata$measurement.start
    res$measurement.end <- object$metadata$measurement.end
    res$measurement.deltat <- object$metadata$measurement.deltat
    res$processing.log <- processing.log.summary(object)
    ts.names <- names(object$data$ts)
    fives <- matrix(nrow=(-1+length(ts.names)), ncol=5)
    ii <- 1
    for (i in 1:length(ts.names)) {
        if (names(object$data$ts)[i] != "time") {
            fives[ii,] <- fivenum(object$data$ts[[ts.names[i]]], na.rm=TRUE)
            ii <- ii + 1
        }
    }
    rownames(fives) <- ts.names[ts.names != "time"]
    colnames(fives) <- c("Min.", "1st Qu.", "Median", "3rd Qu.", "Max.")
    v.dim <- dim(object$data$ma$v)
    res$subsample.start <- object$data$ts$time[1]
    res$subsample.end.time <- object$data$ts$time[length(object$data$ts$time)]
    res$subsample.deltat <- mean(diff(as.numeric(object$data$ts$time)),na.rm=TRUE)
    res$distance <- object$data$ss$distance
    res$fives <- fives
    res$time <- object$data$ts$time
    res$ts.names <- names(object$data$ts)
    res$ma.names <- names(object$data$ma)
    class(res) <- "summary.cm"
    res
}                                       # summary.cm()

print.summary.cm <- function(x, digits=max(6, getOption("digits") - 1), ...)
{
    cat("cm Summary\n-----------\n\n", ...)
    cat(paste("* Instrument:         ", x$instrument.type, ", serial number ``", paste(x$serial.number, collapse=""), "``\n", sep=""), ...)
    cat(paste("* Source filename:   ``", x$filename, "``\n", sep=""), ...)
    if ("latitude" %in% names(x)) {
        cat(paste("* Location:           ", if (is.na(x$latitude)) "unknown latitude" else sprintf("%.5f N", x$latitude), ", ",
                  if (is.na(x$longitude)) "unknown longitude" else sprintf("%.5f E", x$longitude), "\n"))
    }
    cat(sprintf("* Measurements:       %s %s to %s %s sampled at %.4g Hz\n",
                format(x$measurement.start), attr(x$measurement.start, "tzone"),
                format(x$measurement.end), attr(x$measurement.end, "tzone"),
                1 / x$measurement.deltat), ...)
    cat(sprintf("* Subsample:          %s %s to %s %s sampled at %.4g Hz\n",
                format(x$subsample.start), attr(x$subsample.start, "tzone"),
                format(x$subsample.end),  attr(x$subsample.end, "tzone"),
                1 / x$subsample.deltat), ...)
    cat(sprintf("* Cells:              %d, centered at %.3f m to %.3f m, spaced by %.3f m\n",
                x$number.of.cells, x$distance[1],  x$distance[length(x$distance)], diff(x$distance[1:2])),  ...)
    cat("* Frequency:         ", x$frequency, "kHz\n", ...)
    cat("* Statistics of subsample\n  ::\n\n", ...)
    cat(show.fives(x, indent='     '), ...)
    ##cat("\n* Processing log::\n\n", ...)
    cat("\n")
    print(x$processing.log, ...)
    invisible(x)
}

plot.cm <- function(x,
                    which=1:2,
                    type="l",
                    adorn=NULL,
                    draw.time.range=getOption("oce.draw.time.range"),
                    draw.zero.line=FALSE,
                    mgp=getOption("oce.mgp"),
                    mar=c(mgp[1]+1.5,mgp[1]+1.5,1.5,1.5),
                    debug=getOption("oce.debug"),
                    ...)
{
    oce.debug(debug, "\n")
    oce.debug(debug, "Entering plot.cm()\n")
    oce.debug(debug, "  par(mar)=", paste(par('mar'), collapse=" "), "\n")
    oce.debug(debug, "  par(mai)=", paste(par('mai'), collapse=" "), "\n")
    if (!inherits(x, "cm")) stop("method is only for cm objects")
    if (!(is.null(x$metadata$have.actual.data) || x$metadata$have.actual.data)) {
        warning("there are no profiles in this dataset")
        return
    }
    opar <- par(no.readonly = TRUE)
    lw <- length(which)
    oce.debug(debug, "length(which) =", lw, "\n")
    if (lw > 1) on.exit(par(opar))
    par(mgp=mgp, mar=mar)
    dots <- list(...)
    gave.ylim <- "ylim" %in% names(dots)
    ylim.given <- if (gave.ylim) dots[["ylim"]] else NULL

    oce.debug(debug, "later on in plot.adp:\n")
    oce.debug(debug, "  par(mar)=", paste(par('mar'), collapse=" "), "\n")
    oce.debug(debug, "  par(mai)=", paste(par('mai'), collapse=" "), "\n")

    ## Translate word-style (FIXME: ugly coding)
    which2 <- vector("numeric", length(which))
    for (w in 1:lw) {
        ww <- which[w]
        if (is.numeric(ww)) {
            which2[w] <- ww
        } else {
            if (     ww == "u") which2[w] <- 1
            else if (ww == "v") which2[w] <- 2
            else if (ww == "uv") which2[w] <- 3
            else stop("unknown 'which':", ww)
        }
    }
    adorn.length <- length(adorn)
    if (adorn.length == 1) {
        adorn <- rep(adorn, lw)
        adorn.length <- lw
    }

    tt <- x$data$ts$time
    class(tt) <- "POSIXct"              # otherwise image() gives warnings
    if (lw > 1) {
        par(mfrow=c(lw, 1))
        oce.debug(debug, "calling par(mfrow=c(", lw, ", 1)\n")
    }
    for (w in 1:lw) {
        oce.debug(debug, "which[", w, "]=", which[w], "; draw.time.range=", draw.time.range, "\n")
        if (which[w] == 1) {
            oce.plot.ts(x$data$ts$time, x$data$ts$u, type=type, ...)
            if (draw.zero.line)
                abline(h=0)
        } else if (which[w] == 2) {
            oce.plot.ts(x$data$ts$time, x$data$ts$v, type=type, ...)
            if (draw.zero.line)
                abline(h=0)
        } else if (which[w] == 2) {
            plot(x$data$ts$u, x$data$ts$v, type=type, ...)
        } else {
            stop("unknown value of which (", which[w], ")")
        }
        if (w <= adorn.length) {
            t <- try(eval(adorn[w]), silent=TRUE)
            if (class(t) == "try-error") warning("cannot evaluate adorn[", w, "]\n")
        }
    }
}
