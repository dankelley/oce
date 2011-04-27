## vim: tw=120 shiftwidth=4 softtabstop=4 expandtab:

## cm.R current-meter support (interocean S4)
read.cm <- function(file, from=1, to, by=1, tz=getOption("oceTz"),
                    type=c("s4"),
                    latitude=NA, longitude=NA,
                    debug=getOption("oceDebug"), monitor=TRUE, history, ...)
{
    if (debug > 2)
        debug <- 2
    if (debug < 0)
        debug  <- 0
    oceDebug(debug, "\b\bread.cm(file=\"",file,
              "\", from=", format(from),
              ", to=", if (missing(to)) "(missing)" else format(to), ", by=", by, "type=", type, ", ...) {\n", sep="")
    type <- match.arg(type)
    if (type == "s4")
        read.cm.s4(file=file, from=from, to=to, by=by, tz=tz,
                   latitude=latitude, longitude=longitude,
                   debug=debug-1, monitor=monitor, history=history, ...)
    else
        stop("unknown type of current meter")
}

read.cm.s4 <- function(file, from=1, to, by=1, tz=getOption("oceTz"),
                       latitude=NA, longitude=NA,
                       debug=getOption("oceDebug"), monitor=TRUE, history, ...)
{
    if (debug > 1)
        debug <- 1
    oceDebug(debug, "\b\bread.cm.s4(file=\"",file,
              "\", from=", format(from),
              ", to=", if (missing(to)) "(missing)" else format(to), ", by=", by, ", ...) {\n", sep="")
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
    metadata <- list(filename=filename)
    ## Examine the first line of the file, to get serial number, etc.
    items <- scan(file, "character", nlines=1, sep="\t", quiet=TRUE) # slow, but just one line
    oceDebug(debug, "line 1 contains: ", paste(items, collapse=" "), "\n")
    metadata$manufacturer <- "interocean"
    metadata$instrumentType <- "s4"
    for (i in 1:(-1 + length(items))) {
        if (length(grep("Serial", items[i])))
            metadata$serialNumber <- items[i+1]
        else if (length(grep("Version", items[i])))
            metadata$version <- items[i+1]
        else if (length(grep("Type", items[i])))
            metadata$type <- items[i+1]
    }
    ## Skip through the rest of the header, and start paying attention when
    ## row number is 1, 2, and then 3.  These first rows give us the time
    ## sequence.
    foundNames <- FALSE
    for (skip in 2:20) {
        items <- scan(file, "character",nlines=1,sep="\t", quiet=TRUE) # slow, but just 20 lines, max
        oceDebug(debug, "line", skip, "contains: ", paste(items, collapse=" "), "\n")
        if (items[1] == "Sample #") {
            names <- sub('[ ]+$', '', sub('^[ ]+','', items))
            names <- ifelse(0 == nchar(names), paste("column", 1:length(names), sep=""), names)
            foundNames <- TRUE
        } else if (items[1] == "1") {
            start.day <- items[2]
        } else if (items[1] == "2") {
            start.hms <- items[3]
        } else if (items[1] == "3") {
            t0 <- strptime(paste(start.day, start.hms), "%m/%d/%Y %H:%M:%s", tz=tz)
            t1 <- strptime(paste(start.day, items[3]), "%m/%d/%Y %H:%M:%s", tz=tz)
            deltat <- as.numeric(t1) - as.numeric(t0)
            break
        }
    }
    ## NOTE: we *skip* the first 3 lines of data.  This is mainly because those lines
    ## had a different number of TAB-separated elements than the rows below, which
    ## thwarted the use of read.table() to read the data.  Besides, those first
    ## 3 lines are likely to just be in-air measurements, anyway ... there is likely
    ## to be little harm in skipping quite a lot more than just 3 points!
    metadata$measurementStart <- t0 + (2 + skip) * deltat
    metadata$measurementDeltat <- deltat
    d <- read.table(file, skip=skip, sep='\t', stringsAsFactors=FALSE)
    col.north <- 5
    col.east <- 6
    col.conductivity <- 13
    col.temperature <- 13
    col.depth <- 14
    col.heading <- 17
    col.salinity <- 19
    if (foundNames) {
        names <- names[1:dim(d)[2]]
        col.east <- which(names == "Veast")
        if (length(col.east) > 0)
            col.east <- col.east[1]
        col.north <- which(names == "Vnorth")
        if (length(col.north) > 0)
            col.north <- col.north[1]
        col.heading <- which(names == "Hdg")
        if (length(col.heading) > 0)
            col.heading <- col.heading[1]
        col.conductivity <- which(names == "Cond")
        if (length(col.conductivity) > 0)
            col.conductivity <- col.conductivity[1]
        col.salinity <- which(names == "Sal")
        if (length(col.salinity) > 0)
            col.salinity <- col.salinity[1]
        col.temperature <- which(names == "T-Temp")
        if (length(col.temperature) > 0)
            col.temperature <- col.temperature[1]
        col.depth <- which(names == "Depth")
        if (length(col.depth) > 0)
            col.depth <- col.depth[1]
    }
    u <- d[, col.east] / 100
    v <- d[, col.north] / 100
    heading <- d[, col.heading]
    conductivity <- d[, col.conductivity] / 100 / (swConductivity(35, 15, 0)) # cond. ratio
    temperature <- d[, col.temperature]
    depth <- d[, col.depth]
    calculate.salinity.from.conductivity <- TRUE # FIXME: why is "Sal" so wrong in the sample file?
    if (calculate.salinity.from.conductivity)
        salinity <- swSCTp(conductivity, temperature, depth) # FIXME: should really be pressure
    else
        salinity <- d[, col.salinity]
    sample <- as.numeric(d[, 1])
    n <- length(u)
    time <- metadata$measurementStart + seq(0,n-1)*metadata$measurementDeltat
    if (inherits(from, "POSIXt")) {
        if (!inherits(to, "POSIXt"))
            stop("if 'from' is POSIXt, then 'to' must be, also")
        if (!is.numeric(by) || by != 1)
            stop("sorry, 'by' must equal 1, in this (early) version of read.cm.s4()")
        from.to.POSIX <- TRUE
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
        if (!is.numeric(to))
            stop("'to' must be either POSIXt or numeric")
        keep <- seq(from, to)
    }
    keep <- keep[1 <= keep]
    keep <- keep[keep <= n]
    ts <- list(sample=sample[keep], time=time[keep], u=u[keep], v=v[keep], heading=heading[keep], salinity=salinity[keep], temperature=temperature[keep], depth=depth[keep])
    metadata$measurementEnd <- ts$time[length(ts$time)]
    data <- list(ts=ts)
    if (missing(history))
        history <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- historyItem(history)
    rval <- list(metadata=metadata, data=data, history=log.item)
    class(rval) <- c("cm", "s4", "oce")
    oceDebug(debug, "\b\b} # read.cm()\n")
    rval
}


summary.cm <- function(object, ...)
{
    if (!inherits(object, "cm"))
        stop("method is only for cm objects")
    if (inherits(object, "s4")) {
        resSpecific <- list(foo="bar")
    } else {
        stop("can only summarize cm objects of sub-type \"s4\", not class ", paste(class(object),collapse=","))
    }
    res <- resSpecific
    res$latitude <- object$metadata$latitude
    res$longitude <- object$metadata$longitude
    res$filename <- object$metadata$filename
    res$instrumentType <- object$metadata$instrumentType
    res$serialNumber <- object$metadata$serialNumber
    res$measurementStart <- object$metadata$measurementStart
    res$measurementEnd <- object$metadata$measurementEnd
    res$measurementDeltat <- object$metadata$measurementDeltat
    res$history <- object$history
    ts.names <- names(object$data$ts)
    print(ts.names)
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
    res$subsampleStart <- object$data$ts$time[1]
    res$subsampleEndTime <- object$data$ts$time[length(object$data$ts$time)]
    res$subsampleDeltat <- mean(diff(as.numeric(object$data$ts$time)),na.rm=TRUE)
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
    cat("cm Summary\n----------\n\n", ...)
    cat(paste("* Instrument:         ", x$instrumentType, ", serial number ``", paste(x$serialNumber, collapse=""), "``\n", sep=""), ...)
    cat(paste("* Source filename:    ``", x$filename, "``\n", sep=""), ...)
    if ("latitude" %in% names(x)) {
        cat(paste("* Location:           ", if (is.na(x$latitude)) "unknown latitude" else sprintf("%.5f N", x$latitude), ", ",
                  if (is.na(x$longitude)) "unknown longitude" else sprintf("%.5f E", x$longitude), "\n"))
    }
    cat(sprintf("* Measurements:       %s %s to %s %s sampled at %.4g Hz\n",
                format(x$measurementStart), attr(x$measurementStart, "tzone"),
                format(x$measurementEnd), attr(x$measurementEnd, "tzone"),
                1 / x$measurementDeltat), ...)
    cat(sprintf("* Subsample:          %s %s to %s %s sampled at %.4g Hz\n",
                format(x$subsampleStart), attr(x$subsampleStart, "tzone"),
                format(x$subsampleEnd),  attr(x$subsampleEnd, "tzone"),
                1 / x$subsampleDeltat), ...)
    cat(sprintf("* Cells:              %d, centered at %.3f m to %.3f m, spaced by %.3f m\n",
                x$numberOfCells, x$distance[1],  x$distance[length(x$distance)], diff(x$distance[1:2])),  ...)
    cat("* Statistics of subsample\n  ::\n\n", ...)
    cat(showFives(x, indent='     '), ...)
    ##cat("\n* history::\n\n", ...)
    cat("\n")
    print(summary(x$history))
    invisible(x)
}

plot.cm <- function(x,
                    which=c(1, 2, 7, 9),
                    type="l",
                    adorn=NULL,
                    drawTimeRange=getOption("oceDrawTimeRange"),
                    drawZeroLine=FALSE,
                    mgp=getOption("oceMgp"),
                    mar=c(mgp[1]+1.5,mgp[1]+1.5,1.5,1.5),
                    small=2000,
                    main="",
                    debug=getOption("oceDebug"),
                    ...)
{
    oceDebug(debug, "\b\bplot.cm() {\n")
    oceDebug(debug, "  par(mar)=", paste(par('mar'), collapse=" "), "\n")
    oceDebug(debug, "  par(mai)=", paste(par('mai'), collapse=" "), "\n")
    if (!inherits(x, "cm"))
        stop("method is only for cm objects")
    if (!(is.null(x$metadata$have.actual.data) || x$metadata$have.actual.data)) {
        warning("there are no profiles in this dataset")
        return
    }
    opar <- par(no.readonly = TRUE)
    lw <- length(which)
    oceDebug(debug, "length(which) =", lw, "\n")
    if (lw > 1)
        on.exit(par(opar))
    par(mgp=mgp, mar=mar)
    dots <- list(...)
    gave.ylim <- "ylim" %in% names(dots)
    ylim.given <- if (gave.ylim) dots[["ylim"]] else NULL

    oceDebug(debug, "later on in plot.adp:\n")
    oceDebug(debug, "  par(mar)=", paste(par('mar'), collapse=" "), "\n")
    oceDebug(debug, "  par(mai)=", paste(par('mai'), collapse=" "), "\n")

    ## Translate word-style (FIXME: ugly coding)
    which2 <- vector("numeric", length(which))
    for (w in 1:lw) {
        ww <- which[w]
        if (is.numeric(ww)) {
            which2[w] <- ww
        } else {
            if (     ww == "u")
                which2[w] <- 1
            else if (ww == "v")
                which2[w] <- 2
            else if (ww == "progressive vector")
                which2[w] <- 3
            else if (ww == "uv")
                which2[w] <- 4
            else if (ww == "uv+ellipse")
                which2[w] <- 5
            else if (ww == "uv+ellipse+arrow")
                which2[w] <- 6
            else if (ww == "depth")
                which2[w] <- 7
            else if (ww == "salinity")
                which2[w] <- 8
            else if (ww == "temperature")
                which2[w] <- 9
            else if (ww == "heading")
                which2[w] <- 10
            else if (ww == "TS")
                which2[w] <- 11
            else
                stop("unknown 'which':", ww)
        }
    }
    which <- which2
    adorn.length <- length(adorn)
    if (adorn.length == 1) {
        adorn <- rep(adorn, lw)
        adorn.length <- lw
    }

    tt <- x$data$ts$time
    class(tt) <- "POSIXct"              # otherwise image() gives warnings
    if (lw > 1) {
        par(mfrow=c(lw, 1))
        oceDebug(debug, "calling par(mfrow=c(", lw, ", 1)\n")
    }
    len <- length(x$data$ts$u)
    for (w in 1:lw) {
        oceDebug(debug, "which[", w, "]=", which[w], "; drawTimeRange=", drawTimeRange, "\n")
        if (which[w] == 1) {
            oce.plot.ts(x$data$ts$time, x$data$ts$u,
                        type=type, xlab="", ylab="u [m/s]", main=main, mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5), ...)
        } else if (which[w] == 2) {
            oce.plot.ts(x$data$ts$time, x$data$ts$v,
                        type=type, xlab="", ylab="v [m/s]", main=main, mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5), ...)
        } else if (which[w] == 3) {     # or "progressive vector"
            oceDebug(debug, "progressive vector plot\n")
            dt <- as.numeric(difftime(x$data$ts$time[2], x$data$ts$time[1],units="sec")) # FIXME: assuming equal dt
            m.per.km <- 1000
            u <- x$data$ts$u
            v <- x$data$ts$v
            u[is.na(u)] <- 0        # zero out missing
            v[is.na(v)] <- 0
            x.dist <- cumsum(u) * dt / m.per.km
            y.dist <- cumsum(v) * dt / m.per.km
            plot(x.dist, y.dist, xlab="km", ylab="km", type='l', asp=1, ...)
        } else if (which[w] %in% 4:6) {     # "uv" (if 4), "uv+ellipse" (if 5), or "uv+ellipse+arrow" (if 6)
            oceDebug(debug, "\"uv\", \"uv+ellipse\", or \"uv+ellipse+arrow\" plot\n")
            if (len <= small)
                plot(x$data$ts$u, x$data$ts$v, type=type, xlab="u [m/s]", ylab="v [m/s]", asp=1, ...)
            else
                smoothScatter(x$data$ts$u, x$data$ts$v, xlab="u [m/s]", ylab="v [m/s]", asp=1, ...)
            if (which[w] >= 5) {
                oceDebug(debug, "\"uv+ellipse\", or \"uv+ellipse+arrow\" plot\n")
                ok <- !is.na(x$data$ts$u) & !is.na(x$data$ts$v)
                e <- eigen(cov(data.frame(u=x$data$ts$u[ok], v=x$data$ts$v[ok])))
                major <- sqrt(e$values[1])
                minor <- sqrt(e$values[2])
                theta <- seq(0, 2*pi, length.out=360/5)
                xx <- major * cos(theta)
                yy <- minor * sin(theta)
                theta0 <- atan2(e$vectors[2,1], e$vectors[1,1])
                rotate <- matrix(c(cos(theta0), -sin(theta0), sin(theta0), cos(theta0)), nrow=2, byrow=TRUE)
                xxyy <- rotate %*% rbind(xx, yy)
                col <- if ("col" %in% names(dots)) col else "darkblue"
                lines(xxyy[1,], xxyy[2,], lwd=5, col="yellow")
                lines(xxyy[1,], xxyy[2,], lwd=2, col=col)
                if (which[w] >= 6) {
                    oceDebug(debug, "\"uv+ellipse+arrow\" plot\n")
                    umean <- mean(x$data$ts$u, na.rm=TRUE)
                    vmean <- mean(x$data$ts$v, na.rm=TRUE)
                    arrows(0, 0, umean, vmean, lwd=5, length=1/10, col="yellow")
                    arrows(0, 0, umean, vmean, lwd=2, length=1/10, col=col)
                }
            }
        } else if (which[w] == 7) {
            oce.plot.ts(x$data$ts$time, x$data$ts$depth,
                        type=type, xlab="", ylab="Depth [m]", main=main, mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5), ...)
        } else if (which[w] == 8) {
            oce.plot.ts(x$data$ts$time, x$data$ts$salinity,
                        type=type, xlab="", ylab=resizableLabel("S", "y"), main=main, mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5), ...)
        } else if (which[w] == 9) {
            oce.plot.ts(x$data$ts$time, x$data$ts$temperature,
                        type=type, xlab="", ylab=resizableLabel("T", "y"), main=main, mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5), ...)
        } else if (which[w] == 10) {
            oce.plot.ts(x$data$ts$time, x$data$ts$heading,
                        type=type, xlab="", ylab="Heading", main=main, mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5), ...)
        } else if (which[w] == 11) {
            plot.TS(as.ctd(x$data$ts$salinity, x$data$ts$temperature, x$data$ts$depth), main=main, ...)
        } else {
            stop("unknown value of which (", which[w], ")")
        }
        if (w <= adorn.length) {
            t <- try(eval(adorn[w]), silent=TRUE)
            if (class(t) == "try-error")
                warning("cannot evaluate adorn[", w, "]\n")
        }
    }
    oceDebug(debug, "\b\b} # plot.cm()\n")
    invisible()
}
