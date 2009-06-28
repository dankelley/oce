read.adv <- function(file, from=0, to, by=1,
                     type=c("sontek"),
                     withHeader=TRUE, sampling.start, deltat,
                     debug=0, monitor=TRUE, log.action)
{
    type = match.arg(type)
    if (type == "sontek")
        read.adv.sontek(file=file, from=from, to=to, by=by,
                        withHeader=withHeader, sampling.start=sampling.start, deltat=deltat,
                        debug=debug, monitor=monitor, log.action=log.action)
}

read.adv.nortek <- function(file, from=0, to, by=1,
                            type="vector",
                            withHeader=TRUE, sampling.start, deltat,
                            debug=0, monitor=TRUE, log.action)
{
    if (missing(to)) stop("must supply \"to\" (this limitation may be relaxed in a future version)")
    if (!inherits(from, "POSIXt")) stop("\"from\" must be a POSIXt time (this limitation may be relaxed in a future version)")
    if (!inherits(to, "POSIXt")) stop("\"to\" must be a POSIXt time (this limitation may be relaxed in a future version)")
    if (!missing(sampling.start)) stop("cannot handle argument \"sampling.start\"")
    if (!missing(deltat)) stop("cannot handle argument \"deltat\"")
    if (!missing(by)) stop("cannot handle argument \"by\"")

    if (is.character(file)) {
        filename <- file
        file <- file(file, "rb")
        on.exit(close(file))
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        filename <- "(connection)"
        open(file, "rb")
        on.exit(close(file))
    }
    type <- match.arg(type)
    if (!withHeader) stop("withHeader must be TRUE")
    header <- read.header.nortek(file)
    metadata <- list(instrument.type="vector",
                     filename=filename,
                     sampling.start=if (missing(sampling.start)) NA else sampling.start,
                     sampling.end=NA,   # FIXME
                     ##size=header$head$size, # FIXME: does this get used?
                     number.of.beams=header$head$number.of.beams, # FIXME: check that this is correct
                     serial.number=header$hardware$serial.number,
                     frequency=header$head$frequency,
                     internal.code.version=header$hardware$pic.version,
                     hardware.revision=header$hardware$hw.revision,
                     rec.size=header$hardware$rec.size,
                     velocity.range=header$hardware$velocity.range,
                     firmware.version=header$hardware$fw.version,
                     config=header$hardware$config,
                     config.pressure.sensor=header$head$config.pressure.sensor,
                     config.magnetometer.sensor=header$head$config.magnetometer.sensor,
                     config.tilt.sensor=header$head$config.tilt.sensor,
                     beam.angle=25,     # FIXME: should read from file
                     orientation=header$head$orientation,
                     frequency=header$head$frequency,
                     head.serial.number=header$head$head.serial.number,
                     bin1.distance=header$user$blanking.distance, # FIXME: is this right?
                     blanking.distance=header$user$blanking.distance,
                     measurement.interval=header$user$measurement.interval,
                     beam.to.xyz=header$head$beam.to.xyz,
                     deployment.name=header$user$deployment.name,
                     cell.size=header$user$cell.size,
                     velocity.scale=header$user$velocity.scale,
                     coordinate.system=header$user$coordinate.system,
                     oce.coordinate=header$user$coordinate.system,
                     oce.beam.attenuated=FALSE
                     )
    if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processing.log.item(log.action)

    # find file length
    seek(file, 0, "end")
    file.length <- seek(file, 0, "start")
    if (debug) cat("file.length=", file.length, "\n")

    ## Find the focus time by bisection, based on "sd" (system data, containing a time).
    bisect.nortek.vector.sd <- function(file, file.length, value)
    {
        lower <- 0
        upper <- file.length
        passes <- 3 + log(file.length, 2) # won't need this many; only do this to catch coding errors
        for (pass in 1:passes) {
            middle <- (upper + lower) / 2
            bis.chunk <- 1000           # only need about 1/4 of this
            seek(file, middle)
            buf <- readBin(file, "raw", n=bis.chunk, endian="little")
            sd.start <- match.bytes(buf, 0xa5, 0x11, 0x0e)[1] # 3-byte code for system-data chunks
            sd.t <- ISOdatetime(2000 + bcd2integer(buf[sd.start+8]),  # year
                                bcd2integer(buf[sd.start+9]), # month
                                bcd2integer(buf[sd.start+6]), # day
                                bcd2integer(buf[sd.start+7]), # hour
                                bcd2integer(buf[sd.start+4]), # min
                                bcd2integer(buf[sd.start+5]), # sec
                                tz=getOption("oce.tz"))
            if (value < sd.t)
                upper <- middle
            else
                lower <- middle
            if (upper - lower < bis.chunk)
                return(middle)
        }
        return(middle)
    }
    ## Window data buffer, using bisection in case of a variable number of vd between sd pairs.
    from.index <- bisect.nortek.vector.sd(file, file.length, from) # just an estimate; see below
    to.index <- bisect.nortek.vector.sd(file, file.length, to)
    if (debug) cat("from.index=", from.index, "to.index=",to.index,"\n")
    seek(file, from.index - 1000)       # add 1kb on each side (trimmed a few lines below)
    buf <- readBin(file, "raw", n=2000+(to.index - from.index))

    ## sd (system data) are interspersed in the vd sequence
    sd.start <- match.bytes(buf, 0xa5, 0x11, 0x0e)
    sd.t <- ISOdatetime(2000 + bcd2integer(buf[sd.start+8]),  # year
                        bcd2integer(buf[sd.start+9]), # month
                        bcd2integer(buf[sd.start+6]), # day
                        bcd2integer(buf[sd.start+7]), # hour
                        bcd2integer(buf[sd.start+4]), # min
                        bcd2integer(buf[sd.start+5]), # sec
                        tz=getOption("oce.tz"))
    ok <- from <= sd.t & sd.t <= to     # trim to limits
    sd.start <- sd.start[ok]
    sd.t <- sd.t[ok]
    if (0 != diff(range(diff(sd.start)))) warning("ignoring the fact that vector data unequal burst lengths")

    sd.len <- length(sd.start)
    sd.start2 <- sort(c(sd.start, 1 + sd.start))
    heading <- 0.1 * readBin(buf[sd.start2 + 14], "integer", size=2, n=sd.len, signed=TRUE, endian="little")
    pitch <- 0.1 * readBin(buf[sd.start2 + 16], "integer", size=2, n=sd.len, signed=TRUE, endian="little")
    roll <- 0.1 * readBin(buf[sd.start2 + 18], "integer", size=2, n=sd.len, signed=TRUE, endian="little")
    temperature <- 0.01 * readBin(buf[sd.start2 + 20], "integer", size=2, n=sd.len, signed=TRUE, endian="little")

    heading.resampled <- approx(seq(0,1,length.out=sd.len), heading)
    ##
    ## vd (velocity data) -- several of these are found between each pair of sd
    vd.start <- match.bytes(buf, 0xa5, 0x10)
    metadata$burst.length <- round(length(vd.start) / length(sd.start), 0)

    vd.start2 <- sort(c(vd.start, 1 + vd.start))
    vd.len <- length(vd.start)

    p.MSB <- as.numeric(buf[vd.start + 4])
    p.LSW <- readBin(buf[vd.start2 + 6], "integer", size=2, n=vd.len, signed=FALSE, endian="little")
    pressure <- (65536 * p.MSB + p.LSW) / 1000

    x <- readBin(buf[vd.start2 + 10], "integer", size=2, n=vd.len, signed=TRUE, endian="little") / 1000
    y <- readBin(buf[vd.start2 + 12], "integer", size=2, n=vd.len, signed=TRUE, endian="little") / 1000
    z <- readBin(buf[vd.start2 + 14], "integer", size=2, n=vd.len, signed=TRUE, endian="little") / 1000
    a1 <- buf[vd.start + 16]
    a2 <- buf[vd.start + 17]
    a3 <- buf[vd.start + 18]
    c1 <- buf[vd.start + 19]
    c2 <- buf[vd.start + 20]
    c3 <- buf[vd.start + 21]
    coarse <- seq(0,1,length.out=sd.len)
    fine <- seq(0,1,length.out=vd.len)

    print(length(pressure))
    print(length(coarse))
    print(length(fine))

    data <- data.frame(time=seq(from=from, to=to, length.out=vd.len),
                       heading=approx(coarse, heading, xout=fine)$y,
                       pitch=approx(coarse, pitch, xout=fine)$y,
                       roll=approx(coarse, roll, xout=fine)$y,
                       temperature=approx(coarse, temperature, xout=fine)$y,
                       pressure=pressure,
                       x=x,
                       y=y,
                       z=z,
                       a1=a1,
                       a2=a2,
                       a3=a3,
                       c1=c1,
                       c2=c2,
                       c3=c3)
    res <- list(data=data, metadata=metadata, processing.log=log.item)
    class(res) <- c("adv", "nortek", "vector", "oce")
    res
}

read.adv.sontek <- function(file, from=0, to, by=1,
                            type="default",
                            withHeader=TRUE, sampling.start, deltat,
                            debug=0, monitor=TRUE, log.action)
{
    if (is.character(file)) {
        filename <- file
        file <- file(file, "rb")
        on.exit(close(file))
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")

    if (!isOpen(file)) {
        filename <- "(connection)"
        open(file, "rb")
        on.exit(close(file))
    }
    type <- match.arg(type)
    if (withHeader) {
        stop("cannot read with header yet")
    } else {
        if (missing(sampling.start)) stop("must give 'sampling.start' if withHeader is FALSE")
        if (missing(deltat)) stop("must give 'deltat' if withHeader is FALSE")
        seek(file, 0, "end")
        file.size <- seek(file, 0, "start")
        if (debug) cat("file", filename, "has", file.size, "bytes\n")
        buf <- readBin(file, "raw", n=file.size, endian="little")
        flag1 <- as.raw(0x85)           # id
        flag2 <- as.raw(0x16)           # number of bytes (22 in decimal)
        match.flag1 <- which(buf==flag1)
        sample.start <- match.flag1[buf[match.flag1 + 1] == flag2]
        sample.start <- sample.start[1:(-1 + length(sample.start))] # last may be partial
        if (buf[sample.start[1] + as.integer(flag2)] != flag1) stop("problem reading first sample")
        ## FIXME: should run along the data for a while, to confirm that it's ok
    }
    n <- length(sample.start)
    ## id <- buf[sample.start]
    ## number.of.bytes <- buf[sample.start + 1]
    sample.start2 <- sort(c(sample.start, sample.start+1)) # use this to subset for 2-byte reads
    ##print((sample.start2 + 2)[1:10])
    sample.number <- readBin(buf[sample.start2 + 2], "integer", signed=FALSE, endian="little", size=2, n=n)
##?##    temperature <- readBin(buf[sample.start2 + 4], "integer", signed=TRUE, endian="little", size=2, n=n) / 100.0
    ## in next, divide by 100 to get to cm/s, then by 100 to get to m/s
    x <- readBin(buf[sample.start2 +  6], "integer", signed=TRUE, endian="little", size=2, n=n) / 10000.0
    y <- readBin(buf[sample.start2 +  8], "integer", signed=TRUE, endian="little", size=2, n=n) / 10000.0
    z <- readBin(buf[sample.start2 + 10], "integer", signed=TRUE, endian="little", size=2, n=n) / 10000.0
    a1 <- as.numeric(buf[sample.start + 12])
    a2 <- as.numeric(buf[sample.start + 13])
    a3 <- as.numeric(buf[sample.start + 14])
    c1 <- as.numeric(buf[sample.start + 15])
    c2 <- as.numeric(buf[sample.start + 16])
    c3 <- as.numeric(buf[sample.start + 17])

    ##print(buf[sample.start2 + 18][1:10])

    temperature <- readBin(buf[sample.start2 + 18], "integer", signed=TRUE, endian="little", size=2, n=n) / 100.0
    pressure <- readBin(buf[sample.start2 + 20], "integer", signed=FALSE, endian="little", size=2, n=n) / 1000 # mbar?
    ## 21 and 22 are checksum

    time <- seq(from=sampling.start, by=deltat, length.out=length(x))
    attr(time, "tzone") <- attr(sampling.start, "tzone")

    data <- data.frame(time=time,
                       sample.number=sample.number,
                       x=x, y=y, z=z,
                       a1=a1, a2=a2, a3=a3,
                       c1=c1, c2=c2, c3=c3,
                       temperature=temperature,
                       pressure=pressure)
    metadata <- list(filename=filename,
                     instrument.type="sontek",
                     number.of.samples=length(x),
                     sampling.start=sampling.start,
                     deltat=deltat)
    if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processing.log.item(log.action)
    res <- list(data=data, metadata=metadata, processing.log=log.item)
    class(res) <- c("adv", "sontek", "oce")
    res
}

summary.adv <- function(object, ...)
{
    if (!inherits(object, "adv")) stop("method is only for adv objects")
    if (inherits(object, "sontek")) {
        res.specific <- NULL;
    } else if (inherits(object, "nortek")) {
        res.specific <- list(burst.length=object$burst.length);
    } else stop("can only summarize ADV objects of sub-type \"nortek\" or \"sontek\", not class ", paste(class(object),collapse=","))
    names <- names(object$data)
    fives <- matrix(nrow=(-1+length(names)), ncol=5)
    ii <- 1
    for (i in 1:length(names)) {
        if (names(object$data)[i] != "time") {
            fives[ii,] <- fivenum(as.numeric(object$data[[names[i]]]), na.rm=TRUE)
            ii <- ii + 1
        }
    }
    rownames(fives) <- names[names != "time"]
    colnames(fives) <- c("Min.", "1st Qu.", "Median", "3rd Qu.", "Max.")
    res <- list(filename=object$metadata$filename,
                sampling.start=object$metadata$sampling.start,
                sampling.end=object$metadata$sampling.start + object$metadata$number.of.samples*object$metadata$deltat,
                deltat=object$metadata$deltat,
                instrument.type=object$metadata$instrument.type,
                number.of.samples=length(object$data$x),
                fives=fives,
                processing.log=processing.log.summary(object))
    if (inherits(object, "nortek"))
        res$burst.length <- object$metadata$burst.length
    class(res) <- "summary.adv"
    res
}                                       # summary.adv()

print.summary.adv <- function(x, digits=max(6, getOption("digits") - 1), ...)
{
    cat("ADV summary\n")
    cat("  Instrument type:            ", x$instrument.type, "\n")
    cat("  Filename:                   ", x$filename, "\n")
##    cat("  Instrument serial number:   ", x$metadata$serial.number, "\n")
##    cat("  Coordinate system:          ", x$coordinate.system, "[originally],", x$oce.coordinate, "[presently]\n")
    cat("  Measurements at times:      ", format(x$sampling.start), attr(x$sampling.end, "tzone"),
        "to",
        format(x$sampling.end), attr(x$sampling.end, "tzone"),
        "at interval", x$deltat, "s\n")
##    cat("  Orientation:                ", x$orientation, "\n")
##    cat("  Beam angle:                 ", x$metadata$beam.angle, "\n")
    cat("  Number of samples:          ", x$number.of.samples, "\n")

    if (x$instrument.type == "sontek") {
        cat("  SonTek-specific\n")
        cat("    -na-\n")
    }
    if (x$instrument.type == "nortek") {
        cat("  NorTek-specific\n")
        cat("    Burst Length: ", x$burst.length, "\n")
    }
    cat("\nStatistics:\n")
    print(x$fives)
    cat("\n")
    print(x$processing.log)
    invisible(x)
}

plot.adv <- function(x,
                     which=1:3,
                     smooth=TRUE,
                     titles,
                     adorn=NULL,
                     draw.time.range=getOption("oce.draw.time.range"),
                     mgp=getOption("oce.mgp"),
                     mar=c(mgp[1],mgp[1]+1,1,1+par("cex")),
                     margins.as.image=FALSE,
                     ...)
{
    if (!inherits(x, "adv")) stop("method is only for adv objects")
    if (!all(which %in% c(1:3,14:15))) stop("\"which\" must be in the range c(1:3,14:15)")
    opar <- par(no.readonly = TRUE)
    lw <- length(which)
    if (margins.as.image) {
        ## scale <- (0.132 + (0.2 - 0.132) * exp(-(lw - 1))) / 0.2
        scale <- 0.7
        w <- (1.5 + par("mgp")[2]) * par("csi") * scale * 2.54 + 0.5
        lay <- layout(matrix(1:(2*lw), nrow=lw, byrow=TRUE), widths=rep(c(1, lcm(w)), lw))
    } else {
        if (lw > 1)
            lay <- layout(cbind(1:lw))
    }

    if (!missing(titles) && length(titles) != lw) stop("length of 'titles' must equal length of 'which'")
    if (lw > 1) on.exit(par(opar))
    par(mgp=mgp, mar=mar)
    dots <- list(...)
    gave.ylim <- "ylim" %in% names(dots)

    ylim.given <- if (gave.ylim) dots[["ylim"]] else NULL

    adorn.length <- length(adorn)
    if (adorn.length == 1) {
        adorn <- rep(adorn, lw)
        adorn.length <- lw
    }
    for (w in 1:lw) {
        ##cat("which[w]=", which[w], "smooth=",smooth,"\n")
        if (which[w] == 1) {
            oce.plot.ts(x$data$time,
                        if (smooth) smooth(x$data$x) else x$data$x,
                        ylab="u [m/s]", type='l', draw.time.range=draw.time.range, ...)
        } else if (which[w] == 2) {
            oce.plot.ts(x$data$time,
                        if (smooth) smooth(x$data$y) else x$data$y,
                        ylab="v [m/s]", type='l', draw.time.range=draw.time.range, ...)
        } else if (which[w] == 3) {
            oce.plot.ts(x$data$time,
                        if (smooth) smooth(x$data$z) else x$data$z,
                        ylab="w [m/s]", type='l', draw.time.range=draw.time.range, ...)
        } else if (which[w] == 14) {    # temperature time-series
            oce.plot.ts(x$data$time,
                        if (smooth) smooth(x$data$temperature) else x$data$temperature,
                        ylab=resizable.label("T"), type='l', draw.time.range=draw.time.range, ...)
        } else if (which[w] == 15) {    # pressure time-series
            oce.plot.ts(x$data$time,
                        if (smooth) smooth(x$data$pressure) else x$data$pressure,
                        ylab=resizable.label("p"), type='l', draw.time.range=draw.time.range, ...)
        } else {
            stop("unknown value of \"which\":", which)
        }
        if (margins.as.image)  {
            ## blank plot, to get axis length same as for images
            omar <- par("mar")
            par(mar=c(mar[1], 1/4, mgp[2]+1/2, mgp[2]+1))
            plot(1:2, 1:2, type='n', axes=FALSE, xlab="", ylab="")
            par(mar=omar)
        }
        draw.time.range <- FALSE
        if (w <= adorn.length) {
            t <- try(eval(adorn[w]), silent=TRUE)
            if (class(t) == "try-error") warning("cannot evaluate adorn[", w, "]\n")
        }
    }
}
