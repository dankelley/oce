read.adv <- function(file, from=0, to, by=1,
                     type=c("nortek", "sontek", "sontek.text"),
                     withHeader=TRUE, sampling.start, deltat,
                     tz=getOption("oce.tz"), debug=getOption("oce.debug"), monitor=TRUE, log.action)
{
    type = match.arg(type)
    if (type == "nortek")
        read.adv.nortek(file=file, from=from, to=to, by=by,
                        withHeader=withHeader, sampling.start=sampling.start, deltat=deltat,
                        tz=tz, debug=debug, monitor=monitor, log.action=log.action)
    else if (type == "sontek")
        read.adv.sontek(file=file, from=from, to=to, by=by,
                        withHeader=withHeader, sampling.start=sampling.start, deltat=deltat,
                        tz=tz, debug=debug, monitor=monitor, log.action=log.action)
    else if (type == "sontek.text")
        read.adv.sontek.text(basefile=file, from=from, to=to, by=by,
                             tz=tz, debug=debug, log.action=log.action)
}

read.adv.nortek <- function(file, from=0, to, by=1,
                            type="vector",
                            withHeader=TRUE, sampling.start, deltat,
                            tz=getOption("oce.tz"), debug=getOption("oce.debug"), monitor=TRUE, log.action)
{
    by.is.broken <- TRUE
    if (missing(to)) stop("must supply \"to\" (this limitation may be relaxed in a future version)")
    if (!inherits(from, "POSIXt")) stop("\"from\" must be a POSIXt time (this limitation may be relaxed in a future version)")
    if (!inherits(to, "POSIXt")) stop("\"to\" must be a POSIXt time (this limitation may be relaxed in a future version)")
    if (!missing(sampling.start)) stop("cannot handle argument \"sampling.start\"")
    if (!missing(deltat)) stop("cannot handle argument \"deltat\"")

    if (!missing(by) && debug == 0) stop("cannot use 'by' unless 'debug' is non-zero (even then, it fails)")
    if (by < 1) stop("cannot handle argument \"by\"<1")

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
    if (debug) cat("read.adv.nortek()...\n")
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
                     transformation.matrix=header$transformation.matrix,
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
    file.size <- seek(file, 0, "start")
    if (debug) cat("file.size=", file.size, "\n")

    ## Find the focus time by bisection, based on "sd" (system data, containing a time).
    bisect.nortek.vector.sd <- function(file, file.size, value)
    {
        lower <- 0
        upper <- file.size
        passes <- floor(3 + log(file.size, 2)) # won't need this many; only do this to catch coding errors
        for (pass in 1:passes) {
            middle <- floor((upper + lower) / 2)
            bis.chunk <- 1000           # only need about 1/4 of this
            seek(file, middle)
            buf <- readBin(file, "raw", n=bis.chunk, endian="little")
            sd.start <- match.bytes(buf, 0xa5, 0x11, 0x0e)[1] # 3-byte code for system-data chunks
            #str(sd.start)                                     # DEBUG
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
                return(list(index=middle, time=sd.t[1]))
            if (debug) cat("bisection (for \"from\" or \"to\" time) examining indices", lower, "to", upper, " (pass", pass, "of max", passes, ")\n")
        }
        return(list(index=middle, time=sd.t[1]))
    }
    ## Window data buffer, using bisection in case of a variable number of vd between sd pairs.
    from.index <- bisect.nortek.vector.sd(file, file.size, from)
    to.index <- bisect.nortek.vector.sd(file, file.size, to)
    if (debug) cat("Bisection for \"from\" and \"to\" times yielded indices", from.index$index, "and",to.index$index,"\n")

    if (to.index$index <= from.index$index) stop("no data in specified time range ", format(from), " to ", format(to), "; the closest times in the file are ", format(from.index$time), " and ", format(to.index$time))

    start <- max(1, from.index$index - 1000)
    n <- min(file.size - start - 1, 2000 + (to.index$index - from.index$index)) # FIXME: need the -1?
    seek(file, start)
    buf <- readBin(file, "raw", n=n)
    ## sd (system data) are interspersed in the vd sequence

    if (debug) cat("about to try to match bytes... note that length(buf) is", length(buf), "\n")

    sd.start <- match.bytes(buf, 0xa5, 0x11, 0x0e)

    if (debug) str(sd.start)

    sd.t <- ISOdatetime(2000 + bcd2integer(buf[sd.start+8]),  # year
                        bcd2integer(buf[sd.start+9]), # month
                        bcd2integer(buf[sd.start+6]), # day
                        bcd2integer(buf[sd.start+7]), # hour
                        bcd2integer(buf[sd.start+4]), # min
                        bcd2integer(buf[sd.start+5]), # sec
                        tz=getOption("oce.tz"))

    sd.start <- subset(sd.start, from <= sd.t & sd.t <= to) # trim before/after (typically a few remnants)
    sd.tt <- subset(sd.t, from <= sd.t & sd.t <= to) # trim before/after (typically a few remnants)

    if (debug) {
        cat("before doing 'by' operation, sd.start is:\n")
        str(sd.start)
    }

    if (by > 1) {
        sd.start <- sd.start[seq(1, length(sd.start), by=by)]
        sd.t <- sd.t[seq(1, length(sd.t), by=by)]
    }

    if (debug) {
        cat("after doing 'by' operation, sd.start is:\n")
        str(sd.start)
        cat("after doing 'by' operation, sd.t     is:\n")
        str(sd.t)
    }

    if (0 != diff(range(diff(sd.start)))) warning("ignoring the fact that the vector data have unequal burst lengths")

    sd.len <- length(sd.start)

    sd.start2 <- sort(c(sd.start, 1 + sd.start))
    heading <- 0.1 * readBin(buf[sd.start2 + 14], "integer", size=2, n=sd.len, signed=TRUE, endian="little")
    pitch <- 0.1 * readBin(buf[sd.start2 + 16], "integer", size=2, n=sd.len, signed=TRUE, endian="little")
    roll <- 0.1 * readBin(buf[sd.start2 + 18], "integer", size=2, n=sd.len, signed=TRUE, endian="little")
    temperature <- 0.01 * readBin(buf[sd.start2 + 20], "integer", size=2, n=sd.len, signed=TRUE, endian="little")

    ##
    ## vd (velocity data) -- several of these are found between each pair of sd
    vd.start <- match.bytes(buf, 0xa5, 0x10)
    metadata$burst.length <- round(length(vd.start) / length(sd.start), 0)

    vd.start2 <- sort(c(vd.start, 1 + vd.start))
    vd.len <- length(vd.start)          # FIXME: should be subsampled with 'by' ... but how???

    p.MSB <- as.numeric(buf[vd.start + 4])
    p.LSW <- readBin(buf[vd.start2 + 6], "integer", size=2, n=vd.len, signed=FALSE, endian="little")
    pressure <- (65536 * p.MSB + p.LSW) / 1000

    v <- array(dim=c(vd.len, 3))
    v[,1] <- readBin(buf[vd.start2 + 10], "integer", size=2, n=vd.len, signed=TRUE, endian="little") / 1000
    v[,2] <- readBin(buf[vd.start2 + 12], "integer", size=2, n=vd.len, signed=TRUE, endian="little") / 1000
    v[,3] <- readBin(buf[vd.start2 + 14], "integer", size=2, n=vd.len, signed=TRUE, endian="little") / 1000

    a <- array(raw(), dim=c(vd.len, 3))
    a[,1] <- buf[vd.start + 16]
    a[,2] <- buf[vd.start + 17]
    a[,3] <- buf[vd.start + 18]

    c <- array(raw(), dim=c(vd.len, 3))
    c[,1] <- buf[vd.start + 19]
    c[,2] <- buf[vd.start + 20]
    c[,3] <- buf[vd.start + 21]
    coarse <- seq(0,1,length.out=sd.len)
    fine <- seq(0,1,length.out=vd.len)
    rm(buf)
    gc()
    data <- list(ts=list(time=seq(from=from, to=to, length.out=vd.len),
                 heading=approx(coarse, heading, xout=fine)$y,
                 pitch=approx(coarse, pitch, xout=fine)$y,
                 roll=approx(coarse, roll, xout=fine)$y,
                 temperature=approx(coarse, temperature, xout=fine)$y,
                 pressure=pressure),
                 ss=list(distance=0),
                 ma=list(v=v, a=a, c=c))
    res <- list(data=data, metadata=metadata, processing.log=log.item)
    class(res) <- c("nortek", "adv", "oce")
    if (debug) {
        cat("sd.len=",sd.len," at return\n")
        cat("vd.len=",vd.len," at return\n")
        cat("check:",length(res$data$ts$time)," at return\n")
    }
    gc()
    res
}

read.adv.sontek <- function(file, from=0, to, by=1,
                            type="default",
                            withHeader=TRUE, sampling.start, deltat,
                            tz=getOption("oce.tz"), debug=getOption("oce.debug"), monitor=TRUE, log.action)
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
    if (!missing(by)) stop("cannot handle argument 'by' in this version of Oce")
    if (debug) cat("read.adv.sontek()...\n")
    if (withHeader) {
        stop("cannot read with header yet")
    } else {
        if (missing(sampling.start)) stop("must give 'sampling.start' if withHeader is FALSE")
        if (missing(deltat)) stop("must give 'deltat' if withHeader is FALSE")
        seek(file, 0, "end")
        file.size <- seek(file, 0, "start")
        if (debug) cat("file", filename, "has", file.size, "bytes\n")
        buf <- readBin(file, "raw", n=file.size, endian="little")

        ## See page 95 of SonTek/YSI ADVField/Hydra Acoustic Doppler Velocimeter (Field)
        ## Technical Documentation (Sept 1, 2001)
        flag1 <- as.raw(0x85)           # id
        flag2 <- as.raw(0x16)           # number of bytes (22 in decimal)
        sample.start <- match.bytes(buf, 0x85, 0x16)
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

    ## in next, divide by 100 to get to cm/s, then by 100 to get to m/s

    v <- array(dim=c(n, 3))
    v[,1] <- readBin(buf[sample.start2 + 4], "integer", signed=TRUE, endian="little", size=2, n=n) * 1e-4
    v[,2] <- readBin(buf[sample.start2 + 6], "integer", signed=TRUE, endian="little", size=2, n=n) * 1e-4
    v[,3] <- readBin(buf[sample.start2 + 8], "integer", signed=TRUE, endian="little", size=2, n=n) * 1e-4
    a <- array(raw(), dim=c(n, 3))
    a[,1] <- buf[sample.start + 10]
    a[,2] <- buf[sample.start + 11]
    a[,3] <- buf[sample.start + 12]
    c <- array(raw(), dim=c(n, 3))
    c[,1] <- buf[sample.start + 13]
    c[,2] <- buf[sample.start + 14]
    c[,3] <- buf[sample.start + 15]

    temperature <- readBin(buf[sample.start2 + 16], "integer", signed=TRUE, endian="little", size=2, n=n) * 1e-2
    pressure <- readBin(buf[sample.start2 + 18], "integer", signed=FALSE, endian="little", size=2, n=n) * 1e-4 # FIXME unit?
    ## offsets 20 and 21 are the checksum (filling out for 22 bytes in total)

    time <- seq(from=sampling.start, by=deltat, length.out=n)
    attr(time, "tzone") <- attr(sampling.start, "tzone")

    data <- list(ts=list(time=time,
                 sample.number=sample.number,
                 temperature=temperature,
                 pressure=pressure),
                 ss=list(distance=0),
                 ma=list(v=v, a=a, c=c))
    metadata <- list(filename=filename,
                     instrument.type="sontek",
                     number.of.samples=length(time),
                     sampling.start=sampling.start,
                     deltat=deltat,
                     orientation="downward", # FIXME: a total guess
                     oce.coordinate="beam" # FIXME: we don't actually know this, if there is no header
                     )
    if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processing.log.item(log.action)
    res <- list(data=data, metadata=metadata, processing.log=log.item)
    class(res) <- c("sontek", "adv", "oce")
    res
}


read.adv.sontek.text <- function(basefile, from=0, to, by=1,
                                 coordinate.system="xyz",
                                 transformation.matrix,
                                 tz=getOption("oce.tz"),
                                 debug=getOption("oce.debug"), log.action)
{
    ## FIXME: It would be better to deal with the binary file, but the format is unclear to me;
    ## FIXME: two files are available to me, and they differ considerably, neither matching the
    ## FIXME: SonTek documentation.
    if (by != 1) stop("must have \"by\"=1, in this version of the package")
    suffices <- c("hd1", "ts1")
    items.per.sample <- 16
    if (missing(basefile)) stop("need to supply a basefile, e.g. \"A\" to read \"A.hd1\" and \"A.ts1\"")

    hd <- paste(basefile, suffices[1], sep=".")
    ts <- paste(basefile, suffices[2], sep=".")

    ## The hd1 file holds per-burst information
    hdt <-  read.table(hd)
    number.of.bursts <- dim(hdt)[1]
    if (debug) cat("number of bursts: ", number.of.bursts, "\n")
    t <- ISOdatetime(year=hdt[,2], month=hdt[,3], day=hdt[,4], hour=hdt[,5], min=hdt[,6], sec=hdt[,7], tz=tz)
    if (!missing(from) && inherits(from, "POSIXt")) {
        ignore <- t < from
        if (sum(ignore) == 0) stop("no data in this time interval, starting at time ", from, "\n")
        from.burst <- which(ignore == FALSE)[1]
        if (debug) cat("\"from\" is burst number", from.burst, "at", format(t[from.burst]), "\n")
    }
    if (!missing(to) && inherits(from, "POSIXt")) {
        ignore <- t < to
        if (sum(ignore) == 0) stop("no data in this time interval, starting at time ", to, "\n")
        to.burst <- which(ignore == FALSE)[1] + 1 # add 1 since we'll chop later
        to.burst <- min(to.burst, length(t))
        if (debug) cat("\"to\" is burst number", to.burst, "at", format(t[to.burst]), "\n")
    }
    ##voltage <- hdt[,14]
    heading <- hdt[,24]
    roll <- hdt[,25]
    pitch <- hdt[,26]
    spb <- hdt[1,9]                      # FIXME may this change over time?
    sr <- spb / 3600

    ts.file <- file(ts, "rb")
    on.exit(close(ts.file))
    if (!inherits(ts.file, "connection"))
        stop("argument `ts.file' must be a character string or connection")

    ## Examine ".ts1" file to see if we can deal with it.
    seek(ts.file, where=0, origin="end")
    bytes.in.file <- seek(ts.file, where=0, origin="start")
    if (debug) cat("length of \".", suffices[2], "\" file: ",bytes.in.file," bytes\n", sep="")
    look <- min(5000, bytes.in.file)
    b <- readBin(ts.file, "raw", n=look)
    newlines <- which(b == 0x0a)
    if (0 != diff(range(fivenum(diff(newlines))))) stop("need equal line lengths in ", ts)
    ## Line length
    bytes.in.sample <- diff(newlines)[1]
    if (debug) cat("line length in \".", suffices[2], "\" file: ", bytes.in.sample, " bytes\n", sep="")
    ## elements per line
    seek(ts.file, where=newlines[1], origin="start")
    d <- scan(ts.file, what="character", nlines=1, quiet=TRUE)
    if (debug) cat("first line in \".", suffices[2], "\" file: ", paste(d, collapse=" "), "\n", sep="")
    items.per.line <- length(d)
    if (items.per.sample != length(d)) stop("file \".", suffices[2], "\" should have ", items.per.sample, " elemetns per line, but it has ", length(d))
    if (debug) cat("elements per line in \".", suffices[2], "\" file: ", length(d), "\n", sep="")
    lines.in.file <- bytes.in.file / bytes.in.sample
    if (debug) cat("lines in \".", suffices[2], "\" file: ", lines.in.file, "\n", sep="")

    samples.per.burst <- lines.in.file / number.of.bursts
    if (debug) cat("samples per burst: ", samples.per.burst, "\n")

    from.byte <- from.burst * samples.per.burst * bytes.in.sample
    to.byte <- to.burst * samples.per.burst * bytes.in.sample
    if (debug) {
        cat("seek from:", from.byte, "\n")
        cat("seek to:", to.byte, "\n")
    }
    seek(ts.file, where=from.byte, origin="start")
    ts <- matrix(scan(ts.file, n=items.per.sample*(to.burst - from.burst + 1)*samples.per.burst, quiet=TRUE),
                      ncol=items.per.sample, byrow=TRUE)
    len <- dim(ts)[1]
    v <- array(numeric(), dim=c(len, 3))
    v[,1] <- ts[,3] / 100
    v[,2] <- ts[,4] / 100
    v[,3] <- ts[,5] / 100
    a <- array(raw(), dim=c(len, 3))
    a[,1] <- as.raw(ts[,6])
    a[,2] <- as.raw(ts[,7])
    a[,3] <- as.raw(ts[,8])
    c <- array(raw(), dim=c(len, 3))
    c[,1] <- as.raw(ts[,9])
    c[,2] <- as.raw(ts[,10])
    c[,3] <- as.raw(ts[,11])
    temperature <- ts[,15]
    pressure <- ts[,16]
    rm(ts)                              # may run tight on space
    tt <- seq(t[from.burst], t[to.burst], length.out=len)
    ## trim to the requested interval
    ok <- (from - 1/2) <= tt & tt <= (to + 1/2) # give 1/2 second extra
    data <- list(ts=list(time=tt[ok],
                 heading=approx(t, heading, xout=tt)$y[ok],
                 pitch=approx(t, pitch, xout=tt)$y[ok],
                 roll=approx(t, roll, xout=tt)$y[ok],
                 temperature=temperature,
                 pressure=pressure),
                 ss=list(distance=0),
                 ma=list(v=v[ok,],a=a[ok,],c=c[ok,]))
    metadata <- list(instrument.type="adv",
                     filename=basefile,
                     transformation.matrix=if(!missing(transformation.matrix)) transformation.matrix else NULL,
                     number.of.samples=length(data$x),
                     number.of.beams=3,
                     orientation="upward", # FIXME: guessing on the orientation
                     deltat=as.numeric(difftime(tt[2], tt[1], unit="secs")),
                     sampling.start=data$t[1],
                     oce.coordinate=coordinate.system,
                     coordinate.system=coordinate.system)
    if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processing.log.item(log.action)
    res <- list(data=data, metadata=metadata, processing.log=log.item)
    class(res) <- c("sontek", "adv", "oce")
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
    ts.names <- names(object$data$ts)
    ma.names <- names(object$data$ma)
    fives <- matrix(nrow=(-1+length(ts.names)+length(ma.names)), ncol=5)
    ii <- 1
    for (i in 1:length(ts.names)) {
        if (names(object$data$ts)[i] != "time") {
            fives[ii,] <- fivenum(as.numeric(object$data$ts[[ts.names[i]]]), na.rm=TRUE)
            ii <- ii + 1
        }
    }
    for (i in 1:length(ma.names)) {
        fives[ii,] <- fivenum(as.numeric(object$data$ma[[i]]), na.rm=TRUE)
        ii <- ii + 1
    }
    rownames(fives) <- c(ts.names[ts.names != "time"], ma.names)
    colnames(fives) <- c("Min.", "1st Qu.", "Median", "3rd Qu.", "Max.")
    res <- list(filename=object$metadata$filename,
                number.of.beams=object$metadata$number.of.beams,
                transformation.matrix=object$metadata$transformation.matrix,
                sampling.start=min(object$data$ts$time, na.rm=TRUE),
                sampling.end=max(object$data$ts$time, na.rm=TRUE),
                deltat=object$metadata$deltat,
                instrument.type=object$metadata$instrument.type,
                number.of.samples=length(object$data$ts$time),
                coordinate.system=object$metadata$coordinate.system,
                oce.coordinate=object$metadata$oce.coordinate,
                fives=fives,
                processing.log=processing.log.summary(object))
    if (inherits(object, "nortek"))
        res$burst.length <- object$metadata$burst.length
    class(res) <- "summary.adv"
    res
}

print.summary.adv <- function(x, digits=max(6, getOption("digits") - 1), ...)
{
    cat("ADV summary\n")
    cat("  Instrument type:       ", x$instrument.type, "\n")
    cat("  Filename:              ", x$filename, "\n")
##    cat("  Instrument serial number:   ", x$metadata$serial.number, "\n")
    cat("  Coordinate system:     ", x$coordinate.system, "[originally],", x$oce.coordinate, "[presently]\n")
    cat("  Measurements at times: ", format(x$sampling.start), attr(x$sampling.end, "tzone"),
        "to",
        format(x$sampling.end), attr(x$sampling.end, "tzone"),
        "at interval", x$deltat, "s\n")
##    cat("  Orientation:          ", x$orientation, "\n")
##    cat("  Beam angle:           ", x$metadata$beam.angle, "\n")
    cat("  Number of samples:     ", x$number.of.samples, "\n")

    if (x$instrument.type == "vector") {
        cat("  Burst Length:          ", x$burst.length, "\n")
    }
    if (!is.null(x$transformation.matrix)) {
        cat("  Transformation matrix:      ", format(x$transformation.matrix[1,], width=digits+3, digits=digits), "\n")
        cat("                              ", format(x$transformation.matrix[2,], width=digits+3, digits=digits), "\n")
        cat("                              ", format(x$transformation.matrix[3,], width=digits+3, digits=digits), "\n")
        if (x$number.of.beams > 3)
            cat("                              ", format(x$transformation.matrix[4,], width=digits+3, digits=digits), "\n")
    }
    cat("\nStatistics:\n")
    print(x$fives)
    cat("\n")
    print(x$processing.log)
    invisible(x)
}

plot.adv <- function(x,
                     which=1:3,
                     titles,
                     adorn=NULL,
                     draw.time.range=getOption("oce.draw.time.range"),
                     mgp=getOption("oce.mgp"),
                     mar=c(mgp[1],mgp[1]+1,1,1),
                     margins.as.image=FALSE,
                     cex=1,
                     ylim,
                     ...)
{
    if (!inherits(x, "adv")) stop("method is only for adv objects")
    if (!all(which %in% c(1:3,14:18))) stop("\"which\" must be in the range c(1:3,14:18)")
    opar <- par(no.readonly = TRUE)
    lw <- length(which)

    if (!missing(titles) && length(titles) != lw) stop("length of 'titles' must equal length of 'which'")
    if (lw > 1) on.exit(par(opar))
    par(mgp=mgp, mar=mar, cex=cex)
    dots <- list(...)

    ## user may specify a matrix for ylim
    gave.ylim <- !missing(ylim)
    if (gave.ylim) {
        if (is.matrix(ylim)) {
            if (dim(ylim)[2] != lw) {
                ylim <- matrix(ylim, ncol=2, nrow=lw) # FIXME: is this what I want?
            }
        } else if (is.vector(ylim)) {
            ylim <- matrix(ylim, ncol=2, nrow=lw) # FIXME: is this what I want?
        } else stop("cannot understand ylim; should be 2-element vector, or 2-column matrix")
    }

    adorn.length <- length(adorn)
    if (adorn.length == 1) {
        adorn <- rep(adorn, lw)
        adorn.length <- lw
    }
    if (margins.as.image) {
        w <- 1.5
        lay <- layout(matrix(1:(2*lw), nrow=lw, byrow=TRUE), widths=rep(c(1, lcm(w)), lw))
    } else {
        lay <- layout(cbind(1:lw))
    }
    for (w in 1:lw) {
        par(mgp=mgp, mar=mar, cex=cex)
        if (which[w] == 1) {
            oce.plot.ts(x$data$ts$time, x$data$ma$v[,1],
                        ylab=ad.beam.name(x, 1), type='l', draw.time.range=draw.time.range,
                        adorn=adorn[w],
                        ylim=if (gave.ylim) ylim[w,] else NULL,
                        ...)
        } else if (which[w] == 2) {
            oce.plot.ts(x$data$ts$time, x$data$ma$v[,2],
                        ylab=ad.beam.name(x, 2), type='l', draw.time.range=draw.time.range,
                        adorn=adorn[w],
                        ylim=if (gave.ylim) ylim[w,] else NULL,
                        ...)
        } else if (which[w] == 3) {
            oce.plot.ts(x$data$ts$time, x$data$ma$v[,3],
                        ylab=ad.beam.name(x, 3), type='l', draw.time.range=draw.time.range,
                        adorn=adorn[w],
                        ylim=if (gave.ylim) ylim[w,] else NULL,
                        ...)
        } else if (which[w] == 14) {    # temperature time-series
            oce.plot.ts(x$data$ts$time, x$data$ts$temperature,
                        ylab=resizable.label("T"), type='l', draw.time.range=draw.time.range,
                        adorn=adorn[w],
                        ylim=if (gave.ylim) ylim[w,] else NULL,
                        ...)
        } else if (which[w] == 15) {    # pressure time-series
            oce.plot.ts(x$data$ts$time, x$data$ts$pressure,
                        ylab=resizable.label("p"), type='l', draw.time.range=draw.time.range,
                        adorn=adorn[w],
                        ylim=if (gave.ylim) ylim[w,] else NULL,
                        ...)
        } else if (which[w] == 16) {    # heading
            oce.plot.ts(x$data$ts$time, x$data$ts$heading,
                        ylab="heading", type='l', draw.time.range=draw.time.range,
                        adorn=adorn[w],
                        ylim=if (gave.ylim) ylim[w,] else NULL,
                        ...)
        } else if (which[w] == 17) {    # pitch
            oce.plot.ts(x$data$ts$time, x$data$ts$pitch,
                        ylab="pitch", type='l', draw.time.range=draw.time.range,
                        adorn=adorn[w],
                        ylim=if (gave.ylim) ylim[w,] else NULL,
                        ...)
        } else if (which[w] == 18) {    # roll
            oce.plot.ts(x$data$ts$time, x$data$ts$roll,
                        ylab="roll", type='l', draw.time.range=draw.time.range,
                        adorn=adorn[w],
                        ylim=if (gave.ylim) ylim[w,] else NULL,
                        ...)
        } else {
            stop("unknown value of \"which\":", which)
        }
        draw.time.range <- FALSE
        if (margins.as.image)  {
            ## blank plot, to get axis length same as for images
            omar <- par("mar")
            par(mar=c(mar[1], 1/4, mgp[2]+1/2, mgp[2]+1))
            plot(1:2, 1:2, type='n', axes=FALSE, xlab="", ylab="")
            par(mar=omar)
        }
    }
}

adv.beam2xyz <- function(x)
{
    if (!inherits(x, "adv")) stop("method is only for objects of class \"adv\"")
    if (x$metadata$oce.coordinate != "beam") stop("input must be in beam coordinates, but it is in ", x$metadata$oce.coordinate, " coordinates")
    res <- x
    if (is.null(x$metadata$transformation.matrix)) stop("can't convert coordinates because object metadata$transformation.matrix is NULL")
    transformation.matrix <- x$metadata$transformation.matrix
    ##print(transformation.matrix)
    if (x$metadata$orientation == "downward") {
        transformation.matrix[2,] <- -transformation.matrix[2,]
        transformation.matrix[3,] <- -transformation.matrix[3,]
    }
    ##print(transformation.matrix)
    enu <- transformation.matrix %*% rbind(x$data$ma$v[,1], x$data$ma$v[,2], x$data$ma$v[,3])
    res$data$ma$v[,1] <- enu[1,]
    res$data$ma$v[,2] <- enu[2,]
    res$data$ma$v[,3] <- enu[3,]
    res$metadata$oce.coordinate <- "xyz"
    log.action <- paste(deparse(match.call()), sep="", collapse="")
    processing.log.append(res, log.action)
}

adv.xyz2enu <- function(x)
{
    if (!inherits(x, "adv")) stop("method is only for objects of class \"adv\"")
    if (x$metadata$oce.coordinate != "xyz") stop("input must be in xyz coordinates, but it is in ", x$metadata$oce.coordinate, " coordinates")
    res <- x
    to.radians <- pi / 180
    CH <- cos(to.radians * x$data$ts$heading)
    SH <- sin(to.radians * x$data$ts$heading)
    CP <- cos(to.radians * x$data$ts$pitch)
    SP <- sin(to.radians * x$data$ts$pitch)
    CR <- cos(to.radians * x$data$ts$roll)
    SR <- sin(to.radians * x$data$ts$roll)
    if (x$metadata$orientation == "downward") { #FIXME: I think this is plain wrong; should change sign of row 2 and 3 (??)
        SP <- -SP
        SR <- -SR
    }
    np <- dim(x$data$ma$v)[1]
    tr.mat <- array(numeric(), dim=c(3, 3, np))
    tr.mat[1,1,] <-  CH * CR + SH * SP * SR
    tr.mat[1,2,] <-  SH * CP
    tr.mat[1,3,] <-  CH * SR - SH * SP * CR
    tr.mat[2,1,] <- -SH * CR + CH * SP * SR
    tr.mat[2,2,] <-  CH * CP
    tr.mat[2,3,] <- -SH * SR - CH * SP * CR
    tr.mat[3,1,] <- -CP * SR
    tr.mat[3,2,] <-  SP
    tr.mat[3,3,] <-  CP * CR
    rm(CH,SH,CP,SP,CR,SR)               # might be tight on space
    rotated <- matrix(unlist(lapply(1:np, function(p) tr.mat[,,p] %*% x$data$ma$v[p,])), nrow=3)
    res$data$ma$v[,1] <- rotated[1,]
    res$data$ma$v[,2] <- rotated[2,]
    res$data$ma$v[,3] <- rotated[3,]
    res$metadata$oce.coordinate <- "enu"
    log.action <- paste(deparse(match.call()), sep="", collapse="")
    processing.log.append(res, log.action)
}

adv.enu2other <- function(x, heading=0, pitch=0, roll=0)
{
    if (!inherits(x, "adv")) stop("method is only for objects of class \"adv\"")
    if (x$metadata$oce.coordinate != "enu") stop("input must be in \"enu\" coordinates, but it is in ", x$metadata$oce.coordinate, " coordinates")
    res <- x
    to.radians <- pi / 180
    CH <- cos(to.radians * heading)
    SH <- sin(to.radians * heading)
    CP <- cos(to.radians * pitch)
    SP <- sin(to.radians * pitch)
    CR <- cos(to.radians * roll)
    SR <- sin(to.radians * roll)
    tr.mat <- matrix(c( CH * CR + SH * SP * SR,  SH * CP,  CH * SR - SH * SP * CR,
                       -SH * CR + CH * SP * SR,  CH * CP, -SH * SR - CH * SP * CR,
                       -CP * SR,                 SP,       CP * CR),               nrow=3, byrow=TRUE)
    other <- tr.mat %*% rbind(x$data$ma$v[,1], x$data$ma$v[,2], x$data$ma$v[,3])
    res$data$ma$v[,1] <- other[1,]
    res$data$ma$v[,2] <- other[2,]
    res$data$ma$v[,3] <- other[3,]
    res$metadata$oce.coordinate <- "other"
    log.action <- paste(deparse(match.call()), sep="", collapse="")
    processing.log.append(res, log.action)
}
