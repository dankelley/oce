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
                            type="default",
                            withHeader=TRUE, sampling.start, deltat,
                            debug=0, monitor=TRUE, log.action)
{
    sync.code <- as.raw(0xa5)
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
                     size=header$head$size,
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
    cat("AT:", seek(file, 0, "end"), "\n")
    file.length <- seek(file, 0, "start")
    cat("file.length=", file.length, "\n")

    file.length <- 9e6L                 # FIXME: trim to test

    buf <<- readBin(file, "raw", n=file.length, endian="little")


    ## m8 >> vec(1).vel(1:3,1:10)
    ##
    ##ans =
    ##
    ##-0.3260    0.4520   -0.3920    0.5130   -0.2260   -0.2580    0.6820    0.2060   -0.1840    0.3340
    ## 0.1990   -0.0250    0.3510    0.6770   -0.0360   -0.4170   -0.0890    0.0060   -0.5630    0.4050
    ## 0.1140    0.7170    0.0950   -0.4090   -0.1860   -0.7460    0.0140    0.5230    0.2810   -0.0300



    ## items seem to be as follows.  FIXME: is there a required order or interlacing scheme?
    ## a5 a12 = vector velocity data header (putatively 42 bytes)
    ## a5 a10 = vector velocity data (putatively 24 bytes)
    ## a5 a11 = vector system data (putatively 28 bytes)

    ## velocity header data start 0xa5 0x12; for offsets, see page 35 of System Integrator Guide

    ## Using match.bytes() may speed up the operation but it may us more memory.
    if (TRUE) {
        vhi <- match.bytes(buf, 0xa5, 0x12, 0x15) # need 3rd byte (header length) to avoid spurious matches
    } else {
        vhi <- which(buf == 0xa5)
        vhi <- vhi[buf[vhi+1] == 0x12]
        vhi <- vhi[buf[vhi+2] == 0x15]
    }
    cat("velo-headers at:\n", paste(vhi,collapse=" "), "\n")

    lvh <-  length(vhi)
    vhi2 <- sort(c(vhi, vhi+1))
    vh.size <- readBin(buf[vhi2+2], "integer", size=2, n=lvh, signed=FALSE, endian="little")
    cat("vh.size=",paste(vh.size, collapse=" "),"\n")
    time <- ISOdatetime(2000 + bcd2integer(buf[vhi+8]),  # seems to start in Y2K
                        bcd2integer(buf[vhi+9]),         # month
                        bcd2integer(buf[vhi+6]),         # day
                        bcd2integer(buf[vhi+7]),         # hour
                        bcd2integer(buf[vhi+4]),         # min
                        bcd2integer(buf[vhi+5]),         # sec
                        tz=getOption("oce.tz"))

    ## 42 bytes [velocity data header]
    vsd <- vhi + 2*vh.size

    cat("next should be 0xa5 0x11 if velo-system (28 bytes)\n")
    print(buf[vsd+seq(0,27,1)])

    cat("next should be 0xa5 0x11 if velo-system (28 bytes)\n")
    print(buf[vsd+28+seq(0,27,1)])

    offset <- 6                         # FIXME: WHY DOES THIS WORK?
    for (chunk in 1:10) {
        cat("chunk=",chunk,"is velo (0xa5 0x10)? indices:",
            offset+vsd+2*vh.size+2*28 + chunk*24,
            "to",
            offset+vsd+2*vh.size+2*28 + chunk*24+23,"\n")
        print(buf[offset+vsd+2*vh.size+2*28+chunk*24+seq(0,23,1)])
    }

    stop()

    cat("trying to get nrecords from", paste(buf[vhi2+10], collapse=" "), "\n")
    print(vhi2+10)

    nrecords <- readBin(buf[vhi2+10], "integer", size=2, n=lvh, signed=FALSE, endian="little")

    cat("nrecords=",paste(nrecords, collapse=" "),"\n")

    cat("time="); print(format(time))

    cat("buf for this header is:\n")
    print(buf[vhi:(vhi+size)])
    stop()


    lbuf <- length(buf)
    velo.data.at <- match.bytes(buf[vhi[1] + vh.size[1]]:lbuf, 0xa5, 0x10)
    print(velo.data.at[1:10])

    stop()

    ## velocity data start 0xa5 0x10
    vdi <- which(buf == 0xa5)           # vdi stands for velocity data index
    vdi <- vdi[buf[vdi+1] == 0x10]
    export.vdi <<- vdi
    lvd <- length(vdi)
    vdi2 <- sort(c(vdi, vdi+1))
    cat("length(header data)=",lvh," and length(velocity data)=",lvd,"\n")
    count <- as.integer(buf[vdi+3])
    p  <- readBin(buf[vdi2+ 6], "integer", size=2, n=lvd, signed=FALSE, endian="little")
    v1 <- readBin(buf[vdi2+10], "integer", size=2, n=lvd, signed=TRUE,  endian="little")
    v2 <- readBin(buf[vdi2+12], "integer", size=2, n=lvd, signed=TRUE,  endian="little")
    v3 <- readBin(buf[vdi2+14], "integer", size=2, n=lvd, signed=TRUE,  endian="little")

    ## NOTES:
    ## 1. count increases, modulo 255
    ## 2. the number of samples varies -- I don't understand the
    ##    timing of samples, or, really, of times.
    ##> d$data$time[1:10]
    ##[1] "2008-06-25 10:00:01 UTC" "2008-06-25 10:00:02 UTC" "2008-06-25 10:00:03 UTC"
    ##[4] "2008-06-25 10:00:04 UTC" "2008-06-25 10:00:05 UTC" "2008-06-25 10:00:06 UTC"
    ##[7] "2008-06-25 10:00:07 UTC" "2008-06-25 10:00:08 UTC" "2008-06-25 10:00:09 UTC"
    ##[10] "2008-06-25 10:00:10 UTC"
    ##> d$data$nrecords[1:10]
    ##[1] 133 131 131 130 130 130 130 131 130 130


    data <- data.frame(time=time,
                       nrecords=nrecords,
                       sample.number=rep(1,length(time)),
                       count=count[1:length(time)],
                       p=p[1:length(time)],
                       v1=v1[1:length(time)],
                       v2=v2[1:length(time)],
                       v3=v3[1:length(time)],
                       a1=rep(1,length(time)),
                       a2=rep(1,length(time)),
                       a3=rep(1,length(time)),
                       c1=rep(1,length(time)),
                       c2=rep(1,length(time)),
                       c3=rep(1,length(time)),
                       temperature=rep(1,length(time)),
                       pressure=rep(1,length(time)))
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
        ;
    } else if (inherits(object, "nortek")) {
        ;
    } else stop("can only summarize ADV objects of sub-type \"nortek\" or \"sontek\", not class ", paste(class(object),collapse=","))
    names <- names(object$data)
    fives <- matrix(nrow=(-1+length(names)), ncol=5)
    ii <- 1
    for (i in 1:length(names)) {
        if (names(object$data)[i] != "time") {
            fives[ii,] <- fivenum(object$data[[names[i]]], na.rm=TRUE)
            ii <- ii + 1
        }
    }
    rownames(fives) <- names[names != "time"]
    colnames(fives) <- c("Min.", "1st Qu.", "Median", "3rd Qu.", "Max.")
    res <- list(#res.specific,
                filename=object$metadata$filename,
                sampling.start=object$metadata$sampling.start,
                sampling.end=object$metadata$sampling.start + object$metadata$number.of.samples*object$metadata$deltat,
                deltat=object$metadata$deltat,
                instrument.type=object$metadata$instrument.type,
                number.of.samples=length(object$data$x),
                fives=fives,
                processing.log=processing.log.summary(object))
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
    if (!all(which %in% 1:3)) stop("\"which\" must be in the range 1:3")
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
                        if (smooth) as.numeric(smooth(x$data$x)) else x$data$x,
                        ylab="u [m/s]", type='l', draw.time.range=draw.time.range, ...)
        }
        if (which[w] == 2) {
            oce.plot.ts(x$data$time,
                        if (smooth) as.numeric(smooth(x$data$y)) else x$data$y,
                        ylab="v [m/s]", type='l', draw.time.range=draw.time.range, ...)
        }
        if (which[w] == 3) {
            oce.plot.ts(x$data$time,
                        if (smooth) as.numeric(smooth(x$data$z)) else x$data$z,
                        ylab="w [m/s]", type='l', draw.time.range=draw.time.range, ...)
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
