read.adv <- function(file, from=1, to, by=1, type=c("nortek", "sontek", "sontek.adr", "sontek.text"), withHeader=TRUE, sampling.start, deltat, tz=getOption("oce.tz"), debug=getOption("oce.debug"), monitor=TRUE, log.action)
{
    type = match.arg(type)
    if (type == "nortek") read.adv.nortek(file=file, from=from, to=to, by=by,
        withHeader=withHeader, sampling.start=sampling.start, deltat=deltat,
        tz=tz, debug=debug, monitor=monitor, log.action=log.action)
    else if (type == "sontek") read.adv.sontek(file=file, from=from, to=to, by=by,
             withHeader=withHeader, sampling.start=sampling.start, deltat=deltat,
             tz=tz, debug=debug, monitor=monitor, log.action=log.action)
    else if (type == "sontek.adr") read.adv.sontek.adr(file=file, from=from, to=to, by=by,
             tz=tz, debug=debug, log.action=log.action)
    else if (type == "sontek.text") read.adv.sontek.text(basefile=file, from=from, to=to, by=by,
             tz=tz, debug=debug, log.action=log.action)
}

read.adv.nortek <- function(file, from=1, to, by=1, type="vector", withHeader=TRUE, sampling.start, deltat, tz=getOption("oce.tz"), debug=getOption("oce.debug"), monitor=TRUE, log.action)
{
    oce.debug(debug, "read.adv.nortek(...,type=\"", type, "\", ...)\n")
    by.is.broken <- TRUE
    if (is.numeric(by)   && by   < 1) stop("argument \"by\" must be 1 or larger")
    if (is.numeric(from) && from < 1) stop("argument \"from\" must be 1 or larger")
    if (is.numeric(to)   && to   < 1) stop("argument \"to\" must be 1 or larger")

    ## if (missing(to)) stop("must supply \"to\" (this limitation may be relaxed in a future version)")
    ## if (!inherits(from, "POSIXt")) stop("\"from\" must be a POSIXt time (this limitation may be relaxed in a future version)")
    ## if (!inherits(to, "POSIXt")) stop("\"to\" must be a POSIXt time (this limitation may be relaxed in a future version)")
    if (!missing(sampling.start)) stop("cannot handle argument \"sampling.start\"")
    if (!missing(deltat)) stop("cannot handle argument \"deltat\"")

    if (is.character(file)) {
        filename <- full.filename(file)
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
    oce.debug(debug, "  read.adv.nortek() about to read header\n")
    oce.debug(debug, "  read.adv.nortek() finished reading header\n")
    # find file length
    seek(file, 0, "end")
    file.size <- seek(file, 0, "start")
    oce.debug(debug, "  file.size=", file.size, "\n")
    buf <- readBin(file, "raw", file.size)
    header <- decode.header.nortek(buf, debug=debug-1)
    metadata <- list(instrument.type="vector",
                     filename=filename,
                     sampling.start=if (missing(sampling.start)) NA else sampling.start,
                     sampling.end=NA,   # FIXME
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
                     transformation.matrix=header$head$transformation.matrix,
                     deployment.name=header$user$deployment.name,
                     cell.size=header$user$cell.size,
                     velocity.scale=header$user$velocity.scale,
                     coordinate.system=header$user$coordinate.system,
                     oce.coordinate=header$user$coordinate.system,
                     oce.beam.attenuated=FALSE)
    if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processing.log.item(log.action)


    ## Find the focus time by bisection, based on "sd" (system data, containing a time).
    bisect.nortek.vector.sd <- function(t.find, add=0, debug=0) { # t.find=time add=offset debug=debug
        oce.debug(debug, "  bisect.nortek.vector.sd(t.find=", format(t.find), ", add=", add, ", debug=", debug, ")\n")
        vsd.len <- length(vsd.start)
        lower <- 1
        upper <- vsd.len
        passes <- floor(10 + log(vsd.len, 2)) # won't need this many; only do this to catch coding errors
        for (pass in 1:passes) {
            middle <- floor((upper + lower) / 2)
            oce.debug(debug, "pass=",pass,"middle=", middle, "\n")
            t <- ISOdatetime(2000 + bcd2integer(buf[vsd.start[middle]+8]),  # year
                             bcd2integer(buf[vsd.start[middle]+9]), # month
                             bcd2integer(buf[vsd.start[middle]+6]), # day
                             bcd2integer(buf[vsd.start[middle]+7]), # hour
                             bcd2integer(buf[vsd.start[middle]+4]), # min
                             bcd2integer(buf[vsd.start[middle]+5]), # sec
                             tz=getOption("oce.tz"))
            if (t.find < t)
                upper <- middle
            else
                lower <- middle
            if (upper - lower < 2)
                break
            oce.debug(debug, "examine: t=", format(t), " middle=", middle, " lower=", lower, " upper=", upper, " pass=", pass, " of max=", passes, "\n")
        }
        middle <- middle + add
        if (middle < 1) middle <- 1
        if (middle > vsd.len) middle <- vsd.len
        t <- ISOdatetime(2000 + bcd2integer(buf[vsd.start[middle]+8]),  # year
                         bcd2integer(buf[vsd.start[middle]+9]), # month
                         bcd2integer(buf[vsd.start[middle]+6]), # day
                         bcd2integer(buf[vsd.start[middle]+7]), # hour
                         bcd2integer(buf[vsd.start[middle]+4]), # min
                         bcd2integer(buf[vsd.start[middle]+5]), # sec
                         tz=getOption("oce.tz"))
        oce.debug(debug, "result: t=", format(t), " at vsd.start[", middle, "]=", vsd.start[middle], "\n")
        return(list(index=middle, time=t)) # index is within vsd
    }

    ## system.time() reveals that a 100Meg file is scanned in 0.2s [macpro desktop, circa 2009]
    vvd.start <- .Call("locate_byte_sequences", buf, c(0xa5, 0x10), 24, c(0xb5, 0x8c), 0)
    vsd.start <- .Call("locate_byte_sequences", buf, c(0xa5, 0x11), 28, c(0xb5, 0x8c), 0)

    ## Window data buffer, using bisection in case of a variable number of vd between sd pairs.
    if (inherits(from, "POSIXt")) {
        if (!inherits(to, "POSIXt")) stop("if 'from' is POSIXt, then 'to' must be, also")
        from.pair <- bisect.nortek.vector.sd(from, -1, debug-1)
        from <- from.index <- from.pair$index
        to.pair <- bisect.nortek.vector.sd(to, 1, debug-1)
        to <- to.index <- to.pair$index
        by.time <- ctime.to.seconds(by)
        oce.debug(debug,
                  "  from=", format(from.pair$t), " yields vsd.start[", from.index, "]\n",
                  "  to  =", format(to.pair$t),   " yields vsd.start[", to.index, "]\n",
                  "  by=", by, "by.time=", by.time, "s\n",
                  "vsd.start[",from.pair$index, "]=", vsd.start[from.pair$index], "at time", format(from.pair$t), "\n",
                  "vsd.start[",  to.pair$index, "]=", vsd.start[  to.pair$index], "at time", format(  to.pair$t), "\n",
                  "vsd.start:\n", str(vsd.start),
                  "vvd.start:\n", str(vvd.start))
        two.times <- ISOdatetime(2000 + bcd2integer(buf[vsd.start[1:2]+8]),  # year
                                 bcd2integer(buf[vsd.start[1:2]+9]), # month
                                 bcd2integer(buf[vsd.start[1:2]+6]), # day
                                 bcd2integer(buf[vsd.start[1:2]+7]), # hour
                                 bcd2integer(buf[vsd.start[1:2]+4]), # min
                                 bcd2integer(buf[vsd.start[1:2]+5]), # sec
                                 tz=getOption("oce.tz"))
        vsd.dt <- as.numeric(difftime(two.times[2], two.times[1], units="secs"))
        vvd.start <- vvd.start[vsd.start[from.index] < vvd.start & vvd.start < vsd.start[to.index]]
        vvd.dt <- vsd.dt * (to.index - from.index) / length(vvd.start)
        by <- by.time / vvd.dt
        vvd.start <- vvd.start[seq(1, length(vvd.start), by=by)]
        oce.debug(debug,
                  'vvd.dt=',vvd.dt,'\n',
                  'by=',by, "1/by=",1/by,"\n",
                  "vvd.start after indexing:\n",
                  str(vvd.start))
        ## find vvd region that lies inside the vsd [from, to] region.
        vvd.start.from <- max(1, vvd.start[vvd.start < from.pair$index])
        vvd.start.to   <- min(length(vvd.start), vvd.start[vvd.start > to.pair$index])
    } else {
        from.index <- from
        to.index <- to
        if (to.index < 1 + from.index) stop("need more separation between from and to")
        oce.debug(debug,
                  'numeric values for args from=',from,'to=',to,'\n',
                  "vvd.start BEFORE\n",
                  str(vvd.start),"\n",
                  "vvd.start AFTER\n",
                  str(vvd.start))
        vvd.start <- vvd.start[from.index:to.index]
        oce.debug(debug, "vsd.start BEFORE\n", str(vsd.start))
        vsd.start <- subset(vsd.start, vvd.start[1] <= vsd.start & vsd.start <= vvd.start[length(vvd.start)])
        oce.debug(debug, "vsd.start AFTER\n", str(vsd.start))
    }
    oce.debug(debug, "step 1 vsd.start:\n", str(vsd.start))
    vsd.start <- vsd.start[vvd.start[1] < vsd.start & vsd.start < vvd.start[length(vvd.start)]]
    oce.debug(debug, "step 2 vsd.start:\n", str(vsd.start))

    if (2 > length(vsd.start)) stop("need at least 2 velocity-system-data chunks to determine the timing; try increasing the difference between 'from' and 'to'")

    if (to.index <= from.index) stop("no data in specified range from=", format(from), " to=", format(to))
    ## we make the times *after* trimming, because this is a slow operation
    vsd.t <- ISOdatetime(2000 + bcd2integer(buf[vsd.start+8]),  # year
                         bcd2integer(buf[vsd.start+9]), # month
                         bcd2integer(buf[vsd.start+6]), # day
                         bcd2integer(buf[vsd.start+7]), # hour
                         bcd2integer(buf[vsd.start+4]), # min
                         bcd2integer(buf[vsd.start+5]), # sec
                         tz=getOption("oce.tz"))
    ##str(vsd.t)
    vsd.len <- length(vsd.start)
    vsd.start2 <- sort(c(vsd.start, 1 + vsd.start))
    heading <- 0.1 * readBin(buf[vsd.start2 + 14], "integer", size=2, n=vsd.len, signed=TRUE, endian="little")
    pitch <-   0.1 * readBin(buf[vsd.start2 + 16], "integer", size=2, n=vsd.len, signed=TRUE, endian="little")
    roll <-    0.1 * readBin(buf[vsd.start2 + 18], "integer", size=2, n=vsd.len, signed=TRUE, endian="little")
    temperature <- 0.01 * readBin(buf[vsd.start2 + 20], "integer", size=2, n=vsd.len, signed=TRUE, endian="little")
    oce.debug(debug,
              "vvd.start (velocity chunks):\n", str(vvd.start),
              "vsd.start (header chunks):\n", str(vsd.start),
              "vsd time:\n", str(vsd.t),
              "vsd heading:\n", str(heading),
              "vsd pitch:\n", str(pitch),
              "vsd roll:\n", str(roll),
              "vsd temperature:\n", str(temperature))
    metadata$burst.length <- round(length(vvd.start) / length(vsd.start), 0)
    vvd.start2 <- sort(c(vvd.start, 1 + vvd.start))
    vvd.len <- length(vvd.start)          # FIXME: should be subsampled with 'by' ... but how???
    p.MSB <- as.numeric(buf[vvd.start + 4])
    p.LSW <- readBin(buf[vvd.start2 + 6], "integer", size=2, n=vvd.len, signed=FALSE, endian="little")
    pressure <- (65536 * p.MSB + p.LSW) / 1000
    oce.debug(debug, "pressure:\n", str(pressure))
    v <- array(dim=c(vvd.len, 3))
    v[,1] <- readBin(buf[vvd.start2 + 10], "integer", size=2, n=vvd.len, signed=TRUE, endian="little") / 1000
    v[,2] <- readBin(buf[vvd.start2 + 12], "integer", size=2, n=vvd.len, signed=TRUE, endian="little") / 1000
    v[,3] <- readBin(buf[vvd.start2 + 14], "integer", size=2, n=vvd.len, signed=TRUE, endian="little") / 1000
    oce.debug(debug, "v[", dim(v), "] begins...\n",  format(v[1:min(10,vvd.len),]))
    a <- array(raw(), dim=c(vvd.len, 3))
    a[,1] <- buf[vvd.start + 16]
    a[,2] <- buf[vvd.start + 17]
    a[,3] <- buf[vvd.start + 18]
    if (debug > 0) {
        cat("a[", dim(a), "] begins...\n")
        print(matrix(as.numeric(a[1:min(10,vvd.len),]), ncol=3))
    }
    c <- array(raw(), dim=c(vvd.len, 3))
    c[,1] <- buf[vvd.start + 19]
    c[,2] <- buf[vvd.start + 20]
    c[,3] <- buf[vvd.start + 21]
    if (debug > 0) {
        cat("c[", dim(c), "] begins...\n")
        print(matrix(as.numeric(c[1:min(10,vvd.len),]), ncol=3))
    }
    sec <- as.numeric(vsd.t - vsd.t[1])
    if (0 != var(diff(sec))) warning("the times in the file are not equi-spaced, but they are taken to be so")
    vsd.i <- 1:length(vsd.start)
    vvd.i <- 1:length(vvd.start)
    vvd.t <- approx(x=vsd.i, y=sec, xout=vvd.i, rule=2)$y
    oce.debug(debug, "vvd.t:\n", str(vvd.t))
    rm(buf)
    gc()
    data <- list(ts=list(time=vvd.t + vsd.t[1],
                 heading=approx(vsd.start, heading, xout=vvd.start, rule=2)$y,
                 pitch=approx(vsd.start, pitch, xout=vvd.start, rule=2)$y,
                 roll=approx(vsd.start, roll, xout=vvd.start, rule=2)$y,
                 temperature=approx(vsd.start, temperature, xout=vvd.start, rule=2)$y,
                 pressure=pressure),
                 ss=list(distance=0),
                 ma=list(v=v, a=a, c=c))
    res <- list(data=data, metadata=metadata, processing.log=log.item)
    class(res) <- c("nortek", "adv", "oce")
    gc()
    res
}

read.adv.sontek <- function(file, from=1, to, by=1, type="default", withHeader=TRUE, sampling.start, deltat, tz=getOption("oce.tz"), debug=getOption("oce.debug"), monitor=TRUE, log.action)
{
    if (is.character(file)) {
        filename <- full.filename(file)
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
    oce.debug(debug, "read.adv.sontek()...\n")
    if (withHeader) {
        stop("cannot read with header yet")
    } else {
        if (missing(sampling.start)) stop("must give 'sampling.start' if withHeader is FALSE")
        if (missing(deltat)) stop("must give 'deltat' if withHeader is FALSE")
        seek(file, 0, "end")
        file.size <- seek(file, 0, "start")
        oce.debug(debug, "file", filename, "has", file.size, "bytes\n")
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

read.adv.sontek.adr <- function(file, from=1, to, by=1, tz=getOption("oce.tz"), debug=getOption("oce.debug"), monitor=TRUE, log.action)
{
    oce.debug(debug, "read.adv.sontek.adr() ENTRY\n")
    if (is.character(file)) {
        filename <- full.filename(file)
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
    hardware.configuration.length <- 24
    probe.configuration.length <- 164
    deployment.parameters.length <- 253
    burst.header.length <- 60
    checksum.length <- 2
    data.length <- 22                   # FIXME: this should be determined based on the headers

    seek(file, 0, "end", rw="read")
    filesize <- seek(file, 0, origin="start", rw="read")
    oce.debug(debug, "filesize=",filesize,"\n")
    ## All details of the binary format come from Appendix 3 of the Sontek ADV
    ## operation Manual - Firmware Version 4.0 (Oct 1997).
    hardware.configuration <- readBin(file, "raw", n=hardware.configuration.length) # 24 total
    pressure.scale <- 1e-9 * readBin(hardware.configuration[9:12], "integer", size=4, n=1, endian="little")
    oce.debug(debug, "pressure.scale=", pressure.scale,"\n")
    probe.configuration <- readBin(file, "raw", n=probe.configuration.length) # 188 total
    deployment.parameters <- readBin(file, "raw", n=deployment.parameters.length) # 441 total
    ## Done reading file for a while; now study the information in these headers ...
    ## Orientation
    orientation.code <- as.integer(hardware.configuration[4])
    if (orientation.code == 0) orientation <- "downward"
    else if (orientation.code == 1) orientation <- "upward"
    else if (orientation.code == 2) orientation <- "sideward"
    else stop("sensor orientation code should be 0 (downward), 1 (upward) or 2 (sideward), but got ", orientation.code)
    ## Is a compass installed (need later, to parse data chunks)?
    compass.code <- as.integer(hardware.configuration[5])
    if (compass.code == 0) compass.installed <- FALSE
    else if (compass.code == 1) compass.installed <- TRUE
    else stop("compass-installed code should be 0 (no) or 1 (yes), but got ", compass.code)
    oce.debug(debug, if (compass.installed) "have a compass in this device\n" else "no compass installed\n")
    ## Is a thermometer installed (need later, to parse data chunks)?
    thermometer.code <- as.integer(hardware.configuration[7])
    if (thermometer.code == 0) thermometer.installed <- FALSE
    else if (thermometer.code == 1) thermometer.installed <- TRUE
    else stop("thermometer-installed code should be 0 (no) or 1 (yes), but got ", thermometer.code)
    oce.debug(debug, if (thermometer.installed) "have a thermometer in this device\n" else "no thermometer installed\n")
    ## Is a pressure gauge installed (need later, to parse data chunks)?
    pressure.code <- as.integer(hardware.configuration[8])
    if (pressure.code == 0) pressure.installed <- FALSE
    else if (pressure.code == 1) pressure.installed <- TRUE
    else stop("pressure-installed code should be 0 (no) or 1 (yes), but got ", pressure.code)
    oce.debug(debug, if (pressure.installed) "have a pressure gauge in this device\n" else "no pressure installed\n")
    ## FIXME: in the above, ignoring "RecorderInstalled" on p105 of docs -- what is that??

    ## m3 (MUN ADV) data, as a test: 22 bytes in data chunks
    cpu.software.ver.num <- as.integer(hardware.configuration[1])/10 # 8.5
    dsp.software.ver.num <- as.integer(hardware.configuration[2])/10 # 4.1
    serial.number <- paste(integer2ascii(as.integer(probe.configuration[10+1:5])), collapse="")  # "B373H"
    oce.debug(debug, "serial.number=",serial.number,"\n")
    if (deployment.parameters[1]!=0x12) stop("first byte of deployment-parameters header should be 0x12 but it is 0x", deployment.parameters[1])
    if (deployment.parameters[2]!=0x01) stop("first byte of deployment-parameters header should be 0x01 but it is 0x", deployment.parameters[2])
    coordinate.system.code <- as.integer(deployment.parameters[22]) # 1 (0=beam 1=xyz 2=ENU)
    if (coordinate.system.code == 0)
        coordinate.system <- "beam"
    else if (coordinate.system.code == 1)
        coordinate.system <- "xyz"
    else if (coordinate.system.code == 2)
        coordinate.system <- "enu"
    else {
        warning("cannot understand coordinate system code of ", coordinate.system.code, "so assuming \"xyz\" coordinates")
        coordinate.system <- "xyz"
    }
    if (coordinate.system == "beam") stop("cannot deal with beam coordinates in this version of the package, because the SonTek documentation (Appendix 3 of ADV Operation Manual) does not say how the transformation matrix is stored in the file header")

    oce.debug(debug, "coordinate.system=", coordinate.system, "\n")

    ## bug: docs say sampling rate in 0.1Hz, but the SLEIWEX-2008-m3 data file shows 0.01Hz
    sampling.rate <- 0.01*readBin(deployment.parameters[23:28], "integer", n=3, size=2, endian="little", signed=FALSE) # 600 0 0
    if (sampling.rate[2] !=0 || sampling.rate[3] != 0) stop("due to a limitation in the package, the sampling rate must be a number and then two zeros, not ", paste(sampling.rate, collapse=" "))
    if (sampling.rate[1] < 0) stop("sampling rate must be a positive integer, but got ", sampling.rate)
    burst.interval <- readBin(deployment.parameters[29:34], "integer", n=3, size=2, endian="little", signed=FALSE) # 3600 0 0
    if (burst.interval[2] !=0 || burst.interval[3] != 0) stop("due to a limitation in the package, the burst interval must be a number and then two zeros, not ", paste(burst.interval, collapse=" "))

    samples.per.burst <- readBin(deployment.parameters[35:40], "integer", n=3, size=2, endian="little", signed=FALSE) # 21540     0     0
    if (samples.per.burst[2] !=0 || samples.per.burst[3] != 0) stop("due to a limitation in the package, samples/burst must be a number and then two zeros, not ", paste(samples.per.burst, collapse=" "))

    if (samples.per.burst[1] < 0) stop("samples/burst must be a positive integer, but got ", samples.per.burst)

    deployment.name <- paste(integer2ascii(as.integer(deployment.parameters[49:57])), collapse="") # "SLW08"
    comments1 <- paste(integer2ascii(as.integer(deployment.parameters[66:125])), collapse="")
    comments2 <- paste(integer2ascii(as.integer(deployment.parameters[126:185])), collapse="")
    comments3 <- paste(integer2ascii(as.integer(deployment.parameters[126:185])), collapse="")
    ## Run through to get bursts.  These files are so large that I don't trust a match against codes
    ## to get the burst headers (plus, the burst header in my data file does not match the docs!),
    ## so the algorithm is to go through the file burst by burst, skipping the data portions.  If
    ## bursts have a lot of data, this won't slow things down terribly.
    burst.number <- burst.location <- burst.time <- samples.per.burst <- NULL
    file.pointer <- seek(file, origin="start", rw="read")          # see where we are now
    while (file.pointer < filesize) {
        ## BurstHeader
        burst.location <- c(burst.location, file.pointer) # burst location in file
        burst.header <- readBin(file, "raw", burst.header.length)
        oce.debug(debug, "burst header:", paste(burst.header, collapse=" "), "\n")
        if (burst.header[1] != 0xa5) stop("expecting first byte after header to be 0xa5, but got 0x", burst.header[1])
        ## 0x10 is from manual, but we have 0x11 in the SLEIWEX-2008/m3 device, so permit that also
        if (burst.header[2] != 0x10 && burst.header[2] != 0x11) stop("expecting second byte after header to be 0x10 or 0x11, but got 0x", burst.header[2])
        burst.number <- c(burst.number, readBin(burst.header[15:18], "integer", size=4, signed=FALSE, endian="little"))
        ## NOTE: in next line, factor 0.01 disagrees with documentation on page 107, but matches data SLEIWEX-2008/m3
        burst.sampling.rate <- 0.01*readBin(burst.header[29:30], "integer", size=2, n=1, endian="little", signed=FALSE)
        this.samples.per.burst <- readBin(burst.header[31:32], "integer", size=2, n=1, endian="little", signed=FALSE)
        oce.debug(debug, "samples in burst:", this.samples.per.burst, "\n")
        samples.per.burst <- c(samples.per.burst, this.samples.per.burst)
        ## burst.header[34] is meant to indicate what is recorded, but values in SLEIWEX2008/m3 do not match docs
        ## print(byte2binary(burst.header[34], endian="little"))
        year <- readBin(burst.header[19:20], "integer", size=2, n=1, signed=FALSE, endian="little")
        day <- as.integer(burst.header[21])
        month <- as.integer(burst.header[22])
        minute <- as.integer(burst.header[23])
        hour <- as.integer(burst.header[24])
        sec100 <- as.integer(burst.header[25])
        sec <- as.integer(burst.header[26])
        oce.debug(debug, "burst time:  year=",year, "day=",day, "month=",month, "hour=",hour, "minute=",minute, "sec=",sec, "sec100=",sec100,"\n")
        time <- ISOdatetime(year=year, month=month, day=day, hour=hour, min=minute, sec=sec+0.01*sec100, tz=tz)
        burst.time <- c(burst.time, time)
        oce.debug(debug, "  i.e. ", format(time), "\n")
        file.pointer <- file.pointer + (burst.header.length + checksum.length + data.length * this.samples.per.burst)
        oce.debug(debug, "seeking to byte ", file.pointer, " to get next burst\n")
        seek(file, where=file.pointer, origin="start", rw="read")
    }
    class(burst.time) <- c("POSIXt", "POSIXct")
    attr(burst.time, "tzone") <- tz
    ## Now, determine relevant bursts, using the parameters: from, to, and by.
    if (!missing(by)) stop("cannot handle argument 'by' in this version of Oce")
    ok <- (from - 3600) <= burst.time & burst.time <= (to + 3600) # add an hour as a grace period
    if (sum(ok) < 1) stop("no data in the stated time interval")
    burst.number <- burst.number[ok]
    burst.time <- burst.time[ok]

    samples.per.burst <- samples.per.burst[ok]
    burst.location <- burst.location[ok]
    if (debug > 0) print(data.frame(number=burst.number,
                                time=burst.time,
                                start.byte=burst.location,
                                data.start.byte=burst.location+burst.header.length,
                                samples=samples.per.burst))
    bursts <- length(burst.location)
    ntotal <- sum(samples.per.burst)
    oce.debug(debug, "ntotal:", ntotal,"***\n")
    v <- array(numeric(), dim=c(ntotal, 3))
    heading <- array(numeric(), dim=c(ntotal, 1))
    pitch <- array(numeric(), dim=c(ntotal, 1))
    roll <- array(numeric(), dim=c(ntotal, 1))
    temperature <- array(numeric(), dim=c(ntotal, 1))
    pressure <- array(numeric(), dim=c(ntotal, 1))
    a <- array(raw(), dim=c(ntotal, 3))
    c <- array(raw(), dim=c(ntotal, 3))
    row.offset <- 0
    for (burst in 1:bursts) {
        n <- samples.per.burst[burst]
        oce.debug(debug, "burst", burst.number[burst], "at", format(burst.time[burst]), "data start at byte", burst.location[burst]+burst.header.length, "n=",n,"\n")
        seek(file, burst.location[burst]+burst.header.length, origin="start", rw="read")
        b <- readBin(file, "raw", n=data.length*n, endian="little")
        m <- matrix(b, ncol=data.length, byrow=TRUE)
        if (n != dim(m)[1]) stop("something is wrong with the data.  Perhaps the record length is not the assumed value of ", data.length)
        ## FIXME possibly this as.raw() and t() business is slow ... not sure how else to do it!
        r <- row.offset + 1:n
        v[r,1] <- 1e-4 * readBin(as.raw(t(m[,1:2])), "integer", size=2, n=n, signed=TRUE, endian="little")
        v[r,2] <- 1e-4 * readBin(as.raw(t(m[,3:4])), "integer", size=2, n=n, signed=TRUE, endian="little")
        v[r,3] <- 1e-4 * readBin(as.raw(t(m[,5:6])), "integer", size=2, n=n, signed=TRUE, endian="little")
        a[r,1] <- m[,7]
        a[r,2] <- m[,8]
        a[r,3] <- m[,9]
        c[r,1] <- m[,10]
        c[r,2] <- m[,11]
        c[r,3] <- m[,12]
        heading[r] <- 0.1 * readBin(as.raw(t(m[,13:14])), "integer", n=n, size=2, signed=TRUE, endian="little")
        pitch[r] <- 0.1 * readBin(as.raw(t(m[,15:16])), "integer", n=n, size=2, signed=TRUE, endian="little")
        roll[r] <- 0.1 * readBin(as.raw(t(m[,17:18])), "integer", n=n, size=2, signed=TRUE, endian="little")
        temperature[r] <- 0.01 * readBin(as.raw(t(m[,19:20])), "integer", size=2, n=n, signed=TRUE, endian="little")
        pressure[r] <- pressure.scale*readBin(as.raw(t(m[,21:22])), "integer", size=2, n=n, signed=TRUE, endian="little")
        row.offset <- row.offset + n
    }
    rm(b, m)                            # save a bit of space
    time <- seq(from=burst.time[1], by=1/burst.sampling.rate, length.out=ntotal)
    ok <- (from - 1) <= time & time <= (to+1) # final trim
    metadata <- list(filename=filename,
                     filesize=filesize,
                     instrument.type="sontek",
                     number.of.samples=ntotal, # FIXME
                     deltat=1/sampling.rate[1], #CHECK
                     sampling.start=0,       #FIXME
                     orientation=orientation,
                     cpu.software.ver.num=cpu.software.ver.num,
                     dsp.software.ver.num=dsp.software.ver.num,
                     serial.number=serial.number,
                     coordinate.system=coordinate.system,
                     oce.coordinate=coordinate.system,
                     ## sampling.rate=sampling.rate, # not used
                     samples.per.burst=samples.per.burst[1],
                     burst.interval=burst.interval[1],
                     deployment.name=deployment.name,
                     comments1=comments1,
                     comments2=comments2,
                     comments3=comments3)
    data <- list(ts=list(time=time[ok],
                 heading=heading[ok], pitch=pitch[ok], roll=roll[ok], temperature=temperature[ok], pressure=pressure[ok]),
                 ss=list(distance=0),
                 ma=list(v=v[ok,], a=a[ok,], c=c[ok,]))
    if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processing.log.item(log.action)
    res <- list(data=data, metadata=metadata, processing.log=log.item)
    class(res) <- c("sontek", "adv", "oce")
    res
}


read.adv.sontek.text <- function(basefile, from=1, to, by=1, coordinate.system="xyz", transformation.matrix, tz=getOption("oce.tz"), debug=getOption("oce.debug"), log.action)
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
    oce.debug(debug, "number of bursts: ", number.of.bursts, "\n")
    t <- ISOdatetime(year=hdt[,2], month=hdt[,3], day=hdt[,4], hour=hdt[,5], min=hdt[,6], sec=hdt[,7], tz=tz)
    if (inherits(from, "POSIXt")) {
        ignore <- t < from
        if (sum(ignore) == 0) stop("no data in this time interval, starting at time ", from, "\n")
        from.burst <- which(ignore == FALSE)[1]
        oce.debug(debug, "\"from\" is burst number", from.burst, "at", format(t[from.burst]), "\n")
    } else {
        from.burst <- from + 1          # 0 means first burst
    }
    if (missing(to)) {
        stop("must supply \"to\"")
    } else {
        if (inherits(from, "POSIXt")) {
            ignore <- t < to
            if (sum(ignore) == 0) stop("no data in this time interval, starting at time ", to, "\n")
            to.burst <- which(ignore == FALSE)[1] + 1 # add 1 since we'll chop later
            to.burst <- min(to.burst, length(t))
            oce.debug(debug, "\"to\" is burst number", to.burst, "at", format(t[to.burst]), "\n")
        } else {
            to.burst <- to
        }
    }
    ##voltage <- hdt[,14]
    heading <- hdt[,24]
    pitch <- hdt[,25]
    roll <- hdt[,26]
    spb <- hdt[1,9]                      # FIXME may this change over time?
    sr <- spb / 3600

    ts.file <- file(ts, "rb")
    on.exit(close(ts.file))
    if (!inherits(ts.file, "connection"))
        stop("argument `ts.file' must be a character string or connection")

    ## Examine ".ts1" file to see if we can deal with it.
    seek(ts.file, where=0, origin="end")
    bytes.in.file <- seek(ts.file, where=0, origin="start")
    oce.debug(debug, "length of \".", suffices[2], "\" file: ",bytes.in.file," bytes\n")
    look <- min(5000, bytes.in.file)
    b <- readBin(ts.file, "raw", n=look)
    newlines <- which(b == 0x0a)
    if (0 != diff(range(fivenum(diff(newlines))))) stop("need equal line lengths in ", ts)
    ## Line length
    bytes.in.sample <- diff(newlines)[1]
    oce.debug(debug, "line length in \".", suffices[2], "\" file: ", bytes.in.sample, " bytes\n")
    ## elements per line
    seek(ts.file, where=newlines[1], origin="start")
    d <- scan(ts.file, what="character", nlines=1, quiet=TRUE)
    oce.debug(debug, "first line in \".", suffices[2], "\" file: ", paste(d, collapse=" "), "\n")
    items.per.line <- length(d)
    if (items.per.sample != length(d)) stop("file \".", suffices[2], "\" should have ", items.per.sample, " elemetns per line, but it has ", length(d))
    oce.debug(debug, "elements per line in \".", suffices[2], "\" file: ", length(d), "\n")
    lines.in.file <- bytes.in.file / bytes.in.sample
    oce.debug(debug, "lines in \".", suffices[2], "\" file: ", lines.in.file, "\n")

    samples.per.burst <- lines.in.file / number.of.bursts
    oce.debug(debug, "samples per burst: ", samples.per.burst, "\n")

    from.byte <- from.burst * samples.per.burst * bytes.in.sample
    to.byte <- to.burst * samples.per.burst * bytes.in.sample
    oce.debug(debug, "seek from:", from.byte, "\n", "seek to:", to.byte, "\n")
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
                 heading=approx(t, heading, xout=tt, rule=2)$y[ok],
                 pitch=approx(t, pitch, xout=tt, rule=2)$y[ok],
                 roll=approx(t, roll, xout=tt, rule=2)$y[ok],
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
                     deltat=as.numeric(difftime(tt[2], tt[1], units="secs")),
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
    len <- length(object$data$ts$time)
    res <- list(filename=object$metadata$filename,
                number.of.beams=object$metadata$number.of.beams,
                transformation.matrix=object$metadata$transformation.matrix,
                subsample.start=min(object$data$ts$time, na.rm=TRUE),
                subsample.end=max(object$data$ts$time, na.rm=TRUE),
                subsample.deltat=as.numeric(difftime(object$data$ts$time[len], object$data$ts$time[1], units="secs")/len), # BUG: wrong
                instrument.type=object$metadata$instrument.type,
                serial.number=object$metadata$serial.number,
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
    cat("ADV Summary\n", ...)
    cat("  Instrument:            ", x$instrument.type, "; serial number:", x$serial.number, "\n")
    cat("  Source:                ", x$filename, "\n")
    cat(sprintf("  Subsamples:         %s %s to %s %s at interval %.2f s\n",
                format(x$subsample.start), attr(x$subsample.start, "tzone"),
                format(x$subsample.end),  attr(x$subsample.end, "tzone"),
                x$subsample.deltat), ...)
    ## cat("  Orientation:          ", x$orientation, "\n")
    ## cat("  Beam angle:           ", x$metadata$beam.angle, "\n")
    cat("  Number of samples:     ", x$number.of.samples, "\n")
    cat("  Coordinate system:     ", x$coordinate.system, "[originally],", x$oce.coordinate, "[presently]\n")
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
    cat("\nStatistics:\n", ...)
    cat(show.fives(x), ...)
    cat("\n", ...)
    cat("Processing Log:\n", ...)
    cat(x$processing.log, ...)
    invisible(x)
}

plot.adv <- function(x,
                     which=1:3,
                     titles,
                     adorn=NULL,
                     draw.time.range=getOption("oce.draw.time.range"),
                     mgp=getOption("oce.mgp"),
                     mar=c(mgp[1]+1,mgp[1]+1,2,1.5),
                     margins.as.image=FALSE,
                     cex=1,
                     xlim, ylim,
                     debug=getOption("oce.debug"),
                     ...)
{
    if (!inherits(x, "adv")) stop("method is only for adv objects")
    if (!all(which %in% c(1:3,14:21))) stop("\"which\" must be in the range c(1:3,14:21)")
    opar <- par(no.readonly = TRUE)
    lw <- length(which)

    if (!missing(titles) && length(titles) != lw) stop("length of 'titles' must equal length of 'which'")
    if (lw > 1) on.exit(par(opar))
    par(mgp=mgp, mar=mar, cex=cex)
    dots <- list(...)

    ## user may specify a matrix for xlim and ylim
    gave.ylim <- !missing(ylim)
    if (gave.ylim) {
        if (is.matrix(ylim)) {
            if (dim(ylim)[2] != lw) {
                ylim2 <- matrix(ylim, ncol=2, nrow=lw) # FIXME: is this what I want?
            }
        } else {
            ylim2 <- matrix(ylim, ncol=2, nrow=lw) # FIXME: is this what I want?
        }
        class(ylim2) <- class(ylim)
        ylim <- ylim2
    }
    gave.xlim <- !missing(xlim)
    if (gave.xlim) {
        if (is.matrix(xlim)) {
            if (dim(xlim)[2] != lw) {
                xlim2 <- matrix(xlim, ncol=2, nrow=lw) # FIXME: is this what I want?
            }
        } else {
            xlim2 <- matrix(xlim, ncol=2, nrow=lw) # FIXME: is this what I want?
        }
        class(xlim2) <- class(xlim)
        xlim <- xlim2
    }

    adorn.length <- length(adorn)
    if (adorn.length == 1) {
        adorn <- rep(adorn, lw)
        adorn.length <- lw
    }
    if (debug > 0) {
        cat("adorn:\n")
        print(adorn)
    }
    if (lw > 1) {
        if (margins.as.image) {
            w <- 1.5
            lay <- layout(matrix(1:(2*lw), nrow=lw, byrow=TRUE), widths=rep(c(1, lcm(w)), lw))
        } else {
            lay <- layout(cbind(1:lw))
        }
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
                        ylab=resizable.label("T", "y"), type='l', draw.time.range=draw.time.range,
                        adorn=adorn[w],
                        ylim=if (gave.ylim) ylim[w,] else NULL,
                        ...)
        } else if (which[w] == 15) {    # pressure time-series
            oce.plot.ts(x$data$ts$time, x$data$ts$pressure,
                        ylab=resizable.label("p", "y"), type='l', draw.time.range=draw.time.range,
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
        } else if (which[w] == 19) {    # beam 1 corrleation-amplitude diagnostic plot
            a <- as.integer(x$data$ma$a[,1])
            c <- as.integer(x$data$ma$c[,1])
            smoothScatter(a, c, nbin=64, xlab="Amplitude", ylab="Correlation",
                          xlim=if (gave.xlim) xlim[w,], ylim=if (gave.ylim) ylim[w,])
            mtext(ad.beam.name(x, 1))
        } else if (which[w] == 20) {    # beam 2 corrleation-amplitude diagnostic plot
            a <- as.integer(x$data$ma$a[,2])
            c <- as.integer(x$data$ma$c[,2])
            smoothScatter(a, c, nbin=64, xlab="Amplitude", ylab="Correlation",
                          xlim=if (gave.xlim) xlim[w,], ylim=if (gave.ylim) ylim[w,])
            mtext(ad.beam.name(x, 2))
        } else if (which[w] == 21) {    # beam 3 corrleation-amplitude diagnostic plot
            a <- as.integer(x$data$ma$a[,3])
            c <- as.integer(x$data$ma$c[,3])
            smoothScatter(a, c, nbin=64, xlab="Amplitude", ylab="Correlation",
                          xlim=if (gave.xlim) xlim[w,], ylim=if (gave.ylim) ylim[w,])
            mtext(ad.beam.name(x, 3))
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
