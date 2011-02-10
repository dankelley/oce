read.adv <- function(file, from=1, to, by=1, tz=getOption("oce.tz"),
                     type=c("nortek", "sontek", "sontek.adr", "sontek.text"),
                     header=TRUE,
                     latitude=NA, longitude=NA,
                     start, deltat,
                     debug=getOption("oce.debug"), monitor=TRUE, log.action)
{
    type = match.arg(type)
    ## FIXME: all these read.adv variants should have the same argument list
    if (type == "nortek")
        read.adv.nortek(file=file, from=from, to=to, by=by, tz=tz,
                        header=header,
                        latitude=latitude, longitude=longitude,
                        debug=debug, monitor=monitor, log.action=log.action)
    else if (type == "sontek") # guess
        read.adv.sontek.serial(file=file, from=from, to=to, by=by, tz=tz,
                               latitude=latitude, longitude=longitude,
                               start=start, deltat=deltat,
                               debug=debug, monitor=monitor, log.action=log.action)
    else if (type == "sontek.adr")
        read.adv.sontek.adr(file=file, from=from, to=to, by=by, tz=tz,
                            latitude=latitude, longitude=longitude,
                            debug=debug, log.action=log.action)
    else if (type == "sontek.text")
        read.adv.sontek.text(basefile=file, from=from, to=to, by=by, tz=tz,
                             latitude=latitude, longitude=longitude,
                             debug=debug, log.action=log.action)
    else
        stop("read.adv() cannot understand type = \"", type, "\"")
}

read.adv.nortek <- function(file, from=1, to, by=1, tz=getOption("oce.tz"),
                            type="vector",
                            header=TRUE,
                            latitude=NA, longitude=NA,
                            debug=getOption("oce.debug"), monitor=TRUE, log.action)
{
    ## abbreviations:
    ##   SIG=System Integrator Guide
    ##   vvd=vector velocity data [p35 SIG], containing the data: pressure, vel, amp, corr (plus sensemble counter etc)
    ##   vsd=velocity system data [p36 SIG], containing times, temperatures, angles, etc
    ## NOTE: we interpolate from vsd to vvd, to get the final data$ts$time, etc.

    oce.debug(debug, "\b\bread.adv.nortek(file=\"", file, "\", type=\"", type, "\", from=", format(from), ", to=", format(to), ", by=", by, ", tz=\"", tz, "\", type=\"", type, "\", header=", header, ", debug=", debug, ", monitor=", monitor, ", log.action=(not shown)) {\n", sep="")
    if (is.numeric(by) && by < 1)
        stop("cannot handle negative 'by' values")
    if (is.numeric(by)   && by   < 1) stop("argument \"by\" must be 1 or larger")
    if (is.numeric(from) && from < 1) stop("argument \"from\" must be 1 or larger")
    if (!missing(to) && is.numeric(to)   && to   < 1) stop("argument \"to\" must be 1 or larger")

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
    if (!header) stop("header must be TRUE")
    oce.debug(debug, "  read.adv.nortek() about to read header\n")
    oce.debug(debug, "  read.adv.nortek() finished reading header\n")
                                        # find file length
    seek(file, 0, "end")
    file.size <- seek(file, 0, "start")
    oce.debug(debug, "  file.size=", file.size, "\n")
    buf <- readBin(file, "raw", file.size)
    header <- decode.header.nortek(buf, debug=debug-1)
    if (debug > 1) {                    # Note: need high debugging to get this
        cat("\nheader is as follows:\n")
        str(header)
    }
    metadata <- list(manufacturer="nortek",
                     instrument.type="vector",
                     filename=filename,
                     latitude=latitude, longitude=longitude,
                     measurement.start=NA, # FIXME
                     measurement.end=NA,   # FIXME
                     sampling.rate=NA, # FIXME
                     number.of.beams=header$head$number.of.beams, # FIXME: check that this is correct
                     serial.number=header$hardware$serial.number,
                     frequency=header$head$frequency,
                     internal.code.version=header$hardware$pic.version,
                     software.version=header$user$sw.version,
                     hardware.revision=header$hardware$hw.revision,
                     rec.size=header$hardware$rec.size,
                     velocity.range=header$hardware$velocity.range,
                     firmware.version=header$hardware$fw.version,
                     config=header$hardware$config,
                     config.pressure.sensor=header$head$config.pressure.sensor,
                     config.magnetometer.sensor=header$head$config.magnetometer.sensor,
                     config.tilt.sensor=header$head$config.tilt.sensor,
                     beam.angle=25,     # FIXME: should read from file
                     tilt.sensor.orientation=header$head$tilt.sensor.orientation,
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
                     oce.beam.attenuated=FALSE,
                     deploy.name=header$user$deploy.name,
                     comments=header$user$comments)
    if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processing.log.item(log.action)
    ## Find the focus time by bisection, based on "sd" (system data, containing a time).
    bisect.nortek.vector.sd <- function(t.find, add=0, debug=0) { # t.find=time add=offset debug=debug
        oce.debug(debug, "\n")
        oce.debug(debug, "bisect.nortek.vector.sd(t.find=", format(t.find), ", add=", add, ", debug=", debug, ")\n")
        vsd.len <- length(vsd.start)
        lower <- 1
        upper <- vsd.len
        passes <- floor(10 + log(vsd.len, 2)) # won't need this many; only do this to catch coding errors
        for (pass in 1:passes) {
            middle <- floor((upper + lower) / 2)
            t <- ISOdatetime(2000 + bcd2integer(buf[vsd.start[middle]+8]),  # year
                             bcd2integer(buf[vsd.start[middle]+9]), # month
                             bcd2integer(buf[vsd.start[middle]+6]), # day
                             bcd2integer(buf[vsd.start[middle]+7]), # hour
                             bcd2integer(buf[vsd.start[middle]+4]), # min
                             bcd2integer(buf[vsd.start[middle]+5]), # sec
                             tz=tz)
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
                         tz=tz)
        oce.debug(debug, "result: t=", format(t), " at vsd.start[", middle, "]=", vsd.start[middle], "\n")
        return(list(index=middle, time=t)) # index is within vsd
    }
    ## system.time() reveals that a 100Meg file is scanned in 0.2s [macpro desktop, circa 2009]
    vvd.start <- .Call("locate_byte_sequences", buf, c(0xa5, 0x10), 24, c(0xb5, 0x8c), 0)
    vsd.start <- .Call("locate_byte_sequences", buf, c(0xa5, 0x11), 28, c(0xb5, 0x8c), 0)
    ##TEST## .vsd <<- buf[vsd.start[1] + seq(0, 27)] # FIXME: remove
    ## FIXME: determine whether to use the velocity scale in next line, or other value.
    oce.debug(debug, "VSD", paste("0x", format(as.raw(buf[vsd.start[1]+0:27])),sep=""), "\n")

    ## Velocity scale.  Nortek's System Integrator Guide (p36) says
    ## the velocity scale is in bit 1 of "status" byte (at offset 23)
    ## in the Vector System Data header.  However, they seem to count
    ## bits in the opposite way as oce does, so their bit 1 (starting
    ## from 0) corresponds to our bit 7 (ending at 8).
    ##
    ## NOTE: the S.I.G. is confusing on the velocity scale, and this
    ## confusion resulted in a change to the present code on
    ## 2010-09-13.  Page 35 of S.I.G. clearly states that velocities
    ## are in mm/s, which was used in the code previously.  However,
    ## p44 contradicts this, saying that there are two possible scale
    ## factors, namely 1mm/s and 0.1mm/s.  Starting on 2010-09-13, the
    ## present function started using this possibility of two scale
    ## factors, as determined in the next code line, following p36.
    metadata$velocity.scale <- if ("0" == substr(byte2binary(buf[vsd.start[1] + 23], endian="big"), 7, 7)) 1e-3 else 0.1e-3
    oce.debug(debug, "velocity scale:", metadata$velocity.scale, "m/s (from VSD header byte 24, 0x",
              as.raw(buf[vsd.start[1] + 23]), "(bit 7 of",
              byte2binary(buf[vsd.start[1] + 23], endian="big"), ")\n")

    ## Measurement start and end times.
    vsd.len <- length(vsd.start)
    metadata$measurement.start <- ISOdatetime(2000 + bcd2integer(buf[vsd.start[1]+8]),  # year
                                              bcd2integer(buf[vsd.start[1]+9]), # month
                                              bcd2integer(buf[vsd.start[1]+6]), # day
                                              bcd2integer(buf[vsd.start[1]+7]), # hour
                                              bcd2integer(buf[vsd.start[1]+4]), # min
                                              bcd2integer(buf[vsd.start[1]+5]), # sec
                                              tz=tz)
    metadata$measurement.end <- ISOdatetime(2000 + bcd2integer(buf[vsd.start[vsd.len]+8]),  # year
                                            bcd2integer(buf[vsd.start[vsd.len]+9]), # month
                                            bcd2integer(buf[vsd.start[vsd.len]+6]), # day
                                            bcd2integer(buf[vsd.start[vsd.len]+7]), # hour
                                            bcd2integer(buf[vsd.start[vsd.len]+4]), # min
                                            bcd2integer(buf[vsd.start[vsd.len]+5]), # sec
                                            tz=tz)
    vvd.len <- length(vvd.start)
    metadata$measurement.deltat <- (as.numeric(metadata$measurement.end) - as.numeric(metadata$measurement.start)) / (vvd.len - 1)

    if (missing(to))
        stop("must supply 'to'")

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
                  "vsd.start[",  to.pair$index, "]=", vsd.start[  to.pair$index], "at time", format(  to.pair$t), "\n")
        two.times <- ISOdatetime(2000 + bcd2integer(buf[vsd.start[1:2]+8]),  # year
                                 bcd2integer(buf[vsd.start[1:2]+9]), # month
                                 bcd2integer(buf[vsd.start[1:2]+6]), # day
                                 bcd2integer(buf[vsd.start[1:2]+7]), # hour
                                 bcd2integer(buf[vsd.start[1:2]+4]), # min
                                 bcd2integer(buf[vsd.start[1:2]+5]), # sec  NOTE: nortek files lack fractional seconds
                                 tz=tz)
        vsd.dt <- as.numeric(two.times[2]) - as.numeric(two.times[1]) # FIXME: need # samples per burst here

        ## Next two lines suggest that readBin() can be used instead of bcd2integer ... I imagine it would be faster
        ##cat("month=", readBin(buf[vsd.start[1]+9], "integer", n=1, size=1, endian="little"), "(as readBin)\n")
        ##cat("month=", bcd2integer(buf[vsd.start[1]+9]), "(as bcd)\n")

        oce.debug(debug, "nrecords=", readBin(buf[vsd.start[1]+10:11], "integer", n=1, size=2, endian="little"), "\n")
        oce.debug(debug, "vsd.dt=",vsd.dt,"(from two.times)\n")

        vvd.start <- vvd.start[vsd.start[from.index] <= vvd.start & vvd.start <= vsd.start[to.index]]
        vvd.dt <- vsd.dt * (to.index - from.index) / length(vvd.start)
        ## find vvd region that lies inside the vsd [from, to] region.
        vvd.start.from <- max(1, vvd.start[vvd.start < from.pair$index])
        vvd.start.to   <- min(length(vvd.start), vvd.start[vvd.start > to.pair$index])
    } else {
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
                      "vsd.start[",  to.pair$index, "]=", vsd.start[  to.pair$index], "at time", format(  to.pair$t), "\n")
            two.times <- ISOdatetime(2000 + bcd2integer(buf[vsd.start[1:2]+8]),  # year
                                     bcd2integer(buf[vsd.start[1:2]+9]), # month
                                     bcd2integer(buf[vsd.start[1:2]+6]), # day
                                     bcd2integer(buf[vsd.start[1:2]+7]), # hour
                                     bcd2integer(buf[vsd.start[1:2]+4]), # min
                                     bcd2integer(buf[vsd.start[1:2]+5]), # sec  NOTE: nortek files lack fractional seconds
                                     tz=tz)
            vsd.dt <- as.numeric(two.times[2]) - as.numeric(two.times[1]) # FIXME: need # samples per burst here
            ## Next two lines suggest that readBin() can be used instead of bcd2integer ... I imagine it would be faster
            ##cat("month=", readBin(buf[vsd.start[1]+9], "integer", n=1, size=1, endian="little"), "(as readBin)\n")
            ##cat("month=", bcd2integer(buf[vsd.start[1]+9]), "(as bcd)\n")
            oce.debug(debug, "nrecords=", readBin(buf[vsd.start[1]+10:11], "integer", n=1, size=2, endian="little"), "\n")
            oce.debug(debug, "vsd.dt=",vsd.dt,"(from two.times)\n")
            vvd.start <- vvd.start[vsd.start[from.index] < vvd.start & vvd.start < vsd.start[to.index]]
            vvd.dt <- vsd.dt * (to.index - from.index) / length(vvd.start)
            oce.debug(debug,
                      'vvd.dt=',vvd.dt,'\n',
                      'by=',by, "1/by=",1/by,"\n",
                      "vvd.start after indexing:\n",
                      str(vvd.start))
            ## find vvd region that lies inside the vsd [from, to] region.
            vvd.start.from <- max(1, vvd.start[vvd.start < from.pair$index])
            vvd.start.to   <- min(length(vvd.start), vvd.start[vvd.start > to.pair$index])
        } else {
            oce.debug(debug, 'numeric values for args from=',from,'to=',to,'by=', by, '\n')
            from.index <- from
            to.index <- to
            if (to.index < 1 + from.index) stop("need more separation between from and to")
            oce.debug(debug, "from.index=", from.index, "to.index=", to.index, "\n")
            oce.debug(debug, vector.show(vvd.start, "before subset, vvd.start is"))
            vvd.start <- vvd.start[from.index:to.index]
            oce.debug(debug, vector.show(vvd.start, "    ... later, vvd.start is"))
            oce.debug(debug, vector.show(vsd.start, "before subset, vsd.start is"))
            vsd.start.from <- which(vvd.start[1] < vsd.start)[1]
            vsd.start.to <- which(vsd.start > vvd.start[length(vvd.start)])[1]
            oce.debug(debug, "vsd.start.from=", vsd.start.from, "and vsd.start.to=", vsd.start.to, "(raw)\n")
            vsd.start <- vsd.start[seq(vsd.start.from, vsd.start.to)]
            oce.debug(debug, vector.show(vsd.start, "    ... later, vsd.start is"))
        }
    }
    oce.debug(debug, "about to trim vsd.start, based on vvd.start[1]=", vvd.start[1], " and vvd.start[length(vvd.start)]=", vvd.start[length(vvd.start)], "\n")
    oce.debug(debug, vector.show(vsd.start, "before trimming, vsd.start:"))
    oce.debug(debug, "from=", from, "to=", to, "\n")

    ## Find spanning subset, expanded a little for now
    subset.start <- which.max(vvd.start[1] < vsd.start)
    if (subset.start > 1)
        subset.start <- subset.start - 1 # extend a bit (for now)
    subset.end <- which.min(vsd.start < vvd.start[length(vvd.start)])
    oce.debug(debug, "first guess: subset.end=", subset.end, "\n")

    if (subset.end < length(vsd.start))
        subset.end <- subset.end + 1

    oce.debug(debug, "try start vsd.start[subset.start=", subset.start, "] = ", vsd.start[subset.start], "\n")
    oce.debug(debug, "try end   vsd.start[subset.end=  ", subset.end,   "] = ", vsd.start[subset.end],   "\n")
    oce.debug(debug, vector.show(vsd.start, "before trimming, vsd.start:"))
    vsd.start <- vsd.start[seq(subset.start, subset.end-1, 1)]
    oce.debug(debug, vector.show(vsd.start, "after  trimming, vsd.start:"))

    if (2 > length(vsd.start))
        stop("need at least 2 velocity-system-data chunks to determine the timing; try increasing the difference between 'from' and 'to'")

    if (to.index <= from.index)
        stop("no data in specified range from=", format(from), " to=", format(to))

    ## we make the times *after* trimming, because this is a slow operation
    ## NOTE: the ISOdatetime() call takes 60% of the entire time for this function.
    vsd.t <- ISOdatetime(2000 + bcd2integer(buf[vsd.start+8]),  # year
                         bcd2integer(buf[vsd.start+9]), # month
                         bcd2integer(buf[vsd.start+6]), # day
                         bcd2integer(buf[vsd.start+7]), # hour
                         bcd2integer(buf[vsd.start+4]), # min
                         bcd2integer(buf[vsd.start+5]), # sec
                         tz=tz)

    oce.debug(debug, "reading Nortek Vector, and using timezone: ", tz, "\n")

    ## update metadata$measurement.deltat
    metadata$measurement.deltat <- mean(diff(as.numeric(vsd.t)), na.rm=TRUE) * length(vsd.start) / length(vvd.start) # FIXME

    vsd.len <- length(vsd.start)
    vsd.start2 <- sort(c(vsd.start, 1 + vsd.start))
    heading <- 0.1 * readBin(buf[vsd.start2 + 14], "integer", size=2, n=vsd.len, signed=TRUE, endian="little")
    oce.debug(debug, vector.show(heading, "heading"))
    pitch <-   0.1 * readBin(buf[vsd.start2 + 16], "integer", size=2, n=vsd.len, signed=TRUE, endian="little")
    oce.debug(debug, vector.show(pitch, "pitch"))
    roll <-    0.1 * readBin(buf[vsd.start2 + 18], "integer", size=2, n=vsd.len, signed=TRUE, endian="little")
    oce.debug(debug, vector.show(roll, "roll"))
    temperature <- 0.01 * readBin(buf[vsd.start2 + 20], "integer", size=2, n=vsd.len, signed=TRUE, endian="little")
    oce.debug(debug, vector.show(temperature, "temperature"))
    ## byte 22 is an error code
    ## byte 23 is status, with bit 0 being orientation (p36 of Nortek's System Integrator Guide)
    status <- buf[vsd.start[floor(0.5*length(vsd.start))] + 23]
    metadata$orientation <- if ("0" == substr(byte2binary(status, endian="big"), 1, 1)) "upwards" else "downwards"
    ##
    metadata$burst.length <- round(length(vvd.start) / length(vsd.start), 0) # FIXME: surely this is in the header (?!?)
    oce.debug(debug, vector.show(metadata$burst.length, "burst.length"))
    vvd.start2 <- sort(c(vvd.start, 1 + vvd.start))
    vvd.len <- length(vvd.start)          # FIXME: should be subsampled with 'by' ... but how???
    p.MSB <- as.numeric(buf[vvd.start + 4])
    p.LSW <- readBin(buf[vvd.start2 + 6], "integer", size=2, n=vvd.len, signed=FALSE, endian="little")
    pressure <- (65536 * p.MSB + p.LSW) / 1000
    oce.debug(debug, vector.show(pressure, "pressure"))
    v <- array(dim=c(vvd.len, 3))
    v[,1] <- metadata$velocity.scale * readBin(buf[vvd.start2 + 10], "integer", size=2, n=vvd.len, signed=TRUE, endian="little")
    v[,2] <- metadata$velocity.scale * readBin(buf[vvd.start2 + 12], "integer", size=2, n=vvd.len, signed=TRUE, endian="little")
    v[,3] <- metadata$velocity.scale * readBin(buf[vvd.start2 + 14], "integer", size=2, n=vvd.len, signed=TRUE, endian="little")
    if (debug > 0) {
        oce.debug(debug, "v[", dim(v), "] begins...\n")
        print(matrix(as.numeric(v[1:min(3,vvd.len),]), ncol=3))
    }
    a <- array(raw(), dim=c(vvd.len, 3))
    a[,1] <- buf[vvd.start + 16]
    a[,2] <- buf[vvd.start + 17]
    a[,3] <- buf[vvd.start + 18]
    if (debug > 0) {
        oce.debug(debug, "a[", dim(a), "] begins...\n")
        print(matrix(as.numeric(a[1:min(3,vvd.len),]), ncol=3))
    }
    c <- array(raw(), dim=c(vvd.len, 3))
    c[,1] <- buf[vvd.start + 19]
    c[,2] <- buf[vvd.start + 20]
    c[,3] <- buf[vvd.start + 21]
    if (debug > 0) {
        cat("c[", dim(c), "] begins...\n")
        print(matrix(as.numeric(c[1:min(3,vvd.len),]), ncol=3))
    }
    sec <- as.numeric(vsd.t) - as.numeric(vsd.t[1])
    vds <- var(diff(sec))
    if (!is.na(vds) & 0 != vds)
        warning("the times in the file are not equi-spaced, but they are taken to be so")
    vvd.sec <- .Call("stutter_time", sec, 8)
    oce.debug(debug, vector.show(vvd.sec, "vvd.sec"))
    oce.debug(debug, vector.show(vsd.start, "vsd.start"))
    oce.debug(debug, vector.show(vvd.start, "vvd.start"))
    rm(buf)
    gc()
    ## subset using 'by'
    by.orig <- by
    if (is.character(by)) {
        oce.debug(debug, "by='",by,"' given as argument to read.adv.nortek()\n",sep="")
        oce.debug(debug, " ... infer to be", ctime.to.seconds(by), "s\n")
        by <- ctime.to.seconds(by) / metadata$measurement.deltat
        oce.debug(debug, " ... so step by" ,by,"through the data\n")
    }
    len <- length(vvd.start)
    look <- seq(1, len, by=by)
    oce.debug(debug, "length(vvd.start)=",length(vvd.start),"\n")
    vvd.start.orig <- vvd.start
    vvd.start <- vvd.start[look]
    oce.debug(debug, "length(vvd.start)=",length(vvd.start),"(after 'look'ing) with by=", by, "\n")
    ##heading <- approx(vsd.start, heading, xout=vvd.start, rule=2)$y
    ##pitch <- approx(vsd.start, pitch, xout=vvd.start, rule=2)$y
    ##roll <- approx(vsd.start, roll, xout=vvd.start, rule=2)$y
    ##temperature <- approx(vsd.start, temperature, xout=vvd.start, rule=2)$y
    vvd.sec <- vvd.sec[look]
    pressure <- pressure[look]          # only output at burst headers, not with velo
    v <- v[look,]
    a <- a[look,]
    c <- c[look,]
    ##oce.debug(debug, "vvd.sec=", vvd.sec[1], ",", vvd.sec[2], "...\n")
    ##cat(vector.show(vsd.t[1:10]))
    ## vsd at 1Hz; vvd at sampling rate
    time <- vvd.sec + vsd.t[1]
    ##print(attributes(time)) # is time out somehow?
    ##print(time[1])
    data <- list(ts=list(time=time,
                         ##heading=heading, pitch=pitch, roll=roll, temperature=temperature,
                         pressure=pressure),
                 ts.slow=list(time=vsd.t, heading=heading, pitch=pitch, roll=roll, temperature=temperature),
                 ##ss=list(distance=0),   # FIXME: why even have this?
                 ma=list(v=v, a=a, c=c))
    res <- list(data=data, metadata=metadata, processing.log=log.item)
    class(res) <- c("nortek", "adv", "oce")
    oce.debug(debug, "\b\b} # read.adv.nortek(file=\"", filename, "\", ...)\n", sep="")
    res
}

read.adv.sontek.serial <- function(file, from=1, to, by=1, tz=getOption("oce.tz"),
                                   type="default",
                                   latitude=NA, longitude=NA,
                                   start, deltat,
                                   debug=getOption("oce.debug"), monitor=TRUE, log.action)
{
    oce.debug(debug, paste("\b\bread.adv.sontek.serial(file[1]=\"", file[1],
                           "\", from=", format(from),
                           if (!missing(to)) sprintf(", to=%s, ", format(to)),
                           ", by=", by,
                           ", type=\"", type,
                           if (!missing(start)) sprintf(", start[1]=%s, ", format(start[1])),
                           if (!missing(deltat)) sprintf(", deltat=%f, ", deltat),
                           "debug=", debug,
                           ", monitor=", monitor,
                           ", log.action=(not shown)) {\n", sep=""))
    if (missing(start))
        stop("must supply start, a POSIXct time (or suitable string for the time, in UTC) at which the first observation was made")
    if (is.numeric(start))
        stop("'start' must be a string, or a POSIXt time")
    if (is.character(start))
        start <- as.POSIXct(start, tz=tz)
    if (missing(deltat))
        stop("must supply deltat, the number of seconds between observations")
    nstart <- length(start)
    nfile <- length(file)
    if (nstart != nfile)
        stop("length of 'file' must equal length of 'start', but they are ", nfile, " and ", nstart, " respectively")
    warning("cannot infer coordinate system, etc., since header=FALSE; see documentation.")
    oce.debug(debug, "time series is inferred to start at", format(start[1]), "\n")
    if (is.character(deltat))
        deltat <- ctime.to.seconds(deltat)
    oce.debug(debug, "time series is inferred to have data every", deltat, "s\n")

    if (nstart > 1) {                   # handle multiple files
        oce.debug(debug, "handling multiple files\n")
        buf <- NULL
        for (i in 1:nfile) {
            oce.debug(debug, "loading \"", file[i], "\" (start.time ", format(start[i]), " ", attr(start[i], "tzone"), ")\n", sep="")
            this.file <- file(file[i], "rb")
            seek(this.file, 0, "end", rw="read")
            file.size <- seek(this.file, 0, origin="start", rw="read")
            oce.debug(debug, "file.size=",file.size,"\n")
            buf <- c(buf, readBin(this.file, what="raw", n=file.size, endian="little"))
            close(this.file)
        }
        filename <- paste("(\"", file[i], "\", ...)", sep="")
    } else {                            # handle single file (which might be a connection, etc)
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
        ## read whole file into buffer
        seek(file, 0, "end", rw="read")
        file.size <- seek(file, 0, origin="start", rw="read")
        oce.debug(debug, "filesize=",file.size,"\n")
        buf <- readBin(file, what="raw", n=file.size, endian="little")
    }

    p <- .Call("ldc_sontek_adv_22", buf, 0) # the 0 means to get all pointers to data chunks
    pp <- sort(c(p, p+1))
    len <- length(p)
    oce.debug(debug, "dp:", paste(unique(diff(p)), collapse=","), "\n")
    serial.number <- readBin(buf[pp+2], "integer", size=2, n=len, signed=FALSE, endian="little")
    serial.number <- .Call("unwrap_sequence_numbers", serial.number, 2)
    velocity.scale <- 0.1e-3
    v <- array(numeric(), dim=c(len, 3))
    v[,1] <- readBin(buf[pp+4], "integer", size=2, n=len, signed=TRUE, endian="little") * velocity.scale
    v[,2] <- readBin(buf[pp+6], "integer", size=2, n=len, signed=TRUE, endian="little") * velocity.scale
    v[,3] <- readBin(buf[pp+8], "integer", size=2, n=len, signed=TRUE, endian="little") * velocity.scale
    a <- array(raw(), dim=c(len, 3))
    a[,1] <- as.raw(readBin(buf[p+10], "integer", size=1, n=len, signed=FALSE, endian="little"))
    a[,2] <- as.raw(readBin(buf[p+11], "integer", size=1, n=len, signed=FALSE, endian="little"))
    a[,3] <- as.raw(readBin(buf[p+12], "integer", size=1, n=len, signed=FALSE, endian="little"))
    c <- array(raw(), dim=c(len, 3))
    c[,1] <- as.raw(readBin(buf[p+13], "integer", size=1, n=len, signed=FALSE, endian="little"))
    c[,2] <- as.raw(readBin(buf[p+14], "integer", size=1, n=len, signed=FALSE, endian="little"))
    c[,3] <- as.raw(readBin(buf[p+15], "integer", size=1, n=len, signed=FALSE, endian="little"))
    temperature <- 0.01 * readBin(buf[pp+16], "integer", size=2, n=len, signed=TRUE, endian="little")
    pressure <- readBin(buf[pp+18], "integer", size=2, n=len, signed=FALSE, endian="little") # may be 0 for all

    ## FIXME: Sontek ADV transformation matrix equal for all units?  (Nortek Vector is not.)
    transformation.matrix <- rbind(c(11033,  -8503, -5238),
                                   c(  347, -32767,  9338),
                                   c(-1418,  -1476, -1333)) / 4096
    time <- start[1] + (serial.number - serial.number[1]) * deltat
    deltat <- mean(diff(as.numeric(time)))
    metadata <- list(manufacturer="sontek",
                     instrument.type="adv",
                     serial.number="?",
                     filename=filename,
                     latitude=latitude,
                     longitude=longitude,
                     transformation.matrix=transformation.matrix,
                     measurement.start=time[1],
                     measurement.end=time[length(time)],
                     measurement.deltat=deltat,
                     subsample.start=time[1],
                     subsample.end=mean(diff(as.numeric(time))),
                     subsample.deltat=deltat,
                     coordinate.system="xyz", # guess
                     oce.coordinate="xyz",    # guess
                     orientation="up")        # guess

    nt <- length(time)
    data <- list(ts=list(time=time,
                         heading=rep(0, nt), # user will need to fill this in
                         pitch=rep(0, nt), #  user will need to fill this in
                         roll=rep(0, nt),  # user will need to fill this in
                         temperature=temperature,
                         pressure=pressure),
                 ##ss=list(distance=0),
                 ma=list(v=v,a=a,c=c))
    warning("sontek adv in serial format lacks heading, pitch and roll: user must fill in")
    if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processing.log.item(log.action)
    res <- list(data=data, metadata=metadata, processing.log=log.item)
    class(res) <- c("sontek", "adv", "oce")
    res
}

read.adv.sontek.adr <- function(file, from=1, to, by=1, tz=getOption("oce.tz"),      # FIXME (two timescales)
                                header=TRUE,
                                latitude=NA, longitude=NA,
                                type="",
                                debug=getOption("oce.debug"), monitor=TRUE, log.action)
{
    bisect.adv.sontek.adr <- function(t.find, add=0, debug=0) {
        oce.debug(debug, "bisect.adv.sontek.adr(t.find=", format(t.find), ", add=", add, "\n")
        len <- length(burst.time)
        lower <- 1
        upper <- len
        passes <- floor(10 + log(len, 2)) # won't need this many; only do this to catch coding errors
        for (pass in 1:passes) {
            middle <- floor((upper + lower) / 2)
            t <- burst.time[middle]
            if (t.find < t)
                upper <- middle
            else
                lower <- middle
            if (upper - lower < 2)
                break
            oce.debug(debug, paste("burst.time[", middle, "] = ", format(t), " (at pass ", pass, " of ", passes, ")\n", sep=""))
        }
        middle <- middle + add          # may use add to extend before and after window
        if (middle < 1) middle <- 1
        if (middle > len) middle <- len
        t <- burst.time[middle]
        oce.debug(debug, "result: t=", format(t), "\n")
        return(list(index=middle, time=t))
    }

    ## The binary format is documented in Appendix 2.2.3 of the Sontek ADV
    ## operation Manual - Firmware Version 4.0 (Oct 1997).
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
    ## read whole file into 'buf'
    seek(file, 0, "end", rw="read")
    file.size <- seek(file, 0, origin="start", rw="read")
    oce.debug(debug, "filesize=",file.size,"\n")
    buf <- readBin(file, what="raw", n=file.size, endian="little")

    ## Read header, or create a nominal default one.
    ##  24 bytes hardware configuration ("AdvSystemConfigType" in the docs)
    ## 164 bytes probe configuration ("AdvConfType" in the docs)
    ## 253 bytes deployment setup ("AdvDeploymentSetupType" in the docs)
    hardware.configuration.length <- 24
    probe.configuration.length <- 164
    deployment.parameters.length <- 253
    burst.header.length <- 60
    checksum.length <- 2
    data.length <- 22                   # FIXME: this should be determined based on the headers
    metadata <- list(manufacturer="sontek",
                     instrument.type="adv", # FIXME or "adr"???
                     filename=filename,
                     latitude=latitude, longitude=longitude,
                     measurement.deltat=1,
                     velocity.scale.factor=1)
    if (header) {
        ##
        ## Slice out three headers
        ##
        hardware.configuration <- buf[1:hardware.configuration.length]
        probe.configuration <- buf[hardware.configuration.length + 1:probe.configuration.length]
        deployment.parameters <- buf[hardware.configuration.length+probe.configuration.length+1:deployment.parameters.length]

        ##
        ## Analyze "hardware configuration" header
        ##
        metadata$cpu.software.ver.num <- 0.1 * as.numeric(hardware.configuration[1])
        oce.debug(debug, "cpu.software.ver.num=", metadata$cpu.software.ver.num, "\n")

        metadata$dsp.software.ver.num <- 0.1 * as.numeric(hardware.configuration[2])
        oce.debug(debug, "dsp.software.ver.num=", metadata$dsp.software.ver.num, "\n")

        metadata$orientation <- c("downward", "upward", "sideways")[1 + as.numeric(hardware.configuration[4])]
        oce.debug(debug, "orientation=", metadata$orientation, "\n")

        metadata$compass.installed <- as.integer(hardware.configuration[5]) == 1
        oce.debug(debug, "compass.installed=", metadata$compass.installed, "\n")
        if (!metadata$compass.installed)
            stop("cannot handle data files for ADV files that lack compass data")

        metadata$recorder.installed <- if (as.integer(hardware.configuration[6]) == 1) TRUE else FALSE;
        oce.debug(debug, "recorder.installed=", metadata$recorder.installed, "\n")

        metadata$thermometer.installed <- as.integer(hardware.configuration[7]) == 1
        oce.debug(debug, "thermometer.installed=", metadata$thermometer.installed, "\n")
        if (!metadata$thermometer.installed)
            stop("cannot handle data files for ADV files that lack thermometer data")

        metadata$pressure.installed <- as.integer(hardware.configuration[8]) == 1
        oce.debug(debug, "pressure.installed=", metadata$pressure.installed, "\n")
        if (!metadata$pressure.installed)
            stop("cannot handle data files for ADV files that lack pressure data")

        ## we report pressure in dbar, so use the fact that 1 nanobar/count = 1e-8 dbar/count
        metadata$pressure.scale <- 1e-8 * readBin(hardware.configuration[9:12], "integer", size=4, n=1, endian="little", signed=FALSE)
        oce.debug(debug, "pressure.scale=", metadata$pressure.scale,"dbar/count (header gives in nanobar/count)\n")

        ## we report pressure in dbar, so use the fact that 1 microbar = 1e-5 dbar
        metadata$pressure.offset <- 1e-5 * readBin(hardware.configuration[13:16], "integer", size=4, n=1, endian="little", signed=TRUE)
        oce.debug(debug, "pressure.offset=", metadata$pressure.offset,"dbar (header gives in microbar)\n")

        metadata$compass.offset <- readBin(hardware.configuration[23:24], "integer", size=2, n=1, endian="little", signed=TRUE)
        oce.debug(debug, "compass offset=", metadata$compass.offset,"(degrees to East of North)\n")

        metadata$press.freq.offset <- as.integer(hardware.configuration[25])
        oce.debug(debug, "press.freq.offset=", metadata$press.freq.offset,"(\"Frequency Pres Sensor Offset\" in docs)\n")

        metadata$ext.sensor.installed <- as.integer(hardware.configuration[26])
        oce.debug(debug, "ext.sensor.installed=", metadata$ext.sensor.installed,"(\"0=None, 1=Standard (ch 1/3)\" in docs)\n")

        metadata$ext.press.installed <- as.integer(hardware.configuration[27])
        oce.debug(debug, "ext.press.installed=", metadata$ext.press.installed,"(1=Paros 2=Druck 3=ParosFreq)\n")

        ## we report pressure in dbar, so use the fact that 1 pbar = 1e-11 dbar
        metadata$pressure.scale.2 <- 1e-11 * readBin(hardware.configuration[28:29], "integer", size=2, n=1, endian="little", signed=TRUE)
        oce.debug(debug, "pressure.scale.2=", metadata$pressure.scale.2,"dbar/count^2 (file gives in picobar/count^2)\n")


        ##
        ## Analyze "probe configuration" header
        ## Docs (p102 of Sontek-ADV-op-man-2001.pdf) say as follows (the initial index number is mine):
        ## [1] unsigned char FileType
        ## [2] unsigned char FileVer
        ## [3:6] DateType FileDate (4 bytes for real-time clock, 2 for year, 1 for day, 1 for month)
        ## [7:10] long FileNbytes
        ## [11:16] SerialNum[6]
        ## [16] char ProbeType
        ## [17] char ProbeSize
        ## [18:19] int ProbeNBeams
        ## ...

        metadata$serial.number <- paste(readBin(probe.configuration[11:16],"character",n=5,size=1), collapse="")  # "B373H"
        oce.debug(debug, "serial.number=",metadata$serial.number,"\n")

        metadata$probe.type <- readBin(probe.configuration[17], "integer", n=1, size=1)
        oce.debug(debug, "probe.type=", metadata$probe.type, "(\"3/2-d orientation\", according to the docs)\n")

        metadata$probe.size <- readBin(probe.configuration[18], "integer", n=1, size=1)
        oce.debug(debug, "probe.size=", metadata$probe.size, "(0 means 5cm; 1 means 10cm probe, according to docs)\n")

        metadata$number.of.beams <- readBin(probe.configuration[19:20], "integer", n=1, size=2, endian="little")
        oce.debug(debug, "number.of.beams=", metadata$number.of.beams, "(should be 3)\n")
        if (metadata$number.of.beams != 3)
            warning("number of beams should be 3, but it is ", metadata$number.of.beams, " ... reseting to 3")

        metadata$probe.nom.peak.pos <- readBin(probe.configuration[21:22], "integer", n=1, size=2, endian="little")
        oce.debug(debug, "probe.nom.peak.pos=", metadata$probe.nom.peak.pos, "(not used here)\n")

        metadata$probe.nsamp <- readBin(probe.configuration[23:24], "integer", n=1, size=2, endian="little")
        oce.debug(debug, "probe.nsamp=", metadata$probe.nsamp, "(not used here)\n")

        metadata$probe.samp.interval <- readBin(probe.configuration[25:26], "integer", n=1, size=2, endian="little")
        oce.debug(debug, "probe.samp.interval=", metadata$probe.samp.interval, "(not used here)\n")

        metadata$probe.pulse.lag <- readBin(probe.configuration[27:56], "integer", n=15, size=2, endian="little")
        oce.debug(debug, "probe.pulse.lag=", metadata$probe.pulse.lag, "([5][3], not used here)\n")

        metadata$probe.nxmit <- readBin(probe.configuration[57:86], "integer", n=15, size=2, endian="little")
        oce.debug(debug, "probe.nxmit=", metadata$probe.nxmit, "([5][3], not used here)\n")

        metadata$probe.lag.delay <- readBin(probe.configuration[87:116], "integer", n=15, size=2, endian="little")
        oce.debug(debug, "probe.lag.delay=", metadata$probe.lag.delay, "([5][3], not used here)\n")

        metadata$probe.beam.delay <- readBin(probe.configuration[117:118], "integer", n=1, size=2, endian="little")
        oce.debug(debug, "probe.beam.delay=", metadata$probe.beam.delay, "(not used here)\n")

        metadata$probe.ping.delay <- readBin(probe.configuration[119:120], "integer", n=1, size=2, endian="little")
        oce.debug(debug, "probe.ping.delay=", metadata$probe.ping.delay, "(not used here)\n")

        metadata$transformation.matrix <- matrix(readBin(probe.configuration[121:157], "numeric", n=9, size=4, endian="little"),
                                                 nrow=3, byrow=TRUE)
        oce.debug(debug, "transformation matrix:\n")
        oce.debug(debug, "  ", format(metadata$transformation.matrix[1,], width=10, digits=5, justify="right"), "\n")
        oce.debug(debug, "  ", format(metadata$transformation.matrix[2,], width=10, digits=5, justify="right"), "\n")
        oce.debug(debug, "  ", format(metadata$transformation.matrix[3,], width=10, digits=5, justify="right"), "\n")

        ## [158:161] float XmitRecDist
        ## [162:165] float CalCw
        ## FIXME why is this not 164 bytes in total?

        ##
        ## Analyze "deployment parameters" header

        if (deployment.parameters[1]!=0x12)
            stop("first byte of deployment-parameters header should be 0x12 but it is 0x", deployment.parameters[1])

        if (deployment.parameters[2]!=0x01)
            stop("first byte of deployment-parameters header should be 0x01 but it is 0x", deployment.parameters[2])

        metadata$velocity.range.index <- as.numeric(deployment.parameters[20])
        oce.debug(debug, "velocity.range.index=", metadata$velocity.range.index, "\n")
        if (metadata$velocity.range.index == 4)
            metadata$velocity.scale.factor <- 2 # range indices 1 through 3 have factor 1

        coordinate.system.code <- as.integer(deployment.parameters[22]) # 1 (0=beam 1=xyz 2=ENU)
        metadata$coordinate.system <- c("beam", "xyz", "enu")[1+coordinate.system.code]
        metadata$oce.coordinate <- metadata$coordinate.system
        oce.debug(debug, "coordinate.system=", metadata$coordinate.system, "\n")
        if (metadata$coordinate.system == "beam")
            stop("cannot handle beam coordinates")

        ## bug: docs say sampling rate in units of 0.1Hz, but the SLEIWEX-2008-m3 data file is in 0.01Hz

        sampling.rate <- 0.01*readBin(deployment.parameters[23:28], "integer", n=3, size=2, endian="little", signed=FALSE)
        if (sampling.rate[2] != 0 || sampling.rate[3] != 0)
            warning("ignoring non-zero items 2 and/or 3 of sampling.rate vector")
        metadata$sampling.rate <- sampling.rate[1]
        if (metadata$sampling.rate < 0)
            stop("sampling rate must be a positive integer, but got ", metadata$sampling.rate)
        metadata$measurement.deltat <- 1 / metadata$sampling.rate
        metadata$burst.interval <- readBin(deployment.parameters[29:34], "integer", n=3, size=2, endian="little", signed=FALSE)
        if (metadata$burst.interval[2] !=0 || metadata$burst.interval[3] != 0)
            warning("ignoring non-zero items 2 and/or 3 in burst.interval vector")
        metadata$burst.interval <- metadata$burst.interval[1]
        metadata$samples.per.burst <- readBin(deployment.parameters[35:40], "integer", n=3, size=2, endian="little", signed=FALSE)
        if (metadata$samples.per.burst[2] !=0 || metadata$samples.per.burst[3] != 0)
            warning("ignoring non-zero items 2 and/or 3 in samples.per.burst vector")
        metadata$samples.per.burst <- metadata$samples.per.burst[1]
        if (metadata$samples.per.burst < 0)
            stop("samples.per.burst must be a positive integer, but got ", metadata$samples.per.burst)
        metadata$deployment.name <- paste(integer2ascii(as.integer(deployment.parameters[49:57])), collapse="")
        metadata$comments1 <- paste(integer2ascii(as.integer(deployment.parameters[66:125])), collapse="")
        metadata$comments2 <- paste(integer2ascii(as.integer(deployment.parameters[126:185])), collapse="")
        metadata$comments3 <- paste(integer2ascii(as.integer(deployment.parameters[126:185])), collapse="")
    }                                   # if (header)

    ## Use 3-byte flag to find bursts in buf.  Then find their times, and # samples in each.
    ## Note: checking not just on the 2 "official" bytes, but also on third (3c=60=number of bytes in header)
    burst.bufindex <- match.bytes(buf, 0xA5, 0x11, 0x3c)

    oce.debug(debug, "burst.bufindex[1:10]=", paste(burst.bufindex[1:10], collapse=" "), "\n")

    nbursts <- length(burst.bufindex)
    metadata$number.of.bursts <- nbursts

    burst.bufindex2 <- sort(c(burst.bufindex, 1 + burst.bufindex))
    year <- readBin(buf[burst.bufindex2 + 18], "integer", n=nbursts, size=2, endian="little", signed=FALSE)
    day <- as.integer(buf[burst.bufindex+20])
    month <- as.integer(buf[burst.bufindex+21])
    minute <- as.integer(buf[burst.bufindex+22])
    hour <- as.integer(buf[burst.bufindex+23])
    sec100 <- as.integer(buf[burst.bufindex+24])
    sec <- as.integer(buf[burst.bufindex+25])
    burst.time <- as.POSIXct(ISOdatetime(year=year, month=month, day=day, hour=hour, min=minute, sec=sec+0.01*sec100, tz=tz))
    oce.debug(debug, "burst.time ranges", paste(range(burst.time), collapse=" to "), "\n")
    nbursts <- length(burst.time)
    samples.per.burst <- readBin(buf[burst.bufindex2 + 30], "integer", size=2, n=nbursts, endian="little", signed=FALSE)
    oce.debug(debug, "samples.per.burst[1:10]=", paste(samples.per.burst[1:10], collapse=" "), "\n")

    ## ".extended" refers to a burst sequence to which a final item has been appended,
    ## to allow the use of approx() for various things.
    burst.time.extended <- c(burst.time, burst.time[nbursts] + samples.per.burst[nbursts] / metadata$sampling.rate)
    attr(burst.time.extended, "tzone") <- attr(burst.time, "tzone")

    metadata$measurement.start <- min(burst.time.extended)
    metadata$measurement.end <- max(burst.time.extended)
    metadata$measurement.deltat <- (as.numeric(burst.time[length(burst.time)]) - as.numeric(burst.time[1])) / sum(samples.per.burst)

    oce.debug(debug, "burst.time.extended ranges", paste(range(burst.time.extended), collapse=" to "), "\n")

    ## Sample indices (not buf indices) of first sample in each burst
    burst.sampleindex.extended <- c(1, cumsum(samples.per.burst))
    burst.sampleindex <- burst.sampleindex.extended[-length(burst.sampleindex.extended)]
    oce.debug(debug, "burst.sampleindex[1:10]=", paste(burst.sampleindex[1:10], collapse=" "), "\n")

    ## Map from sample number to burst number
    burst <- 1:nbursts
    if (debug > 0)
        print(data.frame(burst, burst.time, burst.bufindex)[1:5,])

    ## Interpret 'from', 'to', and 'by', possibly integers, POSIX times, or strings for POSIX tiems
    from.keep <- from
    to.keep <- to
    if (inherits(from, "POSIXt")) {
        if (!inherits(to, "POSIXt"))
            stop("if 'from' is POSIXt, then 'to' must be, also")
        from.to.POSIX <- TRUE
        from.pair <- bisect.adv.sontek.adr(from, add=-1, debug=debug-1)
        from.burst <- from.pair$index
        oce.debug(debug, "from.keep=", format(from.keep), " yields burst.time[", from.burst, "]=", format(from.pair$t), "\n")
        to.pair <- bisect.adv.sontek.adr(to, add=1, debug=debug-1)
        to.burst <- to.pair$index
        oce.debug(debug, "to.keep=", format(to.keep), " yields burst.time[", to.burst, "]=", format(to.pair$t), "\n")
        ## burst offsets  FIXME: do we need these?
        from.burst.offset <- floor(0.5 + (as.numeric(from) - as.numeric(burst.time[from.burst])) * metadata$sampling.rate)
        to.burst.offset <- floor(0.5 + (as.numeric(to) - as.numeric(burst.time[to.burst-1])) * metadata$sampling.rate)
        oce.debug(debug, "from.burst.offset=", from.burst.offset, "to.burst.offset=",to.burst.offset,"\n")
        from.index <- 1
        to.index <- sum(samples.per.burst[from.burst:to.burst])
        oce.debug(debug, "from.index=", from.index, "to.index=", to.index, "\n")
    } else {
        from.to.POSIX <- FALSE
        from.index <- from
        to.index <- to
        ## Determine bursts, and offsets within bursts, for from.index and to.index
        tmp <- approx(burst.sampleindex, burst, from.index)$y
        if (is.na(tmp))
            stop("from index", from, " is not in this file")
        from.burst <- floor(tmp)
        from.burst.offset <- floor(0.5 + (tmp - from.burst)*samples.per.burst[from.burst])
        oce.debug(debug, "from is at index", from.index, "which is in burst", from.burst, ", at offset", from.burst.offset, "\n")
        tmp <- approx(burst.sampleindex, burst, to.index)$y
        if (is.na(tmp))
            stop("to index", from, " is not in this file")
        to.burst <- floor(tmp)
        to.burst.offset <- floor(0.5 + (tmp - to.burst)*samples.per.burst[to.burst])
        oce.debug(debug, "to is at index", to.index, "which is in burst", to.burst, ", at offset", to.burst.offset, "\n")
    }

    ## Set up focus region (not needed; just saves some subscripts later)
    focus <- unique(seq(from.burst, to.burst)) # collapse, e.g. if in same burst
    burst.focus <- burst[focus]
    oce.debug(debug, "burst.focus=", paste(burst.focus, collapse=" "), "\n")
    nbursts.focus <- length(burst.focus)
    burst.bufindex.focus <- burst.bufindex[focus]
    burst.time.focus <- burst.time[focus]
    samples.per.burst.focus <- samples.per.burst[focus]

    if (debug > 0)
        print(data.frame(burst.focus, burst.time.focus, burst.bufindex.focus, samples.per.burst.focus))

    ## set up to read everything in every relevant burst (trim later)
    oce.debug(debug, "sum(samplers.burst.focus)", sum(samples.per.burst.focus), "vs", nbursts * as.numeric(burst.time[2]-burst.time[1])*metadata$sampling.rate,"\n")

    ntotal <- sum(samples.per.burst.focus)
    oce.debug(debug, "ntotal=", ntotal, "\n")
    v <- array(numeric(), dim=c(ntotal, 3))
    time <- array(numeric(), dim=c(ntotal, 1))
    heading <- array(numeric(), dim=c(ntotal, 1))
    pitch <- array(numeric(), dim=c(ntotal, 1))
    roll <- array(numeric(), dim=c(ntotal, 1))
    temperature <- array(numeric(), dim=c(ntotal, 1))
    pressure <- array(numeric(), dim=c(ntotal, 1))
    a <- array(raw(), dim=c(ntotal, 3))
    c <- array(raw(), dim=c(ntotal, 3))
    row.offset <- 0

    oce.debug(debug, "data.length=", data.length, "\n")
    oce.debug(debug, "burst.header.length=",burst.header.length,"\n")

    oce.debug(debug, "burst.bufindex.focus:", paste(burst.bufindex.focus, collapse=" "), "\n")

    for (b in 1:nbursts.focus) {
        n <- samples.per.burst.focus[b]
        oce.debug(debug, "burst", b, "at", format(burst.time.focus[b]), "data start at byte", burst.bufindex.focus[b]+burst.header.length, "n=",n,"\n")
        buf.subset <- buf[burst.bufindex.focus[b]+burst.header.length+0:(-1+data.length*n)]
        m <- matrix(buf.subset, ncol=data.length, byrow=TRUE)
        if (n != dim(m)[1])
            stop("something is wrong with the data.  Perhaps the record length is not the assumed value of ", data.length)
        r <- row.offset + 1:n
        v[r,1] <- 1e-4 * readBin(t(m[,1:2]), "integer", n=n, size=2, signed=TRUE, endian="little")
        v[r,2] <- 1e-4 * readBin(t(m[,3:4]), "integer", n=n, size=2, signed=TRUE, endian="little")
        v[r,3] <- 1e-4 * readBin(t(m[,5:6]), "integer", n=n, size=2, signed=TRUE, endian="little")
        a[r,1] <- m[,7]
        a[r,2] <- m[,8]
        a[r,3] <- m[,9]
        c[r,1] <- m[,10]
        c[r,2] <- m[,11]
        c[r,3] <- m[,12]
        time[r] <- as.numeric(burst.time.focus[b]) + seq(0, n-1) / metadata$sampling.rate
        ##cat(sprintf("%.2f %.2f %.2f\n", time[r[1]], time[r[2]], time[r[3]]))
        ##cat("time=", format(time[r[1]]), ";", format(burst.time.focus[b]), "\n")
        ##print(range(time[r]))
        heading[r] <- 0.1 * readBin(as.raw(t(m[,13:14])), "integer", n=n, size=2, signed=TRUE, endian="little")
        pitch[r] <-   0.1 * readBin(as.raw(t(m[,15:16])), "integer", n=n, size=2, signed=TRUE, endian="little")
        roll[r] <-    0.1 * readBin(as.raw(t(m[,17:18])), "integer", n=n, size=2, signed=TRUE, endian="little")
        temperature[r] <- 0.01 * readBin(as.raw(t(m[,19:20])), "integer", n=n, size=2, signed=TRUE, endian="little")

        ## Pressure, using quadratic conversion from counts
        p.count <- readBin(as.raw(t(m[,21:22])), "integer", n=n, size=2, signed=FALSE, endian="little")
        pressure[r] <- metadata$pressure.offset + p.count * (metadata$pressure.scale + p.count * metadata$pressure.scale.2)

        row.offset <- row.offset + n
        if (monitor) {
            cat(".")
            if (!(b %% 50))
                cat(b, "\n")
        }
    }
    if (monitor)
        cat("\n")
    rm(buf, buf.subset, m)              # clean up, in case space is tight
    class(time) <- c("POSIXt", "POSIXct")
    attr(time, "tzone") <- attr(burst.time.focus[1], "tzone")
    oce.debug(debug, "burst.time[1]=", format(burst.time.focus[1]), "\n   times=", format(time[1:20]),"\n")
    ## Subset data to match the provided 'from', 'to' and 'by'
    if (from.to.POSIX) {
        iii <- from <= time & time <= to
        if (is.character(by)) {
            subsampling.rate <- floor(0.5 + ctime.to.seconds(by) * metadata$sampling.rate)
            oce.debug(debug, paste(" by = '", by, "' yields subsampling.rate=", subsampling.rate, "\n"), sep="")
            samples <- 1:length(iii)
            oce.debug(debug, "before interpreting 'by', iii true for", sum(iii), "cases\n")
            iii <- iii & !(samples %% subsampling.rate)
            oce.debug(debug, "after  interpreting 'by', iii true for", sum(iii), "cases\n")
            ##!(1:100)%%metadata$sampling.rate
            oce.debug(debug, "'by' is character, so subsampling by", floor(0.5 + ctime.to.seconds(by) * metadata$sampling.rate), "\n")
        }
    } else {
        indices <- seq(from.index, to.index) # FIXME: ignoring 'by'
        oce.debug(debug, "indices[1:10]=", paste(indices[1:10], collapse=" "), "\n")
        time <- approx(burst.sampleindex.extended, burst.time.extended - burst.time[1], indices)$y + burst.time[1]
        if (any(is.na(time)))
            warning("some times are NA; this is an internal coding error")
        focus.from <- from.burst.offset
        focus.to <- to.burst.offset + sum(samples.per.burst.focus[-length(samples.per.burst.focus)])
        oce.debug(debug, "focus.from=",focus.from, "focus.to=", focus.to,"\n")
        iii <- seq(focus.from, focus.to, by=by)
    }
    oce.debug(debug, "iii=", iii[1], iii[2], "...", iii[-1+length(iii)], iii[length(iii)], "\n")
    if (any(iii < 0))
        stop("got negative numbers in iii, which indicates a coding problem; range(iii)=",paste(range(iii), collapse=" to "))
    oce.debug(debug, "dim(v)=", paste(dim(v), collapse=" "),"\n")
    v <- v[iii,] * metadata$velocity.scale.factor
    a <- a[iii,]
    c <- c[iii,]
    time <- time[iii]
    pressure <- pressure[iii]
    temperature <- temperature[iii]
    pitch <- pitch[iii]
    heading <- heading[iii]
    roll <- roll[iii]
    data <- list(ts=list(time=time,
                         heading=heading,
                         pitch=pitch,
                         roll=roll,
                         temperature=temperature,
                         pressure=pressure),
                 ##ss=list(distance=0),
                 ma=list(v=v, a=a, c=c))
    if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processing.log.item(log.action)
    res <- list(data=data, metadata=metadata, processing.log=log.item)
    class(res) <- c("sontek", "adv", "oce")
    res
}

read.adv.sontek.text <- function(basefile, from=1, to, by=1, tz=getOption("oce.tz"),
                                 coordinate.system="xyz", transformation.matrix,
                                 latitude=NA, longitude=NA,
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
                 ##ss=list(distance=0),
                 ma=list(v=v[ok,],a=a[ok,],c=c[ok,]))
    metadata <- list(manufacturer="sontek",
                     instrument.type="adv", # FIXME or "adr"?
                     latitude=latitude, longitude=longitude,
                     cpu.software.ver.num=metadata$cpu.software.ver.num,
                     dsp.software.ver.num=metadata$dsp.software.ver.num,
                     filename=basefile,
                     transformation.matrix=if(!missing(transformation.matrix)) transformation.matrix else NULL,
                     number.of.samples=length(data$x),
                     number.of.beams=3,
                     orientation="upward", # FIXME: guessing on the orientation
                     deltat=as.numeric(difftime(tt[2], tt[1], units="secs")),
                     subsample.start=data$t[1],
                     oce.coordinate=coordinate.system,
                     coordinate.system=coordinate.system)
    warning("sensor orientation cannot be inferred without a header; \"", metadata$orientation, "\" was assumed.")
    if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processing.log.item(log.action)
    res <- list(data=data, metadata=metadata, processing.log=log.item)
    class(res) <- c("sontek", "adv", "oce")
    res
}

summary.adv <- function(object, ...)
{
    if (!inherits(object, "adv")) stop("method is only for adv objects")
    ts.names <- names(object$data$ts)
    nrow <- length(ts.names) - 1          # the -1 is for 'time'
    have.ts.slow <- "ts.slow" %in% names(object$data)
    if (have.ts.slow) {
        ts.slow.names <- names(object$data$ts.slow)
        nrow <- nrow + length(ts.slow.names) - 1 # the -1 is for 'time'
    }
    ma.names <- names(object$data$ma)
    nrow <- nrow + length(ma.names)
    fives <- matrix(nrow=nrow, ncol=5)
    ii <- 1
    for (name in ts.names) {
        if (name != "time") {
            fives[ii,] <- fivenum(as.numeric(object$data$ts[[name]]), na.rm=TRUE)
            ii <- ii + 1
        }
    }
    if (have.ts.slow) {
        for (name in ts.slow.names) {
            if (name != "time") {
                fives[ii,] <- fivenum(as.numeric(object$data$ts.slow[[name]]), na.rm=TRUE)
                ii <- ii + 1
            }
        }
    }
    for (name in ma.names) {
        fives[ii,] <- fivenum(as.numeric(object$data$ma[[name]]), na.rm=TRUE)
        ii <- ii + 1
    }
    if (have.ts.slow)
        rownames(fives) <- c(ts.names[ts.names != "time"], ts.slow.names[ts.slow.names != "time"], ma.names)
    else
        rownames(fives) <- c(ts.names[ts.names != "time"], ma.names)
    colnames(fives) <- c("Min.", "1st Qu.", "Median", "3rd Qu.", "Max.")
    res <- list(filename=object$metadata$filename,
                number.of.beams=if (!is.null(object$metadata$number.of.beams)) object$metadata$number.of.beams else 3,
                latitude=object$metadata$latitude,
                longitude=object$metadata$longitude,
                orientation=object$metadata$orientation,
                velocity.range.index=object$metadata$velocity.range.index,
                transformation.matrix=object$metadata$transformation.matrix,
                sampling.rate=object$metadata$sampling.rate,
                measurement.start=object$metadata$measurement.start,
                measurement.end=object$metadata$measurement.end,
                measurement.deltat=object$metadata$measurement.deltat,
                subsample.start=min(object$data$ts$time, na.rm=TRUE),
                subsample.end=max(object$data$ts$time, na.rm=TRUE),
                subsample.deltat=mean(diff(as.numeric(object$data$ts$time)),na.rm=TRUE),
                instrument.type=object$metadata$instrument.type,
                serial.number=object$metadata$serial.number,
                number.of.samples=length(object$data$ts$time),
                coordinate.system=object$metadata$coordinate.system,
                oce.coordinate=object$metadata$oce.coordinate,
                fives=fives,
                processing.log=processing.log.summary(object))
    if (inherits(object, "nortek")) {
        res$software.version <- object$metadata$software.version
        res$internal.code.version <- object$metadata$internal.code.version
        res$revision.number <- object$metadata$hardware.revision
        res$burst.length <- object$metadata$burst.length
        res$deploy.name <- object$metadata$deploy.name
        res$comments <- object$metadata$comments
        res$head.frequency <- object$metadata$frequency
    } else if (inherits(object, "sontek")) {
        res$cpu.software.ver.num <- object$metadata$cpu.software.ver.num
        res$dsp.software.ver.num <- object$metadata$dsp.software.ver.num
        res$samples.per.burst <- object$metadata$samples.per.burst
    }
    class(res) <- "summary.adv"
    res
}

print.summary.adv <- function(x, digits=max(5, getOption("digits") - 1), ...)
{
    cat("ADV Summary\n-----------\n\n", ...)
    cat(paste("* Instrument:             ", x$instrument.type, ", serial number ``", x$serial.number, "``\n",sep=""))
    cat(paste("* Source filename:        ``", x$filename, "``\n", sep=""))
    if ("latitude" %in% names(x)) {
        cat(paste("* Location:              ", if (is.na(x$latitude)) "unknown latitude" else sprintf("%.5f N", x$latitude), ", ",
                  if (is.na(x$longitude)) "unknown longitude" else sprintf("%.5f E", x$longitude), "\n"))
    }
    cat(sprintf("* Measurements:           %s %s to %s %s sampled at %.4g Hz (on average)\n",
                format(x$measurement.start), attr(x$measurement.start, "tzone"),
                format(x$measurement.end), attr(x$measurement.end, "tzone"),
                1 / x$measurement.deltat), ...)
    cat(sprintf("* Subsample:              %s %s to %s %s sampled at %.4g Hz (on average)\n",
                format(x$subsample.start), attr(x$subsample.start, "tzone"),
                format(x$subsample.end),  attr(x$subsample.end, "tzone"),
                1 / x$subsample.deltat), ...)
    ## cat("  Beam angle:           ", x$metadata$beam.angle, "\n")
    cat("* Number of samples:     ", x$number.of.samples, "\n")
    cat("* Coordinate system:     ", x$coordinate.system, "[originally],", x$oce.coordinate, "[presently]\n")
    cat("* Orientation:           ", x$orientation, "\n")
    if (x$instrument.type == "vector") {
        cat("\n* Nortek vector specific\n\n")
        cat("  * Internal code version:  ", x$internal.code.version, "\n")
        cat("  * Revision number:        ", x$revision.number, "\n")
        cat("  * Software version:       ", x$software.version, "\n")
        cat("  * Head frequency:         ", x$head.frequency, "kHz\n")
        ## FIXME: put other info here, e.g. software version, sampling volume, etc.; the manufacturer file is a good guide
        cat("  * Samples per burst:      ", x$burst.length, "\n") # FIXME: use same names throughout
        cat("  * Deploy name:            ", x$deploy.name, "\n")
        cat("  * Comments:               ", x$comments, "\n")
    } else if (x$instrument.type == "sontek adr") {
        cat("\n* Sontek adr specific\n\n")
        cat("  * CPU software version:  ", x$cpu.software.ver.num, "\n")
        cat("  * DSP software version:  ", x$dsp.software.ver.num, "\n")
        cat("  * Samples per burst:     ", x$samples.per.burst, "\n")
        cat("  * Velocity range index:  ", x$velocity.range.index, "\n")
    }
    if (!is.null(x$transformation.matrix)) {
        cat("\n* Transformation matrix\n  ::\n\n")
        cat("  ", format(x$transformation.matrix[1,], width=digits+4, digits=digits, justify="right"), "\n")
        cat("  ", format(x$transformation.matrix[2,], width=digits+4, digits=digits, justify="right"), "\n")
        cat("  ", format(x$transformation.matrix[3,], width=digits+4, digits=digits, justify="right"), "\n")
        if (x$number.of.beams > 3)
            cat("  ", format(x$transformation.matrix[4,], width=digits+4, digits=digits, justify="right"), "\n")
    }
    cat("\n",...)
    cat("* Statistics of subsample\n  ::\n\n", ...)
    cat(show.fives(x, indent='     '), ...)
    ##cat("\n* Processing log\n\n", ...)
    cat("\n")
    print(x$processing.log, ...)
    invisible(x)
}

plot.adv <- function(x,
                     which=c(1:3,14,15),
                     titles,
                     type="l",
                     adorn=NULL,
                     draw.time.range=getOption("oce.draw.time.range"),
                     draw.zero.line=FALSE,
                     mgp=getOption("oce.mgp"),
                     mar=c(mgp[1]+1.5,mgp[1]+1.5,1.5,1.5),
                     margins.as.image=FALSE,
                     cex=par("cex"), cex.axis=par("cex.axis"), cex.main=par("cex.main"),
                     xlim, ylim,
                     brush.correlation, col.brush="red",
                     debug=getOption("oce.debug"),
                     ...)
{
    debug <- round(debug)
    if (debug < 0) debug <- 0
    if (debug > 4) debug <- 4
    oce.debug(debug, "\bplot.adv(x, which=c(", paste(which,collapse=","),"), type=\"", type, "\", ...) {\n", sep="")
    have.brush.correlation <- !missing(brush.correlation)
    oce.debug(debug, "brush.correlation", if (have.brush.correlation) brush.correlation else "not given", "\n")
    oce.debug(debug, "cex=",cex," cex.axis=", cex.axis, " cex.main=", cex.main, "\n")
    oce.debug(debug, "mar=c(",paste(mar, collapse=","), ")\n")
    if (!inherits(x, "adv")) stop("method is only for adv objects")
    dots <- names(list(...))
                                        #if (!all(which %in% c(1:3,5:7,9:11,14:21,23))) stop("\"which\" must be in the range c(1:3,5:7,9:11,14:21,23) but it is ", which)
    opar <- par(no.readonly = TRUE)
    lw <- length(which)

    if (!missing(titles) && length(titles) != lw) stop("length of 'titles' must equal length of 'which'")
    if (lw > 1)
        on.exit(par(opar))
    par(mgp=mgp, mar=mar)
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
            if (length(xlim) != 2)
                stop("xlim must be a vector of length 2, or a 2-column matrix")
            xlim2 <- matrix(xlim[1:2], ncol=2, nrow=lw, byrow=TRUE)
        }
        xlim <- xlim2
    }
    adorn.length <- length(adorn)
    if (adorn.length == 1) {
        adorn <- rep(adorn, lw)
        adorn.length <- lw
    }
    oce.debug(debug, "before layout, cex=", par('cex'), "\n")
    if (lw > 1) {
        if (margins.as.image) {
            w <- 1.5
            lay <- layout(matrix(1:(2*lw), nrow=lw, byrow=TRUE), widths=rep(c(1, lcm(w)), lw))
        } else {
            lay <- layout(cbind(1:lw))
        }
    }
    ## Translate word-style (FIXME: ugly coding)
    lw <- length(which)
    oce.debug(debug, "before nickname-substitution, which=c(", paste(which, collapse=","), ")\n")
    which2 <- vector("numeric", lw)
    for (w in 1:lw) {
        ww <- which[w]
        if (is.numeric(ww)) {
            which2[w] <- ww
        } else {
            if (     ww == "u1") which2[w] <- 1
            else if (ww == "u2") which2[w] <- 2
            else if (ww == "u3") which2[w] <- 3
            ## 4 not allowed since ADV is 3-beam
            else if (ww == "a1") which2[w] <- 5
            else if (ww == "a2") which2[w] <- 6
            else if (ww == "a3") which2[w] <- 7
            ## 4 not allowed since ADV is 3-beam
            else if (ww == "q1") which2[w] <- 9
            else if (ww == "q2") which2[w] <- 10
            else if (ww == "q3") which2[w] <- 11
            ## 4 not allowed since ADV is 3-beam
            ## 13 not allowed since ADV do not measure salinity
            else if (ww == "temperature") which2[w] <- 14
            else if (ww == "pressure") which2[w] <- 15
            else if (ww == "heading") which2[w] <- 16
            else if (ww == "pitch") which2[w] <- 17
            else if (ww == "roll") which2[w] <- 18
            ## 19 beam-1 correlation-amplitude diagnostic plot
            ## 20 beam-2 correlation-amplitude diagnostic plot
            ## 21 beam-3 correlation-amplitude diagnostic plot
            ## 22 not allowed, since ADVs have only 3 beams
            else if (ww == "progressive vector") which2[w] <- 23
            else if (ww == "uv") which2[w] <- 28
            else if (ww == "uv+ellipse") which2[w] <- 29
            else if (ww == "uv+ellipse+arrow") which2[w] <- 30
            else stop("unknown 'which':", ww)
        }
    }
    which <- which2
    oce.debug(debug, "after nickname-substitution, which=c(", paste(which, collapse=","), ")\n")

    oce.debug(debug, "after layout, cex=", par('cex'), "\n")
    have.ts.slow <- "ts.slow" %in% names(x$data)
    tlim <- range(x$data$ts$time, na.rm=TRUE)
    if (have.ts.slow)
        tslim <- range(x$data$ts.slow$time, na.rm=TRUE)
    for (w in 1:lw) {
        oce.debug(debug, "plotting which[", w, "]=", which[w], "\n")
        par(mgp=mgp, mar=mar)
        if (which[w] %in% 1:3) {        # u1, u2, u3
            y <- as.numeric(x$data$ma$v[,which[w]])
            if (have.brush.correlation && type == "p") {
                good <- as.numeric(x$data$ma$c[,which[w]]) >= brush.correlation
                oce.plot.ts(x$data$ts$time[good], y[good], ylab=ad.beam.name(x, which[w]),
                            draw.time.range=draw.time.range,
                            adorn=adorn[w],
                            xlim=if (gave.xlim) xlim[w,] else tlim,
                            ylim=if (gave.ylim) ylim[w,] else range(y, na.rm=TRUE),
                            type=type,
                            cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                            mgp=mgp,
                            mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                            debug=debug-1,
                            ...)
                points(x$data$ts$time[!good], x$data$ma$v[!good,which[w]], col=col.brush)
            } else {
                oce.plot.ts(x$data$ts$time, y, ylab=ad.beam.name(x, which[w]),
                            draw.time.range=draw.time.range,
                            adorn=adorn[w],
                            xlim=if (gave.xlim) xlim[w,] else tlim,
                            ylim=if (gave.ylim) ylim[w,] else range(y, na.rm=TRUE),
                            type=type,
                            cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                            mgp=mgp,
                            mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                            debug=debug-1,
                            ...)
            }
            if (draw.zero.line)
                abline(h=0)
            rm(y)                       # space may be tight
        } else if (which[w] %in% 5:7) { # a1, a2, a3
            ## FIXME/DRY: alter a1,a2,a3 if alter q1,q2,q3, since both almost the same
            oce.debug(debug, "plotting a1, a2, or a3 since which[w] == ", which[w], "\n")
            y <- as.numeric(x$data$ma$a[,which[w]-4])
            oce.debug("range(y):", range(y, na.rm=TRUE), "\n")
            if (have.brush.correlation && type == "p") {
                good <- as.numeric(x$data$ma$c[,which[w]-4]) >= brush.correlation
                oce.plot.ts(x$data$ts$time[good], y[good], ylab=expression(a[which[w]-4]),
                            draw.time.range=draw.time.range,
                            adorn=adorn[w],
                            xlim=if (gave.xlim) xlim[w,] else tlim,
                            ylim=if (gave.ylim) ylim[w,] else range(y, na.rm=TRUE),
                            type=type,
                            cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                            mgp=mgp,
                            mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                            debug=debug-1,
                            ...)
                points(x$data$ts$time[!good], y[!good], col=col.brush)
            } else {
                oce.plot.ts(x$data$ts$time, y, ylab=expression(a[which[w]-4]),
                            draw.time.range=draw.time.range,
                            adorn=adorn[w],
                            xlim=if (gave.xlim) xlim[w,] else tlim,
                            ylim=if (gave.ylim) ylim[w,] else range(y, na.rm=TRUE),
                            type=type,
                            cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                            mgp=mgp,
                            mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                            debug=debug-1,
                            ...)
            }
            rm(y)                       # space may be tight
        } else if (which[w] %in% 9:11) { # q1, q2, q3 (named c1, c2, and c3 in the object)
            y <- as.numeric(x$data$ma$c[,which[w]-8])
            if (have.brush.correlation && type == "p") {
                good <- as.numeric(x$data$ma$c[,which[w]-8]) >= brush.correlation
                oce.plot.ts(x$data$ts$time[good], y[good], ylab=expression(q[which[w-8]]),
                            draw.time.range=draw.time.range,
                            adorn=adorn[w],
                            xlim=if (gave.xlim) xlim[w,] else tlim,
                            ylim=if (gave.ylim) ylim[w,] else range(y, na.rm=TRUE),
                            type=type,
                            cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                            mgp=mgp,
                            mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                            debug=debug-1,
                            ...)
                points(x$data$ts$time[!good], y[!good], col=col.brush)
            } else {
                oce.plot.ts(x$data$ts$time, y, ylab=expression(q[which[w-8]]),
                            draw.time.range=draw.time.range,
                            adorn=adorn[w],
                            xlim=if (gave.xlim) xlim[w,] else tlim,
                            ylim=if (gave.ylim) ylim[w,] else range(y, na.rm=TRUE),
                            type=type,
                            cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                            mgp=mgp,
                            mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                            debug=debug-1,
                            ...)
            }
            rm(y)                       # space may be tight
        } else if (which[w] == 14 || which[w] == "temperature") {
            if (have.ts.slow) {
                oce.plot.ts(x$data$ts.slow$time, x$data$ts.slow$temperature, ylab=resizable.label("T", "y"),
                            draw.time.range=draw.time.range,
                            adorn=adorn[w],
                            xlim=if (gave.xlim) xlim[w,] else tslim,
                            ylim=if (gave.ylim) ylim[w,] else range(x$data$ts.slow$temperature, na.rm=TRUE),
                            type=type,
                            cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                            mgp=mgp,
                            mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                            debug=debug-1,
                            ...)
            } else {
                oce.plot.ts(x$data$ts$time, x$data$ts$temperature, ylab=resizable.label("T", "y"),
                            draw.time.range=draw.time.range,
                            adorn=adorn[w],
                            xlim=if (gave.xlim) xlim[w,] else tlim,
                            ylim=if (gave.ylim) ylim[w,] else range(x$data$ts$temperature, na.rm=TRUE),
                            type=type,
                            cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                            mgp=mgp,
                            mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                            debug=debug-1,
                            ...)
            }
        } else if (which[w] == 15 || which[w] == "pressure") {
            oce.plot.ts(x$data$ts$time, x$data$ts$pressure, ylab=resizable.label("p", "y"),
                        draw.time.range=draw.time.range,
                        adorn=adorn[w],
                        xlim=if (gave.xlim) xlim[w,] else tlim,
                        ylim=if (gave.ylim) ylim[w,] else range(x$data$ts$pressure, na.rm=TRUE),
                        type=type,
                        cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                        mgp=mgp,
                        mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                        debug=debug-1,
                        ...)
        } else if (which[w] == 16 || which[w] == "heading") {
            if (have.ts.slow) {
                oce.plot.ts(x$data$ts.slow$time, x$data$ts.slow$heading, ylab="heading",
                            draw.time.range=draw.time.range,
                            adorn=adorn[w],
                            xlim=if (gave.xlim) xlim[w,] else tslim,
                            ylim=if (gave.ylim) ylim[w,] else range(x$data$ts.slow$heading, na.rm=TRUE),
                            type=type,
                            cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                            mgp=mgp,
                            mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                            debug=debug-1,
                            ...)
            } else {
                oce.plot.ts(x$data$ts$time, x$data$ts$heading, ylab="heading",
                            draw.time.range=draw.time.range,
                            adorn=adorn[w],
                            xlim=if (gave.xlim) xlim[w,] else tlim,
                            ylim=if (gave.ylim) ylim[w,] else range(x$data$ts$heading, na.rm=TRUE),
                            type=type,
                            cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                            mgp=mgp,
                            mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                            debug=debug-1,
                            ...)
            }
        } else if (which[w] == 17 || which[w] == "pitch") {    # pitch
            if (have.ts.slow) {
                oce.plot.ts(x$data$ts.slow$time, x$data$ts.slow$pitch, ylab="pitch",
                            draw.time.range=draw.time.range,
                            adorn=adorn[w],
                            xlim=if (gave.xlim) xlim[w,] else tslim,
                            ylim=if (gave.ylim) ylim[w,] else range(x$data$ts.slow$pitch, na.rm=TRUE),
                            type=type,
                            cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                            mgp=mgp,
                            mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                            debug=debug-1,
                            ...)
            } else {
                oce.plot.ts(x$data$ts$time, x$data$ts$pitch, ylab="pitch",
                            draw.time.range=draw.time.range,
                            adorn=adorn[w],
                            xlim=if (gave.xlim) xlim[w,] else tlim,
                            ylim=if (gave.ylim) ylim[w,] else range(x$data$ts$pitch, na.rm=TRUE),
                            cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                            mgp=mgp,
                            mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                            debug=debug-1,
                            ...)
            }
        } else if (which[w] == 18 || which[w] == "roll") {
            if (have.ts.slow) {
                oce.plot.ts(x$data$ts.slow$time, x$data$ts.slow$roll, ylab="roll",
                            draw.time.range=draw.time.range,
                            adorn=adorn[w],
                            xlim=if (gave.xlim) xlim[w,] else tslim,
                            ylim=if (gave.ylim) ylim[w,] else range(x$data$ts.slow$roll, na.rm=TRUE),
                            type=type,
                            cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                            mgp=mgp,
                            mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                            debug=debug-1,
                            ...)
            } else {
                oce.plot.ts(x$data$ts$time, x$data$ts$roll, ylab="roll",
                            draw.time.range=draw.time.range,
                            adorn=adorn[w],
                            xlim=if (gave.xlim) xlim[w,] else tlim,
                            ylim=if (gave.ylim) ylim[w,] else range(x$data$ts$roll, na.rm=TRUE),
                            type=type,
                            cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                            mgp=mgp,
                            mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                            debug=debug-1,
                            ...)
            }
            ## FIXME: should plot.adv() be passing mar, cex, etc to smoothScatter?
        } else if (which[w] == 19) {    # beam 1 correlation-amplitude diagnostic plot
            a <- as.numeric(x$data$ma$a[,1])
            c <- as.numeric(x$data$ma$c[,1])
            smoothScatter(a, c, nbin=64, xlab="Amplitude", ylab="Correlation",
                          xlim=if (gave.xlim) xlim[w,] else range(a),
                          ylim=if (gave.ylim) ylim[w,] else range(c))
            mtext("beam 1")
        } else if (which[w] == 20) {    # beam 2 correlation-amplitude diagnostic plot
            a <- as.numeric(x$data$ma$a[,2])
            c <- as.numeric(x$data$ma$c[,2])
            smoothScatter(a, c, nbin=64, xlab="Amplitude", ylab="Correlation",
                          xlim=if (gave.xlim) xlim[w,] else range(a),
                          ylim=if (gave.ylim) ylim[w,] else range(c))
            mtext("beam 2")
        } else if (which[w] == 21) {    # beam 3 correlation-amplitude diagnostic plot
            a <- as.numeric(x$data$ma$a[,3])
            c <- as.numeric(x$data$ma$c[,3])
            smoothScatter(a, c, nbin=64, xlab="Amplitude", ylab="Correlation",
                          xlim=if (gave.xlim) xlim[w,] else range(a),
                          ylim=if (gave.ylim) ylim[w,] else range(c))
            mtext("beam 3")
        } else if (which[w] == 23 || which[w] == "progressive vector") {    # progressive vector
            par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
            dt <- diff(as.numeric(x$data$ts$time))
            dt <- c(dt[1], dt)    # make right length by copying first
            dt <- mean(dt, na.rm=TRUE)
            m.per.km <- 1000
            u <- x$data$ma$v[,1]
            v <- x$data$ma$v[,2]
            u[is.na(u)] <- 0        # zero out missing
            v[is.na(v)] <- 0
            x.dist <- cumsum(u) * dt / m.per.km
            y.dist <- cumsum(v) * dt / m.per.km
            plot(x.dist, y.dist, xlab="km", ylab="km", type=type,
                 cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                 asp=1, ...)
        } else if (which[w] >= 28) {
            oce.debug(debug, "doing horizontal-velocity diagram\n")
            par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
            n <- length(x$data$ts$time)
            if (n < 2000) {
                plot(x$data$ma$v[,1], x$data$ma$v[,2], xlab="u [m/s]", ylab="v [m/s]", type=type,
                     cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                     asp=1, xlim=xlim, ylim=ylim, ...)
            } else {
                smoothScatter(x$data$ma$v[,1], x$data$ma$v[,2], xlab="u [m/s]", ylab="v [m/s]",
                              cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                              asp=1, xlim=xlim, ylim=ylim, ...)
            }
            if (which[w] >= 29) {
                ok <- !is.na(x$data$ma$v[,1]) & !is.na(x$data$ma$v[,2])
                e <- eigen(cov(data.frame(u=x$data$ma$v[ok,1], v=x$data$ma$v[ok,2])))
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
                if (which[w] >= 30) {
                    umean <- mean(x$data$ma$v[,1], na.rm=TRUE)
                    vmean <- mean(x$data$ma$v[,2], na.rm=TRUE)
                    arrows(0, 0, umean, vmean, lwd=5, length=1/10, col="yellow")
                    arrows(0, 0, umean, vmean, lwd=2, length=1/10, col=col)
                }
            }
        } else {
            stop("unknown value of \"which\":", which[w])
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
    oce.debug(debug, "\b\b} # plot.adv()\n")
    invisible()
}

adv.2enu <- function(x, declination=0, debug=getOption("oce.debug"))
{
    oce.debug(debug, "\b\badv.2enu() {\n")
    coord <- x$metadata$oce.coordinate
    if (coord == "beam") {
        x <- adv.xyz2enu(adv.beam2xyz(x, debug=debug-1), declination=declination, debug=debug-1)
    } else if (coord == "xyz") {
        x <- adv.xyz2enu(x, declination=declination, debug=debug-1)
    } else if (coord == "enu") {
        ;
    } else {
        warning("adv.2enu cannot convert from coordinate system ", coord, " to ENU, so returning argument as-is")
    }
    oce.debug(debug, "\b\b} # adv.2enu()\n")
    x
}

adv.beam2xyz <- function(x, debug=getOption("oce.debug"))
{
    oce.debug(debug, "\b\badv.beam2xyz() {\n")
    if (!inherits(x, "adv"))
        stop("method is only for objects of class \"adv\"")
    if (x$metadata$oce.coordinate != "beam")
        stop("input must be in beam coordinates, but it is in ", x$metadata$oce.coordinate, " coordinates")
    if (is.null(x$metadata$transformation.matrix))
        stop("can't convert coordinates because object metadata$transformation.matrix is NULL")
    tm <- x$metadata$transformation.matrix
    ## alter transformation matrix if pointing downward. FIXME: is this right?
    ##if (FALSE) {  # FIXME: should we modify the transformation matrix?
    ##    if (x$metadata$orientation == "downward") {
    ##        tm[2,] <- -tm[2,]
    ##        tm[3,] <- -tm[3,]
    ##    }
    ##}
    if (x$metadata$orientation == "downward")
        warning("Q: since the instrument points downwards, should the sign of rows 2 and 3 of transformation matrix be altered?")
    oce.debug(debug, "Transformation matrix:\n")
    oce.debug(debug, sprintf("%.10f %.10f %.10f\n", tm[1,1], tm[1,2], tm[1,3]))
    oce.debug(debug, sprintf("%.10f %.10f %.10f\n", tm[2,1], tm[2,2], tm[2,3]))
    oce.debug(debug, sprintf("%.10f %.10f %.10f\n", tm[3,1], tm[3,2], tm[3,3]))
    ## Not using the matrix method because it might consume more memory, and measures no faster
    ## xyz <- tm %*% rbind(x$data$ma$v[,1], x$data$ma$v[,2], x$data$ma$v[,3])
    u <- tm[1,1] * x$data$ma$v[,1] + tm[1,2] * x$data$ma$v[,2] + tm[1,3] * x$data$ma$v[,3]
    v <- tm[2,1] * x$data$ma$v[,1] + tm[2,2] * x$data$ma$v[,2] + tm[2,3] * x$data$ma$v[,3]
    w <- tm[3,1] * x$data$ma$v[,1] + tm[3,2] * x$data$ma$v[,2] + tm[3,3] * x$data$ma$v[,3]
    x$data$ma$v[,1] <- u
    x$data$ma$v[,2] <- v
    x$data$ma$v[,3] <- w
    x$metadata$oce.coordinate <- "xyz"
    x$processing.log <- processing.log.add(x$processing.log,
                                           paste(deparse(match.call()), sep="", collapse=""))
    oce.debug(debug, "\b\b} # adv.beam2xyz()\n")
    x
}

adv.xyz2enu <- function(x, declination=0, debug=getOption("oce.debug"))
{
    oce.debug(debug, "\b\badv.xyz2enu(x, declination=", declination, ",debug) {\n")
    if (!inherits(x, "adv")) stop("method is only for objects of class \"adv\"")
    if (x$metadata$oce.coordinate != "xyz") stop("input must be in xyz coordinates, but it is in ", x$metadata$oce.coordinate, " coordinates")
    have.ts.slow <- "ts.slow" %in% names(x$data)
    have.steady.angles <- (have.ts.slow && length(x$data$ts.slow$heading) == 1 && length(x$data$ts.slow$pitch) == 1 && length(x$data$ts.slow$roll) == 1) || (!have.ts.slow && length(x$data$ts$heading) == 1 && length(x$data$ts$pitch) == 1 && length(x$data$ts$roll) == 1)
    oce.debug(debug, "have.steady.angles=",have.steady.angles,"\n")
    if (have.ts.slow) {
        oce.debug(debug, "adv data has data$ts.slow\n")
        if (have.steady.angles) {
            oce.debug(debug, "adv data has constant heading, pitch, and roll\n")
            heading <- x$data$ts.slow$heading
            pitch <- x$data$ts.slow$pitch
            roll <- x$data$ts.slow$roll
        } else {
            oce.debug(debug, "adv data has time-varying heading, pitch, and roll\n")
            t0 <- as.numeric(x$data$ts.slow$time[1])    # arbitrary; done in case approx hates large x values
            t.fast <- as.numeric(x$data$ts$time) - t0
            t.slow <- as.numeric(x$data$ts.slow$time) - t0
            heading <- approx(t.slow, x$data$ts.slow$heading, xout=t.fast)$y
            pitch <- approx(t.slow, x$data$ts.slow$pitch, xout=t.fast)$y
            roll <- approx(t.slow, x$data$ts.slow$roll, xout=t.fast)$y
        }
    } else {
        oce.debug(debug, "adv data does not have data$ts.slow; time-series data are data$ts\n")
        heading <- x$data$ts$heading
        pitch <- x$data$ts$pitch
        roll <- x$data$ts$roll
    }
    heading <- heading + declination
    ##print(x$metadata)
    if (1 == length(agrep("nortek", x$metadata$manufacturer)) ||
        1 == length(agrep("sontek", x$metadata$manufacturer))) {
        ## Adjust the heading, so that the formulae (based on RDI) will work here
        heading <- heading - 90
        pitch <- - pitch
        warning("since nortek-adv or sontek-adv, changed sign of pitch and subtracted 90 from heading")
    }
    oce.debug(debug, vector.show(heading, "heading"))
    oce.debug(debug, vector.show(pitch, "pitch"))
    oce.debug(debug, vector.show(roll, "roll"))
    to.radians <- atan2(1,1) / 45
    hrad <- to.radians * heading        # This could save millions of multiplies
    prad <- to.radians * pitch          # although the trig is probably taking
    rrad <- to.radians * roll           # most of the time.
    CH <- cos(hrad)
    SH <- sin(hrad)
    CP <- cos(prad)
    SP <- sin(prad)
    CR <- cos(rrad)
    SR <- sin(rrad)
    if (x$metadata$orientation == "downward") { #FIXME: I think this is plain wrong; should change sign of row 2 and 3 (??)
        warning("adv.xyz2enu() switching signs of pitch and roll, because unit is oriented downward. BUT IS THIS CORRECT??")
        SP <- -SP
        SR <- -SR
    }
    np <- dim(x$data$ma$v)[1]
    if (have.steady.angles) {
        oce.debug(debug, "the heading, pitch, and roll are all constant\n")
        R <- array(numeric(), dim=c(3, 3))
        R[1,1] <-  CH * CR + SH * SP * SR
        R[1,2] <-  SH * CP
        R[1,3] <-  CH * SR - SH * SP * CR
        R[2,1] <- -SH * CR + CH * SP * SR
        R[2,2] <-  CH * CP
        R[2,3] <- -SH * SR - CH * SP * CR
        R[3,1] <- -CP * SR
        R[3,2] <-  SP
        R[3,3] <-  CP * CR
        u <- R[1,1] * x$data$ma$v[,1] + R[1,2] * x$data$ma$v[,2] + R[1,3] * x$data$ma$v[,3]
        v <- R[2,1] * x$data$ma$v[,1] + R[2,2] * x$data$ma$v[,2] + R[2,3] * x$data$ma$v[,3]
        w <- R[3,1] * x$data$ma$v[,1] + R[3,2] * x$data$ma$v[,2] + R[3,3] * x$data$ma$v[,3]
        x$data$ma$v[,1] <- u
        x$data$ma$v[,2] <- v
        x$data$ma$v[,3] <- w
        ##(speed test; replace above 3 lines with this) x$data$ma$v <- t(R %*% t(x$data$ma$v))
    } else {
        ## as with corresponding adp routine, construct single 3*3*np matrix
        oce.debug(debug, "the heading, pitch, and roll vary with time\n")
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
        ##rm(hrad,prad,rrad,CH,SH,CP,SP,CR,SR) # might be tight on space (but does this waste time?)
        rotated <- matrix(unlist(lapply(1:np, function(p) tr.mat[,,p] %*% x$data$ma$v[p,])), nrow=3)
        x$data$ma$v[,1] <- rotated[1,]
        x$data$ma$v[,2] <- rotated[2,]
        x$data$ma$v[,3] <- rotated[3,]
    }
    x$metadata$oce.coordinate <- "enu"
    x$processing.log <- processing.log.add(x$processing.log,
                                           paste(deparse(match.call()), sep="", collapse=""))
    oce.debug(debug, "\b\b} # adv.xyz2enu()\n")
    x
}

adv.enu2other <- function(x, heading=0, pitch=0, roll=0)
{
    oce.debug(debug, "\b\badv.enu2other() {\n")
    if (!inherits(x, "adv")) stop("method is only for objects of class \"adv\"")
    if (x$metadata$oce.coordinate != "enu") stop("input must be in \"enu\" coordinates, but it is in ", x$metadata$oce.coordinate, " coordinates")
    to.radians <- atan2(1,1) / 45
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
    x$data$ma$v[,1] <- other[1,]
    x$data$ma$v[,2] <- other[2,]
    x$data$ma$v[,3] <- other[3,]
    x$metadata$oce.coordinate <- "other"
    x$processing.log <- processing.log.add(x$processing.log,
                                           paste(deparse(match.call()), sep="", collapse=""))
    oce.debug(debug, "\b\b} # adv.enu2other()\n")
    x
}
