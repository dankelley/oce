### AQUADOPP
## notes for nortek:
## 1. "spare" at offset 74 (page 31) now seems to be salinity
## 2. extra byte
## 3. should state the order of headers at the start, not end
## 4. should state the algorithms to infer cell size, blanking distance, etc. from file
## 5. beam angle should be in data file
## 6. generally, docs should indicate everything that is in the files, e.g. (prominently!)
##    the beam angles in the 'head' configuration section.
## 7. the C code suggests the velocity scale is in the second bit of conf.hMode
##    but the docs suggest the fifth bit (page 31)

read.profile.aquadopp <- function(file, debug=getOption("oce.debug"))
{
    sync.code <- as.raw(0xa5)
    id.high.resolution.aquadopp.profile.data <- as.raw(0x2a) # page 38 of System Integrator Guide
    start <- readBin(file, "raw", 54) # see page 38 of System Integrator Guide (was 54 until 2009-07-01)
    oce.debug(debug, "first 4 bytes of AquaDopp profile data:", paste(start[1:4], collapse=" "), "\n")
    time <- sontek.time(start[5:12])
    oce.debug(debug, "  time=", format(time), "\n")
    sound.speed <-  readBin(start[17:18], "integer", n=1, size=2, endian="little", signed=FALSE) * 0.1
    oce.debug(debug, "  sound.speed=",sound.speed,"\n")
    heading <-  readBin(start[19:20], "integer", n=1, size=2, endian="little") * 0.1
    oce.debug(debug, "  heading=",heading,"\n")
    pitch <-  readBin(start[21:22], "integer", n=1, size=2, endian="little") * 0.1
    oce.debug(debug, "  pitch=",pitch,"\n")
    roll <-  readBin(start[23:24], "integer", n=1, size=2, endian="little") * 0.1
    oce.debug(debug, "  roll=",roll,"\n")
    pressureMSB <-  start[25]
    pressureLSW <-  readBin(start[27:28], "integer", n=1, size=2, endian="little")
    pressure <- (as.integer(pressureMSB)*65536 + pressureLSW) * 0.001
    oce.debug(debug, "  pressure=",pressure,"\n")
    temperature <-  readBin(start[29:30], "integer", n=1, size=2, endian="little") * 0.01
    oce.debug(debug, "  temperature=", temperature, "\n")
    beams <-  as.integer(start[35])
    oce.debug(debug, "  beams=", beams, "\n")
    cells <-  as.integer(start[36])
    oce.debug(debug, "  cells=", cells, "\n")

    ##fill <- if (cells %% 2) 1 else 0
    data.bytes <- beams * cells * (2 + 1 + 1) + 2

    ## The System Integrator Guide is contradictory on the matter of a fill byte.  On page 38
    ## it says it is needed.  But on page 57, the data declaration for cFill is commented out.
    ## I find that if I retain this skipping of a byte, then I cannot read one
    ## of the SLEIWEX files (sl08AQ01.prf), so I am hiding this in a FALSE block.
    if (FALSE)
        if (fill) readBin(file, "raw", n=1)

    ## bug: should perhaps be using velocity.scale instead of /1000
    v <- matrix(readBin(file, "integer", n=beams*cells, size=2, endian="little"), ncol=beams, byrow=FALSE) / 1000
    a <- matrix(readBin(file, "integer", n=beams*cells, size=1, signed=FALSE), ncol=beams, byrow=FALSE)
    q <- matrix(readBin(file, "integer", n=beams*cells, size=1, signed=FALSE), ncol=beams, byrow=FALSE)

    checksum <- readBin(file, "raw", n=2, size=1)

    two.bytes <- peek.ahead(file, 2)
    if (two.bytes[1] != sync.code) stop("expecting sync code 0x", sync.code, " but got 0x", two.bytes[1], " and 0x", two.bytes[2])
    if (two.bytes[2] != id.high.resolution.aquadopp.profile.data) stop("expecting id code 0x", id.high.resolution.aquadopp.profile.data, " but got 0x", two.bytes[2], " (while checking for next profile)")

    ### ready for another profile
    list(v=v, a=a, q=q,
         heading=heading, pitch=pitch, roll=roll,
         time=time, temperature=temperature, pressure=pressure)
}

read.header.nortek <- function(file, debug=getOption("oce.debug"), ...)
{
    sync.code <- as.raw(0xa5)
    id.hardware.configuration <- as.raw(0x05)
    id.head.configuration <- as.raw(0x04)
    id.user.configuration <- as.raw(0x00)
    header.length.hardware <- 48
    header.length.head <- 224
    header.length.user <- 512
    hardware <- head <- user <- list()
    for (header in 1:3) { # FIXME: code is needlessly written as if headers could be in different order
        two.bytes <- peek.ahead(file)
        if (two.bytes[1] != sync.code)
            stop("expecting sync code 0x", sync.code, " at byte ", seek(file), " but got 0x", two.bytes[1], " instead (while reading header #", header, ")")
        if (two.bytes[2] == id.hardware.configuration) {         # see page 29 of System Integrator Guide
            oce.debug(debug, "** scanning Hardware Configuration **\n")
            buf <- readBin(file, "raw", header.length.hardware)
            if (buf[2] != 0x05) stop("byte 2 must be 0x05 but is 0x", buf[2])
            hardware$size <- readBin(buf[3:4], "integer",signed=FALSE, n=1, size=2)
            oce.debug(debug, "  hardware$size=", hardware$size, "\n")
            hardware$serial.number <- gsub(" *$", "", paste(readBin(buf[5:18], "character", n=14, size=1), collapse=""))
            oce.debug(debug, "  hardware$serial.number", hardware$serial.number, "\n")
            hardware$config <- readBin(buf[19:20], "raw", n=2, size=1)
            oce.debug(debug, "  hardware$config:", hardware$config, "\n")
            hardware$frequency <- readBin(buf[21:22], "integer", n=1, size=2, endian="little", signed=FALSE) # not used
            oce.debug(debug, "  hardware$frequency:", hardware$frequency, "\n")
            hardware$pic.version <- readBin(buf[23:24], "integer", n=1, size=2, endian="little")
            oce.debug(debug, "  hardware$pic.version=", hardware$pic.version, "\n")
            hardware$hw.revision <- readBin(buf[25:26], "integer", n=1, size=2, endian="little")
            oce.debug(debug, "  hardware$hw.revision=", hardware$hw.revision, "\n")
            hardware$rec.size <- readBin(buf[27:28], "integer", n=1, size=2, endian="little")
            oce.debug(debug, "  hardware$rec.size=", hardware$rec.size, "\n")
            hardware$velocity.range <- readBin(buf[29:30], "integer", n=1, size=2, endian="little")
            oce.debug(debug, "  hardware$velocity.range=", hardware$velocity.range, "\n")
            hardware$fw.version <- as.numeric(paste(readBin(buf[43:46], "character", n=4, size=1), collapse=""))
            oce.debug(debug, "  hardware$fw.version=", hardware$fw.version, "\n")
        } else if (two.bytes[2] == id.head.configuration) {     # see page 30 of System Integrator Guide
            oce.debug(debug, "** scanning Head Configuration **\n")
            buf <- readBin(file, "raw", header.length.head)
            head$size <- readBin(buf[3:4], "integer",signed=FALSE, n=1, size=2)
            oce.debug(debug, "  head$size=", head$size, "\n")
            head$config <- byte2binary(buf[5:6], endian="little")
            oce.debug(debug, "  head$config=", head$config, "\n")
            head$config.pressure.sensor <- substr(head$config[1], 1, 1) == "1"
            oce.debug(debug, "  head$config.pressure.sensor=", head$config.pressure.sensor,"\n")
            head$config.magnetometer.sensor <- substr(head$config[1], 2, 2) == "1"
            oce.debug(debug, "  head$config.magnetometer.sensor=", head$config.magnetometer.sensor,"\n")
            head$config.tilt.sensor <- substr(head$config[1], 3, 3) == "1"
            oce.debug(debug, "  head$config.tilt.sensor=", head$config.tilt.sensor,"\n")
            head$orientation <- if (substr(head$config[1], 4, 4) == "1") "downward" else "upward"
            oce.debug(debug, "  head$orientation=", head$orientation, "\n")
            head$frequency <- readBin(buf[7:8], "integer", n=1, size=2, endian="little", signed=FALSE)
            oce.debug(debug, "  head$frequency=", head$frequency, "kHz\n")
            head$head.type <- readBin(buf[9:10], "integer", n=1, size=2, endian="little")
            oce.debug(debug, "  head$head.type=", head$head.type, "\n")
            head$head.serial.number <- gsub(" *$", "", paste(readBin(buf[11:22], "character", n=12, size=1), collapse=""))
            oce.debug(debug, "  head$head.serial.number=", head$head.serial.number, "\n")
            ## NOTE: p30 of System Integrator Guide does not detail anything from offsets 23 to 199;
            ## the inference of beam.angles and transformation.matrix is drawn from other code.
            head$beam.angles <- readBin(buf[23:30], "integer", n=4, size=2, endian="little", signed=TRUE) / 32767 * pi
            oce.debug(debug, "  head$beam.angles=", head$beam.angles, "(rad)\n")
            ## Transformation matrix (before division by 4096)
            ## FIXME: should we change the sign of rows 2 and 3 if pointed down??
            head$transformation.matrix <- matrix(readBin(buf[31:48], "integer", n=9, size=2, endian="little") , nrow=3, byrow=TRUE) / 4096
            if (debug > 0) {
                oce.debug(debug, "head$transformation.matrix\n")
                print(head$transformation.matrix);
            }
            head$number.of.beams <- readBin(buf[221:222], "integer", n=1, size=2, endian="little")
            oce.debug(debug, "  head$number.of.beams=", head$number.of.beams, "\n")
        } else if (two.bytes[2] == id.user.configuration) {     # User Configuration [p30-32 of System Integrator Guide]
            oce.debug(debug, "** scanning User Configuration **\n")
            buf <- readBin(file, "raw", header.length.user)
            user$blanking.distance <- readBin(buf[7:8], "integer", n=1, size=2, endian="little", signed=FALSE)
            oce.debug(debug, "  user$blanking.distance=", user$blanking.distance, "??? expect 0.05 m\n")
            user$measurement.interval <- readBin(buf[39:40], "integer", n=1, size=2, endian="little")
            oce.debug(debug, "  user$measurement.interval=", user$measurement.interval, "\n")
            user$T1 <- readBin(buf[5:6], "integer", n=1, size=2, endian="little")
            user$T2 <- readBin(buf[7:8], "integer", n=1, size=2, endian="little")
            user$T3 <- readBin(buf[9:10], "integer", n=1, size=2, endian="little")
            user$T4 <- readBin(buf[11:12], "integer", n=1, size=2, endian="little")
            user$T5 <- readBin(buf[13:14], "integer", n=1, size=2, endian="little")
            user$NPings <- readBin(buf[15:16], "integer", n=1, size=2, endian="little")
            user$AvgInterval <- readBin(buf[17:18], "integer", n=1, size=2, endian="little")
            user$number.of.beams <- readBin(buf[19:20], "integer", n=1, size=2, endian="little")
            oce.debug(debug, "\n user$T1=",user$T1,"user$T2=",user$T2,"user$T5=",user$T5,"user$NPings=",user$NPings,"user$AvgInterval=",user$AvgInterval,"user$number.of.beams=",user$number.of.beams,"\n")
            user$mode <- byte2binary(buf[59:60], endian="little")
            oce.debug(debug, "  user$mode: ", user$mode, "\n")
            user$velocity.scale <- if (substr(user$mode[2], 4, 4) == "0") 0.001 else 0.00001
            oce.debug(debug, "  user$velocity.scale: ", user$velocity.scale, "\n")
            tmp.cs <- readBin(buf[33:34], "integer", n=1, size=2, endian="little")
            if (tmp.cs == 0) user$coordinate.system <- "enu" # page 31 of System Integrator Guide
            else if (tmp.cs == 1) user$coordinate.system <- "xyz"
            else if (tmp.cs == 2) user$coordinate.system <- "beam"
            else stop("unknown coordinate system ", tmp.cs)
            oce.debug(debug, "  user$coordinate.system: ", user$coordinate.system, "\n")
            user$number.of.cells <- readBin(buf[35:36], "integer", n=1, size=2, endian="little")
            oce.debug(debug, "  user$number.of.cells: ", user$number.of.cells, "\n")
            user$hBinLength <- readBin(buf[37:38], "integer", n=1, size=2, endian="little", signed=FALSE)
            if (isTRUE(all.equal.numeric(head$frequency, 1000))) {
                ##  printf("\nCell size (m) ------------ %.2f", cos(DEGTORAD(25.0))*conf.hBinLength*0.000052734375);
                user$cell.size <- cos(25*pi/180) * user$hBinLength * 0.000052734375
            } else if (isTRUE(all.equal.numeric(head$frequency, 2000))) { # FIXME: use head$frequency or hardware$frequency?
                ##  printf("\nCell size (m) ------------ %.2f",     cos(DEGTORAD(25.0))*conf.hBinLength*0.0000263671875);
                user$cell.size <- cos(25*pi/180) * user$hBinLength *0.0000263671875
            } else {
                user$cell.size <- NA    # FIXME what should we do here?  Probably an ADV, so no concern
            }
            oce.debug(debug, "cell.size=", user$cell.size, "m\n")
            user$measurement.interval <- readBin(buf[39:40], "integer", n=1, size=2, endian="little")
            if (isTRUE(all.equal.numeric(head$frequency, 1000))) {
                ## printf("\nBlanking distance (m) ---- %.2f", cos(DEGTORAD(25.0))*(0.0135*conf.hT2 - 12.0*conf.hT1/head.hFrequency));
                user$blanking.distance <- cos(25*pi/180) * (0.0135 * user$T2 - 12 * user$T1 / head$frequency)
            } else if (isTRUE(all.equal.numeric(head$frequency, 2000))) {
                ## printf("\nBlanking distance (m) ---- %.2f", cos(DEGTORAD(25.0))*(0.00675*conf.hT2 - 12.0*conf.hT1/head.hFrequency));
                user$blanking.distance <- cos(25*pi/180) * (0.00675 * user$T2 - 12 * user$T1 / head$frequency)
            } else {
                user$blanking.distance <- 0
            }
            oce.debug(debug, "blanking.distance=", user$blanking.distance, "; user$T1=", user$T1, "and user$T2=", user$T2, "\n")
            oce.debug(debug, "measurement.interval=", user$measurement.interval, "\n")
            user$deployment.name <- readBin(buf[41:46], "character")
            user$sw.version <- readBin(buf[73:74], "integer", n=1, size=2, endian="little")
            oce.debug(debug, "sw.version=", user$sw.version,"\n")
            user$salinity <- readBin(buf[75:76], "integer", n=1, size=2, endian="little") * 0.1
            oce.debug(debug, "salinity=", user$salinity,"\n")
        } else {
            stop("cannot understand byte 0x", two.bytes[2], "; expecting one of the following: 0x", id.hardware.configuration, " [hardware configuration] 0x", id.head.configuration, " [head configuration] or 0x", id.user.configuration, " [user configuration]\n")
        }
    }
    list(hardware=hardware, head=head, user=user)
}

read.adp.nortek <- function(file, from=0, to, by=1, type=c("aquadopp high resolution"), debug=getOption("oce.debug"), monitor=TRUE, log.action, ...)
{
    from.keep <- from
    to.keep <- to
    sync.code <- as.raw(0xa5)
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
    header <- read.header.nortek(file)

    ## codes
    ## data
    two.bytes <- peek.ahead(file, 2)
    if (two.bytes[1] != sync.code)
        stop("expecting sync code 0x", sync.code, " at byte ", seek(file)-1, " but got 0x", two.bytes[1], " instead (while looking for the start of a profile)")

    id.profiler.data <- as.raw(0x21) # page 37 of System Integrator Guide
    id.high.resolution.aquadopp.profile.data <- as.raw(0x2a) # page 38 of System Integrator Guide

    if (two.bytes[2] == id.profiler.data) {
        stop("cannot yet read 'Aquadopp Profiler Velocity Data")
    } else if (two.bytes[2] == id.high.resolution.aquadopp.profile.data) {
        ;
    } else {
        stop("id code: 0x", two.bytes[2], " ... not understood by this version of read.aquadopp()\n")
    }

    ## read profiles
    header.length.hardware <- 48
    header.length.head <- 224
    header.length.user <- 512

    data.start <- header.length.hardware + header.length.head + header.length.user
    bytes.per.profile <- 54 + header$user$number.of.cells * header$head$number.of.beams * (2+1+1) + 2

    ## Measure file length to determine number of profiles, using floor() in case there is extra stuff at end
    seek(file, where=0, origin="end")
    file.size <- seek(file)
    profiles.in.file <- floor((file.size - data.start) / bytes.per.profile)
    ## Possibly interpret from and to as starting and ending times.
    seek(file, where=data.start, origin="start")
    t1 <- read.profile.aquadopp(file,debug=debug)$time
    t2 <- read.profile.aquadopp(file,debug=debug)$time
    dt <- as.numeric(difftime(t2, t1, units="sec"))
    sampling.start <- t1
    sampling.end <- sampling.start + profiles.in.file * as.numeric(difftime(t2, t1, units="sec"))

    if (!missing(from) && inherits(from, "POSIXt")) {
        from <- max(as.numeric(difftime(from, t1, units="sec")) / dt, 0)
        if (from < 0) warning("\"from\"=", format(from), " ignored, since it predates the first datum at ", format(t1))
        oce.debug(debug, "from=",from,"\n")
    }
    if (!missing(by) && is.character(by)) {
        if (length(grep(":", by)) > 0) {
            parts <- as.numeric(strsplit(by, ":")[[1]])
            if (length(parts == 2)) by.time <- parts[1] * 60 + parts[2]
            else if (length(parts == 3)) by.time <- parts[1] * 3600 + parts[2] * 60 + parts[3]
            else stop("malformed by time", by)
            by <- by.time / dt
        } else {
            warning("converting \"by\" from string to numeric.  (Use e.g. \"00:10\" to indicate 10s)")
            by <- as.numeric(by)
        }
    }
    if (!missing(from) && inherits(to, "POSIXt")) {
        to <- 1 + (as.numeric(difftime(to, t1, units="sec")) / dt - from) / by
        if (to < 0) stop("cannot have fewer than zero points.  You gave from = ", from.keep, " and to = ", to.keep)
    }

    if (from > 0)
        seek(file, data.start + from * bytes.per.profile)
    else
        seek(file, data.start)
    time <- pressure <- temperature <- heading <- pitch <- roll <- NULL
    if (by < 1) stop("the value of \"by\" must be an integer of 1 or larger")
    if (missing(to)) {
        to <- profiles.in.file
    }
    if (to > 0) {
        v <- array(dim=c(to, header$user$number.of.cells, header$head$number.of.beams))
        a <- array(dim=c(to, header$user$number.of.cells, header$head$number.of.beams))
        q <- array(dim=c(to, header$user$number.of.cells, header$head$number.of.beams))
        for (i in 1:to) {
            seek(file, data.start + (from + by*(i-1)) * bytes.per.profile)
            p <- read.profile.aquadopp(file,debug=debug)
            oce.debug(debug, "successfully read profile", i, "at time ", format(p$time), "\n")
            for (beam in 1:header$head$number.of.beams) {
                v[i,,beam] <- p$v[,beam]
                a[i,,beam] <- p$a[,beam]
                q[i,,beam] <- p$q[,beam]
            }
            time <- c(time, p$time)
            temperature <- c(temperature, p$temperature)
            pressure <- c(pressure, p$pressure)
            heading <- c(heading, p$heading)
            pitch <- c(pitch, p$pitch)
            roll <- c(roll, p$roll)
            if (monitor) {
                cat(".")
                if (!(i %% 50)) cat(i, "\n")
            }
        }
        if (monitor) cat("\nRead", to, "profiles\n")
        salinity <- rep(header$user$salinity, to)     # fake a time-series
        class(time) <- c("POSIXt", "POSIXct")
        attr(time, "tzone") <- getOption("oce.tz") # Q: does file hold the zone?
        data <- list(ma=list(v=v, a=a, q=q),
                     ss=list(distance=seq(header$user$blanking.distance,
                             by=header$user$cell.size,
                             length.out=header$user$number.of.cells)),
                     ts=list(time=time,
                     pressure=pressure,
                     temperature=temperature,
                     salinity=header$user$salinity,
                     heading=heading,
                     pitch=pitch,
                     roll=roll))

    } else {
        data <- list(ma=NULL, ss=NULL, ts=NULL)
    }
    metadata <- list(instrument.type="aquadopp high resolution",
                     filename=filename,
                     sampling.start=sampling.start,
                     sampling.end=sampling.end,
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
                     transformation.matrix=header$head$transformation.matrix,
                     deployment.name=header$user$deployment.name,
                     cell.size=header$user$cell.size,
                     velocity.scale=header$user$velocity.scale,
                     coordinate.system=header$user$coordinate.system,
                     oce.coordinate=header$user$coordinate.system,
                     oce.beam.attenuated=FALSE
                     )
    if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processing.log.item(log.action)
    res <- list(data=data, metadata=metadata, processing.log=log.item)
    class(res) <- c("nortek", "adp", "oce")
    res
}                                       # read.adp.nortek()
