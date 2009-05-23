## Questions for SonTek (regarding High Resolution Aquadopp Profile data),
## with SIG standing for System Integrator Guide.
##  1. should we use "frequency" from "head configuration" or "hardware configuration"
##  2. why is the cFill line commented out in the code on page 57 of SIG?
##  3. what is the unit of blanking distance, called "counts" on page 31 of SIG
##  4. how to decode the cell size?  (I get 1674 ... should be 0.04m)

read.profile.aquadopp <- function(file, debug=!TRUE)
{
    sync.code <- as.raw(0xa5)
    id.high.resolution.aquadopp.profile.data <- as.raw(0x2a) # page 38 of System Integrator Guide
    start <- readBin(file, "raw", 54) # see page 38 of System Integrator Guide
    time <- sontek.time(start[5:12])
    if (debug) cat("  time=", format(time), "\n")
    sound.speed <-  readBin(start[17:18], "integer", n=1, size=2, endian="little", signed=FALSE) * 0.1
    if (debug) cat("  sound.speed=",sound.speed,"\n")
    heading <-  readBin(start[19:20], "integer", n=1, size=2, endian="little") * 0.1
    if (debug) cat("  heading=",heading,"\n")
    pitch <-  readBin(start[21:22], "integer", n=1, size=2, endian="little") * 0.1
    if (debug) cat("  pitch=",pitch,"\n")
    roll <-  readBin(start[23:24], "integer", n=1, size=2, endian="little") * 0.1
    if (debug) cat("  roll=",roll,"\n")
    pressureMSB <-  start[25]
    pressureLSW <-  readBin(start[27:28], "integer", n=1, size=2, endian="little")
    pressure <- (as.integer(pressureMSB)*65536 + pressureLSW) * 0.001
    if (debug) cat("  pressure=",pressure,"\n")
    temperature <-  readBin(start[29:30], "integer", n=1, size=2, endian="little") * 0.01
    if (debug) cat("  temperature=", temperature, "\n")
    beams <-  as.integer(start[35])
    if (debug) cat("  beams=", beams, "\n")
    cells <-  as.integer(start[36])
    if (debug) cat("  cells=", cells, "\n")

    ##fill <- if (cells %% 2) 1 else 0
    data.bytes <- beams * cells * (2 + 1 + 1) + 2

    ## The System Integrator Guide is contradictory on the matter of a fill byte.  On page 38
    ## it says it is needed.  But on page 57, the data declaration for cFill is commented out.
    ## I find that if I retain this skipping of a byte, then I cannot read one
    ## of the SLEIWEX files (sl08AQ01.prf), so I am hiding this in a FALSE block.
    if (FALSE)
        if (fill) readBin(file, "raw", n=1)
    checksum <- readBin(file, "raw", n=2, size=1)

    ## bug: should perhaps be using velocity.scale instead of /1000
    v <- matrix(readBin(file, "integer", n=beams*cells, size=2, endian="little"), ncol=beams, byrow=FALSE) / 1000
    a <- matrix(readBin(file, "integer", n=beams*cells, size=1, signed=FALSE), ncol=beams, byrow=FALSE)
    q <- matrix(readBin(file, "integer", n=beams*cells, size=1, signed=FALSE), ncol=beams, byrow=FALSE)

    two.bytes <- peek.ahead(file, 2)
    if (two.bytes[1] != sync.code) stop("expecting sync code 0x", sync.code, " but got 0x", two.bytes[1], " (WTF)")
    if (two.bytes[2] != id.high.resolution.aquadopp.profile.data) stop("expecting id code 0x", id.high.resolution.aquadopp.profile.data, " but got 0x", two.bytes[2], " (while checking for next profile)")


    ### ready for another profile
    list(v=v, a=a, q=q,
         heading=heading, pitch=pitch, roll=roll,
         time=time, temperature=temperature, pressure=pressure)
}

peek.ahead <- function(file, bytes=2, debug=!TRUE)
{
    pos <- seek(file)
    res <- readBin(file, "raw", n=bytes, size=1)
    if (debug) cat("peeked at", paste("0x", paste(res, sep=" "), sep=""), "\n")
    seek(file, pos)
    res
}

sontek.time <- function(t, tz="UTC")
{
    minute <- bcd2integer(t[1])
    second <- bcd2integer(t[2])
    day <- bcd2integer(t[3])
    hour <- bcd2integer(t[4])
    year <- bcd2integer(t[5])
    year <- year + if (year >= 90) 1900 else 2000 # page 51 of System Integrator Guide
    month <- bcd2integer(t[6])
    milliseconds <- readBin(t[7:8], "integer", n=1, size=2, endian="little", signed=FALSE)
    ISOdatetime(year, month, day, hour, minute, second+milliseconds/1000, tz=tz)
}

display.bytes <- function(b, label="")
{
    n <- length(b)
    cat("\n", label, " (", n, "bytes)\n", sep="")
    print(b)
}



read.aquadopp <- function(file,
                          type="high resolution",
                          skip=0, read, stride=1,
                          debug=0,
                          monitor=TRUE,
                          log.action) {
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
    ## codes
    sync.code <- as.raw(0xa5)
    id.hardware.configuration <- as.raw(0x05)
    id.head.configuration <- as.raw(0x04)
    id.user.configuration <- as.raw(0x00)
    id.profiler.data <- as.raw(0x21) # page 37 of System Integrator Guide
    id.high.resolution.aquadopp.profile.data <- as.raw(0x2a) # page 38 of System Integrator Guide
    header.length.hardware <- 48
    header.length.head <- 224
    header.length.user <- 512
    for (header in 1:3) {
        two.bytes <- peek.ahead(file)
        if (two.bytes[1] != sync.code)
            stop("expecting sync code 0x", sync.code, " at byte ", seek(file)-1, " but got 0x", buf[1], " instead (while reading header #", header, ")")
        if (two.bytes[2] == id.hardware.configuration) {         # see page 29 of System Integrator Guide
            if (debug) cat("** scanning Hardware Configuration **\n")
            buf <- readBin(file, "raw", header.length.hardware)
            if (buf[2] != 0x05) stop("byte 2 must be 0x05 but is 0x", buf[2])
            size <- readBin(buf[3:4], "integer",signed=FALSE, n=1, size=2)
            if (debug) cat("  size=", size, "\n")
            hardware.serial.number <- gsub(" *$", "", paste(readBin(buf[5:18], "character", n=14, size=1), collapse=""))
            if (debug) cat("  hardware.serial.number", hardware.serial.number, "\n")
            config <- readBin(buf[19:20], "raw", n=2, size=1)
            if (debug) cat("  config:", config, "\n")
            frequency <- readBin(buf[21:22], "integer", n=1, size=2, endian="little") # not used
            if (debug) cat("  frequency:", frequency, "\n")
            pic.version <- readBin(buf[23:24], "integer", n=1, size=2, endian="little")
            if (debug) cat("  pic.version=", pic.version, "\n")
            hw.revision <- readBin(buf[25:26], "integer", n=1, size=2, endian="little")
            if (debug) cat("  hw.revision=", buf[25:26], "\n")
            rec.size <- readBin(buf[27:28], "integer", n=1, size=2, endian="little")
            if (debug) cat("  rec.size=", rec.size, "\n")
            status <- readBin(buf[29:30], "integer", n=1, size=2, endian="little")
            if (debug) cat("  status=", status, "\n")
            fw.version <- as.numeric(paste(readBin(buf[43:46], "character", n=4, size=1), collapse=""))
            if (debug) cat("  fw.version=", fw.version, "\n")
        } else if (two.bytes[2] == id.head.configuration) {     # see page 30 of System Integrator Guide
            if (debug) cat("** scanning Head Configuration **\n")
            buf <- readBin(file, "raw", header.length.head)
            size <- readBin(buf[3:4], "integer",signed=FALSE, n=1, size=2)
            if (debug) cat("  size=", size, "\n")
            config <- byte2binary(buf[5:6], endian="little")
            if (debug) cat("  config=", config, "\n")
            config.pressure.sensor <- substr(config[1], 8, 8) == "1"
            if (debug) cat("  config.pressure.sensor=",config.pressure.sensor,"\n")
            config.magnetometer.sensor <- substr(config[1], 7, 7) == "1"
            if (debug) cat("  config.magnetometer.sensor=",config.magnetometer.sensor,"\n")
            config.tilt.sensor <- substr(config[1], 6, 6) == "1"
            if (debug) cat("  config.tilt.sensor=",config.tilt.sensor,"\n")
            config.downward.looking <- substr(config[1], 5, 5) == "1"
            if (debug) cat("  config.downward.looking=",config.downward.looking,"\n")
            frequency <- readBin(buf[7:8], "integer", n=1, size=2, endian="little")
            if (debug) cat("  frequency=", frequency, "kHz\n")
            head.type <- readBin(buf[9:10], "integer", n=1, size=2, endian="little")
            if (debug) cat("  head.type=", head.type, "\n")
            head.serial.number <- gsub(" *$", "", paste(readBin(buf[11:22], "character", n=12, size=1), collapse=""))
            if (debug) cat("  head.serial.number=", head.serial.number, "\n")
            number.of.beams <- readBin(buf[221:222], "integer", n=1, size=2, endian="little")
            if (debug) cat("  number.of.beams=", number.of.beams, "\n")
        } else if (two.bytes[2] == id.user.configuration) {     # User Configuration [p30-32 of System Integrator Guide]
            if (debug) cat("** scanning User Configuration **\n")
            buf <- readBin(file, "raw", header.length.user)
            blanking.distance <- readBin(buf[7:8], "integer", n=1, size=2, endian="little", signed=FALSE)
            if (debug) cat("  blanking.distance=", blanking.distance, "??? expect 0.05 m\n")
            measurement.interval <- readBin(buf[39:40], "integer", n=1, size=2, endian="little")
            if (debug) cat("  measurement.inteval=", measurement.interval, "\n")
            T1 <- readBin(buf[9:10], "integer", n=1, size=2, endian="little")
            T2 <- readBin(buf[11:12], "integer", n=1, size=2, endian="little")
            T5 <- readBin(buf[13:14], "integer", n=1, size=2, endian="little")
            NPings <- readBin(buf[15:16], "integer", n=1, size=2, endian="little")
            AvgInterval <- readBin(buf[17:18], "integer", n=1, size=2, endian="little")
            NBeams <- readBin(buf[19:20], "integer", n=1, size=2, endian="little")
            cat("\n.... T1=",T1,"T2=",T2,"T5=",T5,"NPings=",NPings,"AvgInterval=",AvgInterval,"NBeams=",NBeams,"\n\n")

            ##2     T1                   8        receive length (counts)
            ##2     T2                  10        time between pings (counts)
            ##2     T5                  12        time between burst sequences (counts)
            ##2     NPings              14        number of beam sequences per burst
            ##2     AvgInterval         16        average interval in seconds
            ##2     NBeams              18        number of beams


            mode <- byte2binary(buf[59:60], endian="little")
            if (debug) cat("  mode: ", mode, "\n")
            velocity.scale <- if (substr(mode[2], 4, 4) == "0") 0.001 else 0.00001
            if (debug) cat("  velocity.scale: ", velocity.scale, "\n")
            tmp.cs <- readBin(buf[33:34], "integer", n=1, size=2, endian="little")
            if (tmp.cs == 0) coordinate.system <- "earth" # ENU in page 31 of System Integrator Guide
            else if (tmp.cs == 1) coordinate.system <- "frame" # ENU in page 31 of System Integrator Guide
            else if (tmp.cs == 2) coordinate.system <- "beam" # ENU in page 31 of System Integrator Guide
            if (debug) cat("  coordinate.system: ", coordinate.system, "\n")
            number.of.cells <- as.integer(buf[35]) # should be using 35 and 36
            cat("  number.of.cells: ", number.of.cells, "\n")
            cat(rep("-",10),"\n\n")
            cell.size <- readBin(buf[37:38], "integer", n=1, size=2, endian="little")
            cat("cell.size ... should be  0.04 m\n")
            cat("cell.size??? ", buf[37], " ", buf[38] , "***\n")
            cat("cell.size??? ", as.integer(buf[37]), " ", as.integer(buf[38]), "***\n")
            cat("***cell.size:", cell.size, "WRONG\n")
            cat(rep("-",10),"\n\n")
            measurement.interval <- readBin(buf[39:40], "integer", n=1, size=2, endian="little")
            if (debug) cat("measurement.interval=", measurement.interval, "****\n\n")
            deployment.name <- readBin(buf[41:46], "character")
        } else {
            stop("cannot understand byte 0x", two.bytes[2], "; expecting one of the following: 0x", id.hardware.configuration, " [hardware configuration] 0x", id.head.configuration, " [head configuration] or 0x", id.user.configuration, " [user configuration]\n")
        }
    }

    ## data
    two.bytes <- peek.ahead(file, 2)
    if (two.bytes[1] != sync.code)
        stop("expecting sync code 0x", sync.code, " at byte ", seek(file)-1, " but got 0x", buf[1], " instead (while looking for the start of a profile)")
    if (two.bytes[2] == id.profiler.data) {
        stop("cannot yet read 'Aquadopp Profiler Velocity Data")
    } else if (two.bytes[2] == id.high.resolution.aquadopp.profile.data) {
        if (debug) cat("\n*** should read 'High Resolution Aquadopp Profile Data' now -- TESTING ONLY!! **\n\n")
    } else {
        stop("id code: 0x", two.bytes[2], " ... not understood by this version of read.aquadopp()\n")
    }

    ## read profiles
    data.start <- header.length.hardware + header.length.head + header.length.user
    bytes.per.profile <- 54 + number.of.cells*number.of.beams*(2+1+1) + 2

    ## Measure file length to determine number of profiles, using floor() in case there is extra stuff at end
    seek(file, where=0, origin="end")
    file.size <- seek(file)
    profiles.in.file <- floor((file.size - data.start) / bytes.per.profile)

    if (skip > 0)
        seek(file, data.start + skip * bytes.per.profile)
    else
        seek(file, data.start)
    time <- pressure <- temperature <- salinity <- heading <- pitch <- roll <- NULL
    if (stride != 1) stop("cannot handle 'stride' values other than 1")
    if (read < 1) stop("cannot read fewer than 1 profile")
    if (missing(read)) {
        read <- profiles.in.file
    }
    v1 <- array(dim=c(read, number.of.cells))
    v2 <- array(dim=c(read, number.of.cells))
    v3 <- array(dim=c(read, number.of.cells))

    for (i in 1:read) {
        p <- read.profile.aquadopp(file,debug=debug)
        v1[i,] <- p$v[,1]
        v2[i,] <- p$v[,2]
        v3[i,] <- p$v[,3]
        time <- c(time, p$time)
        temperature <- c(temperature, p$temperature)
        salinity <- c(salinity, NA)
        pressure <- c(pressure, p$pressure)
        heading <- c(heading, p$heading)
        pitch <- c(pitch, p$pitch)
        roll <- c(roll, p$roll)
        if (monitor) {
            cat(".")
            if (!(i %% 50)) cat(i, "\n")
        }
    }
    class(time) <- c("POSIXt", "POSIXct")
    attr(time, "tzone") <- "UTC"        # BUG should let user control this
                                        # Q: does file hold the zone?

    data <- list(ma=list(v1=v1, v2=v2, v3=v3),
                 ss=list(distance=1:number.of.cells),
                 ts=list(time=time,
                 pressure=pressure,
                 temperature=temperature,
                 salinity=salinity,
                 heading=heading,
                 pitch=pitch,
                 roll=roll)
                 )

    metadata <- list(
                     filename=filename,
                     size=size,
                     hardware.serial.number=hardware.serial.number,
                     frequency=frequency,
                     internal.code.version=pic.version,
                     hardware.revision=hw.revision,
                     rec.size=rec.size,
                     status=status,
                     firmware.version=fw.version,
                     config=config,
                     config.pressure.sensor=config.pressure.sensor,
                     config.magnetometer.sensor=config.magnetometer.sensor,
                     config.tilt.sensor=config.tilt.sensor,
                     config.downward.looking=config.downward.looking,
                     frequency=frequency,
                     head.serial.number=head.serial.number,
                     blanking.distance=blanking.distance,
                     measurement.interval=measurement.interval,
                     number.of.beams=number.of.beams,
                     number.of.cells=number.of.cells,
                     deployment.name=deployment.name,
                     cell.size=cell.size,
                     velocity.scale=velocity.scale,
                     coordinate.system=coordinate.system
                     )
    if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processing.log.item(log.action)
    res <- list(data=data, metadata=metadata, processing.log=log.item)
    class(res) <- c("aquadopp", "oce")
    res
}
summary.aquadopp <- function(object, ...)
{
    if (!inherits(object, "aquadopp")) stop("method is only for aquadopp objects")
    res <- list(filename=object$metadata$filename,
                hardware.serial.number=object$metadata$hardware.serial.number,
                internal.code.version=object$metadata$internal.code.version,
                hardware.revision=object$metadata$hardware.revision,
                frequency=object$metadata$frequency,
                rec.size=object$metadata$rec.size*65536,
                status=object$metadata$status,
                firmware.version=object$metadata$firmware.version,
                config=object$metadata$config,
                config.pressure.sensor=object$metadata$config.pressure.sensor,
                config.magnetometer.sensor=object$metadata$config.magnetometer.sensor,
                config.tilt.sensor=object$metadata$config.pressure.sensor,
                config.pressure.sensor=object$metadata$config.tilt.sensor,
                config.downward.looking=object$metadata$config.downward.looking,
                frequency=object$metadata$frequency,
                head.serial.number=object$metadata$head.serial.number,
                blanking.distance=object$metadata$blanking.distance,
                measurement.interval=object$metadata$measurement.interval,
                number.of.beams=object$metadata$number.of.beams,
                number.of.cells=object$metadata$number.of.cells,
                deployment.name=object$metadata$deployment.name,
                cell.size=object$metadata$cell.size,
                velocity.scale=object$metadata$velocity.scale,
                coordinate.system=object$metadata$coordinate.system,
                processing.log=processing.log.summary(object))
    class(res) <- "summary.aquadopp"
    res
}
print.summary.aquadopp <- function(x, digits=max(6, getOption("digits") - 1), ...)
{
    cat("aquadopp timeseries\n")
    cat("    Deployment name:            ", x$deployment.name, "\n")
    cat("    Filename:                   ", x$filename, "\n")
    cat("  Hardware Configuration\n")
    cat("    Serial number:              ", x$hardware.serial.number, "\n")
    cat("    Internal version code:      ", x$internal.code.version, "\n")
    cat("    Revision number:            ", x$hardware.revision, "\n")
    cat("    Recorder size:              ", x$rec.size, " (???)\n")
    cat("    Firmware version:           ", x$firmware.version, "\n")
    cat("    Velocity range:             ", if(x$status) "high\n" else "normal\n")
    cat("  Head Configuration\n")
    cat("    Pressure sensor:            ", if (x$config.pressure.sensor) "yes\n" else "no\n")
    cat("    Compass:                    ", if (x$config.magnetometer.sensor) "yes\n" else "no\n")
    cat("    Tilt sensor:                ", if (x$config.tilt.sensor) "yes\n" else "no\n")
    cat("    System 1 and 2:              [not coded yet]\n")
    cat("    Frequency:                  ", x$frequency, "kHz\n")
    cat("    Serial number:              ", x$head.serial.number, "\n")
    cat("    Transformation matrix:       [not coded yet]\n")
    cat("    Pressure sensor calibration: [not coded yet]\n")
    cat("    Number of beams:            ", x$number.of.beams, "\n")
    cat("    System 5 through 20:         [not coded yet]\n")
    cat("  User Setup\n")
    cat("    Measurement/burst interval: ", x$measurement.interval, "s\n")
    cat("    Cell size                   ", x$cell.size, "*** should be 0.04m ***\n")
    cat("    Orientation:                ", if (x$config.downward.looking) "downward-looking\n" else "upward-looking\n")
    cat("    Velocity scale:             ", x$velocity.scale, "m/s\n")
    cat("    Coordinate system:          ", x$coordinate.system, "... should be BEAM ...\n")
    cat("
? Distance to bottom                    1.00 m
? Extended velocity range               ON
? Pulse distance (Lag1)                 1.10 m
? Pulse distance (Lag2)                 0.36 m
? Profile range                         1.00 m
? Horizontal velocity range             0.84 m/s
? Vertical velocity range               0.35 m/s
")
    cat("    Number of cells:             ", x$number.of.cells, "\n")
    cat("
? Average interval                      10 sec
? Blanking distance                     0.05 m
")
    cat("    Blanking distance:           ", x$blanking.distance, "ok?\n")
    cat("
? Measurement load                      42 %
? Burst sampling                        OFF
? Samples per burst                     N/A
? Sampling rate                         N/A
? Compass update rate                   10 sec
? Analog input 1                        NONE
? Analog input 2                        NONE
? Power output                          DISABLED
? Powerlevel first ping                 HIGH-
? Powerlevel ping 2                     HIGH
? Coordinate system                     BEAM
? Sound speed                           MEASURED
? Salinity                              35.0 ppt
? Number of beams                       3
? Number of pings per burst             17
? Software version                      1.03
? Deployment name                       sl08AQ
? Wrap mode                             OFF
? Deployment time                       6/25/2008 10:00:00 AM
? Comments                              aquadopp high res (pc mode).
? System1                               7
? System2                               15
? System3                               6
? System4                               296
? System5                               512
? System9                               34
? System10                              0
? System11                              0
? System12                              0
? System13                              0
? System14                              10
? System16                              25
? System17                              1674
? System22                              3600
? System28                              1
? System29                              1
? System30                              20
? System31                              15618 15646 15673 15699
? System32 (PhaseToVel1)                233
? System33 (Ua1)                        32768
? System34 (Uah1)                       16384
? System35 (PhaseToVel2)                706
? System36 (Ua2)                        7035
? System37 (Uah2)                       36285
? System38 (Lag1)                       179
? System39 (Lag2)                       59
? System40 (T1Lag2)                     5
? System41 (T2Lag2)                     41
? System42 (T3Lag2)                     17
? System42                              0
? System43                              30
? System44                              0
? System45                              5
? Start command                         3
? CRC download                          ON
")
    cat("*status", x$status, "OK???\n")
    print(x$processing.log)
    invisible(x)
}

plot.aquadopp <- function(x, which=1:3, col=oce.colors.palette(128, 1),
                          zlim,
                          adorn=NULL,
                          draw.timerange=getOption("oce.draw.timerange"),
                          mgp=getOption("oce.mgp"), ...)
{
    if (!inherits(x, "aquadopp")) stop("method is only for aquadopp objects")
    opar <- par(no.readonly = TRUE)
    lw <- length(which)
    if (lw > 1) on.exit(par(opar))
    par(mgp=mgp)

    images <- 1:12
    timeseries <- 13:18
    if (any(!which %in% c(images, timeseries))) stop("unknown value of 'which'")
    adorn.length <- length(adorn)
    if (adorn.length == 1) {
        adorn <- rep(adorn, lw)
        adorn.length <- lw
    }

    par(mar=c(mgp[1],mgp[1]+1,1,1))
    data.names <- names(x$data$ma)
    shown.time.interval <- FALSE
    tt <- x$data$ts$time
    class(tt) <- "POSIXct"              # otherwise image() gives warnings
    zlim.not.given <- missing(zlim)

    if (zlim.not.given && all(which %in% 4:6)) { # amplitude uses a single scale for all
        zlim <- range(abs(x$data$ma[[which[1]]]), na.rm=TRUE)
        for (w in 2:length(which)) {
            zlim <- range(abs(c(zlim, x$data$ma[[which[w]]])), na.rm=TRUE)
        }
        zlim.not.given <- FALSE                                    # fake it
    }
    if (any(which %in% images)) {
        scale <- (0.132 + (0.2 - 0.132) * exp(-(lw - 1))) / 0.2
        w <- (1.5 + par("mgp")[2]) * par("csi") * scale * 2.54 + 0.5
        ##cat("csi=", par("csi"), "w=", w, "\n")
        lay <- layout(matrix(1:(2*lw), nrow=lw, byrow=TRUE), widths=rep(c(1, lcm(w)), lw))
    } else {
        lay <- layout(cbind(1:lw))
    }
    ##layout.show(lay)
    ##stop()
    ma.names <- names(x$data$ma)
    for (w in 1:lw) {
        ##cat("which[w]=", which[w], "csi=", par("csi"), "\n")
        if (zlim.not.given) {
            if (which[w] %in% 7:9) {    # correlation goes from 0 to 100 percent
                zlim <- c(0, 100)
            } else {
                if (which[w] < 12) {
                    zlim <- max(abs(x$data$ma[[which[w]]]), na.rm=TRUE) * c(-1,1)
                }
            }
        }
        if (which[w] %in% images) {                   # image types
            imagep(x=tt, y=x$data$ss$distance, z=x$data$ma[[ma.names[which[w]]]],
                   zlim=zlim,
                   col=col,
                   ylab=resizable.label("distance"),
                   xlab="Time",
                   zlab=ma.names[which[w]],
                   draw.time.range=!shown.time.interval,
                   draw.contours=FALSE,
                   do.layout=FALSE,
                   ...)
            shown.time.interval <- TRUE
        }
        if (which[w] %in% timeseries) { # time-series types
            if (which[w] == 13) plot(x$data$ts$time, x$data$ts$salinity,    ylab="S [psu]",       type='l', axes=FALSE)
            if (which[w] == 14) plot(x$data$ts$time, x$data$ts$temperature, ylab= expression(paste("T [ ", degree, "C ]")), type='l', axes=FALSE)
            if (which[w] == 15) plot(x$data$ts$time, x$data$ts$pressure,    ylab="p [dbar]",       type='l', axes=FALSE)
            if (which[w] == 16) plot(x$data$ts$time, x$data$ts$heading,     ylab="heading", type='l', axes=FALSE)
            if (which[w] == 17) plot(x$data$ts$time, x$data$ts$pitch,       ylab="pitch",   type='l', axes=FALSE)
            if (which[w] == 18) plot(x$data$ts$time, x$data$ts$roll,        ylab="roll",    type='l', axes=FALSE)
            oce.axis.POSIXct(1, x=x$data$ts$time)
            box()
            axis(2)
            if (!shown.time.interval) {
                mtext(paste(paste(format(range(x$data$ts$time)), collapse=" to "),
                            attr(x$data$ts$time[1], "tzone")),
                      side=3, cex=5/6*par("cex"), adj=0)
                shown.time.interval <- TRUE
            }
            if (w <= adorn.length) {
                t <- try(eval(adorn[w]), silent=TRUE)
                if (class(t) == "try-error") warning("cannot evaluate adorn[", w, "]\n")
            }
        }
    }
}
