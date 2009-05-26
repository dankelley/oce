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


## To do
##  1. transformation matrix so we can have earth and frame coords

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
            frequency <- readBin(buf[21:22], "integer", n=1, size=2, endian="little", signed=FALSE) # not used
            if (debug) cat("  frequency:", frequency, "\n")
            pic.version <- readBin(buf[23:24], "integer", n=1, size=2, endian="little")
            if (debug) cat("  pic.version=", pic.version, "\n")
            hw.revision <- readBin(buf[25:26], "integer", n=1, size=2, endian="little")
            if (debug) cat("  hw.revision=", buf[25:26], "\n")
            rec.size <- readBin(buf[27:28], "integer", n=1, size=2, endian="little")
            if (debug) cat("  rec.size=", rec.size, "\n")
            velocity.range <- readBin(buf[29:30], "integer", n=1, size=2, endian="little")
            if (debug) cat("  velocity.range=", velocity.range, "\n")
            fw.version <- as.numeric(paste(readBin(buf[43:46], "character", n=4, size=1), collapse=""))
            if (debug) cat("  fw.version=", fw.version, "\n")
        } else if (two.bytes[2] == id.head.configuration) {     # see page 30 of System Integrator Guide
            if (debug) cat("** scanning Head Configuration **\n")
            buf <- readBin(file, "raw", header.length.head)
            size <- readBin(buf[3:4], "integer",signed=FALSE, n=1, size=2)
            if (debug) cat("  size=", size, "\n")
            config <- byte2binary(buf[5:6], endian="little")
            if (debug) cat("  config=", config, "\n")
            config.pressure.sensor <- substr(config[1], 1, 1) == "1"
            if (debug) cat("  config.pressure.sensor=",config.pressure.sensor,"\n")
            config.magnetometer.sensor <- substr(config[1], 2, 2) == "1"
            if (debug) cat("  config.magnetometer.sensor=",config.magnetometer.sensor,"\n")
            config.tilt.sensor <- substr(config[1], 3, 3) == "1"
            if (debug) cat("  config.tilt.sensor=",config.tilt.sensor,"\n")
            config.downward.looking <- substr(config[1], 4, 4) == "1"
            if (debug) cat("  config.downward.looking=",config.downward.looking,"\n")
            frequency <- readBin(buf[7:8], "integer", n=1, size=2, endian="little", signed=FALSE)
            if (debug) cat("  frequency=", frequency, "kHz\n")
            head.type <- readBin(buf[9:10], "integer", n=1, size=2, endian="little")
            if (debug) cat("  head.type=", head.type, "\n")
            head.serial.number <- gsub(" *$", "", paste(readBin(buf[11:22], "character", n=12, size=1), collapse="")) # 12 chars
            if (debug) cat("  head.serial.number=", head.serial.number, "\n")

            beam.angles <- readBin(buf[23:30], "integer", n=4, size=2, endian="little", signed=TRUE) / 32767 * pi
            if (debug) cat("BEAM ANGLES=", beam.angles, "(rad)\n")

            ## short hBeamToXYZ[9];          // beam to XYZ transformation matrix for up orientation
            ##Transformation matrix (before division by 4096) -- checks out ok
            ## 6461 -3232 -3232
            ##    0 -5596  5596
            ## 1506  1506  1506
            beam.to.xyz <- matrix(readBin(buf[31:48], "integer", n=9, size=2, endian="little") , nrow=3, byrow=TRUE) / 4096
            if (debug) {cat("beam.to.xyz\n");print(beam.to.xyz);}

            number.of.beams <- readBin(buf[221:222], "integer", n=1, size=2, endian="little")
            if (debug) cat("  number.of.beams=", number.of.beams, "\n")
        } else if (two.bytes[2] == id.user.configuration) {     # User Configuration [p30-32 of System Integrator Guide]
            if (debug) cat("** scanning User Configuration **\n")
            buf <- readBin(file, "raw", header.length.user)
            blanking.distance <- readBin(buf[7:8], "integer", n=1, size=2, endian="little", signed=FALSE)
            if (debug) cat("  blanking.distance=", blanking.distance, "??? expect 0.05 m\n")
            measurement.interval <- readBin(buf[39:40], "integer", n=1, size=2, endian="little")
            if (debug) cat("  measurement.inteval=", measurement.interval, "\n")
            T1 <- readBin(buf[5:6], "integer", n=1, size=2, endian="little")
            T2 <- readBin(buf[7:8], "integer", n=1, size=2, endian="little")
            T3 <- readBin(buf[9:10], "integer", n=1, size=2, endian="little")
            T4 <- readBin(buf[11:12], "integer", n=1, size=2, endian="little")
            T5 <- readBin(buf[13:14], "integer", n=1, size=2, endian="little")
            NPings <- readBin(buf[15:16], "integer", n=1, size=2, endian="little")
            AvgInterval <- readBin(buf[17:18], "integer", n=1, size=2, endian="little")
            NBeams <- readBin(buf[19:20], "integer", n=1, size=2, endian="little")
            if (debug) cat("\n T1=",T1,"T2=",T2,"T5=",T5,"NPings=",NPings,"AvgInterval=",AvgInterval,"NBeams=",NBeams,"\n\n")
            mode <- byte2binary(buf[59:60], endian="little")
            if (debug) cat("  mode: ", mode, "\n")
            velocity.scale <- if (substr(mode[2], 4, 4) == "0") 0.001 else 0.00001
            if (debug) cat("  velocity.scale: ", velocity.scale, "\n")
            tmp.cs <- readBin(buf[33:34], "integer", n=1, size=2, endian="little")
            if (tmp.cs == 0) coordinate.system <- "earth" # page 31 of System Integrator Guide
            else if (tmp.cs == 1) coordinate.system <- "frame"
            else if (tmp.cs == 2) coordinate.system <- "beam"
            else stop("unknown coordinate system ", tmp.cs)
            if (debug) cat("  coordinate.system: ", coordinate.system, "\n")
            number.of.cells <- readBin(buf[35:36], "integer", n=1, size=2, endian="little")
            if (debug) cat("  number.of.cells: ", number.of.cells, "\n")
            hBinLength <- readBin(buf[37:38], "integer", n=1, size=2, endian="little", signed=FALSE)
            if (isTRUE(all.equal.numeric(frequency, 1000))) {
                ##  printf("\nCell size (m) ------------ %.2f", cos(DEGTORAD(25.0))*conf.hBinLength*0.000052734375);
                cell.size <- cos(25*pi/180) * hBinLength * 0.000052734375
            } else if (isTRUE(all.equal.numeric(frequency, 2000))) {
                ##  printf("\nCell size (m) ------------ %.2f",     cos(DEGTORAD(25.0))*conf.hBinLength*0.0000263671875);
                cell.size <- cos(25*pi/180) * hBinLength *0.0000263671875
            } else {
                stop("The frequency must be 1000 or 2000, but it is ", frequency)
            }
            if (debug) cat("cell.size=", cell.size, "(should be  0.04 m)\n")
            measurement.interval <- readBin(buf[39:40], "integer", n=1, size=2, endian="little")
            if (isTRUE(all.equal.numeric(frequency, 1000))) {
                ## printf("\nBlanking distance (m) ---- %.2f", cos(DEGTORAD(25.0))*(0.0135*conf.hT2 - 12.0*conf.hT1/head.hFrequency));
                blanking.distance <- cos(25*pi/180) * (0.0135 * T2 - 12 * T1 / frequency)
            } else if (isTRUE(all.equal.numeric(frequency, 2000))) {
                ## printf("\nBlanking distance (m) ---- %.2f", cos(DEGTORAD(25.0))*(0.00675*conf.hT2 - 12.0*conf.hT1/head.hFrequency));
                blanking.distance <- cos(25*pi/180) * (0.00675 * T2 - 12 * T1 / frequency)
            } else {
                stop("The frequency must be 1000 or 2000, but it is ", frequency)
            }
            if (debug) cat("blanking.distance=", blanking.distance, "(should be 0.05).  T1=", T1, "and T2=", T2, "\n")
            if (debug) cat("measurement.interval=", measurement.interval, "****\n\n")
            deployment.name <- readBin(buf[41:46], "character")
            sw.version <- readBin(buf[73:74], "integer", n=1, size=2, endian="little")
            if (debug) cat("sw.version=", sw.version,"\n")
            salinity <- readBin(buf[75:76], "integer", n=1, size=2, endian="little") * 0.1
            if (debug) cat("salinity=", salinity,"\n")
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
    time <- pressure <- temperature <- heading <- pitch <- roll <- NULL
    if (stride != 1) stop("cannot handle 'stride' values other than 1")
    if (missing(read)) {
        read <- profiles.in.file
    }
    if (read > 1) {
        v <- array(dim=c(read, number.of.cells, number.of.beams))
        a <- array(dim=c(read, number.of.cells, number.of.beams))
        q <- array(dim=c(read, number.of.cells, number.of.beams))

        for (i in 1:read) {
            p <- read.profile.aquadopp(file,debug=debug)
            for (beam in 1:number.of.beams) {
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
        if (monitor) cat("\nRead", read, "profiles\n")
        salinity <- rep(salinity, read)     # fake a time-series
        class(time) <- c("POSIXt", "POSIXct")
        attr(time, "tzone") <- "UTC"        # BUG should let user control this
                                        # Q: does file hold the zone?

        data <- list(ma=list(v=v, a=a, q=q),
                     ss=list(distance=seq(blanking.distance, by=cell.size, length.out=number.of.cells)),
                     ts=list(time=time,
                     pressure=pressure,
                     temperature=temperature,
                     salinity=salinity,
                     heading=heading,
                     pitch=pitch,
                     roll=roll)
                     )

    } else {
        data <- list(ma=NULL, ss=NULL, ts=NULL)
    }
    metadata <- list(
                     filename=filename,
                     size=size,
                     hardware.serial.number=hardware.serial.number,
                     frequency=frequency,
                     internal.code.version=pic.version,
                     hardware.revision=hw.revision,
                     rec.size=rec.size,
                     velocity.range=velocity.range,
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
                     coordinate.system=coordinate.system,
                     salinity=salinity
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
                rec.size=object$metadata$rec.size*65536/1024/1024,
                velocity.range=object$metadata$velocity.range,
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
    cat("    Recorder size:              ", x$rec.size, "Mb\n")
    cat("    Firmware version:           ", x$firmware.version, "\n")
    cat("    Velocity range:             ", if(x$velocity.range) "high\n" else "normal\n")
    cat("  Head Configuration\n")
    cat("    Pressure sensor:            ", if (x$config.pressure.sensor) "yes\n" else "no\n")
    cat("    Compass:                    ", if (x$config.magnetometer.sensor) "yes\n" else "no\n")
    cat("    Tilt sensor:                ", if (x$config.tilt.sensor) "yes\n" else "no\n")
    if (FALSE) cat("    System 1 and 2:              [not coded yet]\n")
    cat("    Frequency:                  ", x$frequency, "kHz\n")
    cat("    Serial number:              ", x$head.serial.number, "\n")
    if (FALSE) cat("    Transformation matrix:       [not coded yet]\n")
    if (FALSE) cat("    Pressure sensor calibration: [not coded yet]\n")
    cat("    Number of beams:            ", x$number.of.beams, "\n")
    if (FALSE) cat("    System 5 through 20:         [not coded yet]\n")
    cat("  User Setup\n")
    cat("    Measurement/burst interval: ", x$measurement.interval, "s\n")
    cat("    Cell size                   ", x$cell.size, "\n")
    ##cat("    *** above should be 0.04m ***\n")
    cat("    Orientation:                ", if (x$config.downward.looking) "downward-looking\n" else "upward-looking\n")
    cat("    Velocity scale:             ", x$velocity.scale, "m/s\n")
    cat("    Coordinate system:          ", x$coordinate.system, "\n")
    if (FALSE) cat("
? Distance to bottom                    1.00 m
? Extended velocity range               ON
? Pulse distance (Lag1)                 1.10 m
? Pulse distance (Lag2)                 0.36 m
? Profile range                         1.00 m
? Horizontal velocity range             0.84 m/s
? Vertical velocity range               0.35 m/s
")
    cat("    Number of cells:            ", x$number.of.cells, "\n")
    if (FALSE) cat("
? Average interval                      10 sec
? Blanking distance                     0.05 m
")
    cat("    Blanking distance:          ", x$blanking.distance, "\n")
    ##cat("    *** above should be 0.05m ***\n")
    if (FALSE) cat("
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
    print(x$processing.log)
    invisible(x)
}

plot.aquadopp <- function(x, which=1:3, col=oce.colors.palette(128, 1), zlim,
                          titles,
                          adorn=NULL,
                          draw.timerange=getOption("oce.draw.timerange"),
                          mgp=getOption("oce.mgp"), ...)
{
    if (!inherits(x, "aquadopp")) stop("method is only for aquadopp objects")
    opar <- par(no.readonly = TRUE)
    lw <- length(which)
    if (!missing(titles) && length(titles) != lw) stop("length of 'titles' must equal length of 'which'")
    if (lw > 1) on.exit(par(opar))
    par(mgp=mgp)
    dots <- list(...)

    gave.zlim <- !missing(zlim)
    zlim.given <- if (gave.zlim) zlim else NULL
    gave.ylim <- "ylim" %in% names(dots)
    ylim.given <- if (gave.ylim) dots[["ylim"]] else NULL

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

    if (!gave.zlim && all(which %in% 5:7)) { # amplitude uses a single scale for all
        zlim <- range(abs(x$data$ma$a[,,which[1]-4]), na.rm=TRUE)
        for (w in 2:length(which)) {
            zlim <- range(abs(c(zlim, x$data$ma$a[,,which[w]-4])), na.rm=TRUE)
        }
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
        if (which[w] %in% images) {                   # image types
            ## note that 4 and 8 cannot be used
            skip <- FALSE
            if (which[w] %in% 1:3) {    #velocity
                z <- x$data$ma$v[,,which[w]]
                y.look <- if (gave.ylim)
                    ylim.given[1] <= x$data$ss$distance & x$data$ss$distance <= ylim.given[2]
                else rep(TRUE, length(x$data$ss$distance))
                zlim <- if (gave.zlim) zlim.given else max(abs(x$data$ma$v[,y.look,which[w]]), na.rm=TRUE) * c(-1,1)
                if (x$metadata$coordinate.system == "beam")
                    zlab <- if (missing(titles)) c("bm1", "bm2", "bm3")[which[w]] else titles[w]
                else if (x$metadata$coordinate.system == "earth")
                    zlab <- if (missing(titles)) c("east", "north", "up")[which[w]] else titles[w]
                else if (x$metadata$coordinate.system == "frame")
                    zlab <- if (missing(titles)) c("u", "v", "w")[which[w]] else titles[w]
                else zlab <- "?"
            } else if (which[w] %in% 5:7) { # amplitude
                z <- x$data$ma$a[,,which[w]-4]
                y.look <- if (gave.ylim)
                    ylim.given[1] <= x$data$ss$distance & x$data$ss$distance <= ylim.given[2]
                else rep(TRUE, length(x$data$ss$distance))
                zlim <- range(x$data$ma$a[,y.look,], na.rm=TRUE)
                zlab <- c(expression(a[1]),expression(a[2]),expression(a[3]))[which[w]-4]
            } else if (which[w] %in% 9:11) { # correlation
                z <- x$data$ma$q[,,which[w]-8]
                zlim <- c(0, 100)
                zlab <- c(expression(q[1]),expression(q[2]),expression(q[3]))[which[w]-8]
            } else skip <- TRUE
            if (!skip) {
                imagep(x=tt, y=x$data$ss$distance, z=z,
                       zlim=zlim,
                       col=col,
                       ylab=resizable.label("distance"),
                       xlab="Time",
                       zlab=zlab,
                       draw.time.range=!shown.time.interval,
                       draw.contours=FALSE,
                       do.layout=FALSE,
                       ...)
                shown.time.interval <- TRUE
            }
        }
        if (which[w] %in% timeseries) { # time-series types
            if (which[w] == 13) {
                if (any(!is.na(x$data$ts$salinity)))
                    plot(x$data$ts$time, x$data$ts$salinity,    ylab="S [psu]",       type='l', axes=FALSE)
                else {
                    warning("cannot plot panel which=13 because there are no salinity data")
                    next
                }
            }
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
