read.header <- function(file, debug)
{
    ##
    ## header, of length 6 + 2 * number.of.data.types bytes
    ##

    ##cat("before reading header.part1, ftell=", seek(file),"\n")

    header.part1 <- readBin(file, "raw", n=6, size=1)
    if (debug > 1)
        cat("First 6 bytes of header:", paste(header.part1, sep=' '), "\n")
    header <- header.part1
    if (header.part1[1] != 0x7f) stop("first byte in file must be 0x7f, but it was", header.part1[1])
    if (header.part1[2] != 0x7f) stop("second byte in file must be 0x7f but it was", header.part1[2])
    num.bytes.in.ensemble <- readBin(header.part1[3:4], "integer", n=1, size=2, endian="little")
    if (debug) cat("num.bytes.in.ensemble=", num.bytes.in.ensemble,"\n")
    ## header.part1[5] spare
    number.of.data.types <- readBin(header.part1[6], "integer", n=1, size=1)
    if (number.of.data.types < 1) stop("cannot have ", number.of.data.types, " data types, as header indicates")
    if (debug) cat("number.of.data.types=", number.of.data.types, "\n")
    ## part 2 of header is these data offsets
    header.part2 <- readBin(file, "raw", n=2*number.of.data.types, size=1)

    ##cat("after reading header, ftell=", seek(file), "\n")

    header <- c(header, header.part2)
    data.offset <- readBin(header.part2, "integer", n=number.of.data.types, size=2, endian="little")
    if (debug) cat("data.offset=", paste(data.offset, sep=" "), "\n")
    ##
    ## FLD (fixed leader data) 59 bytes
    ##
    FLD <- readBin(file, "raw", n=59, size=1) # binary fixed leader data (Figure D-5)

    ##cat("after reading FLD, ftell=", seek(file), "\n")

    header <- c(header, FLD)
    if (debug > 1) {
        cat("fixed leader data (59 bytes):\n")
        print(FLD)
    }
    if (FLD[1] != 0x00) stop("first byte of fixed leader header must be 0x00 but it was ", FLD[1])
    if (FLD[2] != 0x00) stop("second byte of fixed leader header must be 0x00 but it was ", FLD[2])
    fv <- readBin(FLD[3], "integer", n=1, size=1)
    fr <- readBin(FLD[4], "integer", n=1, size=1)
    ##program.version <- paste(fv, fr, sep=".") # don't want to rely on number of digits
    ##if (debug) cat("program version=", program.version, "\n")
##    system.configuration <- readBin(FLD[5:6], "raw", n=2, size=1)
    system.configuration <- paste(byte2binary(FLD[5]),
                                  byte2binary(FLD[6]),sep="-")
    bits <- substr(system.configuration, 6, 8)
    if (bits == "000") kHz <- 75
    else if (bits == "001") kHz <-  150
    else if (bits == "010") kHz <-  300
    else if (bits == "011") kHz <-  600
    else if (bits == "100") kHz <- 1200
    else if (bits == "101") kHz <- 2400
    bits <- substr(system.configuration, 16, 17)
    if (bits == "00") beam.angle <- 15
    else if (bits == "01") beam.angle <- 20
    else if (bits == "10") beam.angle <- 30
    else if (bits == "11") beam.angle <- NA # means 'other'
    bits <- substr(system.configuration, 5, 5)
    if (bits == "0") beam.pattern <- "concave"
    else beam.pattern <- "convex"
    ##cat("BITS='",bits,"'\n",sep="")
    beam.config <- "?"
    bits <- substr(system.configuration, 10, 13)
    if (bits == "0100") beam.config <- "janus"
    else if (bits == "0101") beam.config <- "janus demod"
    else if (bits == "1111") beam.config <- "janus 2 demd"
    bits <- substr(system.configuration, 1, 1)
    if (bits == "1") orientation <- "up"
    else orientation <- "down"
    ##cat("beam.config=", beam.config, "\n")
    real.sim.flag <- readBin(FLD[7], "integer", n=1, size=1)
    lag.length <- readBin(FLD[8], "integer", n=1, size=1)
    number.of.beams <- readBin(FLD[9], "integer", n=1, size=1)
    number.of.cells <- readBin(FLD[10], "integer", n=1, size=1) # WN
    pings.per.ensemble <- readBin(FLD[11:12], "integer", n=1, size=2, endian="little")
    depth.cell.length <- readBin(FLD[13:14], "integer", n=1, size=2, endian="little") / 100 # WS in m
    if (depth.cell.length < 0 || depth.cell.length > 64) stop("depth cell length of ", depth.cell.length, "is not in the allowed range of 0m to 64m")
    blank.after.transmit <- readBin(FLD[15:16], "integer", n=1, size=2, endian="little") / 100 # in m
    profiling.mode <- readBin(FLD[17], "integer", n=1, size=1) # WM
    low.corr.thresh <- readBin(FLD[18], "integer", n=1, size=1)
    number.of.code.reps <- readBin(FLD[19], "integer", n=1, size=1)
    percent.gd.minimum <- readBin(FLD[20], "integer", n=1, size=1)
    error.velocity.maximum <- readBin(FLD[21:22], "integer", n=1, size=2, endian="little")
    tpp.minutes <- readBin(FLD[23], "integer", n=1, size=1)
    tpp.seconds <- readBin(FLD[24], "integer", n=1, size=1)
    tpp.hundredths <- readBin(FLD[25], "integer", n=1, size=1)
    bits <- substr(byte2binary(FLD[26]), 4, 5)
    coordinate.transformation <- "???"
    if (bits == "00") coordinate.transformation <- "beam"
    else if (bits == "01") coordinate.transformation <- "instrument"
    else if (bits == "10") coordinate.transformation <- "ship"
    else if (bits == "11") coordinate.transformation <- "earth"
    heading.alignment <- readBin(FLD[27:28], "integer", n=1, size=2, endian="little")
    heading.bias <- readBin(FLD[29:30], "integer", n=1, size=2, endian="little")
    sensor.source <- readBin(FLD[31], "integer", n=1, size=1)
    sensors.available <- readBin(FLD[32], "integer", n=1, size=1)
    bin1.distance <- readBin(FLD[33:34], "integer", n=1, size=2, endian="little") * 0.01
    xmit.pulse.length <- readBin(FLD[35:36], "integer", n=1, size=2, endian="little")
    wp.ref.layer.average <- readBin(FLD[37:38], "integer", n=1, size=2, endian="little")
    false.target.thresh <- readBin(FLD[39], "integer", n=1, size=1)
    ## FLD[40] spare
    transmit.lag.distance <- readBin(FLD[41:42], "integer", n=1, size=2, endian="little")
    cpu.board.serial.number <- c(readBin(FLD[43], "integer", n=1, size=1, signed=FALSE),
                                 readBin(FLD[44], "integer", n=1, size=1, signed=FALSE),
                                 readBin(FLD[45], "integer", n=1, size=1, signed=FALSE),
                                 readBin(FLD[46], "integer", n=1, size=1, signed=FALSE),
                                 readBin(FLD[47], "integer", n=1, size=1, signed=FALSE),
                                 readBin(FLD[48], "integer", n=1, size=1, signed=FALSE),
                                 readBin(FLD[49], "integer", n=1, size=1, signed=FALSE),
                                 readBin(FLD[50], "integer", n=1, size=1, signed=FALSE))
    if (debug) cat("CPU.BOARD.SERIAL.NUMBER = '", FLD[43:50], "'\n", sep="")
    if (debug) cat("CPU.BOARD.SERIAL.NUMBER = '", cpu.board.serial.number, "'\n", sep="")
    system.bandwidth <- readBin(FLD[51:52], "integer", n=1, size=2, endian="little")
    system.power <- readBin(FLD[53], "integer", n=1, size=1)
    ## FLD[54] spare
    instrument.serial.number <- readBin(FLD[55:58], "integer", n=1, size=4, endian="little")
    ##cat("INSTRUMENT SERIAL NUMBER", FLD[55:58], "\n")

    ##beam.angle <- readBin(FLD[59], "integer", n=1, size=1) # NB 0 in first test case
    ##cat("BEAM ANGLE=", FLD[59], "or", beam.angle, "\n")

    ##
    ## VLD (variable leader data) 65 bytes
    ##

    ##cat("before reading VLD (65 bytes), ftell=", seek(file), "\n")

    VLD <- readBin(file, "raw", n=65, size=1)
    header <- c(header, VLD)
    ##cat("position in file=", seek(file, NA), "after reading VLD\n")
    if (debug > 1) {
        cat("variable leader data (65 bytes):\n")
        print(VLD)
    }
    ## ensure that header is not ill-formed
    if (VLD[1] != 0x80) stop("byte 1 of variable leader data should be 0x80, but it is ", VLD[1])
    if (VLD[2] != 0x00) stop("byte 1 of variable leader data should be 0x00, but it is ", VLD[2])
    ensemble.number <- readBin(VLD[3:4], "integer", n=1, size=2, endian="little")
    RTC.year <- readBin(VLD[5], "integer", n=1, size=1)
    if (RTC.year < 1800) RTC.year <- RTC.year + 2000 # fix Y2K problem
    RTC.month <- readBin(VLD[6], "integer", n=1, size=1)
    RTC.day <- readBin(VLD[7], "integer", n=1, size=1)
    RTC.hour <- readBin(VLD[8], "integer", n=1, size=1)
    RTC.minute <- readBin(VLD[9], "integer", n=1, size=1)
    RTC.second <- readBin(VLD[10], "integer", n=1, size=1)
    RTC.hundredths <- readBin(VLD[11], "integer", n=1, size=1)
    RTC.time <- ISOdatetime(RTC.year, RTC.month, RTC.day, RTC.hour, RTC.minute, RTC.second + RTC.hundredths / 100, tz = "GMT") # not sure on TZ
    ensemble.number.MSB <- readBin(VLD[12], "integer", n=1, size=1)
    bit.result <- readBin(VLD[13:14], "integer", n=1, size=2, endian="little")
    speed.of.sound  <- readBin(VLD[15:16], "integer", n=1, size=2, endian="little")
    if (speed.of.sound < 1400 || speed.of.sound > 1600) stop("speed of sound is ", speed.of.sound, ", which is outside the permitted range of 1400 m/s to 1600 m/s")
    depth.of.transducer <- readBin(VLD[17:18], "integer", n=1, size=2, endian="little") * 0.1
    heading <- readBin(VLD[19:20], "integer", n=1, size=2, endian="little") * 0.01
    pitch <- readBin(VLD[21:22], "integer", n=1, size=2, endian="little") * 0.01
    roll <- readBin(VLD[23:24], "integer", n=1, size=2, endian="little") * 0.01
    salinity <- readBin(VLD[25:26], "integer", n=1, size=2, endian="little")
    if (salinity < 0 || salinity > 40) stop("salinity is ", salinity, ", which is outside the permitted range of 0 to 40 PSU")
    temperature <- readBin(VLD[27:28], "integer", n=1, size=2, endian="little") * 0.01
    if (temperature < -5 || temperature > 40) stop("temperature is ", temperature, ", which is outside the permitted range of -5 to 40 degC")

    ## Skipping a lot ...
    pressure <- readBin(VLD[49:52], "integer", n=1, size=4, endian="little", signed=FALSE) * 0.001

    if (0) {#debug) {
        cat("PRESSURE: ", VLD[49:52], "\n")
        cat(" little/signed   ", readBin(VLD[49:52], "integer", n=1, size=4, endian="little", signed=TRUE),"\n")
        cat(" little/unsigned ", readBin(VLD[49:52], "integer", n=1, size=4, endian="little", signed=FALSE),"\n")
        cat(" big/signed      ", readBin(VLD[49:52], "integer", n=1, size=4, endian="big",    signed=TRUE),"\n")
        cat(" big/unsigned    ", readBin(VLD[49:52], "integer", n=1, size=4, endian="big",    signed=FALSE),"\n")
        cat("\nbyte by byte:")
        cat(readBin(VLD[49], "integer", n=1, size=1, signed=FALSE), " ")
        cat(readBin(VLD[50], "integer", n=1, size=1, signed=FALSE), " ")
        cat(readBin(VLD[51], "integer", n=1, size=1, signed=FALSE), " ")
        cat(readBin(VLD[52], "integer", n=1, size=1, signed=FALSE), " ")
        cat("\n")
    }

    list(header=header,
         header.length=length(header),
         program.version.major=fv,
         program.version.minor=fr,
         ##program.version=program.version,
         system.configuration=system.configuration,
         kHz=kHz,
         beam.angle=beam.angle,
         beam.pattern=beam.pattern,
         beam.config=beam.config,
         orientation=orientation,
         number.of.data.types=number.of.data.types,
         data.offset=data.offset,
         number.of.beams=number.of.beams,
         number.of.cells=number.of.cells,
         heading=heading,
         pitch=pitch,
         roll=roll,
         pings.per.ensemble=pings.per.ensemble,
         depth.cell.length=depth.cell.length,
         profiling.mode=profiling.mode,
         low.corr.thresh=low.corr.thresh,
         number.of.code.reps=number.of.code.reps,
         percent.gd.minimum=percent.gd.minimum,
         error.velocity.maximum=error.velocity.maximum,
         tpp.minutes=tpp.minutes,
         tpp.seconds=tpp.seconds,
         tpp.hundredths=tpp.hundredths,
         coordinate.transformation=coordinate.transformation,
         heading.alignment=heading.alignment,
         heading.bias=heading.bias,
         sensor.source=sensor.source,
         sensors.available=sensors.available,
         bin1.distance=bin1.distance,
         xmit.pulse.length=xmit.pulse.length,
         wp.ref.layer.average=wp.ref.layer.average,
         false.target.thresh=false.target.thresh,
         transmit.lag.distance=transmit.lag.distance,
         cpu.board.serial.number=cpu.board.serial.number,
         system.bandwidth=system.bandwidth,
         system.power=system.power,
         instrument.serial.number=instrument.serial.number,
         ## beam.angle=beam.angle,  # wrong in my tests, anyway
         ensemble.number=ensemble.number,
         RTC.year=RTC.year,
         RTC.month=RTC.month,
         RTC.day=RTC.day,
         RTC.hour=RTC.hour,
         RTC.minute=RTC.minute,
         RTC.second=RTC.second,
         RTC.hundredths=RTC.hundredths,
         RTC.time=RTC.time,
         ensemble.number.MSB=ensemble.number.MSB,
         bit.result=bit.result,
         speed.of.sound=speed.of.sound,
         depth.of.transducer=depth.of.transducer,
         heading=heading,
         pitch=pitch,
         roll=roll,
         salinity=salinity,             # seems to be constant
         temperature=temperature,
         pressure=pressure  # FIXME: -244 in SLEIWEX data SL08F001.000
         )
}


read.profile <- function(file, header, debug)
{
    if (missing(header)) {
        header <- read.header(file, debug=debug)
    } else {
        junk <- readBin(file, "raw", n=header$header.length, size=1)
    }
    ## velocity, should start with 0x00 then 0x01
    v.ID <- readBin(file, "raw", n=2, size=1)
    if (v.ID[1] != 0x00) stop("first byte of velocity segment should be 0x00 but is ", v.ID[1])
    if (v.ID[2] != 0x01) stop("first byte of velocity segment should be 0x01 but is ", v.ID[2])
    if (debug) cat("got velo\n")
    v <- readBin(file, "integer",
                 n=header$number.of.beams * header$number.of.cells,
                 size=2, endian="little")
    v[v==(-32768)] <- NA       # blank out bad data
    v <- matrix(v / 1000, ncol=header$number.of.beams, byrow=TRUE)
    ## correlation magnitude, should start with 0x00 0x01
    cm.ID <- readBin(file, "raw", n=2, size=1)
    if (cm.ID[1] != 0x00) stop("first byte of correlation-magnitude segment should be 0x00 but is ", cm.ID[1])
    if (cm.ID[2] != 0x02) stop("first byte of correlation-magnitude segment should be 0x02 but is ", cm.ID[2])
    cm <- readBin(file, "integer",
                  n=header$number.of.beams * header$number.of.cells,
                  size=1, endian="little")
    if (debug) cat("got correlation magnitude\n")
    ## echo intensity, should start with 0x00 0x03
    ei.ID <- readBin(file, "raw", n=2, size=1)
    if (ei.ID[1] != 0x00) stop("first byte of echo-intensity segment should be 0x00 but is ", ei.ID[1])
    if (ei.ID[2] != 0x03) stop("first byte of echo-intensity segment should be 0x03 but is ", ei.ID[2])
    ei <- readBin(file, "integer",
                  n=header$number.of.beams * header$number.of.cells,
                  size=1, endian="little", signed=FALSE)
    ei <- matrix(ei, ncol=header$number.of.beams, byrow=TRUE)

    if (debug) cat("got echo intensity\n")
    ## percent good, should start with 0x00 0x04
    pg.ID <- readBin(file, "raw", n=2, size=1)
    if (pg.ID[1] != 0x00) stop("first byte of percent-good segment should be 0x00 but is ", pg.ID[1])
    if (pg.ID[2] != 0x04) stop("first byte of percent-good segment should be 0x04 but is ", pg.ID[2])
    pg <- readBin(file, "integer",
                  n=header$number.of.beams * header$number.of.cells,
                  size=1, endian="little", signed=FALSE)
    pg <- matrix(pg, ncol=header$number.of.beams, byrow=TRUE)
    if (debug) cat("got percent-good\n")
    bt <- NULL
    if (header$number.of.data.types > 6) {
        stop("not yet coded to read bottom track")
        ## need to check if bottom track.  I think that's only if
        ## num.data.types exceeds 6
        ## bottom track, 85 bytes (always present??) ID=0x00 0x06
        bt <- NULL
    }
    reserved <- readBin(file, "raw", n=2, size=1)
    if (debug > 0) cat("reserved=", paste(reserved, collapse=" "), "\n")
    checksum <- readBin(file, "raw", n=2, size=1)
    if (debug > 0) cat("checksum=", paste(checksum, collapse=" "), "\n")
    list(header=header, v=v, ei=ei, pg=pg, bt=bt)
}

read.adcp <- function(file, type ="RDI",
                      skip=0, read,
                      debug=0,
                      monitor=TRUE,
                      log.action)
{
    if (is.character(file)) {
        filename <- file
        file <- file(file, "rb")
        on.exit(close(file))
    }
    if (!inherits(file, "connection")) {
        stop("argument `file' must be a character string or connection")
    }
    if (!isOpen(file)) {
        filename <- "(connection)"
        open(file, "rb")
        on.exit(close(file))
    }
    ## read a profile, to get length so we can seek
    p <- read.profile(file, debug=debug)
    bytes.per.profile <- seek(file)
    seek(file, where=bytes.per.profile * skip)
    if (read < 1) stop("cannot read fewer than one profile")
    b1 <- array(dim=c(read, p$header$number.of.cells))
    b2 <- array(dim=c(read, p$header$number.of.cells))
    b3 <- array(dim=c(read, p$header$number.of.cells))
    b4 <- array(dim=c(read, p$header$number.of.cells))
    ei1 <- array(dim=c(read, p$header$number.of.cells)) # echo intensity
    ei2 <- array(dim=c(read, p$header$number.of.cells))
    ei3 <- array(dim=c(read, p$header$number.of.cells))
    ei4 <- array(dim=c(read, p$header$number.of.cells))
##    pg1 <- array(dim=c(read, p$header$number.of.cells))
##    pg2 <- array(dim=c(read, p$header$number.of.cells))
##    pg3 <- array(dim=c(read, p$header$number.of.cells))
##    pg4 <- array(dim=c(read, p$header$number.of.cells))
    time <- pressure <- temperature <- salinity <- depth.of.transducer <- heading <- pitch <- roll <- NULL
    for (i in 1:read) {
        p <- read.profile(file,debug=debug)
        b1[i,] <- p$v[,1]
        b2[i,] <- p$v[,2]
        b3[i,] <- p$v[,3]
        b4[i,] <- p$v[,4]
        ei1[i,] <- p$ei[,1]
        ei2[i,] <- p$ei[,2]
        ei3[i,] <- p$ei[,3]
        ei4[i,] <- p$ei[,4]
##        pg1[i,] <- p$pg[,1]
##        pg2[i,] <- p$pg[,2]
##        pg3[i,] <- p$pg[,3]
##        pg4[i,] <- p$pg[,4]
        time <- c(time, p$header$RTC.time)
        pressure <- c(pressure, p$header$pressure)
        temperature <- c(temperature, p$header$temperature)
        salinity <- c(salinity, p$header$salinity)
        depth.of.transducer <- c(depth.of.transducer, p$header$depth.of.transducer)
        heading <- c(heading, p$header$heading)
        pitch <- c(pitch, p$header$pitch)
        roll <- c(roll, p$header$roll)
        if (i == 1) metadata <- c(p$header,
            filename=filename,
            number.of.profiles=read,
            oce.coordinate="beam")
        if (monitor) {
            cat(".")
            if (!(i %% 50)) cat(i, "\n")
        }
    }
    ##cat("\nfivenum(ei1,na.rm=TRUE)"); print(fivenum(ei1, na.rm=TRUE))
    class(time) <- c("POSIXt", "POSIXct")
    attr(time, "tzone") <- attr(p$header$RTC.time, "tzone")
    data <- list(b1=b1, b2=b2, b3=b3, b4=b4,
                 ei1=ei1, ei2=ei2, ei3=ei3, ei4=ei4,
                 ##pg1=ETC,
                 time=time,
                 distance=seq(p$header$bin1.distance, by=p$header$depth.cell.length, length.out=p$header$number.of.cells),
                 pressure=pressure,
                 temperature=temperature,
                 salinity=salinity,
                 depth.of.transducer=depth.of.transducer,
                 heading=heading,
                 pitch=pitch,
                 roll=roll
                 )                     # FIXME: this is too hard-wired
    if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processing.log.item(log.action)
    res <- list(data=data, metadata=metadata, processing.log=log.item)
    class(res) <- c("adcp", "oce")
    res
}

summary.adcp <- function(object, ...)
{
    if (!inherits(object, "adcp")) stop("method is only for adcp objects")
    names <- names(object$data)
    names <- names[names != "time"]     # do not report stats on time column
    fives <- matrix(nrow=length(names), ncol=5)
    for (i in 1:length(names)) {
        ##cat("DEBUG: doing fivenum for name=", names[i], "\n")
        fives[i,] <- fivenum(object$data[[names[i]]], na.rm=TRUE)
    }
    rownames(fives) <- names
    colnames(fives) <- c("Min.", "1st Qu.", "Median", "3rd Qu.", "Max.")
    res <- list(filename=object$metadata$filename,
                start.time=object$data$time[1],
                delta.time=difftime(object$data$time[2], object$data$time[1], units="secs"),
                number.of.profiles=object$metadata$number.of.profiles,
                metadata=object$metadata,
                kHz=object$metadata$kHz,
                number.of.data.types=object$metadata$number.of.data.types,
                number.of.beams=object$metadata$number.of.beams,
                beam.angle=object$metadata$beam.angle,
                beam.config=object$metadata$beam.config,
                orientation=object$metadata$orientation,
                beam.pattern=object$metadata$beam.pattern,
                coordinate.transformation=object$metadata$coordinate.transformation,
                fives=fives,
                oce.coordinate=object$metadata$oce.coordinate,
                number.of.profiles=object$metadata$number.of.profiles,
                processing.log=processing.log.summary(object))
    class(res) <- "summary.adcp"
    res
}

print.summary.adcp <- function(x, digits=max(6, getOption("digits") - 1), ...)
{
    cat("ADCP timeseries\n")
    cat("  Filename:                   ", x$filename, "\n")
    cat("  Software version:           ", paste(x$metadata$program.version.major, x$metadata$program.version.minor, sep="."), "\n")
    cat("  System configuration:       ", x$metadata$system.configuration, "\n")
    cat("  CPU board serial number:    ", x$metadata$cpu.board.serial.number, "\n")
    cat("  Instrument serial number:   ", x$metadata$instrument.serial.number, "\n")
    cat("  Coordinate transformation:  ", x$coordinate.transformation, "[originally],",
        x$oce.coordinate, "[presently]\n")
    cat("  Transducer depth:           ", x$metadata$depth.of.transducer, "m\n")
##    cat("  Pressure:                   ", x$metadata$pressure*0.01, "dbar (in first record)\n")
##    cat("  Salinity:                   ", x$metadata$salinity, "PSU (in first record)\n")
##    cat("  Temperature:                ", x$metadata$temperature, "degC (in first record)\n")
    cat("  Sampling\n",
        "    First profile at:     ", as.character(x$start.time), "\n",
        "    Time between profiles:", x$delta.time, "sec\n",
        "    Number of data types: ", x$number.of.data.types, "\n",
        "    Frequency:            ", x$kHz, "kHz\n",
        paste("    Beams:                 ",
              "", x$number.of.beams, " beams",
              ", ", x$beam.config, " configuration",
              ", ", x$beam.pattern," geometry, directed ",
              x$orientation,"wards",
              ", ", x$beam.angle, " deg angle to the vertical.\n", sep=""),
        paste("    Cells:                ",
              " number=", x$metadata$number.of.cells,
              "           size=", x$metadata$depth.cell.length, "m",
              "  dist. to first=", x$metadata$bin1.dist,"m\n", sep=""),
        "    Pings per ensemble:   ", x$metadata$pings.per.ensemble, "\n",
        "    Number of profiles:   ", x$number.of.profiles, "\n"
        )
    cat("\nStatistics:\n")
    print(x$fives)
    cat("\n")
    print(x$processing.log)
    invisible(x)
}

plot.adcp <- function(x, which=1:4, col=oce.colors.two(128), zlim, ...)
{
    lw <- length(which)
    if (lw > 1) {
        oldpar <- par(no.readonly = TRUE)
        par(mfrow = c(lw, 1))
    }
    if (!"mgp" %in% names(list(...))) par(mgp = getOption("oce.mgp"))
    mgp <- par("mgp")
    par(mar=c(mgp[1],mgp[1]+1,1,1))
    data.names <- names(x$data)
    ##dots <- names(list(...))
    ##gave.zlim <- "zlim" %in% dots
    shown.time.interval <- FALSE
    tt <- x$data$time
    class(tt) <- "POSIXct"              # otherwise image() gives warnings
    zlim.not.given <- missing(zlim)
    if (zlim.not.given && all(which %in% 5:8)) { # ei uses a single scale for all
        zlim <- range(abs(x$data[[which[1]]]), na.rm=TRUE)
        for (w in 2:length(which)) {
            zlim <- range(abs(c(zlim, x$data[[which[w]]])), na.rm=TRUE)
        }
        zlim.not.given <- FALSE                                    # fake it
    }
    for (w in 1:length(which)) {
        if (zlim.not.given)
            zlim <- max(abs(x$data[[which[w]]]), na.rm=TRUE) * c(-1,1)
        if (which[w] == 1) {
            image(x=tt, y=x$data$distance, z=x$data[[1]],
                  xlab="Time", ylab="Distance", axes=FALSE,
                  zlim=zlim,
                  col=col, ...)
            axis.POSIXct(1, x=x$data$time)
            box()
            axis(2)
            ##mtext(data.names[1], side=3, cex=2/3, adj=1)
            mtext(paste(data.names[1], " [colours for ", round(zlim[1], 2), " to ", round(zlim[2], 2), "]", sep=""), side=3, cex=2/3, adj=1)
        }
        if (which[w] == 2) {
            image(x=tt, y=x$data$distance, z=x$data[[2]],
                  xlab="Time", ylab="Distance", axes=FALSE,
                  zlim=zlim,
                  col=col, ...)
            axis.POSIXct(1, x=x$data$time)
            box()
            axis(2)
            mtext(paste(data.names[2], " [colours for ", round(zlim[1], 2), " to ", round(zlim[2], 2), "]", sep=""), side=3, cex=2/3, adj=1)
        }
        if (which[w] == 3) {
            image(x=tt, y=x$data$distance, z=x$data[[3]],
                  xlab="Time", ylab="Distance", axes=FALSE,
                  zlim=zlim,
                  col=col, ...)
            axis.POSIXct(1, x=x$data$time)
            box()
            axis(2)
            mtext(paste(data.names[3], " [colours for ", round(zlim[1], 2), " to ", round(zlim[2], 2), "]", sep=""), side=3, cex=2/3, adj=1)
        }
        if (which[w] == 4) {
            image(x=tt, y=x$data$distance, z=x$data[[4]],
                  xlab="Time", ylab="Distance", axes=FALSE,
                  zlim=zlim,
                  col=col, ...)
            axis.POSIXct(1, x=x$data$time)
            box()
            axis(2)
            mtext(paste(data.names[4], " [colours for ", round(zlim[1], 2), " to ", round(zlim[2], 2), "]", sep=""), side=3, cex=2/3, adj=1)
        }
        if (which[w] == 5) {            # ei1
            image(x=tt, y=x$data$distance, z=x$data$ei1,
                  xlab="Time", ylab="Distance", axes=FALSE,
                  zlim=zlim,
                  col=col, ...)
            axis.POSIXct(1, x=x$data$time)
            box()
            axis(2)
            mtext(paste("ei1 [colours for ", round(zlim[1], 2), " to ", round(zlim[2], 2), "]", sep=""), side=3, cex=2/3, adj=1)
        }
        if (which[w] == 6) {            # ei2
            image(x=tt, y=x$data$distance, z=x$data$ei2,
                  xlab="Time", ylab="Distance", axes=FALSE,
                  zlim=zlim,
                  col=col, ...)
            axis.POSIXct(1, x=x$data$time)
            box()
            axis(2)
            mtext(paste("ei2 [colours for ", round(zlim[1], 2), " to ", round(zlim[2], 2), "]", sep=""), side=3, cex=2/3, adj=1)
        }
        if (which[w] == 7) {            # ei3
            image(x=tt, y=x$data$distance, z=x$data$ei3,
                  xlab="Time", ylab="Distance", axes=FALSE,
                  zlim=zlim,
                  col=col, ...)
            axis.POSIXct(1, x=x$data$time)
            box()
            axis(2)
            mtext(paste("ei3 [colours for ", round(zlim[1], 2), " to ", round(zlim[2], 2), "]", sep=""), side=3, cex=2/3, adj=1)
        }
        if (which[w] == 8) {            # ei4
            image(x=tt, y=x$data$distance, z=x$data$ei4,
                  xlab="Time", ylab="Distance", axes=FALSE,
                  zlim=zlim,
                  col=col, ...)
            axis.POSIXct(1, x=x$data$time)
            box()
            axis(2)
            mtext(paste("ei4 [colours for ", round(zlim[1], 2), " to ", round(zlim[2], 2), "]", sep=""), side=3, cex=2/3, adj=1)
        }
        if (!shown.time.interval) {
            mtext(paste(format(range(x$data$time)), collapse=" to "), side=3, cex=2/3, adj=0)
            shown.time.interval <- TRUE
        }
    }
    if (lw > 1) par(oldpar)
}

adcp.beam.attenuate <- function(x, count2db=c(0.45, 0.45, 0.45, 0.45))
{
    res <- x
    num.profiles <- dim(x$data$ei1)[1]
    ##print(num.profiles)
    correction <- matrix(rep(20 * log10(x$data$distance), num.profiles), nrow=num.profiles, byrow=TRUE)
    ##print(dim(x$data$ei1))
    ##print(dim(correction))
    res$data$ei1 <- count2db[1] * x$data$ei1 + correction
    res$data$ei2 <- count2db[2] * x$data$ei2 + correction
    res$data$ei3 <- count2db[3] * x$data$ei3 + correction
    res$data$ei4 <- count2db[4] * x$data$ei4 + correction
    log.action <- paste(deparse(match.call()), sep="", collapse="")
    res <- processing.log.append(res, log.action)
}

adcp.beam2frame <- function(x)
{
    if (!inherits(x, "adcp")) stop("method is only for objects of class 'adcp'")
    dim <- dim(x$data$b1)
    u <- array(dim=dim)
    v <- array(dim=dim)
    w <- array(dim=dim)
    e <- array(dim=dim)
    c <- if(x$metadata$beam.pattern == "convex") 1 else -1;
    a <- 1 / (2 * sin(x$metadata$beam.angle * pi / 180))
    b <- 1 / (4 * cos(x$metadata$beam.angle * pi / 180))
    d <- a / sqrt(2)
    u <- c * a * (x$data$b1 - x$data$b2)
    v <- c * a * (x$data$b4 - x$data$b3)
    w <- b * (x$data$b1 + x$data$b2 + x$data$b3 + x$data$b4)
    e <- d * (x$data$b1 + x$data$b2 - x$data$b3 - x$data$b4)
    ##print(list(a=a,b=b,c=c,d=d))
    metadata <- x$metadata
    metadata$oce.coordinate <- "frame"
    res <- list(metadata=metadata,
                data=list(u=u, v=v, w=w, e=e,
                ei1=x$data$ei1,
                ei2=x$data$ei2,
                ei3=x$data$ei3,
                ei4=x$data$ei4,
                #pg1=x$data$pg1, ETC
                time=x$data$time,
                distance=x$data$distance,
                pressure=x$data$pressure,
                temperature=x$data$temperature,
                salinity=x$data$salinity,
                depth.of.transducer=x$data$depth.of.transducer,
                heading=x$data$heading,
                pitch=x$data$pitch,
                roll=x$data$roll
                ),                     # FIXME: this is too hard-wired
                processing.log=x$processing.log)
    log.action <- paste(deparse(match.call()), sep="", collapse="")
    class(res) <- c("adcp", "oce")
    res <- processing.log.append(res, log.action)
    return(res)
}

adcp.frame2earth <- function(x, pitch, heading, roll)
{
    if (!inherits(x, "adcp")) stop("method is only for adcp objects")
    if (!("u" %in% names(x$data))) stop("first, use adcp.beam2frame")

    ## FIXME: ignoring time-dependent heading, etc
    if (missing(pitch)) {
        pitch <- x$data$pitch[1]
        heading <- x$data$heading[1]
        roll <- x$data$roll[1]
    }

    ##str(pitch)
    ##str(heading)
    ##str(roll)


    ## RD Instruments, 1998.
    ## ADCP Coordinate Transformation
    ## P/N 951-6079-00 (July 1998)
    ## Pages 13 and 14
    a <- pi / 180
    CH <- cos(a * heading)
    SH <- sin(a * heading)
    CP <- cos(a * pitch)
    SP <- sin(a * pitch)
    CR <- cos(a * roll)
    SR <- sin(a * roll)
    m1 <- matrix(c(CH, SH, 0,
                   -SH, CH, 0,
                   0, 0, 1),
                 nrow=3, byrow=TRUE)
    m2 <- matrix(c(1, 0, 0,
                   0, CP, -SP,
                   0, SP, CP),
                 nrow=3, byrow=TRUE)
    m3 <-  matrix(c(CR, 0, SR,
                    0, 1, 0,
                    -SR, 0, CR),
                  nrow=3, byrow=TRUE)
    ## print(round(m1))
    ## print(round(m2))
    ## print(round(m3))
    rotation.matrix <- m3 %*% m2 %*% m1
    if (!TRUE) {
        cat("Rotation matrix:\n")
        print(rotation.matrix)
    }
    res <- x
    dim <- dim(x$data$u)
    len <- prod(dim)
    ## Flatten the velocity-component arrays
    u <- x$data$u
    dim(u) <- len
    v <- x$data$v
    dim(v) <- len
    w <- x$data$w
    dim(w) <- len
    ## construct velocity vectors
    vec <- matrix(c(u, v, w), nrow=3, byrow=TRUE)
    ##str(vec)
    ##str(m)
    rvec <- rotation.matrix %*% vec
    ##str(rvec)
    res$data$u <- rvec[1,]
    dim(res$data$u) <- dim
    res$data$v <- rvec[2,]
    dim(res$data$v) <- dim
    res$data$w <- rvec[3,]
    dim(res$data$w) <- dim
    ##cat("names for earth ...", paste(names(res$data), collapse=","), "\n")
    ##str(res$data$u)
    ##str(x$data$u)
    res$metadata$oce.coordinate <- "earth"
    res
}
