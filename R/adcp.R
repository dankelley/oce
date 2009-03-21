read.header <- function(file, debug) {
    ##
    ## header, 6 + 2 * number.of.data.types
    ##
    header.part1 <- readBin(file, "raw", n=6, size=1)
    if (debug > 1) {
        cat("First 6 bytes of header:", paste(header.part1, sep=' '), "\n")
    }
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
    header <- c(header, header.part2)
    data.offset <- readBin(header.part2, "integer", n=number.of.data.types, size=2, endian="little")
    if (debug) cat("data.offset=", paste(data.offset, sep=" "), "\n")
    ##
    ## FLD (fixed leader data) 59 bytes
    ##
    FLD <- readBin(file, "raw", n=59, size=1) # binary fixed leader data (Figure D-5)
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
    system.configuration <- readBin(FLD[5:6], "raw", n=2, size=1)
    system.configuration <- paste(byte2binary(system.configuration[1]),
                                  byte2binary(system.configuration[2]),sep="-")
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
    coordinate.transform <- readBin(FLD[26], "integer", n=1, size=1)
    heading.alignment <- readBin(FLD[27:28], "integer", n=1, size=2, endian="little")
    heading.bias <- readBin(FLD[29:30], "integer", n=1, size=2, endian="little")
    sensor.source <- readBin(FLD[31], "integer", n=1, size=1)
    sensors.available <- readBin(FLD[32], "integer", n=1, size=1)
    bin1.distance <- readBin(FLD[33:34], "integer", n=1, size=2, endian="little")
    xmit.pulse.length <- readBin(FLD[35:36], "integer", n=1, size=2, endian="little")
    wp.ref.layer.average <- readBin(FLD[37:38], "integer", n=1, size=2, endian="little")
    false.target.thresh <- readBin(FLD[39], "integer", n=1, size=1)
    ## FLD[40] spare
    transmit.lag.distance <- readBin(FLD[41:42], "integer", n=1, size=2, endian="little")
    cpu.board.serial.number <- readBin(FLD[43:50], "integer", n=1, size=8, endian="little")
    system.bandwidth <- readBin(FLD[51:52], "integer", n=1, size=2, endian="little")
    system.power <- readBin(FLD[53], "integer", n=1, size=1)
    ## FLD[54] spare
    instrument.serial.number <- readBin(FLD[55:58], "integer", n=1, size=4, endian="little")
    ##cat("INSTRUMENT SERIAL NUMBER", FLD[55:58], "\n")

    beam.angle <- readBin(FLD[59], "integer", n=1, size=1) # NB 0 in first test case
    ##cat("BEAM ANGLE=", FLD[59], "or", beam.angle, "\n")
    dist.bin1  <- readBin(FLD[65:68], "integer", n=1, size=4, endian="little")
    ##
    ## VLD (variable leader data) 65 bytes
    ##
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
    if (speed.of.sound < 1400 || speed.of.sound > 1500) stop("speed of sound is ", speed.of.sound, ", which is outside the permitted range of 1400 m/s to 1500 m/s")
    depth.of.transducer <- readBin(VLD[17:18], "integer", n=1, size=2, endian="little")
    heading <- readBin(VLD[19:20], "integer", n=1, size=2, endian="little") * 0.01
    pitch <- readBin(VLD[21:22], "integer", n=1, size=2, endian="little") * 0.01
    roll <- readBin(VLD[23:24], "integer", n=1, size=2, endian="little") * 0.01
    salinity <- readBin(VLD[25:26], "integer", n=1, size=2, endian="little")
    if (salinity < 0 || salinity > 40) stop("salinity is ", salinity, ", which is outside the permitted range of 0 to 40 PSU")
    temperature <- readBin(VLD[27:28], "integer", n=1, size=2, endian="little") * 0.01
    if (temperature < -5 || temperature > 40) stop("temperature is ", temperature, ", which is outside the permitted range of -5 to 40 degC")
    ## Skipping a lot ...
    pressure <- readBin(VLD[49:52], "integer", n=1, size=4, endian="little", signed=FALSE)
    list(header=header,
         header.length=length(header),
         program.version.major=fv,
         program.version.minor=fr,
         ##program.version=program.version,
         system.configuration=system.configuration,
         number.of.data.types=number.of.data.types,
         data.offset=data.offset,
         number.of.beams=number.of.beams,
         number.of.cells=number.of.cells,
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
         coordinate.transform=coordinate.transform,
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
         beam.angle=beam.angle,
         dist.bin1=dist.bin1,
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
         salinity=salinity,
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
                  size=1, endian="little")
    ei <- matrix(ei, ncol=header$number.of.beams, byrow=TRUE)

    if (debug) cat("got echo intensity\n")
    ## percent good, should start with 0x00 0x04
    pg.ID <- readBin(file, "raw", n=2, size=1)
    if (pg.ID[1] != 0x00) stop("first byte of percent-good segment should be 0x00 but is ", pg.ID[1])
    if (pg.ID[2] != 0x04) stop("first byte of percent-good segment should be 0x04 but is ", pg.ID[2])
    pg <- readBin(file, "integer",
                  n=header$number.of.beams * header$number.of.cells,
                  size=1, endian="little")
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
                      debug=0, log.action)
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
        open(file, "r")
        on.exit(close(file))
    }
    ## read a profile, to get length so we can seek
    junk <- read.profile(file, debug=debug)
    bytes.per.profile <- seek(file)
    seek(file, where=bytes.per.profile * skip)
    if (read < 1) stop("cannot read fewer than one profile")
    b1 <- array(dim=c(read, a1$header$number.of.cells))
    b2 <- array(dim=c(read, a1$header$number.of.cells))
    b3 <- array(dim=c(read, a1$header$number.of.cells))
    b4 <- array(dim=c(read, a1$header$number.of.cells))
    times <- NULL
    for (i in 1:read) {
        p <- read.profile(file,debug=debug)
        b1[i,] <- p$v[,1]
        b2[i,] <- p$v[,2]
        b3[i,] <- p$v[,3]
        b4[i,] <- p$v[,4]
        times <- c(times, p$header$RTC.time)
        if (i == 1) metadata <- c(p$header, filename=filename)
    }
    class(times) <- "POSIXct"           # BUG: should make GMT
    attr(times, "tzone") <- attr(p$header$RTC.time, "tzone")
    data <- list(b1=b1, b2=b2, b3=b3, b4=b4, times=times, depths=seq(0, by=p$header$depth.cell.length, length.out=p$header$number.of.cells))
    if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processing.log.item(log.action)
    res <- list(data=data, metadata=metadata, processing.log=log.item)
    class(res) <- c("adcp", "oce")
    res
}

summary.adcp <- function(object, ...)
{
    if (!inherits(object, "adcp")) stop("method is only for adcp objects")
    fives <- matrix(nrow=4, ncol=5) # 4 beams
    fives[1,] <- fivenum(object$data$b1, na.rm=TRUE)
    fives[2,] <- fivenum(object$data$b2, na.rm=TRUE)
    fives[3,] <- fivenum(object$data$b3, na.rm=TRUE)
    fives[4,] <- fivenum(object$data$b4, na.rm=TRUE)
    rownames(fives) <- paste("Beam", 1:4)
    colnames(fives) <- c("Min.", "1st Qu.", "Median", "3rd Qu.", "Max.")
    bits <- substr(object$metadata$system.configuration, 6, 8)
    if (bits == "000") kHz <- 75
    else if (bits == "001") kHz <-  150
    else if (bits == "010") kHz <-  300
    else if (bits == "011") kHz <-  600
    else if (bits == "100") kHz <- 1200
    else if (bits == "101") kHz <- 2400
    bits <- substr(object$metadata$system.configuration, 16, 17)
    if (bits == "00") beam.angle <- 15
    else if (bits == "01") beam.angle <- 20
    else if (bits == "10") beam.angle <- 30
    else if (bits == "11") beam.angle <- NA # means 'other'
    res <- list(filename=object$metadata$filename,
                metadata=object$metadata,
                kHz=kHz,
                beam.angle=beam.angle,
                fives=fives,
                profiles=length(object$data$times),
                processing.log=processing.log.summary(object))
    class(res) <- "summary.adcp"
    res
}

print.summary.adcp <- function(x, digits=max(6, getOption("digits") - 1), ...)
{
    cat("ADCP timeseries\n")
    cat("  Filename:                 ", x$filename, "\n")
    cat("  Software version:         ", paste(x$metadata$program.version.major, x$metadata$program.version.minor, sep="."), "\n")
    cat("  System configuration:     ", x$metadata$system.configuration, "\n")
    cat("  CPU board serial number:  ", x$metadata$cpu.board.serial.number, "\n")
    cat("  Instrument serial number: ", x$metadata$instrument.serial.number, "\n")
    cat("  Pressure:                 ", x$metadata$pressure, "db (in first record)\n")
    cat("  Salinity:                 ", x$metadata$salinity, "PSU (in first record)\n")
    cat("  Temperature:              ", x$metadata$temperature, "degC (in first record)\n")
    cat("  Sampling\n",
        "    Frequency:          ", x$kHz, "kHz\n",
        "    Number of beams:    ", x$metadata$number.of.beams, "\n",
        "    Beam angle:         ", x$beam.angle, "degrees\n",
        "    Number of cells:    ", x$metadata$number.of.cells, "\n",
        "    Cell length:        ", x$metadata$depth.cell.length, "m\n",
        "    First cell:         ", x$metadata$bin1.dist,"m from the instrument\n",
        "    Pings per ensemble: ", x$metadata$pings.per.ensemble, "\n",
        "    Start time:         ", as.character(x$metadata$RTC.time), "\n",
        "    Profiles:           ", x$profiles, "\n"
        )
    cat("\nStatistics:\n")
    print(x$fives)
    cat("\n")
    print(x$processing.log)
    invisible(x)
}

## TESTS
if (FALSE) {
    source("misc.R")
    source("processing.log.R")
    source("adcp.R")
    d <- read.adcp("~/SL08F001.000",read=50)
    print(summary(d))
    image(x=d$data$times,y=d$data$depths, z=d$data$b2, xlab="Time", ylab="Distance", axes=FALSE)
    axis.POSIXct(1, at=d$data$times)
    box()
    axis(2)
    title("Beam 1")
}

