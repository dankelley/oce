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
    if (bits == "0100") beam.config <- "4-beam janus"
    else if (bits == "0101") beam.config <- "5-beam janus demod"
    else if (bits == "1111") beam.config <- "5-beam janus 2 demd"
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
    cpu.board.serial.number <- readBin(FLD[43:50], "integer", n=1, size=8, endian="little")
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
         kHz=kHz,
         beam.angle=beam.angle,
         beam.pattern=beam.pattern,
         beam.config=beam.config,
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
    p <- read.profile(file, debug=debug)
    bytes.per.profile <- seek(file)
    seek(file, where=bytes.per.profile * skip)
    if (read < 1) stop("cannot read fewer than one profile")
    b1 <- array(dim=c(read, p$header$number.of.cells))
    b2 <- array(dim=c(read, p$header$number.of.cells))
    b3 <- array(dim=c(read, p$header$number.of.cells))
    b4 <- array(dim=c(read, p$header$number.of.cells))
    time <- NULL
    for (i in 1:read) {
        p <- read.profile(file,debug=debug)
        b1[i,] <- p$v[,1]
        b2[i,] <- p$v[,2]
        b3[i,] <- p$v[,3]
        b4[i,] <- p$v[,4]
        time <- c(time, p$header$RTC.time)
        if (i == 1) metadata <- c(p$header, filename=filename)
    }
    class(time) <- "POSIXct"
    attr(time, "tzone") <- attr(p$header$RTC.time, "tzone")
    data <- list(b1=b1,
                 b2=b2,
                 b3=b3,
                 b4=b4,
                 time=time,
                 distance=seq(p$header$bin1.distance, by=p$header$depth.cell.length, length.out=p$header$number.of.cells)
                 )
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
    fives[1,] <- fivenum(object$data[[1]], na.rm=TRUE)
    fives[2,] <- fivenum(object$data[[2]], na.rm=TRUE)
    fives[3,] <- fivenum(object$data[[3]], na.rm=TRUE)
    fives[4,] <- fivenum(object$data[[4]], na.rm=TRUE)
    rownames(fives) <- names(object$data)[1:4]
    colnames(fives) <- c("Min.", "1st Qu.", "Median", "3rd Qu.", "Max.")
    res <- list(filename=object$metadata$filename,
                metadata=object$metadata,
                kHz=object$metadata$kHz,
                number.of.beams=object$metadata$number.of.beams,
                beam.angle=object$metadata$beam.angle,
                beam.config=object$metadata$beam.config,
                beam.pattern=object$metadata$beam.pattern,
                coordinate.transformation=object$metadata$coordinate.transformation,
                fives=fives,
                profiles=length(object$data$time),
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
    cat("  Coordinate transformation:  ", x$coordinate.transformation, "\n")
    cat("  Transducer depth:           ", x$metadata$depth.of.transducer*0.01, "\n")
    cat("  Pressure:                   ", x$metadata$pressure*0.01, "dbar (in first record)\n")
    cat("  Salinity:                   ", x$metadata$salinity, "PSU (in first record)\n")
    cat("  Temperature:                ", x$metadata$temperature, "degC (in first record)\n")
    cat("  Sampling\n",
        "    Frequency:          ", x$kHz, "kHz\n",
        "    Number of beams:    ", x$number.of.beams, "\n",
        "    Beam configuration: ", x$beam.config, "\n",
        "    Beam angle:         ", x$beam.angle, "degree\n",
        "    Beam pattern:       ", x$beam.pattern, "\n",
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

plot.adcp <- function(x, which=1:4, ...)
{
    lw <- length(which)
    if (lw > 1) oldpar <- par(no.readonly = TRUE)
    par(mfrow = c(lw, 1))
    if (!"mgp" %in% names(list(...))) par(mgp = getOption("oce.mgp"))
    mgp <- par("mgp")
    par(mar=c(mgp[1],mgp[1]+1,1,1))
    data.names <- names(x$data)
    for (w in 1:length(which)) {
        if (which[w] == 1) {
            image(x=x$data$time,y=x$data$distance, z=x$data[[1]],
                  xlab="Time", ylab="Distance", axes=FALSE, ...)
            axis.POSIXct(1, at=x$data$time)
            box()
            axis(2)
            mtext(data.names[1], side=3, cex=2/3, adj=1)
        }
        if (which[w] == 2) {
            image(x=x$data$time,y=x$data$distance, z=x$data[[2]],
                  xlab="Time", ylab="Distance", axes=FALSE, ...)
            axis.POSIXct(1, at=x$data$time)
            box()
            axis(2)
            mtext(data.names[2], side=3, cex=2/3, adj=1)
        }
        if (which[w] == 3) {
            image(x=x$data$time,y=x$data$distance, z=x$data[[3]],
                  xlab="Time", ylab="Distance", axes=FALSE, ...)
            axis.POSIXct(1, at=x$data$time)
            box()
            axis(2)
            mtext(data.names[3], side=3, cex=2/3, adj=1)
        }
        if (which[w] == 4) {
            image(x=x$data$time,y=x$data$distance, z=x$data[[4]],
                  xlab="Time", ylab="Distance", axes=FALSE, ...)
            axis.POSIXct(1, at=x$data$time)
            box()
            axis(2)
            mtext(data.names[4], side=3, cex=2/3, adj=1)
        }
    }
    if (lw > 1) par(oldpar)
}

adcp.beam2velo <- function(x)
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
    res <- list(metadata=x$metadata,
                data=list(u=u, v=v, w=w, e=e, time=x$data$time, distance=x$data$distance), # FIXME
                processing.log=x$processing.log)
    log.action <- paste(deparse(match.call()), sep="", collapse="")
    class(res) <- c("adcp", "oce")
    res <- processing.log.append(res, log.action)
    return(res)
}
