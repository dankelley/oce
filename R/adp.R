ad.beam.name <- function(x, which)
{
    if (x$metadata$oce.coordinate == "beam")
        c("beam 1", "beam 2", "beam 3", "beam 4")[which]
    else if (x$metadata$oce.coordinate == "earth")
        c("east", "north", "up", "error")[which]
    else if (x$metadata$oce.coordinate == "frame")
        c("u", "v", "w", "e")[which]
    else if (x$metadata$oce.coordinate == "other")
        c("u'", "v'", "w'", "e")[which]
    else " "
}

read.header.rdi <- function(file, debug)
{
    ##
    ## header, of length 6 + 2 * number.of.data.types bytes
    ##
    header.part1 <- readBin(file, "raw", n=6, size=1)
    if (debug > 1)
        cat("First 6 bytes of header:", paste(header.part1, sep=' '), "\n")
    header <- header.part1
    if (header.part1[1] != 0x7f) stop("first byte in file must be 0x7f, but it was 0x", header.part1[1])
    if (header.part1[2] != 0x7f) stop("second byte in file must be 0x7f but it was 0x", header.part1[2])
    num.bytes.in.ensemble <- readBin(header.part1[3:4], "integer", n=1, size=2, endian="little")
    if (debug) cat("num.bytes.in.ensemble=", num.bytes.in.ensemble,"\n")
    ## header.part1[5] spare
    number.of.data.types <- readBin(header.part1[6], "integer", n=1, size=1)
    if (number.of.data.types < 1) stop("cannot have ", number.of.data.types, " data types, as header indicates")
    if (debug) cat("number.of.data.types=", number.of.data.types, "\n")
    ## part 2 of header is these data offsets
    header.part2 <- readBin(file, "raw", n=2*number.of.data.types, size=1)

    ##cat("after reading header, seek(file)=", seek(file), "\n")

    header <- c(header, header.part2)
    data.offset <- readBin(header.part2, "integer", n=number.of.data.types, size=2, endian="little")
    if (debug) cat("data.offset=", paste(data.offset, sep=" "), "\n")
    ##
    ## FLD (fixed leader data) 59 bytes
    ## OLD: FLD <- readBin(file, "raw", n=59, size=1) # binary fixed leader data (Figure D-5)
    FLD <- readBin(file, "raw", n=data.offset[2]-data.offset[1], size=1)
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
    system.configuration <- paste(byte2binary(FLD[5]),
                                  byte2binary(FLD[6]),sep="-")
    bits <- substr(system.configuration, 6, 8)
    if (bits == "000") frequency <- 75        # kHz
    else if (bits == "001") frequency <-  150
    else if (bits == "010") frequency <-  300
    else if (bits == "011") frequency <-  600
    else if (bits == "100") frequency <- 1200
    else if (bits == "101") frequency <- 2400
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
    cell.size <- readBin(FLD[13:14], "integer", n=1, size=2, endian="little") / 100 # WS in m
    if (cell.size < 0 || cell.size > 64) stop("cell size of ", cell.size, "is not in the allowed range of 0m to 64m")
    blank.after.transmit <- readBin(FLD[15:16], "integer", n=1, size=2, endian="little") / 100 # in m
    profiling.mode <- readBin(FLD[17], "integer", n=1, size=1) # WM
    low.corr.thresh <- readBin(FLD[18], "integer", n=1, size=1)
    number.of.code.reps <- readBin(FLD[19], "integer", n=1, size=1)
    percent.gd.minimum <- readBin(FLD[20], "integer", n=1, size=1)
    error.velocity.maximum <- readBin(FLD[21:22], "integer", n=1, size=2, endian="little")
    tpp.minutes <- readBin(FLD[23], "integer", n=1, size=1)
    tpp.seconds <- readBin(FLD[24], "integer", n=1, size=1)
    tpp.hundredths <- readBin(FLD[25], "integer", n=1, size=1)
    bits <- substr(byte2binary(FLD[26], endian="big"), 4, 5)
    coordinate.system <- "???"
    if (bits == "00") coordinate.system <- "beam"
    else if (bits == "01") coordinate.system <- "instrument"
    else if (bits == "10") coordinate.system <- "frame"
    else if (bits == "11") coordinate.system <- "earth"
    heading.alignment <- readBin(FLD[27:28], "integer", n=1, size=2, endian="little")
    heading.bias <- readBin(FLD[29:30], "integer", n=1, size=2, endian="little")
    sensor.source <- readBin(FLD[31], "integer", n=1, size=1)
    sensors.available <- readBin(FLD[32], "integer", n=1, size=1)
    bin1.distance <- readBin(FLD[33:34], "integer", n=1, size=2, endian="little", signed=FALSE) * 0.01
    ##cat("bin1.distance being inferred from 0x", FLD[33:34], " as ", bin1.distance, "\n", sep="")
    xmit.pulse.length <- readBin(FLD[35:36], "integer", n=1, size=2, endian="little", signed=FALSE) * 0.01
    ##cat("xmit.pulse.length being inferred from 0x", FLD[35:36], " as ", xmit.pulse.length, "\n", sep="")
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
    serial.number <- readBin(FLD[55:58], "integer", n=1, size=4, endian="little")
    if (debug) cat("SERIAL NUMBER", serial.number, "\n")
    if (debug) cat("SERIAL NUMBER", FLD[55:58], "\n")
    if (serial.number == 0) serial.number <- c(cpu.board.serial.number, "(CPU board)") # FIXME: where is serial #?

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
    if (VLD[2] != 0x00) stop("byte 2 of variable leader data should be 0x00, but it is ", VLD[2])
    ensemble.number <- readBin(VLD[3:4], "integer", n=1, size=2, endian="little")
    RTC.year <- readBin(VLD[5], "integer", n=1, size=1)
    if (RTC.year < 1800) RTC.year <- RTC.year + 2000 # fix Y2K problem
    RTC.month <- readBin(VLD[6], "integer", n=1, size=1)
    RTC.day <- readBin(VLD[7], "integer", n=1, size=1)
    RTC.hour <- readBin(VLD[8], "integer", n=1, size=1)
    RTC.minute <- readBin(VLD[9], "integer", n=1, size=1)
    RTC.second <- readBin(VLD[10], "integer", n=1, size=1)
    RTC.hundredths <- readBin(VLD[11], "integer", n=1, size=1)
    time <- ISOdatetime(RTC.year, RTC.month, RTC.day, RTC.hour, RTC.minute, RTC.second + RTC.hundredths / 100, tz = "UTC") # not sure on TZ
    if (debug > 2) cat("time:", format(time), "\n")

    ensemble.number.MSB <- readBin(VLD[12], "integer", n=1, size=1)
    bit.result <- readBin(VLD[13:14], "integer", n=1, size=2, endian="little")
    speed.of.sound  <- readBin(VLD[15:16], "integer", n=1, size=2, endian="little")
    if (speed.of.sound < 1400 || speed.of.sound > 1600) warning("speed of sound is ", speed.of.sound, ", which is outside the permitted range of 1400 m/s to 1600 m/s")
    depth.of.transducer <- readBin(VLD[17:18], "integer", n=1, size=2, endian="little") * 0.1
    if (debug > 2) cat("depth of transducer:", depth.of.transducer, "\n")
    heading <- readBin(VLD[19:20], "integer", n=1, size=2, endian="little") * 0.01
    if (debug > 2) cat("heading:", heading, "\n")
    pitch <- readBin(VLD[21:22], "integer", n=1, size=2, endian="little") * 0.01
    if (debug > 2) cat("pitch:", pitch, "\n")
    roll <- readBin(VLD[23:24], "integer", n=1, size=2, endian="little") * 0.01
    if (debug > 2) cat("roll:", roll, "\n")
    salinity <- readBin(VLD[25:26], "integer", n=1, size=2, endian="little")
    if (salinity < 0 || salinity > 40) warning("salinity is ", salinity, ", which is outside the permitted range of 0 to 40 PSU")
    temperature <- readBin(VLD[27:28], "integer", n=1, size=2, endian="little") * 0.01
    if (temperature < -5 || temperature > 40) warning("temperature is ", temperature, ", which is outside the permitted range of -5 to 40 degC")

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

    list(instrument.type="rdi",
         header=header,
         header.length=length(header),
         program.version.major=fv,
         program.version.minor=fr,
         ##program.version=program.version,
         system.configuration=system.configuration,
         frequency=frequency,
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
         cell.size=cell.size,
         profiling.mode=profiling.mode,
         low.corr.thresh=low.corr.thresh,
         number.of.code.reps=number.of.code.reps,
         percent.gd.minimum=percent.gd.minimum,
         error.velocity.maximum=error.velocity.maximum,
         tpp.minutes=tpp.minutes,
         tpp.seconds=tpp.seconds,
         tpp.hundredths=tpp.hundredths,
         coordinate.system=coordinate.system,
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
         serial.number=serial.number,
         ## beam.angle=beam.angle,  # wrong in my tests, anyway
         ensemble.number=ensemble.number,
         time=time,
         ensemble.number.MSB=ensemble.number.MSB,
         bit.result=bit.result,
         speed.of.sound=speed.of.sound,
         depth.of.transducer=depth.of.transducer,
         heading=heading,
         pitch=pitch,
         roll=roll,
         salinity=salinity,             # seems to be constant
         temperature=temperature,
         pressure=pressure)
}                                       # read.header.rdi()

read.profile.rdi <- function(file, header, debug)
{
    if (missing(header)) {
        header <- read.header.rdi(file, debug=debug)
    } else {
        junk <- readBin(file, "raw", n=header$header.length, size=1)
    }
    ## velocity, should start with 0x00 then 0x01
    v.ID <- readBin(file, "raw", n=2, size=1)
    if (v.ID[1] != 0x00) stop("first byte of velocity segment should be 0x00 but is ", v.ID[1], " at file position ", seek(file)-2)
    if (v.ID[2] != 0x01) stop("first byte of velocity segment should be 0x01 but is ", v.ID[2], " at file position ", seek(file)-1)
    if (debug) cat("got velo\n")
    v <- readBin(file, "integer",
                 n=header$number.of.beams * header$number.of.cells,
                 size=2, endian="little")
    v[v==(-32768)] <- NA       # blank out bad data
    v <- matrix(v / 1000, ncol=header$number.of.beams, byrow=TRUE)
    ## correlation magnitude, should start with 0x00 0x01
    q.ID <- readBin(file, "raw", n=2, size=1)
    if (q.ID[1] != 0x00) stop("first byte of correlation-magnitude segment should be 0x00 but is ", q.ID[1])
    if (q.ID[2] != 0x02) stop("first byte of correlation-magnitude segment should be 0x02 but is ", q.ID[2])
    q <- matrix(readBin(file, "raw", n=header$number.of.beams * header$number.of.cells),
                ncol=header$number.of.beams, byrow=TRUE)
    if (debug) cat("got correlation magnitude\n")
    ## echo intensity, should start with 0x00 0x03
    a.ID <- readBin(file, "raw", n=2, size=1)
    if (a.ID[1] != 0x00) stop("first byte of echo-intensity segment should be 0x00 but is ", a.ID[1])
    if (a.ID[2] != 0x03) stop("first byte of echo-intensity segment should be 0x03 but is ", a.ID[2])
    a <- matrix(readBin(file, "raw", n=header$number.of.beams * header$number.of.cells),
                ncol=header$number.of.beams, byrow=TRUE)
    if (debug) cat("got echo intensity\n")
    ## percent good, should start with 0x00 0x04
    pg.ID <- readBin(file, "raw", n=2, size=1)
    if (pg.ID[1] != 0x00) stop("first byte of percent-good segment should be 0x00 but is ", pg.ID[1])
    if (pg.ID[2] != 0x04) stop("first byte of percent-good segment should be 0x04 but is ", pg.ID[2])
    pg <- matrix(readBin(file, "raw", n=header$number.of.beams * header$number.of.cells),
                 ncol=header$number.of.beams, byrow=TRUE)
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
    list(header=header, v=v, a=a, q=q, pg=pg, bt=bt)
}

read.adp <- function(file, from=0, to, by=1,
                     type=c("rdi", "nortek", "sontek"),
                     debug=0, monitor=TRUE, log.action)
{
    type = match.arg(type)
    if (monitor) cat(file, "\n")
    if (type == "rdi")
        read.adp.rdi(file=file, from=from, to=to, by=by,
                     debug=debug, monitor=monitor, log.action=log.action)
    else if (type == "nortek")
        read.adp.nortek(file=file, from=from, to=to, by=by,
                        debug=debug, monitor=monitor, log.action=log.action)
    else if (type == "sontek")
        read.adp.sontek(file=file, from=from, to=to, by=by,
                        debug=debug, monitor=monitor, log.action=log.action)
}

read.adp.sontek <- function(file, from=0, to, by=1,
                            type=c("adp"),
                            withHeader=FALSE,
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
        seek(file, 0, "end")
        file.size <- seek(file, 0, "start")
        if (debug) cat("file", filename, "has", file.size, "bytes\n")
        buf <- readBin(file, "raw", n=file.size, endian="little")
        flag1 <- as.raw(0xa5)           # id
        flag2 <- as.raw(0x10)           # number of bytes (22 in decimal)
        match.flag1 <- which(buf==flag1)
        profile.start <- match.flag1[buf[match.flag1 + 1] == flag2]
        profile.start <- profile.start[1:(-1 + length(profile.start))] # last may be partial
        if (debug) {
            cat("first 100 bytes of first profile:\n")
            print(buf[profile.start[1]:(99+profile.start[1])])
        }
    }
    header.length <- 80                 # FIXME: should this really be hard-wired??
    s <- profile.start[1]
    ## Only read (important) things that don't change profile-by-profile
    number.of.beams <- as.integer(buf[s+26])
    if (debug) cat("number.of.beams=", number.of.beams, "\n")
    if (number.of.beams != 3) stop("there should be 3 beams, but the file indicates ", number.of.beams)

    orientation <- as.integer(buf[s+27])

    temp.mode <- as.integer(buf[s+28])
    if (debug) cat("temp.mode=", temp.mode, "\n")

    coordinate.system <- as.integer(buf[s+29])
    if (debug) cat("coordinate.system=", coordinate.system, "\n")

    number.of.cells <- readBin(buf[s+30:31], "integer", n=1, size=2, endian="little", signed=FALSE)
    if (debug) cat("number.of.cells=", number.of.cells, "\n")

    cell.size <- readBin(buf[s+32:33], "integer", n=1, size=2, endian="little", signed=FALSE) / 100 # metres
    if (debug) cat("cell.size=", cell.size, "m\n")

    blanking.distance <- readBin(buf[s+34:35], "integer", n=1, size=2, endian="little", signed=FALSE) / 100 # metres
    if (debug) cat("blanking.distance=", blanking.distance, "m\n")

    sound.speed <- readBin(buf[s+60:61], "integer", n=1, size=2, endian="little", signed=FALSE) / 10
    if (debug) cat("sound.speed=", sound.speed, "m/s\n")

    profiles.in.file <- length(profile.start)
    id <- buf[profile.start]
    bytes.per.profile <- diff(profile.start[1:2])
    if (debug) cat("bytes.per.profile=", bytes.per.profile, "\n")

    ## number.of.bytes <- buf[sample.start + 1]
    profile.start2 <- sort(c(profile.start, profile.start+1)) # use this to subset for 2-byte reads

    temperature <- readBin(buf[profile.start2 + 46], "integer", n=profiles.in.file, size=2, endian="little", signed=TRUE) / 100
    pressure <- readBin(buf[profile.start2 + 48], "integer", n=profiles.in.file, size=2, endian="little", signed=FALSE) / 100
    heading <- readBin(buf[profile.start2 + 40], "integer", n=profiles.in.file, size=2, endian="little", signed=TRUE) / 10
    pitch <- readBin(buf[profile.start2 + 42], "integer", n=profiles.in.file, size=2, endian="little", signed=TRUE) / 10
    roll <- readBin(buf[profile.start2 + 44], "integer", n=profiles.in.file, size=2, endian="little", signed=TRUE) / 10

    year <- readBin(buf[profile.start2+18], "integer", size=2, signed=FALSE, endian="little")
    day <- as.integer(buf[profile.start+20])
    month <- as.integer(buf[profile.start+21])
    minute <- as.integer(buf[profile.start+22])
    hour <- as.integer(buf[profile.start+23])
    sec100 <- as.integer(buf[profile.start+24])     # FIXME: determine whether this is 1/100th second
    second <- as.integer(buf[profile.start+25])
    time <- ISOdatetime(year, month, day, hour, minute, second+sec100/100, tz="UTC")

    to <- profiles.in.file
    warning("setting \"to\" to number of profiles in file, IGNORING any instructions you gave!")

    v <- array(dim=c(to, number.of.cells, number.of.beams))
    a <- array(dim=c(to, number.of.cells, number.of.beams))
    q <- array(dim=c(to, number.of.cells, number.of.beams))
    nd <- number.of.cells * number.of.beams
    if (debug) {
        cat("nd=",nd,"\n")
        cat("header.length=",header.length,"\n")
    }
    if (to > 0) {
        for (i in 1:to) {
            ## Doug does:
            ##   fseek(fid,adpProfiles(1)-1+18+8+58+(ii-1)*numCells(1)*2+(jj-1)*2,'bof');
            ##   v = fread(fid,numSamples,'int16=>double',adpPacketSize-1)/10;
            ## (he offsets by 83 ... but he also says the profile chunk size is 80+4*Nb*Nc+2-1,
            ## or 561, but I see repeats on length 562, so I think the formula should be 80+4*Nb*Nc+2.
            ## From that, I infer that there are 2 bytes after the profile data.
            ## I've tried (note using 1000 to get m/s)
            ##vv <- matrix(readBin(buf[profile.start[i] + header.length + 1:(2*nd)],
            ##                    "integer", n=nd, size=2, signed=TRUE, endian="little"), ncol=number.of.beams, byrow=FALSE)/1000

            vv <- matrix(readBin(buf[profile.start[i] + header.length + seq(0, 2*nd-1)],
                                 "integer", n=nd, size=2, signed=TRUE, endian="little"), ncol=number.of.beams, byrow=FALSE)/1000
            ##if (i == 1) {
            ##    print(t(matrix(buf[profile.start[i] + header.length + seq(0, 2*nd-1)],ncol=number.of.beams,byrow=FALSE)))
            ##    print(t(vv))
            ##}

            aa <- matrix(as.numeric(buf[profile.start[i] + header.length + 2*nd + seq(0, nd-1)]),
                         ncol=number.of.beams, byrow=FALSE)
            qq <- matrix(as.numeric(buf[profile.start[i] + header.length + 3*nd + seq(0, nd-1)]),
                         ncol=number.of.beams, byrow=FALSE)

            for (b in 1:number.of.beams) {
                v[i,,b] <- vv[,b]
                a[i,,b] <- aa[,b]
                q[i,,b] <- qq[,b]
            }

            if (monitor) {
                cat(".")
                if (!(i %% 50)) cat(i, "\n")
            }
        }
        if (monitor) cat("\nRead", to,  "profiles, out of a total of",profiles.in.file,"profiles in", filename, "\n")
        ma <- list(v=v, a=a, q=q)
    } else {
        ma <- NULL
    }
    data <- list(ma=ma,
                 ss=list(distance=seq(blanking.distance, by=cell.size, length.out=number.of.cells)),
                 ts=list(time=time,
                 temperature=temperature,
                 pressure=pressure,
                 heading=heading, pitch=pitch, roll=roll))

    metadata <- list(filename=filename,
                     instrument.type="sontek",
                     number.of.samples=profiles.in.file,
                     oce.coordinate=c("beam", "frame", "earth", "other")[coordinate.system+1], # FIXME: check this
                     number.of.beams=number.of.beams,
                     oce.beam.attenuated=FALSE,
                     orientation=if(orientation==1) "upward" else "downward")
    if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processing.log.item(log.action)
    res <- list(data=data, metadata=metadata, processing.log=log.item)
    class(res) <- c("adp", "sontek", "oce")
    res
}

read.adp.rdi <- function(file, from=0, to, by=1,
                          type=c("workhorse"),
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
    ## read two profiles so we can infer total number and their times
    p1 <- read.profile.rdi(file, debug=debug)
    bytes.per.profile <- seek(file)     # NOTE: be careful on these seek calls
    p2 <- read.profile.rdi(file, debug=debug)
    bin1.distance <- p1$header$bin1.distance
    xmit.pulse.length <- p1$header$xmit.pulse.length
    cell.size <- p1$header$cell.size
    ## go to the end, so the next seek (to get to the data) reveals file length
    seek(file, where=0, origin="end")
    bytes.in.file <- seek(file, where=0)
    profiles.in.file <- bytes.in.file / bytes.per.profile
    sampling.start <- p1$header$time
    sampling.end <- sampling.start + profiles.in.file * as.numeric(difftime(p2$header$time, p1$header$time, units="sec"))

    ## Possibly interpret from and to as starting and ending times.
    t1 <- p1$header$time[1]
    t2 <- p2$header$time[1]
    dt <- as.numeric(difftime(t2, t1, units="sec"))
    if (!missing(from) && inherits(from, "POSIXt")) {
        from <- max(as.numeric(difftime(from, t1, units="sec")) / dt, 0)
    }
    if (!missing(by) && is.character(by)) {
        if (length(grep(":", by)) > 0) {
            parts <- as.numeric(strsplit(by, ":")[[1]])
            if (length(parts == 2)) by.time <- parts[1] * 60 + parts[2]
            else if (length(parts == 3)) by.time <- parts[1] * 3600 + parts[2] * 60 + parts[3]
            else stop("cannot interpret \"by\" as POSIX time", by)
            by <- by.time / dt
        } else {
            warning("converting \"by\" from string to numeric.  (Use e.g. \"00:10\" to indicate 10s)")
            by <- as.numeric(by)
        }
    }

    if (!missing(to) && inherits(to, "POSIXt")) {
        to <- 1 + (as.numeric(difftime(to, t1, units="sec")) / dt - from) / by
        if (to < 0) stop("cannot have to < 0")
    }

    seek(file, bytes.per.profile * from, origin="start")
    ##cat("bytes.per.profile=", bytes.per.profile," bytes.in.file=", bytes.in.file, "\n")
    ##cat("profiles in file:", profiles.in.file, "\n")
    if (to < 1) stop("cannot read fewer than one profile")

    v <- array(dim=c(to, p1$header$number.of.cells, p1$header$number.of.beams))
    a <- array(raw(), dim=c(to, p1$header$number.of.cells, p1$header$number.of.beams))
    q <- array(raw(), dim=c(to, p1$header$number.of.cells, p1$header$number.of.beams))
    time <- pressure <- temperature <- salinity <- depth.of.transducer <- heading <- pitch <- roll <- NULL
    for (i in 1:to) {
        p <- read.profile.rdi(file,debug=debug)
        for (beam in 1:p$header$number.of.beams) {
            v[i,,beam] <- p$v[,beam]
            a[i,,beam] <- p$a[,beam]
            q[i,,beam] <- p$q[,beam]
        }
        time <- c(time, p$header$time)
        pressure <- c(pressure, p$header$pressure)
        temperature <- c(temperature, p$header$temperature)
        salinity <- c(salinity, p$header$salinity)
        depth.of.transducer <- c(depth.of.transducer, p$header$depth.of.transducer)
        heading <- c(heading, p$header$heading)
        pitch <- c(pitch, p$header$pitch)
        roll <- c(roll, p$header$roll)
        if (i == 1)  {
            metadata <- p$header
            metadata$bin1.distance <- bin1.distance
            metadata$xmit.pulse.length <- xmit.pulse.length
            metadata$sampling.start <- sampling.start
            metadata$sampling.end <- sampling.end
            metadata$filename <- filename
            metadata$oce.beam.attenuated <- FALSE
            metadata$oce.coordinate <- p$header$coordinate.system
            metadata$number.of.beams <- p1$header$number.of.beams
        }
        if (monitor) {
            cat(".")
            if (!(i %% 50)) cat(i, "\n")
        }
        if (by > 1) {
            seek(file, bytes.per.profile * (by - 1), origin="current")
        }
    }
    if (monitor) cat("\nRead", to,  "profiles, out of a total of",profiles.in.file,"profiles in", filename, "\n")
    ##cat("\nfivenum(ei1,na.rm=TRUE)"); print(fivenum(ei1, na.rm=TRUE))
    class(time) <- c("POSIXt", "POSIXct")
    attr(time, "tzone") <- attr(p$header$time, "tzone")
    data <- list(ma=list(v=v, a=a, q=q),
                 ss=list(distance=seq(bin1.distance, by=cell.size, length.out=p$header$number.of.cells)),
                 ts=list(time=time,
                 pressure=pressure,
                 temperature=temperature,
                 salinity=salinity,
                 depth.of.transducer=depth.of.transducer,
                 heading=heading,
                 pitch=pitch,
                 roll=roll))
    if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processing.log.item(log.action)
    res <- list(data=data, metadata=metadata, processing.log=log.item)
    class(res) <- c("adp", "rdi", "oce")
    res
}

summary.adp <- function(object, ...)
{
    if (!inherits(object, "adp")) stop("method is only for adp objects")
    if (inherits(object, "aquadopp")) {
        res.specific <- list(internal.code.version=object$metadata$internal.code.version,
                             hardware.revision=object$metadata$hardware.revision,
                             rec.size=object$metadata$rec.size*65536/1024/1024,
                             velocity.range=object$metadata$velocity.range,
                             firmware.version=object$metadata$firmware.version,
                             config=object$metadata$config,
                             config.pressure.sensor=object$metadata$config.pressure.sensor,
                             config.magnetometer.sensor=object$metadata$config.magnetometer.sensor,
                             config.tilt.sensor=object$metadata$config.pressure.sensor,
                             config.pressure.sensor=object$metadata$config.tilt.sensor,
                             serial.number.head=object$metadata$serial.number.head,
                             blanking.distance=object$metadata$blanking.distance,
                             measurement.interval=object$metadata$measurement.interval,
                             deployment.name=object$metadata$deployment.name,
                             velocity.scale=object$metadata$velocity.scale)
    } else if (inherits(object, "rdi")) {
        res.specific <- list(number.of.data.types=object$metadata$number.of.data.types,
                             bin1.distance=object$metadata$bin1.distance,
                             xmit.pulse.length=object$metadata$xmit.pulse.length,
                             oce.beam.attenuated=object$metadata$oce.beam.attenuated,
                             number.of.data.types=object$metadata$number.of.data.types,
                             beam.config=object$metadata$beam.config)
    } else if (inherits(object, "sontek")) {
        res.specific <- NULL
    } else if (inherits(object, "nortek")) {
        res.specific <- NULL
    } else stop("can only summarize ADP objects of sub-type \"rdi\", \"sontek\", or \"nortek\", not class ", paste(class(object),collapse=","))
    ts.names <- names(object$data$ts)
    ma.names <- names(object$data$ma)
    fives <- matrix(nrow=(-1+length(ts.names)+length(ma.names)), ncol=5)
    ii <- 1
    for (i in 1:length(ts.names)) {
        if (names(object$data$ts)[i] != "time") {
            fives[ii,] <- fivenum(object$data$ts[[ts.names[i]]], na.rm=TRUE)
            ii <- ii + 1
        }
    }
    for (i in 1:length(ma.names)) {
        fives[ii,] <- fivenum(as.numeric(object$data$ma[[ma.names[i]]]), na.rm=TRUE)
        ii <- ii + 1
    }
    rownames(fives) <- c(ts.names[ts.names != "time"], ma.names)
    colnames(fives) <- c("Min.", "1st Qu.", "Median", "3rd Qu.", "Max.")

    v.dim <- dim(object$data$ma$v)

    res <- list(res.specific,
                filename=object$metadata$filename,
                instrument.type=object$metadata$instrument.type,
                serial.number=object$metadata$serial.number,
                start.time=object$data$ts$time[1],
                delta.time=difftime(object$data$ts$time[2], object$data$ts$time[1], units="secs"),
                end.time=object$data$ts$time[length(object$data$ts$time)],
                sampling.start=object$metadata$sampling.start,
                sampling.end=object$metadata$sampling.end,
                distance=object$data$ss$distance,
                metadata=object$metadata,
                frequency=object$metadata$frequency,
                number.of.profiles=v.dim[1],
                number.of.cells=v.dim[2],
                number.of.beams=v.dim[3],
                number.of.data.types=object$metadata$number.of.data.type,
                bin1.distance=object$metadata$bin1.distance,
                cell.size=object$metadata$cell.size,
                xmit.pulse.length=object$metadata$xmit.pulse.length,
                oce.beam.attenuated=object$metadata$oce.beam.attenuated,
                beam.angle=object$metadata$beam.angle,
                beam.config=object$metadata$beam.config,
                orientation=object$metadata$orientation,
                coordinate.system=object$metadata$coordinate.system,
                oce.coordinate=object$metadata$oce.coordinate,
                fives=fives,
                time=object$data$ts$time,
                processing.log=processing.log.summary(object))
    class(res) <- "summary.adp"
    res
}                                       # summary.adp()

print.summary.adp <- function(x, digits=max(6, getOption("digits") - 1), ...)
{
    cat("ADP summary\n")
    cat("  Instrument type:            ", x$instrument.type, "\n")
    cat("  Filename:                   ", x$filename, "\n")
    cat("  Instrument serial number:   ", x$metadata$serial.number, "\n")
    cat("  Coordinate system:          ", x$coordinate.system, "[originally],", x$oce.coordinate, "[presently]\n")
    ##cat("  Number of data types:       ", x$number.of.data.types, "\n")
    cat("  Frequency:                  ", x$frequency, "kHz\n")
    cat("  Beams:                      ", x$number.of.beams, if (x$oce.beam.attenuated) "(attenuated)\n" else "(not attenuated)\n")
    cat("  Orientation:                ", x$orientation, "\n")
    cat("  Beam angle:                 ", x$metadata$beam.angle, "\n")
    cat("  Number of cells:            ", x$number.of.cells, "\n")
    ##cat("  Cell size:                  ", x$cell.size, "m\n")
    ##cat("  First cell centred:         ", x$bin1.dist,"m from sensor\n")
    cat("  Number of profiles :        ", x$number.of.profiles, "\n")
    cat("  Distances within profiles:  ", x$distance[1], "to", x$distance[length(x$distance)], "m at interval", diff(x$distance[1:2]), "m\n")
    cat("  Profile times:              ", as.character(x$start.time), attr(x$start.time,"tzone"),
        "to", as.character(x$end.time), attr(x$end.time, "tzone"), "at interval", x$delta.time, "s\n")

    if (x$instrument.type == "rdi") {
        cat("  RDI-specific\n")
        cat("    Transducer depth:           ", x$metadata$depth.of.transducer, "m\n")
        cat("    System configuration:       ", x$metadata$system.configuration, "\n")
        cat("    Software version:           ", paste(x$metadata$program.version.major, x$metadata$program.version.minor, sep="."), "\n")
        cat("    CPU board serial number:    ", x$metadata$cpu.board.serial.number, "\n")
        cat("    Xmit pulse length:          ", x$metadata$xmit.pulse.length,"m\n")
        cat("    Beam pattern:               ", x$metadata$beam.pattern, "\n")
        cat("    Pings per ensemble:         ", x$metadata$pings.per.ensemble, "\n")
    }
    if (x$instrument.type == "aquadopp high resolution") {
        cat("  Aquadopp-specific:\n")
        cat("    Internal code version:       ", x$metadata$internal.code.version, "\n")
        cat("    Hardware revision:           ", x$metadata$hardware.revision, "\n")
        cat("    Head serial number:          ", x$metadata$head.serial.number, "\n")
        ##cat("    Transformation matrix:\n")
        ##print(x$metadata$beam.to.xyz)
    }
    cat("\nStatistics:\n")
    print(x$fives)
    cat("\n")
    print(x$processing.log)
    invisible(x)
}

plot.adp <- function(x,
                     which=1:dim(x$data$ma$v)[3],
                     col=oce.colors.palette(128, 1),
                     zlim,
                     titles,
                     ytype=c("profile", "distance"),
                     adorn=NULL,
                     draw.time.range=getOption("oce.draw.time.range"),
                     mgp=getOption("oce.mgp"),
                     mar=c(mgp[1],mgp[1]+1,1,1/4),
                     margins.as.image=FALSE,
                     cex=1,
                     ...)
{
    if (!inherits(x, "adp")) stop("method is only for adp objects")
    ##opar <- par(mgp, mar, cex)
    opar <- par(no.readonly = TRUE)
    lw <- length(which)
    if (!missing(titles) && length(titles) != lw) stop("length of 'titles' must equal length of 'which'")
    if (lw > 1) on.exit(par(opar))
    par(mgp=mgp, mar=mar, cex=cex)
    dots <- list(...)
    ytype <- match.arg(ytype)
    ytype <- match.arg(ytype)
    if (missing(zlim)) {
        gave.zlim <- FALSE
        zlim.given <- NULL
    } else {
        gave.zlim <- TRUE
        if (is.vector(zlim)) {
            if (length(zlim) == 2) {
                zlim.given <- matrix(rep(zlim, length(which)),ncol=2,byrow=TRUE)
            } else {
                stop("zlim must be a vector of length 2, or a matrix with 2 columns")
            }
        }
        zlim.given <- zlim
    }
    gave.ylim <- "ylim" %in% names(dots)
    ylim.given <- if (gave.ylim) dots[["ylim"]] else NULL

    images <- 1:12
    timeseries <- 13:22
    spatial <- 23:27
    if (any(!which %in% c(images, timeseries, spatial))) stop("unknown value of 'which'")

    adorn.length <- length(adorn)
    if (adorn.length == 1) {
        adorn <- rep(adorn, lw)
        adorn.length <- lw
    }

    tt <- x$data$ts$time
    class(tt) <- "POSIXct"              # otherwise image() gives warnings
    if (gave.zlim && all(which %in% 5:8)) { # single scale for all
        zlim <- range(abs(as.numeric(x$data$ma[,,which[1]])), na.rm=TRUE)
        for (w in 2:length(which)) {
            zlim <- range(abs(c(zlim, x$data$ma[[which[w]]])), na.rm=TRUE)
        }
    }
    if (any(which %in% images) || margins.as.image) {
        ## scale <- (0.132 + (0.2 - 0.132) * exp(-(lw - 1))) / 0.2
        ##scale <- 0.7
        #w <- (1.5 + par("mgp")[2]) * par("csi") * scale * 2.54 + 0.5
        w <- 1.5 #par("mgp")[2] + 1/2
        lay <- layout(matrix(1:(2*lw), nrow=lw, byrow=TRUE), widths=rep(c(1, lcm(w)), lw))
    } else {
        lay <- layout(cbind(1:lw))
    }
    flip.y <- ytype == "profile" && x$metadata$orientation == "downward"
    for (w in 1:lw) {
        ##cat("which[w]=", which[w], "\n")
        if (which[w] %in% images) {                   # image types
            skip <- FALSE
            if (which[w] %in% 1:(0+x$metadata$number.of.beams)) {    #velocity
                z <- x$data$ma$v[,,which[w]]
                y.look <- if (gave.ylim)
                    ylim.given[1] <= x$data$ss$distance & x$data$ss$distance <= ylim.given[2]
                else rep(TRUE, length(x$data$ss$distance))
                zlim <- if (gave.zlim) zlim.given[w,] else max(abs(x$data$ma$v[,y.look,which[w]]), na.rm=TRUE) * c(-1,1)
                zlab <- if (missing(titles)) ad.beam.name(x, which[w]) else titles[w]
            } else if (which[w] %in% 5:(4+x$metadata$number.of.beams)) { # amplitude
                z <- as.numeric(x$data$ma$a[,,which[w]-4])
                dim(z) <- dim(x$data$ma$a)[1:2]
                y.look <- if (gave.ylim)
                    ylim.given[1] <= x$data$ss$distance & x$data$ss$distance <= ylim.given[2]
                else
                    rep(TRUE, length(x$data$ss$distance))
                zlim <- range(as.numeric(x$data$ma$a[,y.look,]), na.rm=TRUE)
                zlab <- c(expression(a[1]),expression(a[2]),expression(a[3]))[which[w]-4]
            } else if (which[w] %in% 9:(8+x$metadata$number.of.beams)) { # correlation
                z <- as.numeric(x$data$ma$q[,,which[w]-8])
                dim(z) <- dim(x$data$ma$q)[1:2]
                zlim <- c(0, 100)
                zlab <- c(expression(q[1]),expression(q[2]),expression(q[3]))[which[w]-8]
            } else skip <- TRUE
            if (!skip) {
                imagep(x=tt, y=x$data$ss$distance, z=z,
                       zlim=zlim,
                       flip.y=flip.y,
                       col=col,
                       ylab=resizable.label("distance"),
                       xlab="Time",
                       zlab=zlab,
                       draw.time.range=draw.time.range,
                       draw.contours=FALSE,
                       do.layout=FALSE,
                       adorn=adorn[w],
                       mgp=mgp,
                       mar=mar,
                       cex=1,
                       ...)
                draw.time.range <- TRUE
            }
        } else if (which[w] %in% timeseries) { # time-series types
            par(mgp=mgp, mar=mar, cex=cex)
            if (which[w] == 13) oce.plot.ts(x$data$ts$time, x$data$ts$salinity,    ylab="S [psu]",       type='l',
                     draw.time.range=draw.time.range, adorn=adorn[w])
            if (which[w] == 14) oce.plot.ts(x$data$ts$time, x$data$ts$temperature, ylab= expression(paste("T [ ", degree, "C ]")), type='l',
                     draw.time.range=draw.time.range, adorn=adorn[w])
            if (which[w] == 15) oce.plot.ts(x$data$ts$time, x$data$ts$pressure,    ylab="p [dbar]",       type='l',
                     draw.time.range=draw.time.range, adorn=adorn[w])
            if (which[w] == 16) oce.plot.ts(x$data$ts$time, x$data$ts$heading,     ylab="heading", type='l',
                     draw.time.range=draw.time.range, adorn=adorn[w])
            if (which[w] == 17) oce.plot.ts(x$data$ts$time, x$data$ts$pitch,       ylab="pitch",   type='l',
                     draw.time.range=draw.time.range, adorn=adorn[w])
            if (which[w] == 18) oce.plot.ts(x$data$ts$time, x$data$ts$roll,        ylab="roll",    type='l',
                     draw.time.range=draw.time.range, adorn=adorn[w])
            if (which[w] == 19) {
                if (x$metadata$number.of.beams > 0)
                    oce.plot.ts(x$data$ts$time, apply(x$data$ma$v[,,1], 1, mean, na.rm=TRUE),
                                ylab=ad.beam.name(x, 1),
                                type='l', draw.time.range=draw.time.range, cex.axis=cex,
                                adorn=adorn[w], ...)
                else warning("cannot plot beam/velo 1 because the device no beams")
            }
            if (which[w] == 20) {
                if (x$metadata$number.of.beams > 1)
                    oce.plot.ts(x$data$ts$time, apply(x$data$ma$v[,,2], 1, mean, na.rm=TRUE),
                                ylab=ad.beam.name(x, 2),
                                type='l', draw.time.range=draw.time.range,
                                adorn=adorn[w], ...)
                else warning("cannot plot beam/velo 2 because the device has only", x$metadata$number.of.beams, "beams")
            }
            if (which[w] == 21) {
                if (x$metadata$number.of.beams > 2)
                    oce.plot.ts(x$data$ts$time, apply(x$data$ma$v[,,3], 1, mean, na.rm=TRUE),
                                ylab=ad.beam.name(x, 3),
                                type='l', draw.time.range=draw.time.range,
                                adorn=adorn[w], ...)
                else warning("cannot plot beam/velo 3 because the device has only", x$metadata$number.of.beams, "beams")
            }
            if (which[w] == 22) {
                if (x$metadata$number.of.beams > 3)
                    oce.plot.ts(x$data$ts$time, apply(x$data$ma$v[,,4], 1, mean, na.rm=TRUE),
                                ylab=ad.beam.name(x, 4),
                                type='l', draw.time.range=draw.time.range,
                                adorn=adorn[w], ...)
                else warning("cannot plot beam/velo 4 because the device has only", x$metadata$number.of.beams, "beams")
            }
            draw.time.range <- FALSE
            if (margins.as.image)  {
                ## blank plot, to get axis length same as for images
                omar <- par("mar")
                par(mar=c(mar[1], 1/4, mgp[2]+1/2, mgp[2]+1))
                plot(1:2, 1:2, type='n', axes=FALSE, xlab="", ylab="")
                par(mar=omar)
            }
        } else if (which[w] %in% spatial) {                   # various spatial types
            if (which[w] == 23) {                             # progressive-vector
                par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
                dt <- as.numeric(diff(x$data$ts$time[1:2],units="sec"))
                m.per.km <- 1000
                x.dist <- cumsum(apply(x$data$ma$v[,,1], 1, mean, na.rm=TRUE)) * dt / m.per.km
                y.dist <- cumsum(apply(x$data$ma$v[,,2], 1, mean, na.rm=TRUE)) * dt / m.per.km
                plot(x.dist, y.dist, xlab="km", ylab="km", type='l', asp=1, ...)
            } else if (which[w] == 24) {
                par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
                value <- apply(x$data$ma$v[,,1], 2, mean, na.rm=TRUE)
                plot(value, x$data$ss$distance, xlab=ad.beam.name(x, 1), ylab="Distance [m]", type='l', ...)
            } else if (which[w] == 25) {
                par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
                value <- apply(x$data$ma$v[,,2], 2, mean, na.rm=TRUE)
                plot(value, x$data$ss$distance, xlab=ad.beam.name(x, 2), ylab="Distance [m]", type='l', ...)
            } else if (which[w] == 26) {
                par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
                value <- apply(x$data$ma$v[,,3], 2, mean, na.rm=TRUE)
                plot(value, x$data$ss$distance, xlab=ad.beam.name(x, 3), ylab="Distance [m]", type='l', ...)
                ##grid()
            } else if (which[w] == 27) {
                if (x$metadata$number.of.beams > 3) {
                    par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
                    value <- apply(x$data$ma$v[,,4], 2, mean, na.rm=TRUE)
                    plot(value, x$data$ss$distance, xlab=ad.beam.name(x, 4), ylab="Distance [m]", type='l', ...)
                    ##grid()
                } else warning("cannot use which=27 because this device did not have 4 beams")
            }
            if (w <= adorn.length) {
                t <- try(eval(adorn[w]), silent=TRUE)
                if (class(t) == "try-error") warning("cannot evaluate adorn[", w, "]\n")
            }
        }
    }
}

adp.beam.attenuate <- function(x, count2db=c(0.45, 0.45, 0.45, 0.45))
{
    if (!inherits(x, "adp")) stop("method is only for adp objects")
    if (x$metadata$oce.beam.attenuated) stop("the beams are already attenuated in this dataset")
    res <- x
    num.profiles <- dim(x$data$ma$a)[1]
    correction <- matrix(rep(20 * log10(x$data$ss$distance), num.profiles),
                         nrow=num.profiles, byrow=TRUE)
    for (beam in 1:x$metadata$number.of.beams)
        res$data$ma$a[,,beam] <- as.raw(count2db[1] * as.numeric(x$data$ma$a[,,beam]) + correction)
    res$metadata$oce.beam.attenuated <- TRUE
    log.action <- paste(deparse(match.call()), sep="", collapse="")
    processing.log.append(res, log.action)
}

adp.beam2frame <- function(x)
{
    if (!inherits(x, "adp")) stop("method is only for objects of class \"adp\"")
    if (x$metadata$oce.coordinate != "beam") stop("input must be in beam coordinates")
    if (inherits(x, "rdi")) {
        vprime <- array(dim=dim(x$data$ma$v))
        c <- if(x$metadata$beam.pattern == "convex") 1 else -1;
        a <- 1 / (2 * sin(x$metadata$beam.angle * pi / 180))
        b <- 1 / (4 * cos(x$metadata$beam.angle * pi / 180))
        d <- a / sqrt(2)
        vprime[,,1] <- -c * a * (x$data$ma$v[,,1] - x$data$ma$v[,,2])
        vprime[,,2] <-  c * a * (x$data$ma$v[,,4] - x$data$ma$v[,,3])
        vprime[,,3] <- -b * (x$data$ma$v[,,1] + x$data$ma$v[,,2] + x$data$ma$v[,,3] + x$data$ma$v[,,4])
        ## FIXME Dal people use 'a' in e, and RDI has two definitions!
        vprime[,,4] <-  d * (x$data$ma$v[,,1] + x$data$ma$v[,,2] - x$data$ma$v[,,3] - x$data$ma$v[,,4])
        res <- x
        res$data$ma$v <- vprime
    } else if (inherits(x, "nortek")) {
        res <- x
        tr.mat <- x$metadata$beam.to.xyz
        if (x$metadata$orientation == "downward") { # flip sign of rows 2 and 3
            ## http://woodshole.er.usgs.gov/pubs/of2005-1429/MFILES/AQDPTOOLS/beam2enu.m
            tr.mat[2,] <- -tr.mat[2,]
            tr.mat[3,] <- -tr.mat[3,]
        } else if (x$metadata$orientation != "upward")
            stop("beam orientation must be \"upward\" or \"downward\", but is \"", x$metadata$orientation, "\"")
        np <- dim(x$data$ma$v)[1]
        nc <- dim(x$data$ma$v)[2]
        transformed <- array(unlist(lapply(1:np, function(p) tr.mat %*% t(x$data$ma$v[p,,1:3]))), dim=c(3, nc, np))
        res$data$ma$v[,,1] <- t(transformed[1,,])
        res$data$ma$v[,,2] <- t(transformed[2,,])
        res$data$ma$v[,,3] <- t(transformed[3,,])
    } else {
        stop("adp type must be either \"rdi\" or \"nortek\"")
    }
    res$metadata$oce.coordinate <- "frame"
    log.action <- paste(deparse(match.call()), sep="", collapse="")
    processing.log.append(res, log.action)
}

adp.frame2earth <- function(x)
{
    if (!inherits(x, "adp")) stop("method is only for adp objects")
    if (x$metadata$oce.coordinate != "frame") stop("input must be in frame coordinates")
    res <- x
    heading <- res$data$ts$heading
    pitch <- res$data$ts$pitch
    roll <- res$data$ts$roll
    if (res$metadata$orientation == "down") {
        pitch <- -pitch
        roll <- -roll
    }
    to.radians <- pi / 180
    CH <- cos(to.radians * heading)
    SH <- sin(to.radians * heading)
    CP <- cos(to.radians * pitch)
    SP <- sin(to.radians * pitch)
    CR <- cos(to.radians * roll)
    SR <- sin(to.radians * roll)
    np <- dim(x$data$ma$v)[1]
    nc <- dim(x$data$ma$v)[2]
    tr.mat <- array(dim=c(3, 3, np))
    tr.mat[1,1,] <-  CH * CR + SH * SP * SR
    tr.mat[1,2,] <-  SH * CP
    tr.mat[1,3,] <-  CH * SR - SH * SP * CR
    tr.mat[2,1,] <- -SH * CR + CH * SP * SR
    tr.mat[2,2,] <-  CH * CP
    tr.mat[2,3,] <- -SH * SR - CH * SP * CR
    tr.mat[3,1,] <- -CP * SR
    tr.mat[3,2,] <-  SP
    tr.mat[3,3,] <-  CP * CR
    rotated <- array(unlist(lapply(1:np, function(p) tr.mat[,,p] %*% t(x$data$ma$v[p,,1:3]))), dim=c(3, nc, np))
    res$data$ma$v[,,1] <- t(rotated[1,,])
    res$data$ma$v[,,2] <- t(rotated[2,,])
    res$data$ma$v[,,3] <- t(rotated[3,,])
    res$metadata$oce.coordinate <- "earth"
    log.action <- paste(deparse(match.call()), sep="", collapse="")
    processing.log.append(res, log.action)
}

adp.earth2other <- function(x, heading=0, pitch=0, roll=0)
{
    if (!inherits(x, "adp")) stop("method is only for adp objects")
    if (x$metadata$oce.coordinate != "earth") stop("input must be in earth coordinates")
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
    np <- dim(x$data$ma$v)[1]
    nc <- dim(x$data$ma$v)[2]
    rotated <- array(unlist(lapply(1:np, function(p) tr.mat %*% t(x$data$ma$v[p,,1:3]))), dim=c(3, nc, np))
    res$data$ma$v[,,1] <- t(rotated[1,,])
    res$data$ma$v[,,2] <- t(rotated[2,,])
    res$data$ma$v[,,3] <- t(rotated[3,,])
    res$metadata$oce.coordinate <- "other"
    log.action <- paste(deparse(match.call()), sep="", collapse="")
    processing.log.append(res, log.action)
}


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

read.header.nortek <- function(file, debug=FALSE)
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
            stop("expecting sync code 0x", sync.code, " at byte ", seek(file)-1, " but got 0x", buf[1], " instead (while reading header #", header, ")")
        if (two.bytes[2] == id.hardware.configuration) {         # see page 29 of System Integrator Guide
            if (debug) cat("** scanning Hardware Configuration **\n")
            buf <- readBin(file, "raw", header.length.hardware)
            if (buf[2] != 0x05) stop("byte 2 must be 0x05 but is 0x", buf[2])
            hardware$size <- readBin(buf[3:4], "integer",signed=FALSE, n=1, size=2)
            if (debug) cat("  hardware$size=", hardware$size, "\n")
            hardware$serial.number <- gsub(" *$", "", paste(readBin(buf[5:18], "character", n=14, size=1), collapse=""))
            if (debug) cat("  hardware$serial.number", hardware$serial.number, "\n")
            hardware$config <- readBin(buf[19:20], "raw", n=2, size=1)
            if (debug) cat("  hardware$config:", hardware$config, "\n")
            hardware$frequency <- readBin(buf[21:22], "integer", n=1, size=2, endian="little", signed=FALSE) # not used
            if (debug) cat("  hardware$frequency:", hardware$frequency, "\n")
            hardware$pic.version <- readBin(buf[23:24], "integer", n=1, size=2, endian="little")
            if (debug) cat("  hardware$pic.version=", hardware$pic.version, "\n")
            hardware$hw.revision <- readBin(buf[25:26], "integer", n=1, size=2, endian="little")
            if (debug) cat("  hardware$hw.revision=", hardware$hw.revision, "\n")
            hardware$rec.size <- readBin(buf[27:28], "integer", n=1, size=2, endian="little")
            if (debug) cat("  hardware$rec.size=", hardware$rec.size, "\n")
            hardware$velocity.range <- readBin(buf[29:30], "integer", n=1, size=2, endian="little")
            if (debug) cat("  hardware$velocity.range=", hardware$velocity.range, "\n")
            hardware$fw.version <- as.numeric(paste(readBin(buf[43:46], "character", n=4, size=1), collapse=""))
            if (debug) cat("  hardware$fw.version=", hardware$fw.version, "\n")
        } else if (two.bytes[2] == id.head.configuration) {     # see page 30 of System Integrator Guide
            if (debug) cat("** scanning Head Configuration **\n")
            buf <- readBin(file, "raw", header.length.head)
            head$size <- readBin(buf[3:4], "integer",signed=FALSE, n=1, size=2)
            if (debug) cat("  head$size=", head$size, "\n")
            head$config <- byte2binary(buf[5:6], endian="little")
            if (debug) cat("  head$config=", head$config, "\n")
            head$config.pressure.sensor <- substr(head$config[1], 1, 1) == "1"
            if (debug) cat("  head$config.pressure.sensor=", head$config.pressure.sensor,"\n")
            head$config.magnetometer.sensor <- substr(head$config[1], 2, 2) == "1"
            if (debug) cat("  head$config.magnetometer.sensor=", head$config.magnetometer.sensor,"\n")
            head$config.tilt.sensor <- substr(head$config[1], 3, 3) == "1"
            if (debug) cat("  head$config.tilt.sensor=", head$config.tilt.sensor,"\n")
            head$orientation <- if (substr(head$config[1], 4, 4) == "1") "downward" else "upward"
            if (debug) cat("  head$orientation=", head$orientation, "\n")
            head$frequency <- readBin(buf[7:8], "integer", n=1, size=2, endian="little", signed=FALSE)
            if (debug) cat("  head$frequency=", head$frequency, "kHz\n")
            head$head.type <- readBin(buf[9:10], "integer", n=1, size=2, endian="little")
            if (debug) cat("  head$head.type=", head$head.type, "\n")
            head$head.serial.number <- gsub(" *$", "", paste(readBin(buf[11:22], "character", n=12, size=1), collapse=""))
            if (debug) cat("  head$head.serial.number=", head$head.serial.number, "\n")
            head$beam.angles <- readBin(buf[23:30], "integer", n=4, size=2, endian="little", signed=TRUE) / 32767 * pi
            if (debug) cat("  head$beam.angles=", head$beam.angles, "(rad)\n")

            ## short hBeamToXYZ[9];          // beam to XYZ transformation matrix for up orientation
            ##Transformation matrix (before division by 4096) -- checks out ok
            ## 6461 -3232 -3232
            ##    0 -5596  5596
            ## 1506  1506  1506
            head$beam.to.xyz <- matrix(readBin(buf[31:48], "integer", n=9, size=2, endian="little") , nrow=3, byrow=TRUE) / 4096
            if (debug) {cat("  head$beam.to.xyz\n");print(head$beam.to.xyz);}
            head$number.of.beams <- readBin(buf[221:222], "integer", n=1, size=2, endian="little")
            if (debug) cat("  head$number.of.beams=", head$number.of.beams, "\n")
        } else if (two.bytes[2] == id.user.configuration) {     # User Configuration [p30-32 of System Integrator Guide]
            if (debug) cat("** scanning User Configuration **\n")
            buf <- readBin(file, "raw", header.length.user)
            user$blanking.distance <- readBin(buf[7:8], "integer", n=1, size=2, endian="little", signed=FALSE)
            if (debug) cat("  user$blanking.distance=", user$blanking.distance, "??? expect 0.05 m\n")
            user$measurement.interval <- readBin(buf[39:40], "integer", n=1, size=2, endian="little")
            if (debug) cat("  user$measurement.interval=", user$measurement.interval, "\n")
            user$T1 <- readBin(buf[5:6], "integer", n=1, size=2, endian="little")
            user$T2 <- readBin(buf[7:8], "integer", n=1, size=2, endian="little")
            user$T3 <- readBin(buf[9:10], "integer", n=1, size=2, endian="little")
            user$T4 <- readBin(buf[11:12], "integer", n=1, size=2, endian="little")
            user$T5 <- readBin(buf[13:14], "integer", n=1, size=2, endian="little")
            user$NPings <- readBin(buf[15:16], "integer", n=1, size=2, endian="little")
            user$AvgInterval <- readBin(buf[17:18], "integer", n=1, size=2, endian="little")
            user$number.of.beams <- readBin(buf[19:20], "integer", n=1, size=2, endian="little")
            if (debug) cat("\n user$T1=",user$T1,"user$T2=",user$T2,"user$T5=",user$T5,"user$NPings=",user$NPings,"user$AvgInterval=",user$AvgInterval,"user$number.of.beams=",user$number.of.beams,"\n\n")
            user$mode <- byte2binary(buf[59:60], endian="little")
            if (debug) cat("  user$mode: ", user$mode, "\n")
            user$velocity.scale <- if (substr(user$mode[2], 4, 4) == "0") 0.001 else 0.00001
            if (debug) cat("  user$velocity.scale: ", user$velocity.scale, "\n")
            tmp.cs <- readBin(buf[33:34], "integer", n=1, size=2, endian="little")
            if (tmp.cs == 0) user$coordinate.system <- "earth" # page 31 of System Integrator Guide
            else if (tmp.cs == 1) user$coordinate.system <- "frame"
            else if (tmp.cs == 2) user$coordinate.system <- "beam"
            else stop("unknown coordinate system ", tmp.cs)
            if (debug) cat("  user$coordinate.system: ", user$coordinate.system, "\n")
            user$number.of.cells <- readBin(buf[35:36], "integer", n=1, size=2, endian="little")
            if (debug) cat("  user$number.of.cells: ", user$number.of.cells, "\n")
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
            if (debug) cat("cell.size=", user$cell.size, "m\n")
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
            if (debug) cat("blanking.distance=", user$blanking.distance, "; user$T1=", user$T1, "and user$T2=", user$T2, "\n")
            if (debug) cat("measurement.interval=", user$measurement.interval, "\n")
            user$deployment.name <- readBin(buf[41:46], "character")
            user$sw.version <- readBin(buf[73:74], "integer", n=1, size=2, endian="little")
            if (debug) cat("sw.version=", user$sw.version,"\n")
            user$salinity <- readBin(buf[75:76], "integer", n=1, size=2, endian="little") * 0.1
            if (debug) cat("salinity=", user$salinity,"\n")
        } else {
            stop("cannot understand byte 0x", two.bytes[2], "; expecting one of the following: 0x", id.hardware.configuration, " [hardware configuration] 0x", id.head.configuration, " [head configuration] or 0x", id.user.configuration, " [user configuration]\n")
        }
    }
    list(hardware=hardware, head=head, user=user)
}

read.adp.nortek <- function(file, from=0, to, by=1,
                             type=c("aquadopp high resolution"),
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
        if (debug) cat("\n*** should read 'High Resolution Aquadopp Profile Data' now -- TESTING ONLY!! **\n\n")
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
        if (debug) cat("from=",from,"\n")
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
        if (to < 0) stop("cannot have to < 0")
        if (debug) cat("to=",to,"\n")
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
            if (debug) cat("successfully read profile", i, "at time ", format(p$time), "\n")
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
        attr(time, "tzone") <- "UTC" # BUG should let user control this
                                        # Q: does file hold the zone?

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
                     roll=roll)
                     )

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
    res <- list(data=data, metadata=metadata, processing.log=log.item)
    class(res) <- c("adp", "nortek", "oce")
    res
}                                       # read.adp.nortek()
