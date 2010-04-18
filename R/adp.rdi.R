read.header.rdi <- function(file, debug=getOption("oce.debug"), ...)
{
    ##
    ## header, of length 6 + 2 * number.of.data.types bytes
    ##
    oce.debug(debug, "read.header.rdi() starting at file position", seek(file), "\n")
    header.part1 <- readBin(file, "raw", n=6, size=1)
    oce.debug(debug, paste("first 6 bytes of header:", paste(header.part1, collapse=' '), "\n"))
    header <- header.part1
    if (header.part1[1] != 0x7f) {
        print(header)
        stop("first byte in file must be 0x7f, but it was 0x", header.part1[1])
    }
    if (header.part1[2] != 0x7f) stop("second byte in file must be 0x7f but it was 0x", header.part1[2])
    num.bytes.in.ensemble <- readBin(header.part1[3:4], "integer", n=1, size=2, endian="little")
    oce.debug(debug, "num.bytes.in.ensemble=", num.bytes.in.ensemble, "\n")
    ## header.part1[5] spare
    number.of.data.types <- readBin(header.part1[6], "integer", n=1, size=1)
    if (number.of.data.types < 1) stop("cannot have ", number.of.data.types, " data types, as header indicates")
    oce.debug(debug, "number.of.data.types=", number.of.data.types, "\n")
    ## part 2 of header is these data offsets
    header.part2 <- readBin(file, "raw", n=2*number.of.data.types, size=1)
    ##cat("after reading header, seek(file)=", seek(file), "\n", ...)
    header <- c(header, header.part2)
    data.offset <- readBin(header.part2, "integer", n=number.of.data.types, size=2, endian="little")
    oce.debug(debug, "data.offset=", paste(data.offset, sep=" "), "\n")
    ##
    ## FLD (fixed leader data) 59 bytes
    ## OLD: FLD <- readBin(file, "raw", n=59, size=1) # binary fixed leader data (Figure D-5)
    FLD <- readBin(file, "raw", n=data.offset[2]-data.offset[1], size=1)
    ##cat("after reading FLD, ftell=", seek(file), "\n", ...)

    header <- c(header, FLD)
    oce.debug(debug, "fixed leader data (59 bytes):", paste(FLD, collapse=" "), "\n")
    if (FLD[1] != 0x00) stop("first byte of fixed leader header must be 0x00 but it was ", FLD[1])
    if (FLD[2] != 0x00) stop("second byte of fixed leader header must be a0x00 but it was ", FLD[2])
    fv <- readBin(FLD[3], "integer", n=1, size=1, signed=FALSE)
    fr <- readBin(FLD[4], "integer", n=1, size=1, signed=FALSE)
    ##program.version <- paste(fv, fr, sep=".") # don't want to rely on number of digits
    oce.debug(debug, "program version=", paste(fv,fr,sep="."), "\n")
    system.configuration <- paste(byte2binary(FLD[5]), byte2binary(FLD[6]),sep="-")
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
    ##cat("BITS='",bits,"'\n",sep="", ...)
    beam.config <- "?"
    bits <- substr(system.configuration, 10, 13)
    if (bits == "0100") beam.config <- "janus"
    else if (bits == "0101") beam.config <- "janus demod"
    else if (bits == "1111") beam.config <- "janus 2 demd"
    bits <- substr(system.configuration, 1, 1)
    if (bits == "1") orientation <- "upward"
    else orientation <- "downward"
    ##cat("beam.config=", beam.config, "\n", ...)
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
    else if (bits == "10") coordinate.system <- "xyz"
    else if (bits == "11") coordinate.system <- "enu"
    heading.alignment <- 0.01 * readBin(FLD[27:28], "integer", n=1, size=2, endian="little")
    heading.bias <- 0.01 * readBin(FLD[29:30], "integer", n=1, size=2, endian="little")
    sensor.source <- readBin(FLD[31], "integer", n=1, size=1)
    sensors.available <- readBin(FLD[32], "integer", n=1, size=1)
    bin1.distance <- readBin(FLD[33:34], "integer", n=1, size=2, endian="little", signed=FALSE) * 0.01
    ##cat("bin1.distance being inferred from 0x", FLD[33:34], " as ", bin1.distance, "\n", sep="", ...)
    xmit.pulse.length <- readBin(FLD[35:36], "integer", n=1, size=2, endian="little", signed=FALSE) * 0.01
    ##cat("xmit.pulse.length being inferred from 0x", FLD[35:36], " as ", xmit.pulse.length, "\n", sep="", ...)
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
    oce.debug(debug, paste("CPU.BOARD.SERIAL.NUMBER = '", paste(cpu.board.serial.number, collapse=""), "'\n", sep=""))
    system.bandwidth <- readBin(FLD[51:52], "integer", n=1, size=2, endian="little")
    system.power <- readBin(FLD[53], "integer", n=1, size=1)
    ## FLD[54] spare
    serial.number <- readBin(FLD[55:58], "integer", n=1, size=4, endian="little")
    oce.debug(debug, "SERIAL NUMBER", serial.number, "\n")
    if (serial.number == 0) serial.number <- c(cpu.board.serial.number, "(CPU board)") # FIXME: where is serial #?

    ##beam.angle <- readBin(FLD[59], "integer", n=1, size=1) # NB 0 in first test case
    ##cat("BEAM ANGLE=", FLD[59], "or", beam.angle, "\n", ...)

    ##
    ## VLD (variable leader data) 65 bytes
    ##
    VLD <- readBin(file, "raw", n=65, size=1)
    header <- c(header, VLD)
    ##cat("position in file=", seek(file, NA), "after reading VLD\n", ...)
    oce.debug(debug, "variable leader data (65 bytes):", paste(VLD, collapse=" "), "\n")
    ## ensure that header is not ill-formed
    if (VLD[1] != 0x80) stop("byte 1 of variable leader data should be 0x80, but it is ", VLD[1])
    if (VLD[2] != 0x00) stop("byte 2 of variable leader data should be 0x00, but it is ", VLD[2])
    ensemble.number <- readBin(VLD[3:4], "integer", n=1, size=2, endian="little")
    ## Assemble the time.  This follows section 5.3 (paper 132, file page 140) of "Workhorse Commands and Output Data Format_Nov07.pdf"

    ## FIXME: probably would save time to read all elements at once.  Instrument to check

    RTC.year <- readBin(VLD[5], "integer", n=1, size=1)
    if (RTC.year < 1800) RTC.year <- RTC.year + 2000 # fix Y2K problem
    RTC.month <- readBin(VLD[6], "integer", n=1, size=1)
    RTC.day <- readBin(VLD[7], "integer", n=1, size=1)
    RTC.hour <- readBin(VLD[8], "integer", n=1, size=1)
    RTC.minute <- readBin(VLD[9], "integer", n=1, size=1)
    RTC.second <- readBin(VLD[10], "integer", n=1, size=1)
    RTC.hundredths <- readBin(VLD[11], "integer", n=1, size=1)
    time <- ISOdatetime(RTC.year, RTC.month, RTC.day, RTC.hour, RTC.minute, RTC.second + RTC.hundredths / 100, tz = "UTC") # not sure on TZ
    oce.debug(debug, "profile time=", format(time), "inferred from RTC.year=", RTC.year,
              "RTC.month=", RTC.month, "RTC.day-", RTC.day, "RTC.hour=", RTC.hour,
              "RTC.minute=", RTC.minute, "RTC.second=", RTC.second, "RTC.hundreds=", RTC.hundredths, "\n")



    ##if (TRUE) { # read the second time indication (my tests shows the values are the same)
    ##    second.RTC.century <- readBin(VLD[58], "integer", n=1, size=1, signed=FALSE)
    ##    oce.debug(debug, "second.RTC.century:", second.RTC.century, "\n")
    ##    second.RTC.year <- readBin(VLD[59], "integer", n=1, size=1, signed=FALSE)
    ##    oce.debug(debug, "second.RTC.year:", second.RTC.year, "\n")
    ##    second.RTC.month <- readBin(VLD[60], "integer", n=1, size=1, signed=FALSE)
    ##    oce.debug(debug, "second.RTC.month:", second.RTC.month, "\n")
    ##    second.RTC.day <- readBin(VLD[61], "integer", n=1, size=1, signed=FALSE)
    ##    oce.debug(debug, "second.RTC.day:", second.RTC.day, "\n")
    ##    second.RTC.hour <- readBin(VLD[62], "integer", n=1, size=1, signed=FALSE)
    ##    oce.debug(debug, "second.RTC.hour:", second.RTC.hour, "\n")
    ##    second.RTC.minute <- readBin(VLD[63], "integer", n=1, size=1, signed=FALSE)
    ##    oce.debug(debug, "second.RTC.minute:", second.RTC.minute, "\n")
    ##    second.RTC.second <- readBin(VLD[64], "integer", n=1, size=1, signed=FALSE)
    ##    oce.debug(debug, "second=", VLD[64], "(as raw)", "or", as.integer(VLD[64]), "(as integer)\n")
    ##    oce.debug(debug, "second.RTC.second:", second.RTC.second, "\n")
    ##    second.RTC.hundredths <- readBin(VLD[65], "integer", n=1, size=1, signed=FALSE)
    ##    oce.debug(debug, "second.RTC.hundredths:", second.RTC.hundredths, "\n")
    ##    second.time <- ISOdatetime(second.RTC.year, second.RTC.month, second.RTC.day, second.RTC.hour, second.RTC.minute, second.RTC.second + second.RTC.hundredths / 100, tz = "UTC") # not sure on TZ
    ##    oce.debug(debug, "secondtime:", format(second.time), "\n")
    ##}

    ensemble.number.MSB <- readBin(VLD[12], "integer", n=1, size=1)
    bit.result <- readBin(VLD[13:14], "integer", n=1, size=2, endian="little")
    speed.of.sound  <- readBin(VLD[15:16], "integer", n=1, size=2, endian="little")
    if (speed.of.sound < 1400 || speed.of.sound > 1600) warning("speed of sound is ", speed.of.sound, ", which is outside the permitted range of 1400 m/s to 1600 m/s")
    depth.of.transducer <- readBin(VLD[17:18], "integer", n=1, size=2, endian="little") * 0.1
    oce.debug(debug, "depth of transducer:", depth.of.transducer, "\n")
    heading <- readBin(VLD[19:20], "integer", n=1, size=2, endian="little", signed=FALSE) * 0.01
    if (heading < 0 || heading > 360) warning("heading ", heading, " should be between 0 and 360 degrees, inclusive")
    pitch <- readBin(VLD[21:22], "integer", n=1, size=2, endian="little") * 0.01
    roll <- readBin(VLD[23:24], "integer", n=1, size=2, endian="little") * 0.01
    oce.debug(debug, "heading=", heading, " pitch=", pitch, " roll=", roll, "\n")
    salinity <- readBin(VLD[25:26], "integer", n=1, size=2, endian="little")
    if (salinity < 0 || salinity > 40) warning("salinity is ", salinity, ", which is outside the permitted range of 0 to 40 PSU")
    temperature <- readBin(VLD[27:28], "integer", n=1, size=2, endian="little") * 0.01
    if (temperature < -5 || temperature > 40) warning("temperature is ", temperature, ", which is outside the permitted range of -5 to 40 degC")

    ## Skipping a lot ...
    pressure <- readBin(VLD[49:52], "integer", n=1, size=4, endian="little", signed=FALSE) * 0.001

    if (0) {#debug) {
        cat("PRESSURE: ", VLD[49:52], "\n", ...)
        cat(" little/signed   ", readBin(VLD[49:52], "integer", n=1, size=4, endian="little", signed=TRUE),"\n", ...)
        cat(" little/unsigned ", readBin(VLD[49:52], "integer", n=1, size=4, endian="little", signed=FALSE),"\n", ...)
        cat(" big/signed      ", readBin(VLD[49:52], "integer", n=1, size=4, endian="big",    signed=TRUE),"\n", ...)
        cat(" big/unsigned    ", readBin(VLD[49:52], "integer", n=1, size=4, endian="big",    signed=FALSE),"\n", ...)
        cat("\nbyte by byte:", ...)
        cat(readBin(VLD[49], "integer", n=1, size=1, signed=FALSE), " ", ...)
        cat(readBin(VLD[50], "integer", n=1, size=1, signed=FALSE), " ", ...)
        cat(readBin(VLD[51], "integer", n=1, size=1, signed=FALSE), " ", ...)
        cat(readBin(VLD[52], "integer", n=1, size=1, signed=FALSE), " ", ...)
        cat("\n", ...)
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
    oce.debug(debug, "read.profile.rdi() entry, at file position", seek(file), "\n")
    if (missing(header)) {
        oce.debug(debug, "missing header, so reading it now...\n")
        header <- read.header.rdi(file, debug=debug-1)
    } else {
        oce.debug(debug, "using known header; no need to read a new one.\n")
        junk <- readBin(file, "raw", n=header$header.length, size=1)
    }
    ## velocity, should start with 0x00 then 0x01
    v.ID <- readBin(file, "raw", n=2, size=1)
    if (v.ID[1] != 0x00) stop("first byte of velocity segment should be 0x00 but is ", v.ID[1], " at file position ", seek(file)-2)
    if (v.ID[2] != 0x01) stop("first byte of velocity segment should be 0x01 but is ", v.ID[2], " at file position ", seek(file)-1)
    oce.debug(debug, "got velo\n")
    v <- readBin(file, "integer", n=header$number.of.beams * header$number.of.cells, size=2, endian="little")
    v[v==(-32768)] <- NA       # blank out bad data
    v <- matrix(v / 1000, ncol=header$number.of.beams, byrow=TRUE)
    ## correlation magnitude, should start with 0x00 0x01
    q.ID <- readBin(file, "raw", n=2, size=1)
    if (q.ID[1] != 0x00) stop("first byte of correlation-magnitude segment should be 0x00 but is ", q.ID[1])
    if (q.ID[2] != 0x02) stop("first byte of correlation-magnitude segment should be 0x02 but is ", q.ID[2])
    q <- matrix(readBin(file, "raw", n=header$number.of.beams * header$number.of.cells),
                ncol=header$number.of.beams, byrow=TRUE)
    oce.debug(debug, "got correlation magnitude\n")
    ## echo intensity, should start with 0x00 0x03
    a.ID <- readBin(file, "raw", n=2, size=1)
    if (a.ID[1] != 0x00) stop("first byte of echo-intensity segment should be 0x00 but is ", a.ID[1])
    if (a.ID[2] != 0x03) stop("first byte of echo-intensity segment should be 0x03 but is ", a.ID[2])
    a <- matrix(readBin(file, "raw", n=header$number.of.beams * header$number.of.cells),
                ncol=header$number.of.beams, byrow=TRUE)
    oce.debug(debug, "got echo intensity\n")
    ## percent good, should start with 0x00 0x04
    pg.ID <- readBin(file, "raw", n=2, size=1)
    if (pg.ID[1] != 0x00) stop("first byte of percent-good segment should be 0x00 but is ", pg.ID[1])
    if (pg.ID[2] != 0x04) stop("first byte of percent-good segment should be 0x04 but is ", pg.ID[2])
    pg <- matrix(readBin(file, "raw", n=header$number.of.beams * header$number.of.cells),
                 ncol=header$number.of.beams, byrow=TRUE)
    oce.debug(debug, "got percent-good\n")
    bt <- NULL
    if (header$number.of.data.types > 6) {
        stop("not yet coded to read bottom track")
        ## need to check if bottom track.  I think that's only if
        ## num.data.types exceeds 6
        ## bottom track, 85 bytes (always present??) ID=0x00 0x06
        bt <- NULL
    }
    reserved <- readBin(file, "raw", n=2, size=1)
    oce.debug(debug, "reserved=", paste(reserved, collapse=" "), "\n")
    checksum <- readBin(file, "raw", n=2, size=1)
    oce.debug(debug, "checksum=", paste(checksum, collapse=" "), "\n")
    list(header=header, v=v, a=a, q=q, pg=pg, bt=bt)
}

decode.profile.rdi <- function(buf, i, debug=getOption("oce.debug"))
{
    stop("NOT USING THIS ANYMORE")
    oce.debug(debug, "read.profile.rdi() entry, at file position", i, "\n")
    ## velocity, should start with 0x00 then 0x01
    if (buf[i] != 0x00) stop("first byte of velocity segment should be 0x00 but is ", buf[i], " at file position ", i)
    if (buf[i+1] != 0x01) stop("first byte of velocity segment should be 0x01 but is ", buf[i+1], " at file position ", i+1)
    return(1)
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
    oce.debug(debug, "got correlation magnitude\n")
    ## echo intensity, should start with 0x00 0x03
    a.ID <- readBin(file, "raw", n=2, size=1)
    if (a.ID[1] != 0x00) stop("first byte of echo-intensity segment should be 0x00 but is ", a.ID[1])
    if (a.ID[2] != 0x03) stop("first byte of echo-intensity segment should be 0x03 but is ", a.ID[2])
    a <- matrix(readBin(file, "raw", n=header$number.of.beams * header$number.of.cells),
                ncol=header$number.of.beams, byrow=TRUE)
    oce.debug(debug, "got echo intensity\n")
    ## percent good, should start with 0x00 0x04
    pg.ID <- readBin(file, "raw", n=2, size=1)
    if (pg.ID[1] != 0x00) stop("first byte of percent-good segment should be 0x00 but is ", pg.ID[1])
    if (pg.ID[2] != 0x04) stop("first byte of percent-good segment should be 0x04 but is ", pg.ID[2])
    pg <- matrix(readBin(file, "raw", n=header$number.of.beams * header$number.of.cells),
                 ncol=header$number.of.beams, byrow=TRUE)
    oce.debug(debug, "got percent-good\n")
    bt <- NULL
    if (header$number.of.data.types > 6) {
        stop("not yet coded to read bottom track")
        ## need to check if bottom track.  I think that's only if
        ## num.data.types exceeds 6
        ## bottom track, 85 bytes (always present??) ID=0x00 0x06
        bt <- NULL
    }
    reserved <- readBin(file, "raw", n=2, size=1)
    oce.debug(debug, "reserved=", paste(reserved, collapse=" "), "\n")
    checksum <- readBin(file, "raw", n=2, size=1)
    oce.debug(debug, "checksum=", paste(checksum, collapse=" "), "\n")
    list(header=header, v=v, a=a, q=q, pg=pg, bt=bt)
}

read.adp.rdi <- function(file, from=0, to, by=1, type=c("workhorse"), debug=getOption("oce.debug"), monitor=TRUE, log.action, ...)
{
    bisect.rdi.adp <- function(t.find, add=0, debug=0) {
        oce.debug(debug, "bisect.rdi.adv(t.find=", format(t.find), ", add=", add, "\n")
        len <- length(profile.start)
        lower <- 1
        upper <- len
        passes <- floor(10 + log(len, 2)) # won't need this many; only do this to catch coding errors
        for (pass in 1:passes) {
            middle <- floor((upper + lower) / 2)
            year   <- 2000 + readBin(buf[profile.start[middle] +  4], what="integer", n=1, size=1, signed=FALSE)
            month  <-        readBin(buf[profile.start[middle] +  5], what="integer", n=1, size=1, signed=FALSE)
            day    <-        readBin(buf[profile.start[middle] +  6], what="integer", n=1, size=1, signed=FALSE)
            hour   <-        readBin(buf[profile.start[middle] +  7], what="integer", n=1, size=1, signed=FALSE)
            minute <-        readBin(buf[profile.start[middle] +  8], what="integer", n=1, size=1, signed=FALSE)
            second <-        readBin(buf[profile.start[middle] +  9], what="integer", n=1, size=1, signed=FALSE)
            sec100 <-        readBin(buf[profile.start[middle] + 10], what="integer", n=1, size=1, signed=FALSE)
            t <- ISOdatetime(year, month, day, hour, minute, second + sec100/100, tz=getOption("oce.tz")) # not sure on TZ
            oce.debug(debug, "t=", format(t), "| y=", year, " m=", month, " d=", format(day, width=2), " h=", format(hour, width=2), " m=", format(minute, width=2), "s=", format(second, width=2), "sec100=", sec100, "| pass", format(pass, width=2), "/", passes, "| middle=", middle, "(", middle/upper*100, "%)\n")
            if (t.find < t)
                upper <- middle
            else
                lower <- middle
            if (upper - lower < 2)
                break
        }
        middle <- middle + add          # may use add to extend before and after window
        if (middle < 1) middle <- 1
        if (middle > len) middle <- len
        t <- ISOdatetime(2000+readBin(buf[profile.start[middle]+4],"integer",size=1,signed=FALSE,endian="little"),  # year
                         as.integer(buf[profile.start[middle]+5]), # month
                         as.integer(buf[profile.start[middle]+6]), # day
                         as.integer(buf[profile.start[middle]+7]), # hour
                         as.integer(buf[profile.start[middle]+8]), # min
                         as.integer(buf[profile.start[middle]+9]), # sec FIXME: should use sec100 too
                         tz=getOption("oce.tz")) # FIXME check on tz
        oce.debug(debug, "result: t=", format(t), " at vsd.start[", middle, "]=", profile.start[middle], "\n")
        return(list(index=middle, time=t))
    }

    oce.debug(debug, "read.adp.rdi(...,from=",format(from),",to=",format(to), "...)\n")
    oce.debug(debug, "class(from)=", class(from), "; class(to)=", class(to), "\n")
    from.keep <- from
    to.keep <- to
    if (is.character(file)) {
        filename <- full.filename(file)
        file <- file(file, "rb")
        on.exit(close(file))
    }
    if (!inherits(file, "connection")) stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        filename <- "(connection)"
        open(file, "rb")
        on.exit(close(file))
    }
    type <- match.arg(type)
    seek(file, 0, "start")
    header <- read.header.rdi(file, debug=debug-1)
    seek(file, 0, "start")
    number.of.beams <- header$number.of.beams
    number.of.cells <- header$number.of.cells
    bin1.distance <- header$bin1.distance
    xmit.pulse.length <- header$xmit.pulse.length
    cell.size <- header$cell.size
    ## go to the end, so the next seek (to get to the data) reveals file length
    seek(file, where=0, origin="end")
    file.size <- seek(file, where=0)
    oce.debug(debug, "file.size=", file.size, "\n")

    if (TRUE) {                         # working spot
        buf <- readBin(file, what="raw", n=file.size, endian="little")
        # vld is "variable leader data"; section 5.3 of Workhorse/RDI 2007
        profile.start <- .Call("match2bytes", buf, 0x80, 0x00, TRUE)
        profiles.in.file <- length(profile.start)
        oce.debug(debug, "profile.start[1:10]=", profile.start[1:10], "(out of ", length(profile.start), ")\n")
        if (inherits(from, "POSIXt")) {
            if (!inherits(to, "POSIXt")) stop("if 'from' is POSIXt, then 'to' must be, also")
            from.pair <- bisect.rdi.adp(from, add=-1, debug=debug-1)
            from <- from.index <- from.pair$index
            to.pair <- bisect.rdi.adp(to, add=1, debug=debug-1)
            to <- to.index <- to.pair$index
            oce.debug(debug, "from=", format(from.pair$t), " yields profile.start[", from.index, "]\n",
                      "  to  =", format(to.pair$t), "yields profile.start[", to.index, "]\n",
                      "  by=", by, "(not yet decoded)\n",
                      "  profile.start[1:10]=", profile.start[1:10],"\n",
                      "  profile.start[",from.pair$index, "]=", profile.start[from.pair$index], "at time", format(from.pair$t), "\n",
                      "  profile.start[",  to.pair$index, "]=", profile.start[  to.pair$index], "at time", format(  to.pair$t), "\n")
            two.times <- ISOdatetime(2000 +readBin(buf[profile.start[1:2]+4],"integer",size=2,signed=FALSE,endian="little"), # year
                                     as.integer(buf[profile.start[1:2]+5]), # month
                                     as.integer(buf[profile.start[1:2]+6]), # day
                                     as.integer(buf[profile.start[1:2]+7]), # hour
                                     as.integer(buf[profile.start[1:2]+8]), # min
                                     as.integer(buf[profile.start[1:2]+9]), # sec
                                     tz=getOption("oce.tz"))
            dt <- as.numeric(difftime(two.times[2], two.times[1], units="secs"))
            oce.debug(debug, "dt=", dt, "s; at this stage, by=", by,"\n")
            if (is.character(by))
                by <- floor(0.5 + ctime.to.seconds(by) / dt)
            oce.debug(debug, "by=",by,"profiles (after decoding)\n")
            profile.start <- profile.start[profile.start[from.index] < profile.start & profile.start < profile.start[to.index]]
            profile.start <- profile.start[seq(1, length(profile.start), by=by)]
        } else {
            from.index <- from
            to.index <- to
            if (to.index < 1 + from.index) stop("need more separation between from and to")
            if (is.character(by)) stop("cannot have string for 'by' if 'from' and 'to' are integers")
            profile.start <- profile.start[seq(from=from, to=to, by=by)]
            oce.debug(debug, "profile.start[1:10] after indexing:", profile.start[1:10], "\n")
        }
        profiles.to.read <- length(profile.start)
        oce.debug(debug, "number.of.beams=",header$number.of.beams,"\n")
    }
    ## 64 is length of Variable Leader Data (Table 33 of rdi manual)
    items <- number.of.beams * number.of.cells
    v <- array(double(), dim=c(profiles.to.read, number.of.cells, number.of.beams))
    a <- array(raw(), dim=c(profiles.to.read, number.of.cells, number.of.beams))
    q <- array(raw(), dim=c(profiles.to.read, number.of.cells, number.of.beams))
    for (i in 1:profiles.to.read) {
        o <- profile.start[i] + 65
        oce.debug(debug, 'getting data chunk',i,' at file position',o,'\n')
        if (buf[o] != 0x00) stop("first byte of velocity segment should be 0x00 but is ", buf[o], " at file position ", o)
        if (buf[o+1] != 0x01) stop("first byte of velocity segment should be 0x01 but is ", buf[o+1], " at file position ", o+1)
        vv <- readBin(buf[o + seq(0, 2*items-1)], "integer", n=items, size=2, endian="little", signed=TRUE)
        vv[vv==(-32768)] <- NA       # blank out bad data
        v[i,,] <- matrix(vv / 1000, ncol=number.of.beams, byrow=TRUE)
        o <- o + items * 2 + 2          # skip over the velo data, plus a checksum; FIXME: use the checksum
        if (buf[o] != 0x00) stop("first byte of correlation segment should be 0x00 but is ", buf[o], " at file position ", o)
        if (buf[o+1] != 0x02) stop("first byte of corrleation segment should be 0x02 but is ", buf[o+1], " at file position ", o+1)
        qq <- matrix(buf[o + seq(0, items-1)], ncol=number.of.beams, byrow=TRUE)
        o <- o + items + 2              # skip over the one-byte data plus a checkum; FIXME: use the checksum
        if (buf[o] != 0x00) stop("first byte of intensity segment should be 0x00 but is ", buf[o], " at file position ", o)
        if (buf[o+1] != 0x03) stop("first byte of intensity segment should be 0x03 but is ", buf[o+1], " at file position ", o+1)
        a[i,,] <- matrix(buf[o + seq(0, items-1)], ncol=number.of.beams, byrow=TRUE)
        o <- o + items + 2              # skip over the one-byte data plus a checkum; FIXME: use the checksum
        if (buf[o] != 0x00) stop("first byte of percent-good segment should be 0x00 but is ", buf[o], " at file position ", o)
        if (buf[o+1] != 0x04) stop("first byte of percent-good segment should be 0x04 but is ", buf[o+1], " at file position ", o+1)
        q[i,,] <- matrix(buf[o + seq(0, items-1)], ncol=number.of.beams, byrow=TRUE)
        if (monitor) {
            cat(".", ...)
            if (!(i %% 50)) cat(i, "\n", ...)
        }
    }
    time <- ISOdatetime(2000+as.integer(buf[profile.start+4]), # year
                        as.integer(buf[profile.start+5]),      # month
                        as.integer(buf[profile.start+6]),      # day
                        as.integer(buf[profile.start+7]),      # hour
                        as.integer(buf[profile.start+8]),      # minute
                        as.integer(buf[profile.start+9]),      # second
                        tz=getOption("oce.tz")) # FIXME: check on tzo
    profile.start2 <- sort(c(profile.start, profile.start + 1)) # lets us index two-byte chunks
    profile.start4 <- sort(c(profile.start, profile.start + 1, profile.start + 2, profile.start + 3)) # lets us index four-byte chunks
    speed.of.sound <- 0.1 * readBin(buf[profile.start2 + 14], "integer", n=profiles.to.read, size=2, endian="little", signed=FALSE)
    depth.of.transducer <- 0.1 * readBin(buf[profile.start2 + 16], "integer", n=profiles.to.read, size=2, endian="little")
    heading <- 0.01 * readBin(buf[profile.start2 + 18], "integer", n=profiles.to.read, size=2, endian="little", signed=TRUE)
    pitch <- 0.01 * readBin(buf[profile.start2 + 20], "integer", n=profiles.to.read, size=2, endian="little", signed=TRUE)
    roll <- 0.01 * readBin(buf[profile.start2 + 22], "integer", n=profiles.to.read, size=2, endian="little", signed=TRUE)
    salinity <- 0.01 * readBin(buf[profile.start2 + 24], "integer", n=profiles.to.read, size=2, endian="little", signed=TRUE)
    temperature <- 0.01 * readBin(buf[profile.start2 + 26], "integer", n=profiles.to.read, size=2, endian="little", signed=TRUE)
    pressure <- 0.001 * readBin(buf[profile.start4 + 48], "integer", n=profiles.to.read, size=4, endian="little", signed=FALSE)
    metadata <- header
    metadata$bin1.distance <- bin1.distance
    metadata$xmit.pulse.length <- xmit.pulse.length
    metadata$sampling.start <- NULL     #FIXME
    metadata$sampling.end <- NULL       #FIXME
    metadata$filename <- filename
    metadata$oce.beam.attenuated <- FALSE
    metadata$oce.coordinate <- header$coordinate.system
    metadata$number.of.beams <- header$number.of.beams
    ## Transformation matrix
    tm.c <- if (metadata$beam.pattern == "convex") 1 else -1;
    tm.a <- 1 / (2 * sin(metadata$beam.angle * pi / 180))
    tm.b <- 1 / (4 * cos(metadata$beam.angle * pi / 180))
    tm.d <- tm.a / sqrt(2)
    ## FIXME Dal people use 'a' in last row of matrix, and RDI has two definitions!
    ##
    ## Notes on coordinate transformation matrix.
    ## From figure 3 on page 12 of ACT (adcp coordinate transformation.pdf)
    ## we have
    ##
    ##    x defined to run from beam 1 to beam 2
    ##    y defined to run from beam 4 to beam 3
    ##    z right-handed from these.
    ##
    ## and the upward-looking orientation (viewed from above) is
    ##
    ##        B3
    ##    B2      B1
    ##        B4
    ##
    ## so we have coords
    ##
    ##            y
    ##            ^
    ##            |
    ##            |
    ##    x <-----*   (z into page, or downward)
    ##
    ## Thus, for the upwards-mounted orientation, we must transform
    ## x to -x and z to -z.  The matrix below is from page 13 (section 5.30
    ## of the ACT.  So, if the orientation is upwards, we need to
    ## change the signs of rows 1 and 3.
    metadata$transformation.matrix <- matrix(c(tm.c*tm.a, -tm.c*tm.a,          0,         0,
                                               0        ,          0, -tm.c*tm.a, tm.c*tm.a,
                                               tm.b     ,       tm.b,       tm.b,      tm.b,
                                               tm.d     ,       tm.d,      -tm.d,     -tm.d),
                                             nrow=4, byrow=TRUE)
    if (metadata$orientation == "upward") {
        metadata$transformation.matrix[1,] <- -metadata$transformation.matrix[1,]
        metadata$transformation.matrix[3,] <- -metadata$transformation.matrix[3,]
    } else if (metadata$orientation == "downward") {
    } else warning("the device orientation should be \"upward\" or \"downward\" but it is", metadata$orientation)
    if (monitor) cat("\nRead", profiles.to.read,  "profiles, out of a total of",profiles.in.file,"profiles in", filename, "\n", ...)
    ##cat("\nfivenum(ei1,na.rm=TRUE)"); print(fivenum(ei1, na.rm=TRUE), ...)
    class(time) <- c("POSIXt", "POSIXct")
    attr(time, "tzone") <- attr(header$time, "tzone")
    data <- list(ma=list(v=v, a=a, q=q),
                 ss=list(distance=seq(bin1.distance, by=cell.size, length.out=number.of.cells)),
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
    class(res) <- c("rdi", "adp", "oce")
    res
}
