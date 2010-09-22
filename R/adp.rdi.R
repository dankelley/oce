## byte sequences at start of items
## FLH 00 00; VLH 00 80; vel 00 01; Cor 00 02;  echo 00 03; percent 00 04; bottom-track 00 06

decode.header.rdi <- function(buf, debug=getOption("oce.debug"), tz=getOption("oce.tz"), ...)
{
    ## reference: WCODF = "WorkHorse Commands and Output Data Format_Nov07.pdf"

    ##
    ## header, of length 6 + 2 * number.of.data.types bytes
    ##
    oce.debug(debug, "read.header.rdi() entry\n")
    if (buf[1] != 0x7f || buf[2] != 0x7f)
        stop("first two bytes in file must be 0x7f 0x7f, but they are 0x", buf[1], " 0x", buf[2])
    bytes.per.ensemble <- readBin(buf[3:4], "integer", n=1, size=2, endian="little", signed=FALSE)
    oce.debug(debug, "bytes.per.ensemble=", bytes.per.ensemble, "\n")
    ## byte5 not used
    number.of.data.types <- readBin(buf[6], "integer", n=1, size=1)
    if (number.of.data.types < 1 || 200 < number.of.data.types)
        stop("cannot have ", number.of.data.types, " data types, as header indicates")
    oce.debug(debug, "number.of.data.types=", number.of.data.types, "\n")
    have.actual.data <- number.of.data.types > 2 # will be 2 if just have headers
    oce.debug(debug, "have.actual.data=", have.actual.data, "\n")
    data.offset <- readBin(buf[7+0:(2*number.of.data.types)], "integer", n=number.of.data.types, size=2, endian="little")
    oce.debug(debug, "data.offset=", paste(data.offset, sep=" "), "\n")
    ##
    ## Fixed Leader Data, abbreviated FLD, pointed to by the data offset
    FLD <- buf[data.offset[1]+1:(data.offset[2] - data.offset[1])]
    oce.debug(debug, "Fixed Leader Data:", paste(FLD, collapse=" "), "\n")
    if (FLD[1] != 0x00) stop("first byte of fixed leader header must be 0x00 but it was ", FLD[1])
    if (FLD[2] != 0x00) stop("second byte of fixed leader header must be a0x00 but it was ", FLD[2])
    program.version.major <- readBin(FLD[3], "integer", n=1, size=1, signed=FALSE)
    program.version.minor <- readBin(FLD[4], "integer", n=1, size=1, signed=FALSE)
    program.version <- paste(program.version.major, program.version.minor, sep=".")
    program.version.numeric <- as.numeric(program.version)
    oce.debug(debug, "program version=", program.version, "(numerically, it is", program.version.numeric,")\n")
    if (program.version < 16.28)
        warning("program version ", program.version, " is less than 16.28, and so read.adp.rdi() may not work properly")

    if (!have.actual.data)
        return(list(instrument.type="adcp",
                    program.version.major=program.version.major,
                    program.version.minor=program.version.minor,
                    program.version=program.version,
                    have.actual.data=have.actual.data))

    system.configuration <- paste(byte2binary(FLD[5], endian="big"), byte2binary(FLD[6],endian="big"),sep="-")
    oce.debug(debug, "FLD[4]=", byte2binary(FLD[4], endian="big"), "(looking near the system.configuration bytes to find a problem)\n")
    oce.debug(debug, "FLD[5]=", byte2binary(FLD[5], endian="big"), "(should be one of the system.configuration bytes)\n")
    oce.debug(debug, "FLD[6]=", byte2binary(FLD[6], endian="big"), "(should be one of the system.configuration bytes)\n")
    oce.debug(debug, "FLD[7]=", byte2binary(FLD[7], endian="big"), "(looking near the system.configuration bytes to find a problem)\n")
    bits <- substr(system.configuration, 6, 8)
    if (bits == "000") frequency <- 75        # kHz
    else if (bits == "001") frequency <-  150
    else if (bits == "010") frequency <-  300
    else if (bits == "011") frequency <-  600
    else if (bits == "100") frequency <- 1200
    else if (bits == "101") frequency <- 2400
    oce.debug(debug, "bits:", bits, "so frequency=", frequency, "\n")
    bits <- substr(system.configuration, 16, 17)
    oce.debug(debug, "system.configuration:", system.configuration,"\n")
    oce.debug(debug, "bits:", bits, "00 is 15deg, 01 is 20deg, 02 is 30deg, 11 is 'other'\n")
    if (bits == "00") beam.angle <- 15
    else if (bits == "01") beam.angle <- 20
    else if (bits == "10") beam.angle <- 30
    else if (bits == "11") beam.angle <- NA # means 'other'
    oce.debug(debug, "bits=", bits, "so beam.angle=", beam.angle, "\n")
    if (beam.angle < 19 || 21 < beam.angle)
        warning("expecting a beam angle of 20deg [more-or-less standard for RDI] but got", beam.angle, "; using the latter in the transformation matrix")
    bits <- substr(system.configuration, 5, 5)
    if (bits == "0") beam.pattern <- "concave"
    else beam.pattern <- "convex"
    oce.debug(debug, "bits=", bits, "so beam.pattern=", beam.pattern, "\n")
    beam.config <- "?"
    bits <- substr(system.configuration, 10, 13)
    if (bits == "0100") beam.config <- "janus"
    else if (bits == "0101") beam.config <- "janus demod"
    else if (bits == "1111") beam.config <- "janus 2 demd"
    bits <- substr(system.configuration, 1, 1)
    if (bits == "1") orientation <- "upward"
    else orientation <- "downward"
    oce.debug(debug, "bits=", bits, "so that orientation=", orientation, "\n")

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
    heading.alignment <- 0.01 * readBin(FLD[27:28], "integer", n=1, size=2, endian="little") # WCODF p 130
    heading.bias <- 0.01 * readBin(FLD[29:30], "integer", n=1, size=2, endian="little") # WCODF p 130
    oce.debug(debug, "heading.alignment=", heading.alignment, "; heading.bias=", heading.bias, "\n")
    sensor.source <- readBin(FLD[31], "integer", n=1, size=1)
    sensors.available <- readBin(FLD[32], "integer", n=1, size=1)
    bin1.distance <- readBin(FLD[33:34], "integer", n=1, size=2, endian="little", signed=FALSE) * 0.01
    ##cat("bin1.distance being inferred from 0x", FLD[33:34], " as ", bin1.distance, "\n", sep="", ...)
    xmit.pulse.length <- readBin(FLD[35:36], "integer", n=1, size=2, endian="little", signed=FALSE) * 0.01
    ##cat("xmit.pulse.length being inferred from 0x", FLD[35:36], " as ", xmit.pulse.length, "\n", sep="", ...)
    wp.ref.layer.average <- readBin(FLD[37:38], "integer", n=1, size=2, endian="little")
    false.target.thresh <- readBin(FLD[39], "integer", n=1, size=1)
    ## FLD[40] spare
    transmit.lag.distance <- readBin(FLD[41:42], "integer", n=1, size=2, endian="little", signed=FALSE)
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
    ## "WorkHorse Commands and Output Data Format_Mar05.pdf" p130: bytes 55:58 = serial number only for REMUS, else spare
    ## "WorkHorse Commands and Output Data Format_Nov07.pdf" p127: bytes 55:58 = serial number
    serial.number <- readBin(FLD[55:58], "integer", n=1, size=4, endian="little")
    oce.debug(debug, "SERIAL NUMBER", serial.number, "from bytes (", FLD[55:58], ")\n")
    if (serial.number == 0) serial.number <- "unknown (not found at expected byte locations in file)"
    ##beam.angle <- readBin(FLD[59], "integer", n=1, size=1) # NB 0 in first test case
    ##cat("BEAM ANGLE=", FLD[59], "or", beam.angle, "\n", ...)
    ##
    ## VLD (variable leader data) 65 bytes
    ##
    ## "WorkHorse Commands and Output Data Format_Mar05.pdf" p132: VLD length 65 bytes
    ## "WorkHorse Commands and Output Data Format_Nov07.pdf" p133: VLD length 65 bytes

    nVLD <- 65
    VLD <- buf[data.offset[2]+1:nVLD]
    oce.debug(debug, "Variable Leader Data (", length(VLD), "bytes):", paste(VLD, collapse=" "), "\n")
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
    time <- ISOdatetime(RTC.year, RTC.month, RTC.day, RTC.hour, RTC.minute, RTC.second + RTC.hundredths / 100, tz=tz)
    oce.debug(debug, "profile time=", format(time), "inferred from RTC.year=", RTC.year,
              "RTC.month=", RTC.month, "RTC.day-", RTC.day, "RTC.hour=", RTC.hour,
              "RTC.minute=", RTC.minute, "RTC.second=", RTC.second, "RTC.hundreds=", RTC.hundredths, "\n")
    ensemble.number.MSB <- readBin(VLD[12], "integer", n=1, size=1)
    bit.result <- readBin(VLD[13:14], "integer", n=1, size=2, endian="little")
    speed.of.sound  <- readBin(VLD[15:16], "integer", n=1, size=2, endian="little")
    if (speed.of.sound < 1400 || speed.of.sound > 1600)
        warning("speed of sound is ", speed.of.sound, ", which is outside the permitted range of 1400 m/s to 1600 m/s")
    ## Comment out some things not needed here (may be wrong, too)
    ##depth.of.transducer <- readBin(VLD[17:18], "integer", n=1, size=2, endian="little") * 0.1
    ##oce.debug(debug, "depth of transducer:", depth.of.transducer, "\n")
    heading <- readBin(VLD[19:20], "integer", n=1, size=2, endian="little", signed=FALSE) * 0.01 - heading.bias
    pitch <- readBin(VLD[21:22], "integer", n=1, size=2, endian="little") * 0.01
    roll <- readBin(VLD[23:24], "integer", n=1, size=2, endian="little") * 0.01
    oce.debug(debug, "VLD header has: heading=", heading, " pitch=", pitch, " roll=", roll, "\n")

    ## Skipping a lot ...
    ##pressure <- readBin(VLD[49:52], "integer", n=1, size=4, endian="little", signed=FALSE) * 0.001

    list(instrument.type="adcp",
         program.version.major=program.version.major,
         program.version.minor=program.version.minor,
         program.version=program.version,
         ##program.version.major=fv,
         ##program.version.minor=fr,
         bytes.per.ensemble=bytes.per.ensemble,
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
         pings.per.ensemble=pings.per.ensemble,
         cell.size=cell.size,
         profiling.mode=profiling.mode,
         low.corr.thresh=low.corr.thresh,
         number.of.code.reps=number.of.code.reps,
         percent.gd.minimum=percent.gd.minimum,
         error.velocity.maximum=error.velocity.maximum,
         ##tpp.minutes=tpp.minutes,
         ##tpp.seconds=tpp.seconds,
         ##tpp.hundredths=tpp.hundredths,
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
         ##system.power=system.power,
         serial.number=serial.number,
         ## beam.angle=beam.angle,  # wrong in my tests, anyway
         ##ensemble.number=ensemble.number,
         ##time=time,
         ##ensemble.number.MSB=ensemble.number.MSB,
         ##bit.result=bit.result,
         ##speed.of.sound=speed.of.sound,
         ##heading=heading,
         ##pitch=pitch,
         ##roll=roll,
         ##salinity=salinity
         heading.alignment,
         heading.bias,
         have.actual.data=have.actual.data)
}                                       # read.header.rdi()

read.adp.rdi <- function(file, from=1, to, by=1, tz=getOption("oce.tz"),
                         latitude=NA, longitude=NA,
                         type=c("workhorse"),
                         debug=getOption("oce.debug"), monitor=TRUE, log.action, ...)
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
            t <- ISOdatetime(year, month, day, hour, minute, second + sec100/100, tz=tz)
            oce.debug(debug, "t=", format(t), "| y=", year, " m=", month, " d=", format(day, width=2), " h=", format(hour, width=2), " m=", format(minute, width=2), "s=", format(second, width=2), "sec100=", sec100, "| pass", format(pass, width=2), "/", passes, "| middle=", middle, "(", format(middle/upper*100,digits=4), "%)\n")
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
                         tz=tz)
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

    ## Read whole file into 'buf'
    seek(file, 0, "start")
    seek(file, where=0, origin="end")
    file.size <- seek(file, where=0)
    oce.debug(debug, "file.size=", file.size, "\n")
    buf <- readBin(file, what="raw", n=file.size, endian="little")

    ## decode header
    header <- decode.header.rdi(buf, debug=debug-1)
    if (header$have.actual.data) {
        number.of.beams <- header$number.of.beams
        number.of.cells <- header$number.of.cells
        bin1.distance <- header$bin1.distance
        xmit.pulse.length <- header$xmit.pulse.length
        cell.size <- header$cell.size
        profile.start <- .Call("match2bytes", buf, 0x80, 0x00, TRUE)
        oce.debug(debug, vector.show(profile.start, "profile.start before trimming:"))
        profiles.in.file <- length(profile.start)
        oce.debug(debug, "profiles.in.file=", profiles.in.file, "(as inferred by a byte-check on the sequence 0x80, 0x00)\n")
        if (profiles.in.file > 0)  {
            measurement.start <- ISOdatetime(2000 + as.integer(buf[profile.start[1]+4]), # year
                                             as.integer(buf[profile.start[1]+5]), # month
                                             as.integer(buf[profile.start[1]+6]), # day
                                             as.integer(buf[profile.start[1]+7]), # hour
                                             as.integer(buf[profile.start[1]+8]), # min
                                             as.integer(buf[profile.start[1]+9]), # sec
                                             tz=tz)
            oce.debug(debug, "measurement.start:", format(measurement.start), "\n")
            measurement.end <- ISOdatetime(2000 + as.integer(buf[profile.start[profiles.in.file]+4]), # year
                                           as.integer(buf[profile.start[profiles.in.file]+5]), # month
                                           as.integer(buf[profile.start[profiles.in.file]+6]), # day
                                           as.integer(buf[profile.start[profiles.in.file]+7]), # hour
                                           as.integer(buf[profile.start[profiles.in.file]+8]), # min
                                           as.integer(buf[profile.start[profiles.in.file]+9]), # sec
                                           tz=tz)
            oce.debug(debug, "measurement.end:", format(measurement.end), "\n")
            ## FIXME: assumes uniform time interval (ok, but document it)
            measurement.deltat <- as.numeric(ISOdatetime(2000 + as.integer(buf[profile.start[2]+4]), # year
                                                         as.integer(buf[profile.start[2]+5]), # month
                                                         as.integer(buf[profile.start[2]+6]), # day
                                                         as.integer(buf[profile.start[2]+7]), # hour
                                                         as.integer(buf[profile.start[2]+8]), # min
                                                         as.integer(buf[profile.start[2]+9]), # sec
                                                         tz=tz)) - as.numeric(measurement.start)
            oce.debug(debug, "measurement.deltat:", measurement.deltat, "\n")
            if (inherits(from, "POSIXt")) {
                if (!inherits(to, "POSIXt")) stop("if 'from' is POSIXt, then 'to' must be, also")
                from.pair <- bisect.rdi.adp(from, add=-1, debug=debug-1)
                from <- from.index <- from.pair$index
                to.pair <- bisect.rdi.adp(to, add=1, debug=debug-1)
                to <- to.index <- to.pair$index
                oce.debug(debug, "from=", format(from.pair$t), " yields profile.start[", from.index, "]\n",
                          "  to  =", format(to.pair$t), "yields profile.start[", to.index, "]\n",
                          "  by=", by, "(not yet decoded)\n",
                          vector.show(profile.start, "profile.start*:"),
                                        #"  profile.start[1:10]=", profile.start[1:10],"\n",
                          "  profile.start[",from.pair$index, "]=", profile.start[from.pair$index], "at time", format(from.pair$t), "\n",
                          "  profile.start[",  to.pair$index, "]=", profile.start[  to.pair$index], "at time", format(  to.pair$t), "\n")
                dt <- measurement.deltat
                oce.debug(debug, "dt=", dt, "s; at this stage, by=", by,"\n")
                if (is.character(by))
                    by <- floor(0.5 + ctime.to.seconds(by) / dt)
                oce.debug(debug, "by=",by,"profiles (after decoding)\n")
                profile.start <- profile.start[profile.start[from.index] < profile.start & profile.start < profile.start[to.index]]
                profile.start <- profile.start[seq(1, length(profile.start), by=by)]
            } else {
                from.index <- from
                to.index <- to
                if (to.index < from.index) stop("need more separation between from and to")
                if (is.character(by)) stop("cannot have string for 'by' if 'from' and 'to' are integers")
                profile.start <- profile.start[seq(from=from, to=to, by=by)]
                oce.debug(debug, vector.show(profile.start, "profile.start after indexing:"))
            }
            profiles.to.read <- length(profile.start)
            oce.debug(debug, "profiles.to.read=",profiles.to.read,"\n")
            oce.debug(debug, "number.of.beams=",number.of.beams,"\n")
            oce.debug(debug, "number.of.cells=",number.of.cells,"\n")
            items <- number.of.beams * number.of.cells
            v <- array(double(), dim=c(profiles.to.read, number.of.cells, number.of.beams))
            a <- array(raw(), dim=c(profiles.to.read, number.of.cells, number.of.beams)) # echo amplitude
            q <- array(raw(), dim=c(profiles.to.read, number.of.cells, number.of.beams)) # correlation
            g <- array(raw(), dim=c(profiles.to.read, number.of.cells, number.of.beams)) # percent good
            bad.profiles <- NULL
            oce.debug(debug, "profiles.to.read=", profiles.to.read, "\n")
            for (i in 1:profiles.to.read) {     # recall: these start at 0x80 0x00
                o <- profile.start[i] + 65      # FIXME: are we *sure* this will be 65?
                oce.debug(debug, 'getting data chunk',i,' at file position',o,'\n')
                if (buf[o] == 0x00 && buf[o+1] == 0x01) {
                    ##cat(vector.show(buf[o + 1 + seq(1, 2*items)], "buf[...]:"))
                    vv <- readBin(buf[o + 1 + seq(1, 2*items)], "integer", n=items, size=2, endian="little", signed=TRUE)
                    ##cat(vector.show(vv, "vv:"))
                    vv[vv==(-32768)] <- NA       # blank out bad data
                    v[i,,] <- matrix(vv / 1000, ncol=number.of.beams, byrow=TRUE)
                    ##cat(vector.show(v[i,,], "v:"))
                    o <- o + items * 2 + 2 # skip over the velo data, plus a checksum; FIXME: use the checksum
                    if (buf[o] != 0x00)
                        warning("first byte of correlation segment should be 0x00 but is ", buf[o], " at file position ", o)
                    if (buf[o+1] != 0x02)
                        warning("second byte of corrleation segment should be 0x02 but is ", buf[o+1], " at file position ", o+1)
                    q[i,,] <- matrix(buf[o + 1 + seq(1, items)], ncol=number.of.beams, byrow=TRUE)
                    ##cat(vector.show(q[i,,], "q:"))
                    o <- o + items + 2              # skip over the one-byte data plus a checkum; FIXME: use the checksum
                    if (buf[o] != 0x00)
                        warning("first byte of intensity segment should be 0x00 but is ", buf[o], " at file position ", o)
                    if (buf[o+1] != 0x03)
                        warning("second byte of intensity segment should be 0x03 but is ", buf[o+1], " at file position ", o+1)
                    a[i,,] <- matrix(buf[o + 1 + seq(1, items)], ncol=number.of.beams, byrow=TRUE)
                    ##cat(vector.show(a[i,,], "a:"))
                    o <- o + items + 2              # skip over the one-byte data plus a checkum; FIXME: use the checksum
                    if (buf[o] != 0x00)
                        warning("first byte of percent-good segment should be 0x00 but is ", buf[o], " at file position ", o)
                    if (buf[o+1] != 0x04)
                        warning("second byte of percent-good segment should be 0x04 but is ", buf[o+1], " at file position ", o+1)
                    g[i,,] <- matrix(buf[o + 1 + seq(1, items)], ncol=number.of.beams, byrow=TRUE) # FIXME: not using this
                    ##cat(vector.show(g[i,,], "g:"))
                    if (monitor) {
                        cat(".", ...)
                        if (!(i %% 50)) cat(i, "\n", ...)
                    }
                } else {
                    bad.profiles <- c(bad.profiles, i)
                    if (monitor) {
                        cat("X", ...)
                        if (!(i %% 50)) cat(i, "\n", ...)
                    }
                    ##if (debug > 0) {
                    ##    cat(buf[profile.start[i]+0:(o-1)], "[", buf[o], buf[o+1], "]", buf[o+2:27], "...\n")
                    ##    cat("Warning: in the above, did not find 00 01 (to mark a velocity segment) at file position ", o, " trying to read profile ", i)
                    ##}
                }
            }
            time <- ISOdatetime(2000+as.integer(buf[profile.start+4]), # year
                                as.integer(buf[profile.start+5]),      # month
                                as.integer(buf[profile.start+6]),      # day
                                as.integer(buf[profile.start+7]),      # hour
                                as.integer(buf[profile.start+8]),      # minute
                                as.integer(buf[profile.start+9]),      # second
                                tz=tz)
            if (length(bad.profiles) > 0) { # remove NAs in time (not sure this is right, but it prevents other problems)
                t0 <- time[match(1, !is.na(time))] # FIXME: should test if any
                time <- fill.gap(as.numeric(time) - as.numeric(t0)) + t0
                warning("Interpolated across ", length(bad.profiles), " bad profile(s) at times: ", paste(format(time[bad.profiles]), sep=", "), ".  (\"Bad\" means that the expected byte code for a velocity segment, 0x00 0x01, was not found 65 bytes after the start of a profile, the latter indicated by the byte sequence 0x80 0x00.)\n")
            }

            profile.start2 <- sort(c(profile.start, profile.start + 1)) # lets us index two-byte chunks
            profile.start4 <- sort(c(profile.start, profile.start + 1, profile.start + 2, profile.start + 3)) # lets us index four-byte chunks
            speed.of.sound <- 0.1 * readBin(buf[profile.start2 + 14], "integer", n=profiles.to.read, size=2, endian="little", signed=FALSE)
            depth.of.transducer <- 0.1 * readBin(buf[profile.start2 + 16], "integer", n=profiles.to.read, size=2, endian="little")
            ## Note that the heading.bias needs to be removed
            heading <- 0.01 * readBin(buf[profile.start2 + 18], "integer", n=profiles.to.read, size=2, endian="little", signed=FALSE) - header$heading.bias
            if (header$heading.bias != 0)
                warning("subtracted a heading bias of ", header$heading.bias, " degrees")
            pitch <- 0.01 * readBin(buf[profile.start2 + 20], "integer", n=profiles.to.read, size=2, endian="little", signed=TRUE)
            roll <- 0.01 * readBin(buf[profile.start2 + 22], "integer", n=profiles.to.read, size=2, endian="little", signed=TRUE)
            ##tmp <- pitch
            oce.debug(debug, vector.show(pitch, "pitch, before correction as on p14 of 'adcp coordinate transformation.pdf'"))
            pitch <- 180 / pi * atan(tan(pitch * pi / 180) / cos(roll * pi / 180)) # correct the pitch (see ACT page 14)
            oce.debug(debug, vector.show(pitch, "pitch, correction"))
            ##oce.debug(debug, "RMS change in pitch:", sqrt(mean((pitch - tmp)^2, na.rm=TRUE)), "\n")
            ##rm(tmp)
            salinity <- readBin(buf[profile.start2 + 24], "integer", n=profiles.to.read, size=2, endian="little", signed=TRUE)
            temperature <- 0.01 * readBin(buf[profile.start2 + 26], "integer", n=profiles.to.read, size=2, endian="little", signed=TRUE)
            pressure <- 0.001 * readBin(buf[profile.start4 + 48], "integer", n=profiles.to.read, size=4, endian="little", signed=FALSE)
            metadata <- header
            metadata$latitude <- latitude
            metadata$longitude <- longitude
            metadata$bin1.distance <- bin1.distance
            metadata$xmit.pulse.length <- xmit.pulse.length
            metadata$measurement.start <- measurement.start
            metadata$measurement.end <- measurement.end
            metadata$measurement.deltat <- measurement.deltat
            metadata$filename <- filename
            metadata$oce.beam.attenuated <- FALSE
            metadata$oce.coordinate <- header$coordinate.system
            metadata$number.of.beams <- header$number.of.beams
            metadata$depth.of.transducer <- mean(depth.of.transducer, na.rm=TRUE)
            ## Transformation matrix
            tm.c <- if (metadata$beam.pattern == "convex") 1 else -1; # control sign of first 2 rows of transformation.matrix
            tm.a <- 1 / (2 * sin(metadata$beam.angle * pi / 180))
            tm.b <- 1 / (4 * cos(metadata$beam.angle * pi / 180))
            tm.d <- tm.a / sqrt(2)
            ## FIXME Dal people use 'a' in last row of matrix, but both
            ## RDI and CODAS use as we have here.  (And I think RDI
            ## may have two definitions...)
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
            ## The matrix below is from page 13 (section 5.30) of the ACT.  Later on,
            ## in adp.beam2xyz(), we will change the sign of rows 1 and 3, if the
            ## device is pointing upwards.
            ##
            ## As a check on coding, see the python software at
            ##   http://currents.soest.hawaii.edu/hg/hgwebdir.cgi/pycurrents/file/tip/adcp/transform.py
            metadata$transformation.matrix <- matrix(c(tm.c*tm.a, -tm.c*tm.a,          0,         0,
                                                       0        ,          0, -tm.c*tm.a, tm.c*tm.a,
                                                       tm.b     ,       tm.b,       tm.b,      tm.b,
                                                       tm.d     ,       tm.d,      -tm.d,     -tm.d),
                                                     nrow=4, byrow=TRUE)
            if (monitor) cat("\nRead", profiles.to.read,  "profiles, out of a total of",profiles.in.file,"profiles in", filename, "\n", ...)
            class(time) <- c("POSIXt", "POSIXct")
            attr(time, "tzone") <- getOption("oce.tz")
            data <- list(ma=list(v=v, a=a, q=q, g=g),
                         ss=list(distance=seq(bin1.distance, by=cell.size, length.out=number.of.cells)),
                         ts=list(time=time,
                         pressure=pressure,
                         temperature=temperature,
                         salinity=salinity,
                         depth.of.transducer=depth.of.transducer,
                         heading=heading,
                         pitch=pitch,
                         roll=roll))
        } else {
            warning("There are no profiles in this file.")
            metadata <- header
            metadata$filename <- filename
            data <- NULL
        }
    } else {
        warning("The header indicates that there are no profiles in this file.")
        metadata <- header
        metadata$filename <- filename
        data <- NULL
    }
    metadata$manufacturer <- "teledyne rdi"
    if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processing.log.item(log.action)
    res <- list(data=data, metadata=metadata, processing.log=log.item)
    class(res) <- c("rdi", "adp", "oce")
    res
}
