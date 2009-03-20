read.adcp <- function(file, type ="RDI", debug=FALSE, log.action)
{
    show.bytes <- function(file, n) {
        cat("next", n, "bytes of file:\n")
        for (i in 1:n) {
            b <- readBin(file, "raw", n=1, size=1)
            cat("[", b, "] ")
            if (!(i %% 10)) cat("\n")
        }
    }
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
    if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
    data <- list(da=1)
    ##
    ## header, 6 + 2 * number.of.data.types
    ##
    header.part1 <- readBin(file, "raw", n=6, size=1)
    if (header.part1[1] != 0x7f) stop("first byte in file must be 0x7f, but it was", header.part1[1])
    if (header.part1[2] != 0x7f) stop("second byte in file must be 0x7f but it was", header.part1[2])
    num.bytes.in.ensemble <- readBin(header.part1[3:4], "integer", n=1, size=2, endian="little")
    if (debug) cat("num.bytes.in.ensemble=", num.bytes.in.ensemble,"\n")
    ## header.part1[5] spare
    number.of.data.types <- readBin(header.part1[6], "integer", n=1, size=1)
    if (number.of.data.types < 1) stop("cannot have ", number.of.data.types, " data types, as header indicates")
    if (debug) cat("number.of.data.types=", number.of.data.types, "\n")
    ## part 2 of header is these data offsets
    data.offset <- readBin(file, "integer", n=number.of.data.types, size=2, endian="little")
    if (debug) cat("data.offset=", paste(data.offset, sep=" "), "\n")
    ##
    ## FLD (fixed leader data) 59 bytes
    ##
    FLD <- readBin(file, "raw", n=59, size=1) # binary fixed leader data (Figure D-5)
    if (debug) {
        cat("fixed leader data (59 bytes):\n")
        print(FLD)
    }
    if (FLD[1] != 0x00) stop("first byte of fixed leader header must be 0x00 but it was ", FLD[1])
    if (FLD[2] != 0x00) stop("second byte of fixed leader header must be 0x00 but it was ", FLD[2])
    fv <- readBin(FLD[3], "integer", n=1, size=1)
    fr <- readBin(FLD[4], "integer", n=1, size=1)
    program.version <- paste(fv, fr, sep=".") # don't want to rely on number of digits
    if (debug) cat("program version=", program.version, "\n")
    system.configuration <- readBin(FLD[5:6], "integer", n=1, size=2, endian="little")
    real.sim.flag <- readBin(FLD[7], "integer", n=1, size=1)
    lag.length <- readBin(FLD[8], "integer", n=1, size=1)
    number.of.beams <- readBin(FLD[9], "integer", n=1, size=1)
    number.of.cells <- readBin(FLD[10], "integer", n=1, size=1) # WN
    pings.per.ensemble <- readBin(FLD[11:12], "integer", n=1, size=2, endian="little")
    depth.cell.length <- readBin(FLD[13:14], "integer", n=1, size=2, endian="little") / 100 # WS in m
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
    ##
    ## VLD (variable leader data) 65 bytes
    ##
    VLD <- readBin(file, "raw", n=65, size=1)
    cat("position in file=", seek(file, NA), "after reading VLD\n")
    if (debug) {
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

    ##
    ## FIXME: more code needed here ... but first fix VLD header
    ##

    metadata <- list(filename=filename,
                     program.version=program.version,
                     number.of.data.types=number.of.data.types,
                     data.offset=data.offset,
                     number.of.beams=number.of.beams,
                     number.of.cells=number.of.cells,
                     pings.per.ensemble=pings.per.ensemble,
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
                     # VLD
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
                     speed.of.sound=speed.of.sound
                     )
    print(metadata)
    #show.bytes(file, 50)
    if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processing.log.item(log.action)
    res <- list(data=data, metadata=metadata, processing.log=log.item)
    class(res) <- c("adcp", "oce")
    res
}

summary.adcp <- function(object, ...)
{
    if (!inherits(object, "adcp")) stop("method is only for adcp objects")
##    dim <- dim(object$data)
##    fives <- matrix(nrow=dim[2], ncol=5)
    res <- list(filename="?",
##                fives=fives,
                processing.log=processing.log.summary(object))
##    if (!is.null(object$metadata$filename.orig))      res$filename <- object$metadata$filename.orig
##    res$levels <- dim[1]
##    for (v in 1:dim[2])
##        fives[v,] <- fivenum(object$data[,v], na.rm=TRUE)
##    rownames(fives) <- names(object$data)
##    colnames(fives) <- c("Min.", "1st Qu.", "Median", "3rd Qu.", "Max.")
##    res$fives <- fives
    class(res) <- "summary.adcp"
    res
}

print.summary.adcp <- function(x, digits=max(6, getOption("digits") - 1), ...)
{
    cat("ADCP record\n")
    cat("\n\nMetadata:\n")
    print(x$metadata)
    print(x$processing.log)
    invisible(x)
}
