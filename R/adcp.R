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
    ## binary header data, length 6 + 2 * num.data.types bytes
    headerID <- readBin(file, "raw", n=1, size=1)
    if (headerID != 0x7f) stop("first byte in file (headID) must be 0x7f, but it was", headerID)
    dataID <- readBin(file, "raw", n=1, size=1)
    if (dataID != 0x7f) stop("second byte in file (dataID) must be 0x7f but it was", dataID)
    num.bytes.in.ensemble <- readBin(file, "integer", n=1, size=2, endian="little")
    if (debug) cat("num.bytes.in.ensemble=", num.bytes.in.ensemble,"\n")
    offset.to.cksum <- readBin(file, "raw", n=2, size=1) # not used
    ## BHD[5] spare (or is that a number that means something??)
    num.data.types <- readBin(file, "integer", n=1, size=1)
    if (num.data.types < 1) stop("cannot have ", num.data.types, " data types, as header indicates")
    data.offset <- readBin(file, "integer", n=num.data.types, size=2, endian="little")
cat("data offset=");print(data.offset);stop()
    where <- seek(file)
    if (where != 1 + 6 + 2 * num.data.types) stop("BHD (binary header data) length should be ", 1 + 6 + 2 * num.data.types, " bytes, but it is ", where)
    ##
    ## FLD (fixed leader data) 59 bytes
    FLD <- readBin(file, "raw", n=59, size=1) # binary fixed leader data (Figure D-5)
    if (debug) {
        cat("fixed leader data (50 bytes):\n")
        print(FLD)
    }
    if (FLD[1] != 0x00) stop("first byte of fixed leader header must be 0x00 but it was ", FLD[1])
    if (FLD[2] != 0x00) stop("second byte of fixed leader header must be 0x00 but it was ", FLD[2])
    fv <- readBin(FLD[3], "integer", n=1, size=1)
    fr <- readBin(FLD[4], "integer", n=1, size=1)
    program.version <- paste(fv, fr, sep=".") # don't want to rely on number of digits
    system.configuration <- readBin(FLD[5:6], "integer", n=1, size=2, endian="little")
    ## FLD[7:8] reserved
    num.beams <- readBin(FLD[9], "integer", n=1, size=1)
    num.cells <- readBin(FLD[10], "integer", n=1, size=1) # WN
    ## FLD[11:12] reserved
    depth.cell.length <- readBin(FLD[13:14], "integer", n=1, size=2, endian="little") / 100 # WS in m
    blank.after.transmit <- readBin(FLD[15:16], "integer", n=1, size=2, endian="little") / 100 # in m
    profiling.mode <- readBin(FLD[17], "integer", n=1, size=1) # WM
    ## FLD[18] reserved
    num.code.reps <- readBin(FLD[19], "integer", n=1, size=1)
    ## FLD[21:26] reserved
    heading.alignment <- readBin(FLD[27:28], "integer", n=1, size=2, endian="little")
    sensor.source <- readBin(FLD[31], "integer", n=1, size=1) # EZ
    sensors.avail <- readBin(FLD[32], "integer", n=1, size=1)
    bin1.distance <- readBin(FLD[33:34], "integer", n=1, size=2, endian="little")
    xmit.pulse.length <- readBin(FLD[35:36], "integer", n=1, size=2, endian="little")
    ref.layer.average <- readBin(FLD[37:38], "integer", n=1, size=2, endian="little") #??
    false.target.thresh <- readBin(FLD[39], "integer", n=1, size=1) # WA
    ## FLD[40] reserved
    transmit.lag.distance <- readBin(FLD[41:42], "integer", n=1, size=1) / 100 # to m
    ## FLD[43:50] reserved
    where <- seek(file)
print(data.offset);stop()
print(num.beams)
print(where);stop()
    if (where != 1 + (59 + 6 + 2 * num.data.types)) stop("problem reading header; should be at byte ", 1 + (59 + 6 + 2 * num.data.types), " but am at byte ", where)
    ##
    ## VLD (variable leader data) 65 bytes
    VLD <- readBin(file, "raw", n=65, size=1) # binary fixed leader data (Figure D-5)
    if (debug) {
        cat("variable leader data (58 bytes):\n")
        print(VLD)
    }
    system.date.year <- readBin(VLD[5], "integer", n=1, size=1, endian="little")
    system.date.month <- readBin(VLD[6], "integer", n=1, size=1, endian="little")
    system.date.day <- readBin(VLD[7], "integer", n=1, size=1, endian="little")
    system.date.hour <- readBin(VLD[8], "integer", n=1, size=1, endian="little")
    system.date.minute <- readBin(VLD[9], "integer", n=1, size=1, endian="little")
    system.date.second <- readBin(VLD[10], "integer", n=1, size=1, endian="little")
    system.date.hundreds <- readBin(VLD[11], "integer", n=1, size=1, endian="little")
    ## ensemble #MSB
    ## reserved 2
    speed.of.sound <- readBin(VLD[15:16], "integer", n=1, size=2, endian="little")
    print(list(
               system.date.year=system.date.year,
               system.date.month=system.date.month,
               system.date.day=system.date.day,
               system.date.hour=system.date.hour,
               system.date.minute=system.date.minute,
               system.date.second=system.date.second,
               system.date.hundreds=system.date.hundreds,
               speed.of.sound=speed.of.sound
               ))
    if (VLD[1] != 0x80) stop("byte 1 of variable leader data should be 0x80, but it is ", VLD[1])
    if (VLD[2] != 0x00) stop("byte 1 of variable leader data should be 0x00, but it is ", VLD[1])

    ##
    ## BT (bottom track) 81 bytes
    ##  ... more, see Figure D-3

    metadata <- list(filename=filename,
                     program.version=program.version, # ok
                     num.cells=num.cells, # ok
                     num.data.types=num.data.types,
                     profiling.mode=profiling.mode,
                     num.beams=num.beams,
                                        # ok (checks with matlab) above this comment
                     blank.after.transmit=blank.after.transmit,
                     heading.alignment=heading.alignment,
                     sensor.source=sensor.source,
                     bin1.distance=bin1.distance,
                     xmit.pulse.length=xmit.pulse.length,
                     ref.layer.average=ref.layer.average,
                     false.target.thresh=false.target.thresh,
                     transmit.lag.distance=transmit.lag.distance,
                     depth.cell.length=depth.cell.length,
                     data.offset=data.offset,
                     ## fv=fv,
                     ## fr=fr,
                     system.configuration=system.configuration)
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
