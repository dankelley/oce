## Data format overview
## hardware [a5 05 X1 X2]  48 bytes, 2*(short word made from X1 and X2)
## head     [a5 04 X1 X2] 224 bytes, 2*(short word made from X1 and X2)
## user     [a5 00 X1 X2] 512 bytes, 2*(short word made from X1 and X2)
## profiles, each starting with a5 2a [aquadoppHR] or ?? [other]
## DOCUMENTATION BUGS
## 1. p38 System Integrator Guide says to offset 53 bytes for velocity, but I have to offset 54 to recover data
#     that match the manufacturer's (*.v1, *.v2, *.v3) files.

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

decode.header.nortek <- function(buf, debug=getOption("oce.debug"), ...)
{
    oce.debug(debug, "decode.header.nortek() entry; buf[1:20]=",buf[1:20],"\n")
    sync.code <- as.raw(0xa5)
    id.hardware.configuration <- as.raw(0x05)
    id.head.configuration <- as.raw(0x04)
    id.user.configuration <- as.raw(0x00)
    header.length.hardware <- 48
    header.length.head <- 224
    header.length.user <- 512
    hardware <- head <- user <- list()
    o <- 0                              # offset
    for (header in 1:3) { # FIXME: code is needlessly written as if headers could be in different order
        oce.debug(debug, "\n")
        oce.debug(debug, "buf[o+2]=", buf[o+2], "\n")
        if (buf[o+1] != sync.code)
            stop("expecting sync code 0x", sync.code, " but got 0x", buf[o+1], " instead (while reading header #", header, ")")
        if (buf[o+2] == id.hardware.configuration) {         # see page 29 of System Integrator Guide
            oce.debug(debug, "HARDWARE CONFIGURATION\n")
            hardware$size <- readBin(buf[o+3:4], "integer",signed=FALSE, n=1, size=2, endian="little")
            if (hardware$size != 24) stop("size of hardware header expected to be 24 two-byte words, but got ", hardware$size)
            if (2 * hardware$size != header.length.hardware)
                stop("size of hardware header expected to be ", header.length.hardware, "but got ", hardware$size)
            oce.debug(debug, "hardware$size=", hardware$size, "\n")
            hardware$serial.number <- gsub(" *$", "", paste(readBin(buf[o+5:18], "character", n=14, size=1), collapse=""))
            oce.debug(debug, "hardware$serial.number", hardware$serial.number, "\n")
            hardware$config <- readBin(buf[o+19:20], "raw", n=2, size=1)
            oce.debug(debug, "hardware$config:", hardware$config, "\n")
            hardware$frequency <- readBin(buf[o+21:22], "integer", n=1, size=2, endian="little", signed=FALSE) # not used
            oce.debug(debug, "hardware$frequency:", hardware$frequency, "\n")
            hardware$pic.version <- readBin(buf[o+23:24], "integer", n=1, size=2, endian="little")
            oce.debug(debug, "hardware$pic.version=", hardware$pic.version, "\n")
            hardware$hw.revision <- readBin(buf[o+25:26], "integer", n=1, size=2, endian="little")
            oce.debug(debug, "hardware$hw.revision=", hardware$hw.revision, "\n")
            hardware$rec.size <- readBin(buf[o+27:28], "integer", n=1, size=2, endian="little")
            oce.debug(debug, "hardware$rec.size=", hardware$rec.size, "\n")
            hardware$velocity.range <- readBin(buf[o+29:30], "integer", n=1, size=2, signed=FALSE, endian="little")
            oce.debug(debug, "hardware$velocity.range=", hardware$velocity.range, "\n")
            hardware$fw.version <- as.numeric(paste(readBin(buf[o+43:46], "character", n=4, size=1), collapse=""))
            oce.debug(debug, "hardware$fw.version=", hardware$fw.version, "\n")
            o <- o + 2 * hardware$size
        } else if (buf[o+2] == id.head.configuration) {     # see page 30 of System Integrator Guide
            oce.debug(debug, "HEAD CONFIGURATION\n")
            ##buf <- readBin(file, "raw", header.length.head)
            head$size <- readBin(buf[o+3:4], "integer",signed=FALSE, n=1, size=2)
            if (2 * head$size != header.length.head)
                stop("size of head header expected to be ", header.length.head, "but got ", head$size)
            oce.debug(debug, "head$size=", head$size, "\n")
            head$config <- byte2binary(buf[o+5:6], endian="little")
            oce.debug(debug, "head$config=", head$config, "\n")
            head$config.pressure.sensor <- substr(head$config[1], 1, 1) == "1"
            oce.debug(debug, "head$config.pressure.sensor=", head$config.pressure.sensor,"\n")
            head$config.magnetometer.sensor <- substr(head$config[1], 2, 2) == "1"
            oce.debug(debug, "head$config.magnetometer.sensor=", head$config.magnetometer.sensor,"\n")
            head$config.tilt.sensor <- substr(head$config[1], 3, 3) == "1"
            oce.debug(debug, "head$config.tilt.sensor=", head$config.tilt.sensor,"\n")
            head$orientation <- if (substr(head$config[1], 4, 4) == "1") "downward" else "upward"
            oce.debug(debug, "head$orientation=", head$orientation, "\n")
            head$frequency <- readBin(buf[o+7:8], "integer", n=1, size=2, endian="little", signed=FALSE)
            oce.debug(debug, "head$frequency=", head$frequency, "kHz\n")
            head$head.type <- readBin(buf[o+9:10], "integer", n=1, size=2, endian="little")
            oce.debug(debug, "head$head.type=", head$head.type, "\n")
            head$head.serial.number <- gsub(" *$", "", paste(readBin(buf[o+11:22], "character", n=12, size=1), collapse=""))
            oce.debug(debug, "head$head.serial.number=", head$head.serial.number, "\n")
            ## NOTE: p30 of System Integrator Guide does not detail anything from offsets 23 to 119;
            ## the inference of beam.angles and transformation.matrix is drawn from other code.
            ## Since I don't trust any of this, I hard-wire beam angle in at the end.
            head$beam.angles <- readBin(buf[o+23:30], "integer", n=4, size=2, endian="little", signed=TRUE)

            oce.debug(debug, "head$beam.angles=", head$beam.angles, "(deg)\n")
            ## Transformation matrix (before division by 4096)
            ## FIXME: should we change the sign of rows 2 and 3 if pointed down??
            head$transformation.matrix <- matrix(readBin(buf[o+31:48], "integer", n=9, size=2, endian="little") ,
                                                 nrow=3, byrow=TRUE) / 4096
            oce.debug(debug, "head$transformation.matrix\n")
            oce.debug(debug, format(head$transformation.matrix[1,], width=15), "\n")
            oce.debug(debug, format(head$transformation.matrix[2,], width=15), "\n")
            oce.debug(debug, format(head$transformation.matrix[3,], width=15), "\n")
            head$number.of.beams <- readBin(buf[o+221:222], "integer", n=1, size=2, endian="little")
            oce.debug(debug, "head$number.of.beams=", head$number.of.beams, "\n")
            o <- o + 2 * head$size
        } else if (buf[o+2] == id.user.configuration) {     # User Configuration [p30-32 of System Integrator Guide]
            oce.debug(debug, "USER CONFIGURATION\n")
            user$size <- readBin(buf[o+3:4], what="integer", n=1, size=2, endian="little")
            if (2 * user$size != header.length.user)
                stop("size of user header expected to be ", header.length.user, "but got ", user$size)
            ##buf <- readBin(file, "raw", header.length.user)

            user$transmit.pulse.length <- readBin(buf[o+5:6], "integer", n=1, size=2, endian="little", signed=FALSE)
            oce.debug(debug, "user$transmit.pulse.length=", user$transmit.pulse.lengthu, "in counts\n")
            user$blanking.distance <- readBin(buf[o+7:8], "integer", n=1, size=2, endian="little", signed=FALSE)
            oce.debug(debug, "user$blanking.distance=", user$blanking.distance, "in counts\n")
            user$time.between.pings <- readBin(buf[o+9:10], "integer", n=1, size=2, endian="little", signed=FALSE)
            oce.debug(debug, "user$time.between.pings=", user$time.between.pings, "in counts\n")
            user$number.of.beam.sequences.per.burst <- readBin(buf[o+11:12], "integer", n=1, size=2, endian="little", signed=FALSE)
            oce.debug(debug, "user$number.of.beam.sequences.per.burst=", user$number.of.beam.sequences.per.burst, "in counts\n")
            user$time.between.beam.sequences <- readBin(buf[o+13:14], "integer", n=1, size=2, endian="little", signed=FALSE)
            oce.debug(debug, "user$time.between.beam.sequences=", user$time.between.beam.sequences, "in counts\n")
            user$NPings <- readBin(buf[o+15:16], "integer", n=1, size=2, endian="little")
            oce.debug(debug, "user$NPings=", user$NPings, "\n")
            user$AvgInterval <- readBin(buf[o+17:18], "integer", n=1, size=2, endian="little")
            oce.debug(debug, "user$AvgInterval=", user$AvgInterval, "in seconds\n")
            user$number.of.beams <- readBin(buf[o+19:20], "integer", n=1, size=2, endian="little")
            oce.debug(debug, "user$number.of.beams=", user$number.of.beams, "\n")
            user$measurement.interval <- readBin(buf[o+39:40], "integer", n=1, size=2, endian="little")
            oce.debug(debug, "user$measurement.interval=", user$measurement.interval, "\n")
            user$deploy.name <- readBin(buf[o+41:47], "character", n=1, size=6)
            oce.debug(debug, "user$deploy.name=", user$deploy.name, "\n")
            user$comments <- readBin(buf[o+257+0:179], "character", n=1, size=180)
            oce.debug(debug, "user$comments=", user$comments, "\n")

            user$mode <- byte2binary(buf[o+59:60], endian="little")
            oce.debug(debug, "user$mode: ", user$mode, "\n")
            user$velocity.scale <- if (substr(user$mode[2], 4, 4) == "0") 0.001 else 0.00001
            oce.debug(debug, "user$velocity.scale: ", user$velocity.scale, "\n")
            tmp.cs <- readBin(buf[o+33:34], "integer", n=1, size=2, endian="little")
            if (tmp.cs == 0) user$coordinate.system <- "enu" # page 31 of System Integrator Guide
            else if (tmp.cs == 1) user$coordinate.system <- "xyz"
            else if (tmp.cs == 2) user$coordinate.system <- "beam"
            else stop("unknown coordinate system ", tmp.cs)
            oce.debug(debug, "user$coordinate.system: ", user$coordinate.system, "\n")
            user$number.of.cells <- readBin(buf[o+35:36], "integer", n=1, size=2, endian="little")
            oce.debug(debug, "user$number.of.cells: ", user$number.of.cells, "\n")
            user$hBinLength <- readBin(buf[o+37:38], "integer", n=1, size=2, endian="little", signed=FALSE)
            if (isTRUE(all.equal.numeric(head$frequency, 1000))) {
                ##  printf("\nCell size (m) ------------ %.2f", cos(DEGTORAD(25.0))*conf.hBinLength*0.000052734375);
                user$cell.size <- cos(25*pi/180) * user$hBinLength * 0.000052734375
            } else if (isTRUE(all.equal.numeric(head$frequency, 2000))) { # FIXME: use head$frequency or hardware$frequency?
                ##  printf("\nCell size (m) ------------ %.2f",     cos(DEGTORAD(25.0))*conf.hBinLength*0.0000263671875);
                user$cell.size <- cos(25*pi/180) * user$hBinLength *0.0000263671875
            } else {
                user$cell.size <- NA    # FIXME what should we do here?  Probably an ADV, so no concern
            }
            oce.debug(debug, "cell.size=", user$cell.size, "m\n")
            user$measurement.interval <- readBin(buf[o+39:40], "integer", n=1, size=2, endian="little")
            if (isTRUE(all.equal.numeric(head$frequency, 1000))) {
                ## printf("\nBlanking distance (m) ---- %.2f", cos(DEGTORAD(25.0))*(0.0135*conf.hT2 - 12.0*conf.hT1/head.hFrequency));
                user$blanking.distance <- cos(25*pi/180) * (0.0135 * user$T2 - 12 * user$T1 / head$frequency)
            } else if (isTRUE(all.equal.numeric(head$frequency, 2000))) {
                ## printf("\nBlanking distance (m) ---- %.2f", cos(DEGTORAD(25.0))*(0.00675*conf.hT2 - 12.0*conf.hT1/head.hFrequency));
                user$blanking.distance <- cos(25*pi/180) * (0.00675 * user$T2 - 12 * user$T1 / head$frequency)
            } else {
                user$blanking.distance <- 0
            }
            oce.debug(debug, "blanking.distance=", user$blanking.distance, "; user$T1=", user$T1, "and user$T2=", user$T2, "\n")
            oce.debug(debug, "measurement.interval=", user$measurement.interval, "\n")
            user$deployment.name <- readBin(buf[o+41:46], "character")
            user$sw.version <- readBin(buf[o+73:74], "integer", n=1, size=2, endian="little")
            oce.debug(debug, "sw.version=", user$sw.version,"\n")
            user$salinity <- readBin(buf[o+75:76], "integer", n=1, size=2, endian="little") * 0.1
            oce.debug(debug, "salinity=", user$salinity,"\n")
            o <- o + 2 * user$size
        } else {
            stop("cannot understand byte 0x", buf[o+1], "; expecting one of the following: 0x", id.hardware.configuration, " [hardware configuration] 0x", id.head.configuration, " [head configuration] or 0x", id.user.configuration, " [user configuration]\n")
        }
    }
    list(hardware=hardware, head=head, user=user, offset=o+1)
}

read.adp.nortek <- function(file, from=1, to, by=1, tz=getOption("oce.tz"),
                            type=c("aquadopp high resolution"),
                            debug=getOption("oce.debug"), monitor=TRUE, log.action, ...)
{
    bisect.adp.nortek <- function(t.find, add=0, debug=0) {
        oce.debug(debug, "bisect.adp.nortek(t.find=", format(t.find), ", add=", add, "\n")
        len <- length(profile.start)
        lower <- 1
        upper <- len
        passes <- floor(10 + log(len, 2)) # won't need this many; only do this to catch coding errors
        for (pass in 1:passes) {
            middle  <- floor((upper + lower) / 2)
            minute  <- bcd2integer(buf[profile.start[middle] + 4])
            second  <- bcd2integer(buf[profile.start[middle] + 5])
            day     <- bcd2integer(buf[profile.start[middle] + 6])
            hour    <- bcd2integer(buf[profile.start[middle] + 7])
            year    <- bcd2integer(buf[profile.start[middle] + 8])
            year    <- year + ifelse(year >= 90, 1900, 2000)
            month   <- bcd2integer(buf[profile.start[middle] + 9])
            sec1000 <- bcd2integer(buf[profile.start[middle] + 10])
            t <- ISOdatetime(year, month, day, hour, minute, second + sec1000/1000, tz=tz)
            oce.debug(debug, "t=", format(t), "| (from data", sprintf("%4d-%02d-%02d", year, month, day), sprintf("%02d:%02d:%02d.%03d", hour, minute, second, sec1000), ") | pass", format(pass, width=2), "/", passes, " | middle=", middle, "(", middle/upper*100, "%)\n")
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
        minute  <- bcd2integer(buf[profile.start[middle] + 4])
        second  <- bcd2integer(buf[profile.start[middle] + 5])
        day     <- bcd2integer(buf[profile.start[middle] + 6])
        hour    <- bcd2integer(buf[profile.start[middle] + 7])
        year    <- bcd2integer(buf[profile.start[middle] + 8])
        year    <- year + ifelse(year >= 90, 1900, 2000)
        month   <- bcd2integer(buf[profile.start[middle] + 9])
        sec1000 <- bcd2integer(buf[profile.start[middle] + 10])
        t <- ISOdatetime(year, month, day, hour, minute, second + sec1000/1000, tz=tz)
        oce.debug(debug, "result: t=", format(t), " at vsd.start[", middle, "]=", profile.start[middle], "\n")
        return(list(index=middle, time=t))
    }
    oce.debug(debug, "read.adp.nortek(...,from=",format(from),",to=",format(to), "...)\n")
    from.keep <- from
    to.keep <- to
    sync.code <- as.raw(0xa5)
    if (is.character(file)) {
        filename <- full.filename(file)
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
    seek(file, 0, "start")
    seek(file, 0, "start")
    ## go to the end, so the next seek (to get to the data) reveals file length
    seek(file, where=0, origin="end")
    file.size <- seek(file, where=0)
    oce.debug(debug, "file.size=", file.size, "\n")
    buf <- readBin(file, what="raw", n=file.size, size=1)
    header <- decode.header.nortek(buf, debug=debug-1)
    number.of.beams <- header$number.of.beams
    number.of.cells <- header$number.of.cells
    bin1.distance <- header$bin1.distance
    xmit.pulse.length <- header$xmit.pulse.length
    cell.size <- header$cell.size
    ##profiles.in.file <- readBin(buf[header$offset + 2:3], what="integer", n=1, size=2, endian="little")
    oce.debug(debug, "profile data at buf[", header$offset, "] et seq.\n")
    profile.start <- .Call("match3bytes", buf, buf[header$offset], buf[header$offset+1], buf[header$offset+2])
    profiles.in.file <- length(profile.start)
    oce.debug(debug, "profiles.in.file=", profiles.in.file, "\n")
    measurement.start <- ISOdatetime(2000+bcd2integer(buf[profile.start[1]+8]), # year FIXME: have to check if before 1990
                                  bcd2integer(buf[profile.start[1]+9]), # month
                                  bcd2integer(buf[profile.start[1]+6]), # day
                                  bcd2integer(buf[profile.start[1]+7]), # hour
                                  bcd2integer(buf[profile.start[1]+4]), # min
                                  bcd2integer(buf[profile.start[1]+5]), # sec
                                  tz=tz)
    measurement.end <- ISOdatetime(2000+bcd2integer(buf[profile.start[profiles.in.file]+8]), # year FIXME: have to check if before 1990
                                bcd2integer(buf[profile.start[profiles.in.file]+9]), # month
                                bcd2integer(buf[profile.start[profiles.in.file]+6]), # day
                                bcd2integer(buf[profile.start[profiles.in.file]+7]), # hour
                                bcd2integer(buf[profile.start[profiles.in.file]+4]), # min
                                bcd2integer(buf[profile.start[profiles.in.file]+5]), # sec
                                tz=tz)
    measurement.deltat <- as.numeric(
                                     ISOdatetime(2000+bcd2integer(buf[profile.start[2]+8]), # year FIXME: have to check if before 1990
                                                 bcd2integer(buf[profile.start[2]+9]), # month
                                                 bcd2integer(buf[profile.start[2]+6]), # day
                                                 bcd2integer(buf[profile.start[2]+7]), # hour
                                                 bcd2integer(buf[profile.start[2]+4]), # min
                                                 bcd2integer(buf[profile.start[2]+5]), # sec
                                                 tz=tz)) - as.numeric(measurement.start)

    oce.debug(debug, "ORIG measurement.deltat=", measurement.deltat, "\n")

    measurement.deltat <- (as.numeric(measurement.end) - as.numeric(measurement.start)) / profiles.in.file

    oce.debug(debug, "NEW  measurement.deltat=", measurement.deltat, "\n")


    if (inherits(from, "POSIXt")) {
        if (!inherits(to, "POSIXt")) stop("if 'from' is POSIXt, then 'to' must be, also")
        from.pair <- bisect.adp.nortek(from, -1, debug-1)
        from <- from.index <- from.pair$index
        to.pair <- bisect.adp.nortek(to, 1, debug-1)
        to <- to.index <- to.pair$index
        oce.debug(debug, "  from=", format(from.pair$t), " yields profile.start[", from.index, "]\n",
                  "  to  =", format(to.pair$t),   " yields profile.start[", to.index, "]\n",
                  "  by=", by, "s\n",
                  "profile.start[1:10]=", profile.start[1:10],"\n",
                  "profile.start[",from.pair$index, "]=", profile.start[from.pair$index], "at time", format(from.pair$t), "\n",
                  "profile.start[",  to.pair$index, "]=", profile.start[  to.pair$index], "at time", format(  to.pair$t), "\n")
        time1 <- ISOdatetime(2000+bcd2integer(buf[profile.start[1]+8]), # year FIXME: have to check if before 1990
                             bcd2integer(buf[profile.start[1]+9]), # month
                             bcd2integer(buf[profile.start[1]+6]), # day
                             bcd2integer(buf[profile.start[1]+7]), # hour
                             bcd2integer(buf[profile.start[1]+4]), # min
                             bcd2integer(buf[profile.start[1]+5]), # sec
                             tz=tz)
        time2 <- ISOdatetime(2000+bcd2integer(buf[profile.start[2]+8]), # year FIXME: have to check if before 1990
                             bcd2integer(buf[profile.start[2]+9]), # month
                             bcd2integer(buf[profile.start[2]+6]), # day
                             bcd2integer(buf[profile.start[2]+7]), # hour
                             bcd2integer(buf[profile.start[2]+4]), # min
                             bcd2integer(buf[profile.start[2]+5]), # sec
                             tz=tz)
        dt <- as.numeric(difftime(time2, time1, units="secs"))
        oce.debug(debug, "dt=", dt, "s; at this stage, by=", by,"(not interpreted yet)\n")
        profile.start <- profile.start[profile.start[from.index] < profile.start & profile.start < profile.start[to.index]]
        if (is.character(by))
            by <- floor(0.5 + ctime.to.seconds(by) / dt)
        oce.debug(debug, "by=",by,"profiles (after change)\n")
        profile.start <- profile.start[seq(1, length(profile.start), by=by)]
        oce.debug(debug, 'dt=',dt,'\n', 'by=',by, "profile.start[1:10] after indexing:", profile.start[1:10], "\n")
    } else {
        from.index <- from
        to.index <- to
        if (to.index < 1 + from.index) stop("need more separation between from and to")
        if (is.character(by)) stop("cannot have string for 'by' if 'from' and 'to' are integers")
        profile.start <- profile.start[seq(from=from, to=to, by=by)]
        oce.debug(debug, "profile.start[1:10] after indexing:", profile.start[1:10], "\n")
    }
    profiles.to.read <- length(profile.start)
    oce.debug(debug, "profiles.to.read=",profiles.to.read,"\n")
    profile.start2 <- sort(c(profile.start, profile.start+1)) # use this to subset for 2-byte reads
    number.of.cells <- header$user$number.of.cells
    number.of.beams <- header$head$number.of.beams
    oce.debug(debug, "number.of.cells=", number.of.cells,"\n")
    oce.debug(debug, "number.of.beams=", number.of.beams,"\n")
    items <-  number.of.cells *  number.of.beams
    time <- ISOdatetime(2000+bcd2integer(buf[profile.start+8]), # year FIXME: have to check if before 1990
                        bcd2integer(buf[profile.start+9]), # month
                        bcd2integer(buf[profile.start+6]), # day
                        bcd2integer(buf[profile.start+7]), # hour
                        bcd2integer(buf[profile.start+4]), # min
                        bcd2integer(buf[profile.start+5]), # sec
                        tz=tz)
    class(time) <- c("POSIXt", "POSIXct") # FIXME do we need this?
    attr(time, "tzone") <- getOption("oce.tz") # Q: does file hold the zone?
    heading <- 0.1 * readBin(buf[profile.start2 + 18], what="integer", n=profiles.to.read, size=2, endian="little", signed=TRUE)
    pitch <- 0.1 * readBin(buf[profile.start2 + 20], what="integer", n=profiles.to.read, size=2, endian="little", signed=TRUE)
    roll <- 0.1 * readBin(buf[profile.start2 + 22], what="integer", n=profiles.to.read, size=2, endian="little", signed=TRUE)
    pressure.MSB <- readBin(buf[profile.start + 24], what="integer", n=profiles.to.read, size=1, endian="little", signed=FALSE)
    pressure.LSW <- readBin(buf[profile.start2 + 26], what="integer", n=profiles.to.read, size=2, endian="little", signed=FALSE)
    pressure <- (as.integer(pressure.MSB)*65536 + pressure.LSW) * 0.001 # CHECK
    temperature <- 0.01 * readBin(buf[profile.start2 + 28], what="integer", n=profiles.to.read, size=2, endian="little")
    v <- array(double(), dim=c(profiles.to.read, number.of.cells,  number.of.beams))
    a <- array(raw(), dim=c(profiles.to.read,  number.of.cells,  number.of.beams)) # echo amplitude
    q <- array(raw(), dim=c(profiles.to.read,  number.of.cells,  number.of.beams)) # correlation
    for (i in 1:profiles.to.read) {
        o <- profile.start[i] + 54 ## FIXME: why does 54 work, given 53 in docs? [see 38 of System Integrator Guide]
        ##oce.debug(debug, 'getting data chunk',i,' at file position',o,'\n')
        v[i,,] <- matrix(0.001 * readBin(buf[o + seq(0, 2*items-1)], "integer", n=items, size=2, endian="little", signed=TRUE),
                         ncol=number.of.beams, byrow=FALSE)
        o <- o + items * 2
        a[i,,] <- matrix(buf[o + seq(0, items-1)], ncol=items, byrow=TRUE)
        o <- o + items
        q[i,,] <- matrix(buf[o + seq(0, items-1)], ncol=items, byrow=TRUE) # FIXME: this is correlation, not quality
        if (monitor) {
            cat(".", ...)
            if (!(i %% 50)) cat(i, "\n", ...)
        }
    }
    if (monitor) cat("\nRead", profiles.to.read,  "of the", profiles.in.file, "profiles in", filename, "\n", ...)
    data <- list(ma=list(v=v, a=a, q=q),
                 ss=list(distance=seq(header$user$blanking.distance,
                         by=header$user$cell.size,
                         length.out=header$user$number.of.cells)),
                 ts=list(time=time,
                 pressure=pressure,
                 temperature=temperature,
                 heading=heading,
                 pitch=pitch,
                 roll=roll))
    metadata <- list(instrument.type="nortek aquadopp high resolution",
                     filename=filename,
                     measurement.start=measurement.start,
                     measurement.end=measurement.end,
                     measurement.deltat=measurement.deltat,
                     subsample.start=time[1],
                     subsample.end=time[length(time)],
                     subsample.deltat=as.numeric(time[2]) - as.numeric(time[1]),
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
                     beam.angle=25,     # FIXME: may change with new devices
                     orientation=header$head$orientation,
                     frequency=header$head$frequency,
                     head.serial.number=header$head$head.serial.number,
                     bin1.distance=header$user$blanking.distance, # FIXME: is this right?
                     blanking.distance=header$user$blanking.distance,
                     measurement.interval=header$user$measurement.interval,
                     transformation.matrix=header$head$transformation.matrix,
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
    class(res) <- c("nortek", "adp", "oce")
    res
}                                       # read.adp.nortek()
