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
## 4. should state the algorithms to infer cellSize, blankingDistance, etc. from file
## 5. beamAngle should be in data file
## 6. generally, docs should indicate everything that is in the files, e.g. (prominently!)
##    the beamAngles in the 'head' configuration section.
## 7. the C code suggests the velocityScale is in the second bit of conf.hMode
##    but the docs suggest the fifth bit (page 31)

decodeHeaderNortek <- function(buf, debug=getOption("oceDebug"), ...)
{
    oceDebug(debug, "decodeHeaderNortek() entry; buf[1:20]=",buf[1:20],"\n")
    degToRad <- atan2(1, 1) / 45
    syncCode <- as.raw(0xa5)
    idHardwareConfiguration <- as.raw(0x05)
    idHeadConfiguration <- as.raw(0x04)
    idUserConfiguration <- as.raw(0x00)
    headerLengthHardware <- 48
    headerLengthHead <- 224
    headerLengthUser <- 512
    hardware <- head <- user <- list()
    o <- 0                              # offset
    for (header in 1:3) { # FIXME: code is needlessly written as if headers could be in different order
        oceDebug(debug, "\n")
        oceDebug(debug, "examining buf[o+2]=", buf[o+2], "to see what type of header block is next...\n")
        if (buf[o+1] != syncCode)
            stop("expecting syncCode 0x", syncCode, " but got 0x", buf[o+1], " instead (while reading header #", header, ")")
        if (buf[o+2] == idHardwareConfiguration) {         # see page 29 of System Integrator Guide
            oceDebug(debug, "\n\bHARDWARE CONFIGURATION\n")
            hardware$size <- readBin(buf[o+3:4], "integer",signed=FALSE, n=1, size=2, endian="little")
            if (hardware$size != 24)
                stop("size of hardware header expected to be 24 two-byte words, but got ", hardware$size)
            if (2 * hardware$size != headerLengthHardware)
                stop("size of hardware header expected to be ", headerLengthHardware, "but got ", hardware$size)
            oceDebug(debug, "hardware$size=", hardware$size, "\n")
            hardware$serialNumber <- gsub(" *$", "", paste(readBin(buf[o+5:18], "character", n=14, size=1), collapse=""))
            oceDebug(debug, "hardware$serialNumber", hardware$serialNumber, "\n")
            hardware$config <- readBin(buf[o+19:20], "raw", n=2, size=1)
            oceDebug(debug, "hardware$config:", hardware$config, "\n")
            hardware$frequency <- readBin(buf[o+21:22], "integer", n=1, size=2, endian="little", signed=FALSE) # not used
            oceDebug(debug, "hardware$frequency:", hardware$frequency, "\n")
            hardware$picVersion <- readBin(buf[o+23:24], "integer", n=1, size=2, endian="little")
            oceDebug(debug, "hardware$picVersion=", hardware$picVersion, "\n")
            hardware$hwRevision <- readBin(buf[o+25:26], "integer", n=1, size=2, endian="little")
            oceDebug(debug, "hardware$hwRevision=", hardware$hwRevision, "\n")
            hardware$recSize <- readBin(buf[o+27:28], "integer", n=1, size=2, endian="little")
            oceDebug(debug, "hardware$recSize=", hardware$recSize, "\n")
            hardware$velocityRange <- readBin(buf[o+29:30], "integer", n=1, size=2, signed=FALSE, endian="little")
            oceDebug(debug, "hardware$velocityRange=", hardware$velocityRange, "\n")
            hardware$fwVersion <- as.numeric(paste(readBin(buf[o+43:46], "character", n=4, size=1), collapse=""))
            oceDebug(debug, "hardware$fw.version=", hardware$fw.version, "\n")
            o <- o + 2 * hardware$size
        } else if (buf[o+2] == idHeadConfiguration) {     # see page 30 of System Integrator Guide
            oceDebug(debug, "\n\bHEAD CONFIGURATION\n")
            ##buf <- readBin(file, "raw", headerLengthHead)
            head$size <- readBin(buf[o+3:4], "integer",signed=FALSE, n=1, size=2)
            if (2 * head$size != headerLengthHead)
                stop("size of head header expected to be ", headerLengthHead, "but got ", head$size)
            oceDebug(debug, "head$size=", head$size, "\n")
            head$config <- byteToBinary(buf[o+5:6], endian="little")
            oceDebug(debug, "head$config=", head$config, "\n")
            head$configPressureSensor <- substr(head$config[1], 1, 1) == "1"
            oceDebug(debug, "head$configPressureSensor=", head$configPressureSensor,"\n")
            head$configMagnetometerSensor <- substr(head$config[1], 2, 2) == "1"
            oceDebug(debug, "head$configMagnetometerSensor=", head$configMagnetometerSensor,"\n")
            head$configTiltSensor <- substr(head$config[1], 3, 3) == "1"
            oceDebug(debug, "head$configTiltSensor=", head$configTiltSensor,"\n")
            head$tiltSensorOrientation <- if (substr(head$config[1], 4, 4) == "1") "downward" else "upward"
            oceDebug(debug, "head$tiltSensorOrientation=", head$tiltSensorOrientation, "\n")
            head$frequency <- readBin(buf[o+7:8], "integer", n=1, size=2, endian="little", signed=FALSE)
            oceDebug(debug, "head$frequency=", head$frequency, "kHz\n")
            head$headType <- readBin(buf[o+9:10], "integer", n=1, size=2, endian="little")
            oceDebug(debug, "head$headType=", head$headType, "\n")
            head$headSerialNumber <- gsub(" *$", "", paste(readBin(buf[o+11:22], "character", n=12, size=1), collapse=""))
            oceDebug(debug, "head$headSerialNumber=", head$headSerialNumber, "\n")
            ## NOTE: p30 of System Integrator Guide does not detail anything from offsets 23 to 119;
            ## the inference of beamAngles and transformationMatrix is drawn from other code.
            ## Since I don't trust any of this, I hard-wire beamAngle in at the end.
            head$beamAngles <- readBin(buf[o+23:30], "integer", n=4, size=2, endian="little", signed=TRUE)

            oceDebug(debug, "head$beamAngles=", head$beamAngles, "(deg)\n")
            ## Transformation matrix (before division by 4096)
            ## FIXME: should we change the sign of rows 2 and 3 if pointed down??
            head$transformationMatrix <- matrix(readBin(buf[o+31:48], "integer", n=9, size=2, endian="little") ,
                                                 nrow=3, byrow=TRUE) / 4096
            oceDebug(debug, "head$transformationMatrix\n")
            oceDebug(debug, format(head$transformationMatrix[1,], width=15), "\n")
            oceDebug(debug, format(head$transformationMatrix[2,], width=15), "\n")
            oceDebug(debug, format(head$transformationMatrix[3,], width=15), "\n")
            head$numberOfBeams <- readBin(buf[o+221:222], "integer", n=1, size=2, endian="little")
            oceDebug(debug, "head$numberOfBeams=", head$numberOfBeams, "\n")
            o <- o + 2 * head$size
        } else if (buf[o+2] == idUserConfiguration) {     # User Configuration [p30-32 of System Integrator Guide]
            oceDebug(debug, "\n\bUSER CONFIGURATION\n")
            user$size <- readBin(buf[o+3:4], what="integer", n=1, size=2, endian="little")
            if (2 * user$size != headerLengthUser)
                stop("size of user header expected to be ", headerLengthUser, "but got ", user$size)
            ##buf <- readBin(file, "raw", headerLengthUser)
            user$T1 <- readBin(buf[o+5:6], "integer", n=1, size=2, endian="little", signed=FALSE)
            user$T2 <- readBin(buf[o+7:8], "integer", n=1, size=2, endian="little", signed=FALSE)
            oceDebug(debug, "user$T1=", user$T1, "; user$T2=", user$T2, "\n")
            user$transmitPulseLength <- readBin(buf[o+5:6], "integer", n=1, size=2, endian="little", signed=FALSE)
            oceDebug(debug, "user$transmitPulseLength=", user$transmitPulseLengthu, "in counts\n")
            user$blankingDistance <- readBin(buf[o+7:8], "integer", n=1, size=2, endian="little", signed=FALSE)
            oceDebug(debug, "user$blankingDistance=", user$blankingDistance, "in counts\n")
            user$timeBetweenPings <- readBin(buf[o+9:10], "integer", n=1, size=2, endian="little", signed=FALSE)
            oceDebug(debug, "user$timeBetweenPings=", user$timeBetweenPings, "in counts\n")
            user$numberOfBeamSequencesPerBurst <- readBin(buf[o+11:12], "integer", n=1, size=2, endian="little", signed=FALSE)
            oceDebug(debug, "user$numberOfBeamSequencesPerBurst=", user$numberOfBeamSequencesPerBurst, "in counts\n")
            user$timeBetweenBeamSequences <- readBin(buf[o+13:14], "integer", n=1, size=2, endian="little", signed=FALSE)
            oceDebug(debug, "user$timeBetweenBeamSequences=", user$timeBetweenBeamSequences, "in counts\n")
            user$NPings <- readBin(buf[o+15:16], "integer", n=1, size=2, endian="little")
            oceDebug(debug, "user$NPings=", user$NPings, "\n")
            user$AvgInterval <- readBin(buf[o+17:18], "integer", n=1, size=2, endian="little")
            oceDebug(debug, "user$AvgInterval=", user$AvgInterval, "in seconds\n")
            user$numberOfBeams <- readBin(buf[o+19:20], "integer", n=1, size=2, endian="little")
            oceDebug(debug, "user$numberOfBeams=", user$numberOfBeams, "\n")
            user$measurementInterval <- readBin(buf[o+39:40], "integer", n=1, size=2, endian="little")
            oceDebug(debug, "user$measurementInterval=", user$measurementInterval, "\n")
            user$deploy.name <- readBin(buf[o+41:47], "character", n=1, size=6)
            oceDebug(debug, "user$deploy.name=", user$deploy.name, "\n")
            user$comments <- readBin(buf[o+257+0:179], "character", n=1, size=180)
            oceDebug(debug, "user$comments=", user$comments, "\n")

            user$mode <- byteToBinary(buf[o+59:60], endian="little")
            oceDebug(debug, "user$mode: ", user$mode, "\n")
            user$velocityScale <- if (substr(user$mode[2], 4, 4) == "0") 0.001 else 0.00001
            oceDebug(debug, "user$velocityScale: ", user$velocityScale, "\n")
            tmp.cs <- readBin(buf[o+33:34], "integer", n=1, size=2, endian="little")
            if (tmp.cs == 0) user$coordinateSystem <- "enu" # page 31 of System Integrator Guide
            else if (tmp.cs == 1) user$coordinateSystem <- "xyz"
            else if (tmp.cs == 2) user$coordinateSystem <- "beam"
            else stop("unknown coordinateSystem ", tmp.cs)
            oceDebug(debug, "user$coordinateSystem: ", user$coordinateSystem, "\n")
            user$numberOfCells <- readBin(buf[o+35:36], "integer", n=1, size=2, endian="little")
            oceDebug(debug, "user$numberOfCells: ", user$numberOfCells, "\n")
            user$hBinLength <- readBin(buf[o+37:38], "integer", n=1, size=2, endian="little", signed=FALSE)
            oceDebug(debug, "user$hBinLength: ", user$hBinLength, "\n")
            if (isTRUE(all.equal.numeric(head$frequency, 1000))) {
                ##  printf("\nCell size (m) ------------ %.2f", cos(DEGTORAD(25.0))*conf.hBinLength*0.000052734375);
                user$cellSize <- cos(25*pi/180) * user$hBinLength * 0.000052734375
            } else if (isTRUE(all.equal.numeric(head$frequency, 2000))) { # FIXME: use head$frequency or hardware$frequency?
                ##  printf("\nCell size (m) ------------ %.2f",     cos(DEGTORAD(25.0))*conf.hBinLength*0.0000263671875);
                user$cellSize <- cos(25*pi/180) * user$hBinLength *0.0000263671875
            } else {
                user$cellSize <- NA    # FIXME what should we do here?  Probably an ADV, so no concern
            }
            oceDebug(debug, "cellSize=", user$cellSize, "m (FIXME: no docs on this)\n")
            user$measurementInterval <- readBin(buf[o+39:40], "integer", n=1, size=2, endian="little")
            oceDebug(debug, "measurementInterval=", user$measurementInterval, "\n")

            ## FIXME: Sample.cpp has 0.022888 for the factor on user$T2
            if (isTRUE(all.equal.numeric(head$frequency, 1000))) {
                user$blankingDistance <- cos(25*degToRad) * (0.0135 * user$T2 - 12 * user$T1 / head$frequency)
            } else if (isTRUE(all.equal.numeric(head$frequency, 2000))) {
                user$blankingDistance <- cos(25*degToRad) * (0.00675 * user$T2 - 12 * user$T1 / head$frequency)
            } else {
                user$blankingDistance <- 0
            }
            oceDebug(debug, "blankingDistance=", user$blankingDistance, "; user$T1=", user$T1, "and user$T2=", user$T2, "\n")
            user$deploymentName <- readBin(buf[o+41:46], "character")
            user$swVersion <- readBin(buf[o+73:74], "integer", n=1, size=2, endian="little") / 10000
            oceDebug(debug, "swVersion=", user$swVersion,"\n")
            user$salinity <- readBin(buf[o+75:76], "integer", n=1, size=2, endian="little") * 0.1
            oceDebug(debug, "salinity=", user$salinity,"\n")
            o <- o + 2 * user$size
        } else {
            stop("cannot understand byte 0x", buf[o+1], "; expecting one of the following: 0x", idHardwareConfiguration, " [hardware configuration] 0x", idHeadConfiguration, " [head configuration] or 0x", idUserConfiguration, " [user configuration]\n")
        }
    }
    list(hardware=hardware, head=head, user=user, offset=o+1)
}

read.adp.nortek <- function(file, from=1, to, by=1, tz=getOption("oceTz"),
                            latitude=NA, longitude=NA,
                            type=c("aquadopp high resolution"),
                            debug=getOption("oceDebug"), monitor=TRUE, despike=FALSE,
                            processingLog, ...)
{
    bisectAdpNortek <- function(t.find, add=0, debug=0) {
        oceDebug(debug, "bisectAdpNortek(t.find=", format(t.find), ", add=", add, "\n")
        len <- length(profileStart)
        lower <- 1
        upper <- len
        passes <- floor(10 + log(len, 2)) # won't need this many; only do this to catch coding errors
        for (pass in 1:passes) {
            middle  <- floor((upper + lower) / 2)
            minute  <- bcdToInteger(buf[profileStart[middle] + 4])
            second  <- bcdToInteger(buf[profileStart[middle] + 5])
            day     <- bcdToInteger(buf[profileStart[middle] + 6])
            hour    <- bcdToInteger(buf[profileStart[middle] + 7])
            year    <- bcdToInteger(buf[profileStart[middle] + 8])
            year    <- year + ifelse(year >= 90, 1900, 2000)
            month   <- bcdToInteger(buf[profileStart[middle] + 9])
            sec1000 <- bcdToInteger(buf[profileStart[middle] + 10])
            t <- ISOdatetime(year, month, day, hour, minute, second + sec1000/1000, tz=tz)
            oceDebug(debug, "t=", format(t), "| (from data", sprintf("%4d-%02d-%02d", year, month, day), sprintf("%02d:%02d:%02d.%03d", hour, minute, second, sec1000), ") | pass", format(pass, width=2), "/", passes, " | middle=", middle, "(", middle/upper*100, "%)\n")
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
        minute  <- bcdToInteger(buf[profileStart[middle] + 4])
        second  <- bcdToInteger(buf[profileStart[middle] + 5])
        day     <- bcdToInteger(buf[profileStart[middle] + 6])
        hour    <- bcdToInteger(buf[profileStart[middle] + 7])
        year    <- bcdToInteger(buf[profileStart[middle] + 8])
        year    <- year + ifelse(year >= 90, 1900, 2000)
        month   <- bcdToInteger(buf[profileStart[middle] + 9])
        sec1000 <- bcdToInteger(buf[profileStart[middle] + 10])
        t <- ISOdatetime(year, month, day, hour, minute, second + sec1000/1000, tz=tz)
        oceDebug(debug, "result: t=", format(t), " at vsd.start[", middle, "]=", profileStart[middle], "\n")
        return(list(index=middle, time=t))
    }
    oceDebug(debug, "read.adp.nortek(...,from=",format(from),",to=",format(to), "...)\n")
    fromKeep <- from
    toKeep <- to
    syncCode <- as.raw(0xa5)
    if (is.character(file)) {
        filename <- fullFilename(file)
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
    fileSize <- seek(file, where=0)
    oceDebug(debug, "fileSize=", fileSize, "\n")
    buf <- readBin(file, what="raw", n=fileSize, size=1)
    header <- decodeHeaderNortek(buf, debug=debug-1)
    numberOfBeams <- header$numberOfBeams
    numberOfCells <- header$numberOfCells
    bin1Distance <- header$bin1Distance
    xmitPulseLength <- header$xmitPulseLength
    cellSize <- header$cellSize
    ##profilesInFile <- readBin(buf[header$offset + 2:3], what="integer", n=1, size=2, endian="little")
    oceDebug(debug, "profile data at buf[", header$offset, "] et seq.\n")
    profileStart <- .Call("match3bytes", buf, buf[header$offset], buf[header$offset+1], buf[header$offset+2])
    profilesInFile <- length(profileStart)
    oceDebug(debug, "profilesInFile=", profilesInFile, "\n")
    measurementStart <- ISOdatetime(2000+bcdToInteger(buf[profileStart[1]+8]), # year FIXME: have to check if before 1990
                                    bcdToInteger(buf[profileStart[1]+9]), # month
                                    bcdToInteger(buf[profileStart[1]+6]), # day
                                    bcdToInteger(buf[profileStart[1]+7]), # hour
                                    bcdToInteger(buf[profileStart[1]+4]), # min
                                    bcdToInteger(buf[profileStart[1]+5]), # sec
                                    tz=tz)
    measurementEnd <- ISOdatetime(2000+bcdToInteger(buf[profileStart[profilesInFile]+8]), # year FIXME: have to check if before 1990
                                  bcdToInteger(buf[profileStart[profilesInFile]+9]), # month
                                  bcdToInteger(buf[profileStart[profilesInFile]+6]), # day
                                  bcdToInteger(buf[profileStart[profilesInFile]+7]), # hour
                                  bcdToInteger(buf[profileStart[profilesInFile]+4]), # min
                                  bcdToInteger(buf[profileStart[profilesInFile]+5]), # sec
                                  tz=tz)
    measurementDeltat <- as.numeric(ISOdatetime(2000+bcdToInteger(buf[profileStart[2]+8]), # year FIXME: have to check if before 1990
                                                bcdToInteger(buf[profileStart[2]+9]), # month
                                                bcdToInteger(buf[profileStart[2]+6]), # day
                                                bcdToInteger(buf[profileStart[2]+7]), # hour
                                                bcdToInteger(buf[profileStart[2]+4]), # min
                                                bcdToInteger(buf[profileStart[2]+5]), # sec
                                                tz=tz)) - as.numeric(measurementStart)

    oceDebug(debug, "ORIG measurement.deltat=", measurementDeltat, "\n")

    measurementDeltat <- (as.numeric(measurementEnd) - as.numeric(measurementStart)) / profilesInFile

    oceDebug(debug, "NEW  measurementDeltat=", measurementDeltat, "\n")

    if (inherits(from, "POSIXt")) {
        if (!inherits(to, "POSIXt"))
            stop("if 'from' is POSIXt, then 'to' must be, also")
        fromPair <- bisectAdpNortek(from, -1, debug-1)
        from <- fromIndex <- fromPair$index
        toPair <- bisectAdpNortek(to, 1, debug-1)
        to <- toIndex <- toPair$index
        oceDebug(debug, "  from=", format(fromPair$t), " yields profileStart[", fromIndex, "]\n",
                  "  to  =", format(toPair$t),   " yields profileStart[", toIndex, "]\n",
                  "  by=", by, "s\n",
                  "profileStart[1:10]=", profileStart[1:10],"\n",
                  "profileStart[",fromPair$index, "]=", profileStart[fromPair$index], "at time", format(fromPair$t), "\n",
                  "profileStart[",  toPair$index, "]=", profileStart[  toPair$index], "at time", format(  toPair$t), "\n")
        time1 <- ISOdatetime(2000+bcdToInteger(buf[profileStart[1]+8]), # year FIXME: have to check if before 1990
                             bcdToInteger(buf[profileStart[1]+9]), # month
                             bcdToInteger(buf[profileStart[1]+6]), # day
                             bcdToInteger(buf[profileStart[1]+7]), # hour
                             bcdToInteger(buf[profileStart[1]+4]), # min
                             bcdToInteger(buf[profileStart[1]+5]), # sec
                             tz=tz)
        time2 <- ISOdatetime(2000+bcdToInteger(buf[profileStart[2]+8]), # year FIXME: have to check if before 1990
                             bcdToInteger(buf[profileStart[2]+9]), # month
                             bcdToInteger(buf[profileStart[2]+6]), # day
                             bcdToInteger(buf[profileStart[2]+7]), # hour
                             bcdToInteger(buf[profileStart[2]+4]), # min
                             bcdToInteger(buf[profileStart[2]+5]), # sec
                             tz=tz)
        dt <- as.numeric(difftime(time2, time1, units="secs"))
        oceDebug(debug, "dt=", dt, "s; at this stage, by=", by,"(not interpreted yet)\n")
        profileStart <- profileStart[profileStart[fromIndex] < profileStart & profileStart < profileStart[toIndex]]
        if (is.character(by))
            by <- floor(0.5 + ctimeToSeconds(by) / dt)
        oceDebug(debug, "by=",by,"profiles (after change)\n")
        profileStart <- profileStart[seq(1, length(profileStart), by=by)]
        oceDebug(debug, 'dt=',dt,'\n', 'by=',by, "profileStart[1:10] after indexing:", profileStart[1:10], "\n")
    } else {
        fromIndex <- from
        toIndex <- to
        if (toIndex < 1 + fromIndex)
            stop("need more separation between from and to")
        if (is.character(by))
            stop("cannot have string for 'by' if 'from' and 'to' are integers")
        profileStart <- profileStart[seq(from=from, to=to, by=by)]
        oceDebug(debug, "profileStart[1:10] after indexing:", profileStart[1:10], "\n")
    }
    profilesToRead <- length(profileStart)
    oceDebug(debug, "profilesToRead=",profilesToRead,"\n")
    profileStart2 <- sort(c(profileStart, profileStart+1)) # use this to subset for 2-byte reads
    numberOfCells <- header$user$numberOfCells
    numberOfBeams <- header$head$numberOfBeams
    oceDebug(debug, "numberOfCells=", numberOfCells,"\n")
    oceDebug(debug, "numberOfBeams=", numberOfBeams,"\n")
    items <-  numberOfCells *  numberOfBeams
    time <- ISOdatetime(2000+bcdToInteger(buf[profileStart+8]), # year FIXME: have to check if before 1990
                        bcdToInteger(buf[profileStart+9]), # month
                        bcdToInteger(buf[profileStart+6]), # day
                        bcdToInteger(buf[profileStart+7]), # hour
                        bcdToInteger(buf[profileStart+4]), # min
                        bcdToInteger(buf[profileStart+5]), # sec
                        tz=tz)
    class(time) <- c("POSIXt", "POSIXct") # FIXME do we need this?
    attr(time, "tzone") <- getOption("oceTz") # Q: does file hold the zone?
    heading <- 0.1 * readBin(buf[profileStart2 + 18], what="integer", n=profilesToRead, size=2, endian="little", signed=TRUE)
    pitch <- 0.1 * readBin(buf[profileStart2 + 20], what="integer", n=profilesToRead, size=2, endian="little", signed=TRUE)
    roll <- 0.1 * readBin(buf[profileStart2 + 22], what="integer", n=profilesToRead, size=2, endian="little", signed=TRUE)
    pressure.MSB <- readBin(buf[profileStart + 24], what="integer", n=profilesToRead, size=1, endian="little", signed=FALSE)
    pressure.LSW <- readBin(buf[profileStart2 + 26], what="integer", n=profilesToRead, size=2, endian="little", signed=FALSE)
    pressure <- (as.integer(pressure.MSB)*65536 + pressure.LSW) * 0.001 # CHECK
    temperature <- 0.01 * readBin(buf[profileStart2 + 28], what="integer", n=profilesToRead, size=2, endian="little")
    v <- array(double(), dim=c(profilesToRead, numberOfCells,  numberOfBeams))
    a <- array(raw(), dim=c(profilesToRead,  numberOfCells,  numberOfBeams)) # echo amplitude
    q <- array(raw(), dim=c(profilesToRead,  numberOfCells,  numberOfBeams)) # correlation
    for (i in 1:profilesToRead) {
        o <- profileStart[i] + 54 ## FIXME: why does 54 work, given 53 in docs? [see 38 of System Integrator Guide]
        ##oceDebug(debug, 'getting data chunk',i,' at file position',o,'\n')
        v[i,,] <- matrix(0.001 * readBin(buf[o + seq(0, 2*items-1)], "integer", n=items, size=2, endian="little", signed=TRUE),
                         ncol=numberOfBeams, byrow=FALSE)
        o <- o + items * 2
        a[i,,] <- matrix(buf[o + seq(0, items-1)], ncol=items, byrow=TRUE)
        o <- o + items
        q[i,,] <- matrix(buf[o + seq(0, items-1)], ncol=items, byrow=TRUE) # FIXME: this is correlation, not quality
        if (monitor) {
            cat(".", ...)
            if (!(i %% 50)) cat(i, "\n", ...)
        }
    }
    if (monitor) cat("\nRead", profilesToRead,  "of the", profilesInFile, "profiles in", filename, "\n", ...)
    data <- list(v=v, a=a, q=q,
                 distance=seq(header$user$blankingDistance, by=header$user$cellSize, length.out=header$user$numberOfCells),
                 time=time,
                 pressure=pressure,
                 temperature=temperature,
                 heading=heading,
                 pitch=pitch,
                 roll=roll)
    metadata <- list(manufacturer="nortek",
                     instrumentType="aquadopp-hr",
                     filename=filename,
                     manufacturer="nortek",
                     latitude=latitude,
                     longitude=longitude,
                     numberOfSamples=dim(v)[1],
                     numberOfCells=dim(v)[2],
                     numberOfBeams=dim(v)[3],
                     measurementStart=measurementStart,
                     measurementEnd=measurementEnd,
                     measurementDeltat=measurementDeltat,
                     subsampleStart=time[1],
                     subsampleEnd=time[length(time)],
                     subsampleDeltat=as.numeric(time[2]) - as.numeric(time[1]),
                     size=header$head$size,
                     serialNumber=header$hardware$serialNumber,
                     frequency=header$head$frequency,
                     internalCodeVersion=header$hardware$picVersion,
                     hardwareRevision=header$hardware$hwRevision,
                     recSize=header$hardware$recSize,
                     velocityRange=header$hardware$velocityRange,
                     firmwareVersion=header$hardware$fwVersion,
                     config=header$hardware$config,
                     configPressureSensor=header$head$configPressureSensor,
                     configMagnetometerSensor=header$head$configMagnetometerSensor,
                     configTiltSensor=header$head$configTiltSensor,
                     beamAngle=25,     # FIXME: may change with new devices
                     tiltSensorOrientation=header$head$tiltSensorOrientation,
                     orientation=header$head$tiltSensorOrientation,
                     frequency=header$head$frequency,
                     headSerialNumber=header$head$headSerialNumber,
                     bin1Distance=header$user$blankingDistance, # FIXME: is this right?
                     blankingDistance=header$user$blankingDistance,
                     measurementInterval=header$user$measurementInterval,
                     transformationMatrix=header$head$transformationMatrix,
                     deploymentName=header$user$deploymentName,
                     cellSize=header$user$cellSize,
                     velocityScale=header$user$velocityScale,
                     coordinateSystem=header$user$coordinateSystem,
                     oceCoordinate=header$user$coordinateSystem,
                     oceBeamUnattenuated=FALSE
                     )
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    res <- list(data=data, metadata=metadata, processingLog=processingLogItem(processingLog))
    class(res) <- c("nortek", "adp", "oce")
    res
}                                       # read.adp.nortek()
