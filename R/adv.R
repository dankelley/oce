read.adv <- function(file, from=1, to, by=1, tz=getOption("oceTz"),
                     type=c("nortek", "sontek", "sontek.adr", "sontek.text"),
                     header=TRUE,
                     latitude=NA, longitude=NA,
                     start, deltat,
                     debug=getOption("oceDebug"), monitor=TRUE, history)
{
    type = match.arg(type)
    ## FIXME: all these read.adv variants should have the same argument list
    if (type == "nortek")
        read.adv.nortek(file=file, from=from, to=to, by=by, tz=tz,
                        header=header,
                        latitude=latitude, longitude=longitude,
                        debug=debug, monitor=monitor, history=history)
    else if (type == "sontek") # guess
        read.adv.sontek.serial(file=file, from=from, to=to, by=by, tz=tz,
                               latitude=latitude, longitude=longitude,
                               start=start, deltat=deltat,
                               debug=debug, monitor=monitor, history=history)
    else if (type == "sontek.adr")
        read.adv.sontek.adr(file=file, from=from, to=to, by=by, tz=tz,
                            latitude=latitude, longitude=longitude,
                            debug=debug, history=history)
    else if (type == "sontek.text")
        read.adv.sontek.text(basefile=file, from=from, to=to, by=by, tz=tz,
                             latitude=latitude, longitude=longitude,
                             debug=debug, history=history)
    else
        stop("read.adv() cannot understand type = \"", type, "\"")
}

read.adv.nortek <- function(file, from=1, to, by=1, tz=getOption("oceTz"),
                            type="vector",
                            header=TRUE,
                            latitude=NA, longitude=NA,
                            debug=getOption("oceDebug"), monitor=TRUE, history)
{
    ## abbreviations:
    ##   SIG=System Integrator Guide
    ##   vvd=vector velocity data [p35 SIG], containing the data: pressure, vel, amp, corr (plus sensemble counter etc)
    ##   vsd=velocity system data [p36 SIG], containing times, temperatures, angles, etc
    ## NOTE: we interpolate from vsd to vvd, to get the final data$time, etc.

    oceDebug(debug, "\b\bread.adv.nortek(file=\"", file, "\", type=\"", type, "\", from=", format(from), ", to=", format(to), ", by=", by, ", tz=\"", tz, "\", type=\"", type, "\", header=", header, ", debug=", debug, ", monitor=", monitor, ", history=(not shown)) {\n", sep="")
    if (is.numeric(by) && by < 1)
        stop("cannot handle negative 'by' values")
    if (is.numeric(by)   && by   < 1)
        stop("argument \"by\" must be 1 or larger")
    if (is.numeric(from) && from < 1)
        stop("argument \"from\" must be 1 or larger")
    if (!missing(to) && is.numeric(to)   && to   < 1)
        stop("argument \"to\" must be 1 or larger")

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
    if (!header)
        stop("header must be TRUE")
    oceDebug(debug, "  read.adv.nortek() about to read header\n")
    oceDebug(debug, "  read.adv.nortek() finished reading header\n")
                                        # find file length
    seek(file, 0, "end")
    fileSize <- seek(file, 0, "start")
    oceDebug(debug, "  fileSize=", fileSize, "\n")
    buf <- readBin(file, "raw", fileSize)
    header <- decodeHeaderNortek(buf, debug=debug-1)
    if (debug > 1) {                    # Note: need high debugging to get this
        cat("\nheader is as follows:\n")
        str(header)
    }
    metadata <- list(manufacturer="nortek",
                     instrumentType="vector",
                     filename=filename,
                     latitude=latitude, longitude=longitude,
                     measurementStart=NA, # FIXME
                     measurementEnd=NA,   # FIXME
                     samplingRate=NA, # FIXME
                     numberOfBeams=header$head$numberOfBeams, # FIXME: check that this is correct
                     serialNumber=header$hardware$serialNumber,
                     frequency=header$head$frequency,
                     internalCodeVersion=header$hardware$picVersion,
                     softwareVersion=header$user$swVersion,
                     hardwareRevision=header$hardware$hwRevision,
                     recSize=header$hardware$recSize,
                     velocityRange=header$hardware$velocityRange,
                     firmwareVersion=header$hardware$fwVersion,
                     config=header$hardware$config,
                     configPressureSensor=header$head$configPressureSensor,
                     configMagnetometerSensor=header$head$configMagnetometerSensor,
                     configTiltSensor=header$head$configTiltSensor,
                     beamAngle=25,     # FIXME: should read from file
                     tiltSensorOrientation=header$head$tiltSensorOrientation,
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
                     oceBeamUnattenuated=FALSE,
                     deployName=header$user$deployName,
                     comments=header$user$comments)
    if (missing(history))
        history <- paste(deparse(match.call()), sep="", collapse="")
    hitem <- historyItem(history)
    ## Find the focus time by bisection, based on "sd" (system data, containing a time).
    bisectNortekVectorSd <- function(tFind, add=0, debug=0) { # tFind=time add=offset debug=debug
        oceDebug(debug, "\n")
        oceDebug(debug, "bisectNortekVectorSd(tFind=", format(tFind), ", add=", add, ", debug=", debug, ")\n")
        vsdLen <- length(vsdStart)
        lower <- 1
        upper <- vsdLen
        passes <- floor(10 + log(vsdLen, 2)) # won't need this many; only do this to catch coding errors
        for (pass in 1:passes) {
            middle <- floor((upper + lower) / 2)
            t <- ISOdatetime(2000 + bcdToInteger(buf[vsdStart[middle]+8]),  # year
                             bcdToInteger(buf[vsdStart[middle]+9]), # month
                             bcdToInteger(buf[vsdStart[middle]+6]), # day
                             bcdToInteger(buf[vsdStart[middle]+7]), # hour
                             bcdToInteger(buf[vsdStart[middle]+4]), # min
                             bcdToInteger(buf[vsdStart[middle]+5]), # sec
                             tz=tz)
            if (tFind < t)
                upper <- middle
            else
                lower <- middle
            if (upper - lower < 2)
                break
            oceDebug(debug, "examine: t=", format(t), " middle=", middle, " lower=", lower, " upper=", upper, " pass=", pass, " of max=", passes, "\n")
        }
        middle <- middle + add
        if (middle < 1)
            middle <- 1
        if (middle > vsdLen)
            middle <- vsdLen
        t <- ISOdatetime(2000 + bcdToInteger(buf[vsdStart[middle]+8]),  # year
                         bcdToInteger(buf[vsdStart[middle]+9]), # month
                         bcdToInteger(buf[vsdStart[middle]+6]), # day
                         bcdToInteger(buf[vsdStart[middle]+7]), # hour
                         bcdToInteger(buf[vsdStart[middle]+4]), # min
                         bcdToInteger(buf[vsdStart[middle]+5]), # sec
                         tz=tz)
        oceDebug(debug, "result: t=", format(t), " at vsdStart[", middle, "]=", vsdStart[middle], "\n")
        return(list(index=middle, time=t)) # index is within vsd
    }
    ## system.time() reveals that a 100Meg file is scanned in 0.2s [macpro desktop, circa 2009]
    vvdStart <- .Call("locate_byte_sequences", buf, c(0xa5, 0x10), 24, c(0xb5, 0x8c), 0)
    vsdStart <- .Call("locate_byte_sequences", buf, c(0xa5, 0x11), 28, c(0xb5, 0x8c), 0)
    ##TEST## .vsd <<- buf[vsdStart[1] + seq(0, 27)] # FIXME: remove
    ## FIXME: determine whether to use the velocity scale in next line, or other value.
    oceDebug(debug, "VSD", paste("0x", format(as.raw(buf[vsdStart[1]+0:27])),sep=""), "\n")

    ## Velocity scale.  Nortek's System Integrator Guide (p36) says
    ## the velocity scale is in bit 1 of "status" byte (at offset 23)
    ## in the Vector System Data header.  However, they seem to count
    ## bits in the opposite way as oce does, so their bit 1 (starting
    ## from 0) corresponds to our bit 7 (ending at 8).
    ##
    ## NOTE: the S.I.G. is confusing on the velocity scale, and this
    ## confusion resulted in a change to the present code on
    ## 2010-09-13.  Page 35 of S.I.G. clearly states that velocities
    ## are in mm/s, which was used in the code previously.  However,
    ## p44 contradicts this, saying that there are two possible scale
    ## factors, namely 1mm/s and 0.1mm/s.  Starting on 2010-09-13, the
    ## present function started using this possibility of two scale
    ## factors, as determined in the next code line, following p36.
    metadata$velocityScale <- if ("0" == substr(byteToBinary(buf[vsdStart[1] + 23], endian="big"), 7, 7)) 1e-3 else 0.1e-3
    oceDebug(debug, "velocity scale:", metadata$velocityScale, "m/s (from VSD header byte 24, 0x",
              as.raw(buf[vsdStart[1] + 23]), "(bit 7 of",
              byteToBinary(buf[vsdStart[1] + 23], endian="big"), ")\n")

    ## Measurement start and end times.
    vsdLen <- length(vsdStart)
    metadata$measurementStart <- ISOdatetime(2000 + bcdToInteger(buf[vsdStart[1]+8]),  # year
                                              bcdToInteger(buf[vsdStart[1]+9]), # month
                                              bcdToInteger(buf[vsdStart[1]+6]), # day
                                              bcdToInteger(buf[vsdStart[1]+7]), # hour
                                              bcdToInteger(buf[vsdStart[1]+4]), # min
                                              bcdToInteger(buf[vsdStart[1]+5]), # sec
                                              tz=tz)
    metadata$measurementEnd <- ISOdatetime(2000 + bcdToInteger(buf[vsdStart[vsdLen]+8]),  # year
                                           bcdToInteger(buf[vsdStart[vsdLen]+9]), # month
                                           bcdToInteger(buf[vsdStart[vsdLen]+6]), # day
                                           bcdToInteger(buf[vsdStart[vsdLen]+7]), # hour
                                           bcdToInteger(buf[vsdStart[vsdLen]+4]), # min
                                           bcdToInteger(buf[vsdStart[vsdLen]+5]), # sec
                                           tz=tz)
    vvdLen <- length(vvdStart)
    metadata$measurementDeltat <- (as.numeric(metadata$measurementEnd) - as.numeric(metadata$measurementStart)) / (vvdLen - 1)

    if (missing(to))
        stop("must supply 'to'")

    ## Window data buffer, using bisection in case of a variable number of vd between sd pairs.
    if (inherits(from, "POSIXt")) {
        if (!inherits(to, "POSIXt"))
            stop("if 'from' is POSIXt, then 'to' must be, also")
        fromPair <- bisectNortekVectorSd(from, -1, debug-1)
        from <- fromIndex <- fromPair$index
        toPair <- bisectNortekVectorSd(to, 1, debug-1)
        to <- toIndex <- toPair$index
        byTime <- ctimeToSeconds(by)
        oceDebug(debug,
                  "  from=", format(fromPair$t), " yields vsdStart[", fromIndex, "]\n",
                  "  to  =", format(toPair$t),   " yields vsdStart[", toIndex, "]\n",
                  "  by=", by, "byTime=", byTime, "s\n",
                  "vsdStart[",fromPair$index, "]=", vsdStart[fromPair$index], "at time", format(fromPair$t), "\n",
                  "vsdStart[",  toPair$index, "]=", vsdStart[  toPair$index], "at time", format(  toPair$t), "\n")
        twoTimes <- ISOdatetime(2000 + bcdToInteger(buf[vsdStart[1:2]+8]),  # year
                                 bcdToInteger(buf[vsdStart[1:2]+9]), # month
                                 bcdToInteger(buf[vsdStart[1:2]+6]), # day
                                 bcdToInteger(buf[vsdStart[1:2]+7]), # hour
                                 bcdToInteger(buf[vsdStart[1:2]+4]), # min
                                 bcdToInteger(buf[vsdStart[1:2]+5]), # sec  NOTE: nortek files lack fractional seconds
                                 tz=tz)
        vsd.dt <- as.numeric(twoTimes[2]) - as.numeric(twoTimes[1]) # FIXME: need # samplesPerBurst here

        ## Next two lines suggest that readBin() can be used instead of bcdToInteger ... I imagine it would be faster
        ##cat("month=", readBin(buf[vsdStart[1]+9], "integer", n=1, size=1, endian="little"), "(as readBin)\n")
        ##cat("month=", bcdToInteger(buf[vsdStart[1]+9]), "(as bcd)\n")

        oceDebug(debug, "nrecords=", readBin(buf[vsdStart[1]+10:11], "integer", n=1, size=2, endian="little"), "\n")
        oceDebug(debug, "vsd.dt=",vsd.dt,"(from twoTimes)\n")

        vvdStart <- vvdStart[vsdStart[fromIndex] <= vvdStart & vvdStart <= vsdStart[toIndex]]
        vvdDt <- vsd.dt * (toIndex - fromIndex) / length(vvdStart)
        ## find vvd region that lies inside the vsd [from, to] region.
        vvdStartFrom <- max(1, vvdStart[vvdStart < fromPair$index])
        vvdStartTo   <- min(length(vvdStart), vvdStart[vvdStart > toPair$index])
    } else {
        ## Window data buffer, using bisection in case of a variable number of vd between sd pairs.
        if (inherits(from, "POSIXt")) {
            if (!inherits(to, "POSIXt"))
                stop("if 'from' is POSIXt, then 'to' must be, also")
            fromPair <- bisectNortekVectorSd(from, -1, debug-1)
            from <- fromIndex <- fromPair$index
            toPair <- bisectNortekVectorSd(to, 1, debug-1)
            to <- toIndex <- toPair$index
            byTime <- ctimeToSeconds(by)
            oceDebug(debug,
                      "  from=", format(fromPair$t), " yields vsdStart[", fromIndex, "]\n",
                      "  to  =", format(toPair$t),   " yields vsdStart[", toIndex, "]\n",
                      "  by=", by, "byTime=", byTime, "s\n",
                      "vsdStart[",fromPair$index, "]=", vsdStart[fromPair$index], "at time", format(fromPair$t), "\n",
                      "vsdStart[",  toPair$index, "]=", vsdStart[  toPair$index], "at time", format(  toPair$t), "\n")
            twoTimes <- ISOdatetime(2000 + bcdToInteger(buf[vsdStart[1:2]+8]),  # year
                                     bcdToInteger(buf[vsdStart[1:2]+9]), # month
                                     bcdToInteger(buf[vsdStart[1:2]+6]), # day
                                     bcdToInteger(buf[vsdStart[1:2]+7]), # hour
                                     bcdToInteger(buf[vsdStart[1:2]+4]), # min
                                     bcdToInteger(buf[vsdStart[1:2]+5]), # sec  NOTE: nortek files lack fractional seconds
                                     tz=tz)
            vsd.dt <- as.numeric(twoTimes[2]) - as.numeric(twoTimes[1]) # FIXME: need # samplesPerBurst here
            ## Next two lines suggest that readBin() can be used instead of bcdToInteger ... I imagine it would be faster
            ##cat("month=", readBin(buf[vsdStart[1]+9], "integer", n=1, size=1, endian="little"), "(as readBin)\n")
            ##cat("month=", bcdToInteger(buf[vsdStart[1]+9]), "(as bcd)\n")
            oceDebug(debug, "nrecords=", readBin(buf[vsdStart[1]+10:11], "integer", n=1, size=2, endian="little"), "\n")
            oceDebug(debug, "vsd.dt=",vsd.dt,"(from twoTimes)\n")
            vvdStart <- vvdStart[vsdStart[fromIndex] < vvdStart & vvdStart < vsdStart[toIndex]]
            vvdDt <- vsd.dt * (toIndex - fromIndex) / length(vvdStart)
            oceDebug(debug,
                      'vvdDt=',vvdDt,'\n',
                      'by=',by, "1/by=",1/by,"\n",
                      "vvdStart after indexing:\n",
                      str(vvdStart))
            ## find vvd region that lies inside the vsd [from, to] region.
            vvdStartFrom <- max(1, vvdStart[vvdStart < fromPair$index])
            vvdStartTo   <- min(length(vvdStart), vvdStart[vvdStart > toPair$index])
        } else {
            oceDebug(debug, 'numeric values for args from=',from,'to=',to,'by=', by, '\n')
            fromIndex <- from
            toIndex <- to
            if (toIndex < 1 + fromIndex)
                stop("need more separation between from and to")
            oceDebug(debug, "fromIndex=", fromIndex, "toIndex=", toIndex, "\n")
            oceDebug(debug, vectorShow(vvdStart, "before subset, vvdStart is"))
            vvdStart <- vvdStart[fromIndex:toIndex]
            oceDebug(debug, vectorShow(vvdStart, "    ... later, vvdStart is"))
            oceDebug(debug, vectorShow(vsdStart, "before subset, vsdStart is"))
            vsdStartFrom <- which(vvdStart[1] < vsdStart)[1]
            vsdStartTo <- which(vsdStart > vvdStart[length(vvdStart)])[1]
            oceDebug(debug, "vsdStartFrom=", vsdStartFrom, "and vsdStartTo=", vsdStartTo, "(raw)\n")
            vsdStart <- vsdStart[seq(vsdStartFrom, vsdStartTo)]
            oceDebug(debug, vectorShow(vsdStart, "    ... later, vsdStart is"))
        }
    }
    oceDebug(debug, "about to trim vsdStart, based on vvdStart[1]=", vvdStart[1], " and vvdStart[length(vvdStart)]=", vvdStart[length(vvdStart)], "\n")
    oceDebug(debug, vectorShow(vsdStart, "before trimming, vsdStart:"))
    oceDebug(debug, "from=", from, "to=", to, "\n")

    ## Find spanning subset, expanded a little for now
    subsetStart <- which.max(vvdStart[1] < vsdStart)
    if (subsetStart > 1)
        subsetStart <- subsetStart - 1 # extend a bit (for now)
    subsetEnd <- which.min(vsdStart < vvdStart[length(vvdStart)])
    oceDebug(debug, "first guess: subsetEnd=", subsetEnd, "\n")

    if (subsetEnd < length(vsdStart))
        subsetEnd <- subsetEnd + 1

    oceDebug(debug, "try start vsdStart[subsetStart=", subsetStart, "] = ", vsdStart[subsetStart], "\n")
    oceDebug(debug, "try end   vsdStart[subsetEnd=  ", subsetEnd,   "] = ", vsdStart[subsetEnd],   "\n")
    oceDebug(debug, vectorShow(vsdStart, "before trimming, vsdStart:"))
    vsdStart <- vsdStart[seq(subsetStart, subsetEnd-1, 1)]
    oceDebug(debug, vectorShow(vsdStart, "after  trimming, vsdStart:"))

    if (2 > length(vsdStart))
        stop("need at least 2 velocity-system-data chunks to determine the timing; try increasing the difference between 'from' and 'to'")

    if (toIndex <= fromIndex)
        stop("no data in specified range from=", format(from), " to=", format(to))

    ## we make the times *after* trimming, because this is a slow operation
    ## NOTE: the ISOdatetime() call takes 60% of the entire time for this function.
    vsd.t <- ISOdatetime(2000 + bcdToInteger(buf[vsdStart+8]),  # year
                         bcdToInteger(buf[vsdStart+9]), # month
                         bcdToInteger(buf[vsdStart+6]), # day
                         bcdToInteger(buf[vsdStart+7]), # hour
                         bcdToInteger(buf[vsdStart+4]), # min
                         bcdToInteger(buf[vsdStart+5]), # sec
                         tz=tz)

    oceDebug(debug, "reading Nortek Vector, and using timezone: ", tz, "\n")

    ## update metadata$measurementDeltat
    metadata$measurementDeltat <- mean(diff(as.numeric(vsd.t)), na.rm=TRUE) * length(vsdStart) / length(vvdStart) # FIXME

    vsdLen <- length(vsdStart)
    vsdStart2 <- sort(c(vsdStart, 1 + vsdStart))
    heading <- 0.1 * readBin(buf[vsdStart2 + 14], "integer", size=2, n=vsdLen, signed=TRUE, endian="little")
    oceDebug(debug, vectorShow(heading, "heading"))
    pitch <-   0.1 * readBin(buf[vsdStart2 + 16], "integer", size=2, n=vsdLen, signed=TRUE, endian="little")
    oceDebug(debug, vectorShow(pitch, "pitch"))
    roll <-    0.1 * readBin(buf[vsdStart2 + 18], "integer", size=2, n=vsdLen, signed=TRUE, endian="little")
    oceDebug(debug, vectorShow(roll, "roll"))
    temperature <- 0.01 * readBin(buf[vsdStart2 + 20], "integer", size=2, n=vsdLen, signed=TRUE, endian="little")
    oceDebug(debug, vectorShow(temperature, "temperature"))
    ## byte 22 is an error code
    ## byte 23 is status, with bit 0 being orientation (p36 of Nortek's System Integrator Guide)
    status <- buf[vsdStart[floor(0.5*length(vsdStart))] + 23]
    metadata$orientation <- if ("0" == substr(byteToBinary(status, endian="big"), 1, 1)) "upward" else "downward"
    ##
    metadata$burstLength <- round(length(vvdStart) / length(vsdStart), 0) # FIXME: surely this is in the header (?!?)
    oceDebug(debug, vectorShow(metadata$burstLength, "burstLength"))
    vvdStart2 <- sort(c(vvdStart, 1 + vvdStart))
    vvdLen <- length(vvdStart)          # FIXME: should be subsampled with 'by' ... but how???
    p.MSB <- as.numeric(buf[vvdStart + 4])
    p.LSW <- readBin(buf[vvdStart2 + 6], "integer", size=2, n=vvdLen, signed=FALSE, endian="little")
    pressure <- (65536 * p.MSB + p.LSW) / 1000
    oceDebug(debug, vectorShow(pressure, "pressure"))
    v <- array(dim=c(vvdLen, 3))
    v[,1] <- metadata$velocityScale * readBin(buf[vvdStart2 + 10], "integer", size=2, n=vvdLen, signed=TRUE, endian="little")
    v[,2] <- metadata$velocityScale * readBin(buf[vvdStart2 + 12], "integer", size=2, n=vvdLen, signed=TRUE, endian="little")
    v[,3] <- metadata$velocityScale * readBin(buf[vvdStart2 + 14], "integer", size=2, n=vvdLen, signed=TRUE, endian="little")
    if (debug > 0) {
        oceDebug(debug, "v[", dim(v), "] begins...\n")
        print(matrix(as.numeric(v[1:min(3,vvdLen),]), ncol=3))
    }
    a <- array(raw(), dim=c(vvdLen, 3))
    a[,1] <- buf[vvdStart + 16]
    a[,2] <- buf[vvdStart + 17]
    a[,3] <- buf[vvdStart + 18]
    if (debug > 0) {
        oceDebug(debug, "a[", dim(a), "] begins...\n")
        print(matrix(as.numeric(a[1:min(3,vvdLen),]), ncol=3))
    }
    c <- array(raw(), dim=c(vvdLen, 3))
    c[,1] <- buf[vvdStart + 19]
    c[,2] <- buf[vvdStart + 20]
    c[,3] <- buf[vvdStart + 21]
    if (debug > 0) {
        cat("c[", dim(c), "] begins...\n")
        print(matrix(as.numeric(c[1:min(3,vvdLen),]), ncol=3))
    }
    sec <- as.numeric(vsd.t) - as.numeric(vsd.t[1])
    vds <- var(diff(sec))
    if (!is.na(vds) & 0 != vds)
        warning("the times in the file are not equi-spaced, but they are taken to be so")
    vvdSec <- .Call("stutter_time", sec, 8)
    oceDebug(debug, vectorShow(vvdSec, "vvdSec"))
    oceDebug(debug, vectorShow(vsdStart, "vsdStart"))
    oceDebug(debug, vectorShow(vvdStart, "vvdStart"))
    rm(buf)
    gc()
    ## subset using 'by'
    by.orig <- by
    if (is.character(by)) {
        oceDebug(debug, "by='",by,"' given as argument to read.adv.nortek()\n",sep="")
        oceDebug(debug, " ... infer to be", ctimeToSeconds(by), "s\n")
        by <- ctimeToSeconds(by) / metadata$measurementDeltat
        oceDebug(debug, " ... so step by" ,by,"through the data\n")
    }
    len <- length(vvdStart)
    look <- seq(1, len, by=by)
    oceDebug(debug, "length(vvdStart)=",length(vvdStart),"\n")
    vvdStart.orig <- vvdStart
    vvdStart <- vvdStart[look]
    oceDebug(debug, "length(vvdStart)=",length(vvdStart),"(after 'look'ing) with by=", by, "\n")
    ##heading <- approx(vsdStart, heading, xout=vvdStart, rule=2)$y
    ##pitch <- approx(vsdStart, pitch, xout=vvdStart, rule=2)$y
    ##roll <- approx(vsdStart, roll, xout=vvdStart, rule=2)$y
    ##temperature <- approx(vsdStart, temperature, xout=vvdStart, rule=2)$y
    vvdSec <- vvdSec[look]
    pressure <- pressure[look]          # only output at burst headers, not with velo
    v <- v[look,]
    a <- a[look,]
    c <- c[look,]
    ##oceDebug(debug, "vvdSec=", vvdSec[1], ",", vvdSec[2], "...\n")
    ##cat(vectorShow(vsd.t[1:10]))
    ## vsd at 1Hz; vvd at samplingRate
    time <- vvdSec + vsd.t[1]
    ##print(attributes(time)) # is time out somehow?
    ##print(time[1])
    data <- list(time=time, pressure=pressure,
                 timeVsd=NULL, heading=heading, pitch=pitch, roll=roll, temperature=temperature, # FIXME: what about timeVsd?
                 v=v, a=a, c=c)
    res <- list(data=data, metadata=metadata, history=hitem)
    class(res) <- c("nortek", "adv", "oce")
    oceDebug(debug, "\b\b} # read.adv.nortek(file=\"", filename, "\", ...)\n", sep="")
    res
}

read.adv.sontek.serial <- function(file, from=1, to, by=1, tz=getOption("oceTz"),
                                   type="default",
                                   latitude=NA, longitude=NA,
                                   start, deltat,
                                   debug=getOption("oceDebug"), monitor=TRUE, history)
{
    oceDebug(debug, paste("\b\bread.adv.sontek.serial(file[1]=\"", file[1],
                           "\", from=", format(from),
                           if (!missing(to)) sprintf(", to=%s, ", format(to)),
                           ", by=", by,
                           ", type=\"", type,
                           if (!missing(start)) sprintf(", start[1]=%s, ", format(start[1])),
                           if (!missing(deltat)) sprintf(", deltat=%f, ", deltat),
                           "debug=", debug,
                           ", monitor=", monitor,
                           ", history=(not shown)) {\n", sep=""))
    if (missing(start))
        stop("must supply start, a POSIXct time (or suitable string for time, in UTC) at which the first observation was made")
    if (is.numeric(start))
        stop("'start' must be a string, or a POSIXt time")
    if (is.character(start))
        start <- as.POSIXct(start, tz=tz)
    if (missing(deltat))
        stop("must supply deltat, the number of seconds between observations")
    nstart <- length(start)
    nfile <- length(file)
    if (nstart != nfile)
        stop("length of 'file' must equal length of 'start', but they are ", nfile, " and ", nstart, " respectively")
    warning("cannot infer coordinate system, etc., since header=FALSE; see documentation.")
    oceDebug(debug, "time series is inferred to start at", format(start[1]), "\n")
    if (is.character(deltat))
        deltat <- ctimeToSeconds(deltat)
    oceDebug(debug, "time series is inferred to have data every", deltat, "s\n")

    if (nstart > 1) {                   # handle multiple files
        oceDebug(debug, "handling multiple files\n")
        buf <- NULL
        for (i in 1:nfile) {
            oceDebug(debug, "loading \"", file[i], "\" (startTime ", format(start[i]), " ", attr(start[i], "tzone"), ")\n", sep="")
            thisFile <- file(file[i], "rb")
            seek(thisFile, 0, "end", rw="read")
            fileSize <- seek(thisFile, 0, origin="start", rw="read")
            oceDebug(debug, "fileSize=",fileSize,"\n")
            buf <- c(buf, readBin(thisFile, what="raw", n=fileSize, endian="little"))
            close(thisFile)
        }
        filename <- paste("(\"", file[i], "\", ...)", sep="")
    } else {                            # handle single file (which might be a connection, etc)
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
        ## read whole file into buffer
        seek(file, 0, "end", rw="read")
        fileSize <- seek(file, 0, origin="start", rw="read")
        oceDebug(debug, "filesize=",fileSize,"\n")
        buf <- readBin(file, what="raw", n=fileSize, endian="little")
    }

    p <- .Call("ldc_sontek_adv_22", buf, 0) # the 0 means to get all pointers to data chunks
    pp <- sort(c(p, p+1))
    len <- length(p)
    oceDebug(debug, "dp:", paste(unique(diff(p)), collapse=","), "\n")
    serialNumber <- readBin(buf[pp+2], "integer", size=2, n=len, signed=FALSE, endian="little")
    serialNumber <- .Call("unwrap_sequence_numbers", serialNumber, 2)
    velocityScale <- 0.1e-3
    v <- array(numeric(), dim=c(len, 3))
    v[,1] <- readBin(buf[pp+4], "integer", size=2, n=len, signed=TRUE, endian="little") * velocityScale
    v[,2] <- readBin(buf[pp+6], "integer", size=2, n=len, signed=TRUE, endian="little") * velocityScale
    v[,3] <- readBin(buf[pp+8], "integer", size=2, n=len, signed=TRUE, endian="little") * velocityScale
    a <- array(raw(), dim=c(len, 3))
    a[,1] <- as.raw(readBin(buf[p+10], "integer", size=1, n=len, signed=FALSE, endian="little"))
    a[,2] <- as.raw(readBin(buf[p+11], "integer", size=1, n=len, signed=FALSE, endian="little"))
    a[,3] <- as.raw(readBin(buf[p+12], "integer", size=1, n=len, signed=FALSE, endian="little"))
    c <- array(raw(), dim=c(len, 3))
    c[,1] <- as.raw(readBin(buf[p+13], "integer", size=1, n=len, signed=FALSE, endian="little"))
    c[,2] <- as.raw(readBin(buf[p+14], "integer", size=1, n=len, signed=FALSE, endian="little"))
    c[,3] <- as.raw(readBin(buf[p+15], "integer", size=1, n=len, signed=FALSE, endian="little"))
    temperature <- 0.01 * readBin(buf[pp+16], "integer", size=2, n=len, signed=TRUE, endian="little")
    pressure <- readBin(buf[pp+18], "integer", size=2, n=len, signed=FALSE, endian="little") # may be 0 for all

    ## FIXME: Sontek ADV transformation matrix equal for all units?  (Nortek Vector is not.)
    ## below for sontek serial number B373H
    ## Transformation Matrix ----->    2.710   -1.409   -1.299
    ##                       ----->    0.071    2.372   -2.442
    ##                       ----->    0.344    0.344    0.344
    ## > rbind(c(2.710,-1.409,-1.299),c(0.071,2.372,-2.442),c(0.344,0.344,0.344)) * 4096
    ##          [,1]      [,2]       [,3]
    ## [1,] 11100.160 -5771.264  -5320.704
    ## [2,]   290.816  9715.712 -10002.432
    ## [3,]  1409.024  1409.024   1409.024
    ##transformationMatrix <- rbind(c(11100, -5771,  -5321),
    ##                              c(  291,  9716, -10002),
    ##                              c( 1409,  1409,   1409)) / 4096
    transformationMatrix <- NULL
    time <- start[1] + (serialNumber - serialNumber[1]) * deltat
    deltat <- mean(diff(as.numeric(time)))
    metadata <- list(manufacturer="sontek",
                     instrumentType="adv",
                     serialNumber="?",
                     filename=filename,
                     latitude=latitude,
                     longitude=longitude,
                     transformationMatrix=transformationMatrix,
                     measurementStart=time[1],
                     measurementEnd=time[length(time)],
                     measurementDeltat=deltat,
                     subsampleStart=time[1],
                     subsampleEnd=mean(diff(as.numeric(time))),
                     subsampleDeltat=deltat,
                     coordinateSystem="xyz", # guess
                     oceCoordinate="xyz",    # guess
                     orientation="upward") # guess

    nt <- length(time)
    data <- list(time=time, # FIXME: what about other adv time?
                 heading=rep(0, nt), # user will need to fill this in
                 pitch=rep(0, nt), #  user will need to fill this in
                 roll=rep(0, nt),  # user will need to fill this in
                 temperature=temperature,
                 pressure=pressure,
                 v=v,a=a,c=c)
    warning("sontek adv in serial format lacks heading, pitch and roll: user must fill in")
    if (missing(history))
        history <- paste(deparse(match.call()), sep="", collapse="")
    hitem <- historyItem(history)
    res <- list(data=data, metadata=metadata, history=hitem)
    class(res) <- c("sontek", "adv", "oce")
    res
}

read.adv.sontek.adr <- function(file, from=1, to, by=1, tz=getOption("oceTz"),      # FIXME (twoTimescales)
                                header=TRUE,
                                latitude=NA, longitude=NA,
                                type="",
                                debug=getOption("oceDebug"), monitor=TRUE, history)
{
    bisectAdvSontekAdr <- function(tFind, add=0, debug=0) {
        oceDebug(debug, "bisectAdvSontekAdr(tFind=", format(tFind), ", add=", add, "\n")
        len <- length(burstTime)
        lower <- 1
        upper <- len
        passes <- floor(10 + log(len, 2)) # won't need this many; only do this to catch coding errors
        for (pass in 1:passes) {
            middle <- floor((upper + lower) / 2)
            t <- burstTime[middle]
            if (tFind < t)
                upper <- middle
            else
                lower <- middle
            if (upper - lower < 2)
                break
            oceDebug(debug, paste("burstTime[", middle, "] = ", format(t), " (at pass ", pass, " of ", passes, ")\n", sep=""))
        }
        middle <- middle + add          # may use add to extend before and after window
        if (middle < 1) middle <- 1
        if (middle > len) middle <- len
        t <- burstTime[middle]
        oceDebug(debug, "result: t=", format(t), "\n")
        return(list(index=middle, time=t))
    }

    ## The binary format is documented in Appendix 2.2.3 of the Sontek ADV
    ## operation Manual - Firmware Version 4.0 (Oct 1997).
    oceDebug(debug, "read.adv.sontek.adr() ENTRY\n")
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
    ## read whole file into 'buf'
    seek(file, 0, "end", rw="read")
    fileSize <- seek(file, 0, origin="start", rw="read")
    oceDebug(debug, "filesize=",fileSize,"\n")
    buf <- readBin(file, what="raw", n=fileSize, endian="little")

    ## Read header, or create a nominal default one.
    ##  24 bytes hardwareConfiguration ("AdvSystemConfigType" in the docs)
    ## 164 bytes probeConfiguration ("AdvConfType" in the docs)
    ## 253 bytes deployment setup ("AdvDeploymentSetupType" in the docs)
    hardwareConfigurationLength <- 24
    probeConfigurationLength <- 164
    deploymentParametersLength <- 253
    burstHeaderLength <- 60
    checksumLength <- 2
    dataLength <- 22                   # FIXME: this should be determined based on the headers
    metadata <- list(manufacturer="sontek",
                     instrumentType="adv", # FIXME or "adr"???
                     filename=filename,
                     latitude=latitude, longitude=longitude,
                     measurementDeltat=1,
                     velocityScaleFactor=1)
    if (header) {
        ##
        ## Slice out three headers
        ##
        hardwareConfiguration <- buf[1:hardwareConfigurationLength]
        probeConfiguration <- buf[hardwareConfigurationLength + 1:probeConfigurationLength]
        deploymentParameters <- buf[hardwareConfigurationLength+probeConfigurationLength+1:deploymentParametersLength]

        ##
        ## Analyze "hardwareConfiguration" header
        ##
        metadata$cpuSoftwareVerNum <- 0.1 * as.numeric(hardwareConfiguration[1])
        oceDebug(debug, "cpuSoftwareVerNum=", metadata$cpuSoftwareVerNum, "\n")

        metadata$dspSoftwareVerNum <- 0.1 * as.numeric(hardwareConfiguration[2])
        oceDebug(debug, "dspSoftwareVerNum=", metadata$dspSoftwareVerNum, "\n")

        metadata$orientation <- c("downward", "upward", "sideways")[1 + as.numeric(hardwareConfiguration[4])]
        oceDebug(debug, "orientation=", metadata$orientation, "\n")

        metadata$compassInstalled <- as.integer(hardwareConfiguration[5]) == 1
        oceDebug(debug, "compassInstalled=", metadata$compassInstalled, "\n")
        if (!metadata$compassInstalled)
            stop("cannot handle data files for ADV files that lack compass data")

        metadata$recorderInstalled <- if (as.integer(hardwareConfiguration[6]) == 1) TRUE else FALSE;
        oceDebug(debug, "recorderInstalled=", metadata$recorderInstalled, "\n")

        metadata$thermometerInstalled <- as.integer(hardwareConfiguration[7]) == 1
        oceDebug(debug, "thermometerInstalled=", metadata$thermometerInstalled, "\n")
        if (!metadata$thermometerInstalled)
            stop("cannot handle data files for ADV files that lack thermometer data")

        metadata$pressureInstalled <- as.integer(hardwareConfiguration[8]) == 1
        oceDebug(debug, "pressureInstalled=", metadata$pressureInstalled, "\n")
        if (!metadata$pressureInstalled)
            stop("cannot handle data files for ADV files that lack pressure data")

        ## we report pressure in dbar, so use the fact that 1 nanobar/count = 1e-8 dbar/count
        metadata$pressureScale <- 1e-8 * readBin(hardwareConfiguration[9:12], "integer", size=4, n=1, endian="little", signed=FALSE)
        oceDebug(debug, "pressureScale=", metadata$pressureScale,"dbar/count (header gives in nanobar/count)\n")

        ## we report pressure in dbar, so use the fact that 1 microbar = 1e-5 dbar
        metadata$pressureOffset <- 1e-5 * readBin(hardwareConfiguration[13:16], "integer", size=4, n=1, endian="little", signed=TRUE)
        oceDebug(debug, "pressureOffset=", metadata$pressureOffset,"dbar (header gives in microbar)\n")

        metadata$compassOffset <- readBin(hardwareConfiguration[23:24], "integer", size=2, n=1, endian="little", signed=TRUE)
        oceDebug(debug, "compassOffset=", metadata$compassOffset,"(degrees to East of North)\n")

        metadata$pressFreqOffset <- as.integer(hardwareConfiguration[25])
        oceDebug(debug, "pressFreqOffset=", metadata$pressFreqOffset,"(\"Frequency Pres Sensor Offset\" in docs)\n")

        metadata$extSensorInstalled <- as.integer(hardwareConfiguration[26])
        oceDebug(debug, "extSensorInstalled=", metadata$extSensorInstalled,"(\"0=None, 1=Standard (ch 1/3)\" in docs)\n")

        metadata$extPressInstalled <- as.integer(hardwareConfiguration[27])
        oceDebug(debug, "extPressInstalled=", metadata$extPressInstalled,"(1=Paros 2=Druck 3=ParosFreq)\n")

        ## we report pressure in dbar, so use the fact that 1 pbar = 1e-11 dbar
        metadata$pressureScale2 <- 1e-11 * readBin(hardwareConfiguration[28:29], "integer", size=2, n=1, endian="little", signed=TRUE)
        oceDebug(debug, "pressureScale2=", metadata$pressureScale2,"dbar/count^2 (file gives in picobar/count^2)\n")


        ##
        ## Analyze "probeConfiguration" header
        ## Docs (p102 of Sontek-ADV-op-man-2001.pdf) say as follows (the initial index number is mine):
        ## [1] unsigned char FileType
        ## [2] unsigned char FileVer
        ## [3:6] DateType FileDate (4 bytes for real-time clock, 2 for year, 1 for day, 1 for month)
        ## [7:10] long FileNbytes
        ## [11:16] SerialNum[6]
        ## [16] char ProbeType
        ## [17] char ProbeSize
        ## [18:19] int ProbeNBeams
        ## ...

        metadata$serialNumber <- paste(readBin(probeConfiguration[11:16],"character",n=5,size=1), collapse="")  # "B373H"
        oceDebug(debug, "serialNumber=",metadata$serialNumber,"\n")

        metadata$probeType <- readBin(probeConfiguration[17], "integer", n=1, size=1)
        oceDebug(debug, "probeType=", metadata$probeType, "(\"3/2-d orientation\", according to the docs)\n")

        metadata$probeSize <- readBin(probeConfiguration[18], "integer", n=1, size=1)
        oceDebug(debug, "probeSize=", metadata$probeSize, "(0 means 5cm; 1 means 10cm probe, according to docs)\n")

        metadata$numberOfBeams <- readBin(probeConfiguration[19:20], "integer", n=1, size=2, endian="little")
        oceDebug(debug, "numberOfBeams=", metadata$numberOfBeams, "(should be 3)\n")
        if (metadata$numberOfBeams != 3)
            warning("number of beams should be 3, but it is ", metadata$numberOfBeams, " ... reseting to 3")

        metadata$probeNomPeakPos <- readBin(probeConfiguration[21:22], "integer", n=1, size=2, endian="little")
        oceDebug(debug, "probeNomPeakPos=", metadata$probeNomPeakPos, "(not used here)\n")

        metadata$probeNsamp <- readBin(probeConfiguration[23:24], "integer", n=1, size=2, endian="little")
        oceDebug(debug, "probeNsamp=", metadata$probeNsamp, "(not used here)\n")

        metadata$probeSampInterval <- readBin(probeConfiguration[25:26], "integer", n=1, size=2, endian="little")
        oceDebug(debug, "probeSampInterval=", metadata$probeSampInterval, "(not used here)\n")

        metadata$probePulseLag <- readBin(probeConfiguration[27:56], "integer", n=15, size=2, endian="little")
        oceDebug(debug, "probePulseLag=", metadata$probePulseLag, "([5][3], not used here)\n")

        metadata$probeNxmit <- readBin(probeConfiguration[57:86], "integer", n=15, size=2, endian="little")
        oceDebug(debug, "probeNxmit=", metadata$probeNxmit, "([5][3], not used here)\n")

        metadata$probeLagDelay <- readBin(probeConfiguration[87:116], "integer", n=15, size=2, endian="little")
        oceDebug(debug, "probeLagDelay=", metadata$probeLagDelay, "([5][3], not used here)\n")

        metadata$probeBeamDelay <- readBin(probeConfiguration[117:118], "integer", n=1, size=2, endian="little")
        oceDebug(debug, "probeBeamDelay=", metadata$probeBeamDelay, "(not used here)\n")

        metadata$probePingDelay <- readBin(probeConfiguration[119:120], "integer", n=1, size=2, endian="little")
        oceDebug(debug, "probePingDelay=", metadata$probePingDelay, "(not used here)\n")

        metadata$transformationMatrix <- matrix(readBin(probeConfiguration[121:157], "numeric", n=9, size=4, endian="little"),
                                                 nrow=3, byrow=TRUE)
        oceDebug(debug, "transformation matrix:\n")
        oceDebug(debug, "  ", format(metadata$transformationMatrix[1,], width=10, digits=5, justify="right"), "\n")
        oceDebug(debug, "  ", format(metadata$transformationMatrix[2,], width=10, digits=5, justify="right"), "\n")
        oceDebug(debug, "  ", format(metadata$transformationMatrix[3,], width=10, digits=5, justify="right"), "\n")

        ## [158:161] float XmitRecDist
        ## [162:165] float CalCw
        ## FIXME why is this not 164 bytes in total?

        ##
        ## Analyze "deploymentParameters" header

        if (deploymentParameters[1]!=0x12)
            stop("first byte of deploymentParameters header should be 0x12 but it is 0x", deploymentParameters[1])

        if (deploymentParameters[2]!=0x01)
            stop("first byte of deploymentParameters header should be 0x01 but it is 0x", deploymentParameters[2])

        metadata$velocityRangeIndex <- as.numeric(deploymentParameters[20])
        oceDebug(debug, "velocityRangeIndex=", metadata$velocityRangeIndex, "\n")
        if (metadata$velocityRangeIndex == 4)
            metadata$velocityScaleFactor <- 2 # range indices 1 through 3 have factor 1

        coordinateSystemCode <- as.integer(deploymentParameters[22]) # 1 (0=beam 1=xyz 2=ENU)
        metadata$coordinateSystem <- c("beam", "xyz", "enu")[1+coordinateSystemCode]
        metadata$oceCoordinate <- metadata$coordinateSystem
        oceDebug(debug, "coordinateSystem=", metadata$coordinateSystem, "\n")
        if (metadata$coordinateSystem == "beam")
            stop("cannot handle beam coordinates")

        ## bug: docs say samplingRate in units of 0.1Hz, but the SLEIWEX-2008-m3 data file is in 0.01Hz

        samplingRate <- 0.01*readBin(deploymentParameters[23:28], "integer", n=3, size=2, endian="little", signed=FALSE)
        if (samplingRate[2] != 0 || samplingRate[3] != 0)
            warning("ignoring non-zero items 2 and/or 3 of samplingRate vector")
        metadata$samplingRate <- samplingRate[1]
        if (metadata$samplingRate < 0)
            stop("samplingRate must be a positive integer, but got ", metadata$samplingRate)
        metadata$measurementDeltat <- 1 / metadata$samplingRate
        metadata$burstInterval <- readBin(deploymentParameters[29:34], "integer", n=3, size=2, endian="little", signed=FALSE)
        if (metadata$burstInterval[2] !=0 || metadata$burstInterval[3] != 0)
            warning("ignoring non-zero items 2 and/or 3 in burstInterval vector")
        metadata$burstInterval <- metadata$burstInterval[1]
        metadata$samplesPerBurst <- readBin(deploymentParameters[35:40], "integer", n=3, size=2, endian="little", signed=FALSE)
        if (metadata$samplesPerBurst[2] !=0 || metadata$samplesPerBurst[3] != 0)
            warning("ignoring non-zero items 2 and/or 3 in samplesPerBurst vector")
        metadata$samplesPerBurst <- metadata$samplesPerBurst[1]
        if (metadata$samplesPerBurst < 0)
            stop("samplesPerBurst must be a positive integer, but got ", metadata$samplesPerBurst)
        metadata$deploymentName <- paste(integerToAscii(as.integer(deploymentParameters[49:57])), collapse="")
        metadata$comments1 <- paste(integerToAscii(as.integer(deploymentParameters[66:125])), collapse="")
        metadata$comments2 <- paste(integerToAscii(as.integer(deploymentParameters[126:185])), collapse="")
        metadata$comments3 <- paste(integerToAscii(as.integer(deploymentParameters[126:185])), collapse="")
    }                                   # if (header)

    ## Use 3-byte flag to find bursts in buf.  Then find their times, and # samples in each.
    ## Note: checking not just on the 2 "official" bytes, but also on third (3c=60=number of bytes in header)
    burstBufindex <- matchBytes(buf, 0xA5, 0x11, 0x3c)

    oceDebug(debug, "burstBufindex[1:10]=", paste(burstBufindex[1:10], collapse=" "), "\n")

    nbursts <- length(burstBufindex)
    metadata$numberOfBursts <- nbursts

    burstBufindex2 <- sort(c(burstBufindex, 1 + burstBufindex))
    year <- readBin(buf[burstBufindex2 + 18], "integer", n=nbursts, size=2, endian="little", signed=FALSE)
    day <- as.integer(buf[burstBufindex+20])
    month <- as.integer(buf[burstBufindex+21])
    minute <- as.integer(buf[burstBufindex+22])
    hour <- as.integer(buf[burstBufindex+23])
    sec100 <- as.integer(buf[burstBufindex+24])
    sec <- as.integer(buf[burstBufindex+25])
    burstTime <- as.POSIXct(ISOdatetime(year=year, month=month, day=day, hour=hour, min=minute, sec=sec+0.01*sec100, tz=tz))
    oceDebug(debug, "burstTime ranges", paste(range(burstTime), collapse=" to "), "\n")
    nbursts <- length(burstTime)
    samplesPerBurst <- readBin(buf[burstBufindex2 + 30], "integer", size=2, n=nbursts, endian="little", signed=FALSE)
    oceDebug(debug, "samplesPerBurst[1:10]=", paste(samplesPerBurst[1:10], collapse=" "), "\n")

    ## ".extended" refers to a burst sequence to which a final item has been appended,
    ## to allow the use of approx() for various things.
    burstTimeExtended <- c(burstTime, burstTime[nbursts] + samplesPerBurst[nbursts] / metadata$samplingRate)
    attr(burstTimeExtended, "tzone") <- attr(burstTime, "tzone")

    metadata$measurementStart <- min(burstTimeExtended)
    metadata$measurementEnd <- max(burstTimeExtended)
    metadata$measurementDeltat <- (as.numeric(burstTime[length(burstTime)]) - as.numeric(burstTime[1])) / sum(samplesPerBurst)

    oceDebug(debug, "burstTimeExtended ranges", paste(range(burstTimeExtended), collapse=" to "), "\n")

    ## Sample indices (not buf indices) of first sample in each burst
    burstSampleIndex.extended <- c(1, cumsum(samplesPerBurst))
    burstSampleIndex <- burstSampleIndex.extended[-length(burstSampleIndex.extended)]
    oceDebug(debug, "burstSampleIndex[1:10]=", paste(burstSampleIndex[1:10], collapse=" "), "\n")

    ## Map from sample number toBurst number
    burst <- 1:nbursts
    if (debug > 0)
        print(data.frame(burst, burstTime, burstBufindex)[1:5,])

    ## Interpret 'from', 'to', and 'by', possibly integers, POSIX times, or strings for POSIX tiems
    fromKeep <- from
    toKeep <- to
    if (inherits(from, "POSIXt")) {
        if (!inherits(to, "POSIXt"))
            stop("if 'from' is POSIXt, then 'to' must be, also")
        fromToPOSIX <- TRUE
        fromPair <- bisectAdvSontekAdr(from, add=-1, debug=debug-1)
        fromBurst <- fromPair$index
        oceDebug(debug, "fromKeep=", format(fromKeep), " yields burstTime[", fromBurst, "]=", format(fromPair$t), "\n")
        toPair <- bisectAdvSontekAdr(to, add=1, debug=debug-1)
        toBurst <- toPair$index
        oceDebug(debug, "toKeep=", format(toKeep), " yields burstTime[", toBurst, "]=", format(toPair$t), "\n")
        ## burst offsets  FIXME: do we need these?
        fromBurstOffset <- floor(0.5 + (as.numeric(from) - as.numeric(burstTime[fromBurst])) * metadata$samplingRate)
        toBurstOffset <- floor(0.5 + (as.numeric(to) - as.numeric(burstTime[toBurst-1])) * metadata$samplingRate)
        oceDebug(debug, "fromBurstOffset=", fromBurstOffset, "toBurstOffset=",toBurstOffset,"\n")
        fromIndex <- 1
        toIndex <- sum(samplesPerBurst[fromBurst:toBurst])
        oceDebug(debug, "fromIndex=", fromIndex, "toIndex=", toIndex, "\n")
    } else {
        fromToPOSIX <- FALSE
        fromIndex <- from
        toIndex <- to
        ## Determine bursts, and offsets within bursts, for fromIndex and toIndex
        tmp <- approx(burstSampleIndex, burst, fromIndex)$y
        if (is.na(tmp))
            stop("fromIndex", from, " is not in thisFile")
        fromBurst <- floor(tmp)
        fromBurstOffset <- floor(0.5 + (tmp - fromBurst)*samplesPerBurst[fromBurst])
        oceDebug(debug, "from is at index", fromIndex, "which is in burst", fromBurst, ", at offset", fromBurstOffset, "\n")
        tmp <- approx(burstSampleIndex, burst, toIndex)$y
        if (is.na(tmp))
            stop("toIndex", from, " is not in thisFile")
        toBurst <- floor(tmp)
        toBurstOffset <- floor(0.5 + (tmp - toBurst)*samplesPerBurst[toBurst])
        oceDebug(debug, "to is at index", toIndex, "which is in burst", toBurst, ", at offset", toBurstOffset, "\n")
    }

    ## Set up focus region (not needed; just saves some subscripts later)
    focus <- unique(seq(fromBurst, toBurst)) # collapse, e.g. if in same burst
    burstFocus <- burst[focus]
    oceDebug(debug, "burstFocus=", paste(burstFocus, collapse=" "), "\n")
    nburstsFocus <- length(burstFocus)
    burstBufindexFocus <- burstBufindex[focus]
    burstTimeFocus <- burstTime[focus]
    samplesPerBurstFocus <- samplesPerBurst[focus]

    if (debug > 0)
        print(data.frame(burstFocus, burstTimeFocus, burstBufindexFocus, samplesPerBurstFocus))

    ## set up to read everything in every relevant burst (trim later)
    oceDebug(debug, "sum(samplers.burstFocus)", sum(samplesPerBurstFocus), "vs", nbursts * as.numeric(burstTime[2]-burstTime[1])*metadata$samplingRate,"\n")

    ntotal <- sum(samplesPerBurstFocus)
    oceDebug(debug, "ntotal=", ntotal, "\n")
    v <- array(numeric(), dim=c(ntotal, 3))
    time <- array(numeric(), dim=c(ntotal, 1))
    heading <- array(numeric(), dim=c(ntotal, 1))
    pitch <- array(numeric(), dim=c(ntotal, 1))
    roll <- array(numeric(), dim=c(ntotal, 1))
    temperature <- array(numeric(), dim=c(ntotal, 1))
    pressure <- array(numeric(), dim=c(ntotal, 1))
    a <- array(raw(), dim=c(ntotal, 3))
    c <- array(raw(), dim=c(ntotal, 3))
    rowOffset <- 0

    oceDebug(debug, "dataLength=", dataLength, "\n")
    oceDebug(debug, "burstHeaderLength=",burstHeaderLength,"\n")

    oceDebug(debug, "burstBufindexFocus:", paste(burstBufindexFocus, collapse=" "), "\n")

    for (b in 1:nburstsFocus) {
        n <- samplesPerBurstFocus[b]
        oceDebug(debug, "burst", b, "at", format(burstTimeFocus[b]), "data start at byte", burstBufindexFocus[b]+burstHeaderLength, "n=",n,"\n")
        bufSubset <- buf[burstBufindexFocus[b]+burstHeaderLength+0:(-1+dataLength*n)]
        m <- matrix(bufSubset, ncol=dataLength, byrow=TRUE)
        if (n != dim(m)[1])
            stop("something is wrong with the data.  Perhaps the record length is not the assumed value of ", dataLength)
        r <- rowOffset + 1:n
        v[r,1] <- 1e-4 * readBin(t(m[,1:2]), "integer", n=n, size=2, signed=TRUE, endian="little")
        v[r,2] <- 1e-4 * readBin(t(m[,3:4]), "integer", n=n, size=2, signed=TRUE, endian="little")
        v[r,3] <- 1e-4 * readBin(t(m[,5:6]), "integer", n=n, size=2, signed=TRUE, endian="little")
        a[r,1] <- m[,7]
        a[r,2] <- m[,8]
        a[r,3] <- m[,9]
        c[r,1] <- m[,10]
        c[r,2] <- m[,11]
        c[r,3] <- m[,12]
        time[r] <- as.numeric(burstTimeFocus[b]) + seq(0, n-1) / metadata$samplingRate
        ##cat(sprintf("%.2f %.2f %.2f\n", time[r[1]], time[r[2]], time[r[3]]))
        ##cat("time=", format(time[r[1]]), ";", format(burstTimeFocus[b]), "\n")
        ##print(range(time[r]))
        heading[r] <- 0.1 * readBin(as.raw(t(m[,13:14])), "integer", n=n, size=2, signed=TRUE, endian="little")
        pitch[r] <-   0.1 * readBin(as.raw(t(m[,15:16])), "integer", n=n, size=2, signed=TRUE, endian="little")
        roll[r] <-    0.1 * readBin(as.raw(t(m[,17:18])), "integer", n=n, size=2, signed=TRUE, endian="little")
        temperature[r] <- 0.01 * readBin(as.raw(t(m[,19:20])), "integer", n=n, size=2, signed=TRUE, endian="little")

        ## Pressure, using quadratic conversion from counts
        p.count <- readBin(as.raw(t(m[,21:22])), "integer", n=n, size=2, signed=FALSE, endian="little")
        pressure[r] <- metadata$pressureOffset + p.count * (metadata$pressureScale + p.count * metadata$pressureScale2)

        rowOffset <- rowOffset + n
        if (monitor) {
            cat(".")
            if (!(b %% 50))
                cat(b, "\n")
        }
    }
    if (monitor)
        cat("\n")
    rm(buf, bufSubset, m)              # clean up, in case space is tight
    class(time) <- c("POSIXt", "POSIXct")
    attr(time, "tzone") <- attr(burstTimeFocus[1], "tzone")
    oceDebug(debug, "burstTime[1]=", format(burstTimeFocus[1]), "\n   times=", format(time[1:20]),"\n")
    ## Subset data to match the provided 'from', 'to' and 'by'
    if (fromToPOSIX) {
        iii <- from <= time & time <= to
        if (is.character(by)) {
            subsamplingRate <- floor(0.5 + ctimeToSeconds(by) * metadata$samplingRate)
            oceDebug(debug, paste(" by = '", by, "' yields subsamplingRate=", subsamplingRate, "\n"), sep="")
            samples <- 1:length(iii)
            oceDebug(debug, "before interpreting 'by', iii true for", sum(iii), "cases\n")
            iii <- iii & !(samples %% subsamplingRate)
            oceDebug(debug, "after  interpreting 'by', iii true for", sum(iii), "cases\n")
            ##!(1:100)%%metadata$samplingRate
            oceDebug(debug, "'by' is character, so subsampling by", floor(0.5 + ctimeToSeconds(by) * metadata$samplingRate), "\n")
        }
    } else {
        indices <- seq(fromIndex, toIndex) # FIXME: ignoring 'by'
        oceDebug(debug, "indices[1:10]=", paste(indices[1:10], collapse=" "), "\n")
        time <- approx(burstSampleIndex.extended, burstTimeExtended - burstTime[1], indices)$y + burstTime[1]
        if (any(is.na(time)))
            warning("some times are NA; this is an internal coding error")
        focusFrom <- fromBurstOffset
        focusto <- toBurstOffset + sum(samplesPerBurstFocus[-length(samplesPerBurstFocus)])
        oceDebug(debug, "focusFrom=",focusFrom, "focusto=", focusto,"\n")
        iii <- seq(focusFrom, focusto, by=by)
    }
    oceDebug(debug, "iii=", iii[1], iii[2], "...", iii[-1+length(iii)], iii[length(iii)], "\n")
    if (any(iii < 0))
        stop("got negative numbers in iii, which indicates a coding problem; range(iii)=",paste(range(iii), collapse=" to "))
    oceDebug(debug, "dim(v)=", paste(dim(v), collapse=" "),"\n")
    v <- v[iii,] * metadata$velocityScaleFactor
    a <- a[iii,]
    c <- c[iii,]
    time <- time[iii]
    pressure <- pressure[iii]
    temperature <- temperature[iii]
    pitch <- pitch[iii]
    heading <- heading[iii]
    roll <- roll[iii]
    data <- list(time=time,
                 heading=heading,
                 pitch=pitch,
                 roll=roll,
                 temperature=temperature,
                 pressure=pressure,
                 v=v, a=a, c=c)
    if (missing(history))
        history <- paste(deparse(match.call()), sep="", collapse="")
history <- historyItem(history)
res <- list(data=data, metadata=metadata, history=history)
class(res) <- c("sontek", "adv", "oce")
    res
}

read.adv.sontek.text <- function(basefile, from=1, to, by=1, tz=getOption("oceTz"),
                                 coordinateSystem="xyz", transformationMatrix,
                                 latitude=NA, longitude=NA,
                                 debug=getOption("oceDebug"), history)
{
    ## FIXME: It would be better to deal with the binary file, but the format is unclear to me;
    ## FIXME: two files are available to me, and they differ considerably, neither matching the
    ## FIXME: SonTek documentation.
    if (by != 1)
        stop("must have \"by\"=1, in this version of the package")
    suffices <- c("hd1", "ts1")
    itemsPerSample <- 16
    if (missing(basefile))
        stop("need to supply a basefile, e.g. \"A\" to read \"A.hd1\" and \"A.ts1\"")

    hd <- paste(basefile, suffices[1], sep=".")
    ts <- paste(basefile, suffices[2], sep=".")

    ## The hd1 file holds per-burst information
    hdt <-  read.table(hd)
    numberOfBursts <- dim(hdt)[1]
    oceDebug(debug, "numberOfBursts: ", numberOfBursts, "\n")
    t <- ISOdatetime(year=hdt[,2], month=hdt[,3], day=hdt[,4], hour=hdt[,5], min=hdt[,6], sec=hdt[,7], tz=tz)
    if (inherits(from, "POSIXt")) {
        ignore <- t < from
        if (sum(ignore) == 0)
            stop("no data in this time interval, starting at time ", from, "\n")
        fromBurst <- which(ignore == FALSE)[1]
        oceDebug(debug, "\"from\" is burst number", fromBurst, "at", format(t[fromBurst]), "\n")
    } else {
        fromBurst <- from + 1          # 0 means first burst
    }
    if (missing(to)) {
        stop("must supply \"to\"")
    } else {
        if (inherits(from, "POSIXt")) {
            ignore <- t < to
            if (sum(ignore) == 0)
                stop("no data in this time interval, starting at time ", to, "\n")
            toBurst <- which(ignore == FALSE)[1] + 1 # add 1 since we'll chop later
            toBurst <- min(toBurst, length(t))
            oceDebug(debug, "\"to\" is burst number", toBurst, "at", format(t[toBurst]), "\n")
        } else {
            toBurst <- to
        }
    }
    ##voltage <- hdt[,14]
    heading <- hdt[,24]
    pitch <- hdt[,25]
    roll <- hdt[,26]
    spb <- hdt[1,9]                      # FIXME may this change over time?
    sr <- spb / 3600

    tsFile <- file(ts, "rb")
    on.exit(close(tsFile))
    if (!inherits(tsFile, "connection"))
        stop("argument `tsFile' must be a character string or connection")

    ## Examine ".ts1" file to see if we can deal with it.
    seek(tsFile, where=0, origin="end")
    bytesInFile <- seek(tsFile, where=0, origin="start")
    oceDebug(debug, "length of \".", suffices[2], "\" file: ",bytesInFile," bytes\n")
    look <- min(5000, bytesInFile)
    b <- readBin(tsFile, "raw", n=look)
    newlines <- which(b == 0x0a)
    if (0 != diff(range(fivenum(diff(newlines)))))
        stop("need equal line lengths in ", ts)
    ## Line length
    bytesInSample <- diff(newlines)[1]
    oceDebug(debug, "line length in \".", suffices[2], "\" file: ", bytesInSample, " bytes\n")
    ## elements per line
    seek(tsFile, where=newlines[1], origin="start")
    d <- scan(tsFile, what="character", nlines=1, quiet=TRUE)
    oceDebug(debug, "first line in \".", suffices[2], "\" file: ", paste(d, collapse=" "), "\n")
    itemsPerLine <- length(d)
    if (itemsPerSample != length(d))
        stop("file \".", suffices[2], "\" should have ", itemsPerSample, " elemetns per line, but it has ", length(d))
    oceDebug(debug, "elements per line in \".", suffices[2], "\" file: ", length(d), "\n")
    linesInFile <- bytesInFile / bytesInSample
    oceDebug(debug, "lines in \".", suffices[2], "\" file: ", linesInFile, "\n")

    samplesPerBurst <- linesInFile / numberOfBursts
    oceDebug(debug, "samplesPerBurst: ", samplesPerBurst, "\n")

    fromByte <- fromBurst * samplesPerBurst * bytesInSample
    toByte <- toBurst * samplesPerBurst * bytesInSample
    oceDebug(debug, "seek from:", fromByte, "\n", "seek to:", toByte, "\n")
    seek(tsFile, where=fromByte, origin="start")
    ts <- matrix(scan(tsFile, n=itemsPerSample*(toBurst - fromBurst + 1)*samplesPerBurst, quiet=TRUE),
                 ncol=itemsPerSample, byrow=TRUE)
    len <- dim(ts)[1]
    v <- array(numeric(), dim=c(len, 3))
    v[,1] <- ts[,3] / 100
    v[,2] <- ts[,4] / 100
    v[,3] <- ts[,5] / 100
    a <- array(raw(), dim=c(len, 3))
    a[,1] <- as.raw(ts[,6])
    a[,2] <- as.raw(ts[,7])
    a[,3] <- as.raw(ts[,8])
    c <- array(raw(), dim=c(len, 3))
    c[,1] <- as.raw(ts[,9])
    c[,2] <- as.raw(ts[,10])
    c[,3] <- as.raw(ts[,11])
    temperature <- ts[,15]
    pressure <- ts[,16]
    rm(ts)                              # may run tight on space
    tt <- seq(t[fromBurst], t[toBurst], length.out=len)
    ## trim to the requested interval
    ok <- (from - 1/2) <= tt & tt <= (to + 1/2) # give 1/2 second extra
    data <- list(time=tt[ok],
                 heading=approx(t, heading, xout=tt, rule=2)$y[ok],
                 pitch=approx(t, pitch, xout=tt, rule=2)$y[ok],
                 roll=approx(t, roll, xout=tt, rule=2)$y[ok],
                 temperature=temperature,
                 pressure=pressure,
                 v=v[ok,], a=a[ok,], c=c[ok,])
    metadata <- list(manufacturer="sontek",
                     instrumentType="adv", # FIXME or "adr"?
                     latitude=latitude, longitude=longitude,
                     cpuSoftwareVerNum=metadata$cpuSoftwareVerNum,
                     dspSoftwareVerNum=metadata$dspSoftwareVerNum,
                     filename=basefile,
                     transformationMatrix=if(!missing(transformationMatrix)) transformationMatrix else NULL,
                     numberOfSamples=length(data$x),
                     numberOfBeams=3,
                     orientation="upward", # FIXME: guessing on the orientation
                     deltat=as.numeric(difftime(tt[2], tt[1], units="secs")),
                     subsampleStart=data$t[1],
                     oceCoordinate=coordinateSystem,
                     coordinateSystem=coordinateSystem)
    warning("sensor orientation cannot be inferred without a header; \"", metadata$orientation, "\" was assumed.")
    if (missing(history)) history <- paste(deparse(match.call()), sep="", collapse="")
    res <- list(data=data, metadata=metadata, history=historyItem(history))
    class(res) <- c("sontek", "adv", "oce")
    res
}

summary.adv <- function(object, ...)
{
    if (!inherits(object, "adv"))
        stop("method is only for adv objects")
    dataNames <- names(object$data)
    nrow <- length(dataNames) - 1          # the -1 is for 'time'
    nrow <- nrow + length(dataNames)
    threes <- matrix(nrow=nrow, ncol=3)
    ii <- 1
    for (name in dataNames) {
        if (name != "time") {
            threes[ii,] <- threenum(as.numeric(object$data[[name]]))
            ii <- ii + 1
        }
    }
    rownames(threes) <- dataNames
    colnames(threes) <- c("Min.", "Mean", "Max.")
    res <- list(filename=object$metadata$filename,
                numberOfBeams=if (!is.null(object$metadata$numberOfBeams)) object$metadata$numberOfBeams else 3,
                latitude=object$metadata$latitude,
                longitude=object$metadata$longitude,
                orientation=object$metadata$orientation,
                velocityRangeIndex=object$metadata$velocityRangeIndex,
                transformationMatrix=object$metadata$transformationMatrix,
                samplingRate=object$metadata$samplingRate,
                measurementStart=object$metadata$measurementStart,
                measurementEnd=object$metadata$measurementEnd,
                measurementDeltat=object$metadata$measurementDeltat,
                subsampleStart=min(object$data$time, na.rm=TRUE),
                subsampleEnd=max(object$data$time, na.rm=TRUE),
                subsampleDeltat=mean(diff(as.numeric(object$data$time)),na.rm=TRUE),
                instrumentType=object$metadata$instrumentType,
                serialNumber=object$metadata$serialNumber,
                numberOfSamples=length(object$data$time),
                coordinateSystem=object$metadata$coordinateSystem,
                oceCoordinate=object$metadata$oceCoordinate,
                threes=threes,
                history=object$history)
    if (inherits(object, "nortek")) {
        res$softwareVersion <- object$metadata$softwareVersion
        res$internalCodeVersion <- object$metadata$internalCodeVersion
        res$revisionNumber <- object$metadata$hardwareRevision
        res$burstLength <- object$metadata$burstLength
        res$deployName <- object$metadata$deployName
        res$comments <- object$metadata$comments
        res$headFrequency <- object$metadata$frequency
    } else if (inherits(object, "sontek")) {
        res$cpuSoftwareVerNum <- object$metadata$cpuSoftwareVerNum
        res$dspSoftwareVerNum <- object$metadata$dspSoftwareVerNum
        res$samplesPerBurst <- object$metadata$samplesPerBurst
    }
    class(res) <- "summary.adv"
    res
}

print.summary.adv <- function(x, digits=max(5, getOption("digits") - 1), ...)
{
    cat("ADV Summary\n-----------\n\n", ...)
    cat(paste("* Instrument:             ", x$instrumentType, ", serial number ``", x$serialNumber, "``\n",sep=""))
    cat(paste("* Source filename:        ``", x$filename, "``\n", sep=""))
    if ("latitude" %in% names(x)) {
        cat(paste("* Location:              ", if (is.na(x$latitude)) "unknown latitude" else sprintf("%.5f N", x$latitude), ", ",
                  if (is.na(x$longitude)) "unknown longitude" else sprintf("%.5f E", x$longitude), "\n"))
    }
    cat(sprintf("* Measurements:           %s %s to %s %s sampled at %.4g Hz (on average)\n",
                format(x$measurementStart), attr(x$measurementStart, "tzone"),
                format(x$measurementEnd), attr(x$measurementEnd, "tzone"),
                1 / x$measurementDeltat), ...)
    cat(sprintf("* Subsample:              %s %s to %s %s sampled at %.4g Hz (on average)\n",
                format(x$subsampleStart), attr(x$subsampleStart, "tzone"),
                format(x$subsampleEnd),  attr(x$subsampleEnd, "tzone"),
                1 / x$subsampleDeltat), ...)
    ## cat("  Beam angle:           ", x$metadata$beamAngle, "\n")
    cat("* Number of samples:     ", x$numberOfSamples, "\n")
    cat("* Coordinate system:     ", x$coordinateSystem, "[originally],", x$oceCoordinate, "[presently]\n")
    cat("* Orientation:           ", x$orientation, "\n")
    if (x$instrumentType == "vector") {
        cat("\n* Nortek vector specific\n\n")
        cat("  * Internal code version:  ", x$internalCodeVersion, "\n")
        cat("  * Revision number:        ", x$revisionNumber, "\n")
        cat("  * Software version:       ", x$softwareVersion, "\n")
        cat("  * Head frequency:         ", x$headFrequency, "kHz\n")
        ## FIXME: put other info here, e.g. softwareVersion, sampling volume, etc.; the manufacturer file is a good guide
        cat("  * Samples per burst:      ", x$burstLength, "\n") # FIXME: use same names throughout
        cat("  * Deploy name:            ", x$deployName, "\n")
        cat("  * Comments:               ", x$comments, "\n")
    } else if (x$instrumentType == "sontek adr") {
        cat("\n* Sontek adr specific\n\n")
        cat("  * CPU softwareVersion:  ", x$cpuSoftwareVerNum, "\n")
        cat("  * DSP softwareVersion:  ", x$dspSoftwareVerNum, "\n")
        cat("  * Samples per burst:     ", x$samplesPerBurst, "\n")
        cat("  * Velocity range index:  ", x$velocityRangeIndex, "\n")
    }
    if (!is.null(x$transformationMatrix)) {
        cat("\n* Transformation matrix\n  ::\n\n")
        cat("  ", format(x$transformationMatrix[1,], width=digits+4, digits=digits, justify="right"), "\n")
        cat("  ", format(x$transformationMatrix[2,], width=digits+4, digits=digits, justify="right"), "\n")
        cat("  ", format(x$transformationMatrix[3,], width=digits+4, digits=digits, justify="right"), "\n")
        if (x$numberOfBeams > 3)
            cat("  ", format(x$transformationMatrix[4,], width=digits+4, digits=digits, justify="right"), "\n")
    }
    cat("\n",...)
    cat("* Statistics of subsample\n  ::\n\n", ...)
    cat(showThrees(x, indent='     '), ...)
    cat("\n")
    print(summary(x$history))
    invisible(x)
}

plot.adv <- function(x, which=c(1:3,14,15),
                     col,
                     titles,
                     type="l",
                     lwd=par('lwd'),
                     adorn=NULL,
                     drawTimeRange=getOption("oceDrawTimeRange"),
                     drawZeroLine=FALSE,
                     useSmoothScatter,
                     mgp=getOption("oceMgp"),
                     mar=c(mgp[1]+1.5,mgp[1]+1.5,1.5,1.5),
                     marginsAsImage=FALSE,
                     cex=par("cex"), cex.axis=par("cex.axis"), cex.main=par("cex.main"),
                     xlim, ylim,
                     brushCorrelation, colBrush="red",
                     main="",
                     debug=getOption("oceDebug"),
                     ...)
{
    debug <- min(4, max(0, round(debug)))
    oceDebug(debug, "\bplot.adv(x, which=c(", paste(which,collapse=","),"), type=\"", type, "\", ...) {\n", sep="")
    have.brushCorrelation <- !missing(brushCorrelation)
    oceDebug(debug, "brushCorrelation", if (have.brushCorrelation) brushCorrelation else "not given", "\n")
    oceDebug(debug, "cex=",cex," cex.axis=", cex.axis, " cex.main=", cex.main, "\n")
    oceDebug(debug, "mar=c(",paste(mar, collapse=","), ")\n")
    if (!inherits(x, "adv"))
        stop("method is only for adv objects")
    opar <- par(no.readonly = TRUE)
    dots <- names(list(...))
    ##if (!all(which %in% c(1:3,5:7,9:11,14:21,23)))
    ##   stop("\"which\" must be in the range c(1:3,5:7,9:11,14:21,23) but it is ", which)
    nw <- length(which)
    if (nw == 1) {
        pm <- pmatch(which, c("velocity","amplitude","quality","hydrography", "angles"))
        if (!is.na(pm)) {
            nbeams <- 3
            if (pm == 1)
                which <- 0 + seq(1, nbeams)
            else if (pm == 2)
                which <- 4 + seq(1, nbeams)
            else if (pm == 3)
                which <- 8 + seq(1, nbeams)
            else if (pm == 4)
                which <- 14:15
            else if (pm == 5)
                which <- 16:18
            nw <- length(which)
        }
    }
    col.per.point <- FALSE
    if (missing(col)) {
        col <- rep("black", length.out=nw)
    } else {
        col.per.point <- length(col) == length(x$data$time) # FIXME slow timescale here?
        if (!col.per.point)
            col <- rep(col, length.out=nw)
    }
    if (!missing(titles) && length(titles) != nw)
        stop("length of 'titles' must equal length of 'which'")
    if (nw > 1)
        on.exit(par(opar))
    par(mgp=mgp, mar=mar)
    dots <- list(...)

    ## user may specify a matrix for xlim and ylim
    gave.ylim <- !missing(ylim)
    if (gave.ylim) {
        if (is.matrix(ylim)) {
            if (dim(ylim)[2] != nw) {
                ylim2 <- matrix(ylim, ncol=2, nrow=nw) # FIXME: is this what I want?
            }
        } else {
            ylim2 <- matrix(ylim, ncol=2, nrow=nw) # FIXME: is this what I want?
        }
        class(ylim2) <- class(ylim)
        ylim <- ylim2
    }
    gave.xlim <- !missing(xlim)
    if (gave.xlim) {
        if (is.matrix(xlim)) {
            if (dim(xlim)[2] != nw) {
                xlim2 <- matrix(xlim, ncol=2, nrow=nw) # FIXME: is this what I want?
            }
        } else {
            if (length(xlim) != 2)
                stop("xlim must be a vector of length 2, or a 2-column matrix")
            xlim2 <- matrix(xlim[1:2], ncol=2, nrow=nw, byrow=TRUE)
        }
        xlim <- xlim2
    }
    adorn.length <- length(adorn)
    if (adorn.length == 1) {
        adorn <- rep(adorn, nw)
        adorn.length <- nw
    }
    oceDebug(debug, "before layout, cex=", par('cex'), "\n")
    if (nw > 1) {
        if (marginsAsImage) {
            w <- 1.5
            lay <- layout(matrix(1:(2*nw), nrow=nw, byrow=TRUE), widths=rep(c(1, lcm(w)), nw))
        } else {
            lay <- layout(cbind(1:nw))
        }
    }
    ## Translate word-style (FIXME: ugly coding)
    oceDebug(debug, "before nickname-substitution, which=c(", paste(which, collapse=","), ")\n")
    which2 <- vector("numeric", nw)
    if (nw == 1) {
        if (which == "velocity")
            which <- 1:3
        else if (which == "amplitude")
            which <- 5:7
        else if (which == "backscatter")
            which <- 9:11
        else if (which == "hydrography")
            which <- 14:15
        else if (which == "angles")
            which <- 16:18
        nw <- length(which)
    }
    for (w in 1:nw) {
        ww <- which[w]
        if (is.numeric(ww)) {
            which2[w] <- ww
        } else {
            if (     ww == "u1") which2[w] <- 1
            else if (ww == "u2") which2[w] <- 2
            else if (ww == "u3") which2[w] <- 3
            ## 4 not allowed since ADV is 3-beam
            else if (ww == "a1") which2[w] <- 5
            else if (ww == "a2") which2[w] <- 6
            else if (ww == "a3") which2[w] <- 7
            ## 4 not allowed since ADV is 3-beam
            else if (ww == "q1") which2[w] <- 9
            else if (ww == "q2") which2[w] <- 10
            else if (ww == "q3") which2[w] <- 11
            ## 4 not allowed since ADV is 3-beam
            ## 13 not allowed since ADV do not measure salinity
            else if (ww == "temperature") which2[w] <- 14
            else if (ww == "pressure") which2[w] <- 15
            else if (ww == "heading") which2[w] <- 16
            else if (ww == "pitch") which2[w] <- 17
            else if (ww == "roll") which2[w] <- 18
            ## 19 beam-1 correlation-amplitude diagnostic plot
            ## 20 beam-2 correlation-amplitude diagnostic plot
            ## 21 beam-3 correlation-amplitude diagnostic plot
            ## 22 not allowed, since ADVs have only 3 beams
            else if (ww == "progressive vector") which2[w] <- 23
            else if (ww == "uv") which2[w] <- 28
            else if (ww == "uv+ellipse") which2[w] <- 29
            else if (ww == "uv+ellipse+arrow") which2[w] <- 30
            else stop("unknown 'which':", ww)
        }
    }
    which <- which2
    oceDebug(debug, "after nickname-substitution, which=c(", paste(which, collapse=","), ")\n")
    oceDebug(debug, "after layout, cex=", par('cex'), "\n")
    ## FIXME below here, was using tsSlow
    tlim <- range(x$data$time, na.rm=TRUE)
    for (w in 1:nw) {
        if (w > 1)
            main <- ""
        oceDebug(debug, "plotting which[", w, "]=", which[w], "\n")
        par(mgp=mgp, mar=mar)
        if (which[w] %in% 1:3) {        # u1, u2, u3
            y <- as.numeric(x$data$v[,which[w]])
            if (have.brushCorrelation && type == "p") {
                good <- as.numeric(x$data$c[,which[w]]) >= brushCorrelation
                oce.plot.ts(x$data$time[good], y[good], ylab=beamName(x, which[w]),
                            drawTimeRange=drawTimeRange,
                            adorn=adorn[w],
                            xlim=if (gave.xlim) xlim[w,] else tlim,
                            ylim=if (gave.ylim) ylim[w,] else range(y, na.rm=TRUE),
                            type=type,
                            cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                            mgp=mgp,
                            mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                            lwd=lwd[w], col=col[w],
                            main=main,
                            debug=debug-1,
                            ...)
                points(x$data$time[!good], x$data$v[!good,which[w]], col=colBrush)
            } else {
                oce.plot.ts(x$data$time, y, ylab=beamName(x, which[w]),
                            drawTimeRange=drawTimeRange,
                            adorn=adorn[w],
                            xlim=if (gave.xlim) xlim[w,] else tlim,
                            ylim=if (gave.ylim) ylim[w,] else range(y, na.rm=TRUE),
                            type=type,
                            cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                            mgp=mgp,
                            mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                            lwd=lwd[w], col=col[w],
                            main=main,
                            debug=debug-1,
                            ...)
            }
            if (drawZeroLine)
                abline(h=0)
            rm(y)                       # space may be tight
        } else if (which[w] %in% 5:7) { # a1, a2, a3
            ## FIXME/DRY: alter a1,a2,a3 if alter q1,q2,q3, since both almost the same
            oceDebug(debug, "plotting a1, a2, or a3 since which[w] == ", which[w], "\n")
            y <- as.numeric(x$data$a[,which[w]-4])
            oceDebug(debug, "range(y):", paste(range(y, na.rm=TRUE), sep="-"), "\n")
            if (have.brushCorrelation && type == "p") {
                good <- as.numeric(x$data$c[,which[w]-4]) >= brushCorrelation
                oce.plot.ts(x$data$time[good], y[good],
                            ylab=c(expression(a[1]),expression(a[2]),expression(a[3]),expression(a[4]))[which[w]-4],
                            drawTimeRange=drawTimeRange,
                            adorn=adorn[w],
                            xlim=if (gave.xlim) xlim[w,] else tlim,
                            ylim=if (gave.ylim) ylim[w,] else range(y, na.rm=TRUE),
                            type=type,
                            cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                            mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                            lwd=lwd[w], col=col[w],
                            main=main,
                            debug=debug-1,
                            ...)
                points(x$data$time[!good], y[!good], col=colBrush)
            } else {
                oce.plot.ts(x$data$time, y,
                            ylab=c(expression(a[1]),expression(a[2]),expression(a[3]),expression(a[4]))[which[w]-4],
                            drawTimeRange=drawTimeRange,
                            adorn=adorn[w],
                            xlim=if (gave.xlim) xlim[w,] else tlim,
                            ylim=if (gave.ylim) ylim[w,] else range(y, na.rm=TRUE),
                            type=type,
                            cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                            mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                            lwd=lwd[w], col=col[w],
                            main=main,
                            debug=debug-1,
                            ...)
            }
            rm(y)                       # space may be tight
        } else if (which[w] %in% 9:11) { # q1, q2, q3 (named c1, c2, and c3 in the object)
            y <- as.numeric(x$data$c[,which[w]-8])
            if (have.brushCorrelation && type == "p") {
                good <- as.numeric(x$data$c[,which[w]-8]) >= brushCorrelation
                oce.plot.ts(x$data$time[good], y[good],
                            ylab=c(expression(q[1]),expression(q[2]),expression(q[3]),expression(q[4]))[which[w]-8],
                            drawTimeRange=drawTimeRange,
                            adorn=adorn[w],
                            xlim=if (gave.xlim) xlim[w,] else tlim,
                            ylim=if (gave.ylim) ylim[w,] else range(y, na.rm=TRUE),
                            type=type,
                            cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                            mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                            lwd=lwd[w], col=col[w],
                            main=main,
                            debug=debug-1,
                            ...)
                points(x$data$time[!good], y[!good], col=colBrush)
            } else {
                oce.plot.ts(x$data$time, y,
                            ylab=c(expression(q[1]),expression(q[2]),expression(q[3]),expression(q[4]))[which[w]-8],
                            drawTimeRange=drawTimeRange,
                            adorn=adorn[w],
                            xlim=if (gave.xlim) xlim[w,] else tlim,
                            ylim=if (gave.ylim) ylim[w,] else range(y, na.rm=TRUE),
                            type=type,
                            cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                            mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                            lwd=lwd[w], col=col[w],
                            debug=debug-1,
                            ...)
            }
            rm(y)                       # space may be tight
        } else if (which[w] == 14 || which[w] == "temperature") {
            oce.plot.ts(x$data$time, x$data$temperature, ylab=resizableLabel("T", "y"),
                        drawTimeRange=drawTimeRange,
                        adorn=adorn[w],
                        xlim=if (gave.xlim) xlim[w,] else tlim,
                        ylim=if (gave.ylim) ylim[w,] else range(x$data$temperature, na.rm=TRUE),
                        type=type,
                        cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                        mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                        lwd=lwd[w], col=if(col.per.point) col else col[w],
                        main=main,
                        debug=debug-1,
                        ...)
        } else if (which[w] == 15 || which[w] == "pressure") {
            oce.plot.ts(x$data$time, x$data$pressure, ylab=resizableLabel("p", "y"),
                        drawTimeRange=drawTimeRange,
                        adorn=adorn[w],
                        xlim=if (gave.xlim) xlim[w,] else tlim,
                        ylim=if (gave.ylim) ylim[w,] else range(x$data$pressure, na.rm=TRUE),
                        type=type,
                        cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                        mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                        lwd=lwd[w], col=if(col.per.point) col else col[w],
                        main=main,
                        debug=debug-1,
                        ...)
        } else if (which[w] == 16 || which[w] == "heading") {
            oce.plot.ts(x$data$time, x$data$heading, ylab="heading",
                        drawTimeRange=drawTimeRange,
                        adorn=adorn[w],
                        xlim=if (gave.xlim) xlim[w,] else tlim,
                        ylim=if (gave.ylim) ylim[w,] else range(x$data$heading, na.rm=TRUE),
                        type=type,
                        cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                        mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                        lwd=lwd[w], col=if(col.per.point) col else col[w],
                        main=main,
                        debug=debug-1,
                            ...)
        } else if (which[w] == 17 || which[w] == "pitch") {    # pitch
            oce.plot.ts(x$data$time, x$data$pitch, ylab="pitch",
                        drawTimeRange=drawTimeRange,
                        adorn=adorn[w],
                        xlim=if (gave.xlim) xlim[w,] else tlim,
                        ylim=if (gave.ylim) ylim[w,] else range(x$data$pitch, na.rm=TRUE),
                        type=type,
                        cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                        mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                        lwd=lwd[w], col=if(col.per.point) col else col[w],
                        main=main,
                        debug=debug-1,
                        ...)
        } else if (which[w] == 18 || which[w] == "roll") {
            oce.plot.ts(x$data$time, x$data$roll, ylab="roll",
                        drawTimeRange=drawTimeRange,
                        adorn=adorn[w],
                        xlim=if (gave.xlim) xlim[w,] else tlim,
                        ylim=if (gave.ylim) ylim[w,] else range(x$data$roll, na.rm=TRUE),
                        type=type,
                        cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                        mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                        lwd=lwd[w], col=col[w],
                        main=main,
                        debug=debug-1,
                        ...)
            ## FIXME: should plot.adv() be passing mar, cex, etc to smoothScatter?
        } else if (which[w] == 19) {    # beam 1 correlation-amplitude diagnostic plot
            a <- as.numeric(x$data$a[,1])
            c <- as.numeric(x$data$c[,1])
            n <- length(a)
            if (n < 2000 || (!missing(useSmoothScatter) && !useSmoothScatter)) {
                plot(a, c, xlab="Amplitude", ylab="Correlation",
                     xlim=if (gave.xlim) xlim[w,] else range(a),
                     ylim=if (gave.ylim) ylim[w,] else range(c),
                     main=main)
            } else {
                smoothScatter(a, c, nbin=64, xlab="Amplitude", ylab="Correlation",
                     xlim=if (gave.xlim) xlim[w,] else range(a),
                     ylim=if (gave.ylim) ylim[w,] else range(c),
                     main=main)
            }
            mtext("beam 1")
        } else if (which[w] == 20) {    # beam 2 correlation-amplitude diagnostic plot
            a <- as.numeric(x$data$a[,2])
            c <- as.numeric(x$data$c[,2])
            n <- length(a)
            if (n < 2000 || (!missing(useSmoothScatter) && !useSmoothScatter)) {
                plot(a, c, xlab="Amplitude", ylab="Correlation",
                     xlim=if (gave.xlim) xlim[w,] else range(a),
                     ylim=if (gave.ylim) ylim[w,] else range(c),
                     main=main)
            } else {
                smoothScatter(a, c, nbin=64, xlab="Amplitude", ylab="Correlation",
                              xlim=if (gave.xlim) xlim[w,] else range(a),
                              ylim=if (gave.ylim) ylim[w,] else range(c),
                              main=main)
            }
            mtext("beam 2")
        } else if (which[w] == 21) {    # beam 3 correlation-amplitude diagnostic plot
            a <- as.numeric(x$data$a[,3])
            c <- as.numeric(x$data$c[,3])
            n <- length(a)
            if (n < 2000 || (!missing(useSmoothScatter) && !useSmoothScatter)) {
                plot(a, c, xlab="Amplitude", ylab="Correlation",
                     xlim=if (gave.xlim) xlim[w,] else range(a),
                     ylim=if (gave.ylim) ylim[w,] else range(c),
                     main=main)
            } else {
                smoothScatter(a, c, nbin=64, xlab="Amplitude", ylab="Correlation",
                              xlim=if (gave.xlim) xlim[w,] else range(a),
                              ylim=if (gave.ylim) ylim[w,] else range(c),
                              main=main)
            }
            mtext("beam 3")
        } else if (which[w] == 23 || which[w] == "progressive vector") {    # progressive vector
            par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
            dt <- diff(as.numeric(x$data$time))
            dt <- c(dt[1], dt)    # make right length by copying first
            dt <- mean(dt, na.rm=TRUE)
            m.per.km <- 1000
            u <- x$data$v[,1]
            v <- x$data$v[,2]
            u[is.na(u)] <- 0        # zero out missing
            v[is.na(v)] <- 0
            x.dist <- cumsum(u) * dt / m.per.km
            y.dist <- cumsum(v) * dt / m.per.km
            plot(x.dist, y.dist, xlab="km", ylab="km", type=type,
                 cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                 asp=1, lwd=lwd[w], col=col[w], ...)
            if (main[w] != "")
                mtext(main[w], adj=1)
        } else if (which[w] >= 28) {
            oceDebug(debug, "doing horizontal-velocity diagram\n")
            par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
            n <- length(x$data$time)
            if (n < 2000 || (!missing(useSmoothScatter) && !useSmoothScatter)) {
                plot(x$data$v[,1], x$data$v[,2], xlab="u [m/s]", ylab="v [m/s]", type=type,
                     cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                     asp=1, xlim=xlim, ylim=ylim, lwd=lwd[w], col=col[w], main=main, ...)
            } else {
                smoothScatter(x$data$v[,1], x$data$v[,2], xlab="u [m/s]", ylab="v [m/s]",
                              cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                              asp=1, xlim=xlim, ylim=ylim, ...)
            }
            if (which[w] >= 29) {
                ok <- !is.na(x$data$v[,1]) & !is.na(x$data$v[,2])
                e <- eigen(cov(data.frame(u=x$data$v[ok,1], v=x$data$v[ok,2])))
                major <- sqrt(e$values[1])
                minor <- sqrt(e$values[2])
                theta <- seq(0, 2*pi, length.out=360/5)
                xx <- major * cos(theta)
                yy <- minor * sin(theta)
                theta0 <- atan2(e$vectors[2,1], e$vectors[1,1])
                rotate <- matrix(c(cos(theta0), -sin(theta0), sin(theta0), cos(theta0)), nrow=2, byrow=TRUE)
                xxyy <- rotate %*% rbind(xx, yy)
                lines(xxyy[1,], xxyy[2,], lwd=5, col="yellow")
                lines(xxyy[1,], xxyy[2,], lwd=2, col="darkblue")
                if (which[w] >= 30) {
                    umean <- mean(x$data$v[,1], na.rm=TRUE)
                    vmean <- mean(x$data$v[,2], na.rm=TRUE)
                    arrows(0, 0, umean, vmean, lwd=5, length=1/10, col="yellow")
                    arrows(0, 0, umean, vmean, lwd=2, length=1/10, col=col)
                }
                if (main[w] != "")
                    mtext(main[w], adj=1)
            }
        } else {
            stop("unknown value of \"which\":", which[w])
        }
        drawTimeRange <- FALSE
        if (marginsAsImage)  {
            ## blank plot, to get axis length same as for images
            omar <- par("mar")
            par(mar=c(mar[1], 1/4, mgp[2]+1/2, mgp[2]+1))
            plot(1:2, 1:2, type='n', axes=FALSE, xlab="", ylab="")
            par(mar=omar)
        }
    }
    oceDebug(debug, "\b\b} # plot.adv()\n")
    invisible()
}

toEnuAdv <- function(x, declination=0, debug=getOption("oceDebug"))
{
    oceDebug(debug, "\b\badv.2enu() {\n")
    coord <- x$metadata$oceCoordinate
    if (coord == "beam") {
        x <- xyzToEnuAdv(beamToXyzAdv(x, debug=debug-1), declination=declination, debug=debug-1)
    } else if (coord == "xyz") {
        x <- xyzToEnuAdv(x, declination=declination, debug=debug-1)
    } else if (coord == "enu") {
        ;
    } else {
        warning("adv.2enu cannot convert from coordinate system ", coord, " to ENU, so returning argument as-is")
    }
    oceDebug(debug, "\b\b} # adv.2enu()\n")
    x
}

beamToXyzAdv <- function(x, debug=getOption("oceDebug"))
{
    oceDebug(debug, "\b\bbeamToXyzAdv() {\n")
    if (!inherits(x, "adv"))
        stop("method is only for objects of class \"adv\"")
    if (x$metadata$oceCoordinate != "beam")
        stop("input must be in beam coordinates, but it is in ", x$metadata$oceCoordinate, " coordinates")
    if (is.null(x$metadata$transformationMatrix)) {
        cat("How to add a transformation matrix to a velocimeter record named 'x':
            x$metadata$transformationMatrix <- rbind(c(11100, -5771,  -5321),
                                                     c(  291,  9716, -10002),
                                                     c( 1409,  1409,   1409)) / 4096")
        stop("cannot convert coordinates because metadata$transformationMatrix is NULL (see above).")
    }
    tm <- x$metadata$transformationMatrix
    oceDebug(debug, "Transformation matrix:\n")
    oceDebug(debug, sprintf("%.10f %.10f %.10f\n", tm[1,1], tm[1,2], tm[1,3]))
    oceDebug(debug, sprintf("%.10f %.10f %.10f\n", tm[2,1], tm[2,2], tm[2,3]))
    oceDebug(debug, sprintf("%.10f %.10f %.10f\n", tm[3,1], tm[3,2], tm[3,3]))
    ## Not using the matrix method because it might consume more memory, and
    ## measures no faster xyz <- tm %*% rbind(x$data$v[,1], x$data$v[,2],
    ## x$data$v[,3])
    u <- tm[1,1] * x$data$v[,1] + tm[1,2] * x$data$v[,2] + tm[1,3] * x$data$v[,3]
    v <- tm[2,1] * x$data$v[,1] + tm[2,2] * x$data$v[,2] + tm[2,3] * x$data$v[,3]
    w <- tm[3,1] * x$data$v[,1] + tm[3,2] * x$data$v[,2] + tm[3,3] * x$data$v[,3]
    x$data$v[,1] <- u
    x$data$v[,2] <- v
    x$data$v[,3] <- w
    x$metadata$oceCoordinate <- "xyz"
    x$history <- historyAdd(x$history, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "\b\b} # beamToXyzAdv()\n")
    x
}


xyzToEnuAdv <- function(x, declination=0,
                        cabled=FALSE, horizontalCase, sensorOrientation,
                        debug=getOption("oceDebug"))
{
    oceDebug(debug, "\b\bxyzToEnuAdv(x, declination=", declination,
              ",cabled=",cabled,
              ",horizontalCase=",if (missing(horizontalCase)) "(not provided)" else horizontalCase,
              ",sensorOrientation=",if (missing(sensorOrientation)) "(not provided)" else sensorOrientation,
              ",debug) {\n")
    if (!inherits(x, "adv"))
        stop("method is only for objects of class \"adv\"")
    if (x$metadata$oceCoordinate != "xyz")
        stop("input must be in xyz coordinates, but it is in ", x$metadata$oceCoordinate, " coordinates")
    haveSteadyAngles <- length(x$data$heading) == 1 && length(x$data$pitch) == 1 && length(x$data$roll) == 1
    oceDebug(debug, "haveSteadyAngles=",haveSteadyAngles,"\n")
    # FIXME: haveTsSlow necessary here
    oceDebug(debug, "adv data does not have data ts slow; time-series data are data\n")
    heading <- x$data$heading
    pitch <- x$data$pitch
    roll <- x$data$roll
    ## Adjust various things, so that the xyz-to-enu formulae (based on RDI) will work
    ##
    ## The various cases are defined by help(xyzToEnuAdv).
    if (missing(sensorOrientation))
        sensorOrientation  <- x$metadata$orientation
    if (1 == length(agrep("nortek", x$metadata$manufacturer))) {
        if (!cabled) {
            if (sensorOrientation == "upward") {
                oceDebug(debug, "Case 1: Nortek vector velocimeter with upward-pointing sensor attached directly to pressure case.\n")
                oceDebug(debug, "        Using heading=heading=90, pitch=roll, roll=-pitch, S=X, F=-Y, and M=-Z.\n")
                heading <- heading - 90
                tmp <- pitch
                pitch <- roll
                roll <- -tmp
                starboard <- x$data$v[,1]
                forward <- -x$data$v[,2]
                mast <- -x$data$v[,3]
            } else if (sensorOrientation == "downward") {
                oceDebug(debug, "Case 2: Nortek vector velocimeter with downward-pointing sensor attached directly to pressure case.\n")
                oceDebug(debug, "        Using heading=heading=90, pitch=roll, roll=-pitch, S=X, F=Y, and M=Z.\n")
                heading <- heading - 90
                tmp <- pitch
                pitch <- roll
                roll <- -tmp
                starboard <- x$data$v[,1]
                forward <- x$data$v[,2]
                mast <- x$data$v[,3]
            } else {
                stop("need sensor orientation to be 'upward' or 'downward', not '", sensorOrientation,"'")
            }
        } else {
            ## vector cabelled: cases 3 to 6
            if (missing(horizontalCase))
                stop("must give horizontalCase for cabled Nortek Vector (cases 3 to 6)")
            if (!is.logical(horizontalCase))
                stop("must give horizontalCase as TRUE or FALSE")
            if (horizontalCase) {
                if (sensorOrientation == "upward") {
                    oceDebug(debug, "Case 3: Nortek vector velocimeter with upward-pointing sensor, cabled to a horizontal pressure case.\n")
                    oceDebug(debug, "        Using heading=heading=90, pitch=roll, roll=-pitch, S=X, F=Y, and M=Z.\n")
                    heading <- heading - 90
                    tmp <- pitch
                    pitch <- roll
                    roll <- -tmp
                    starboard <- x$data$v[,1]
                    forward <- x$data$v[,2]
                    mast <- x$data$v[,3]
                } else if (sensorOrientation == "downward") {
                    oceDebug(debug, "Case 4: Nortek vector velocimeter with downward-pointing sensor, cabled to a horizontal pressure case.\n")
                    oceDebug(debug, "        Using heading=heading=90, pitch=roll, roll=pitch, S=X, F=-Y, and M=-Z.\n")
                    heading <- heading - 90
                    tmp <- pitch
                    pitch <- roll
                    roll <- tmp
                    starboard <- x$data$v[,1]
                    forward <- x$data$v[,2]
                    mast <- x$data$v[,3]
                } else {
                    stop("need sensor orientation to be 'upward' or 'downward', not '", sensorOrientation,"'")
                }
            } else {
                stop("cannot handle cases 5 and 6 (vector velocimeter cabled to a vertical case)")
            }
        }
    } else if (1 == length(agrep("sontek", x$metadata$manufacturer))) {
        if (cabled)
            stop("cannot handle the case of a cabled Sontek unit (does it even exist?)")
        if (sensorOrientation == "upward") {
            oceDebug(debug, "Case 7: Sontek ADV velocimeter with upward-pointing sensor.\n")
            oceDebug(debug, "        Using heading=heading=90, pitch=roll, roll=-pitch, S=X, F=-Y, and M=-Z.\n")
            heading <- heading - 90
            tmp <- pitch
            pitch <- roll
            roll <- -tmp
            starboard <- x$data$v[,1]
            forward <- -x$data$v[,2]
            mast <- -x$data$v[,3]
        } else if (sensorOrientation == "downward") {
            oceDebug(debug, "Case 8: Sontek ADV velocimeter with downward-pointing sensor.\n")
            oceDebug(debug, "        Using heading=heading=90, pitch=roll, roll=-pitch, S=X, F=Y, and M=Z.\n")
            heading <- heading - 90
            tmp <- pitch
            pitch <- roll
            roll <- -tmp
            starboard <- x$data$v[,1]
            forward <- x$data$v[,2]
            mast <- x$data$v[,3]
        } else {
            stop("need metadata$orientation='upward' or 'downward', not '",x$metadata$orientation,"'")
        }
    } else {
        stop("unknown type of instrument; x$metadata$manufacturer must contain either \"sontek\" or \"nortek\"")
    }
    np <- dim(x$data$v)[1]
    enu <- .C("sfm_enu",
              as.integer(length(heading)), # need not equal np
              as.double(heading + declination),
              as.double(pitch),
              as.double(roll),
              as.integer(np),
              as.double(starboard),
              as.double(forward),
              as.double(mast),
              east = double(np),
              north = double(np),
              up = double(np),
              NAOK=TRUE,
              PACKAGE="oce")
    x$data$v[,1] <- enu$east
    x$data$v[,2] <- enu$north
    x$data$v[,3] <- enu$up
    x$metadata$oceCoordinate <- "enu"
    x$history <- historyAdd(x$history,
                            paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "\b\b} # xyzToEnuAdv()\n")
    x
}

enuToOtherAdv <- function(x, heading=0, pitch=0, roll=0)
{
    oceDebug(debug, "\b\benuToOtherAdv() {\n")
    if (!inherits(x, "adv"))
        stop("method is only for objects of class \"adv\"")
    if (x$metadata$oceCoordinate != "enu")
        stop("input must be in \"enu\" coordinates, but it is in ", x$metadata$oceCoordinate, " coordinates")
    np <- dim(x$data$v)[1]
    other <- .C("sfm_enu",
              as.integer(length(heading)), # need not equal np
              as.double(heading),
              as.double(pitch),
              as.double(roll),
              as.integer(np),
              as.double(x$data$v[,1]),
              as.double(x$data$v[,2]),
              as.double(x$data$v[,3]),
              v1new = double(np),
              v2new = double(np),
              v3new = double(np),
              NAOK=TRUE,
              PACKAGE="oce")
    x$data$v[,1] <- other$v1new
    x$data$v[,2] <- other$v2new
    x$data$v[,3] <- other$v3new
    x$metadata$oceCoordinate <- "other"
    x$history <- historyAdd(x$history,
                            paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "\b\b} # enuToOtherAdv()\n")
    x
}
