read.adv.nortek <- function(file, from=1, to, by=1, tz=getOption("oceTz"),
                            header=TRUE,
                            longitude=NA, latitude=NA,
                            type=c("vector", "aquadopp"),
                            haveAnalog1=FALSE,
                            haveAnalog2=FALSE,
                            debug=getOption("oceDebug"), monitor=FALSE, 
                            processingLog)
{
    ## abbreviations:
    ##   SIG=System Integrator Guide
    ##   vvd=vector velocity data [p35 SIG], containing the data: pressure, vel, amp, corr (plus sensemble counter etc)
    ##   vsd=velocity system data [p36 SIG], containing times, temperatures, angles, etc
    ## NOTE: we interpolate from vsd to vvd, to get the final data$time, etc.

    type <- match.arg(type)
    oceDebug(debug, "read.adv.nortek(file=\"", file, "\", from=", format(from), ", to=", format(to), ", by=", by, ", tz=\"", tz, "\", header=", header, ", longitude=", longitude, ", latitude=", latitude, ", type=\"", type, "\", debug=", debug, ", monitor=", monitor, ", processingLog=(not shown)) {\n", sep="", unindent=1)
    if (is.numeric(by) && by < 1)
        stop("cannot handle negative 'by' values")
    if (by != 1)
        warning("'by' argument only applies to fast-scale items such as velocity; temperature, etc. are not affected")
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
    if (!header)
        stop("header must be TRUE")
    oceDebug(debug, "  read.adv.nortek() about to read header\n")
    oceDebug(debug, "  read.adv.nortek() finished reading header\n")
                                        # find file length
    seek(file, 0, "end")
    fileSize <- seek(file, 0, "start")
    oceDebug(debug, "  fileSize=", fileSize, "\n")
    buf <- readBin(file, "raw", fileSize)
    header <- decodeHeaderNortek(buf, type="vector", debug=debug-1)
    if (debug > 1) {                    # Note: need high debugging to get this
        cat("\nheader is as follows:\n")
        str(header)
    }
    res <- new("adv")
    res@metadata$manufacturer <- "nortek"
    res@metadata$instrumentType <- "vector"
    res@metadata$filename <- filename
    res@metadata$longitude <- longitude
    res@metadata$latitude <- latitude
    res@metadata$numberOfSamples <- NA # filled in later
    res@metadata$numberOfBeams <- header$head$numberOfBeams # FIXME: check that this is correct
    res@metadata$measurementStart <- NA # FIXME
    res@metadata$measurementEnd <- NA   # FIXME
    res@metadata$samplingRate <- 512/header$user$averagingInterval # FIXME: why 512?
    res@metadata$serialNumber <- header$hardware$serialNumber
    res@metadata$internalCodeVersion <- header$hardware$picVersion
    res@metadata$softwareVersion <- header$user$swVersion
    res@metadata$hardwareRevision <- header$hardware$hwRevision
    res@metadata$recSize <- header$hardware$recSize
    res@metadata$velocityRange <- header$hardware$velocityRange
    res@metadata$firmwareVersion <- header$hardware$fwVersion
    res@metadata$hardwareConfiguration <- header$hardware$config
    res@metadata$configPressureSensor <- header$head$configPressureSensor
    res@metadata$configMagnetometerSensor <- header$head$configMagnetometerSensor
    res@metadata$configTiltSensor <- header$head$configTiltSensor
    res@metadata$beamAngle <- 25     # FIXME: should read from file
    res@metadata$tiltSensorOrientation <- header$head$tiltSensorOrientation
    res@metadata$frequency <- header$head$frequency
    res@metadata$headSerialNumber <- header$head$headSerialNumber
    res@metadata$bin1Distance <- header$user$blankingDistance # FIXME: is this right?
    res@metadata$blankingDistance <- header$user$blankingDistance
    res@metadata$measurementInterval <- header$user$measurementInterval
    res@metadata$transformationMatrix <- header$head$transformationMatrix
    res@metadata$deploymentName <- header$user$deploymentName
    res@metadata$cellSize <- header$user$cellSize
    res@metadata$velocityScale <- header$user$velocityScale
    res@metadata$originalCoordinate <- header$user$originalCoordinate
    res@metadata$oceCoordinate <- header$user$originalCoordinate
    res@metadata$oceBeamUnspreaded <- FALSE
    res@metadata$comments <- header$user$comments
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    hitem <- processingLogItem(processingLog)
    ## Find the focus time by bisection, based on "sd" (system data, containing a time).

    vsdStart <- NULL # prevent scope warning from rstudio; defined later anyway
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
    ## NOTE: system.time() indicates 0.2s to scan a 100Meg file [macpro desktop, circa 2009]

    ## "vvd" stands for "Vector Velocity Data" [bottom of p35 of SIG]
    vvdStart <- .Call("locate_byte_sequences", buf, c(0xa5, 0x10), 24, c(0xb5, 0x8c), 0)
    ## "vsd" stands for "Vector System Data" [p36 of SIG]
    vsdStart <- .Call("locate_byte_sequences", buf, c(0xa5, 0x11), 28, c(0xb5, 0x8c), 0)
    ## "vvdh" stands for "Vector Velocity Data Header" [p35 of SIG]
    vvdhStart <- .Call("locate_byte_sequences", buf, c(0xa5, 0x12), 42, c(0xb5, 0x8c), 0)

    vvdhTime <- ISOdatetime(2000 + bcdToInteger(buf[vvdhStart+8]), buf[vvdhStart+9], buf[vvdhStart+6], buf[vvdhStart+7], buf[vvdhStart+4],buf[vvdhStart+5], tz=tz)
    vvdhRecords <- readBin(buf[sort(c(vvdhStart, vvdhStart+1))+10], "integer", size=2, n=length(vvdhStart), signed=FALSE, endian="little")

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
    res@metadata$velocityScale <- if ("0" == substr(byteToBinary(buf[vsdStart[1] + 23], endian="big"), 7, 7)) 1e-3 else 0.1e-3
    oceDebug(debug, "velocityScale=", res@metadata$velocityScale, "m/s (from VSD header byte 24, 0x",
              as.raw(buf[vsdStart[1] + 23]), "(bit 7 of",
              byteToBinary(buf[vsdStart[1] + 23], endian="big"), ")\n")

    ## Measurement start and end times.
    vsdLen <- length(vsdStart)
    res@metadata$measurementStart <- ISOdatetime(2000 + bcdToInteger(buf[vsdStart[1]+8]),  # year
                                                 bcdToInteger(buf[vsdStart[1]+9]), # month
                                                 bcdToInteger(buf[vsdStart[1]+6]), # day
                                                 bcdToInteger(buf[vsdStart[1]+7]), # hour
                                                 bcdToInteger(buf[vsdStart[1]+4]), # min
                                                 bcdToInteger(buf[vsdStart[1]+5]), # sec
                                                 tz=tz)
    res@metadata$measurementEnd <- ISOdatetime(2000 + bcdToInteger(buf[vsdStart[vsdLen]+8]),  # year
                                               bcdToInteger(buf[vsdStart[vsdLen]+9]), # month
                                               bcdToInteger(buf[vsdStart[vsdLen]+6]), # day
                                               bcdToInteger(buf[vsdStart[vsdLen]+7]), # hour
                                               bcdToInteger(buf[vsdStart[vsdLen]+4]), # min
                                               bcdToInteger(buf[vsdStart[vsdLen]+5]), # sec
                                               tz=tz)
    vvdLen <- length(vvdStart)
    res@metadata$measurementDeltat <- (as.numeric(res@metadata$measurementEnd) - as.numeric(res@metadata$measurementStart)) / (vvdLen - 1)

    toGiven <- !missing(to)
    if (!toGiven)
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
            oceDebug(debug, vectorShow(vsdStart, "before subset, vsdStart is"))
            vvdStart <- vvdStart[fromIndex:toIndex]
            oceDebug(debug, vectorShow(vvdStart, "after  subset, vvdStart is"))
            ## ensure that vvdStart pointers are bracketed by vsdStart pointers
            ## FIXME: but will this invalidate fromIndex and toIndex?
            vsdStartFrom <- which(vvdStart[1] >= vsdStart)[1]
            vsdStartTo <- which(vsdStart >= vvdStart[length(vvdStart)])[1]
            oceDebug(debug, "vsdStartFrom=", vsdStartFrom, "and vsdStartTo=", vsdStartTo, "(before NA check)\n")
            if (is.na(vsdStartTo))
                vsdStartTo <- length(vsdStart)
            oceDebug(debug, "vsdStartFrom=", vsdStartFrom, "and vsdStartTo=", vsdStartTo, "(after NA check)\n")
            vsdStart <- vsdStart[seq(vsdStartFrom, vsdStartTo)]
            oceDebug(debug, vectorShow(vsdStart, "after  subset, vsdStart is"))
        }
    }
    oceDebug(debug, "about to trim vsdStart, based on vvdStart[1]=", vvdStart[1], " and vvdStart[length(vvdStart)]=", vvdStart[length(vvdStart)], "\n")
    oceDebug(debug, vectorShow(vsdStart, "before trimming, vsdStart:"))
    oceDebug(debug, "from=", from, "to=", to, "\n")

    ## Find spanning subset, expanded a little for now
    subsetStart <- head(which(vvdStart[1] < vsdStart), 1)
    if (subsetStart > 1)
        subsetStart <- subsetStart - 1 # extend a bit (for now)
    subsetEnd <- tail(which(vsdStart < vvdStart[length(vvdStart)]), 1)
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
    vsdTime <- ISOdatetime(2000 + bcdToInteger(buf[vsdStart+8]),  # year
                           bcdToInteger(buf[vsdStart+9]), # month
                           bcdToInteger(buf[vsdStart+6]), # day
                           bcdToInteger(buf[vsdStart+7]), # hour
                           bcdToInteger(buf[vsdStart+4]), # min
                           bcdToInteger(buf[vsdStart+5]), # sec
                           tz=tz)

    oceDebug(debug, "reading Nortek Vector, and using timezone: ", tz, "\n")

    ## update res@metadata$measurementDeltat
    res@metadata$measurementDeltat <- mean(diff(as.numeric(vsdTime)), na.rm=TRUE) * length(vsdStart) / length(vvdStart) # FIXME

    vsdLen <- length(vsdStart)
    vsdStart2 <- sort(c(vsdStart, 1 + vsdStart))
    voltage <- 0.1 * readBin(buf[vsdStart2 + 10], "integer", size=2, n=vsdLen, signed=FALSE, endian="little")
    heading <- 0.1 * readBin(buf[vsdStart2 + 14], "integer", size=2, n=vsdLen, signed=TRUE, endian="little")
    oceDebug(debug, vectorShow(heading, "heading"))
    pitch <-   0.1 * readBin(buf[vsdStart2 + 16], "integer", size=2, n=vsdLen, signed=TRUE, endian="little")
    oceDebug(debug, vectorShow(pitch, "pitch"))
    roll <-    0.1 * readBin(buf[vsdStart2 + 18], "integer", size=2, n=vsdLen, signed=TRUE, endian="little")
    oceDebug(debug, vectorShow(roll, "roll"))
    temperature <- 0.01 * readBin(buf[vsdStart2 + 20], "integer", size=2, n=vsdLen, signed=TRUE, endian="little")
    oceDebug(debug, vectorShow(temperature, "temperature"))
    salinity <- header$user$salinity
    oceDebug(debug, "salinity (in res@metadata):", salinity, "\n")
    ## byte 22 is an error code
    ## byte 23 is status, with bit 0 being orientation (p36 of Nortek's System Integrator Guide)
    status <- buf[vsdStart[floor(0.5*length(vsdStart))] + 23]
    res@metadata$orientation <- if ("0" == substr(byteToBinary(status, endian="big"), 1, 1)) "upward" else "downward"
    # FIXME: should read roll and pitch "out of range" or "OK" here, in bites 3 and 2
    
    ##FIXME was wrong## res@metadata$burstLength <- round(length(vvdStart) / length(vsdStart), 0) # FIXME: surely this is in the header (?!?)
    ##FIXME was wrong## oceDebug(debug, vectorShow(res@metadata$burstLength, "burstLength"))

    vvdStart2 <- sort(c(vvdStart, 1 + vvdStart))
    vvdLen <- length(vvdStart)          # FIXME: should be subsampled with 'by' ... but how???

    if (haveAnalog1) { # FIXME: shouldn't this be auto-detected from 'USER' header?
        analog1 <- readBin(buf[vvdStart + 8], "integer", n=vvdLen, size=1)
    }
    if (haveAnalog2) { # FIXME: shouldn't this be auto-detected from 'USER' header?
        start <- sort(c(vvdStart+2, vvdStart+5))
        analog2 <- readBin(buf[start], "integer", n=vvdLen, size=2, endian="little", signed=FALSE)
    }

    p.MSB <- as.numeric(buf[vvdStart + 4])
    p.LSW <- readBin(buf[vvdStart2 + 6], "integer", size=2, n=vvdLen, signed=FALSE, endian="little")
    pressure <- (65536 * p.MSB + p.LSW) / 1000
    oceDebug(debug, vectorShow(pressure, "pressure"))
    v <- array(double(), dim=c(vvdLen, 3))
    v[,1] <- res@metadata$velocityScale*readBin(buf[vvdStart2 + 10], "integer", size=2, n=vvdLen, signed=TRUE, endian="little")
    v[,2] <- res@metadata$velocityScale*readBin(buf[vvdStart2 + 12], "integer", size=2, n=vvdLen, signed=TRUE, endian="little")
    v[,3] <- res@metadata$velocityScale*readBin(buf[vvdStart2 + 14], "integer", size=2, n=vvdLen, signed=TRUE, endian="little")
    if (debug > 0.9) {
        oceDebug(debug, "v[", dim(v), "] begins...\n")
        print(matrix(as.numeric(v[1:min(3,vvdLen),]), ncol=3))
    }
    a <- array(raw(), dim=c(vvdLen, 3))
    a[,1] <- buf[vvdStart + 16]
    a[,2] <- buf[vvdStart + 17]
    a[,3] <- buf[vvdStart + 18]
    if (debug > 0.9) {
        oceDebug(debug, "a[", dim(a), "] begins...\n")
        print(matrix(as.numeric(a[1:min(3,vvdLen),]), ncol=3))
    }
    q <- array(raw(), dim=c(vvdLen, 3))
    q[,1] <- buf[vvdStart + 19]
    q[,2] <- buf[vvdStart + 20]
    q[,3] <- buf[vvdStart + 21]
    if (debug > 0.9) {
        cat("q[", dim(q), "] begins...\n")
        print(matrix(as.numeric(q[1:min(3,vvdLen),]), ncol=3))
    }
    ##sec <- as.numeric(vsdTime) - as.numeric(vsdTime[1])
    ##vds <- var(diff(sec))
    ##BAD: vvdSec <- .Call("stutter_time", sec, 8)
    #######vvdSec <- approx(seq(0, 1, length.out=length(vsdTime)), vsdTime, seq(0, 1, length.out=length(vvdStart)))$y
    #######oceDebug(debug, vectorShow(vvdSec, "vvdSec"))
    oceDebug(debug, vectorShow(vsdStart, "vsdStart"))
    oceDebug(debug, vectorShow(vvdStart, "vvdStart"))
    rm(buf)
    gc()
    ## subset using 'by'
    ##by.orig <- by
    if (is.character(by)) {
        oceDebug(debug, "by='",by,"' given as argument to read.adv.nortek()\n",sep="")
        oceDebug(debug, " ... infer to be", ctimeToSeconds(by), "s\n")
        by <- ctimeToSeconds(by) / res@metadata$measurementDeltat
        oceDebug(debug, " ... so step by" ,by,"through the data\n")
    }
    len <- length(vvdStart)
    look <- seq(1, len, by=by)
    oceDebug(debug, "length(vvdStart)=",length(vvdStart),"\n")
    ##vvdStart.orig <- vvdStart
    vvdStart <- vvdStart[look]
    oceDebug(debug, "length(vvdStart)=",length(vvdStart),"(after 'look'ing) with by=", by, "\n")
    ######vvdSec <- vvdSec[look]
    pressure <- pressure[look]          # only output at burst headers, not with velo (FIXME: huh??)
    v <- v[look,]
    a <- a[look,]
    q <- q[look,]
    if (0 < sum(vvdhRecords)) {
        res@metadata$samplingMode <- "burst"

        ## Note: if we knew that all bursts were of the same length, we could use the same method
        ## as for the continuous case, specifying e.g. vvdhRecords[1] instead of 0.  But do we know that?
        ## Also, what I'm doing here is probably fine, since bursts last an hour and so looping
        ## also them won't be expensive.
        sss <- NULL
        for (b in 1:length(vvdhRecords)) {
            sss <- c(sss, as.numeric(vvdhTime[b]) + seq(0, by=1/res@metadata$samplingRate, length.out=vvdhRecords[b]))
        }
        time <- sss[look] + (vsdTime[1] - as.numeric(vsdTime[1]))
        delayForWarmup <- 2 + 1 / (res@metadata$samplingRate * 2) # FIXME: this is from a forum posting, not an official doc.
        time <- time + delayForWarmup
    } else {
        res@metadata$samplingMode <- "continuous"
        time <- numberAsPOSIXct(.Call("adv_vector_time", vvdStart, vsdStart, vsdTime, vvdhStart, vvdhTime, 0, res@metadata$samplingRate))
    }
    res@metadata$numberOfSamples <- dim(v)[1]
    res@metadata$numberOfBeams <- dim(v)[2]
    res@metadata$velocityResolution <- res@metadata$velocityScale / 2^15
    res@metadata$salinity <- salinity

    ## FIXME: guess-based kludge to infer whether continuous or burst-mode sample 
    res@data <- list(v=v, a=a, q=q,
                     time=time,
                     pressure=pressure,

                     timeBurst=vvdhTime,
                     recordsBurst=vvdhRecords,
                     voltageSlow=voltage,

                     timeSlow=vsdTime,
                     headingSlow=heading,
                     pitchSlow=pitch,
                     rollSlow=roll,
                     temperatureSlow=temperature)
    if (haveAnalog1)
        res@data$analog1 <- analog1
    if (haveAnalog2)
        res@data$analog2 <- analog2
    res@metadata$velocityResolution <- res@metadata$velocityScale
    res@metadata$velocityMaximum <- res@metadata$velocityScale * 2^15
    res@metadata$units <- list(v="m/s")
    res@processingLog <- unclass(hitem)
    oceDebug(debug, "} # read.adv.nortek(file=\"", filename, "\", ...)\n", sep="", unindent=1)
    res
}

