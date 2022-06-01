# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' @template readAdvTemplate
#'
#' @param start the time of the first sample, typically created with
#' [as.POSIXct()].  This may be a vector of times,
#' if `filename` is a vector of file names.
#'
#' @param deltat the time between samples.
read.adv.sontek.serial <- function(file,
    from=1,
    to,
    by=1,
    tz=getOption("oceTz"),
    longitude=NA,
    latitude=NA,
    start=NULL,
    deltat=NULL,
    debug=getOption("oceDebug"),
    monitor=FALSE,
    processingLog=NULL)
{
    if (missing(file))
        stop("must supply 'file'")
    if (is.character(file)) {
        if (!file.exists(file))
            stop("cannot find file '", file, "'")
        if (0L == file.info(file)$size)
            stop("empty file '", file, "'")
    }
    if (!interactive())
        monitor <- FALSE
    oceDebug(debug, paste("read.adv.sontek.serial(file[1]=\"", file[1],
                           "\", from=", format(from),
                           if (!missing(to)) sprintf(", to=%s, ", format(to)),
                           ", by=", by,
                           ", start[1]=", format(start[1]),
                           ", deltat=", deltat,
                           ", debug=", debug,
                           ", monitor=", monitor,
                           ", processingLog=(not shown)) {\n", sep=""), unindent=1)
    if (is.null(start) || is.numeric(start))
        stop("'start' must be a string, or a POSIXt time")
    if (is.character(start))
        start <- as.POSIXct(start, tz=tz)
    if (!is.numeric(deltat))
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

    if (nstart > 1) {
        ## handle multiple files
        oceDebug(debug, "handling multiple files\n")
        buf <- NULL
        for (i in 1:nfile) {
            oceDebug(debug, "loading \"", file[i], "\" (startTime ", format(start[i]), " ", attr(start[i], "tzone"), ")\n", sep="")
            thisFile <- file(file[i], "rb")
            seek(thisFile, 0, "end", rw="read")
            fileSize <- seek(thisFile, 0, origin="start", rw="read")
            oceDebug(debug, "fileSize=", fileSize, "\n")
            buf <- c(buf, readBin(thisFile, what="raw", n=fileSize, endian="little"))
            close(thisFile)
        }
        filename <- paste("(\"", file[i], "\", ...)", sep="")
    } else {
        ## handle single file (which might be a connection, etc)
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
        oceDebug(debug, "filesize=", fileSize, "\n")
        buf <- readBin(file, what="raw", n=fileSize, endian="little")
    }

    p <- .Call("ldc_sontek_adv_22", buf, 0) # the 0 means to get all pointers to data chunks
    pp <- sort(c(p, p+1))
    len <- length(p)
    oceDebug(debug, "dp:", paste(unique(diff(p)), collapse=","), "\n")
    serialNumber <- readBin(buf[pp+2], "integer", size=2, n=len, signed=FALSE, endian="little")
    serialNumber <- .Call("unwrap_sequence_numbers", serialNumber, 2)
    velocityScale <- 1e-4
    time <- start[1] + (serialNumber - serialNumber[1]) * deltat
    deltat <- mean(diff(as.numeric(time))) # FIXME: should rename this to avoid confusion
    res <- new("adv", time=time, filename=filename)
    ## FIXME: emulate this direct injection in other functions, in hopes of reducing memory footprint
    res@data$v <- array(numeric(), dim=c(len, 3))
    res@data$v[, 1] <- velocityScale * readBin(buf[pp+4], "integer", size=2, n=len, signed=TRUE, endian="little")
    res@data$v[, 2] <- velocityScale * readBin(buf[pp+6], "integer", size=2, n=len, signed=TRUE, endian="little")
    res@data$v[, 3] <- velocityScale * readBin(buf[pp+8], "integer", size=2, n=len, signed=TRUE, endian="little")
    res@data$a <- array(raw(), dim=c(len, 3))
    res@data$a[, 1] <- as.raw(readBin(buf[p+10], "integer", size=1, n=len, signed=FALSE, endian="little"))
    res@data$a[, 2] <- as.raw(readBin(buf[p+11], "integer", size=1, n=len, signed=FALSE, endian="little"))
    res@data$a[, 3] <- as.raw(readBin(buf[p+12], "integer", size=1, n=len, signed=FALSE, endian="little"))
    res@data$q <- array(raw(), dim=c(len, 3))
    res@data$q[, 1] <- as.raw(readBin(buf[p+13], "integer", size=1, n=len, signed=FALSE, endian="little"))
    res@data$q[, 2] <- as.raw(readBin(buf[p+14], "integer", size=1, n=len, signed=FALSE, endian="little"))
    res@data$q[, 3] <- as.raw(readBin(buf[p+15], "integer", size=1, n=len, signed=FALSE, endian="little"))
    res@data$temperature <- 0.01 * readBin(buf[pp+16], "integer", size=2, n=len, signed=TRUE, endian="little")
    res@data$pressure <- readBin(buf[pp+18], "integer", size=2, n=len, signed=FALSE, endian="little") # may be 0 for all
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
    res@metadata$manufacturer <- "sontek"
    res@metadata$instrumentType <- "adv"
    res@metadata$filename <- filename
    res@metadata$longitude <- longitude
    res@metadata$latitude <- latitude
    res@metadata$numberOfSamples <- len
    res@metadata$numberOfBeams <- 3
    res@metadata$serialNumber <- "?"
    res@metadata$transformationMatrix <- transformationMatrix
    res@metadata$measurementStart <- time[1]
    res@metadata$measurementEnd <- time[length(time)]
    res@metadata$measurementDeltat <- deltat
    res@metadata$subsampleStart <- time[1] # FIXME: this seems wrong
    res@metadata$subsampleEnd <- time[length(time)] # FIXME: this seems wrong
    res@metadata$subsampleDeltat <- deltat
    res@metadata$##velocityScale <- velocityScale
    res@metadata$originalCoordinate <- "xyz" # guess
    res@metadata$velocityResolution <- velocityScale
    res@metadata$velocityMaximum <- velocityScale * 2^15
    res@metadata$oceCoordinate <- "xyz"    # guess
    res@metadata$orientation <- "upward" # guess
    warning("sontek adv in serial format lacks heading, pitch and roll: user must fill in")
    res@data$heading <- rep(0, len)
    res@data$pitch <- rep(0, len)
    res@data$roll <- rep(0, len)
    res@metadata$units$v <- list(unit=expression(m/s), scale="")
    res@metadata$units$pressure <- list(unit=expression(dbar), scale="")
    res@metadata$units$heading <- list(unit=expression(degree), scale="")
    res@metadata$units$pitch <- list(unit=expression(degree), scale="")
    res@metadata$units$roll <- list(unit=expression(degree), scale="")
    res@metadata$units$temperature <- list(unit=expression(degree*C), scale="")
    if (is.null(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    res@processingLog <- processingLogAppend(res@processingLog, processingLog)
    ##gc()
    res
}

#' @template readAdvTemplate
#'
#' @param header A logical value indicating whether the file starts with a header.
#' (This will not be the case for files that are created by data loggers that
#' chop the raw data up into a series of sub-files, e.g. once per hour.)
read.adv.sontek.adr <- function(file,
    from=1,
    to,
    by=1,
    tz=getOption("oceTz"),
    header=TRUE,
    longitude=NA,
    latitude=NA,
    debug=getOption("oceDebug"),
    monitor=FALSE,
    processingLog=NULL)
{
    if (missing(file))
        stop("must supply 'file'")
    if (is.character(file)) {
        if (!file.exists(file))
            stop("cannot find file '", file, "'")
        if (0L == file.info(file)$size)
            stop("empty file '", file, "'")
    }
    if (!interactive())
        monitor <- FALSE
    bisectAdvSontekAdr <- function(burstTime, tFind, add=0, debug=0) {
        oceDebug(debug, "bisectAdvSontekAdr(tFind=", format(tFind), ", add=", add, "\n")
        len <- length(burstTime)
        lower <- 1
        upper <- len
        passes <- floor(10 + log(len, 2)) # won't need this many; only do this to catch coding errors
        for (pass in 1:passes) {
            middle <- floor((upper + lower) / 2) # nolint (no space before opening parenthesis)
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
    oceDebug(debug, "read.adv.sontek.adr() {\n", unindent=1)
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
    oceDebug(debug, "filesize=", fileSize, "\n")
    buf <- readBin(file, what="raw", n=fileSize, endian="little")

    ## Read header, or create a nominal default one.
    ##  24 bytes hardwareConfiguration ("AdvSystemConfigType" in the docs)
    ## 164 bytes probeConfiguration ("AdvConfType" in the docs)
    ## 253 bytes deployment setup ("AdvDeploymentSetupType" in the docs)
    hardwareConfigurationLength <- 24
    probeConfigurationLength <- 164
    deploymentParametersLength <- 253
    burstHeaderLength <- 60
    ##checksumLength <- 2
    dataLength <- 22                   # FIXME: this should be determined based on the headers
    res <- new("adv")
    res@metadata$manufacturer <- "sontek"
    res@metadata$instrumentType <- "adv" # FIXME or "adr"???
    res@metadata$filename <- filename
    res@metadata$longitude <- longitude
    res@metadata$latitude <- latitude
    res@metadata$numberOfSamples <- NA # fill in later
    res@metadata$numberOfBeams <- NA # fill in later
    res@metadata$measurementDeltat <- 1
    res@metadata$velocityScale <- 1e-4
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
        res@metadata$cpuSoftwareVerNum <- 0.1 * as.numeric(hardwareConfiguration[1])
        oceDebug(debug, "cpuSoftwareVerNum=", res@metadata$cpuSoftwareVerNum, "\n")

        res@metadata$dspSoftwareVerNum <- 0.1 * as.numeric(hardwareConfiguration[2])
        oceDebug(debug, "dspSoftwareVerNum=", res@metadata$dspSoftwareVerNum, "\n")

        res@metadata$orientation <- c("downward", "upward", "sideways")[1 + as.numeric(hardwareConfiguration[4])]
        oceDebug(debug, "orientation=", res@metadata$orientation, "\n")

        res@metadata$compassInstalled <- as.integer(hardwareConfiguration[5]) == 1
        oceDebug(debug, "compassInstalled=", res@metadata$compassInstalled, "\n")
        if (!res@metadata$compassInstalled)
            stop("cannot handle data files for ADV files that lack compass data")

        res@metadata$recorderInstalled <- if (as.integer(hardwareConfiguration[6]) == 1) TRUE else FALSE
        oceDebug(debug, "recorderInstalled=", res@metadata$recorderInstalled, "\n")

        res@metadata$thermometerInstalled <- as.integer(hardwareConfiguration[7]) == 1
        oceDebug(debug, "thermometerInstalled=", res@metadata$thermometerInstalled, "\n")
        if (!res@metadata$thermometerInstalled)
            stop("cannot handle data files for ADV files that lack thermometer data")

        res@metadata$pressureInstalled <- as.integer(hardwareConfiguration[8]) == 1
        oceDebug(debug, "pressureInstalled=", res@metadata$pressureInstalled, "\n")
        if (!res@metadata$pressureInstalled)
            stop("cannot handle data files for ADV files that lack pressure data")

        ## we report pressure in dbar, so use the fact that 1 nanobar/count = 1e-8 dbar/count
        res@metadata$pressureScale <- 1e-8 * readBin(hardwareConfiguration[9:12], "integer", size=4, n=1, endian="little")
        oceDebug(debug, "pressureScale=", res@metadata$pressureScale, "dbar/count (header gives in nanobar/count)\n")

        ## we report pressure in dbar, so use the fact that 1 microbar = 1e-5 dbar
        res@metadata$pressureOffset <- 1e-5 * readBin(hardwareConfiguration[13:16], "integer", size=4, n=1, endian="little")
        oceDebug(debug, "pressureOffset=", res@metadata$pressureOffset, "dbar (header gives in microbar)\n")

        res@metadata$compassOffset <- readBin(hardwareConfiguration[23:24], "integer", size=2, n=1, endian="little", signed=TRUE)
        oceDebug(debug, "compassOffset=", res@metadata$compassOffset, "(degrees to East of North)\n")

        res@metadata$pressFreqOffset <- as.integer(hardwareConfiguration[25])
        oceDebug(debug, "pressFreqOffset=", res@metadata$pressFreqOffset, "(\"Frequency Pres Sensor Offset\" in docs)\n")

        res@metadata$extSensorInstalled <- as.integer(hardwareConfiguration[26])
        oceDebug(debug, "extSensorInstalled=", res@metadata$extSensorInstalled, "(\"0=None, 1=Standard (ch 1/3)\" in docs)\n")

        res@metadata$extPressInstalled <- as.integer(hardwareConfiguration[27])
        oceDebug(debug, "extPressInstalled=", res@metadata$extPressInstalled, "(1=Paros 2=Druck 3=ParosFreq)\n")

        ## we report pressure in dbar, so use the fact that 1 pbar = 1e-11 dbar
        res@metadata$pressureScale2 <- 1e-11 * readBin(hardwareConfiguration[28:29], "integer", size=2, n=1, endian="little", signed=TRUE)
        oceDebug(debug, "pressureScale2=", res@metadata$pressureScale2, "dbar/count^2 (file gives in picobar/count^2)\n")


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

        res@metadata$serialNumber <- paste(readBin(probeConfiguration[11:16], "character", n=5, size=1), collapse="")  # "B373H"
        oceDebug(debug, "serialNumber=", res@metadata$serialNumber, "\n")

        res@metadata$probeType <- readBin(probeConfiguration[17], "integer", n=1, size=1)
        oceDebug(debug, "probeType=", res@metadata$probeType, "(\"3/2-d orientation\", according to the docs)\n")

        res@metadata$probeSize <- readBin(probeConfiguration[18], "integer", n=1, size=1)
        oceDebug(debug, "probeSize=", res@metadata$probeSize, "(0 means 5cm; 1 means 10cm probe, according to docs)\n")

        res@metadata$numberOfBeams <- readBin(probeConfiguration[19:20], "integer", n=1, size=2, endian="little")
        oceDebug(debug, "numberOfBeams=", res@metadata$numberOfBeams, "(should be 3)\n")
        if (res@metadata$numberOfBeams != 3)
            warning("number of beams should be 3, but it is ", res@metadata$numberOfBeams, " ... reseting to 3")

        res@metadata$probeNomPeakPos <- readBin(probeConfiguration[21:22], "integer", n=1, size=2, endian="little")
        oceDebug(debug, "probeNomPeakPos=", res@metadata$probeNomPeakPos, "(not used here)\n")

        res@metadata$probeNsamp <- readBin(probeConfiguration[23:24], "integer", n=1, size=2, endian="little")
        oceDebug(debug, "probeNsamp=", res@metadata$probeNsamp, "(not used here)\n")

        res@metadata$probeSampInterval <- readBin(probeConfiguration[25:26], "integer", n=1, size=2, endian="little")
        oceDebug(debug, "probeSampInterval=", res@metadata$probeSampInterval, "(not used here)\n")

        res@metadata$probePulseLag <- readBin(probeConfiguration[27:56], "integer", n=15, size=2, endian="little")
        oceDebug(debug, "probePulseLag=", res@metadata$probePulseLag, "([5][3], not used here)\n")

        res@metadata$probeNxmit <- readBin(probeConfiguration[57:86], "integer", n=15, size=2, endian="little")
        oceDebug(debug, "probeNxmit=", res@metadata$probeNxmit, "([5][3], not used here)\n")

        res@metadata$probeLagDelay <- readBin(probeConfiguration[87:116], "integer", n=15, size=2, endian="little")
        oceDebug(debug, "probeLagDelay=", res@metadata$probeLagDelay, "([5][3], not used here)\n")

        res@metadata$probeBeamDelay <- readBin(probeConfiguration[117:118], "integer", n=1, size=2, endian="little")
        oceDebug(debug, "probeBeamDelay=", res@metadata$probeBeamDelay, "(not used here)\n")

        res@metadata$probePingDelay <- readBin(probeConfiguration[119:120], "integer", n=1, size=2, endian="little")
        oceDebug(debug, "probePingDelay=", res@metadata$probePingDelay, "(not used here)\n")

        res@metadata$transformationMatrix <- matrix(readBin(probeConfiguration[121:157], "numeric", n=9, size=4, endian="little"),
                                                 nrow=3, byrow=TRUE)
        oceDebug(debug, "transformation matrix:\n")
        oceDebug(debug, "  ", format(res@metadata$transformationMatrix[1, ], width=10, digits=5, justify="right"), "\n")
        oceDebug(debug, "  ", format(res@metadata$transformationMatrix[2, ], width=10, digits=5, justify="right"), "\n")
        oceDebug(debug, "  ", format(res@metadata$transformationMatrix[3, ], width=10, digits=5, justify="right"), "\n")

        ## [158:161] float XmitRecDist
        ## [162:165] float CalCw
        ## FIXME why is this not 164 bytes in total?

        ##
        ## Analyze "deploymentParameters" header

        if (deploymentParameters[1]!=0x12)
            stop("first byte of deploymentParameters header should be 0x12 but it is 0x", deploymentParameters[1])

        if (deploymentParameters[2]!=0x01)
            stop("first byte of deploymentParameters header should be 0x01 but it is 0x", deploymentParameters[2])

        res@metadata$velocityRangeIndex <- as.numeric(deploymentParameters[20])
        oceDebug(debug, "velocityRangeIndex=", res@metadata$velocityRangeIndex, "\n")
        if (res@metadata$velocityRangeIndex == 4) {
            res@metadata$velocityScale <- 2 * res@metadata$velocityScale # range 4 differs from ranges 1:3
        }

        originalCoordinateCode <- as.integer(deploymentParameters[22]) # 1 (0=beam 1=xyz 2=ENU)
        res@metadata$originalCoordinate <- c("beam", "xyz", "enu")[1+originalCoordinateCode]
        res@metadata$oceCoordinate <- res@metadata$originalCoordinate
        oceDebug(debug, "originalCoordinate=", res@metadata$originalCoordinate, "\n")
        if (res@metadata$originalCoordinate == "beam")
            stop("cannot handle beam coordinates")

        ## FIXME: bug: docs say samplingRate in units of 0.1Hz, but the SLEIWEX-2008-m3 data file is in 0.01Hz

        samplingRate <- 0.01*readBin(deploymentParameters[23:28], "integer", n=3, size=2, endian="little", signed=FALSE)
        if (samplingRate[2] != 0 || samplingRate[3] != 0)
            warning("ignoring non-zero items 2 and/or 3 of samplingRate vector")
        res@metadata$samplingRate <- samplingRate[1]
        if (res@metadata$samplingRate < 0)
            stop("samplingRate must be a positive integer, but got ", res@metadata$samplingRate)
        res@metadata$measurementDeltat <- 1 / res@metadata$samplingRate
        res@metadata$burstInterval <- readBin(deploymentParameters[29:34], "integer", n=3, size=2, endian="little", signed=FALSE)
        if (res@metadata$burstInterval[2] !=0 || res@metadata$burstInterval[3] != 0)
            warning("ignoring non-zero items 2 and/or 3 in burstInterval vector")
        res@metadata$burstInterval <- res@metadata$burstInterval[1]
        res@metadata$samplesPerBurst <- readBin(deploymentParameters[35:40], "integer", n=3, size=2, endian="little", signed=FALSE)
        if (res@metadata$samplesPerBurst[2] !=0 || res@metadata$samplesPerBurst[3] != 0)
            warning("ignoring non-zero items 2 and/or 3 in samplesPerBurst vector")
        res@metadata$samplesPerBurst <- res@metadata$samplesPerBurst[1]
        if (res@metadata$samplesPerBurst < 0)
            stop("samplesPerBurst must be a positive integer, but got ", res@metadata$samplesPerBurst)
        res@metadata$deploymentName <- paste(integerToAscii(as.integer(deploymentParameters[49:57])), collapse="")
        res@metadata$comments1 <- paste(integerToAscii(as.integer(deploymentParameters[66:125])), collapse="")
        res@metadata$comments2 <- paste(integerToAscii(as.integer(deploymentParameters[126:185])), collapse="")
        res@metadata$comments3 <- paste(integerToAscii(as.integer(deploymentParameters[126:185])), collapse="")
    }                                  # if (header)

    ## Use 3-byte flag to find bursts in buf.  Then find their times, and # samples in each.
    ## Note: checking not just on the 2 "official" bytes, but also on third (3c=60=number of bytes in header)
    burstBufindex <- matchBytes(buf, 0xA5, 0x11, 0x3c)

    oceDebug(debug, "burstBufindex[1:10]=", paste(burstBufindex[1:10], collapse=" "), "\n")

    nbursts <- length(burstBufindex)
    res@metadata$numberOfBursts <- nbursts

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
    burstTimeExtended <- c(burstTime, burstTime[nbursts] + samplesPerBurst[nbursts] / res@metadata$samplingRate)
    attr(burstTimeExtended, "tzone") <- attr(burstTime, "tzone")

    res@metadata$measurementStart <- min(burstTimeExtended)
    res@metadata$measurementEnd <- max(burstTimeExtended)
    res@metadata$measurementDeltat <- (as.numeric(burstTime[length(burstTime)]) - as.numeric(burstTime[1])) / sum(samplesPerBurst)

    oceDebug(debug, "burstTimeExtended ranges", paste(range(burstTimeExtended), collapse=" to "), "\n")

    ## Sample indices (not buf indices) of first sample in each burst
    burstSampleIndex.extended <- c(1, cumsum(samplesPerBurst))
    burstSampleIndex <- burstSampleIndex.extended[-length(burstSampleIndex.extended)]
    oceDebug(debug, "burstSampleIndex[1:10]=", paste(burstSampleIndex[1:10], collapse=" "), "\n")

    ## Map from sample number toBurst number
    burst <- 1:nbursts
    if (debug > 0)
        print(data.frame(burst, burstTime, burstBufindex)[1:5, ])

    ## Interpret 'from', 'to', and 'by', possibly integers, POSIX times, or strings for POSIX tiems
    if (missing(from))
        from <- 1
    if (missing(to))
        to <- burstSampleIndex[length(burstSampleIndex)]
    fromKeep <- from
    toKeep <- to
    if (inherits(from, "POSIXt")) {
        if (!inherits(to, "POSIXt"))
            stop("if 'from' is POSIXt, then 'to' must be, also")
        fromToPOSIX <- TRUE
        fromPair <- bisectAdvSontekAdr(burstTime, from, add=-1, debug=debug-1)
        fromBurst <- fromPair$index
        oceDebug(debug, "fromKeep=", format(fromKeep), " yields burstTime[", fromBurst, "]=", format(fromPair$t), "\n")
        toPair <- bisectAdvSontekAdr(burstTime, to, add=1, debug=debug-1)
        toBurst <- toPair$index
        oceDebug(debug, "toKeep=", format(toKeep), " yields burstTime[", toBurst, "]=", format(toPair$t), "\n")
        ## burst offsets  FIXME: do we need these?
        fromBurstOffset <- floor(0.5 + (as.numeric(from) - as.numeric(burstTime[fromBurst])) * res@metadata$samplingRate)
        toBurstOffset <- floor(0.5 + (as.numeric(to) - as.numeric(burstTime[toBurst-1])) * res@metadata$samplingRate)
        oceDebug(debug, "fromBurstOffset=", fromBurstOffset, "toBurstOffset=", toBurstOffset, "\n")
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
    oceDebug(debug, "sum(samplers.burstFocus)", sum(samplesPerBurstFocus), "vs", nbursts * as.numeric(burstTime[2]-burstTime[1])*res@metadata$samplingRate, "\n")

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
    q <- array(raw(), dim=c(ntotal, 3))
    rowOffset <- 0

    oceDebug(debug, "dataLength=", dataLength, "\n")
    oceDebug(debug, "burstHeaderLength=", burstHeaderLength, "\n")
    oceDebug(debug, "burstBufindexFocus:", paste(burstBufindexFocus, collapse=" "), "\n")

    velocityScale <- res@metadata$velocityScale

    for (b in 1:nburstsFocus) {
        n <- samplesPerBurstFocus[b]
        oceDebug(debug, "burst", b, "at", format(burstTimeFocus[b]), "data start at byte", burstBufindexFocus[b]+burstHeaderLength, "n=", n, "\n")
        bufSubset <- buf[burstBufindexFocus[b]+burstHeaderLength+0:(-1+dataLength*n)]
        m <- matrix(bufSubset, ncol=dataLength, byrow=TRUE)
        if (n != dim(m)[1])
            stop("something is wrong with the data.  Perhaps the record length is not the assumed value of ", dataLength)
        r <- rowOffset + 1:n
        v[r, 1] <- velocityScale * readBin(t(m[, 1:2]), "integer", n=n, size=2, signed=TRUE, endian="little")
        v[r, 2] <- velocityScale * readBin(t(m[, 3:4]), "integer", n=n, size=2, signed=TRUE, endian="little")
        v[r, 3] <- velocityScale * readBin(t(m[, 5:6]), "integer", n=n, size=2, signed=TRUE, endian="little")
        a[r, 1] <- m[, 7]
        a[r, 2] <- m[, 8]
        a[r, 3] <- m[, 9]
        q[r, 1] <- m[, 10]
        q[r, 2] <- m[, 11]
        q[r, 3] <- m[, 12]
        time[r] <- as.numeric(burstTimeFocus[b]) + seq(0, n-1) / res@metadata$samplingRate
        ##cat(sprintf("%.2f %.2f %.2f\n", time[r[1]], time[r[2]], time[r[3]]))
        ##cat("time=", format(time[r[1]]), ";", format(burstTimeFocus[b]), "\n")
        ##print(range(time[r]))
        heading[r] <- 0.1 * readBin(as.raw(t(m[, 13:14])), "integer", n=n, size=2, signed=TRUE, endian="little")
        pitch[r] <-   0.1 * readBin(as.raw(t(m[, 15:16])), "integer", n=n, size=2, signed=TRUE, endian="little")
        roll[r] <-    0.1 * readBin(as.raw(t(m[, 17:18])), "integer", n=n, size=2, signed=TRUE, endian="little")
        temperature[r] <- 0.01 * readBin(as.raw(t(m[, 19:20])), "integer", n=n, size=2, signed=TRUE, endian="little")

        ## Pressure, using quadratic conversion from counts
        p.count <- readBin(as.raw(t(m[, 21:22])), "integer", n=n, size=2, signed=FALSE, endian="little")
        pressure[r] <- res@metadata$pressureOffset + p.count * (res@metadata$pressureScale + p.count * res@metadata$pressureScale2)

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
    oceDebug(debug, "burstTime[1]=", format(burstTimeFocus[1]), "\n   times=", format(time[1:20]), "\n")
    ## Subset data to match the provided 'from', 'to' and 'by'
    if (fromToPOSIX) {
        iii <- from <= time & time <= to
        if (is.character(by)) {
            subsamplingRate <- floor(0.5 + ctimeToSeconds(by) * res@metadata$samplingRate)
            oceDebug(debug, paste(" by = '", by, "' yields subsamplingRate=", subsamplingRate, "\n"), sep="")
            samples <- seq_along(iii)
            oceDebug(debug, "before interpreting 'by', iii true for", sum(iii), "cases\n")
            iii <- iii & !(samples %% subsamplingRate)
            oceDebug(debug, "after  interpreting 'by', iii true for", sum(iii), "cases\n")
            oceDebug(debug, "'by' is character, so subsampling by", floor(0.5 + ctimeToSeconds(by) * res@metadata$samplingRate), "\n")
        }
    } else {
        indices <- seq(fromIndex, toIndex) # FIXME: ignoring 'by'
        oceDebug(debug, "indices[1:10]=", paste(indices[1:10], collapse=" "), "\n")
        time <- approx(burstSampleIndex.extended, burstTimeExtended - burstTime[1], indices)$y + burstTime[1]
        if (any(is.na(time)))
            warning("some times are NA; this is an internal coding error")
        focusFrom <- fromBurstOffset
        focusto <- toBurstOffset + sum(samplesPerBurstFocus[-length(samplesPerBurstFocus)])
        oceDebug(debug, "focusFrom=", focusFrom, "focusto=", focusto, "\n")
        iii <- seq(focusFrom, focusto, by=by)
    }
    oceDebug(debug, "iii=", iii[1], iii[2], "...", iii[-1+length(iii)], iii[length(iii)], "\n")
    if (any(iii < 0))
        stop("got negative numbers in iii, which indicates a coding problem; range(iii)=", paste(range(iii), collapse=" to "))
    oceDebug(debug, "dim(v)=", paste(dim(v), collapse=" "), "\n")
    v <- v[iii, ]
    a <- a[iii, ]
    q <- q[iii, ]
    ## No need to subset time if 'from' and 'to' are integers; I am not really
    ## sure we want to in the POSIX case, either, but I am not changing that for now.
    ## DEK (issue 1386)
    if (fromToPOSIX)
        time <- time[iii]
    pressure <- pressure[iii]
    temperature <- temperature[iii]
    pitch <- pitch[iii]
    heading <- heading[iii]
    roll <- roll[iii]
    res@metadata$numberOfSamples <- dim(v)[1]
    res@metadata$numberOfBeams <- dim(v)[2]
    res@metadata$velocityResolution <- velocityScale
    res@metadata$velocityMaximum <- velocityScale * 2^15
    res@data <- list(v=v, a=a, q=q,
                     time=time,
                     heading=heading,
                     pitch=pitch,
                     roll=roll,
                     temperature=temperature,
                     pressure=pressure)
    res@metadata$units$v <- list(unit=expression(m/s), scale="")
    res@metadata$units$pressure <- list(unit=expression(dbar), scale="")
    res@metadata$units$heading <- list(unit=expression(degree), scale="")
    res@metadata$units$pitch <- list(unit=expression(degree), scale="")
    res@metadata$units$roll <- list(unit=expression(degree), scale="")
    res@metadata$units$temperature <- list(unit=expression(degree*C), scale="")
    if (is.null(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    hitem <- processingLogItem(processingLog)
    res@processingLog <- hitem
    res
}

#' @template readAdvTemplate
#'
#' @param originalCoordinate character string indicating coordinate system, one
#' of `"beam"`, `"xyz"`, `"enu"` or `"other"`.  (This is
#' needed for the case of multiple files that were created by a data logger,
#' because the header information is normally lost in such instances.)
#'
#' @param transformationMatrix transformation matrix to use in converting beam
#' coordinates to xyz coordinates.  This will over-ride the matrix in the file
#' header, if there is one.  An example is \code{rbind(c(2.710, -1.409,
#' -1.299), c(0.071, 2.372, -2.442), c(0.344, 0.344, 0.344))}.
#'
#' @section Note on file name:
#' The `file` argument does not actually name a file. It names a basename
#' for a file. The actual file names are created by appending suffix
#' `.hd1` for one file and `.ts1` for another.
read.adv.sontek.text <- function(file,
    from=1,
    to,
    by=1,
    tz=getOption("oceTz"),
    originalCoordinate="xyz",
    transformationMatrix,
    longitude=NA,
    latitude=NA,
    debug=getOption("oceDebug"),
    monitor=FALSE,
    processingLog=NULL)
{
    if (missing(file))
        stop("must supply 'file'")
    if (is.character(file)) {
        if (!file.exists(file))
            stop("cannot find file '", file, "'")
        if (0L == file.info(file)$size)
            stop("empty file '", file, "'")
    }
    if (!interactive())
        monitor <- FALSE
    ## FIXME: It would be better to deal with the binary file, but the format is unclear to me;
    ## FIXME: two files are available to me, and they differ considerably, neither matching the
    ## FIXME: SonTek documentation.
    if (by != 1)
        stop("must have \"by\"=1, in this version of the package")
    suffices <- c("hd1", "ts1")
    itemsPerSample <- 16
    if (missing(file))
        stop("need to supply a file, e.g. \"A\" to read \"A.hd1\" and \"A.ts1\"")
    basefile <- file
    hd <- paste(basefile, suffices[1], sep=".")
    ts <- paste(basefile, suffices[2], sep=".")

    ## The hd1 file holds per-burst information
    hdt <-  read.table(hd)
    numberOfBursts <- dim(hdt)[1]
    oceDebug(debug, "numberOfBursts: ", numberOfBursts, "\n")
    t <- ISOdatetime(year=hdt[, 2], month=hdt[, 3], day=hdt[, 4], hour=hdt[, 5], min=hdt[, 6], sec=hdt[, 7], tz=tz)
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
    ##voltage <- hdt[, 14]
    heading <- hdt[, 24]
    pitch <- hdt[, 25]
    roll <- hdt[, 26]
    ##spb <- hdt[1, 9]                      # FIXME may this change over time?
    ##sr <- spb / 3600

    tsFile <- file(ts, "rb")
    on.exit(close(tsFile))
    if (!inherits(tsFile, "connection"))
        stop("argument `tsFile' must be a character string or connection")

    ## Examine ".ts1" file to see if we can deal with it.
    seek(tsFile, where=0, origin="end")
    bytesInFile <- seek(tsFile, where=0, origin="start")
    oceDebug(debug, "length of \".", suffices[2], "\" file: ", bytesInFile, " bytes\n")
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
    ##itemsPerLine <- length(d)
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
    ts <- matrix(scan(tsFile, n=itemsPerSample * (toBurst - fromBurst + 1)*samplesPerBurst, quiet=TRUE),
                 ncol=itemsPerSample, byrow=TRUE)
    len <- dim(ts)[1]
    v <- array(numeric(), dim=c(len, 3))
    ## FIXME: the odd velocity scale is because text files use cm/s.
    velocityScale <- 1e-2
    v[, 1] <- velocityScale * ts[, 3]
    v[, 2] <- velocityScale * ts[, 4]
    v[, 3] <- velocityScale * ts[, 5]
    a <- array(raw(), dim=c(len, 3))
    a[, 1] <- as.raw(ts[, 6])
    a[, 2] <- as.raw(ts[, 7])
    a[, 3] <- as.raw(ts[, 8])
    q <- array(raw(), dim=c(len, 3))
    q[, 1] <- as.raw(ts[, 9])
    q[, 2] <- as.raw(ts[, 10])
    q[, 3] <- as.raw(ts[, 11])
    temperature <- ts[, 15]
    pressure <- ts[, 16]
    rm(ts)                              # may run tight on space
    tt <- seq(t[fromBurst], t[toBurst], length.out=len)
    ## trim to the requested interval
    ok <- (from - 1/2) <= tt & tt <= (to + 1/2) # give 1/2 second extra
    v <- v[ok, ]
    a <- a[ok, ]
    q <- q[ok, ]
    tt <- tt[ok]
    heading <- approx(t, heading, xout=tt, rule=2)$y
    pitch <- approx(t, pitch, xout=tt, rule=2)$y
    roll <- approx(t, roll, xout=tt, rule=2)$y
    res <- new("adv")
    res@data <- list(v=v, a=a, q=q,
                     time=tt,
                     heading=heading,
                     pitch=pitch,
                     roll=roll,
                     temperature=temperature,
                     pressure=pressure)
    res@metadata$manufacturer <- "sontek"
    res@metadata$instrumentType <- "adv" # FIXME or "adr"?
    res@metadata$filename <- basefile
    res@metadata$longitude <- longitude
    res@metadata$latitude <- latitude
    res@metadata$numberOfSamples <- dim(v)[1]
    res@metadata$numberOfBeams <- dim(v)[2]
    res@metadata$velocityResolution <- velocityScale/10 # FIXME: guessing on the resolution for text files
    res@metadata$velocityMaximum <- velocityScale/10 * 2^15 # FIXME: guessing on the max velocity for text files
    res@metadata$cpuSoftwareVerNum <- res@metadata$cpuSoftwareVerNum
    res@metadata$dspSoftwareVerNum <- res@metadata$dspSoftwareVerNum
    res@metadata$transformationMatrix <- if (!missing(transformationMatrix)) transformationMatrix else NULL
    res@metadata$orientation <- "upward" # FIXME: guessing on the orientation
    res@metadata$deltat <- as.numeric(difftime(tt[2], tt[1], units="secs"))
    res@metadata$subsampleStart <- data$t[1]
    res@metadata$units$v <- list(unit=expression(m/s), scale="")
    res@metadata$units$pressure <- list(unit=expression(dbar), scale="")
    res@metadata$units$heading <- list(unit=expression(degree), scale="")
    res@metadata$units$pitch <- list(unit=expression(degree), scale="")
    res@metadata$units$roll <- list(unit=expression(degree), scale="")
    res@metadata$units$temperature <- list(unit=expression(degree*C), scale="")
    res@metadata$oceCoordinate <- originalCoordinate
    res@metadata$originalCoordinate <- originalCoordinate
    warning("sensor orientation cannot be inferred without a header; \"", res@metadata$orientation, "\" was assumed.")
    if (is.null(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    hitem <- processingLogItem(processingLog)
    res@processingLog <- hitem
}
