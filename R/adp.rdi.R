## vim: tw=100 shiftwidth=4 softtabstop=4 expandtab:
## byte sequences at start of items
## FLH 00 00; VLH 00 80; vel 00 01; Cor 00 02;  echo 00 03; percent 00 04; bottom-track 00 06

decodeHeaderRDI <- function(buf, debug=getOption("oceDebug"), tz=getOption("oceTz"), ...)
{
    ## reference: WCODF = "WorkHorse Commands and Output Data Format_Nov07.pdf"

    ##
    ## header, of length 6 + 2 * numberOfDataTypes bytes
    ##
    oceDebug(debug, "decodeHeaderRDI() {\n", unindent=1)
    if (buf[1] != 0x7f || buf[2] != 0x7f)
        stop("first two bytes in file must be 0x7f 0x7f, but they are 0x", buf[1], " 0x", buf[2])
    bytesPerEnsemble <- readBin(buf[3:4], "integer", n=1, size=2, endian="little", signed=FALSE)
    oceDebug(debug, "bytesPerEnsemble=", bytesPerEnsemble, "\n")
    ## byte5 not used
    numberOfDataTypes <- readBin(buf[6], "integer", n=1, size=1)
    if (numberOfDataTypes < 1 || 200 < numberOfDataTypes)
        stop("cannot have ", numberOfDataTypes, " data types, as header indicates")
    oceDebug(debug, "numberOfDataTypes=", numberOfDataTypes, "\n")
    haveActualData <- numberOfDataTypes > 2 # will be 2 if just have headers
    oceDebug(debug, "haveActualData=", haveActualData, "\n")
    dataOffset <- readBin(buf[7+0:(2*numberOfDataTypes)], "integer", n=numberOfDataTypes, size=2, endian="little", signed=FALSE)
    oceDebug(debug, "dataOffset=", paste(dataOffset, sep=" "), "\n")
    ##
    ## Fixed Leader Data, abbreviated FLD, pointed to by the dataOffset
    FLD <- buf[dataOffset[1]+1:(dataOffset[2] - dataOffset[1])]
    oceDebug(debug, "Fixed Leader Data:", paste(FLD, collapse=" "), "\n")
    if (FLD[1] != 0x00)
        stop("first byte of fixed leader header must be 0x00 but it was ", FLD[1])
    if (FLD[2] != 0x00)
        stop("second byte of fixed leader header must be a0x00 but it was ", FLD[2])
    firmwareVersionMajor <- readBin(FLD[3], "integer", n=1, size=1, signed=FALSE)
    firmwareVersionMinor <- readBin(FLD[4], "integer", n=1, size=1, signed=FALSE)
    firmwareVersion <- paste(firmwareVersionMajor, firmwareVersionMinor, sep=".")
    firmwareVersionNumeric <- as.numeric(firmwareVersion)
    oceDebug(debug, "firmwareVersion=", firmwareVersion, "(numerically, it is", firmwareVersionNumeric,")\n")
    ##if (firmwareVersion < 16.28) warning("firmwareVersion ", firmwareVersion, " is less than 16.28, and so read.adp.rdi() may not work properly")

    if (!haveActualData)
        return(list(instrumentType="adcp",
                    firmwareVersionMajor=firmwareVersionMajor,
                    firmwareVersionMinor=firmwareVersionMinor,
                    firmwareVersion=firmwareVersion,
                    haveActualData=haveActualData))

    ## FLD[5] = SYSTEM CONFIGURATION LSB (Table 5.2, page 126, System Integrator Guide, Nov 2007)
    ## FLD[6] = SYSTEM CONFIGURATION MSB
    systemConfiguration <- paste(byteToBinary(FLD[5], endian="big"), byteToBinary(FLD[6],endian="big"),sep="-")
    oceDebug(debug, "FLD[4]=", byteToBinary(FLD[4], endian="big"), "(looking near the systemConfiguration bytes to find a problem)\n")
    oceDebug(debug, "FLD[5]=", byteToBinary(FLD[5], endian="big"), "(should be one of the systemConfiguration bytes)\n")
    oceDebug(debug, "FLD[6]=", byteToBinary(FLD[6], endian="big"), "(should be one of the systemConfiguration bytes)\n")
    oceDebug(debug, "FLD[7]=", byteToBinary(FLD[7], endian="big"), "(looking near the systemConfiguration bytes to find a problem)\n")
    bits <- substr(systemConfiguration, 6, 8)
    ## NOTE: the nearby code should perhaps use .Call("get_bit", ...) for speed and clarity
    if (bits == "000") frequency <- 75        # kHz
    else if (bits == "001") frequency <-  150
    else if (bits == "010") frequency <-  300
    else if (bits == "011") frequency <-  600
    else if (bits == "100") frequency <- 1200
    else if (bits == "101") frequency <- 2400
    else stop("unknown freq. bit code:", bits, " (expect 000 for 75kHz, 001 for 150kHz, etc)")
    oceDebug(debug, "bits:", bits, "so frequency=", frequency, "\n")
    bits <- substr(systemConfiguration, 16, 17)
    oceDebug(debug, "systemConfiguration:", systemConfiguration,"\n")
    oceDebug(debug, "bits:", bits, "00 is 15deg, 01 is 20deg, 02 is 30deg, 11 is 'other'\n")
    if (bits == "00") beamAngle <- 15
    else if (bits == "01") beamAngle <- 20
    else if (bits == "10") beamAngle <- 30
    else if (bits == "11") beamAngle <- NA # means 'other'
    oceDebug(debug, "bits=", bits, "so beamAngle=", beamAngle, "\n")
    if (beamAngle < 19 || 21 < beamAngle)
        warning("expecting a beamAngle of 20 deg [more-or-less standard for RDI] but got ", beamAngle, "deg; using the latter in the transformationMatrix")
    bits <- substr(systemConfiguration, 5, 5)
    if (bits == "0") beamPattern <- "concave"
    else beamPattern <- "convex"
    oceDebug(debug, "bits=", bits, "so beamPattern=", beamPattern, "\n")
    beamConfig <- "?"
    bits <- substr(systemConfiguration, 10, 13)
    if (bits == "0100") beamConfig <- "janus"
    else if (bits == "0101") beamConfig <- "janus demod"
    else if (bits == "1111") beamConfig <- "janus 2 demd"
    bits <- substr(systemConfiguration, 1, 1)
    if (bits == "1") orientation <- "upward"
    else orientation <- "downward"
    oceDebug(debug, "bits=", bits, "so that orientation=", orientation, "\n")

    real.sim.flag <- readBin(FLD[7], "integer", n=1, size=1)
    lagLength <- readBin(FLD[8], "integer", n=1, size=1, signed=FALSE) # unused
    numberOfBeams <- readBin(FLD[9], "integer", n=1, size=1, signed=FALSE)
    oceDebug(debug, "numberOfBeams", numberOfBeams, "\n")
    numberOfCells <- abs(readBin(FLD[10], "integer", n=1, size=1, signed=FALSE)) # WN
    oceDebug(debug, "numberOfCells", numberOfCells, "\n")
    pingsPerEnsemble <- readBin(FLD[11:12], "integer", n=1, size=2, endian="little")
    cellSize <- readBin(FLD[13:14], "integer", n=1, size=2, endian="little") / 100 # WS in m
    if (cellSize < 0 || cellSize > 64)
        stop("cellSize of ", cellSize, "m is not in the allowed range of 0m to 64m")
    blank.after.transmit <- readBin(FLD[15:16], "integer", n=1, size=2, endian="little") / 100 # in m
    profilingMode <- readBin(FLD[17], "integer", n=1, size=1) # WM
    lowCorrThresh <- readBin(FLD[18], "integer", n=1, size=1)
    numberOfCodeReps <- readBin(FLD[19], "integer", n=1, size=1)
    percentGdMinimum <- readBin(FLD[20], "integer", n=1, size=1)
    errorVelocityMaximum <- readBin(FLD[21:22], "integer", n=1, size=2, endian="little")
    tpp.minutes <- readBin(FLD[23], "integer", n=1, size=1)
    tpp.seconds <- readBin(FLD[24], "integer", n=1, size=1)
    tpp.hundredths <- readBin(FLD[25], "integer", n=1, size=1)
    bits <- substr(byteToBinary(FLD[26], endian="big"), 4, 5)
    originalCoordinate <- "???"
    if (bits == "00") originalCoordinate <- "beam"
    else if (bits == "01") originalCoordinate <- "instrument"
    else if (bits == "10") originalCoordinate <- "xyz"
    else if (bits == "11") originalCoordinate <- "enu"
    headingAlignment <- 0.01 * readBin(FLD[27:28], "integer", n=1, size=2, endian="little") # WCODF p 130
    headingBias <- 0.01 * readBin(FLD[29:30], "integer", n=1, size=2, endian="little") # WCODF p 130
    oceDebug(debug, "headingAlignment=", headingAlignment, "; headingBias=", headingBias, "\n")
    sensorSource <- byteToBinary(FLD[31], endian="big")
    sensorsAvailable<- byteToBinary(FLD[32], endian="big")
    bin1Distance <- readBin(FLD[33:34], "integer", n=1, size=2, endian="little", signed=FALSE) * 0.01
    ##cat("bin1Distance being inferred from 0x", FLD[33:34], " as ", bin1Distance, "\n", sep="", ...)
    xmitPulseLength <- readBin(FLD[35:36], "integer", n=1, size=2, endian="little", signed=FALSE) * 0.01
    ##cat("xmitPulseLength being inferred from 0x", FLD[35:36], " as ", xmitPulseLength, "\n", sep="", ...)
    wpRefLayerAverage <- readBin(FLD[37:38], "integer", n=1, size=2, endian="little")
    falseTargetThresh <- readBin(FLD[39], "integer", n=1, size=1)
    ## FLD[40] spare
    transmitLagDistance <- readBin(FLD[41:42], "integer", n=1, size=2, endian="little", signed=FALSE)
    cpuBoardSerialNumber <- c(readBin(FLD[43], "integer", n=1, size=1, signed=FALSE),
                              readBin(FLD[44], "integer", n=1, size=1, signed=FALSE),
                              readBin(FLD[45], "integer", n=1, size=1, signed=FALSE),
                              readBin(FLD[46], "integer", n=1, size=1, signed=FALSE),
                              readBin(FLD[47], "integer", n=1, size=1, signed=FALSE),
                              readBin(FLD[48], "integer", n=1, size=1, signed=FALSE),
                              readBin(FLD[49], "integer", n=1, size=1, signed=FALSE),
                              readBin(FLD[50], "integer", n=1, size=1, signed=FALSE))
    oceDebug(debug, paste("CPU.BOARD.SERIAL.NUMBER = '", paste(cpuBoardSerialNumber, collapse=""), "'\n", sep=""))
    systemBandwidth <- readBin(FLD[51:52], "integer", n=1, size=2, endian="little")
    systemPower <- readBin(FLD[53], "integer", n=1, size=1)
    ## FLD[54] spare
    ## "WorkHorse Commands and Output Data Format_Mar05.pdf" p130: bytes 55:58 = serialNumber only for REMUS, else spare
    ## "WorkHorse Commands and Output Data Format_Nov07.pdf" p127: bytes 55:58 = serialNumber
    serialNumber <- readBin(FLD[55:58], "integer", n=1, size=4, endian="little")
    oceDebug(debug, "SERIAL NUMBER", serialNumber, "from bytes (", FLD[55:58], ")\n")
    if (serialNumber == 0)
        serialNumber <- "unknown"
    ##beamAngle <- readBin(FLD[59], "integer", n=1, size=1) # NB 0 in first test case
    ##cat("BEAM ANGLE=", FLD[59], "or", beamAngle, "\n", ...)
    ##
    ## VLD (variable leader data)
    ##   The VLD length varies (see below) so infer its position from dataOffset[1].
    ##
    ## "WorkHorse Commands and Output Data Format_Mar05.pdf" (and Nov07 version) Figure 9 on page 122 (pdf-page 130):
    ##       HEADER (6+2*num.types bytes) bytes
    ##       FLD 59 bytes
    ##       VLD 65 bytes
    ## "Ocean Surveyor Technical Manual.pdf" table D-3 on page D-5 (pdf-page 139):
    ##       HEADER (6+2*num.types bytes) bytes
    ##       FLD 50 bytes
    ##       VLD 58 bytes
    ## dataOffset[1] = within-ensemble byte offset for FLD (e.g. Table D-1 of Surveyor manual)
    ## dataOffset[2] = within-ensemble byte offset for VLD (e.g. Table D-1 of Surveyor manual)
    ## thus, length of FLD is dataOffset[2]-dataOffset[1]
    FLDLength <- dataOffset[2] - dataOffset[1]
    oceDebug(debug, "FLDLength", FLDLength, " (expect 59 for Workhorse, or 40 for Surveyor)\n")
    ## There really seems to be nothing specific in the file to tell instrument type, so, in an act of
    ## desparation (or is that hope) I'm going to flag on the one thing that was clearly stated, and
    ## clearly different, in the two documentation entries.
    if (FLDLength == 59) {
        instrumentSubtype <- "workhorse" # "WorkHorse Commands and Output Data Format_Mar05.pdf" (and Nov07 version) Figure 9 on page 122 (pdf-page 130)
    } else if (FLDLength == 50) {
        instrumentSubtype <- "surveyor" # "Ocean Surveyor Technical Manual.pdf" table D-3 on page D-5 (pdf-page 139)
    } else {
        instrumentSubtype <- "unknown"
        warning("unexpected length ", FLDLength, " of fixed-leader-data header; expecting 50 for 'surveyor' or 59 for 'workhorse'.")
    }
    nVLD <- 65 # FIXME: should use the proper length, but we won't use it all anyway
    VLD <- buf[dataOffset[2]+1:nVLD]
    oceDebug(debug, "Variable Leader Data (", length(VLD), "bytes):", paste(VLD, collapse=" "), "\n")
    ## ensure that header is not ill-formed
    if (VLD[1] != 0x80)
        stop("byte 1 of variable leader data should be 0x80, but it is ", VLD[1])
    if (VLD[2] != 0x00)
        stop("byte 2 of variable leader data should be 0x00, but it is ", VLD[2])
    ensemble.number <- readBin(VLD[3:4], "integer", n=1, size=2, endian="little")
    ## Assemble the time.  This follows section 5.3 (paper 132, file page 140) of "Workhorse Commands and Output Data Format_Nov07.pdf"

    ## FIXME: probably would save time to read all elements at once.  Instrument to check
    RTC.year <- unabbreviateYear(readBin(VLD[5], "integer", n=1, size=1))
    RTC.month <- readBin(VLD[6], "integer", n=1, size=1)
    RTC.day <- readBin(VLD[7], "integer", n=1, size=1)
    RTC.hour <- readBin(VLD[8], "integer", n=1, size=1)
    RTC.minute <- readBin(VLD[9], "integer", n=1, size=1)
    RTC.second <- readBin(VLD[10], "integer", n=1, size=1)
    RTC.hundredths <- readBin(VLD[11], "integer", n=1, size=1)
    time <- ISOdatetime(RTC.year, RTC.month, RTC.day, RTC.hour, RTC.minute, RTC.second + RTC.hundredths / 100, tz=tz)
    oceDebug(debug, "profile time=", format(time), "(year=", RTC.year,
              "month=", RTC.month, "day-", RTC.day, "hour=", RTC.hour,
              "minute=", RTC.minute, "second=", RTC.second, "hundreds=", RTC.hundredths, ")\n")
    ensembleNumberMSB <- readBin(VLD[12], "integer", n=1, size=1)
    bitResult <- readBin(VLD[13:14], "integer", n=1, size=2, endian="little")
    soundSpeed <- readBin(VLD[15:16], "integer", n=1, size=2, endian="little")
    oceDebug(debug, "soundSpeed= ", soundSpeed, "\n") # FIXME possibly wrong
    transducerDepth <- readBin(VLD[17:18], "integer", n=1, size=2, endian="little")
    oceDebug(debug, "transducerDepth = ", transducerDepth, "\n")
    if (soundSpeed < 1400 || soundSpeed > 1600)
        warning("soundSpeed is ", soundSpeed, ", which is outside the permitted range of 1400 m/s to
                1600 m/s.  Something went wrong in decoding the data.")
    oceDebug(debug, "about to create the list to be returned\n")
    list(instrumentType="adcp",
         instrumentSubtype=instrumentSubtype,
         firmwareVersionMajor=firmwareVersionMajor,
         firmwareVersionMinor=firmwareVersionMinor,
         firmwareVersion=firmwareVersion,
         ##firmwareVersionMajor=fv,
         ##programVersionMinor=fr,
         bytesPerEnsemble=bytesPerEnsemble,
         systemConfiguration=systemConfiguration,
         frequency=frequency,
         beamAngle=beamAngle,
         beamPattern=beamPattern,
         beamConfig=beamConfig,
         orientation=orientation,
         numberOfDataTypes=numberOfDataTypes,
         dataOffset=dataOffset,
         numberOfBeams=numberOfBeams,
         numberOfCells=numberOfCells,
         pingsPerEnsemble=pingsPerEnsemble,
         cellSize=cellSize,
         transducerDepth=transducerDepth,
         profilingMode=profilingMode,
         dataOffset=dataOffset,
         lowCorrThresh=lowCorrThresh,
         numberOfCodeReps=numberOfCodeReps,
         percentGdMinimum=percentGdMinimum,
         errorVelocityMaximum=errorVelocityMaximum,
         ##tpp.minutes=tpp.minutes,
         ##tpp.seconds=tpp.seconds,
         ##tpp.hundredths=tpp.hundredths,
         originalCoordinate=originalCoordinate,
         headingAlignment=headingAlignment,
         headingBias=headingBias,
         sensorSource=sensorSource,
         sensorsAvailable=sensorsAvailable,
         bin1Distance=bin1Distance,
         xmitPulseLength=xmitPulseLength,
         wpRefLayerAverage=wpRefLayerAverage,
         falseTargetThresh=falseTargetThresh,
         transmitLagDistance=transmitLagDistance,
         cpuBoardSerialNumber=cpuBoardSerialNumber,
         systemBandwidth=systemBandwidth,
         ##systemPower=systemPower,
         serialNumber=serialNumber,
         ## beamAngle=beamAngle,  # wrong in my tests, anyway
         ##ensemble.number=ensemble.number,
         ##time=time,
         ##ensembleNumberMSB=ensembleNumberMSB,
         ##bitResult=bitResult,
         ##heading=heading,
         ##pitch=pitch,
         ##roll=roll,
         ##salinity=salinity
         ##headingAlignment,
         ##headingBias,
         haveActualData=haveActualData)
}                                       # decodeHeaderRDI

read.adp.rdi <- function(file, from=1, to, by=1, tz=getOption("oceTz"),
                         longitude=NA, latitude=NA,
                         type=c("workhorse"),
                         monitor=FALSE, despike=FALSE, processingLog,
                         testing=FALSE,
                         debug=getOption("oceDebug"),
                         ...)
{
    profileStart <- NULL # prevent scope warning from rstudio; defined later anyway
    bisectAdpRdi <- function(t.find, add=0, debug=0) {
        oceDebug(debug, "bisectAdpRdi(t.find=", format(t.find), ", add=", add, "\n")
        len <- length(profileStart)
        lower <- 1
        upper <- len
        passes <- floor(10 + log(len, 2)) # won't need this many; only do this to catch coding errors
        for (pass in 1:passes) {
            middle <- floor((upper + lower) / 2)
            year   <- unabbreviateYear(readBin(buf[profileStart[middle] +  4], what="integer", n=1, size=1, signed=FALSE))
            month  <- readBin(buf[profileStart[middle] +  5], what="integer", n=1, size=1, signed=FALSE)
            day    <- readBin(buf[profileStart[middle] +  6], what="integer", n=1, size=1, signed=FALSE)
            hour   <- readBin(buf[profileStart[middle] +  7], what="integer", n=1, size=1, signed=FALSE)
            minute <- readBin(buf[profileStart[middle] +  8], what="integer", n=1, size=1, signed=FALSE)
            second <- readBin(buf[profileStart[middle] +  9], what="integer", n=1, size=1, signed=FALSE)
            sec100 <- readBin(buf[profileStart[middle] + 10], what="integer", n=1, size=1, signed=FALSE)
            t <- ISOdatetime(year, month, day, hour, minute, second + sec100/100, tz=tz)
            oceDebug(debug, "t=", format(t), "| y=", year, " m=", month, " d=", format(day, width=2), " h=", format(hour, width=2), " m=", format(minute, width=2), "s=", format(second, width=2), "sec100=", sec100, "| pass", format(pass, width=2), "/", passes, "| middle=", middle, "(", format(middle/upper*100,digits=4), "%)\n")
            if (t.find < t)
                upper <- middle
            else
                lower <- middle
            if (upper - lower < 2)
                break
        }
        middle <- middle + add          # may use add to extend before and after window
        if (middle < 1)
            middle <- 1
        if (middle > len)
            middle <- len
        t <- ISOdatetime(unabbreviateYear(readBin(buf[profileStart[middle]+4],"integer",size=1,signed=FALSE,endian="little")),
                         as.integer(buf[profileStart[middle]+5]), # month
                         as.integer(buf[profileStart[middle]+6]), # day
                         as.integer(buf[profileStart[middle]+7]), # hour
                         as.integer(buf[profileStart[middle]+8]), # min
                         as.integer(buf[profileStart[middle]+9])+0.01*as.integer(buf[profileStart[middle]+10]), # decimal second
                         tz=tz)
        oceDebug(debug, "result: t=", format(t), " at vsdStart[", middle, "]=", profileStart[middle], "\n")
        return(list(index=middle, time=t))
    }
    gaveFromTo <- !missing(from) && !missing(to)
    if (gaveFromTo) {
        oceDebug(debug, "read.adp.rdi(...,from=",format(from),",to=",format(to), "...)\n")
        oceDebug(debug, "class(from)=", class(from), "; class(to)=", class(to), "\n")
    }
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

    ## Read whole file into 'buf'
    seek(file, 0, "start")
    seek(file, where=0, origin="end")
    file.size <- seek(file, where=0)
    oceDebug(debug, "file.size=", file.size, "\n")
    buf <- readBin(file, what="raw", n=file.size, endian="little")
    ## decode header
    header <- decodeHeaderRDI(buf, debug=debug-1)

    if (header$haveActualData) {
        numberOfBeams <- header$numberOfBeams
        numberOfCells <- header$numberOfCells
        bin1Distance <- header$bin1Distance
        xmitPulseLength <- header$xmitPulseLength
        cellSize <- header$cellSize
        oceDebug(debug, "about to call ldc_rdi\n")
        ensembleStart <- .Call("ldc_rdi", buf, 0) # point at bytes (7f 7f)
        oceDebug(debug, "successfully called ldc_rdi\n")

        profileStart <- ensembleStart + as.numeric(buf[ensembleStart[1]+8]) + 256*as.numeric(buf[ensembleStart[1]+9])
        # offset for data type 1 (velocity)
        oceDebug(debug, vectorShow(profileStart, "profileStart before trimming:"))
        profilesInFile <- length(profileStart)
        oceDebug(debug, "profilesInFile=", profilesInFile, "(as inferred by a byte-check on the sequence 0x80, 0x00)\n")
        if (!gaveFromTo) {             # read whole file if 'from' and 'to' not given
            from <- 1
            to <- profilesInFile
        }
        if (profilesInFile > 0)  {
            measurementStart <- ISOdatetime(unabbreviateYear(as.integer(buf[profileStart[1]+4])),
                                            as.integer(buf[profileStart[1]+5]), # month
                                            as.integer(buf[profileStart[1]+6]), # day
                                            as.integer(buf[profileStart[1]+7]), # hour
                                            as.integer(buf[profileStart[1]+8]), # min
                                            as.integer(buf[profileStart[1]+9]), # sec
                                            tz=tz)
            oceDebug(debug, "measurementStart:", format(measurementStart), "\n")
            measurementEnd <- ISOdatetime(unabbreviateYear(as.integer(buf[profileStart[profilesInFile]+4])),
                                          as.integer(buf[profileStart[profilesInFile]+5]), # month
                                          as.integer(buf[profileStart[profilesInFile]+6]), # day
                                          as.integer(buf[profileStart[profilesInFile]+7]), # hour
                                          as.integer(buf[profileStart[profilesInFile]+8]), # min
                                          as.integer(buf[profileStart[profilesInFile]+9]), # sec
                                          tz=tz)
            oceDebug(debug, "measurementEnd:", format(measurementEnd), "\n")
            ## FIXME: assumes uniform time interval (ok, but document it)
            measurementDeltat <- as.numeric(ISOdatetime(unabbreviateYear(as.integer(buf[profileStart[2]+4])),
                                                         as.integer(buf[profileStart[2]+5]), # month
                                                         as.integer(buf[profileStart[2]+6]), # day
                                                         as.integer(buf[profileStart[2]+7]), # hour
                                                         as.integer(buf[profileStart[2]+8]), # min
                                                         as.integer(buf[profileStart[2]+9]), # sec
                                                         tz=tz)) - as.numeric(measurementStart)
            oceDebug(debug, "measurementDeltat:", measurementDeltat, "\n")
            if (inherits(from, "POSIXt")) {
                if (!inherits(to, "POSIXt"))
                    stop("if 'from' is POSIXt, then 'to' must be, also")
                fromPair <- bisectAdpRdi(from, add=-1, debug=debug-1)
                from <- fromIndex <- fromPair$index
                toPair <- bisectAdpRdi(to, add=1, debug=debug-1)
                to <- toIndex <- toPair$index
                oceDebug(debug, "from=", format(fromPair$t), " yields profileStart[", fromIndex, "]\n",
                          "  to  =", format(toPair$t), "yields profileStart[", toIndex, "]\n",
                          "  by=", by, "(not yet decoded)\n",
                          vectorShow(profileStart, "profileStart*:"),
                                        #"  profileStart[1:10]=", profileStart[1:10],"\n",
                          "  profileStart[",fromPair$index, "]=", profileStart[fromPair$index], "at time", format(fromPair$t), "\n",
                          "  profileStart[",  toPair$index, "]=", profileStart[  toPair$index], "at time", format(  toPair$t), "\n")
                dt <- measurementDeltat
                oceDebug(debug, "dt=", dt, "s; at this stage, by=", by,"\n")
                if (is.character(by))
                    by <- floor(0.5 + ctimeToSeconds(by) / dt)
                oceDebug(debug, "by=",by,"profiles (after decoding)\n")
                profileStart <- profileStart[profileStart[fromIndex] < profileStart & profileStart < profileStart[toIndex]]
                ensembleStart <- ensembleStart[ensembleStart[fromIndex] < ensembleStart & ensembleStart < ensembleStart[toIndex]]
                profileStart <- profileStart[seq(1, length(profileStart), by=by)]
                ensembleStart <- ensembleStart[seq(1, length(ensembleStart), by=by)]
            } else {
                fromIndex <- from
                toIndex <- to
                if (toIndex < fromIndex)
                    stop("need more separation between from and to")
                if (is.character(by))
                    stop("cannot have string for 'by' if 'from' and 'to' are integers")
                profileStart <- profileStart[seq(from=from, to=to, by=by)]
                ensembleStart <- ensembleStart[seq(from=from, to=to, by=by)]
                oceDebug(debug, vectorShow(profileStart, "profileStart after indexing:"))
            }
            profileStart <- profileStart[!is.na(profileStart)]
            ensembleStart <- ensembleStart[!is.na(ensembleStart)]
            profilesToRead <- length(profileStart)
            oceDebug(debug, "filename=",filename,"\n")
            oceDebug(debug, "profilesToRead=",profilesToRead,"\n")
            oceDebug(debug, "numberOfBeams=",numberOfBeams,"\n")
            oceDebug(debug, "numberOfCells=",numberOfCells,"\n")

            if (testing) {
                nensembles <- length(ensembleStart)
                numberOfDataTypes <- readBin(buf[ensembleStart[1] + 5], "integer", n=1, size=1) # Note: just using first one
                FLDStart <- ensembleStart + 6 + 2 * numberOfDataTypes
                ## FIXME: decide whether the code below is cleaner than the spot where time is determined
                ## VLDStart <- FLDStart + 59
                ## RTC.year <- unabbreviateYear(readBin(buf[VLDStart+4], "integer", n=nensembles, size=1))
                ## RTC.month <- readBin(buf[VLDStart+5], "integer", n=nensembles, size=1)
                ## RTC.day <- readBin(buf[VLDStart+6], "integer", n=nensembles, size=1)
                ## RTC.hour <- readBin(buf[VLDStart+7], "integer", n=nensembles, size=1)
                ## RTC.minute <- readBin(buf[VLDStart+8], "integer", n=nensembles, size=1)
                ## RTC.second <- readBin(buf[VLDStart+9], "integer", n=nensembles, size=1)
                ## RTC.hundredths <- readBin(buf[VLDStart+10], "integer", n=nensembles, size=1)
                ## time <- ISOdatetime(RTC.year, RTC.month, RTC.day, RTC.hour, RTC.minute, RTC.second + RTC.hundredths / 100, tz=tz)

                ## regarding the "4" below, see p 135 of WorkHorse_commands_data_format_AUG10.PDF,
                ## noting that we subtract 1 because it's an offset; we are thus examining
                ## the LSB of the "Sys Cfg" pair.
                upward <- .Call("get_bit", buf[FLDStart+4], 7)
                ##testingData <- list(time=time, upward=upward)
            }

            items <- numberOfBeams * numberOfCells
            v <- array(numeric(), dim=c(profilesToRead, numberOfCells, numberOfBeams))
            a <- array(raw(), dim=c(profilesToRead, numberOfCells, numberOfBeams)) # echo amplitude
            q <- array(raw(), dim=c(profilesToRead, numberOfCells, numberOfBeams)) # correlation
            g <- array(raw(), dim=c(profilesToRead, numberOfCells, numberOfBeams)) # percent good
            badProfiles <- NULL
            oceDebug(debug, "did allocation; dim(v)=", dim(v), "\n")
            haveBottomTrack <- FALSE          # FIXME maybe we can determine this from the header
            oceDebug(debug, "length(profileStart) = ", length(profileStart), "\n")
            if (profilesToRead < 1)
                stop("no profilesToRead")
            velocityScale <- 1e-3

            ## Next line sets up empty vectors for VMDAS
            isVMDAS <- FALSE # see below where we check bytes in first profile
            badVMDAS <- NULL           # erroneous VMDAS profiles
            for (i in 1:profilesToRead) {     # recall: these start at 0x80 0x00
                o <- profileStart[i] + header$dataOffset[3] - header$dataOffset[2] # 65 for workhorse; 50 for surveyor
                ##oceDebug(debug, "chunk", i, "at byte", o, "; next 2 bytes are", as.raw(buf[o]), " and ", as.raw(buf[o+1]), " (expecting 0x00 and 0x01 for velocity)\n")
                if (buf[o] == 0x00 && buf[o+1] == 0x01) { # velocity
                    ##cat(vectorShow(buf[o + 1 + seq(1, 2*items)], "buf[...]:"))
                    vv <- readBin(buf[o + 1 + seq(1, 2*items)], "integer", n=items, size=2, endian="little", signed=TRUE)
                    ##cat(vectorShow(vv, "vv:"))
                    vv[vv==(-32768)] <- NA       # blank out bad data
                    v[i,,] <- matrix(velocityScale * vv, ncol=numberOfBeams, byrow=TRUE)
                    ##cat(vectorShow(v[i,,], "v:"))
                    o <- o + items * 2 + 2 # skip over the velo data, plus a checksum; FIXME: use the checksum
                    if (buf[o] != 0x00)
                        warning("first byte of correlation segment should be 0x00 but is ", buf[o], " at file position ", o)
                    if (buf[o+1] != 0x02)
                        warning("second byte of correlation segment should be 0x02 but is ", buf[o+1], " at file position ", o+1)
                    ##oceDebug(debug-1, "'q' (correlation) chunk at byte", o, "\n")
                    q[i,,] <- matrix(buf[o + 1 + seq(1, items)], ncol=numberOfBeams, byrow=TRUE)
                    ##cat(vectorShow(q[i,,], "q:"))
                    o <- o + items + 2              # skip over the one-byte data plus a checkum; FIXME: use the checksum
                    if (buf[o] != 0x00)
                        warning("first byte of intensity segment should be 0x00 but is ", buf[o], " at file position ", o)
                    if (buf[o+1] != 0x03)
                        warning("second byte of intensity segment should be 0x03 but is ", buf[o+1], " at file position ", o+1)
                    ##oceDebug(debug-1, "'a' (amplitude) chunk at byte", o, "\n")
                    a[i,,] <- matrix(buf[o + 1 + seq(1, items)], ncol=numberOfBeams, byrow=TRUE)
                    ##cat(vectorShow(a[i,,], "a:"))
                    o <- o + items + 2              # skip over the one-byte data plus a checkum; FIXME: use the checksum
                    if (buf[o] != 0x00)
                        warning("first byte of percent-good segment should be 0x00 but is ", buf[o], " at file position ", o)
                    if (buf[o+1] != 0x04)
                        warning("second byte of percent-good segment should be 0x04 but is ", buf[o+1], " at file position ", o+1)
                    ##oceDebug(debug-1, "'g' (percent good) chunk at byte", o, "\n")
                    g[i,,] <- matrix(buf[o + 1 + seq(1, items)], ncol=numberOfBeams, byrow=TRUE) # FIXME: not using this
                    ##cat(vectorShow(g[i,,], "g:"))
                    o <- o + items + 2              # skip over the one-byte data plus a checkum; FIXME: use the checksum
                    ##oceDebug(debug-1, "next (", o+1, "th) byte is", buf[o+1], "(expect 01 for velo or 06 for bottom track)\n")
                    if (buf[o+1] == 0x06) {
                        ##oceDebug(debug-1, "bottom track (range and velocity) chunk at byte", o, "\n")
                        ## It seems that spurious bottom-track records might occur sometimes,
                        ## and the following tries to prevent that by insisting that bottom
                        ## track data occur in the first profile, if they occur later; otherwise
                        ## this flag will be ignored.
                        if (i == 1 && !haveBottomTrack) {
                            if (numberOfBeams != 4) {
                                stop("expecting 4 beams, for this RDI adcp")
                            }
                            br <- array(double(), dim=c(profilesToRead, numberOfBeams))
                            bv <- array(double(), dim=c(profilesToRead, numberOfBeams))
                            bc <- array(double(), dim=c(profilesToRead, numberOfBeams)) # correlation
                            ba <- array(double(), dim=c(profilesToRead, numberOfBeams)) # amplitude
                            bg <- array(double(), dim=c(profilesToRead, numberOfBeams)) # percent good
                            haveBottomTrack <- TRUE
                        }
                        if (haveBottomTrack) {
                            ## the bottom range is in 3 bytes, split into two chunks
                            rangeLSB <- readBin(buf[o+c(16:23)], "integer",
                                                n=4, size=2, signed=FALSE, endian="little")
                            rangeMSB <- readBin(buf[o+77:80], "integer",
                                                n=4, size=1, signed=FALSE, endian="little")
                            br[i,] <- 0.01 * (65536 * rangeMSB + rangeLSB)
                            bv[i,] <- 0.001 * readBin(buf[o+c(24:31)], "integer",
                                                      n=4, size=2, signed=TRUE, endian="little")
                            bc[i,] <- as.integer(buf[o+32:35])
                            ba[i,] <- as.integer(buf[o+36:39])
                            bg[i,] <- as.integer(buf[o+40:43])
                        }
                    }
                    if (buf[o + 85] == 0x00 && buf[o+85+1] == 0x20) { # VMDAS
                        ## This must be in the first profile, or we won't call this a VMDAS file.
                        if (i == 1) {
                            oceDebug(debug, "This is a VMDAS file\n")
                            isVMDAS <- TRUE
                            navTime <- slongitude <- slatitude <- elatitude <- elongitude <- NULL                      
                        } else {
                            if (!isVMDAS)
                                badVMDAS <- c(badVMDAS, i)
                        }
                        if (isVMDAS) {
                            o <- o + 85
                            tmpTime <- ISOdatetime(as.integer(buf[o+4]) + 256*as.integer(buf[o+5]), #year
                                                   as.integer(buf[o+3]), #month
                                                   as.integer(buf[o+2]), #day
                                                   0, 0, 0,
                                                   tz=tz)
                            sNavTime <- tmpTime + readBin(buf[o+6:9], 'integer', n=4, size=4, endian='little')/10000
                            cfac <- 180/2^31 # from rdradcp.m line 825
                            slatitude <- c(slatitude, readBin(buf[o+14:17], 'integer', n=4, size=4, endian='little')*cfac)
                            slongitude <- c(slongitude, readBin(buf[o+18:21], 'integer', n=4, size=4, endian='little')*cfac)
                            eNavTime <- tmpTime + readBin(buf[o+22:25], 'integer', n=4, size=4, endian='little')/10000
                            elatitude <- c(elatitude, readBin(buf[o+26:29], 'integer', n=4, size=4, endian='little')*cfac)
                            elongitude <- c(elongitude, readBin(buf[o+30:33], 'integer', n=4, size=4, endian='little')*cfac)
                            tmpTime <- ISOdatetime(as.integer(buf[o+54]) + 256*as.integer(buf[o+55]), #year
                                                   as.integer(buf[o+57]), #month
                                                   as.integer(buf[o+56]), #day
                                                   0, 0, 0,
                                                   tz=tz)
                            navTime <- c(navTime, tmpTime + readBin(buf[o+58:61], 'integer', n=4, size=4, endian='little')/100)
                            navTime <- as.POSIXct(navTime, origin='1970-01-01', tz=tz)
                        }
                    }
                    if (monitor) {
                        cat(".", ...)
                        if (!(i %% 50))
                            cat(i, "\n", ...)
                    }
                } else {
                    badProfiles <- c(badProfiles, i)
                    if (monitor) {
                        cat("X", ...)
                        if (!(i %% 50))
                            cat(i, "\n", ...)
                    }
                }
                if (o >= file.size) {
                    warning("got to end of file")
                    break
                }
            }
            time <- ISOdatetime(unabbreviateYear(as.integer(buf[profileStart+4])), # year
                                as.integer(buf[profileStart+5]),      # month
                                as.integer(buf[profileStart+6]),      # day
                                as.integer(buf[profileStart+7]),      # hour
                                as.integer(buf[profileStart+8]),      # minute
                                as.integer(buf[profileStart+9])+0.01*as.integer(buf[profileStart+10]), # decimal second
                                tz=tz)


            if (length(badProfiles) > 0) { # remove NAs in time (not sure this is right, but it prevents other problems)
                t0 <- time[match(1, !is.na(time))] # FIXME: should test if any
                time <- fillGap(as.numeric(time) - as.numeric(t0)) + t0
                nbad <- length(badProfiles)
                if (nbad == 1)
                    warning("Interpolated across a bad profile at time ", format(time[badProfiles]), ".  (\"Bad\" means that the expected byte code for a velocity segment, 0x00 0x01, was not found 65 bytes after the start of a profile, the latter indicated by the byte sequence 0x80 0x00.)\n")
                else
                    warning("Interpolated across ", length(badProfiles), " bad profile(s) at times: ", paste(format(time[badProfiles]), collapse=", "), ".  (\"Bad\" means that the expected byte code for a velocity segment, 0x00 0x01, was not found 65 bytes after the start of a profile, the latter indicated by the byte sequence 0x80 0x00.)\n")
            }

            profileStart2 <- sort(c(profileStart, profileStart + 1)) # lets us index two-byte chunks
            profileStart4 <- sort(c(profileStart, profileStart + 1, profileStart + 2, profileStart + 3)) # lets us index four-byte chunks
            soundSpeed <- readBin(buf[profileStart2 + 14], "integer", n=profilesToRead, size=2, endian="little", signed=FALSE)
            depth <- 0.1 * readBin(buf[profileStart2 + 16], "integer", n=profilesToRead, size=2, endian="little")
            ## Note that the headingBias needs to be removed
            heading <- 0.01 * readBin(buf[profileStart2 + 18], "integer", n=profilesToRead, size=2, endian="little", signed=FALSE) - header$headingBias
            oceDebug(debug, vectorShow(heading, "heading"))
            if (header$headingBias != 0)
                cat("read.adp.rdi(): subtracted a headingBias of ", header$headingBias, " degrees\n")
            pitch <- 0.01 * readBin(buf[profileStart2 + 20], "integer", n=profilesToRead, size=2, endian="little", signed=TRUE)
            oceDebug(debug, vectorShow(profileStart2, "profileStart2"))
            oceDebug(debug, vectorShow(pitch, "pitch"))
            roll <- 0.01 * readBin(buf[profileStart2 + 22], "integer", n=profilesToRead, size=2, endian="little", signed=TRUE)
            oceDebug(debug, vectorShow(roll, "roll"))
            ##tmp <- pitch
            oceDebug(debug, vectorShow(pitch, "pitch, before correction as on p14 of 'adcp coordinate transformation.pdf'"))
            pitch <- 180 / pi * atan(tan(pitch * pi / 180) / cos(roll * pi / 180)) # correct the pitch (see ACT page 14)
            oceDebug(debug, vectorShow(pitch, "pitch, correction"))
            ##oceDebug(debug, "RMS change in pitch:", sqrt(mean((pitch - tmp)^2, na.rm=TRUE)), "\n")
            ##rm(tmp)
            salinity <- readBin(buf[profileStart2 + 24], "integer", n=profilesToRead, size=2, endian="little", signed=TRUE)
            temperature <- 0.01 * readBin(buf[profileStart2 + 26], "integer", n=profilesToRead, size=2, endian="little", signed=TRUE)
            pressure <- 0.001 * readBin(buf[profileStart4 + 48], "integer", n=profilesToRead, size=4, endian="little")
            if (despike) {
                temperature <- despike(temperature, reference="trim", min=-3, max=101)
                pressure <- despike(pressure, reference="trim", min=1, max=10000)
            }
            headingStd <- as.numeric(buf[profileStart + 31]) # p142 WorkHorse_commands_data_format_AUG10.PDF
            pitchStd <- 0.1 * as.numeric(buf[profileStart + 32])
            rollStd <- 0.1 * as.numeric(buf[profileStart + 33])

            ## read ADC channels [CR's code]
            xmitCurrent <- as.numeric(buf[profileStart + 34])
            xmitVoltage <- as.numeric(buf[profileStart + 35])
            ambientTemp <- as.numeric(buf[profileStart + 36])
            pressurePlus <- as.numeric(buf[profileStart + 37])
            pressureMinus <- as.numeric(buf[profileStart + 38])
            attitudeTemp <- as.numeric(buf[profileStart + 39])
            attitude <- as.numeric(buf[profileStart + 40])
            contaminationSensor <- as.numeric(buf[profileStart + 41])

            pressureStd <- readBin(buf[profileStart4 + 52], "integer", n=profilesToRead, size=4, endian="little")
            oceDebug(debug, vectorShow(temperature, "temperature"))
            oceDebug(debug, vectorShow(pressure, "pressure"))
            metadata <- header
            metadata$manufacturer <- "rdi"
            metadata$instrumentType <- "adcp"
            metadata$filename <- filename
            metadata$longitude <- longitude
            metadata$latitude <- latitude
            metadata$velocityResolution <- velocityScale
            metadata$velocityMaximum <- velocityScale * 2^15
            metadata$numberOfSamples <- dim(v)[1]
            metadata$numberOfCells <- dim(v)[2]
            metadata$numberOfBeams <- dim(v)[3]
            metadata$measurementStart <- measurementStart
            metadata$measurementEnd <- measurementEnd
            metadata$measurementDeltat <- measurementDeltat
            metadata$bin1Distance <- bin1Distance
            metadata$xmitPulseLength <- xmitPulseLength
            metadata$oceBeamUnspreaded <- FALSE
            metadata$oceCoordinate <- header$originalCoordinate
            metadata$depthMean <- mean(depth, na.rm=TRUE)
            ## Transformation matrix
            ## FIXME Dal people use 'a' in last row of matrix, but both
            ## RDI and CODAS use as we have here.  (And I think RDI
            ## may have two definitions...)
            ##
            ## Notes on coordinate transformationMatrix.
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
            ## The matrix below is from section 5.3 of the ACT.
            ##
            ## As a check on coding, see the python software at
            ##   http://currents.soest.hawaii.edu/hg/pycurrents/file/3175207488bb/adcp/transform.py
            tm.c <- if (metadata$beamPattern == "convex") 1 else -1; # control sign of first 2 rows of transformationMatrix
            tm.a <- 1 / (2 * sin(metadata$beamAngle * pi / 180))
            tm.b <- 1 / (4 * cos(metadata$beamAngle * pi / 180))
            tm.d <- tm.a / sqrt(2)
            metadata$transformationMatrix <- matrix(c(tm.c*tm.a, -tm.c*tm.a,          0,         0,
                                                      0        ,          0, -tm.c*tm.a, tm.c*tm.a,
                                                      tm.b     ,       tm.b,       tm.b,      tm.b,
                                                      tm.d     ,       tm.d,      -tm.d,     -tm.d),
                                                    nrow=4, byrow=TRUE)
           if (monitor)
                cat("\nRead", profilesToRead,  "profiles, out of a total of",profilesInFile,"profiles in", filename, "\n", ...)

           ## Sometimes a non-VMDAS file will have some profiles that have the VMDAS flag.
           ## It is not clear why this happens, but in any case, provide a warning.
           nbadVMDAS <- length(badVMDAS)
           if (nbadVMDAS > 0) {
               if (1==nbadVMDAS) {
                   warning("erroneous VMDAS flag in profile ", badVMDAS, "\n")
               } else if (nbadVMDAS < 4) {
                   warning("erroneous VMDAS flag in profiles: ", paste(badVMDAS, collapse=" "), "\n")
               } else {
                   warning("erroneous VMDAS flag in ", nbadVMDAS, " profiles, including: ", badVMDAS[1], " ",
                           badVMDAS[2], " ... ", badVMDAS[nbadVMDAS-1], " ",
                           badVMDAS[nbadVMDAS], "\n")
               }
           }
           class(time) <- c("POSIXt", "POSIXct")
           attr(time, "tzone") <- getOption("oceTz")
           if (haveBottomTrack && !isVMDAS) {
               br[br == 0.0] <- NA    # clean up (not sure if needed)
               data <- list(v=v, q=q, a=a, g=g,
                            br=br, bv=bv, bc=bc, ba=ba, bg=bg,
                            distance=seq(bin1Distance, by=cellSize, length.out=numberOfCells),
                            time=time,
                            pressure=pressure,
                            temperature=temperature,
                            salinity=salinity,
                            depth=depth,
                            soundSpeed=soundSpeed,
                            heading=heading,
                            pitch=pitch,
                            roll=roll,
                            headingStd=headingStd,
                            pitchStd=pitchStd,
                            rollStd=rollStd,
                            pressureStd=pressureStd,
                            xmitCurrent=xmitCurrent,
                            xmitVoltage=xmitVoltage,
                            ambientTemp=ambientTemp,
                            pressurePlus=pressurePlus,
                            pressureMinus=pressureMinus,
                            attitudeTemp=attitudeTemp,
                            attitude=attitude,
                            contaminationSensor=contaminationSensor)
           } else if (haveBottomTrack && isVMDAS) {
               br[br == 0.0] <- NA    # clean up (not sure if needed)
               data <- list(v=v, q=q, a=a, g=g,
                            br=br, bv=bv,
                            distance=seq(bin1Distance, by=cellSize, length.out=numberOfCells),
                            time=time,
                            pressure=pressure,
                            temperature=temperature,
                            salinity=salinity,
                            depth=depth,
                            soundSpeed=soundSpeed,
                            heading=heading,
                            pitch=pitch,
                            roll=roll,
                            headingStd=headingStd,
                            pitchStd=pitchStd,
                            rollStd=rollStd,
                            pressureStd=pressureStd,
                            xmitCurrent=xmitCurrent,
                            xmitVoltage=xmitVoltage,
                            ambientTemp=ambientTemp,
                            pressurePlus=pressurePlus,
                            pressureMinus=pressureMinus,
                            attitudeTemp=attitudeTemp,
                            attitude=attitude,
                            contaminationSensor=contaminationSensor,
                            navTime=navTime,
                            slongitude=slongitude,
                            slatitude=slatitude,
                            elongitude=elongitude,
                            elatitude=elatitude)
           } else {
               data <- list(v=v, q=q, a=a, g=g,
                            distance=seq(bin1Distance, by=cellSize, length.out=numberOfCells),
                            time=time,
                            pressure=pressure,
                            temperature=temperature,
                            salinity=salinity,
                            depth=depth,
                            soundSpeed=soundSpeed,
                            heading=heading,
                            pitch=pitch,
                            roll=roll,
                            headingStd=headingStd,
                            pitchStd=pitchStd,
                            rollStd=rollStd,
                            pressureStd=pressureStd,
                            xmitCurrent=xmitCurrent,
                            xmitVoltage=xmitVoltage,
                            ambientTemp=ambientTemp,
                            pressurePlus=pressurePlus,
                            pressureMinus=pressureMinus,
                            attitudeTemp=attitudeTemp,
                            attitude=attitude,
                            contaminationSensor=contaminationSensor)
           }
           if (testing) {
               data$upward=upward
           }
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
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    hitem <- processingLogItem(processingLog)
    res <- new('adp')
    res@metadata <- metadata
    res@data <- data
    res@processingLog <- unclass(hitem)
    res
}
