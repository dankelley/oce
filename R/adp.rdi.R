## vim: tw=100 shiftwidth=4 softtabstop=4 expandtab:
## byte sequences at start of items
## FLH 00 00; VLH 00 80; vel 00 01; Cor 00 02;  echo 00 03; percent 00 04; bottom-track 00 06

## References
##
## teledyne2005wcao: Workhorse Commands and Output Data format. March 2005
## Teledyne RD Instruments, 2005.
## ("WorkHorse Commands and Output Data Format_Mar05.pdf" in Dan Kelley's collection)
##
## teledyne2007wcao: Workhorse Commands and Output Data format. November 2007
## Teledyne RD Instruments, 2007.
## ("WorkHorse Technical Manual_Nov07.pdf" in Dan Kelley's collection)
##
## teledyne2010wcao: Workhorse Commands and Output Data format. August 2010
## Teledyne RD Instruments, 2010.
## ("WorkHorse_commands_data_format_AUG10.PDF" in Dan Kelley's collection)
##
## teledyne2014ostm: Ocean Surveyor / Ocean Observer technical manual
## Teledyne RD Instruments, 2014.
## ("OS_TM_Apr14.pdf" in Dan Kelley's collection)
##
## teledyne2015vsod: V Series output data format
## Teledyne RD Instruments, 2015.
## ("SV_ODF_May15.pdf")

## The following information is from Table 33, p 146 teledyne2014ostm. Note that
## 'i' refers to a byte number offset, with 'i+1' the subsequent byte; this avoid
## the confusing LSB and MSB notation in teledyne2014ostm.
##
## Standard byte flags
##
##  i     i+1    DESCRIPTION
## 7F      7F    Header
## 00      00    Fixed Leader
## 80      00    Variable Leader
## 00      01    Velocity Profile
## 00      02    Data Correlation Profile
## 00      03    Data Echo Intensity Profile
## 00      04    Data Percent Good Profile
## 00      05    Data Status Profile Data
## 00      06    Bottom Track Data
## 00      20    Navigation
## 00      30    Binary Fixed Attitude
## 40-F0   30    Binary Variable Attitude
##
## "Standard plus 1" byte flags (not handled)
##
##  i     i+1    DESCRIPTION
## 7F      7F    Header
## 01      00    Fixed Leader
## 81      00    Variable Leader
## 01      01    Velocity Profile
## 01      02    Data Correlation Profile
## 01      03    Data Echo Intensity Profile
## 01      04    Data Percent Good Profile
## 01      05    Data Status Profile Data
## 01      06    Bottom Track Data
## 00      20    Navigation
## 00      30    Binary Fixed Attitude
## 40-F0   30    Binary Variable Attitude
##



decodeHeaderRDI <- function(buf, debug=getOption("oceDebug"), tz=getOption("oceTz"), ...)
{

    ##
    ## header, of length 6 + 2 * numberOfDataTypes bytes
    ##
    oceDebug(debug, "decodeHeaderRDI() {\n", unindent=1)
    if (buf[1] != 0x7f || buf[2] != 0x7f)
        stop("first two bytes in file must be 0x7f 0x7f, but they are 0x", buf[1], " 0x", buf[2])
    ## FIXME: for sentinel files bytesPerEnsemble isn't the same for all ensembles
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
    if (dataOffset[1]!=6+2*numberOfDataTypes)
        warning("dataOffset and numberOfDataTypes are inconsistent -- this dataset seems damaged")
    oceDebug(debug, "dataOffset=", paste(dataOffset, sep=" "), "\n")
    oceDebug(debug, "sort(diff(dataOffset))=", paste(sort(diff(dataOffset)), sep=" "), "\n")
    ##
    ## See if this is a sentinel file by looking for dataType ID bytes
    ## of 0x00 and 0x70 (V series system configuration)
    codes <- cbind(buf[1 + c(0, dataOffset)], buf[1+c(0, dataOffset) + 1])
    oceDebug(debug, "buf[1:10] near line 95: ", paste("0x", paste(buf[1:10], sep=" "), sep=""), "\n")
    oceDebug(debug, "codes[,1]=", paste("0x", paste(codes[,1], sep=""), sep=""), "\n")
    oceDebug(debug, "codes[,2]=", paste("0x", paste(codes[,2], sep=""), sep=""), "\n")
    if (any(codes[, 1] == 0x00 & codes[, 2] == 0x70)) {
        oceDebug(debug, "Detected dataType 0x00 0x70 for Sentinel V series configuration")
        isSentinel <- TRUE
    } else {
        isSentinel <- FALSE
    }
    oceDebug(debug, "isSentinel=", isSentinel, " as inferred from the codes matrix, near adp.rdi.R line 103\n")
    ##
    ## Fixed Leader Data, abbreviated FLD, pointed to by the dataOffset
    FLD <- buf[dataOffset[1]+1:(dataOffset[2] - dataOffset[1])]
    oceDebug(debug, "Fixed Leader Data:", paste(FLD, collapse=" "), "\n")
    if (FLD[1] != 0x00 && FLD[1] != 0x01)
        stop("first byte of fixed leader header must be 0x00 or 0x01 but it is ", FLD[1])
    if (FLD[2] != 0x00)
        stop("second byte of fixed leader header must be a0x00 but it is ", FLD[2])
    firmwareVersionMajor <- readBin(FLD[3], "integer", n=1, size=1, signed=FALSE)
    firmwareVersionMinor <- readBin(FLD[4], "integer", n=1, size=1, signed=FALSE)
    firmwareVersion <- paste(firmwareVersionMajor, firmwareVersionMinor, sep=".")
    firmwareVersionNumeric <- as.numeric(firmwareVersion)
    oceDebug(debug, "firmwareVersion=", firmwareVersion, "(numerically, it is", firmwareVersionNumeric, ")\n")
    ##if (firmwareVersion < 16.28) warning("firmwareVersion ", firmwareVersion, " is less than 16.28, and so read.adp.rdi() may not work properly")

    if (!haveActualData)
        return(list(instrumentType="adcp",
                    firmwareVersionMajor=firmwareVersionMajor,
                    firmwareVersionMinor=firmwareVersionMinor,
                    firmwareVersion=firmwareVersion,
                    haveActualData=haveActualData))

    ## FLD[5] = SYSTEM CONFIGURATION LSB (Table 5.2, page 126, System Integrator Guide, Nov 2007)
    ## FLD[6] = SYSTEM CONFIGURATION MSB
    systemConfiguration <- paste(byteToBinary(FLD[5], endian="big"), byteToBinary(FLD[6], endian="big"), sep="-")
    oceDebug(debug, "FLD[4]=", byteToBinary(FLD[4], endian="big"), "(looking near the systemConfiguration bytes to find a problem)\n")
    oceDebug(debug, "FLD[5]=", byteToBinary(FLD[5], endian="big"), "(should be one of the systemConfiguration bytes)\n")
    oceDebug(debug, "FLD[6]=", byteToBinary(FLD[6], endian="big"), "(should be one of the systemConfiguration bytes)\n")
    oceDebug(debug, "FLD[7]=", byteToBinary(FLD[7], endian="big"), "(looking near the systemConfiguration bytes to find a problem)\n")
    bits <- substr(systemConfiguration, 6, 8)
    ## NOTE: the nearby code should perhaps use .Call("get_bit", ...) for speed and clarity
    if (isSentinel) {
        if (bits == "010") frequency <- 250        # kHz
        else if (bits == "011") frequency <-  500
        else if (bits == "100") frequency <-  1000
        else stop("unknown freq. bit code:", bits, " (expect 010 for 250kHz, 010 for 500kHz, etc)")
    } else {
        if (bits == "000") frequency <- 75        # kHz
        else if (bits == "001") frequency <-  150
        else if (bits == "010") frequency <-  300
        else if (bits == "011") frequency <-  600
        else if (bits == "100") frequency <- 1200
        else if (bits == "101") frequency <- 2400
        else stop("unknown freq. bit code:", bits, " (expect 000 for 75kHz, 001 for 150kHz, etc)")
    }
    oceDebug(debug, "bits:", bits, "so frequency=", frequency, "\n")
    bits <- substr(systemConfiguration, 16, 17)
    if (isSentinel) {
        oceDebug(debug, "systemConfiguration:", systemConfiguration, "\n")
        oceDebug(debug, "bits:", bits, "Expect 111 for 25 degrees\n")
        bits <- substr(systemConfiguration, 15, 17)
        if (bits != "111") message("Assuming beam angle of 25deg, but SysCon bits aren't 111")
        beamAngle <- 25
    } else {
        oceDebug(debug, "systemConfiguration:", systemConfiguration, "\n")
        oceDebug(debug, "bits:", bits, "00 is 15deg, 01 is 20deg, 02 is 30deg, 11 is 'other'\n")
        if (bits == "00") beamAngle <- 15
        else if (bits == "01") beamAngle <- 20
        else if (bits == "10") beamAngle <- 30
        else if (bits == "11") beamAngle <- NA # means 'other'
    }
    oceDebug(debug, "bits=", bits, "so beamAngle=", beamAngle, "\n")
    ## if (beamAngle < 19 || 21 < beamAngle)
    ##     warning("expecting a beamAngle of 20 deg [more-or-less standard for RDI] but got ", beamAngle, "deg; using the latter in the transformationMatrix")
    bits <- substr(systemConfiguration, 5, 5)
    if (isSentinel) {
        beamPattern <- "convex" # is always convex for a SentinelV
    } else {
        if (bits == "0") beamPattern <- "concave"
        else beamPattern <- "convex"
    }
    oceDebug(debug, "bits=", bits, "so beamPattern=", beamPattern, "\n")
    beamConfig <- "?"
    bits <- substr(systemConfiguration, 10, 13)
    if (isSentinel) {
        if (bits == "0100") beamConfig <- "4 beam janus"
        else if (bits == "0101") beamConfig <- "5 beam janus"
        else beamConfig <- "unknown"
    } else {
        if (bits == "0100") beamConfig <- "janus"
        else if (bits == "0101") beamConfig <- "janus demod"
        else if (bits == "1111") beamConfig <- "janus 2 demd"
        else beamConfig <- "unknown"
    }
    bits <- substr(systemConfiguration, 1, 1)
    if (bits == "1") orientation <- "upward"
    else orientation <- "downward"
    oceDebug(debug, "bits=", bits, "so that orientation=", orientation, "\n")

    ##real.sim.flag <- readBin(FLD[7], "integer", n=1, size=1)
    ##lagLength <- readBin(FLD[8], "integer", n=1, size=1, signed=FALSE) # unused
    numberOfBeams <- readBin(FLD[9], "integer", n=1, size=1, signed=FALSE)
    oceDebug(debug, "numberOfBeams", numberOfBeams, "\n")
    numberOfCells <- abs(readBin(FLD[10], "integer", n=1, size=1, signed=FALSE)) # WN
    oceDebug(debug, "numberOfCells", numberOfCells, "\n")
    pingsPerEnsemble <- readBin(FLD[11:12], "integer", n=1, size=2, endian="little")
    cellSize <- readBin(FLD[13:14], "integer", n=1, size=2, endian="little") / 100 # WS in m
    if (cellSize < 0 || cellSize > 64)
        stop("cellSize of ", cellSize, "m is not in the allowed range of 0m to 64m")
    ##blank.after.transmit <- readBin(FLD[15:16], "integer", n=1, size=2, endian="little") / 100 # in m
    profilingMode <- readBin(FLD[17], "integer", n=1, size=1) # WM
    lowCorrThresh <- readBin(FLD[18], "integer", n=1, size=1)
    numberOfCodeReps <- readBin(FLD[19], "integer", n=1, size=1)
    percentGdMinimum <- readBin(FLD[20], "integer", n=1, size=1)
    errorVelocityMaximum <- readBin(FLD[21:22], "integer", n=1, size=2, endian="little")
    ##tpp.minutes <- readBin(FLD[23], "integer", n=1, size=1)
    ##tpp.seconds <- readBin(FLD[24], "integer", n=1, size=1)
    ##tpp.hundredths <- readBin(FLD[25], "integer", n=1, size=1)
    coordTransform <- byteToBinary(FLD[26], endian="big")
    bits <- substr(byteToBinary(FLD[26], endian="big"), 4, 5)    
    originalCoordinate <- "???"
    if (bits == "00") originalCoordinate <- "beam"
    else if (bits == "01") originalCoordinate <- "xyz"
    else if (bits == "10") originalCoordinate <- "sfm"
    else if (bits == "11") originalCoordinate <- "enu"
    bits <- substr(byteToBinary(FLD[26], endian="big"), 6, 6)
    if (bits == "1") tiltUsed <- TRUE
    else tiltUsed <- FALSE
    bits <- substr(byteToBinary(FLD[26], endian="big"), 7, 7)
    if (bits == "1") threeBeamUsed <- TRUE
    else threeBeamUsed <- FALSE
    bits <- substr(byteToBinary(FLD[26], endian="big"), 8, 8)
    if (bits == "1") binMappingUsed <- TRUE
    else binMappingUsed <- FALSE
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
    oceDebug(debug, paste("cpuBoardSerialNumber = '", paste(cpuBoardSerialNumber, collapse=""), "'\n", sep=""))
    systemBandwidth <- readBin(FLD[51:52], "integer", n=1, size=2, endian="little")
    ##systemPower <- readBin(FLD[53], "integer", n=1, size=1)
    ## FLD[54] spare
    ## "WorkHorse Commands and Output Data Format_Mar05.pdf" p130: bytes 55:58 = serialNumber only for REMUS, else spare
    ## "WorkHorse Commands and Output Data Format_Nov07.pdf" p127: bytes 55:58 = serialNumber
    serialNumber <- readBin(FLD[55:58], "integer", n=1, size=4, endian="little")
    oceDebug(debug, "serialNumber", serialNumber, "from bytes (", FLD[55:58], ")\n")
    if (serialNumber == 0)
        serialNumber <- "unknown"
    ##beamAngle <- readBin(FLD[59], "integer", n=1, size=1) # NB 0 in first test case
    ##cat("BEAM ANGLE=", FLD[59], "or", beamAngle, "\n", ...)
    ##
    ## VLD (variable leader data)
    ##   The VLD length varies (see below) so infer its position from dataOffset[1].
    ##
    ## teledyne2005wcao and teledyne2007wcao (Figure 9 on page 122):
    ##       HEADER (6+2*num.types bytes) bytes
    ##       FLD 59 bytes
    ##       VLD 65 bytes
    ## "Ocean Surveyor Technical Manual.pdf" table D-3 on page D-5 (pdf-page 139):
    ##       HEADER (6+2*num.types) bytes
    ##       FLD 50 bytes
    ##       VLD 58 bytes
    ## teledyne2014ostm figure 45 p144 with WP *or* NP command
    ##       HEADER  (6+2*num.types) bytes
    ##       FLD 50 bytes
    ##       VLD 60 bytes
    ## teledyne2014ostm figure 46 p145 with WP *and* NP command
    ##       HEADER  (6+2*num.types) bytes
    ##       FLD 50 bytes
    ##       VLD 60 bytes
    ## dataOffset[1] = within-ensemble byte offset for FLD (e.g. Table D-1 of Surveyor manual)
    ## dataOffset[2] = within-ensemble byte offset for VLD (e.g. Table D-1 of Surveyor manual)
    ## thus, length of FLD is dataOffset[2]-dataOffset[1]
    FLDLength <- dataOffset[2] - dataOffset[1]
    oceDebug(debug, "FLDLength", FLDLength, " (expect 59 for Workhorse, or 50 for Surveyor/Observer)\n")

    ## ----------
    ## RISKY CODE
    ## ----------
    ##
    ## There really seems to be nothing specific in the file to tell instrument type, so, in an act of
    ## desparation (or is that hope) I'm going to flag on the one thing that was clearly stated, and
    ## clearly different, in the two documentation entries. The exception is the 'sentinel V' class,
    ## which is recognized by a code sequence 0x00 0x70 (see near line 96), so we capture that first.
    if (isSentinel) {
        instrumentSubtype <- "sentinelV"
        ## 5 beam (4 beam workhorse + a vertical centre beam)
    } else if (FLDLength == 59) {
        instrumentSubtype <- "workhorse" # "WorkHorse Commands and Output Data Format_Mar05.pdf" (and Nov07 version) Figure 9 on page 122 (pdf-page 130)
    } else if (FLDLength == 50) {
        instrumentSubtype <- "surveyor/observer"
        ## "Ocean Surveyor Technical Manual.pdf" table D-3 on page D-5 (pdf-page 139)
        ## also teledyne2014ostm page 144 says could be Surveyor or Observer
    } else {
        instrumentSubtype <- "unknown"
        ## FIXME: I think this is a poor way to determine the intrument type. Why do we even try?
        ##> warning("unexpected length ", FLDLength, " of fixed-leader-data header; expecting 50 for
        ##>         'surveyor/observor' or 59 for 'workhorse'.")
    }
    oceDebug(debug, "instrumentSubtype='", instrumentSubtype, "' in R/adp.rdi.R near line 311\n", sep="")
    nVLD <- 65 # FIXME: should use the proper length, but we won't use it all anyway
    VLD <- buf[dataOffset[2]+1:nVLD]
    oceDebug(debug, "Variable Leader Data (", length(VLD), "bytes):", paste(VLD, collapse=" "), "\n")
    ## ensure that header is not ill-formed
    if (VLD[1] != 0x80)
        stop("byte 1 of variable leader data should be 0x80, but it is ", VLD[1])
    if (VLD[2] != 0x00)
        stop("byte 2 of variable leader data should be 0x00, but it is ", VLD[2])
    ##ensemble.number <- readBin(VLD[3:4], "integer", n=1, size=2, endian="little")
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
    ##ensembleNumberMSB <- readBin(VLD[12], "integer", n=1, size=1)
    ##bitResult <- readBin(VLD[13:14], "integer", n=1, size=2, endian="little")
    soundSpeed <- readBin(VLD[15:16], "integer", n=1, size=2, endian="little")
    oceDebug(debug, "soundSpeed= ", soundSpeed, "\n") # FIXME possibly wrong
    transducerDepth <- readBin(VLD[17:18], "integer", n=1, size=2, endian="little")
    oceDebug(debug, "transducerDepth = ", transducerDepth, "\n")
    if (soundSpeed < 1400 || soundSpeed > 1600)
        warning("soundSpeed is ", soundSpeed, ", which is outside the permitted range of 1400 m/s to
                1600 m/s.  Something may be wrong in decoding the data.")
    oceDebug(debug, "about to create the list to be returned\n")
    res <- list(instrumentType="adcp",
                instrumentSubtype=instrumentSubtype,
                firmwareVersionMajor=firmwareVersionMajor,
                firmwareVersionMinor=firmwareVersionMinor,
                firmwareVersion=firmwareVersion,
                bytesPerEnsemble=bytesPerEnsemble,
                systemConfiguration=systemConfiguration,
                frequency=frequency,
                beamAngle=beamAngle,
                beamPattern=beamPattern,
                beamConfig=beamConfig,
                orientation=orientation,
                numberOfDataTypes=numberOfDataTypes,
                dataOffset=dataOffset,
                codes=codes,
                numberOfBeams=numberOfBeams,
                numberOfCells=numberOfCells,
                pingsPerEnsemble=pingsPerEnsemble,
                cellSize=cellSize,
                transducerDepth=transducerDepth,
                profilingMode=profilingMode,
                lowCorrThresh=lowCorrThresh,
                numberOfCodeReps=numberOfCodeReps,
                percentGdMinimum=percentGdMinimum,
                errorVelocityMaximum=errorVelocityMaximum,
                ##tpp.minutes=tpp.minutes,
                ##tpp.seconds=tpp.seconds,
                ##tpp.hundredths=tpp.hundredths,
                coordTransform=coordTransform,
                originalCoordinate=originalCoordinate,
                tiltUsed=tiltUsed,
                threeBeamUsed=threeBeamUsed,
                binMappingUsed=binMappingUsed,
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
    oceDebug(debug, "} # decodeHeaderRDI()\n", unindent=1)
    res
}                                       # decodeHeaderRDI


#' Read a Teledyne/RDI ADP File
#'
#' Read a Teledyne/RDI ADCP file (called 'adp' in oce).
#'
#' As of 2016-09-25, this function has provisional functionality to
#' read data from the new "SentinelV" series ADCP -- essentially a
#' combination of a 4 beam workhorse with an additional vertical
#' centre beam.
#'
#' If a heading bias had been set with the \code{EB} command during the setup
#' for the deployment, then a heading bias will have been stored in the file's
#' header.  This value is stored in the object's metadata as
#' \code{metadata$heading.bias}.  \strong{Importantly}, this value is
#' subtracted from the headings stored in the file, and the result of this
#' subtraction is stored in the objects heading value (in \code{data$heading}).
#' It should be noted that \code{read.adp.rdi()} was tested for firmware
#' version 16.30.  For other versions, there may be problems.  For example, the
#' serial number is not recognized properly for version 16.28.
#'
#' In Teledyne/RDI ADP data files, velocities are coded to signed 2-byte integers, with a
#' scale factor being used to convert to velocity in metres per second.  These
#' two facts control the maximum recordable velocity and the velocity
#' resolution, values that may be retrieved for an ADP object name \code{d}
#' with \code{d[["velocityMaximum"]]} and \code{d[["velocityResolution"]]}.
#'
#' @template adpTemplate
#'
#' @param testing logical value (IGNORED).
#'
#' @param type character string indicating the type of instrument.
#'
#' @param despike if \code{TRUE}, \code{\link{despike}} will be used to clean
#' anomalous spikes in heading, etc.
#'
#' @section Memory considerations:
#'
#' For \code{RDI} files only, and only in the case where \code{by} is not specified,
#' an attempt is made to avoid running out of memory by skipping some profiles
#' in large input files. This only applies if \code{from} and \code{to} are both
#' integers; if they are times, none of the rest of this section applies.
#'
#' A key issue is that RDI files store velocities in 2-byte values, which is
#' not a format that R supports. These velocities become 8-byte (numeric) values
#' in R. Thus, the R object created by \code{read.adp.rdi} will require more memory
#' than that of the data file. A scale factor can be estimated by ignoring
#' vector quanties (e.g. time, which has just one value per profile) and concentrating on matrix properties
#' such as velocity, backscatter, and correlation. These three elements have equal dimensions.
#' Thus, each 4-byte slide in the data file (2 bytes + 1 byte + 1 byte)
#' corresponds to 10 bytes in the object (8 bytes + 1 byte + 1 byte).
#' Rounding up the resultant 10/4 to 3 for safety, we conclude that any limit on the
#' size of the R object corresponds to a 3X smaller limit on file size.
#'
#' Various things can limit the size of objects in R, but a strong upper limit
#' is set by the space the operating system provides to R. The least-performant machines
#' in typical use appear to be Microsoft-Windows systems, which limit R objects to
#' about 2e6 bytes [3].  Since R routinely duplicates objects for certain tasks
#' (e.g. for call-by-value in function evaluation), \code{read.adp.rdi} uses a safety
#' factor in its calculation of when to auto-decimate a file. This factor is set to 3,
#' based partly on the developers' experience with datasets in their possession.
#' Multiplied by the previously stated safety factor of 3, 
#' this suggests that the 2 GB limit on R objects corresponds to approximately a
#' 222 MB limit on file size. In the present version of \code{read.adp.rdi}, this
#' value is lowered to 200 MB for simplicity. Larger files are considered to be "big",
#' and are decimated unless the user supplies a value for the \code{by} argument.
#'
#' The decimation procedure has two cases.
#' \enumerate{
#' \item \emph{Case 1.} If \code{from=1} and
#' \code{to=0} (or if neither \code{from} or \code{to} is given), then the 
#' intention is to process the full span of the data.  If the input file is
#' under 200 MB, then \code{by} defaults to 1, so that all profiles are read.
#' For larger files, \code{by} is set to the \code{\link{ceiling}} of the
#' ratio of input file size to 200 MB.
#'
#' \item \emph{Case 2.} If \code{from} exceeds 1, and/or \code{to} is nonzero, then
#' the intention is to process only an interior subset of the file. In this
#' case, \code{by} is calculated as the \code{\link{ceiling}} of
#' the ratio of \code{bbp*(1+to-from)} to 200 MB, where \code{bbp} is the number
#' of file bytes per profile. Of course, \code{by} is set to 1, if this
#' ratio is less than 1.
#'}
#'
#' If the result of these calculations is that \code{by} exceeds 1, then
#' messages are printed to alert the user that the file will be decimated,
#' and also \code{monitor} is set to \code{TRUE}, so that a textual progress bar
#' is shown.
#'
#' @author Dan Kelley and Clark Richards
#'
#' @references
#' 1. Teledyne-RDI, 2007. \emph{WorkHorse commands and output data
#' format.} P/N 957-6156-00 (November 2007).  (Section 5.3 h details the binary
#' format, e.g. the file should start with the byte \code{0x7f} repeated twice,
#' and each profile starts with the bytes \code{0x80}, followed by \code{0x00},
#' followed by the sequence number of the profile, represented as a
#' little-endian two-byte short integer.  \code{read.adp.rdi} uses these
#' sequences to interpret data files.)
#'
#' 2. Teledyne-RDI, 2015. \emph{V Series output data format.} P/N 95D-6022-00 (May 2015).
#'
#' 3. See \code{\link{Memory-limits}} for more on the 2 GB limit for R on windows
#' machines (but note that this documentation erroneously states the unit as Gb, which is
#' typically used for gigabits).
#'
#' @family things related to \code{adp} data
read.adp.rdi <- function(file, from, to, by, tz=getOption("oceTz"),
                         longitude=NA, latitude=NA,
                         type=c("workhorse"),
                         monitor=FALSE, despike=FALSE, processingLog,
                         testing=FALSE,
                         debug=getOption("oceDebug"),
                         ...)
{
    fromGiven <- !missing(from) # FIXME document THIS
    toGiven <- !missing(to) # FIXME document THIS
    byGiven <- !missing(by) # FIXME document THIS
    oceDebug(debug, "read.adp.rdi(...",
             ", from=", if (fromGiven) format(from) else "(missing)",
             ", to=", if (toGiven) format(to) else "(missing)",
             ", by=", if (byGiven) format(by) else "(missing)",
             "...) {\n", unindent=1)
    if (!fromGiven)
        from <- 1
    if (!byGiven)
        by <- 1
    if (!toGiven)
        to <- 0
    profileStart <- NULL # prevent scope warning from rstudio; defined later anyway
    ## bisectAdpRdiBuf <- function(buf, t.find, add=0, debug=0) {
    ##     oceDebug(debug, "bisectAdpRdiBuffer(t.find=", format(t.find), ", add=", add, ") {\n", unindent=1)
    ##     len <- length(profileStart)
    ##     lower <- 1
    ##     upper <- len
    ##     passes <- floor(10 + log(len, 2)) # won't need this many; only do this to catch coding errors
    ##     for (pass in 1:passes) {
    ##         middle <- floor( (upper + lower) / 2 )
    ##         year   <- unabbreviateYear(readBin(buf[profileStart[middle] +  4], what="integer", n=1, size=1, signed=FALSE))
    ##         month  <- readBin(buf[profileStart[middle] +  5], what="integer", n=1, size=1, signed=FALSE)
    ##         day    <- readBin(buf[profileStart[middle] +  6], what="integer", n=1, size=1, signed=FALSE)
    ##         hour   <- readBin(buf[profileStart[middle] +  7], what="integer", n=1, size=1, signed=FALSE)
    ##         minute <- readBin(buf[profileStart[middle] +  8], what="integer", n=1, size=1, signed=FALSE)
    ##         second <- readBin(buf[profileStart[middle] +  9], what="integer", n=1, size=1, signed=FALSE)
    ##         sec100 <- readBin(buf[profileStart[middle] + 10], what="integer", n=1, size=1, signed=FALSE)
    ##         t <- ISOdatetime(year, month, day, hour, minute, second + sec100/100, tz=tz)
    ##         oceDebug(debug, "t=", format(t), "| y=", year, " m=", month, " d=",
    ##                  format(day, width=2), " h=", format(hour, width=2), " m=",
    ##                  format(minute, width=2), "s=", format(second, width=2), "sec100=", sec100, "| pass",
    ##                  format(pass, width=2), "/", passes, "| middle=", middle, "(",
    ##                  format(middle/upper*100, digits=4), "%)\n")
    ##         if (t.find < t)
    ##             upper <- middle
    ##         else
    ##             lower <- middle
    ##         if (upper - lower < 2)
    ##             break
    ##     }
    ##     middle <- middle + add          # may use add to extend before and after window
    ##     if (middle < 1)
    ##         middle <- 1
    ##     if (middle > len)
    ##         middle <- len
    ##     t <- ISOdatetime(unabbreviateYear(readBin(buf[profileStart[middle]+4], "integer", size=1, signed=FALSE, endian="little")),
    ##                      as.integer(buf[profileStart[middle]+5]), # month
    ##                      as.integer(buf[profileStart[middle]+6]), # day
    ##                      as.integer(buf[profileStart[middle]+7]), # hour
    ##                      as.integer(buf[profileStart[middle]+8]), # min
    ##                      as.integer(buf[profileStart[middle]+9])+0.01*as.integer(buf[profileStart[middle]+10]), # decimal second
    ##                      tz=tz)
    ##     oceDebug(debug, "result: t=", format(t), " at vsdStart[", middle, "]=", profileStart[middle], "\n")
    ##     oceDebug(debug, "} # bisectAdpRdiBuffer()\n", unindent=1)
    ##     return(list(index=middle, time=t))
    ## }
    ##
    ## bisectAdpRdiLdc<- function(ldc, tFind, add=0, debug=0) {
    ##     oceDebug(debug, "bisectAdpRdiLdc(tFind=", format(tFind), ", add=", add, ") {\n", unindent=1, sep="")
    ##     len <- length(ldc$year)
    ##     lower <- 1
    ##     upper <- len
    ##     passes <- floor(10 + log(len, 2)) # won't need this many; only do this to catch coding errors
    ##     for (pass in 1:passes) {
    ##         middle <- floor( (upper + lower) / 2 )
    ##         t <- ISOdatetime(unabbreviateYear(as.numeric(ldc$year[middle])),
    ##                          as.numeric(ldc$month[middle]),
    ##                          as.numeric(ldc$day[middle]),
    ##                          as.numeric(ldc$hour[middle]),
    ##                          as.numeric(ldc$minute[middle]),
    ##                          as.numeric(ldc$second[middle]) + 0.01*as.numeric(ldc$sec100[middle]),
    ##                          tz=tz)
    ##         oceDebug(debug, "middle t=", format(t), "\n")
    ##         if (tFind < t)
    ##             upper <- middle
    ##         else
    ##             lower <- middle
    ##         if (upper - lower < 2)
    ##             break
    ##     }
    ##     middle <- middle + add          # may use add to extend before and after window
    ##     if (middle < 1)
    ##         middle <- 1
    ##     if (middle > len)
    ##         middle <- len
    ##     oceDebug(debug, "result: t=", format(t), " at middle=", middle, "\n")
    ##     oceDebug(debug, "} # bisectAdpRdiLdc()\n", unindent=1)
    ##     return(list(index=middle, time=t))
    ## }

    ## gaveFromTo <- !missing(from) && !missing(to)
    ## if (gaveFromTo) {
    ##     oceDebug(debug, "class(from)=", class(from), "; class(to)=", class(to), "\n")
    ## }
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
    fileSize <- seek(file, where=0)
    oceDebug(debug, "fileSize=", fileSize, "\n")

    ## FIXME 20170107
    ## We process the header wholly in R, and we don't need more than probably 2000 bytes
    ## but let's read 10000 just in case. It might be worth thinking about this in more
    ## detail, in case a file might have a header that is much longer than any studied
    ## in writing this code.
    buf <- readBin(file, what="raw", n=min(fileSize, 10000), endian="little")
    header <- decodeHeaderRDI(buf, debug=debug-1)
    if (header$haveActualData) {
        numberOfBeams <- header$numberOfBeams
        numberOfCells <- header$numberOfCells
        bin1Distance <- header$bin1Distance
        xmitPulseLength <- header$xmitPulseLength
        cellSize <- header$cellSize
        #message("1. isSentinel=", isSentinel)
        isSentinel <- header$instrumentSubtype == "sentinelV"
        oceDebug(debug, "isSentinel=", isSentinel, " near adp.rdi.R line 652\n")
        oceDebug(debug, "about to call ldc_rdi_in_file\n")
        if (is.numeric(from) && is.numeric(to) && is.numeric(by) ) {
            ## check for large files
            byteMax <- 200e6           # for reasoning, see the help file
            if (!byGiven) {
                if (to == 0) {         # whole file
                    by <- if (fileSize < byteMax) 1L else fileSize / byteMax
                } else {
                    byteEstimate <-header$bytesPerEnsemble * (to - from)
                    by <- if (byteEstimate < byteMax) 1L else byteEstimate / byteMax
                }
                by <- max(1L, as.integer(by))
                if (by > 1) {
                    warning("setting by=", by, " for a large RDI file\n")
                    message("setting by=", by, " for a large RDI file")
                    if (!monitor) {
                        warning("setting monitor=TRUE for a large RDI file\n")
                        message("setting monitor=TRUE for a large RDI file")
                        monitor <- TRUE
                    }
                }
            }
            ldc <- .Call("ldc_rdi_in_file", filename,
                         as.integer(from), as.integer(to), as.integer(by), 0L)
        } else {
            ldc <- .Call("ldc_rdi_in_file", filename,
                         as.integer(from), as.integer(to), ctimeToSeconds(by), 1L)
        }
        ## Must now reset from,to,by in the *subsetted* data item, ldc.
        from <- 1
        to <- length(ldc$ensembleStart)
        by <- 1

        oceDebug(debug, "successfully called ldc_rdi_in_file\n")
        ensembleStart <- ldc$ensembleStart

        ## FIXME: do we really want to use this buffer? Why not
        ## use seek()+readBin() on the file itself?
        buf <- ldc$outbuf

        ###################
        message("IMPORTANT DEBUGGING MESSAGE:\n\tread.adp.rdi() is exporting a variable 'ldc' for checking.\n\tIf you see this message after Monday, May 9, 2017,\n\tplease update your oce from github,\n\tand report an error if the message persists")
        ldc<<-ldc
        ###################

        bufSize <- length(buf)

        ## 20170108 ## These three things no longer make sense, since we are not reading
        ## 20170108 ## the file to the end, in this updated scheme.
        ## 20170108 measurementStart <- as.POSIXct(ldc$time[1] + 0.01 * as.integer(ldc$sec100[1]),
        ## 20170108                                origin="1970-01-01", tz="UTC")
        ## 20170108 oceDebug(debug, "measurementStart:", format(measurementStart), "\n")
        ## 20170108 profilesInFile <- length(ldc$year)
        ## 20170108 measurementEnd <- as.POSIXct(ldc$time[profilesInFile] + 0.01 * as.integer(ldc$sec100[profilesInFile]),
        ## 20170108                              origin="1970-01-01", tz="UTC")
        ## 20170108 oceDebug(debug, "measurementEnd:", format(measurementEnd), "\n")
        ## 20170108 measurementDeltat <- (ldc$time[2] + 0.01 * as.integer(ldc$sec100[2])) - (ldc$time[1] + 0.01 * as.integer(ldc$sec100[2]))
        ## 20170108 oceDebug(debug, "measurementDeltat:", measurementDeltat, "s\n")

        ## Now, 'buf' contains *only* the profiles we want, so we may
        ## redefine 'from', 'to' and 'by' to specify each and every profile.
        from <- 1
        to <- length(ensembleStart)
        by <- 1
        oceDebug(debug, "NEW method from=", from, ", by=", by, ", to=", to, "\n", sep="")

        if (isSentinel) {
            oceDebug(debug, "SentinelV type detected, skipping first ensemble\n")
            ensembleStart <- ensembleStart[-1] # remove the first ensemble to simplify parsing
            to <- to - 1
            ## re-read the numberOfDataTypes and dataOffsets from the second ensemble
            header$numberOfDataTypes <- readBin(buf[ensembleStart[1]+5], "integer", n=1, size=1)
            header$dataOffset <- readBin(buf[ensembleStart[1]+6+0:(2*header$numberOfDataTypes)], "integer", n=header$numberOfDataTypes, size=2, endian="little", signed=FALSE)
            oceDebug(debug, "header$dataOffset=", paste(header$dataOffset, sep=" "), " (reread because a sentinelV file)\n")
        }

        ## Profiles start at the VARIABLE LEADER DATA, since there is no point in
        ## re-interpreting the HEADER and the FIXED LEADER DATA over and over,
        ## but we need the VLD to get times, etc. I *think* we can assume a fixed
        ## location for these, based on the "Always Output" indication in Fig 46
        ## on page 145 of teledyne2014ostm.
        profileStart <- ensembleStart + as.numeric(buf[ensembleStart[1]+8]) + 256*as.numeric(buf[ensembleStart[1]+9])
        if (any(profileStart < 1))
            stop("difficulty detecting ensemble (profile) start indices")
        # offset for data type 1 (velocity)
        oceDebug(debug, vectorShow(profileStart, "profileStart before trimming:"))
        profilesInFile <- length(profileStart)
        oceDebug(debug, "profilesInFile=", profilesInFile, "\n")
        if (profilesInFile > 0)  {
            profilesToRead <- length(profileStart)
            oceDebug(debug, "filename: \"", filename, "\"\n", sep="")
            oceDebug(debug, "profilesToRead:", profilesToRead, "\n")
            oceDebug(debug, "numberOfBeams:", numberOfBeams, "\n")
            oceDebug(debug, "numberOfCells:", numberOfCells, "\n")
            items <- numberOfBeams * numberOfCells
            ## 20170112 issue 1168: I think that in a winriver file, the second profile is
            ## 20170112 different, somehow, yielding incorrect 'codes'. FIXME: if the codes differ from
            ## 20170112 profile to profile, then what are we supposed to do?
            ## 20170112 codes <- cbind(buf[ensembleStart[1]+c(0, header$dataOffset)], buf[1+ensembleStart[1]+c(0, header$dataOffset)])
            ## 20170112 oceDebug(debug, "codes[,1]=", paste("0x", paste(codes[,1], sep=""), sep=""), "\n")
            ## 20170112 oceDebug(debug, "codes[,2]=", paste("0x", paste(codes[,2], sep=""), sep=""), "\n")
            codes <- header$codes
            oceDebug(debug, "codes[,1]=", paste("0x", paste(codes[,1], sep=""), sep=""), "\n")
            oceDebug(debug, "codes[,2]=", paste("0x", paste(codes[,2], sep=""), sep=""), "\n")
            oceDebug(debug, "buf[1:10] near line 745: ", paste("0x", paste(buf[1:10], sep=" "), sep=""), "\n")
            vFound <- sum(codes[, 1]==0x00 & codes[, 2]==0x01) # velo
            qFound <- sum(codes[, 1]==0x00 & codes[, 2]==0x02) # corr
            aFound <- sum(codes[, 1]==0x00 & codes[, 2]==0x03) # echo intensity
            gFound <- sum(codes[, 1]==0x00 & codes[, 2]==0x04) # percent good
            ##sFound <- sum(codes[,1]==0x00 & codes[,2]==0x05) # status
            bFound <- sum(codes[, 1]==0x00 & codes[, 2]==0x06) # bottom-track
            ##nFound <- sum(codes[,1]==0x00 & codes[,2]==0x20) # navigation
            tmFound <- sum(codes[, 1]==0x00 & codes[, 2]==0x32) # transformation matrix
            if (vFound) {
                v <- array(numeric(), dim=c(profilesToRead, numberOfCells, numberOfBeams))
                oceDebug(debug, "set up 'v' (velocity) storage for", profilesToRead, "profiles,",
                         numberOfCells, "cells, and", numberOfBeams, "beams\n")
            } else {
                v <- NULL
            }
            if (qFound) {
                q <- array(raw(), dim=c(profilesToRead, numberOfCells, numberOfBeams))
                oceDebug(debug, "set up 'q' (correlation) storage for", profilesToRead, "profiles,",
                         numberOfCells, "cells, and", numberOfBeams, "beams\n")
            } else {
                q <- NULL
            }
            if (aFound) {
                a <- array(raw(), dim=c(profilesToRead, numberOfCells, numberOfBeams))
                oceDebug(debug, "set up 'a' (echo intensity) storage for", profilesToRead, "profiles,",
                         numberOfCells, "cells, and", numberOfBeams, "beams\n")
            } else {
                a <- NULL
            }
            if (gFound) {
                g <- array(raw(), dim=c(profilesToRead, numberOfCells, numberOfBeams))
                oceDebug(debug, "set up 'g' (percent good) storage for", profilesToRead, "profiles,",
                         numberOfCells, "cells, and", numberOfBeams, "beams\n")
            } else {
                g <- NULL
            }
            ii <- which(codes[, 1]==0x01 & codes[, 2]==0x0f)
            if (isSentinel & length(ii) < 1) {
                warning("Didn't find V series leader data ID, treating as a 4 beam ADCP")
                isSentinel <- FALSE
            }
            if (isSentinel) {
                ## Look for sentinel-related codes
                ## FIXME: Fields to look for:
                ## 0x00 0x70: V series Config
                ## 0x01 0x70: V series ping setup
                ## 0x02 0x70: V series ADC data
                ## 0x04 0x70: V series event log
                ## 0x01 0x0f: V beam data leader
                ## 0x00 0x0a: V beam data
                ## 0x00 0x0b: V beam correlation
                ## 0x00 0x0c: V beam amplitude
                ## 0x00 0x0d: V beam percent good
                ## 0x00 0x32: Transformation Matrix
                tmFound <- sum(codes[, 1]==0x00 & codes[, 2]==0x32) # transformation matrix
                vvFound <- sum(codes[, 1]==0x00 & codes[, 2]==0x0a) # v beam data
                vaFound <- sum(codes[, 1]==0x00 & codes[, 2]==0x0c) # v beam amplitude
                vqFound <- sum(codes[, 1]==0x00 & codes[, 2]==0x0b) # v beam correlation
                vgFound <- sum(codes[, 1]==0x00 & codes[, 2]==0x0d) # v beam percent good
                ## Read the relevant V series metadata
                ## remove the first row from codes (7f7f) because it is the header (always has to be there)
                codes <- codes[-1, ]
                ##
                ## transformation matrix
                if (tmFound) {
                    ii <- which(codes[, 1]==0x00 & codes[, 2]==0x32)
                    oceDebug(debug, 'Reading transformation matrix\n')
                    tmx <- 0.0001 * readBin(buf[ensembleStart[1]+header$dataOffset[ii]+0:7+2], "integer", n=4, size=2, signed=TRUE, endian="little")
                    tmy <- 0.0001 * readBin(buf[ensembleStart[1]+header$dataOffset[ii]+0:7+10], "integer", n=4, size=2, signed=TRUE, endian="little")
                    tmz <- 0.0001 * readBin(buf[ensembleStart[1]+header$dataOffset[ii]+0:7+18], "integer", n=4, size=2, signed=TRUE, endian="little")
                    tme <- 0.0001 * readBin(buf[ensembleStart[1]+header$dataOffset[ii]+0:7+26], "integer", n=4, size=2, signed=TRUE, endian="little")
                    transformationMatrix <- matrix(c(tmx, tmy, tmz, tme),
                                                   nrow=4, byrow=TRUE)
                    if (debug > 0) {
                        cat('Transformation matrix:\n')
                        oceDebug(debug, vectorShow(tmy, paste("tmy", sep="")), "\n")
                        oceDebug(debug, vectorShow(tmz, paste("tmz", sep="")), "\n")
                        oceDebug(debug, vectorShow(tme, paste("tme", sep="")), "\n")
                    }
                }
                ## Read the V beam data leader
                ii <- which(codes[, 1]==0x01 & codes[, 2]==0x0f)
                oceDebug(debug, 'Reading V series data leader\n')
                numberOfVCells <- readBin(buf[ensembleStart[1] + header$dataOffset[ii]+2:3], "integer", size=2, endian="little")
                verticalPings <- readBin(buf[ensembleStart[1] + header$dataOffset[ii]+4:5], "integer", size=2, endian="little")
                depthCellSize <- 0.01*readBin(buf[ensembleStart[1] + header$dataOffset[ii]+6:7], "integer", size=2, endian="little")
                firstCellRange <- 0.01*readBin(buf[ensembleStart[1] + header$dataOffset[ii]+8:9], "integer", size=2, endian="little")
                verticalMode <- readBin(buf[ensembleStart[1] + header$dataOffset[ii]+10:11], "integer", size=2, endian="little")
                verticalTransmit <- 0.01*readBin(buf[ensembleStart[1] + header$dataOffset[ii]+12:13], "integer", size=2, endian="little")
                verticalLagLength <- 0.01*readBin(buf[ensembleStart[1] + header$dataOffset[ii]+14:15], "integer", size=2, endian="little")
                transmitCodeElements <- readBin(buf[ensembleStart[1] + header$dataOffset[ii]+16:17], "integer", size=2, endian="little")
                verticalRssiThreshold <- readBin(buf[ensembleStart[1] + header$dataOffset[ii]+18:19], "integer", size=2, endian="little")
                verticalShallowBin <- readBin(buf[ensembleStart[1] + header$dataOffset[ii]+20:21], "integer", size=2, endian="little")
                verticalStartBin <- readBin(buf[ensembleStart[1] + header$dataOffset[ii]+22:23], "integer", size=2, endian="little")
                verticalShallowRssiBin <- readBin(buf[ensembleStart[1] + header$dataOffset[ii]+24:25], "integer", size=2, endian="little")
                maxCoreThreshold <- readBin(buf[ensembleStart[1] + header$dataOffset[ii]+26:27], "integer", size=2, endian="little")
                minCoreThreshold <- readBin(buf[ensembleStart[1] + header$dataOffset[ii]+28:29], "integer", size=2, endian="little")
                pingOffsetTime <- readBin(buf[ensembleStart[1] + header$dataOffset[ii]+30:31], "integer", size=2, endian="little")
                surfSpare1 <- readBin(buf[ensembleStart[1] + header$dataOffset[ii]+32:33], "integer", size=2, endian="little")
                depthScreen <- readBin(buf[ensembleStart[1] + header$dataOffset[ii]+34:35], "integer", size=2, endian="little")
                percentGoodThreshold <- readBin(buf[ensembleStart[1] + header$dataOffset[ii]+36:37], "integer", size=2, endian="little")
                verticalDOproofing <- readBin(buf[ensembleStart[1] + header$dataOffset[ii]+38:39], "integer", size=2, endian="little")
                vBeamHeader <- list(numberOfVCells=numberOfVCells,
                                    verticalPings=verticalPings,
                                    depthCellSize=depthCellSize,
                                    firstCellRange=firstCellRange,
                                    verticalMode=verticalMode,
                                    verticalTransmit=verticalTransmit,
                                    verticalLagLength=verticalLagLength,
                                    transmitCodeElements=transmitCodeElements,
                                    verticalRssiThreshold=verticalRssiThreshold,
                                    verticalShallowBin=verticalShallowBin,
                                    verticalStartBin=verticalStartBin,
                                    verticalShallowRssiBin=verticalShallowRssiBin,
                                    maxCoreThreshold=maxCoreThreshold,
                                    minCoreThreshold=minCoreThreshold,
                                    minCoreThreshold=minCoreThreshold,
                                    pingOffsetTime=pingOffsetTime,
                                    surfSpare1=surfSpare1,
                                    depthScreen=depthScreen,
                                    percentGoodThreshold=percentGoodThreshold,
                                    verticalDOproofing=verticalDOproofing)
                vItems <- numberOfVCells

                ## V series config
                ii <- which(codes[, 1]==0x00 & codes[, 2]==0x70)
                if (length(ii) < 1) stop("Didn't find V series Configuration data ID")
                oceDebug(debug, 'Reading V series configuration\n')
                firmwareVersionPrimary <- as.numeric(buf[ensembleStart[1]+header$dataOffset[ii]+2])
                firmwareVersionSecondary <- as.numeric(buf[ensembleStart[1]+header$dataOffset[ii]+3])
                firmwareVersionBuild <- as.numeric(buf[ensembleStart[1]+header$dataOffset[ii]+4])
                firmwareVersionService <- as.numeric(buf[ensembleStart[1]+header$dataOffset[ii]+5])
                firmwareVersion <- paste(firmwareVersionPrimary, firmwareVersionSecondary,
                                         firmwareVersionBuild, firmwareVersionService,
                                         sep='.')
                oceDebug(debug, "V series firmware version", firmwareVersion, "\n")
                systemFrequency <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 6:9], 'integer', endian='little')
                pressureRating <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 10:11], 'integer', size=2, endian='little')
                schemaMajor <- as.numeric(buf[ensembleStart[1] + header$dataOffset[ii] + 12])
                schemaMinor <- as.numeric(buf[ensembleStart[1] + header$dataOffset[ii] + 13])
                schemaRev <- as.numeric(buf[ensembleStart[1] + header$dataOffset[ii] + 14])
                vBeamHeader$vSeriesConfig <- list(firmwareVersion=firmwareVersion,
                                                  systemFrequency=systemFrequency,
                                                  pressureRating=pressureRating,
                                                  schemaMajor=schemaMajor,
                                                  schemaMinor=schemaMinor,
                                                  schemaRev=schemaRev)
                ## Read V series ping setup
                ii <- which(codes[, 1]==0x01 & codes[, 2]==0x70)
                if (length(ii) < 1) stop("Didn't find V series ping setup data ID")
                oceDebug(debug, 'Reading V series ping setup\n')
                ensembleInterval <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 4:7], 'integer', endian='little')
                numberOfPings <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 8:9], 'integer', size=2, endian='little')
                timeBetweenPings <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 10:13], 'integer', endian='little')
                offsetBetweenPingGroups <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 14:17], 'integer', endian='little')
                pingSequenceNumber <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 22:23], 'integer', size=2, endian='little')
                ambiquityVelocity <- 0.001*readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 24:25], 'integer', size=2, endian='little')
                rxGain <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 26], 'integer', size=1, endian='little')
                rxBeamMask <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 27], 'integer', size=1, endian='little')
                txBeamMask <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 28], 'integer', size=1, endian='little')
                ensembleCount <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 34:35], 'integer', size=2, endian='little')
                deploymentStartCentury <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 36], 'integer', size=1, endian='little')
                deploymentStartYear <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 37], 'integer', size=1, endian='little')
                deploymentStartMonth <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 38], 'integer', size=1, endian='little')
                deploymentStartDay <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 39], 'integer', size=1, endian='little')
                deploymentStartHour <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 40], 'integer', size=1, endian='little')
                deploymentStartMinute <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 41], 'integer', size=1, endian='little')
                deploymentStartSecond <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 42], 'integer', size=1, endian='little')
                deploymentStartHundredths <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 43], 'integer', size=1, endian='little')
                deploymentStart <- ISOdatetime(deploymentStartCentury*100+deploymentStartYear,
                                               deploymentStartMonth, deploymentStartDay, deploymentStartHour,
                                               deploymentStartMinute, deploymentStartSecond + deploymentStartHundredths / 100, tz=tz)
                vBeamHeader$vSeriesPingSetup <- list(ensembleInterval=ensembleInterval,
                                                     numberOfPings=numberOfPings,
                                                     timeBetweenPings=timeBetweenPings,
                                                     offsetBetweenPingGroups=offsetBetweenPingGroups,
                                                     pingSequenceNumber=pingSequenceNumber,
                                                     ambiquityVelocity=ambiquityVelocity,
                                                     rxGain=rxGain,
                                                     rxBeamMask=rxBeamMask,
                                                     txBeamMask=txBeamMask,
                                                     ensembleCount=ensembleCount,
                                                     deploymentStart=deploymentStart)
                header$vBeamHeader <- vBeamHeader
                if (vvFound) {
                    vv <- array(numeric(), dim=c(profilesToRead, numberOfVCells))
                    oceDebug(debug, "set up 'vv' (vertical velocity) storage for", profilesToRead, "profiles, and",
                             numberOfVCells, "cells.\n")
                } else {
                    vv <- NULL
                }
                if (vaFound) {
                    va <- array(raw(), dim=c(profilesToRead, numberOfVCells))
                    oceDebug(debug, "set up 'va' (vertical backscatter amplitude) storage for", profilesToRead, "profiles, and",
                             numberOfVCells, "cells.\n")
                } else {
                    va <- NULL
                }
                if (vqFound) {
                    vq <- array(raw(), dim=c(profilesToRead, numberOfVCells))
                    oceDebug(debug, "set up 'vq' (vertical correlation) storage for", profilesToRead, "profiles, and",
                             numberOfVCells, "cells.\n")
                } else {
                    vq <- NULL
                }
                if (vgFound) {
                    vg <- array(raw(), dim=c(profilesToRead, numberOfVCells))
                    oceDebug(debug, "set up 'vg' (vertical percent good) storage for", profilesToRead, "profiles, and",
                             numberOfVCells, "cells.\n")
                } else {
                    vg <- NULL
                }
            }
            if (bFound) {
                br <- array(double(), dim=c(profilesToRead, numberOfBeams))
                bv <- array(double(), dim=c(profilesToRead, numberOfBeams))
                bc <- array(double(), dim=c(profilesToRead, numberOfBeams)) # correlation
                ba <- array(double(), dim=c(profilesToRead, numberOfBeams)) # amplitude
                bg <- array(double(), dim=c(profilesToRead, numberOfBeams)) # percent good
                oceDebug(debug, "set up 'br', etc. (bottom data) storage for", profilesToRead, "profiles,",
                         numberOfCells, "cells, and", numberOfBeams, "beams\n")
            } else {
                br <- bv <- bc <- ba <- bg <- NULL
            }

            badProfiles <- NULL
            ##haveBottomTrack <- FALSE          # FIXME maybe we can determine this from the header
            oceDebug(debug, "length(profileStart):", length(profileStart), "\n")
            if (profilesToRead < 1)
                stop("no profilesToRead")
            velocityScale <- 1e-3

            isVMDAS <- FALSE           # flag for file type
            badVMDAS <- NULL           # erroneous VMDAS profiles
            VMDASStorageInitialized <- FALSE # flag for whether we have VMDAS storage set up yet

            ##> We do some things differently based on the firmware.
            ##> if (header$firmwareVersionMajor == 23 && header$firmwareVersionMinor >= 12) {
            ##>     warning("Firmware version (", header$firmwareVersionMajor, ".", header$firmwareVersionMinor, ") exceeds 23.11, so using format in Teledyne/RDI document OS_TM_Apr14.pdf, i.e. skipping 'STATUS' chunk\n", sep="")
            ##>     skipStatus <- 2 + 4 * numberOfCells
            ##> } else {
            ##>     skipStatus <- 0
            ##> }

            ## FIXME: the docs say not to assume an order in data chunks but the following works in all
            ## FIXME: data seen to date.
            oceDebug(debug, "header$numberOfDataTypes: ", header$numberOfDataTypes, "\n")

            ##profilesToShow <- 2 # only if debug>0
            if (monitor)
                progressBar = txtProgressBar(max=profilesToRead, style=3, title="Reading profiles")

            oceDebug(debug, "profilesToRead=", profilesToRead, " (issue 1228: expect 8324 or 8323)\n")
            for (i in 1:profilesToRead) {
                ## recall: these start at 0x80 0x00
                for (chunk in 1:header$numberOfDataTypes) {
                    o <- ensembleStart[i] + header$dataOffset[chunk]
                    if (buf[o] == 0x00 & buf[1+o] == 0x00) {
                        ##slow if (i <= profilesToShow) oceDebug(debug, "  fixed leader skipped\n")
                    } else if (buf[o] == 0x80 & buf[1+o] == 0x00) {
                        ##slow if (i <= profilesToShow) oceDebug(debug, "  variable leader skipped\n")
                    } else if (buf[o] == 0x00 & buf[1+o] == 0x01) {
                        vtmp <- readBin(buf[o + 1 + seq(1, 2*items)], "integer", n=items, size=2, endian="little", signed=TRUE)
                        vtmp[-32768 == vtmp] <- NA       # blank out bad data
                        v[i, , ] <- matrix(velocityScale * vtmp, ncol=numberOfBeams, byrow=TRUE)
                        ##slow if (debug && i <= profilesToShow) oceDebug(debug, vectorShow(v[i, , 1], paste("  v[", i, ",,1]", sep="")))
                    } else if (buf[o] == 0x00 & buf[1+o] == 0x02) {
                        q[i, , ] <- matrix(buf[o + 1 + seq(1, items)], ncol=numberOfBeams, byrow=TRUE)
                        ##slow if (debug && i <= profilesToShow) oceDebug(debug, vectorShow(q[i, , 1], paste("  q[", i, ",,1]", sep="")))
                    } else if (buf[o] == 0x00 & buf[1+o] == 0x03) {
                        a[i, , ] <- matrix(buf[o + 1 + seq(1, items)], ncol=numberOfBeams, byrow=TRUE)
                        ##slow if (debug && i <= profilesToShow) oceDebug(debug, vectorShow(a[i, , 1], paste("  a[", i, ",,1]", sep="")))
                    } else if (buf[o] == 0x00 & buf[1+o] == 0x04) {
                        g[i, , ] <- matrix(buf[o + 1 + seq(1, items)], ncol=numberOfBeams, byrow=TRUE)
                        ##slow if (debug && i <= profilesToShow) oceDebug(debug, vectorShow(g[i, , 1], paste("  g[", i, ",,1]", sep="")))
                    } else if (buf[o] == 0x00 & buf[1+o] == 0x05) {
                        ##slow if (i <= profilesToShow) oceDebug(debug, "  status profile ignored\n")
                    } else if (buf[o] == 0x00 & buf[1+o] == 0x06) {
                        rangeLSB <- readBin(buf[o+c(16:23)], "integer", n=4, size=2, signed=FALSE, endian="little")
                        rangeMSB <- readBin(buf[o+77:80], "integer", n=4, size=1, signed=FALSE, endian="little")
                        if (is.null(br)) {
                            warning("cannot store bottom-track data for profile ", i, " because profile 1 lacked such data so no storage was set up (adp.rdi.R near line 964)\n")
                        } else {
                            br[i, ] <- 0.01 * (65536 * rangeMSB + rangeLSB)
                            bv[i, ] <- 0.001 * readBin(buf[o+c(24:31)], "integer", n=4, size=2, signed=TRUE, endian="little")
                            bc[i, ] <- as.integer(buf[o+32:35])
                            ba[i, ] <- as.integer(buf[o+36:39])
                            bg[i, ] <- as.integer(buf[o+40:43])
                            ##slow if (debug && i <= profilesToShow) {
                            ##slow     ## FIXME: speed things up by commenting this out when things are working
                            ##slow     oceDebug(debug, vectorShow(br[i, ], paste("br[", i, ",]", sep="")))
                            ##slow     oceDebug(debug, vectorShow(bv[i, ], paste("bv[", i, ",]", sep="")))
                            ##slow     oceDebug(debug, vectorShow(bc[i, ], paste("bc[", i, ",]", sep="")))
                            ##slow     oceDebug(debug, vectorShow(ba[i, ], paste("ba[", i, ",]", sep="")))
                            ##slow     oceDebug(debug, vectorShow(bg[i, ], paste("bg[", i, ",]", sep="")))
                            ##slow }
                        }
                    } else if (buf[o] == 0x00 & buf[1+o] == 0x20) {
                        ## message("navigation")
                        ## On the first profile, we set up space.
                        if (!VMDASStorageInitialized) {
                            VMDASStorageInitialized <- TRUE
                            oceDebug(debug, "This is a VMDAS file\n")
                            isVMDAS <- TRUE
                            ## FIXME: set up space; this c() method is slow and ugly
                            firstTime <- firstLongitude <- firstLatitude <- NULL
                            lastTime <- lastLongitude <- lastLatitude <- NULL
                            avgSpeed <- avgTrackTrue <- avgTrackMagnetic <- NULL
                            speedMadeGood <- speedMadeGoodNorth <- speedMadeGoodEast <- NULL
                            directionMadeGood <- NULL
                            shipPitch <- shipRoll <- shipHeading <- NULL
                            numberOfSpeedSamplesAveraged <- numberOfTrueTrackSamplesAveraged <-
                                numberOfMagneticTrackSamplesAveraged <- numberOfHeadingSamplesAveraged <-
                                    numberOfPitchRollSamplesAveraged <- NULL
                            avgTrueVelocityNorth <- avgTrueVelocityEast <- NULL
                            avgMagnitudeVelocityNorth <- avgMagnitudeVelocityEast <- NULL
                            primaryFlags <- NULL
                        } else {
                            if (!isVMDAS)
                                badVMDAS <- c(badVMDAS, i)
                        }
                        tmpTime <- as.numeric(ISOdatetime(as.integer(buf[o+4]) + 256*as.integer(buf[o+5]), #year
                                                          as.integer(buf[o+3]), #month
                                                          as.integer(buf[o+2]), #day
                                                          0, 0, 0,
                                                          tz=tz))
                        clockOffset <- 0.001 * readBin(buf[o+10:13], 'integer', n=1, size=4, endian='little')
                        firstTime <- c(firstTime, tmpTime + clockOffset+readBin(buf[o+6:9], 'integer', n=1, size=4, endian='little')/10000)
                        ##704 sNavTime <- as.POSIXct(sNavTime, origin='1970-01-01', tz=tz)
                        cfac <- 180/2^31 # from rdradcp.m line 825
                        ## FIXME: this c() operation is slow and inelegant
                        firstLatitude <- c(firstLatitude, readBin(buf[o+14:17], 'integer', n=1, size=4, endian='little')*cfac)
                        firstLongitude <- c(firstLongitude, readBin(buf[o+18:21], 'integer', n=1, size=4, endian='little')*cfac)
                        lastTime <- c(lastTime,   tmpTime + clockOffset+readBin(buf[o+22:25], 'integer', n=1, size=4, endian='little')/10000)
                        lastLatitude <- c(lastLatitude, readBin(buf[o+26:29], 'integer', n=1, size=4, endian='little')*cfac)
                        lastLongitude <- c(lastLongitude, readBin(buf[o+30:33], 'integer', n=1, size=4, endian='little')*cfac)
                        ## FIXME: DK: I need to figure out the difference between eNavTime and navTime
                        ## FIXME: CR: what you are calling navTime should be the same as the "ADCP time"
                        ## tmpTime <- ISOdatetime(as.integer(buf[o+54]) + 256*as.integer(buf[o+55]), #year
                        ##                        as.integer(buf[o+57]), #month
                        ##                        as.integer(buf[o+56]), #day
                        ##                        0, 0, 0,
                        ##                        tz=tz)
                        ## navTime <- c(navTime, tmpTime + readBin(buf[o+58:61], 'integer', n=1, size=4, endian='little')/100)
                        ##(A) navTime <- as.POSIXct(navTime, origin='1970-01-01', tz=tz)
                        avgSpeed <- c(avgSpeed, 0.001*readBin(buf[o+34:35], 'integer', n=1, size=2, endian='little'))
                        avgTrackTrue <- c(avgTrackTrue, readBin(buf[o+36:37], 'integer', n=1, size=2, endian='little'))
                        avgTrackMagnetic <- c(avgTrackMagnetic, readBin(buf[o+38:39], 'integer', n=1, size=2, endian='little'))
                        speedMadeGood <- c(speedMadeGood, 0.001*readBin(buf[o+40:41], 'integer', n=1, size=2, endian='little'))
                        directionMadeGood <- c(directionMadeGood, (360/2^16)*readBin(buf[o+42:43], 'integer', n=1, size=2, endian='little'))
                        shipPitch <- c(shipPitch,
                                       (360/2^16)*readBin(buf[o+62:63], 'integer', n=1, size=2, endian='little'))
                        shipRoll <- c(shipRoll,
                                      (360/2^16)*readBin(buf[o+64:65], 'integer', n=1, size=2, endian='little'))
                        shipHeading <- c(shipHeading,
                                         (360/2^16)*readBin(buf[o+66:67], 'integer', n=1, size=2, endian='little'))
                        numberOfSpeedSamplesAveraged <- c(numberOfSpeedSamplesAveraged,
                                                          readBin(buf[o+68:69], 'integer', n=1, size=2, endian='little'))
                        numberOfTrueTrackSamplesAveraged <- c(numberOfTrueTrackSamplesAveraged,
                                                              readBin(buf[o+70:71], 'integer', n=1, size=2, endian='little'))
                        numberOfMagneticTrackSamplesAveraged <- c(numberOfMagneticTrackSamplesAveraged,
                                                                  readBin(buf[o+72:73], 'integer', n=1, size=2, endian='little'))
                        numberOfHeadingSamplesAveraged <- c(numberOfHeadingSamplesAveraged,
                                                            readBin(buf[o+74:75], 'integer', n=1, size=2, endian='little'))
                        numberOfPitchRollSamplesAveraged <- c(numberOfPitchRollSamplesAveraged,
                                                              readBin(buf[o+76:77], 'integer', n=1, size=2, endian='little'))
                        avgTrueVelocityNorth <- c(avgTrueVelocityNorth,
                                                  readBin(buf[o+78:79], 'integer', n=1, size=2, endian='little'))
                        avgTrueVelocityEast <- c(avgTrueVelocityEast,
                                                 readBin(buf[o+80:81], 'integer', n=1, size=2, endian='little'))
                        avgMagnitudeVelocityNorth <- c(avgMagnitudeVelocityNorth,
                                                       readBin(buf[o+82:83], 'integer', n=1, size=2, endian='little'))
                        avgMagnitudeVelocityEast <- c(avgMagnitudeVelocityEast,
                                                      readBin(buf[o+84:85], 'integer', n=1, size=2, endian='little'))
                        speedMadeGoodNorth <- c(speedMadeGoodNorth,
                                                0.001*readBin(buf[o+86:87], 'integer', n=1, size=2, endian='little'))
                        speedMadeGoodEast <- c(speedMadeGoodEast,
                                               0.001*readBin(buf[o+88:89], 'integer', n=1, size=2, endian='little'))
                        primaryFlags <- c(primaryFlags,
                                          readBin(buf[o+90:91], 'integer', n=1, size=2, endian='little'))
                        ##slow if (i <= profilesToShow) oceDebug(debug, "Navigaiton, profile", i, "\n")
                    } else if (buf[o] == 0x00 & buf[1+o] == 0x30) {
                        ##slow if (i <= profilesToShow) oceDebug(debug, "Fixed attitude, profile", i, "\n")
                    } else if (buf[1+o] == 0x30) {
                        ## fixme need to check first byte
                        ##slow if (i <= profilesToShow) oceDebug(debug, "Variable attitude, profile", i, "\n")
                    } else if (buf[o] == 0x00 & buf[1+o] == 0x0a) {
                        ## vertical beam data
                        if (isSentinel) {
                            vtmp <- readBin(buf[o + 1 + seq(1, 2*vItems)], "integer", n=vItems, size=2, endian="little", signed=TRUE)
                            vtmp[-32768 == vtmp] <- NA       # blank out bad data
                            vv[i, ] <- velocityScale * vtmp
                            ##slow if (debug && i <= profilesToShow) cat(vectorShow(vv[i, ], paste("vv[", i, ",]", sep="")))
                        } else {
                            warning("Detected vertical beam data chunk, but this is not a SentinelV\n")
                        }
                    } else if (buf[o] == 0x00 & buf[1+o] == 0x0c) {
                        ## vertical beam amplitude
                        if (isSentinel) {
                            va[i, ] <- buf[o + 1 + seq(1, vItems)]
                            ##slow if (debug && i <= profilesToShow) cat(vectorShow(va[i, ], paste("va[", i, ",]", sep="")))
                        } else {
                            warning("Detected vertical beam amplitude chunk, but this is not a SentinelV\n")
                        }
                    } else if (buf[o] == 0x00 & buf[1+o] == 0x0b) {
                        ## vertical beam correlation
                        if (isSentinel) {
                            vq[i, ] <- buf[o + 1 + seq(1, vItems)]
                            ##slow if (debug && i <= profilesToShow) cat(vectorShow(vq[i, ], paste("vq[", i, ",]", sep="")))
                        } else {
                            warning("Detected vertical beam correlation chunk, but this is not a SentinelV\n")
                        }
                    } else if (buf[o] == 0x00 & buf[1+o] == 0x0d) {
                        ## vertical beam percent good
                        if (isSentinel) {
                            vg[i, ] <- buf[o + 1 + seq(1, vItems)]
                            ##slow if (debug && i <= profilesToShow) cat(vectorShow(vg[i, ], paste("vg[", i, ",]", sep="")))
                        } else {
                            warning("Detected vertical beam percent good chunk, but this is not a SentinelV\n")
                        }
                    } else {
                        ## FIXME: maybe should handle all possible combinations here. But
                        ## FIXME: how could we know the possibilities? I've seen the following
                        ## FIXME: in a winriver file file from CR:
                        ## FIXME: 0x00 0x37
                        ## FIXME: 0x01 0x21
                        ## FIXME: 0x02 0x21
                        ## FIXME: 0x03 0x21
                        ## FIXME: ... and over 1000 more. These cannot be real codes, surely.
                        ## FIXME: So, for now, let's just ignore unknown codes.
                        ##> warning("unknown buf[o]=0x", buf[o], " and buf[1+o]=0x", buf[1+o])
                    }
                    if (monitor)
                        setTxtProgressBar(progressBar, i)
                }
                if (monitor)
                    close(progressBar)

                ##20170112 if (FALSE) {
                ##20170112     stop("CANNOT GET HERE -- report to the developer named Dan Kelley please")
                ##20170112     ## FIXME
                ##20170112     o <- profileStart[i] + header$dataOffset[3] - header$dataOffset[2] # 65 for workhorse; 50 for surveyor
                ##20170112     ## Process data chunks, detecting each type by the second byte in the chunk; the
                ##20170112     ## first byte must be 0x00. The second-byte codes are given in
                ##20170112     ## teledyne2014ostm(Table 33, page 146)

                ##20170112     for (dataType in 1:header$numberOfDataTypes) {
                ##20170112         if (buf[o] != 0x00)
                ##20170112             stop("Expecting byte 0x00 but got byte 0x", buf[o], " while trying to read a data chunk for profile ", i)
                ##20170112         oceDebug(debug, "buf[", o, "+1]: ", buf[o+1], "\n")
                ##20170112         if (buf[o+1] == 0x01) {
                ##20170112             oceDebug(debug, "VELOCITY\n")
                ##20170112             ##>message("velo at o=", o, "; profile=", i)
                ##20170112             vv <- readBin(buf[o + 1 + seq(1, 2*items)], "integer", n=items, size=2, endian="little", signed=TRUE)
                ##20170112             ##cat(vectorShow(vv, "vv:"))
                ##20170112             vv[-32768 == vv] <- NA       # blank out bad data
                ##20170112             v[i, , ] <- matrix(velocityScale * vv, ncol=numberOfBeams, byrow=TRUE)
                ##20170112             o <- o + items * 2 + 2 # point to next chunk
                ##20170112             if (debug && i <= profilesToShow) cat(vectorShow(v[i, , ], paste("v[", i, ",,]", sep="")))
                ##20170112         } else if (buf[o+1] == 0x02) {
                ##20170112             oceDebug(debug, "CORRELATION\n")
                ##20170112             q[i, , ] <- matrix(buf[o + 1 + seq(1, items)], ncol=numberOfBeams, byrow=TRUE)
                ##20170112             if (debug && i <= profilesToShow) cat(vectorShow(q[i, , ], paste("q[", i, ",,]", sep="")))
                ##20170112             o <- o + items + 2 # point to next chunk
                ##20170112         } else if (buf[o+1] == 0x03) {
                ##20170112             oceDebug(debug, "ECHO INTENSITY\n")
                ##20170112             a[i, , ] <- matrix(buf[o + 1 + seq(1, items)], ncol=numberOfBeams, byrow=TRUE)
                ##20170112             if (debug && i <= profilesToShow) cat(vectorShow(a[i, , ], paste("a[", i, ",,]", sep="")))
                ##20170112             o <- o + items + 2 # point to next chunk
                ##20170112         } else if (buf[o+1] == 0x04) {
                ##20170112             oceDebug(debug, "PERCENT GOOD\n")
                ##20170112             g[i, , ] <- matrix(buf[o + 1 + seq(1, items)], ncol=numberOfBeams, byrow=TRUE)
                ##20170112             if (debug && i <= profilesToShow) cat(vectorShow(g[i, , ], paste("g[", i, ",,]", sep="")))
                ##20170112             o <- o + items + 2
                ##20170112         } else if (buf[o+1] == 0x05) {
                ##20170112             o <- o + 2 + items
                ##20170112             ## FIXME do something with these STATUS data
                ##20170112             if (debug && i <= profilesToShow) cat("skipping ", 2 + items, " bytes for STATUS data (FIXME: not stored)\n")
                ##20170112         } else if (buf[o+1] == 0x06) {
                ##20170112             ## bottom-track
                ##20170112             if (is.null(br)) {
                ##20170112                 warning("cannot store bottom-track data for profile ", i, " because profile 1 lacked such data so no storage was set up (adp.rdi.R near line 1146)\n")
                ##20170112             } else {
                ##20170112                 ## the bottom range is in 3 bytes, split into two chunks
                ##20170112                 rangeLSB <- readBin(buf[o+c(16:23)], "integer", n=4, size=2, signed=FALSE, endian="little")
                ##20170112                 rangeMSB <- readBin(buf[o+77:80], "integer", n=4, size=1, signed=FALSE, endian="little")
                ##20170112                 br[i, ] <- 0.01 * (65536 * rangeMSB + rangeLSB)
                ##20170112                 bv[i, ] <- 0.001 * readBin(buf[o+c(24:31)], "integer", n=4, size=2, signed=TRUE, endian="little")
                ##20170112                 bc[i, ] <- as.integer(buf[o+32:35])
                ##20170112                 ba[i, ] <- as.integer(buf[o+36:39])
                ##20170112                 bg[i, ] <- as.integer(buf[o+40:43])
                ##20170112                 o <- o + 81 ## BOTTOM data chunk is always 81 bytes (Fig 46, p145 teledyne2014ostm)
                ##20170112                 if (debug && i <= profilesToShow) {
                ##20170112                     ## FIXME: speed things up by commenting this out when things are working
                ##20170112                     cat(vectorShow(br[i, ], paste("br[", i, ",]", sep="")))
                ##20170112                     cat(vectorShow(bv[i, ], paste("bv[", i, ",]", sep="")))
                ##20170112                     cat(vectorShow(bc[i, ], paste("bc[", i, ",]", sep="")))
                ##20170112                     cat(vectorShow(ba[i, ], paste("ba[", i, ",]", sep="")))
                ##20170112                     cat(vectorShow(bg[i, ], paste("bg[", i, ",]", sep="")))
                ##20170112                 }
                ##20170112             }
                ##20170112         } else if (buf[o+1] == 0x20) {
                ##20170112             ## navigation
                ##20170112             oceDebug(debug, "found NAVIGATION block (buf[o+1] is 0x20)")
                ##20170112             ## On the first profile, we set up space.
                ##20170112             if (!VMDASStorageInitialized) {
                ##20170112                 VMDASStorageInitialized <- TRUE
                ##20170112                 oceDebug(debug, "This is a VMDAS file\n")
                ##20170112                 isVMDAS <- TRUE
                ##20170112                 ## FIXME: set up space; this c() method is slow and ugly
                ##20170112                 firstTime <- firstLongitude <- firstLatitude <- NULL
                ##20170112                 lastTime <- lastLongitude <- lastLatitude <- NULL
                ##20170112                 avgSpeed <- avgTrackTrue <- avgTrackMagnetic <- NULL
                ##20170112                 speedMadeGood <- speedMadeGoodNorth <- speedMadeGoodEast <- NULL
                ##20170112                 directionMadeGood <- NULL
                ##20170112                 shipPitch <- shipRoll <- shipHeading <- NULL
                ##20170112                 numberOfSpeedSamplesAveraged <- numberOfTrueTrackSamplesAveraged <-
                ##20170112                     numberOfMagneticTrackSamplesAveraged <- numberOfHeadingSamplesAveraged <-
                ##20170112                         numberOfPitchRollSamplesAveraged <- NULL
                ##20170112                 avgTrueVelocityNorth <- avgTrueVelocityEast <- NULL
                ##20170112                 avgMagnitudeVelocityNorth <- avgMagnitudeVelocityEast <- NULL
                ##20170112                 primaryFlags <- NULL
                ##20170112             } else {
                ##20170112                 if (!isVMDAS)
                ##20170112                     badVMDAS <- c(badVMDAS, i)
                ##20170112             }
                ##20170112             tmpTime <- as.numeric(ISOdatetime(as.integer(buf[o+4]) + 256*as.integer(buf[o+5]), #year
                ##20170112                                               as.integer(buf[o+3]), #month
                ##20170112                                               as.integer(buf[o+2]), #day
                ##20170112                                               0, 0, 0,
                ##20170112                                               tz=tz))
                ##20170112             clockOffset <- 0.001 * readBin(buf[o+10:13], 'integer', n=1, size=4, endian='little')
                ##20170112             firstTime <- c(firstTime, tmpTime + clockOffset+readBin(buf[o+6:9], 'integer', n=1, size=4, endian='little')/10000)
                ##20170112             ##704 sNavTime <- as.POSIXct(sNavTime, origin='1970-01-01', tz=tz)
                ##20170112             cfac <- 180/2^31 # from rdradcp.m line 825
                ##20170112             ## FIXME: this c() operation is slow and inelegant
                ##20170112             firstLatitude <- c(firstLatitude, readBin(buf[o+14:17], 'integer', n=1, size=4, endian='little')*cfac)
                ##20170112             firstLongitude <- c(firstLongitude, readBin(buf[o+18:21], 'integer', n=1, size=4, endian='little')*cfac)
                ##20170112             lastTime <- c(lastTime,   tmpTime + clockOffset+readBin(buf[o+22:25], 'integer', n=1, size=4, endian='little')/10000)
                ##20170112             lastLatitude <- c(lastLatitude, readBin(buf[o+26:29], 'integer', n=1, size=4, endian='little')*cfac)
                ##20170112             lastLongitude <- c(lastLongitude, readBin(buf[o+30:33], 'integer', n=1, size=4, endian='little')*cfac)
                ##20170112             ## FIXME: DK: I need to figure out the difference between eNavTime and navTime
                ##20170112             ## FIXME: CR: what you are calling navTime should be the same as the "ADCP time"
                ##20170112             ## tmpTime <- ISOdatetime(as.integer(buf[o+54]) + 256*as.integer(buf[o+55]), #year
                ##20170112             ##                        as.integer(buf[o+57]), #month
                ##20170112             ##                        as.integer(buf[o+56]), #day
                ##20170112             ##                        0, 0, 0,
                ##20170112             ##                        tz=tz)
                ##20170112             ## navTime <- c(navTime, tmpTime + readBin(buf[o+58:61], 'integer', n=1, size=4, endian='little')/100)
                ##20170112             ##(A) navTime <- as.POSIXct(navTime, origin='1970-01-01', tz=tz)
                ##20170112             avgSpeed <- c(avgSpeed, 0.001*readBin(buf[o+34:35], 'integer', n=1, size=2, endian='little'))
                ##20170112             avgTrackTrue <- c(avgTrackTrue, readBin(buf[o+36:37], 'integer', n=1, size=2, endian='little'))
                ##20170112             avgTrackMagnetic <- c(avgTrackMagnetic, readBin(buf[o+38:39], 'integer', n=1, size=2, endian='little'))
                ##20170112             speedMadeGood <- c(speedMadeGood, 0.001*readBin(buf[o+40:41], 'integer', n=1, size=2, endian='little'))
                ##20170112             directionMadeGood <- c(directionMadeGood, (360/2^16)*readBin(buf[o+42:43], 'integer', n=1, size=2, endian='little'))
                ##20170112             shipPitch <- c(shipPitch,
                ##20170112                            (360/2^16)*readBin(buf[o+62:63], 'integer', n=1, size=2, endian='little'))
                ##20170112             shipRoll <- c(shipRoll,
                ##20170112                           (360/2^16)*readBin(buf[o+64:65], 'integer', n=1, size=2, endian='little'))
                ##20170112             shipHeading <- c(shipHeading,
                ##20170112                              (360/2^16)*readBin(buf[o+66:67], 'integer', n=1, size=2, endian='little'))
                ##20170112             numberOfSpeedSamplesAveraged <- c(numberOfSpeedSamplesAveraged,
                ##20170112                                               readBin(buf[o+68:69], 'integer', n=1, size=2, endian='little'))
                ##20170112             numberOfTrueTrackSamplesAveraged <- c(numberOfTrueTrackSamplesAveraged,
                ##20170112                                                   readBin(buf[o+70:71], 'integer', n=1, size=2, endian='little'))
                ##20170112             numberOfMagneticTrackSamplesAveraged <- c(numberOfMagneticTrackSamplesAveraged,
                ##20170112                                                       readBin(buf[o+72:73], 'integer', n=1, size=2, endian='little'))
                ##20170112             numberOfHeadingSamplesAveraged <- c(numberOfHeadingSamplesAveraged,
                ##20170112                                                 readBin(buf[o+74:75], 'integer', n=1, size=2, endian='little'))
                ##20170112             numberOfPitchRollSamplesAveraged <- c(numberOfPitchRollSamplesAveraged,
                ##20170112                                                   readBin(buf[o+76:77], 'integer', n=1, size=2, endian='little'))
                ##20170112             avgTrueVelocityNorth <- c(avgTrueVelocityNorth,
                ##20170112                                       readBin(buf[o+78:79], 'integer', n=1, size=2, endian='little'))
                ##20170112             avgTrueVelocityEast <- c(avgTrueVelocityEast,
                ##20170112                                      readBin(buf[o+80:81], 'integer', n=1, size=2, endian='little'))
                ##20170112             avgMagnitudeVelocityNorth <- c(avgMagnitudeVelocityNorth,
                ##20170112                                            readBin(buf[o+82:83], 'integer', n=1, size=2, endian='little'))
                ##20170112             avgMagnitudeVelocityEast <- c(avgMagnitudeVelocityEast,
                ##20170112                                           readBin(buf[o+84:85], 'integer', n=1, size=2, endian='little'))
                ##20170112             speedMadeGoodNorth <- c(speedMadeGoodNorth,
                ##20170112                                     0.001*readBin(buf[o+86:87], 'integer', n=1, size=2, endian='little'))
                ##20170112             speedMadeGoodEast <- c(speedMadeGoodEast,
                ##20170112                                    0.001*readBin(buf[o+88:89], 'integer', n=1, size=2, endian='little'))
                ##20170112             primaryFlags <- c(primaryFlags,
                ##20170112                               readBin(buf[o+90:91], 'integer', n=1, size=2, endian='little'))
                ##20170112             o <- o + 78 ## NAVIGATION data chunk is always 78 bytes (Fig 46, p145 teledyne2014ostm)
                ##20170112         } else if (buf[o+1] == 0x30) {
                ##20170112             ## FIXED ATTITUDE
                ##20170112             oceDebug(debug, "FIXED ATTITUDE\n")
                ##20170112             o <- o + 41 ## FIXED ATTITUDE data chunk is always 41 bytes (Fig 46, p145 teledyne2014ostm)
                ##20170112             warning("skipping BINARY FIXED ATTITUDE data chunk, profile ", i)
                ##20170112         } else {
                ##20170112             oceDebug(debug, "0x", buf[o+1], " flag... unknown. FIXME: handle 0x40-0xF0 0x30 (p146 teledynRDI 2014)\n")
                ##20170112             oceDebug(debug, "also, how far should we skip forward?\n")
                ##20170112             stop("unknown byte code 0x", buf[o+1], " encountered while trying to read profile ", i)
                ##20170112         }
                ##20170112     }
                ##20170112 } ## this whole block is being skipped and will be deleted later
                ##VMDAS }
                ##VMDAS } else {
                ##VMDAS     badProfiles <- c(badProfiles, i)
                ##VMDAS     if (monitor) {
                ##VMDAS         cat("X", ...)
                ##VMDAS         if (!(i %% 50))
                ##VMDAS             cat(i, "\n", ...)
                ##VMDAS     }
                ##VMDAS }
                ##if (o >= fileSize) {
                if (o >= bufSize) {
                    warning("got to end of file; o=", o, ", fileSize=", bufSize, "\n")
                    break
                }
            }
            ## time <- ISOdatetime(unabbreviateYear(as.integer(buf[profileStart+4])), # year
            ##                     as.integer(buf[profileStart+5]),      # month
            ##                     as.integer(buf[profileStart+6]),      # day
            ##                     as.integer(buf[profileStart+7]),      # hour
            ##                     as.integer(buf[profileStart+8]),      # minute
            ##                     as.integer(buf[profileStart+9])+0.01*as.integer(buf[profileStart+10]), # decimal second
            ##                     tz=tz)
            time <- as.POSIXct(ldc$time + 0.01 * as.numeric(ldc$sec100), origin="1970-01-01")

            ## Identify "junk" profiles by NA times
            junkProfiles <- which(is.na(time))
            if (isVMDAS) {
                #navTime <- as.POSIXct(navTime, origin='1970-01-01', tz=tz)
                firstTime <- firstTime + as.POSIXct("1970-01-01 00:00:00", tz=tz)
                lastTime <- lastTime + as.POSIXct("1970-01-01 00:00:00", tz=tz)
            }
            if (length(badProfiles) > 0) {
                ## remove NAs in time (not sure this is right, but it prevents other problems)
                ## FIXME: won't we need to handle VmDas here, also?
                t0 <- time[match(1, !is.na(time))] # FIXME: should test if any
                time <- fillGap(as.numeric(time) - as.numeric(t0)) + t0
                nbad <- length(badProfiles)
                if (nbad == 1)
                    warning("Interpolated across a bad profile at time ", format(time[badProfiles]),
                            ".  (\"Bad\" means that the expected byte code for a velocity segment, 0x00 0x01, was not found 65 bytes after the start of a profile, the latter indicated by the byte sequence 0x80 0x00.)")
                else
                    warning("Interpolated across ", length(badProfiles), " bad profile(s) at times: ",
                            paste(format(time[badProfiles]), collapse=", "),
                            ".  (\"Bad\" means that the expected byte code for a velocity segment, 0x00 0x01, was not found 65 bytes after the start of a profile, the latter indicated by the byte sequence 0x80 0x00.)")
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
            oceDebug(debug, "will adjust the pitch as explained on page 14 of 'adcp coordinate transformation.pdf'\n")
            oceDebug(debug, vectorShow(pitch, "pitch, before correction"))
            pitch <- 180 / pi * atan(tan(pitch * pi / 180) / cos(roll * pi / 180)) # correct the pitch (see ACT page 14)
            oceDebug(debug, vectorShow(pitch, "pitch, after correction"))
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
            res <- new('adp')
            for (name in names(header))
                res@metadata[[name]] <- header[[name]]
            res@metadata$manufacturer <- "rdi"
            res@metadata$instrumentType <- "adcp"
            res@metadata$filename <- filename
            res@metadata$longitude <- longitude
            res@metadata$latitude <- latitude
            res@metadata$velocityResolution <- velocityScale
            res@metadata$velocityMaximum <- velocityScale * 2^15
            res@metadata$numberOfSamples <- dim(v)[1]
            res@metadata$numberOfCells <- dim(v)[2]
            res@metadata$numberOfBeams <- dim(v)[3]
            ##res@metadata$measurementStart <- measurementStart
            ##res@metadata$measurementEnd <- measurementEnd
            ##res@metadata$measurementDeltat <- measurementDeltat
            res@metadata$bin1Distance <- bin1Distance
            res@metadata$xmitPulseLength <- xmitPulseLength
            res@metadata$oceBeamUnspreaded <- FALSE
            res@metadata$oceCoordinate <- header$originalCoordinate
            res@metadata$depthMean <- mean(depth, na.rm=TRUE)
            if (isSentinel) {
                if (exists('transformationMatrix')) {
                    res@metadata$transformationMatrix <- transformationMatrix
                } else {
                    warning('Sentinel file detected but no transformation matrix found.')
                }
            } else {
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
                tm.c <- if (res@metadata$beamPattern == "convex") 1 else -1; # control sign of first 2 rows of transformationMatrix
                tm.a <- 1 / (2 * sin(res@metadata$beamAngle * pi / 180))
                tm.b <- 1 / (4 * cos(res@metadata$beamAngle * pi / 180))
                tm.d <- tm.a / sqrt(2)
                res@metadata$transformationMatrix <- matrix(c(tm.c*tm.a, -tm.c*tm.a,          0,         0,
                                                                      0,          0, -tm.c*tm.a, tm.c*tm.a,
                                                                   tm.b,       tm.b,       tm.b,      tm.b,
                                                                   tm.d,       tm.d,      -tm.d,     -tm.d),
                                                            nrow=4, byrow=TRUE)
            }
            if (monitor)
                cat("\nFinished reading ", profilesToRead, " profiles from \"", filename, "\"\n", sep="")

           ## Sometimes a non-VMDAS file will have some profiles that have the VMDAS flag.
           ## It is not clear why this happens, but in any case, provide a warning.
           nbadVMDAS <- length(badVMDAS)
           if (nbadVMDAS > 0) {
               if (1==nbadVMDAS) {
                   warning("erroneous VMDAS flag in profile ", badVMDAS)
               } else if (nbadVMDAS < 4) {
                   warning("erroneous VMDAS flag in profiles: ", paste(badVMDAS, collapse=" "))
               } else {
                   warning("erroneous VMDAS flag in ", nbadVMDAS, " profiles, including: ", badVMDAS[1], " ",
                           badVMDAS[2], " ... ", badVMDAS[nbadVMDAS-1], " ",
                           badVMDAS[nbadVMDAS], "\n")
               }
           }
           class(time) <- c("POSIXct", "POSIXt")
           attr(time, "tzone") <- getOption("oceTz")
           if (bFound && !isVMDAS) {
               oceDebug(debug, "creating data slot for a file with bFound&&!isVMDAS\n")
               br[br == 0.0] <- NA    # clean up (not sure if needed)
               ## issue1228
               res@data <- list(v=v, q=q, a=a, g=g,
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
           } else if (bFound && isVMDAS) {
               oceDebug(debug, "creating data slot for a file with bFound&&isVMDAS\n")
               br[br == 0.0] <- NA    # clean up (not sure if needed)
               res@data <- list(v=v, q=q, a=a, g=g,
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
                                ## Next are as described starting on p77 of VmDas_Users_Guide_May12.pdf
                                avgSpeed=avgSpeed,
                                avgMagnitudeVelocityEast=avgMagnitudeVelocityEast,
                                avgMagnitudeVelocityNorth=avgMagnitudeVelocityNorth,
                                avgTrackMagnetic=avgTrackMagnetic,
                                avgTrackTrue=avgTrackTrue,
                                avgTrueVelocityEast=avgTrueVelocityEast,
                                avgTrueVelocityNorth=avgTrueVelocityNorth,
                                directionMadeGood=directionMadeGood,
                                firstLatitude=firstLatitude,
                                firstLongitude=firstLongitude,
                                firstTime=firstTime,
                                lastLatitude=lastLatitude,
                                lastLongitude=lastLongitude,
                                lastTime=lastTime,
                                numberOfHeadingSamplesAveraged=numberOfHeadingSamplesAveraged,
                                numberOfMagneticTrackSamplesAveraged=numberOfMagneticTrackSamplesAveraged,
                                numberOfPitchRollSamplesAveraged=numberOfPitchRollSamplesAveraged,
                                numberOfSpeedSamplesAveraged=numberOfSpeedSamplesAveraged,
                                numberOfTrueTrackSamplesAveraged=numberOfTrueTrackSamplesAveraged,
                                primaryFlags=primaryFlags,
                                shipHeading=shipHeading,
                                shipPitch=shipPitch,
                                shipRoll=shipRoll,
                                speedMadeGood=speedMadeGood,
                                speedMadeGoodEast=speedMadeGoodEast,
                                speedMadeGoodNorth=speedMadeGoodNorth)
           } else if (isSentinel) {
               oceDebug(debug, "creating data slot for a SentinelV file\n")
               res@data <- list(v=v, q=q, a=a, g=g,
                                vv=vv, vq=vq, va=va, vg=vg,
                                vdistance=seq(firstCellRange, b=depthCellSize, length.out=numberOfVCells),
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
           } else {
               oceDebug(debug, "creating data slot for a non-SentinelV file\n")
               res@data <- list(v=v, q=q, a=a, g=g,
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
        } else {
            warning("There are no profiles in this file.")
            for (name in names(header))
                res@metadata[[name]] <- header[[name]]
            res@metadata$filename <- filename
            res@data <- NULL
        }
    } else {
        warning("The header indicates that there are no profiles in this file.")
        for (name in names(header))
            res@metadata[[name]] <- header[[name]]
        res@metadata$filename <- filename
        res@data <- NULL
    }

    ## Remove "junk" profiles
    if (length(junkProfiles) > 0) {
        ## remove all data from the profile
        for (field in names(res@data)) {
            if (!(field %in% c('distance', 'vdistance'))) {
                dim <- dim(res@data[[field]])
                if (is.null(dim)) {
                    res@data[[field]] <- res@data[[field]][-junkProfiles]
                } else if (length(dim) == 2) {
                    res@data[[field]] <- res@data[[field]][-junkProfiles, ]
                } else if (length(dim) == 3) {
                    res@data[[field]] <- res@data[[field]][-junkProfiles, , ]
                }
            }
        }
        njunk <- length(junkProfiles)
        if (njunk == 1)
            warning("Removed a junk profile at time ",
                    format(time[junkProfiles-1]),
                    ".  (\"Junk\" means that the decoded time was NA and all data fields were garbage)")
        else
            warning("Removed ", length(junkProfiles),
                    " junk profile(s) at times: ",
                    paste(format(time[junkProfiles-1]), collapse=", "),
                    ".  (\"Junk\" means that the decoded time was NA and all data fields were garbage)")
    }

    res@metadata$manufacturer <- "teledyne rdi"
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    hitem <- processingLogItem(processingLog)
    res@processingLog <- unclass(hitem)
    res@metadata$units$v <- list(unit=expression(m/s), scale="")
    res@metadata$units$distance <- list(unit=expression(m), scale="")
    res@metadata$units$pressure <- list(unit=expression(dbar), scale="")
    res@metadata$units$salinity <- list(unit=expression(), scale="PSS-78")
    res@metadata$units$temperature <- list(unit=expression(degree*C), scale="ITS-90")
    res@metadata$units$soundSpeed <- list(unit=expression(m/s), scale="")
    res@metadata$units$heading <- list(unit=expression(degree), scale="")
    res@metadata$units$pitch <- list(unit=expression(degree), scale="")
    res@metadata$units$roll <- list(unit=expression(degree), scale="")
    res@metadata$units$headingStd <- list(unit=expression(degree), scale="")
    res@metadata$units$pitchStd <- list(unit=expression(degree), scale="")
    res@metadata$units$rollStd <- list(unit=expression(degree), scale="")
    res@metadata$units$attitude <- list(unit=expression(degree), scale="")
    res@metadata$units$depth <- list(unit=expression(m), scale="")
    oceDebug(debug, "} # read.adp.rdi()\n", unindent=1)
    res
}
