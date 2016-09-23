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
    ## if (beamAngle < 19 || 21 < beamAngle)
    ##     warning("expecting a beamAngle of 20 deg [more-or-less standard for RDI] but got ", beamAngle, "deg; using the latter in the transformationMatrix")
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
    ##systemPower <- readBin(FLD[53], "integer", n=1, size=1)
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
    ## There really seems to be nothing specific in the file to tell instrument type, so, in an act of
    ## desparation (or is that hope) I'm going to flag on the one thing that was clearly stated, and
    ## clearly different, in the two documentation entries.
    if (FLDLength == 59) {
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
                1600 m/s.  Something went wrong in decoding the data.")
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
    oceDebug(debug, "} # decodeHeaderRDI()\n", unindent=1)
    res
}                                       # decodeHeaderRDI


#' Read a Teledyne/RDI ADP File
#'
#' Read a Teledyne/RDI ADCP file (called 'adp' in oce).
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
#' @param testing Logical value, indicating whether 
#' the time-varying device orientation is to be inferred from the
#' per-profile header information, and the boolean result is stored in an
#' integer vector named \code{upward} within the \code{data} slot.
#' @param type A character string indicating the type of instrument.
#' @template adpTemplate
#' @param despike if \code{TRUE}, \code{\link{despike}} will be used to clean
#' anomalous spikes in heading, etc.
#' @author Dan Kelley and Clark Richards
#' @references
#' 1. Teledyne-RDI, 2007. \emph{WorkHorse commands and output data
#' format.} P/N 957-6156-00 (November 2007).  (Section 5.3 h details the binary
#' format, e.g. the file should start with the byte \code{0x7f} repeated twice,
#' and each profile starts with the bytes \code{0x80}, followed by \code{0x00},
#' followed by the sequence number of the profile, represented as a
#' little-endian two-byte short integer.  \code{read.adp.rdi()} uses these
#' sequences to interpret data files.)
#' @family things related to \code{adp} data
read.adp.rdi <- function(file, from=1, to, by=1, tz=getOption("oceTz"),
                         longitude=NA, latitude=NA,
                         type=c("workhorse"),
                         monitor=FALSE, despike=FALSE, processingLog,
                         testing=FALSE,
                         debug=getOption("oceDebug"),
                         ...)
{
    oceDebug(debug, "read.adp.rdi(...,from=",format(from),
             ",to=",if(missing(to)) "missing" else format(to), "...) {\n", unindent=1)
    profileStart <- NULL # prevent scope warning from rstudio; defined later anyway
    bisectAdpRdi <- function(buf, t.find, add=0, debug=0) {
        oceDebug(debug, "bisectAdpRdi(t.find=", format(t.find), ", add=", add, ") {\n", unindent=1)
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
        oceDebug(debug, "} # bisectAdpRdi()\n", unindent=1)
        return(list(index=middle, time=t))
    }
    gaveFromTo <- !missing(from) && !missing(to)
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

        ## Profiles start at the VARIABLE LEADER DATA, since there is no point in
        ## re-interpreting the HEADER and the FIXED LEADER DATA over and over,
        ## but we need the VLD to get times, etc. I *think* we can assume a fixed
        ## location for these, based on the "Always Output" indication in Fig 46
        ## on page 145 of teledyne2014ostm.
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
                fromPair <- bisectAdpRdi(buf, from, add=-1, debug=debug-1)
                from <- fromIndex <- fromPair$index
                toPair <- bisectAdpRdi(buf, to, add=1, debug=debug-1)
                to <- toIndex <- toPair$index
                oceDebug(debug, "from:", format(fromPair$t), " yields profileStart[", fromIndex, "]\n")
                oceDebug(debug, "to:", format(toPair$t), "yields profileStart[", toIndex, "]\n")
                oceDebug(debug, "by:", by, "(not yet decoded)\n")
                oceDebug(debug, "head(profileStart):", paste(head(profileStart), collapse=" "), "\n")
                oceDebug(debug, "tail(profileStart):", paste(tail(profileStart), collapse=" "), "\n")
                oceDebug(debug, "'from' is profileStart[", fromPair$index, "]:", profileStart[fromPair$index], "at time", format(fromPair$t), "\n")
                oceDebug(debug, "'to' is profileStart[", toPair$index, "]:", profileStart[toPair$index], "at time", format(toPair$t), "\n")
                dt <- measurementDeltat
                oceDebug(debug, "dt:", dt, "s, by:", by,"\n")
                if (is.character(by))
                    by <- floor(0.5 + ctimeToSeconds(by) / dt)
                oceDebug(debug, "by:",by,"profiles (after decoding)\n")
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
            oceDebug(debug, "filename: \"",filename,"\"\n", sep="")
            oceDebug(debug, "profilesToRead:",profilesToRead,"\n")
            oceDebug(debug, "numberOfBeams:",numberOfBeams,"\n")
            oceDebug(debug, "numberOfCells:",numberOfCells,"\n")

            ##20151121 if (testing) {
            ##20151121     nensembles <- length(ensembleStart)
            ##20151121     numberOfDataTypes <- readBin(buf[ensembleStart[1] + 5], "integer", n=1, size=1) # Note: just using first one
            ##20151121     FLDStart <- ensembleStart + 6 + 2 * numberOfDataTypes
            ##20151121     ## FIXME: decide whether the code below is cleaner than the spot where time is determined
            ##20151121     ## VLDStart <- FLDStart + 59
            ##20151121     ## RTC.year <- unabbreviateYear(readBin(buf[VLDStart+4], "integer", n=nensembles, size=1))
            ##20151121     ## RTC.month <- readBin(buf[VLDStart+5], "integer", n=nensembles, size=1)
            ##20151121     ## RTC.day <- readBin(buf[VLDStart+6], "integer", n=nensembles, size=1)
            ##20151121     ## RTC.hour <- readBin(buf[VLDStart+7], "integer", n=nensembles, size=1)
            ##20151121     ## RTC.minute <- readBin(buf[VLDStart+8], "integer", n=nensembles, size=1)
            ##20151121     ## RTC.second <- readBin(buf[VLDStart+9], "integer", n=nensembles, size=1)
            ##20151121     ## RTC.hundredths <- readBin(buf[VLDStart+10], "integer", n=nensembles, size=1)
            ##20151121     ## time <- ISOdatetime(RTC.year, RTC.month, RTC.day, RTC.hour, RTC.minute, RTC.second + RTC.hundredths / 100, tz=tz)

            ##20151121     ## regarding the "4" below, see p 135 of WorkHorse_commands_data_format_AUG10.PDF,
            ##20151121     ## noting that we subtract 1 because it's an offset; we are thus examining
            ##20151121     ## the LSB of the "Sys Cfg" pair.
            ##20151121     upward <- .Call("get_bit", buf[FLDStart+4], 7)
            ##20151121     ##testingData <- list(time=time, upward=upward)
            ##20151121 }

            items <- numberOfBeams * numberOfCells

            ## set up storage
            codes <- cbind(buf[ensembleStart[1]+c(0,header$dataOffset)], buf[1+ensembleStart[1]+c(0,header$dataOffset)])
            oceDebug(debug, "below are the data-chunk codes; see Table 33 p145 of Teledyne/RDI OS_TM_Apr14.pdf\n")
            if (debug)
                print(codes)
            vFound <- sum(codes[,1]==0x00 & codes[,2]==0x01) # velo
            qFound <- sum(codes[,1]==0x00 & codes[,2]==0x02) # corr
            aFound <- sum(codes[,1]==0x00 & codes[,2]==0x03) # echo intensity
            gFound <- sum(codes[,1]==0x00 & codes[,2]==0x04) # percent good
            ##sFound <- sum(codes[,1]==0x00 & codes[,2]==0x05) # status
            bFound <- sum(codes[,1]==0x00 & codes[,2]==0x06) # bottom-track
            ##nFound <- sum(codes[,1]==0x00 & codes[,2]==0x20) # navigation
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

            ## Next line sets up empty vectors for VMDAS
            isVMDAS <- FALSE           # see below where we check bytes in first profile
            badVMDAS <- NULL           # erroneous VMDAS profiles

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

            profilesToShow <- 2 # only if debug>0

            for (i in 1:profilesToRead) {     # recall: these start at 0x80 0x00
                for (chunk in 1:header$numberOfDataTypes) {
                    o <- ensembleStart[i] + header$dataOffset[chunk]
                    if (i <= profilesToShow)
                        oceDebug(debug, "profile:", i, ", chunk:", chunk, ", buf: 0x", buf[o], " 0x", buf[1+o], "\n", sep="")
                    if (buf[o] == 0x00 & buf[1+o] == 0x00) {
                        if (i <= profilesToShow) oceDebug(debug, "Fixed leader skipped\n")
                    } else if (buf[o] == 0x80 & buf[1+o] == 0x00) {
                        if (i <= profilesToShow) oceDebug(debug, "Variable leader skipped\n")
                    } else if (buf[o] == 0x00 & buf[1+o] == 0x01) {
                        vv <- readBin(buf[o + 1 + seq(1, 2*items)], "integer", n=items, size=2, endian="little", signed=TRUE)
                        vv[vv==(-32768)] <- NA       # blank out bad data
                        v[i,,] <- matrix(velocityScale * vv, ncol=numberOfBeams, byrow=TRUE)
                        if (debug && i <= profilesToShow) cat(vectorShow(v[i,,1], paste("v[", i, ",,1]", sep="")))
                    } else if (buf[o] == 0x00 & buf[1+o] == 0x02) {
                        q[i,,] <- matrix(buf[o + 1 + seq(1, items)], ncol=numberOfBeams, byrow=TRUE)
                        if (debug && i <= profilesToShow) cat(vectorShow(q[i,,1], paste("q[", i, ",,1]", sep="")))
                    } else if (buf[o] == 0x00 & buf[1+o] == 0x03) {
                        a[i,,] <- matrix(buf[o + 1 + seq(1, items)], ncol=numberOfBeams, byrow=TRUE)
                        if (debug && i <= profilesToShow) cat(vectorShow(a[i,,1], paste("a[", i, ",,1]", sep="")))
                    } else if (buf[o] == 0x00 & buf[1+o] == 0x04) {
                        g[i,,] <- matrix(buf[o + 1 + seq(1, items)], ncol=numberOfBeams, byrow=TRUE)
                        if (debug && i <= profilesToShow) cat(vectorShow(g[i,,1], paste("g[", i, ",,1]", sep="")))
                    } else if (buf[o] == 0x00 & buf[1+o] == 0x05) {
                        if (i <= profilesToShow) oceDebug(debug, "Status profile is ignored\n")
                    } else if (buf[o] == 0x00 & buf[1+o] == 0x06) {
                        rangeLSB <- readBin(buf[o+c(16:23)], "integer", n=4, size=2, signed=FALSE, endian="little")
                        rangeMSB <- readBin(buf[o+77:80], "integer", n=4, size=1, signed=FALSE, endian="little")
                        br[i,] <- 0.01 * (65536 * rangeMSB + rangeLSB)
                        bv[i,] <- 0.001 * readBin(buf[o+c(24:31)], "integer", n=4, size=2, signed=TRUE, endian="little")
                        bc[i,] <- as.integer(buf[o+32:35])
                        ba[i,] <- as.integer(buf[o+36:39])
                        bg[i,] <- as.integer(buf[o+40:43])
                        if (debug && i <= profilesToShow) cat(vectorShow(br[i,], paste("br[", i, ",]", sep="")))
                        if (debug && i <= profilesToShow) cat(vectorShow(bv[i,], paste("bv[", i, ",]", sep="")))
                        if (debug && i <= profilesToShow) cat(vectorShow(bc[i,], paste("bc[", i, ",]", sep="")))
                        if (debug && i <= profilesToShow) cat(vectorShow(ba[i,], paste("ba[", i, ",]", sep="")))
                        if (debug && i <= profilesToShow) cat(vectorShow(bg[i,], paste("bg[", i, ",]", sep="")))
                    } else if (buf[o] == 0x00 & buf[1+o] == 0x20) {
                        ## message("navigation")
                        ## On the first profile, we set up space.
                        if (i == 1) {
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
                        firstTime <- c(firstTime, tmpTime + clockOffset+readBin(buf[o+6:9],'integer',n=1,size=4,endian='little')/10000)
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
                        if (i <= profilesToShow) oceDebug(debug, "Navigaiton, profile", i, "\n")
                    } else if (buf[o] == 0x00 & buf[1+o] == 0x30) {
                        if (i <= profilesToShow) oceDebug(debug, "Fixed attitude, profile", i, "\n")
                    } else if (buf[1+o] == 0x30) { ## fixme need to check first byte
                        if (i <= profilesToShow) oceDebug(debug, "Variable attitude, profile", i, "\n")
                    }
                }
                if (FALSE) { ### FIXME
                o <- profileStart[i] + header$dataOffset[3] - header$dataOffset[2] # 65 for workhorse; 50 for surveyor
                ## Process data chunks, detecting each type by the second byte in the chunk; the
                ## first byte must be 0x00. The second-byte codes are given in
                ## teledyne2014ostm(Table 33, page 146)
                for (dataType in 1:header$numberOfDataTypes) {
                    if (buf[o] != 0x00)
                        stop("Expecting byte 0x00 but got byte 0x", buf[o], " while trying to read a data chunk for profile ", i)
                    message("buf[", o, "+1]: ", buf[o+1])
                    if (buf[o+1] == 0x01) {
                        message("VELOCITY")
                        ##>message("velo at o=", o, "; profile=", i)
                        vv <- readBin(buf[o + 1 + seq(1, 2*items)], "integer", n=items, size=2, endian="little", signed=TRUE)
                        ##cat(vectorShow(vv, "vv:"))
                        vv[vv==(-32768)] <- NA       # blank out bad data
                        v[i,,] <- matrix(velocityScale * vv, ncol=numberOfBeams, byrow=TRUE)
                        o <- o + items * 2 + 2 # point to next chunk
                        if (debug && i <= profilesToShow) cat(vectorShow(v[i,,], paste("v[", i, ",,]", sep="")))
                    } else if (buf[o+1] == 0x02) {
                        message("CORRELATION")
                        q[i,,] <- matrix(buf[o + 1 + seq(1, items)], ncol=numberOfBeams, byrow=TRUE)
                        if (debug && i <= profilesToShow) cat(vectorShow(q[i,,], paste("q[", i, ",,]", sep="")))
                        o <- o + items + 2 # point to next chunk
                    } else if (buf[o+1] == 0x03) {
                        message("ECHO INTENSITY")
                        a[i,,] <- matrix(buf[o + 1 + seq(1, items)], ncol=numberOfBeams, byrow=TRUE)
                        if (debug && i <= profilesToShow) cat(vectorShow(a[i,,], paste("a[", i, ",,]", sep="")))
                        o <- o + items + 2 # point to next chunk
                    } else if (buf[o+1] == 0x04) {
                        message("PERCENT GOOD")
                        g[i,,] <- matrix(buf[o + 1 + seq(1, items)], ncol=numberOfBeams, byrow=TRUE)
                        if (debug && i <= profilesToShow) cat(vectorShow(g[i,,], paste("g[", i, ",,]", sep="")))
                        o <- o + items + 2
                    } else if (buf[o+1] == 0x05) {
                        message("STATUS")
                        o <- o + 2 + items
                        ## FIXME do something with these STATUS data
                        if (debug && i <= profilesToShow) cat("skipping ", 2 + items, " bytes for STATUS data (FIXME: not stored)\n")
                    } else if (buf[o+1] == 0x06) { # bottom-track
                        message("BOTTOM TRACK")
                        ## On the first profile, we set up space.
                        ## the bottom range is in 3 bytes, split into two chunks
                        rangeLSB <- readBin(buf[o+c(16:23)], "integer", n=4, size=2, signed=FALSE, endian="little")
                        rangeMSB <- readBin(buf[o+77:80], "integer", n=4, size=1, signed=FALSE, endian="little")
                        br[i,] <- 0.01 * (65536 * rangeMSB + rangeLSB)
                        bv[i,] <- 0.001 * readBin(buf[o+c(24:31)], "integer", n=4, size=2, signed=TRUE, endian="little")
                        bc[i,] <- as.integer(buf[o+32:35])
                        ba[i,] <- as.integer(buf[o+36:39])
                        bg[i,] <- as.integer(buf[o+40:43])
                        o <- o + 81 ## BOTTOM data chunk is always 81 bytes (Fig 46, p145 teledyne2014ostm)
                        if (debug && i <= profilesToShow) cat(vectorShow(br[i,], paste("br[", i, ",]", sep="")))
                        if (debug && i <= profilesToShow) cat(vectorShow(bv[i,], paste("bv[", i, ",]", sep="")))
                        if (debug && i <= profilesToShow) cat(vectorShow(bc[i,], paste("bc[", i, ",]", sep="")))
                        if (debug && i <= profilesToShow) cat(vectorShow(ba[i,], paste("ba[", i, ",]", sep="")))
                        if (debug && i <= profilesToShow) cat(vectorShow(bg[i,], paste("bg[", i, ",]", sep="")))
                    } else if (buf[o+1] == 0x20) { # navigation
                        message("NAVIGATION")
                        ## On the first profile, we set up space.
                        if (i == 1) {
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
                        firstTime <- c(firstTime, tmpTime + clockOffset+readBin(buf[o+6:9],'integer',n=1,size=4,endian='little')/10000)
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
                        o <- o + 78 ## NAVIGATION data chunk is always 78 bytes (Fig 46, p145 teledyne2014ostm)
                    } else if (buf[o+1] == 0x30) { # FIXED ATTITUDE
                        message("FIXED ATTITUDE")
                        o <- o + 41 ## FIXED ATTITUDE data chunk is always 41 bytes (Fig 46, p145 teledyne2014ostm)
                        warning("skipping BINARY FIXED ATTITUDE data chunk, profile ", i)
                    } else {
                        message("0x", buf[o+1], " flag... unknown. FIXME: handle 0x40-0xF0 0x30 (p146 teledynRDI 2014)")
                        message("also, how far should we skip forward???")
                        stop("unknown byte code 0x", buf[o+1], " encountered while trying to read profile ", i)
                    }
                }
                } ## FIXME
                ##oceDebug(debug, "chunk", i, "at byte", o, "; next 2 bytes are", as.raw(buf[o]), " and ", as.raw(buf[o+1]), " (expecting 0x00 and 0x01 for velocity)\n")
                ##>if (buf[o] == 0x00 && buf[o+1] == 0x01) { # velocity
                    ##> ##cat(vectorShow(buf[o + 1 + seq(1, 2*items)], "buf[...]:"))
                    ##> vv <- readBin(buf[o + 1 + seq(1, 2*items)], "integer", n=items, size=2, endian="little", signed=TRUE)
                    ##> ##cat(vectorShow(vv, "vv:"))
                    ##> vv[vv==(-32768)] <- NA       # blank out bad data
                    ##> v[i,,] <- matrix(velocityScale * vv, ncol=numberOfBeams, byrow=TRUE)
                    ##> ##cat(vectorShow(v[i,,], "v:"))
                    ##> o <- o + items * 2 + 2 # skip over the velo data, plus a checksum; FIXME: use the checksum
                    ##> if (buf[o] != 0x00)
                    ##>     warning("first byte of correlation segment should be 0x00 but is ", buf[o], " at file position ", o)
                    ##> if (buf[o+1] != 0x02)
                    ##>     warning("second byte of correlation segment should be 0x02 but is ", buf[o+1], " at file position ", o+1)
                    ##> ##oceDebug(debug-1, "'q' (correlation) chunk at byte", o, "\n")
                    ##> q[i,,] <- matrix(buf[o + 1 + seq(1, items)], ncol=numberOfBeams, byrow=TRUE)
                    ##> ##cat(vectorShow(q[i,,], "q:"))
                    ##> o <- o + items + 2              # skip over the one-byte data plus a checkum; FIXME: use the checksum
                    ##> if (buf[o] != 0x00)
                    ##>     warning("first byte of intensity segment should be 0x00 but is ", buf[o], " at file position ", o)
                    ##> if (buf[o+1] != 0x03)
                    ##>     warning("second byte of intensity segment should be 0x03 but is ", buf[o+1], " at file position ", o+1)
                    ##> ##oceDebug(debug-1, "'a' (amplitude) chunk at byte", o, "\n")
                    ##> a[i,,] <- matrix(buf[o + 1 + seq(1, items)], ncol=numberOfBeams, byrow=TRUE)
                    ##> cat(vectorShow(a[i,,], "a:"))
                    ##> o <- o + items + 2              # skip over the one-byte data plus a checkum; FIXME: use the checksum
                    ##> if (buf[o] != 0x00)
                    ##>     warning("first byte of percent-good segment should be 0x00 but is ", buf[o], " at file position ", o)
                    ##> if (buf[o+1] != 0x04)
                    ##>     warning("second byte of percent-good segment should be 0x04 but is ", buf[o+1], " at file position ", o+1)
                    ##> ##oceDebug(debug-1, "'g' (percent good) chunk at byte", o, "\n")
                    ##> g[i,,] <- matrix(buf[o + 1 + seq(1, items)], ncol=numberOfBeams, byrow=TRUE) # FIXME: not using this
                    ##> ##cat(vectorShow(g[i,,], "g:"))
                    ##> o <- o + items + 2              # skip over the one-byte data plus a checkum; FIXME: use the checksum
                    ##> ##oceDebug(debug, "buf[", o+1, "]=", buf[o+1], "; expect 01 for velo or 06 for bottom track\n")
                    ##> o <- o + skipStatus # FIXME: a kludge; see above
                    ##> if (buf[o+1] == 0x06) {
                    ##>     ## bottom-track byte code (teledyne2010wcao, Table 38 p155)
                    ##>     ## It seems that spurious bottom-track records might occur sometimes,
                    ##>     ## and the following tries to prevent that by insisting that bottom
                    ##>     ## track data occur in the first profile, if they occur later; otherwise
                    ##>     ## this flag will be ignored.
                    ##>     if (i == 1 && !haveBottomTrack) {
                    ##>         if (numberOfBeams != 4)
                    ##>             stop("expecting 4 beams, for this RDI adcp")
                    ##>         br <- array(double(), dim=c(profilesToRead, numberOfBeams))
                    ##>         bv <- array(double(), dim=c(profilesToRead, numberOfBeams))
                    ##>         bc <- array(double(), dim=c(profilesToRead, numberOfBeams)) # correlation
                    ##>         ba <- array(double(), dim=c(profilesToRead, numberOfBeams)) # amplitude
                    ##>         bg <- array(double(), dim=c(profilesToRead, numberOfBeams)) # percent good
                    ##>         haveBottomTrack <- TRUE
                    ##>     }
                    ##>     if (haveBottomTrack) {
                    ##>         ## the bottom range is in 3 bytes, split into two chunks
                    ##>         rangeLSB <- readBin(buf[o+c(16:23)], "integer",
                    ##>                             n=4, size=2, signed=FALSE, endian="little")
                    ##>         rangeMSB <- readBin(buf[o+77:80], "integer",
                    ##>                             n=4, size=1, signed=FALSE, endian="little")
                    ##>         br[i,] <- 0.01 * (65536 * rangeMSB + rangeLSB)
                    ##>         bv[i,] <- 0.001 * readBin(buf[o+c(24:31)], "integer",
                    ##>                                   n=4, size=2, signed=TRUE, endian="little")
                    ##>         bc[i,] <- as.integer(buf[o+32:35])
                    ##>         ba[i,] <- as.integer(buf[o+36:39])
                    ##>         bg[i,] <- as.integer(buf[o+40:43])
                    ##>     }
                    ##> }
                    ##VMDAS if (buf[o + 85] == 0x00 && buf[o+85+1] == 0x20) { # VMDAS
                    ##VMDAS     ## This must be in the first profile, or we won't call this a VMDAS file.
                    ##VMDAS     if (i == 1) {
                    ##VMDAS         oceDebug(debug, "This is a VMDAS file\n")
                    ##VMDAS         isVMDAS <- TRUE
                    ##VMDAS         firstTime <- firstLongitude <- firstLatitude <- NULL
                    ##VMDAS         lastTime <- lastLongitude <- lastLatitude <- NULL
                    ##VMDAS         avgSpeed <- avgTrackTrue <- avgTrackMagnetic <- NULL
                    ##VMDAS         speedMadeGood <- speedMadeGoodNorth <- speedMadeGoodEast <- NULL
                    ##VMDAS         directionMadeGood <- NULL
                    ##VMDAS         shipPitch <- shipRoll <- shipHeading <- NULL
                    ##VMDAS         numberOfSpeedSamplesAveraged <- numberOfTrueTrackSamplesAveraged <-
                    ##VMDAS             numberOfMagneticTrackSamplesAveraged <- numberOfHeadingSamplesAveraged <-
                    ##VMDAS                 numberOfPitchRollSamplesAveraged <- NULL
                    ##VMDAS         avgTrueVelocityNorth <- avgTrueVelocityEast <- NULL
                    ##VMDAS         avgMagnitudeVelocityNorth <- avgMagnitudeVelocityEast <- NULL
                    ##VMDAS         primaryFlags <- NULL
                    ##VMDAS     } else {
                    ##VMDAS         if (!isVMDAS)
                    ##VMDAS             badVMDAS <- c(badVMDAS, i)
                    ##VMDAS     }
                    ##VMDAS     if (isVMDAS) {
                    ##VMDAS         o <- o + 85
                    ##VMDAS         tmpTime <- as.numeric(ISOdatetime(as.integer(buf[o+4]) + 256*as.integer(buf[o+5]), #year
                    ##VMDAS                                           as.integer(buf[o+3]), #month
                    ##VMDAS                                           as.integer(buf[o+2]), #day
                    ##VMDAS                                           0, 0, 0,
                    ##VMDAS                                           tz=tz))
                    ##VMDAS         clockOffset <- 0.001 * readBin(buf[o+10:13], 'integer', n=1, size=4, endian='little')
                    ##VMDAS         firstTime <- c(firstTime, tmpTime + clockOffset+readBin(buf[o+6:9],'integer',n=1,size=4,endian='little')/10000)
                    ##VMDAS         ##704 sNavTime <- as.POSIXct(sNavTime, origin='1970-01-01', tz=tz)
                    ##VMDAS         cfac <- 180/2^31 # from rdradcp.m line 825
                    ##VMDAS         ## FIXME: this c() operation is slow and inelegant
                    ##VMDAS         firstLatitude <- c(firstLatitude, readBin(buf[o+14:17], 'integer', n=1, size=4, endian='little')*cfac)
                    ##VMDAS         firstLongitude <- c(firstLongitude, readBin(buf[o+18:21], 'integer', n=1, size=4, endian='little')*cfac)
                    ##VMDAS         lastTime <- c(lastTime,   tmpTime + clockOffset+readBin(buf[o+22:25], 'integer', n=1, size=4, endian='little')/10000)
                    ##VMDAS         lastLatitude <- c(lastLatitude, readBin(buf[o+26:29], 'integer', n=1, size=4, endian='little')*cfac)
                    ##VMDAS         lastLongitude <- c(lastLongitude, readBin(buf[o+30:33], 'integer', n=1, size=4, endian='little')*cfac)
                    ##VMDAS         ## FIXME: DK: I need to figure out the difference between eNavTime and navTime
                    ##VMDAS         ## FIXME: CR: what you are calling navTime should be the same as the "ADCP time"
                    ##VMDAS         ## tmpTime <- ISOdatetime(as.integer(buf[o+54]) + 256*as.integer(buf[o+55]), #year
                    ##VMDAS         ##                        as.integer(buf[o+57]), #month
                    ##VMDAS         ##                        as.integer(buf[o+56]), #day
                    ##VMDAS         ##                        0, 0, 0,
                    ##VMDAS         ##                        tz=tz)
                    ##VMDAS         ## navTime <- c(navTime, tmpTime + readBin(buf[o+58:61], 'integer', n=1, size=4, endian='little')/100)
                    ##VMDAS         ##(A) navTime <- as.POSIXct(navTime, origin='1970-01-01', tz=tz)
                    ##VMDAS         avgSpeed <- c(avgSpeed, 0.001*readBin(buf[o+34:35], 'integer', n=1, size=2, endian='little'))
                    ##VMDAS         avgTrackTrue <- c(avgTrackTrue, readBin(buf[o+36:37], 'integer', n=1, size=2, endian='little'))
                    ##VMDAS         avgTrackMagnetic <- c(avgTrackMagnetic, readBin(buf[o+38:39], 'integer', n=1, size=2, endian='little'))
                    ##VMDAS         speedMadeGood <- c(speedMadeGood, 0.001*readBin(buf[o+40:41], 'integer', n=1, size=2, endian='little'))
                    ##VMDAS         directionMadeGood <- c(directionMadeGood, (360/2^16)*readBin(buf[o+42:43], 'integer', n=1, size=2, endian='little'))
                    ##VMDAS         ## jul21 {
                    ##VMDAS         shipPitch <- c(shipPitch,
                    ##VMDAS                        (360/2^16)*readBin(buf[o+62:63], 'integer', n=1, size=2, endian='little'))
                    ##VMDAS         shipRoll <- c(shipRoll,
                    ##VMDAS                       (360/2^16)*readBin(buf[o+64:65], 'integer', n=1, size=2, endian='little'))
                    ##VMDAS         shipHeading <- c(shipHeading,
                    ##VMDAS                          (360/2^16)*readBin(buf[o+66:67], 'integer', n=1, size=2, endian='little'))
                    ##VMDAS         numberOfSpeedSamplesAveraged <- c(numberOfSpeedSamplesAveraged,
                    ##VMDAS                                           readBin(buf[o+68:69], 'integer', n=1, size=2, endian='little'))
                    ##VMDAS         numberOfTrueTrackSamplesAveraged <- c(numberOfTrueTrackSamplesAveraged,
                    ##VMDAS                                               readBin(buf[o+70:71], 'integer', n=1, size=2, endian='little'))
                    ##VMDAS         numberOfMagneticTrackSamplesAveraged <- c(numberOfMagneticTrackSamplesAveraged,
                    ##VMDAS                                                   readBin(buf[o+72:73], 'integer', n=1, size=2, endian='little'))
                    ##VMDAS         numberOfHeadingSamplesAveraged <- c(numberOfHeadingSamplesAveraged,
                    ##VMDAS                                             readBin(buf[o+74:75], 'integer', n=1, size=2, endian='little'))
                    ##VMDAS         numberOfPitchRollSamplesAveraged <- c(numberOfPitchRollSamplesAveraged,
                    ##VMDAS                                               readBin(buf[o+76:77], 'integer', n=1, size=2, endian='little'))
                    ##VMDAS         avgTrueVelocityNorth <- c(avgTrueVelocityNorth,
                    ##VMDAS                                   readBin(buf[o+78:79], 'integer', n=1, size=2, endian='little'))
                    ##VMDAS         avgTrueVelocityEast <- c(avgTrueVelocityEast,
                    ##VMDAS                                  readBin(buf[o+80:81], 'integer', n=1, size=2, endian='little'))
                    ##VMDAS         avgMagnitudeVelocityNorth <- c(avgMagnitudeVelocityNorth,
                    ##VMDAS                                        readBin(buf[o+82:83], 'integer', n=1, size=2, endian='little'))
                    ##VMDAS         avgMagnitudeVelocityEast <- c(avgMagnitudeVelocityEast,
                    ##VMDAS                                       readBin(buf[o+84:85], 'integer', n=1, size=2, endian='little'))
                    ##VMDAS         speedMadeGoodNorth <- c(speedMadeGoodNorth,
                    ##VMDAS                                 0.001*readBin(buf[o+86:87], 'integer', n=1, size=2, endian='little'))
                    ##VMDAS         speedMadeGoodEast <- c(speedMadeGoodEast,
                    ##VMDAS                                0.001*readBin(buf[o+88:89], 'integer', n=1, size=2, endian='little'))
                    ##VMDAS         primaryFlags <- c(primaryFlags,
                    ##VMDAS                           readBin(buf[o+90:91], 'integer', n=1, size=2, endian='little'))
                    ##VMDAS         ## } jul21
                    ##VMDAS     }
                    ##VMDAS }
                    ##VMDAS if (monitor) {
                    ##VMDAS     cat(".", ...)
                    ##VMDAS     if (!(i %% 50))
                    ##VMDAS         cat(i, "\n", ...)
                    ##VMDAS }
                    ##VMDAS } else {
                    ##VMDAS     badProfiles <- c(badProfiles, i)
                    ##VMDAS     if (monitor) {
                    ##VMDAS         cat("X", ...)
                    ##VMDAS         if (!(i %% 50))
                    ##VMDAS             cat(i, "\n", ...)
                    ##VMDAS     }
                    ##VMDAS }
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
            if (isVMDAS) {
                #navTime <- as.POSIXct(navTime, origin='1970-01-01', tz=tz)
                firstTime <- firstTime + as.POSIXct("1970-01-01 00:00:00", tz=tz)
                lastTime <- lastTime + as.POSIXct("1970-01-01 00:00:00", tz=tz)
            }
            if (length(badProfiles) > 0) { # remove NAs in time (not sure this is right, but it prevents other problems)
                ## FIXME: won't we need to handle VmDas here, also?
                t0 <- time[match(1, !is.na(time))] # FIXME: should test if any
                time <- fillGap(as.numeric(time) - as.numeric(t0)) + t0
                nbad <- length(badProfiles)
                if (nbad == 1)
                    warning("Interpolated across a bad profile at time ", format(time[badProfiles]), ".  (\"Bad\" means that the expected byte code for a velocity segment, 0x00 0x01, was not found 65 bytes after the start of a profile, the latter indicated by the byte sequence 0x80 0x00.)")
                else
                    warning("Interpolated across ", length(badProfiles), " bad profile(s) at times: ", paste(format(time[badProfiles]), collapse=", "), ".  (\"Bad\" means that the expected byte code for a velocity segment, 0x00 0x01, was not found 65 bytes after the start of a profile, the latter indicated by the byte sequence 0x80 0x00.)")
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
            res@metadata$measurementStart <- measurementStart
            res@metadata$measurementEnd <- measurementEnd
            res@metadata$measurementDeltat <- measurementDeltat
            res@metadata$bin1Distance <- bin1Distance
            res@metadata$xmitPulseLength <- xmitPulseLength
            res@metadata$oceBeamUnspreaded <- FALSE
            res@metadata$oceCoordinate <- header$originalCoordinate
            res@metadata$depthMean <- mean(depth, na.rm=TRUE)
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
                   warning("erroneous VMDAS flag in profile ", badVMDAS)
               } else if (nbadVMDAS < 4) {
                   warning("erroneous VMDAS flag in profiles: ", paste(badVMDAS, collapse=" "))
               } else {
                   warning("erroneous VMDAS flag in ", nbadVMDAS, " profiles, including: ", badVMDAS[1], " ",
                           badVMDAS[2], " ... ", badVMDAS[nbadVMDAS-1], " ",
                           badVMDAS[nbadVMDAS], "\n")
               }
           }
           class(time) <- c("POSIXt", "POSIXct")
           attr(time, "tzone") <- getOption("oceTz")
           if (bFound && !isVMDAS) {
               br[br == 0.0] <- NA    # clean up (not sure if needed)
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
               br[br == 0.0] <- NA    # clean up (not sure if needed)
               res$data <- list(v=v, q=q, a=a, g=g,
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
           } else {
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
           ##>if (testing) {
           ##>    data$upward=upward
           ##>}
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
    res@metadata$manufacturer <- "teledyne rdi"
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    hitem <- processingLogItem(processingLog)
    res@processingLog <- unclass(hitem)
    res@metadata$units$v=list(unit=expression(m/s), scale="")
    res@metadata$units$distance=list(unit=expression(m), scale="")
    res@metadata$units$pressure=list(unit=expression(dbar), scale="")
    res@metadata$units$salinity=list(unit=expression(), scale="PSS-78")
    res@metadata$units$temperature=list(unit=expression(degree*C), scale="ITS-90")
    res@metadata$units$soundSpeed=list(unit=expression(m/s), scale="")
    res@metadata$units$heading=list(unit=expression(degree), scale="")
    res@metadata$units$pitch=list(unit=expression(degree), scale="")
    res@metadata$units$roll=list(unit=expression(degree), scale="")
    res@metadata$units$headingStd=list(unit=expression(degree), scale="")
    res@metadata$units$pitchStd=list(unit=expression(degree), scale="")
    res@metadata$units$rollStd=list(unit=expression(degree), scale="")
    res@metadata$units$attitude=list(unit=expression(degree), scale="")
    res@metadata$units$depth=list(unit=expression(m), scale="")
    oceDebug(debug, "} # read.adp.rdi()\n", unindent=1)
    res
}

### -------------------------------------------------------------------------
### Below is preliminary work for reading a SentinelV file (e.g. 4 beam workhorse combined with a 5th vertical beam
decodeHeaderRDIsentinel <- function(buf, debug=getOption("oceDebug"), tz=getOption("oceTz"), ...)
{

    ##
    ## header, of length 6 + 2 * numberOfDataTypes bytes
    ##
    oceDebug(debug, "decodeHeaderRDIsentinel() {\n", unindent=1)
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
    if (dataOffset[1]!=6+2*numberOfDataTypes)
        warning("dataOffset and numberOfDataTypes are inconsistent -- this dataset seems damaged")
    oceDebug(debug, "dataOffset=", paste(dataOffset, sep=" "), "\n")
    ##
    ## Check to make sure this is a sentinel file by looking for
    ## dataType ID bytes of 0x00 and 0x70 (V series system
    ## configuration)
    codes <- cbind(buf[1 + c(0, dataOffset)], buf[1+c(0, dataOffset) + 1])
    if (any(codes[,1] == 0x00 & codes[,2] == 0x70)) {
        message('Detected dataType 0x00 0x70 for Sentinel V series configuration')
        isSentinel <- TRUE
    } else {
        isSentinel <- FALSE
    }
    
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
    if (bits == "010") frequency <- 250        # kHz
    else if (bits == "011") frequency <-  500
    else if (bits == "100") frequency <-  1000
    else stop("unknown freq. bit code:", bits, " (expect 010 for 250kHz, 010 for 500kHz, etc)")
    oceDebug(debug, "bits:", bits, "so frequency=", frequency, "\n")
    bits <- substr(systemConfiguration, 15, 17)
    oceDebug(debug, "systemConfiguration:", systemConfiguration,"\n")
    oceDebug(debug, "bits:", bits, "Expect 111 for 25 degrees\n")
    if (bits == "111") beamAngle <- 25
    else beamAngle <- NA 
    oceDebug(debug, "bits=", bits, "so beamAngle=", beamAngle, "\n")
    ## if (beamAngle < 19 || 21 < beamAngle)
    ##     warning("expecting a beamAngle of 20 deg [more-or-less standard for RDI] but got ", beamAngle, "deg; using the latter in the transformationMatrix")
    beamPattern <- "convex" # is always convex for a SentinelV
    beamConfig <- "?"
    bits <- substr(systemConfiguration, 10, 13)
    if (bits == "0100") beamConfig <- "4 beam janus"
    else if (bits == "0101") beamConfig <- "5 beam janus"
    else beamConfig <- "unknown"
    bits <- substr(systemConfiguration, 1, 1)
    if (bits == "1") orientation <- "upward"
    else orientation <- "downward"
    oceDebug(debug, "bits=", bits, "so that orientation=", orientation, "\n")

    ##real.sim.flag <- readBin(FLD[7], "integer", n=1, size=1)
    ##lagLength <- readBin(FLD[8], "integer", n=1, size=1, signed=FALSE) # unused
    numberOfBeams <- readBin(FLD[9], "integer", n=1, size=1, signed=FALSE) # should be 4, even for a 5 beam system
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
    bits <- substr(byteToBinary(FLD[26], endian="big"), 4, 5)
    originalCoordinate <- "???"
    if (bits == "00") originalCoordinate <- "beam"
    else if (bits == "01") originalCoordinate <- "instrument"
    else if (bits == "10") originalCoordinate <- "xyz"
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
    oceDebug(debug, paste("CPU.BOARD.SERIAL.NUMBER = '", paste(cpuBoardSerialNumber, collapse=""), "'\n", sep=""))
    systemBandwidth <- readBin(FLD[51:52], "integer", n=1, size=2, endian="little")
    ##systemPower <- readBin(FLD[53], "integer", n=1, size=1)
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
    ## There really seems to be nothing specific in the file to tell instrument type, so, in an act of
    ## desparation (or is that hope) I'm going to flag on the one thing that was clearly stated, and
    ## clearly different, in the two documentation entries.
    if (FLDLength == 59) {
        instrumentSubtype <- "workhorse" # "WorkHorse Commands and Output Data Format_Mar05.pdf" (and Nov07 version) Figure 9 on page 122 (pdf-page 130)
    } else if (FLDLength == 50) {
        instrumentSubtype <- "surveyor/observer"
        ## "Ocean Surveyor Technical Manual.pdf" table D-3 on page D-5 (pdf-page 139)
        ## also teledyne2014ostm page 144 says could be Surveyor or Observer
    } else if (isSentinel) {
        instrumentSubtype <- 'sentinelV'
        ## 5 beam (4 beam workhorse + a vertical centre beam)
    } else {
        instrumentSubtype <- "unknown"
        ## FIXME: I think this is a poor way to determine the intrument type. Why do we even try?
        ##> warning("unexpected length ", FLDLength, " of fixed-leader-data header; expecting 50 for
        ##>         'surveyor/observor' or 59 for 'workhorse'.")
    }
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
    oceDebug(debug, "} # decodeHeaderRDIsentinel()\n", unindent=1)
    res
}                                       # decodeHeaderRDIsentinel

#' Read a Teledyne/RDI SentinelV ADCP File
#'
#' Read a Teledyne/RDI SentinelV ADCP file (called 'adp' in oce).
#'
#' This is a provisional function based on the original read.adp.rdi
#' code for reading a 5 beam SentinelV ADCP.
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
#' @param testing Logical value, indicating whether 
#' the time-varying device orientation is to be inferred from the
#' per-profile header information, and the boolean result is stored in an
#' integer vector named \code{upward} within the \code{data} slot.
#' @param type A character string indicating the type of instrument.
#' @template adpTemplate
#' @param despike if \code{TRUE}, \code{\link{despike}} will be used to clean
#' anomalous spikes in heading, etc.
#' @author Dan Kelley and Clark Richards
#' @references
#' 1. Teledyne-RDI, 2007. \emph{WorkHorse commands and output data
#' format.} P/N 957-6156-00 (November 2007).  (Section 5.3 h details the binary
#' format, e.g. the file should start with the byte \code{0x7f} repeated twice,
#' and each profile starts with the bytes \code{0x80}, followed by \code{0x00},
#' followed by the sequence number of the profile, represented as a
#' little-endian two-byte short integer.  \code{read.adp.rdi()} uses these
#' sequences to interpret data files.)
#' @family things related to \code{adp} data
read.adp.rdi.sentinel <- function(file, from=1, to, by=1, tz=getOption("oceTz"),
                                  longitude=NA, latitude=NA,
                                  type=c("workhorse"),
                                  monitor=FALSE, despike=FALSE, processingLog,
                                  testing=FALSE,
                                  debug=getOption("oceDebug"),
                                  ...)
{
    isVMDAS <- FALSE # FIXME
    oceDebug(debug, "read.adp.rdi(...,from=",format(from),
             ",to=",if(missing(to)) "missing" else format(to), "...) {\n", unindent=1)
    profileStart <- NULL # prevent scope warning from rstudio; defined later anyway
    bisectAdpRdi <- function(buf, t.find, add=0, debug=0) {
        oceDebug(debug, "bisectAdpRdi(t.find=", format(t.find), ", add=", add, ") {\n", unindent=1)
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
        oceDebug(debug, "} # bisectAdpRdi()\n", unindent=1)
        return(list(index=middle, time=t))
    }
    gaveFromTo <- !missing(from) && !missing(to)
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
    file.size <- seek(file, where=0)
    oceDebug(debug, "file.size=", file.size, "\n")
    buf <- readBin(file, what="raw", n=file.size, endian="little")
    ## decode header
    header <- decodeHeaderRDIsentinel(buf, debug=debug-1)
    if (header$haveActualData) {
        numberOfBeams <- header$numberOfBeams
        numberOfCells <- header$numberOfCells
        bin1Distance <- header$bin1Distance
        xmitPulseLength <- header$xmitPulseLength
        cellSize <- header$cellSize
        ## oceDebug(debug, "about to call ldc_rdi\n")
        ensembleStart <- .Call("ldc_rdi", buf, 0) # point at bytes (7f 7f)
        oceDebug(debug, "successfully called ldc_rdi\n")
        if (header$instrumentSubtype == 'sentinelV') {
            oceDebug(debug, "SentinelV type detected, skipping first ensemble\n")
            ensembleStart <- ensembleStart[-1] # remove the first ensemble to simplify parsing
            ## re-read the numberOfDataTypes and dataOffsets from the second ensemble
            header$numberOfDataTypes <- readBin(buf[ensembleStart[1]+5], "integer", n=1, size=1)
            header$dataOffset <- readBin(buf[ensembleStart[1]+6+0:(2*header$numberOfDataTypes)], "integer", n=header$numberOfDataTypes, size=2, endian="little", signed=FALSE)
        }
        
        ## Profiles start at the VARIABLE LEADER DATA, since there is no point in
        ## re-interpreting the HEADER and the FIXED LEADER DATA over and over,
        ## but we need the VLD to get times, etc. I *think* we can assume a fixed
        ## location for these, based on the "Always Output" indication in Fig 46
        ## on page 145 of teledyne2014ostm.
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
                fromPair <- bisectAdpRdi(buf, from, add=-1, debug=debug-1)
                from <- fromIndex <- fromPair$index
                toPair <- bisectAdpRdi(buf, to, add=1, debug=debug-1)
                to <- toIndex <- toPair$index
                oceDebug(debug, "from:", format(fromPair$t), " yields profileStart[", fromIndex, "]\n")
                oceDebug(debug, "to:", format(toPair$t), "yields profileStart[", toIndex, "]\n")
                oceDebug(debug, "by:", by, "(not yet decoded)\n")
                oceDebug(debug, "head(profileStart):", paste(head(profileStart), collapse=" "), "\n")
                oceDebug(debug, "tail(profileStart):", paste(tail(profileStart), collapse=" "), "\n")
                oceDebug(debug, "'from' is profileStart[", fromPair$index, "]:", profileStart[fromPair$index], "at time", format(fromPair$t), "\n")
                oceDebug(debug, "'to' is profileStart[", toPair$index, "]:", profileStart[toPair$index], "at time", format(toPair$t), "\n")
                dt <- measurementDeltat
                oceDebug(debug, "dt:", dt, "s, by:", by,"\n")
                if (is.character(by))
                    by <- floor(0.5 + ctimeToSeconds(by) / dt)
                oceDebug(debug, "by:",by,"profiles (after decoding)\n")
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
            oceDebug(debug, "filename: \"",filename,"\"\n", sep="")
            oceDebug(debug, "profilesToRead:",profilesToRead,"\n")
            oceDebug(debug, "numberOfBeams:",numberOfBeams,"\n")
            oceDebug(debug, "numberOfCells:",numberOfCells,"\n")

            ##20151121 if (testing) {
            ##20151121     nensembles <- length(ensembleStart)
            ##20151121     numberOfDataTypes <- readBin(buf[ensembleStart[1] + 5], "integer", n=1, size=1) # Note: just using first one
            ##20151121     FLDStart <- ensembleStart + 6 + 2 * numberOfDataTypes
            ##20151121     ## FIXME: decide whether the code below is cleaner than the spot where time is determined
            ##20151121     ## VLDStart <- FLDStart + 59
            ##20151121     ## RTC.year <- unabbreviateYear(readBin(buf[VLDStart+4], "integer", n=nensembles, size=1))
            ##20151121     ## RTC.month <- readBin(buf[VLDStart+5], "integer", n=nensembles, size=1)
            ##20151121     ## RTC.day <- readBin(buf[VLDStart+6], "integer", n=nensembles, size=1)
            ##20151121     ## RTC.hour <- readBin(buf[VLDStart+7], "integer", n=nensembles, size=1)
            ##20151121     ## RTC.minute <- readBin(buf[VLDStart+8], "integer", n=nensembles, size=1)
            ##20151121     ## RTC.second <- readBin(buf[VLDStart+9], "integer", n=nensembles, size=1)
            ##20151121     ## RTC.hundredths <- readBin(buf[VLDStart+10], "integer", n=nensembles, size=1)
            ##20151121     ## time <- ISOdatetime(RTC.year, RTC.month, RTC.day, RTC.hour, RTC.minute, RTC.second + RTC.hundredths / 100, tz=tz)

            ##20151121     ## regarding the "4" below, see p 135 of WorkHorse_commands_data_format_AUG10.PDF,
            ##20151121     ## noting that we subtract 1 because it's an offset; we are thus examining
            ##20151121     ## the LSB of the "Sys Cfg" pair.
            ##20151121     upward <- .Call("get_bit", buf[FLDStart+4], 7)
            ##20151121     ##testingData <- list(time=time, upward=upward)
            ##20151121 }

            items <- numberOfBeams * numberOfCells

            ## set up storage
            codes <- cbind(buf[ensembleStart[1]+c(0,header$dataOffset)], buf[1+ensembleStart[1]+c(0,header$dataOffset)])
            oceDebug(debug, "below are the data-chunk codes; see Table 33 p145 of Teledyne/RDI OS_TM_Apr14.pdf\n")
            if (debug)
                print(codes)
            vFound <- sum(codes[,1]==0x00 & codes[,2]==0x01) # velo
            qFound <- sum(codes[,1]==0x00 & codes[,2]==0x02) # corr
            aFound <- sum(codes[,1]==0x00 & codes[,2]==0x03) # echo intensity
            gFound <- sum(codes[,1]==0x00 & codes[,2]==0x04) # percent good
            ##sFound <- sum(codes[,1]==0x00 & codes[,2]==0x05) # status
            bFound <- sum(codes[,1]==0x00 & codes[,2]==0x06) # bottom-track
            ##nFound <- sum(codes[,1]==0x00 & codes[,2]==0x20) # navigation
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

            profilesToShow <- 3 # only if debug>0
            
            for (i in 1:profilesToRead) {     # recall: these start at 0x80 0x00
                ## for (chunk in 1:header$numberOfDataTypes) {
                for (chunk in 1:5) { ## FIXME: will want to read the sentinel chunks too

                    o <- ensembleStart[i] + header$dataOffset[chunk]
                    if (i <= profilesToShow)
                        oceDebug(debug, "profile:", i, ", chunk:", chunk, ", buf: 0x", buf[o], " 0x", buf[1+o], "\n", sep="")
                    if (buf[o] == 0x00 & buf[1+o] == 0x00) {
                        if (i <= profilesToShow) oceDebug(debug, "Fixed leader skipped\n")
                    } else if (buf[o] == 0x80 & buf[1+o] == 0x00) {
                        if (i <= profilesToShow) oceDebug(debug, "Variable leader skipped\n")
                    } else if (buf[o] == 0x00 & buf[1+o] == 0x01) {
                        vv <- readBin(buf[o + 1 + seq(1, 2*items)], "integer", n=items, size=2, endian="little", signed=TRUE)
                        vv[vv==(-32768)] <- NA       # blank out bad data
                        v[i,,] <- matrix(velocityScale * vv, ncol=numberOfBeams, byrow=TRUE)
                        if (debug && i <= profilesToShow) cat(vectorShow(v[i,,1], paste("v[", i, ",,1]", sep="")))
                    } else if (buf[o] == 0x00 & buf[1+o] == 0x02) {
                        q[i,,] <- matrix(buf[o + 1 + seq(1, items)], ncol=numberOfBeams, byrow=TRUE)
                        if (debug && i <= profilesToShow) cat(vectorShow(q[i,,1], paste("q[", i, ",,1]", sep="")))
                    } else if (buf[o] == 0x00 & buf[1+o] == 0x03) {
                        a[i,,] <- matrix(buf[o + 1 + seq(1, items)], ncol=numberOfBeams, byrow=TRUE)
                        if (debug && i <= profilesToShow) cat(vectorShow(a[i,,1], paste("a[", i, ",,1]", sep="")))
                    } else if (buf[o] == 0x00 & buf[1+o] == 0x04) {
                        g[i,,] <- matrix(buf[o + 1 + seq(1, items)], ncol=numberOfBeams, byrow=TRUE)
                        if (debug && i <= profilesToShow) cat(vectorShow(g[i,,1], paste("g[", i, ",,1]", sep="")))
                    } else if (buf[o] == 0x00 & buf[1+o] == 0x30) {
                        if (i <= profilesToShow) oceDebug(debug, "Fixed attitude, profile", i, "\n")
                    } else if (buf[1+o] == 0x30) { ## fixme need to check first byte
                        if (i <= profilesToShow) oceDebug(debug, "Variable attitude, profile", i, "\n")
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
            message("DAN 2") 
            isVMDAS <- FALSE # FIXME FIXME
            if (isVMDAS) {
                #navTime <- as.POSIXct(navTime, origin='1970-01-01', tz=tz)
                firstTime <- firstTime + as.POSIXct("1970-01-01 00:00:00", tz=tz)
                lastTime <- lastTime + as.POSIXct("1970-01-01 00:00:00", tz=tz)
            }
            if (length(badProfiles) > 0) { # remove NAs in time (not sure this is right, but it prevents other problems)
                ## FIXME: won't we need to handle VmDas here, also?
                t0 <- time[match(1, !is.na(time))] # FIXME: should test if any
                time <- fillGap(as.numeric(time) - as.numeric(t0)) + t0
                nbad <- length(badProfiles)
                if (nbad == 1)
                    warning("Interpolated across a bad profile at time ", format(time[badProfiles]), ".  (\"Bad\" means that the expected byte code for a velocity segment, 0x00 0x01, was not found 65 bytes after the start of a profile, the latter indicated by the byte sequence 0x80 0x00.)")
                else
                    warning("Interpolated across ", length(badProfiles), " bad profile(s) at times: ", paste(format(time[badProfiles]), collapse=", "), ".  (\"Bad\" means that the expected byte code for a velocity segment, 0x00 0x01, was not found 65 bytes after the start of a profile, the latter indicated by the byte sequence 0x80 0x00.)")
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
            res@metadata$measurementStart <- measurementStart
            res@metadata$measurementEnd <- measurementEnd
            res@metadata$measurementDeltat <- measurementDeltat
            res@metadata$bin1Distance <- bin1Distance
            res@metadata$xmitPulseLength <- xmitPulseLength
            res@metadata$oceBeamUnspreaded <- FALSE
            res@metadata$oceCoordinate <- header$originalCoordinate
            res@metadata$depthMean <- mean(depth, na.rm=TRUE)
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
                                                          0        ,          0, -tm.c*tm.a, tm.c*tm.a,
                                                          tm.b     ,       tm.b,       tm.b,      tm.b,
                                                          tm.d     ,       tm.d,      -tm.d,     -tm.d),
                                                        nrow=4, byrow=TRUE)
           if (monitor)
                cat("\nRead", profilesToRead,  "profiles, out of a total of",profilesInFile,"profiles in", filename, "\n", ...)

           ## Sometimes a non-VMDAS file will have some profiles that have the VMDAS flag.
           ## It is not clear why this happens, but in any case, provide a warning.
           badVMDAS <- NULL # FIXME FIXME
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
           class(time) <- c("POSIXt", "POSIXct")
           attr(time, "tzone") <- getOption("oceTz")
            message("DAN 3")
           if (bFound && !isVMDAS) {
               br[br == 0.0] <- NA    # clean up (not sure if needed)
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
               br[br == 0.0] <- NA    # clean up (not sure if needed)
               res$data <- list(v=v, q=q, a=a, g=g,
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
           } else {
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
           ##>if (testing) {
           ##>    data$upward=upward
           ##>}
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
    res@metadata$manufacturer <- "teledyne rdi"
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    hitem <- processingLogItem(processingLog)
    res@processingLog <- unclass(hitem)
    res@metadata$units$v=list(unit=expression(m/s), scale="")
    res@metadata$units$distance=list(unit=expression(m), scale="")
    res@metadata$units$pressure=list(unit=expression(dbar), scale="")
    res@metadata$units$salinity=list(unit=expression(), scale="PSS-78")
    res@metadata$units$temperature=list(unit=expression(degree*C), scale="ITS-90")
    res@metadata$units$soundSpeed=list(unit=expression(m/s), scale="")
    res@metadata$units$heading=list(unit=expression(degree), scale="")
    res@metadata$units$pitch=list(unit=expression(degree), scale="")
    res@metadata$units$roll=list(unit=expression(degree), scale="")
    res@metadata$units$headingStd=list(unit=expression(degree), scale="")
    res@metadata$units$pitchStd=list(unit=expression(degree), scale="")
    res@metadata$units$rollStd=list(unit=expression(degree), scale="")
    res@metadata$units$attitude=list(unit=expression(degree), scale="")
    res@metadata$units$depth=list(unit=expression(m), scale="")
    oceDebug(debug, "} # read.adp.rdi()\n", unindent=1)
    res
}
