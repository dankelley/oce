## Data format overview
## hardware [a5 05 X1 X2]  48 bytes, 2*(short word made from X1 and X2)
## head     [a5 04 X1 X2] 224 bytes, 2*(short word made from X1 and X2)
## user     [a5 00 X1 X2] 512 bytes, 2*(short word made from X1 and X2)
## profiles, each starting with a5 2a [aquadoppHR] or a5 21 [aquadoopp profiler] [other]
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



#' Decode a Nortek Header
#'
#' Decode data in a Nortek ADV or ADP header.
#'
#' Decodes the header in a binary-format Nortek ADV/ADP file.  This function is
#' designed to be used by \code{\link{read.adp}} and \code{\link{read.adv}},
#' but can be used directly as well.  The code is based on information in the
#' Nortek System Integrator Guide (2008) and on postings on the Nortek
#' ``knowledge center'' discussion board.  One might assume that the latter is
#' less authoritative than the former.  For example, the inference of cell size
#' follows advice found at
#' \url{http://www.nortekusa.com/en/knowledge-center/forum/hr-profilers/736804717}
#' (downloaded June 2012)), which contains a typo in an early posting that is
#' corrected later on.
#'
#' @param buf a ``raw'' buffer containing the header
#' @param type type of device
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, or to 2 to get more.
#' @param \dots additional arguments, passed to called routines.
#' @return A list containing elements \code{hardware}, \code{head}, \code{user}
#' and \code{offset}.  The easiest way to find the contents of these is to run
#' this function with \code{debug=3}.
#' @author Dan Kelley and Clark Richards
#' @seealso Most users should employ the functions \code{\link{read.adp}} and
#' \code{\link{read.adv}} instead of this one.
#' @references 1. Information on Nortek profilers (including the System
#' Integrator Guide, which explains the data format byte-by-byte) is available
#' at \url{http://www.nortekusa.com/}.  (One must join the site to see the
#' manuals.)
#'
#' 2. The Nortek Knowledge Center
#' \url{http://www.nortekusa.com/en/knowledge-center} may be of help if
#' problems arise in dealing with data from Nortek instruments.
decodeHeaderNortek <- function(buf, type=c("aquadoppHR", "aquadoppProfiler", "aquadopp", "vector"), debug=getOption("oceDebug"), ...)
{
    type <- match.arg(type)
    oceDebug(debug, "decodeHeaderNortek(buf, type=\"", type, "\", ...) {\n", sep="", unindent=1)
    oceDebug(debug, "buf starts:", buf[1:20], "\n")
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
    for (header in 1:3) {
        ## FIXME: code is needlessly written as if headers could be in different order
        oceDebug(debug, "\n")
        ##oceDebug(debug, "examining buf[o+2]=", buf[o+2], "to see what type of header block is next...\n")
        if (buf[o+1] != syncCode)
            stop("expecting syncCode 0x", syncCode, " but got 0x", buf[o+1], " instead (while reading header #", header, ")")
        if (buf[o+2] == idHardwareConfiguration) {
            ## see page 29 of System Integrator Guide
            oceDebug(debug, "\nHARDWARE CONFIGURATION\n", unindent=1)
            hardware$size <- readBin(buf[o+3:4], "integer", signed=FALSE, n=1, size=2, endian="little")
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
        } else if (buf[o+2] == idHeadConfiguration) {
            ## see page 30 of System Integrator Guide
            oceDebug(debug, "HEAD CONFIGURATION\n", unindent=1)
            ##buf <- readBin(file, "raw", headerLengthHead)
            head$size <- readBin(buf[o+3:4], "integer", signed=FALSE, n=1, size=2)
            if (2 * head$size != headerLengthHead)
                stop("size of head header expected to be ", headerLengthHead, "but got ", head$size)
            oceDebug(debug, "head$size=", head$size, "\n")

            ## Nortek doc "system-integrator-manual_Mar2016.pdf" (page 23) says for the "head configuration":
            ## bit 0: Pressure sensor (0=no, 1=yes)
            ## bit 1: Magnetometer sensor (0=no, 1=yes)
            ## bit 2: Tilt sensor (0=no, 1=yes)
            ## bit 3: Tilt sensor mounting (0=up, 1=down)
            ## issue1220 debug <- debug + 10
            ## issue1220 head$config <- byteToBinary(buf[o+5:6], endian="little")
            ## issue1220 oceDebug(debug, "head$config=", head$config, "\n")
            ## issue1220 configTEST <- rawToBits(buf[o+5:6])
            ## issue1220 oceDebug(debug, "configTEST=", configTEST, "\n")
            ## issue1220 oceDebug(debug, "head$config=", head$config, "\n")
            ## issue1220 head$configPressureSensor <- substr(head$config[1], 1, 1) == "1"
            ## issue1220 oceDebug(debug, "head$configPressureSensor=", head$configPressureSensor, "\n")
            ## issue1220 head$configMagnetometerSensor <- substr(head$config[1], 2, 2) == "1"
            ## issue1220 oceDebug(debug, "head$configMagnetometerSensor=", head$configMagnetometerSensor, "\n")
            ## issue1220 head$configTiltSensor <- substr(head$config[1], 3, 3) == "1"
            ## issue1220 oceDebug(debug, "head$configTiltSensor=", head$configTiltSensor, "\n")
            ## issue1220 head$tiltSensorOrientation <- if (substr(head$config[1], 4, 4) == "1") "downward" else "upward"
            ## issue1220 oceDebug(debug, "head$tiltSensorOrientation=", head$tiltSensorOrientation, "\n")
            tmpBits <- rawToBits(buf[o+5])
            head$configPressureSensor <- tmpBits[1] == as.raw(0x1)
            head$configMagnetometerSensor <- tmpBits[2] == as.raw(0x1)
            head$configTiltSensor <- tmpBits[3] == as.raw(0x1)
            head$configTiltSensorOrientation <- ifelse(tmpBits[4] == as.raw(0x1), "downward", "upward")
            oceDebug(debug, "head$configPressureSensor=", head$configPressureSensor, "\n")
            oceDebug(debug, "head$configMagnetometerSensor=", head$configMagnetometerSensor, "\n")
            oceDebug(debug, "head$configTiltSensor=", head$configTiltSensor, "\n")
            oceDebug(debug, "head$configTiltSensorOrientation=", head$configTiltSensorOrientation, "\n")

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
            head$transformationMatrix <- matrix(readBin(buf[o+31:48], "integer", n=9, size=2, endian="little"),
                                                 nrow=3, byrow=TRUE) / 4096
            oceDebug(debug, "head$transformationMatrix\n")
            oceDebug(debug, format(head$transformationMatrix[1, ], width=15), "\n")
            oceDebug(debug, format(head$transformationMatrix[2, ], width=15), "\n")
            oceDebug(debug, format(head$transformationMatrix[3, ], width=15), "\n")
            head$numberOfBeams <- readBin(buf[o+221:222], "integer", n=1, size=2, endian="little")
            oceDebug(debug, "head$numberOfBeams=", head$numberOfBeams, "\n")
            o <- o + 2 * head$size
        } else if (buf[o+2] == idUserConfiguration) {
            ## User Configuration [p30-32 of System Integrator Guide]
            oceDebug(debug, "USER CONFIGURATION\n", unindent=1)
            user$size <- readBin(buf[o+3:4], what="integer", n=1, size=2, endian="little")
            if (2 * user$size != headerLengthUser)
                stop("size of user header expected to be ", headerLengthUser, "but got ", user$size)
            ##buf <- readBin(file, "raw", headerLengthUser)
            user$transmitPulseLength <- readBin(buf[o+5:6], "integer", n=1, size=2, endian="little", signed=FALSE)
            oceDebug(debug, "user$transmitPulseLength=", user$transmitPulseLength, "\n")

            user$T2 <- readBin(buf[o+7:8], "integer", n=1, size=2, endian="little", signed=FALSE)
            oceDebug(debug, "user$T2=", user$T2, "(blanking distance, in counts)\n")

            user$receiveLength <- readBin(buf[o+9:10], "integer", n=1, size=2, endian="little", signed=FALSE)
            oceDebug(debug, "user$receiveLength=T3=", user$receiveLength, "counts\n")
            oceDebug(debug, " ABOVE FROM buf[o+9]=0x", buf[o+9], " and buf[o+10]=0x", buf[0+10], '\n', sep='')

            user$timeBetweenPings <- readBin(buf[o+11:12], "integer", n=1, size=2, endian="little", signed=FALSE)
            oceDebug(debug, "user$timeBetweenPings=", user$timeBetweenPings, "in counts\n")

            user$timeBetweenBurstSequences <- readBin(buf[o+13:14], "integer", n=1, size=2, endian="little", signed=FALSE)
            oceDebug(debug, "user$timeBetweenBurstSequences=", user$timeBetweenBurstSequences, "in counts\n")

            user$numberOfBeamSequencesPerBurst <- readBin(buf[o+15:16], "integer", n=1, size=2, endian="little", signed=FALSE)
            oceDebug(debug, "user$numberOfBeamSequencesPerBurst=", user$numberOfBeamSequencesPerBurst, "in counts\n")

            user$averagingInterval <- readBin(buf[o+17:18], "integer", n=1, size=2, endian="little")
            oceDebug(debug, "user$averagingInterval=", user$averagingInterval, "in seconds\n")
            user$numberOfBeams <- readBin(buf[o+19:20], "integer", n=1, size=2, endian="little")
            oceDebug(debug, "user$numberOfBeams=", user$numberOfBeams, "\n")

            user$timCtrlReg <- readBin(buf[o+21:22], "raw", n=2)
            oceDebug(debug, "user$timCtrlReg=", user$timCtrlReg, "\n")

            user$measurementInterval <- readBin(buf[o+39:40], "integer", n=1, size=2, endian="little")
            oceDebug(debug, "user$measurementInterval=", user$measurementInterval, "\n")
            user$deploymentName <- readBin(buf[o+41:47], "character", n=1, size=6)
            oceDebug(debug, "user$deploymentName=", user$deploymentName, "\n")
            user$comments <- readBin(buf[o+257+0:179], "character", n=1, size=180)
            oceDebug(debug, "user$comments=", user$comments, "\n")

            ## Nortek doc "system-integrator-manual_Mar2016.pdf" (page 24-25) says for the "mode":
            ##   bit 0: use user specified sound speed (0=no, 1=yes)
            ##   bit 1: diagnostics/wave mode 0=disable, 1=enable)
            ##   bit 2: analog output mode (0=disable, 1=enable)
            ##   bit 3: output format (0=Vector, 1=ADV)
            ##   bit 4: scaling (0=1 mm, 1=0.1 mm)
            ##   bit 5: serial output (0=disable, 1=enable)
            ##   bit 6: reserved EasyQ
            ##   bit 7: stage (0=disable, 1=enable)
            ##   bit 8: output power for analog input (0=disable, 1=enable)
            ## and we want bit 4. In R notation, that is rawToBits()[5]
            ##
            ## issue1220 user$mode <- byteToBinary(buf[o+59:60], endian="little")
            ## issue1220 user$velocityScale <- if (substr(user$mode[2], 4, 4) == "0") 0.001 else 0.0001
            user$mode <- rawToBits(buf[o+59:60])
            oceDebug(debug, "user$mode: ", user$mode, "\n")
            user$velocityScale <- ifelse(user$mode[5] == 0x00, 0.001, 0.0001)
            oceDebug(debug, "user$velocityScale: ", user$velocityScale, "\n")

            tmp.cs <- readBin(buf[o+33:34], "integer", n=1, size=2, endian="little")
            if (tmp.cs == 0) user$originalCoordinate <- "enu" # page 31 of System Integrator Guide
            else if (tmp.cs == 1) user$originalCoordinate <- "xyz"
            else if (tmp.cs == 2) user$originalCoordinate <- "beam"
            else stop("unknown originalCoordinate ", tmp.cs)
            oceDebug(debug, "user$originalCoordinate: ", user$originalCoordinate, "\n")
            user$numberOfCells <- readBin(buf[o+35:36], "integer", n=1, size=2, endian="little")
            oceDebug(debug, "user$numberOfCells: ", user$numberOfCells, "\n")
            user$hBinLength <- readBin(buf[o+37:38], "integer", n=1, size=2, endian="little", signed=FALSE)
            oceDebug(debug, "user$hBinLength: ", user$hBinLength, " (p31 of System Integrator Guide)\n")
            ## The cell size is computed with different formulae for different devices, and
            ## different frequencies.  See
            ##   http://www.nortekusa.com/en/knowledge-center/forum/hr-profilers/736804717
            ## for the details (and note that there are some typos from the Nortek advisor, which
            ## get corrected later on that webpage).
            if (type == "aquadoppHR") {
                ## (NOTE: 1674 is hBinLength)
                ## CS = (1674/256)*0.00675*cos(25) = 0.04 m, for a 2 MHz instrument
                ## For a 1 MHz instrument you must multiply by twice the sampling distance, i.e. 0.0135 m
                if (isTRUE(all.equal.numeric(head$frequency, 2000))) {
                    user$cellSize <- user$hBinLength / 256 * 0.00675 * cos(25 * degToRad)
                    user$blankingDistance <- user$T2 * 0.00675 * cos(25 * degToRad) - user$cellSize
                } else if (isTRUE(all.equal.numeric(head$frequency, 1000))) {
                    user$cellSize <- user$hBinLength / 256 * 0.01350 * cos(25 * degToRad)
                    user$blankingDistance <- user$T2 * 0.01350 * cos(25 * degToRad) - user$cellSize
                } else {
                    warning("unknown frequency ", head$frequency, " (only understand 1MHz and 2MHz); using 2Mhz formula to calculate cell size")
                    user$cellSize <- user$hBinLength / 256 * 0.00675 * cos(25 * degToRad)
                    user$blankingDistance <- user$T2 * 0.00675 * cos(25 * degToRad) - user$cellSize
                }
            } else if (type == "aquadoppProfiler") {
                if (isTRUE(all.equal.numeric(head$frequency, 2000))) {
                    user$cellSize <- user$hBinLength / 256 * 0.0239 * cos(25 * degToRad)
                } else if (isTRUE(all.equal.numeric(head$frequency, 1000))) {
                    user$cellSize <- user$hBinLength / 256 * 0.0478 * cos(25 * degToRad)
                } else if (isTRUE(all.equal.numeric(head$frequency, 600))) {
                    user$cellSize <- user$hBinLength / 256 * 0.0797 * cos(25 * degToRad)
                } else if (isTRUE(all.equal.numeric(head$frequency, 400))) {
                    user$cellSize <- user$hBinLength / 256 * 0.1195 * cos(25 * degToRad)
                } else {
                    warning("unknown frequency", head$frequency, "(only understand 1MHz and 2MHz); using 1Mhz formula to calculate cell size")
                    user$cellSize <- user$hBinLength / 256 * 0.0478 * cos(25 * degToRad)
                    ##user$cellSize <- user$hBinLength / 256 * 0.00675 * cos(25 * degToRad)
                }
                user$blankingDistance <- user$T2 * 0.0229 * cos(25 * degToRad) - user$cellSize
            } else if (type == "aquadopp") {
                ##user$cellSize <- user$hBinLength / 256 * 0.00675 * cos(25 * degToRad)
                ##user$blankingDistance <- user$T2 * 0.00675 * cos(25 * degToRad) - user$cellSize
                warning("using fixed cell size and blanking distance for Aquadopp, since cannot infer these from the file")
                user$cellSize <- 0.75  # value for one particular test file
                user$blankingDistance <- 0.37 # value for one particular test file
            } else if (type == "vector") {
                ## Formula (revised) from Nortek http://www.nortekusa.com/en/knowledge-center/forum/hr-profilers/595666030
                ## Cell size (mm) = T3counts * 1000 * 750 / 480000
                soundspeed <- 1500
                user$cellSize <- user$receiveLength * (soundspeed / 2) / 480.0e3 # drop the 1000 to get m
                user$blankingDistance <- 0 # ?
            } else {
                warning("unknown instrument type \"", type, "\", so calculating cell size as though it is a 2MHz AquadoppHR")
                user$cellSize <- user$hBinLength / 256 * 0.00675 * cos(25 * degToRad)
                user$blankingDistance <- user$T2 * 0.00675 * cos(25 * degToRad) - user$cellSize
            }
            ## FIXME: eventually, incorporate the AWAC and Continental devices, using
            ## the above formulae, with coefficients as below (from the nortek forum)
            ##
            ## For the AWAC the table for cell size looks like this:
            ## 0.0478 for 1 MHz
            ## 0.0797 for 600 kHz
            ##
            ## And for the Continental:
            ## 0.0945 for 470 kHz
            ## 0.2221 for 190 kHz

            oceDebug(debug, "cellSize=", user$cellSize, "m (FIXME: using formula from unknown source)\n")
            user$measurementInterval <- readBin(buf[o+39:40], "integer", n=1, size=2, endian="little")
            oceDebug(debug, "measurementInterval=", user$measurementInterval, "\n")

            ## if (debug > 0) { # tests for issue146
            ##    cat("TEMPORARY debugging for branch 'issue145': attempt to infer from USER header whether we have extra analog data...\n")
            ##    modeBinaryTmp <- byteToBinary(buf[o+59], endian="big")
            ##    cat("TEST 1: modeBinaryTmp=", modeBinaryTmp, '(p34 of SIG 2011; should have info on analog in bit #3, measured from left or right though?)\n')
            ##for (iii in (-3):3) {
            ##    tmp <- buf[o+59+iii]
            ##    tmpb <- byteToBinary(buf[o+59+iii], endian="big")
            ##    cat("buf[o+59+", iii, "]", tmp, "x  ", tmpb, "\n")
            ##}
            ##                cat("docs say (I *think* about byte buf[o+59], but this could be out by a byte or two)
            ##  bit 0: use user specified sound speed (0=no, 1=yes)
            ##  bit 1: diagnostics/wave mode 0=disable, 1=enable)
            ##  bit 2: analog output mode (0=disable, 1=enable)
            ##  bit 3: output format (0=Vector, 1=ADV)
            ##  bit 4: scaling (0=1 mm, 1=0.1 mm)
            ##  bit 5: serial output (0=disable, 1=enable)
            ##  bit 6: reserved EasyQ
            ##  bit 7: stage (0=disable, 1=enable)
            ##  bit 8: output power for analog input (0=disable, 1=enable)
            ##\n")
            ##}

            #### FIXME: Sample.cpp has 0.022888 for the factor on user$T2
            ##if (isTRUE(all.equal.numeric(head$frequency, 1000))) {
            ##    ##user$blankingDistance <- cos(25*degToRad) * (0.0135 * user$T2 - 12 * user$T1 / head$frequency)
            ##    user$blankingDistance <- cos(25*degToRad) * (0.0135 * user$blankingDistance - 12 * user$transmitPulseLength / head$frequency)
            ##} else if (isTRUE(all.equal.numeric(head$frequency, 2000))) {
            ##    ##user$blankingDistance <- cos(25*degToRad) * (0.00675 * user$T2 - 12 * user$T1 / head$frequency)
            ##    user$blankingDistance <- cos(25*degToRad) * (0.00675 * user$blankingDistance - 12 * user$transmitPulseLength / head$frequency)
            ##} else {
            ##    user$blankingDistance <- 0
            ##}
            ##cat("adp.nortek.R:245 user$blankingDistance", user$blankingDistance, "\n")
            oceDebug(1+debug, "blankingDistance=", user$blankingDistance, "; user$T1=", user$T1, "and user$T2=", user$T2, "\n")
            user$swVersion <- readBin(buf[o+73:74], "integer", n=1, size=2, endian="little") / 10000
            oceDebug(debug, "swVersion=", user$swVersion, "\n")
            user$salinity <- readBin(buf[o+75:76], "integer", n=1, size=2, endian="little") * 0.1
            oceDebug(debug, "salinity=", user$salinity, "\n")
            o <- o + 2 * user$size
        } else {
            stop("cannot understand byte 0x", buf[o+1], "; expecting one of the following: 0x",
                 idHardwareConfiguration, " [hardware configuration] 0x",
                 idHeadConfiguration, " [head configuration] or 0x", idUserConfiguration, " [user configuration]\n")
        }
    }
    list(hardware=hardware, head=head, user=user, offset=o+1)
}

#' Read an AD2CP File
#'
#' This function, introduced in April 2017, is just a temporary interface
#' for separate interpretation code that lives in the 1219 issue
#' directory. THIS IS ONLY FOR DEVELOPERS.
#'
#' @param orientation Optional character string specifying the orientation of the
#' sensor, provided for those cases in which it cannot be inferred from the
#' data file.  The valid choices are \code{"upward"}, \code{"downward"}, and
#' \code{"sideward"}.
#' @param distance Optional vector holding the distances of bin centres from the
#' sensor.  This argument is ignored except for Nortek profilers, and need not
#' be given if the function determines the distances correctly from the data.
#' The problem is that the distance is poorly documented in the Nortek System
#' Integrator Guide (2008 edition, page 31), so the function must rely on
#' word-of-mouth formulae that do not work in all cases.
#' @param despike if \code{TRUE}, \code{\link{despike}} will be used to clean
#' anomalous spikes in heading, etc.
#' @template adpTemplate
#'
#' @examples
#' \dontrun{
#' f <- "/Users/kelley/Dropbox/oce_ad2cp/labtestsig3.ad2cp"
#' d <- read.ad2cp(f, 1, 10, 1)
#'}
#'
#' @author Dan Kelley
#'
#' @family things related to \code{adp} data
read.ad2cp <- function(file, from=1, to, by=1, tz=getOption("oceTz"),
                       longitude=NA, latitude=NA,
                       orientation, distance,
                       monitor=FALSE, despike=FALSE, processingLog,
                       debug=getOption("oceDebug"), ...)
{
    oceDebug(debug, "read.ad2cp(...,from=", format(from), ",to=", if (missing(to)) "(missing)" else format(to), "...)\n", sep="", unindent=1)
    if (missing(to))
        error("Must supply 'to'. (This is a temporary constraint, whilst read.ad2cp() is being developed.)")
    if (is.character(file)) {
        filename <- fullFilename(file)
        file <- file(file, "rb")
        on.exit(close(file))
    } else {
        filename <- "(connection)"
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        filename <- "(connection)"
        open(file, "rb")
        on.exit(close(file))
    }
    seek(file, 0, "start")
    seek(file, 0, "start")
    ## go to the end, so the next seek (to get to the data) reveals file length
    seek(file, where=0, origin="end")
    fileSize <- seek(file, where=0)
    oceDebug(debug, "fileSize:", fileSize, "\n")
    buf <- readBin(file, what="raw", n=fileSize, size=1)
    oceDebug(debug, 'first 10 bytes in file: ',
             paste(paste("0x", buf[1+0:9], sep=""), collapse=" "), "\n", sep="")
    headerSize <- as.integer(buf[2])
    oceDebug(debug, "headerSize:", headerSize, "\n")
    ID <- buf[3]
    oceDebug(debug, "ID: 0x", ID, " (NB: 0x15=burst data record; 0x16=avg data record; 0x17=bottom track record; 0x18=interleaved data record; 0xa0=string data record, e.g. GPS NMEA, comment from the FWRITE command\n", sep="")
    dataSize <- readBin(buf[5:6], what="integer", n=1, size=2, endian="little", signed=FALSE)
    oceDebug(debug, "dataSize:", dataSize, "\n")
    ## if (ID == 0xa0) {
    ##     oceDebug(debug, "type is 0xa0 so trying to read a string...\n")
    ##     a <- readBin(buf[headerSize+1:dataSize], "character", 1)
    ##     text <- gsub("\\r","",strsplit(a, "\\n")[[1]])
    ##     print(text)
    ## }
    oceDebug(debug, "buf[1+headerSize+dataSize=", 1+headerSize+dataSize, "]=0x", buf[1+headerSize+dataSize], " (expect 0xa5)\n", sep="")
    ## if (0xa5 == buf[1+headerSize+dataSize]) {
    ##     o <- 1 + headerSize + dataSize
    ##     oceDebug(debug, 'record 2 starts at BUF[', o, "]\n", sep="")
    ##     oceDebug(debug, 'first 10 bytes in record 2: ',
    ##              paste(paste("0x", buf[o+0:9], sep=""), collapse=" "),
    ##              "\n", sep="")
    ##     tst <- readBin(buf[o+headerSize+1:dataSize], "character", 1)
    ## }
    nav <- do_ldc_ad2cp_in_file(filename, from, to, by)
    d <- list(buf=buf, index=nav$index, length=nav$length, id=nav$id)
    res <- new("adp")
    if (0x10 == d$buf[d$index[1]+1]) # 0x10 = AD2CP (p38 integrators guide)
        res <- oceSetMetadata(res, "instrumentType", "AD2CP")
    ## DK NOTES FROM A SAMPLE FILE...
    ## I know that the second one is a data/burst chunk. I think it is
    ## version "3", at least in the beginning ones. Certainly, the time
    ## fields seem (mostly) to be ok (except that they are slightly
    ## out of order).
    if (by != 1)
        stop("must have by=1 for this preliminary version of read.ad2cp()")
    N <- as.integer((to - from) / by)
    if (N <= 0)
        stop("must have to > from")
    time <- vector("numeric", N)
    id <- vector("numeric", N)
    version <- vector("numeric", N)
    soundSpeed <- vector("numeric", N)
    temperature <- vector("numeric", N)
    pressure <- vector("numeric", N)
    heading <- vector("numeric", N)
    pitch <- vector("numeric", N)
    roll <- vector("numeric", N)
    BCC <- vector("character", N)
    coordinateSystem <- vector("character", N)
    ncells <- vector("numeric", N)
    nbeams <- vector("numeric", N)
    cellSize <- vector("numeric", N)
    blanking <- vector("numeric", N)
    nominalCorrelation <- vector("numeric", N)
    accelerometerx <- vector("numeric", N)
    accelerometery <- vector("numeric", N)
    accelerometerz <- vector("numeric", N)
    transmitEnergy <- vector("numeric", N)
    velocityScaling <- vector("numeric", N)
    powerLevel <- vector("numeric", N)
    temperatureMagnetometer <- vector("numeric", N)
    temperatureRTC <- vector("numeric", N)
    status <- vector("integer", N)
    ensemble <- vector("numeric", N)
    v <- vector("list", N)
    amplitude <- vector("list", N)
    correlation <- vector("list", N)

    for (ch in 1:N) {
        id[ch] <- d$id[ch]
        if (d$id[ch] == 160) {
            ## skip an extra byte at start because it is a code
            chars <- rawToChar(d$buf[seq.int(2+d$index[ch], by=1, length.out=-1+d$length[ch])])
            header <- strsplit(chars, "\r\n")[[1]]
        } else if (d$id[ch] %in% c(21, 22)) {
            version[ch] <- as.integer(d$buf[d$index[ch]+1])
            if (version[ch] == 3) {
                ## oceDebug(debug, "got version[ch=", ch, "]==3 (if more than once, consider caching)\n", sep="")
                offsetOfData <- as.integer(d$buf[d$index[ch]+2])
                ## if (ch==2) message("offsetOfData=", offsetOfData)
                i <- d$index[ch]
                if (is.null(res@metadata$serialNumber)) # I'm not sure if this speeds up or not
                    res@metadata$serialNumber <- readBin(d$buf[i+5:8], "integer", size=4, endian="little")
                year<-as.integer(d$buf[i+9])
                month <- 1 + as.integer(d$buf[i+10]) # starts at 0, based on matlab valueults
                day<-as.integer(d$buf[i+11])
                hour<-as.integer(d$buf[i+12])
                min<-as.integer(d$buf[i+13])
                sec<-as.integer(d$buf[i+14])
                usec100 <- readBin(d$buf[d$index[ch]+15:16], "integer", size=2, signed=FALSE, endian="little")
                t <- ISOdatetime(1900+year,month,day,hour,min,sec+usec100/1e4,tz="UTC")
                time[ch] <- t
                soundSpeed[ch] <- 0.1 * readBin(d$buf[i+17:18], "integer", size=2, endian="little")
                temperature[ch] <- 0.01 * readBin(d$buf[i+19:20], "integer", size=2, endian="little")
                pressure[ch] <- 0.001 * readBin(d$buf[i+21:24], "integer", size=4, endian="little")
                heading[ch] <- 0.01 * readBin(d$buf[i+25:26], "integer", size=2, endian="little")
                pitch[ch] <- 0.01 * readBin(d$buf[i+27:28], "integer", size=2, endian="little")
                roll[ch] <- 0.01 * readBin(d$buf[i+29:30], "integer", size=2, endian="little")
                ## Notes on decoding the nbeam/coord/ncell structure.
                ## According to p47 of the ad2cp doc:
                ##  bits  9 to  0: ncells
                ##  bits 11 to 10: coord system (00=enu, 01=xyz, 10=beam, 11=NA)
                ##  bits 15 to 12: nbeams
                ##
                ## Try using intToBits() etc ... these functions return with least-signficant
                ## bit first, e.g. 2 decimal corvalueponds to 0,1 and not 1,0 as one would write
                ## on paper. This has to be borne in mind whilst reading the Nortek documents,
                ## which list e.g. bit 9 to bit 0 (which in R, with intToBits() etc,
                ## corvalueponds to bit 1 to bit 10).
                ##
                ## ch==2 (burst) expect nbeam=1, ncell=256, "beam"(?)
                ## > substr(paste(ifelse(intToBits(256)==0x01, "1", "0"), collapse=""), 1, 10)
                ## [1] "0000000010"
                ## > substr(paste(ifelse(intToBits(1)==0x01, "1", "0"), collapse=""), 1, 4)
                ## [1] "1000"
                ## > paste(ifelse(rawToBits(d$buf[i+31:32])==0x01, "1", "0"), collapse="")
                ## [1] "0000000010011000"
                ## bit  0 to  9: 0000000010 OK
                ## bit 10 to 11: 01 OK
                ## bit 12 to 15: 1000 OK
                ##
                ## ch==3 (avg) expect nbeam=4, ncell=150, "beam"(?)
                ## ncell=150 so expect bit pattern
                ## > substr(paste(ifelse(intToBits(150)==0x01, "1", "0"), collapse=""), 1, 10)
                ## [1] "0110100100"
                ## > substr(paste(ifelse(intToBits(4)==0x01, "1", "0"), collapse=""), 1, 4)
                ## [1] "0010"
                ## > paste(ifelse(rawToBits(d$buf[i+31:32])==0x01, "1", "0"), collapse="")
                ## [1] "0110100100010010"
                ## bit  0 to  9: 0110100100 OK
                ## bit 10 to 11: 01 OK
                ## bit 12 to 15: 0010 OK

                bits <- rawToBits(d$buf[i+31:32])==0x01
                ncells[ch] <- sum(unlist(lapply(1:10, function(i) bits[0+i]*2^(i-1))))
                nbeams[ch] <- sum(unlist(lapply(1:4, function(i) bits[12+i]*2^(i-1))))
                BCC[ch] <- paste(ifelse(bits, "1", "0"), collapse="")
                coordinateSystem[ch] <- c("enu", "xyz", "beam", "?")[1+bits[11]+2*bits[12]]
                cellSize[ch] <- 0.001 * readBin(d$buf[i+33:34], "integer", size=2, signed=FALSE, endian="little")
                ## FIXME: the docs say mm for blanking, but cm matches matlab and the .cfg file
                ## Update from nortek forum: the blanking unit depends on the configuration; see
                ## http://www.nortek-as.com/en/knowledge-center/forum/current-profilers-and-current-meters/519140451
                ## NOTE that we may multiply blanking[ch] by 10 to get cm, a few lines below when we check
                ## status.
                blanking[ch] <- 0.001 * readBin(d$buf[i+35:36], "integer", size=2, signed=FALSE, endian="little")
                nominalCorrelation[ch] <- as.integer(d$buf[i+37])
                ## skipping some variables
                accelerometerz[ch] <- 1/16384 * readBin(d$buf[i+51:52], "integer", size=2, signed=TRUE, endian="little")
                ## skipping some variables
                transmitEnergy[ch] <- readBin(d$buf[i+57:58], "integer", size=2, signed=FALSE, endian="little")
                velocityScaling[ch] <- readBin(d$buf[i+59], "integer", size=1, signed=TRUE, endian="little")
                velocityFactor <- 10^velocityScaling[ch]
                powerLevel[ch] <- readBin(d$buf[i+60], "integer", size=1, signed=TRUE, endian="little")
                temperatureMagnetometer[ch] <- 0.001 * readBin(d$buf[i+61:62], "integer", size=2, signed=TRUE, endian="little")
                temperatureRTC[ch] <- 0.01 * readBin(d$buf[i+63:64], "integer", size=2, endian="little")
                error <- readBin(d$buf[i+65:68], "integer", size=4, endian="little") # NOT USED YET
                status[ch] <- readBin(d$buf[i+69:72], "integer", size=4, endian="little")
                statusBits <- intToBits(status[ch])
                if (statusBits[2] == 0x01) {
                    blanking[ch] <- blanking[ch] * 10
                }
                ## if (ch<5) message("status= ", paste(statusBits, collapse=" "))
                ensemble[ch] <- readBin(d$buf[i+73:76], "integer", size=4, endian="little")
                ## 77=1+offsetOfData
                nn <- nbeams[ch] * ncells[ch]
                nbytes <- 2 * nn
                velocity <- velocityFactor * readBin(d$buf[i+77+seq(0, nbytes-1)],
                                                     "integer", size=2, n=nbeams[ch]*ncells[ch],
                                                     endian="little")
                v[[ch]] <- matrix(velocity, ncol=nbeams[ch], nrow=ncells[ch], byrow=FALSE)
                amp <- d$buf[i+77+nbytes+seq(0, nn-1)]
                amplitude[[ch]] <- matrix(amp, ncol=nbeams[ch], nrow=ncells[ch], byrow=FALSE)
                cor <- d$buf[i+77+nbytes+nn+seq(0, nn-1)]
                correlation[[ch]] <- matrix(cor, ncol=nbeams[ch], nrow=ncells[ch], byrow=FALSE)
                ## FIXME: read correlation etc
                oceDebug(debug, "chunk ", ch, " decoded\n")
            } else {
                oceDebug(debug, "chunk ", ch, " is id=", d$id[ch], " version=", version, "\n", sep="")
            }
        } else {
            oceDebug(debug, "chunk ", ch, " is id=", d$id[ch], "\n", sep="")
        }
    }
    time <- as.POSIXct(time, origin="1970-01-01 00:00:00", tz="UTC")
    value <- list(header=header,
                  time=time, id=id, vsn=version,
                  soundSpeed=soundSpeed, temperature=temperature, pressure=pressure,
                  heading=heading, pitch=pitch, roll=roll,
                  BCC=BCC, coord=coordinateSystem, nbeams=nbeams, ncells=ncells,
                  cellSize=cellSize, blanking=blanking,
                  nomcor=nominalCorrelation,
                  accz=accelerometerz,
                  transmitEnergy=transmitEnergy,
                  velocityScaling=velocityScaling,
                  powerLevel=powerLevel,
                  temperatureMagnetometer=temperatureMagnetometer,
                  temperatureRTC=temperatureRTC,
                  status=status,
                  ensemble=ensemble,
                  v=v, amplitude=amplitude, correlation=correlation)

    res@metadata$header <- header
    res@metadata$id <- id
    res@metadata$numberOfBeams <- 4
    firstVelo <- which(id == 22)[1]
    res@metadata$numberOfSamples <- sum(id == 22)
    res@metadata$numberOfCells <- dim(v[[firstVelo]])[1] # FIXME: is this the same for all data records?

    ## Delete some temporary working items
    value$header <- NULL
    value$time <- NULL
    value$id <- NULL
    res@data <- value
    res@metadata$cellSize <- cellSize[firstVelo]
    res@data$distance <- blanking[firstVelo] + res@metadata$cellSize*1:res@metadata$numberOfCells
    res@data$time <- time
    if (missing(processingLog))
        processingLog <- paste("read.ad2cp(file=\"", filename, "\", from=", from, ", to=", to, ", by=", by, ")", sep="")
    res@processingLog <- processingLogItem(processingLog)
    oceDebug(debug, "} # read.ad2cp()\n", unindent=1)
    res
}

#' Read a Nortek Aquadopp File
#'
#' The R code is based on information in
#' the Nortek System Integrator Guide (2008) and on postings on the Nortek
#' ``knowledge center'' discussion board.  One might assume that the latter is
#' less authoritative than the former.  For example, the inference of cell size
#' follows advice found at
#' \url{http://www.nortekusa.com/en/knowledge-center/forum/hr-profilers/736804717}
#' (downloaded June 2012), which contains a typo in an early posting that is
#' corrected later on.
#'
#' @param orientation Optional character string specifying the orientation of the
#' sensor, provided for those cases in which it cannot be inferred from the
#' data file.  The valid choices are \code{"upward"}, \code{"downward"}, and
#' \code{"sideward"}.
#' @param distance Optional vector holding the distances of bin centres from the
#' sensor.  This argument is ignored except for Nortek profilers, and need not
#' be given if the function determines the distances correctly from the data.
#' The problem is that the distance is poorly documented in the Nortek System
#' Integrator Guide (2008 edition, page 31), so the function must rely on
#' word-of-mouth formulae that do not work in all cases.
#' @param despike if \code{TRUE}, \code{\link{despike}} will be used to clean
#' anomalous spikes in heading, etc.
#'
#' @references
#' 1. Information on Nortek profilers (including the System Integrator Guide,
#' which explains the data format byte-by-byte) is available at
#' \url{http://www.nortekusa.com/}.  (One must join the site to see the
#' manuals.)
#'
#' 2. The Nortek Knowledge Center
#' \url{http://www.nortekusa.com/en/knowledge-center} may be of help if
#' problems arise in dealing with data from Nortek instruments.
#'
#' @template adpTemplate
#'
#' @author Dan Kelley
#'
#' @family things related to \code{adp} data
read.aquadopp <- function(file, from=1, to, by=1, tz=getOption("oceTz"),
                          longitude=NA, latitude=NA,
                          orientation, distance,
                          monitor=FALSE, despike=FALSE, processingLog,
                          debug=getOption("oceDebug"), ...)
{
    return(read.adp.nortek(file, from=from, to=to, by=by, tz=tz,
                           longitude=longitude, latitude=latitude,
                           type="aquadopp", orientation=orientation, distance=distance,
                           monitor=monitor, despike=despike, processingLog=processingLog,
                           debug=debug, ...))
}


#' Read Nortek Aquadopp-HR File
#'
#' The R code is based on information in
#' the Nortek System Integrator Guide (2008) and on postings on the Nortek
#' ``knowledge center'' discussion board.  One might assume that the latter is
#' less authoritative than the former.  For example, the inference of cell size
#' follows advice found at
#' \url{http://www.nortekusa.com/en/knowledge-center/forum/hr-profilers/736804717}
#' (downloaded June 2012)), which contains a typo in an early posting that is
#' corrected later on.
#'
#' @param despike if \code{TRUE}, \code{\link{despike}} will be used to clean
#' anomalous spikes in heading, etc.
#' @param orientation Optional character string specifying the orientation of the
#' sensor, provided for those cases in which it cannot be inferred from the
#' data file.  The valid choices are \code{"upward"}, \code{"downward"}, and
#' \code{"sideward"}.
#' @param distance Optional vector holding the distances of bin centres from the
#' sensor.  This argument is ignored except for Nortek profilers, and need not
#' be given if the function determines the distances correctly from the data.
#' The problem is that the distance is poorly documented in the Nortek System
#' Integrator Guide (2008 edition, page 31), so the function must rely on
#' word-of-mouth formulae that do not work in all cases.
#'
#' @references
#' 1. Information on Nortek profilers (including the System Integrator Guide,
#' which explains the data format byte-by-byte) is available at
#' \url{http://www.nortekusa.com/}.  (One must join the site to see the
#' manuals.)
#'
#' 2. The Nortek Knowledge Center
#' \url{http://www.nortekusa.com/en/knowledge-center} may be of help if
#' problems arise in dealing with data from Nortek instruments.
#'
#' @template adpTemplate
#'
#' @author Dan Kelley
#'
#' @family things related to \code{adp} data
read.aquadoppHR <- function(file, from=1, to, by=1, tz=getOption("oceTz"),
                            longitude=NA, latitude=NA,
                            orientation=orientation, distance,
                            monitor=FALSE, despike=FALSE, processingLog,
                            debug=getOption("oceDebug"), ...)
{
    return(read.adp.nortek(file, from=from, to=to, by=by, tz=tz,
                           longitude=longitude, latitude=latitude,
                           type="aquadoppHR",
                           orientation=orientation, distance=distance,
                           monitor=monitor, despike=despike, processingLog=processingLog,
                           debug=debug, ...))
}


#' Read a Nortek Aquadopp-Profiler File
#'
#' The R code is based on information in
#' the Nortek System Integrator Guide (2008) and on postings on the Nortek
#' ``knowledge center'' discussion board.  One might assume that the latter is
#' less authoritative than the former.  For example, the inference of cell size
#' follows advice found at
#' \url{http://www.nortekusa.com/en/knowledge-center/forum/hr-profilers/736804717}
#' (downloaded June 2012)), which contains a typo in an early posting that is
#' corrected later on.
#'
#' @param despike if \code{TRUE}, \code{\link{despike}} will be used to clean
#' anomalous spikes in heading, etc.
#' @param orientation Optional character string specifying the orientation of the
#' sensor, provided for those cases in which it cannot be inferred from the
#' data file.  The valid choices are \code{"upward"}, \code{"downward"}, and
#' \code{"sideward"}.
#' @param distance Optional vector holding the distances of bin centres from the
#' sensor.  This argument is ignored except for Nortek profilers, and need not
#' be given if the function determines the distances correctly from the data.
#' The problem is that the distance is poorly documented in the Nortek System
#' Integrator Guide (2008 edition, page 31), so the function must rely on
#' word-of-mouth formulae that do not work in all cases.
#'
#' @references
#' 1. Information on Nortek profilers (including the System Integrator Guide,
#' which explains the data format byte-by-byte) is available at
#' \url{http://www.nortekusa.com/}.  (One must join the site to see the
#' manuals.)
#'
#' 2. The Nortek Knowledge Center
#' \url{http://www.nortekusa.com/en/knowledge-center} may be of help if
#' problems arise in dealing with data from Nortek instruments.
#'
#' @template adpTemplate
#'
#' @author Dan Kelley
#'
#' @family things related to \code{adp} data
read.aquadoppProfiler <- function(file, from=1, to, by=1, tz=getOption("oceTz"),
                                  longitude=NA, latitude=NA,
                                  orientation, distance,
                                  monitor=FALSE, despike=FALSE, processingLog,
                                  debug=getOption("oceDebug"), ...)
{
    return(read.adp.nortek(file, from=from, to=to, by=by, tz=tz,
                           longitude=longitude, latitude=latitude,
                           type="aquadoppProfiler",
                           orientation=orientation, distance=distance,
                           monitor=monitor, despike=despike, processingLog=processingLog,
                           debug=getOption("oceDebug"), ...))
}

#' Read a Nortek ADP File
#'
#' @param despike if \code{TRUE}, \code{\link{despike}} will be used to clean
#' anomalous spikes in heading, etc.
#' @param type a character string indicating the type of instrument.
#' @param orientation optional character string specifying the orientation of
#' the sensor, provided for those cases in which it cannot be inferred from the
#' data file.  The valid choices are \code{"upward"}, \code{"downward"}, and
#' \code{"sideward"}.
#' @param distance optional vector holding the distances of bin centres from
#' the sensor.  This argument is ignored except for Nortek profilers, and need
#' not be given if the function determines the distances correctly from the
#' data.  The problem is that the distance is poorly documented in the Nortek
#' System Integrator Guide (2008 edition, page 31), so the function must rely
#' on word-of-mouth formulae that do not work in all cases.
#'
#' @references
#' 1. Information on Nortek profilers (including the System Integrator Guide,
#' which explains the data format byte-by-byte) is available at
#' \url{http://www.nortekusa.com/}.  (One must join the site to see the
#' manuals.)
#'
#' 2. The Nortek Knowledge Center
#' \url{http://www.nortekusa.com/en/knowledge-center} may be of help if
#' problems arise in dealing with data from Nortek instruments.
#'
#' @template adpTemplate
#'
#' @author Dan Kelley
#'
#' @family things related to \code{adp} data
read.adp.nortek <- function(file, from=1, to, by=1, tz=getOption("oceTz"),
                            longitude=NA, latitude=NA,
                            type=c("aquadoppHR", "aquadoppProfiler", "aquadopp"),
                            orientation, distance,
                            monitor=FALSE, despike=FALSE, processingLog,
                            debug=getOption("oceDebug"),
                            ...)
{
    ##degToRad <- atan2(1, 1) / 45
    profileStart <- NULL # prevents scope warning in rstudio; defined later anyway
    bisectAdpNortek <- function(buf, t.find, add=0, debug=0) {
        oceDebug(debug, "bisectAdpNortek(t.find=", format(t.find), ", add=", add, "\n")
        len <- length(profileStart)
        lower <- 1
        upper <- len
        passes <- floor(10 + log(len, 2)) # won't need this many; only do this to catch coding errors
        for (pass in 1:passes) {
            middle  <- floor( (upper + lower) / 2 )
            minute  <- bcdToInteger(buf[profileStart[middle] + 4])
            second  <- bcdToInteger(buf[profileStart[middle] + 5])
            day     <- bcdToInteger(buf[profileStart[middle] + 6])
            hour    <- bcdToInteger(buf[profileStart[middle] + 7])
            year    <- bcdToInteger(buf[profileStart[middle] + 8])
            year    <- year + ifelse(year >= 90, 1900, 2000)
            month   <- bcdToInteger(buf[profileStart[middle] + 9])
            sec1000 <- bcdToInteger(buf[profileStart[middle] + 10])
            t <- ISOdatetime(year, month, day, hour, minute, second + sec1000/1000, tz=tz)
            oceDebug(debug, "t=", format(t), "| (from data",
                     sprintf("%4d-%02d-%02d", year, month, day),
                     sprintf("%02d:%02d:%02d.%03d", hour, minute, second, sec1000), ") | pass",
                     format(pass, width=2), "/", passes, " | middle=", middle, "(", middle/upper*100, "%)\n")
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
    if (missing(to))
        to <- NA                       # will catch this later
    oceDebug(debug, "read.adp.nortek(...,from=", format(from), ",to=", format(to), "...)\n")
    res <- new("adp")
    ##fromKeep <- from
    ##toKeep <- to
    ##syncCode <- as.raw(0xa5)
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
    header <- decodeHeaderNortek(buf, type=type, debug=debug-1)
    ##averagingInterval <- header$user$averagingInterval
    numberOfBeams <- header$numberOfBeams
    numberOfCells <- header$numberOfCells
    ##bin1Distance <- header$bin1Distance
    ##xmitPulseLength <- header$xmitPulseLength
    ##cellSize <- header$cellSize
    ##profilesInFile <- readBin(buf[header$offset + 2:3], what="integer", n=1, size=2, endian="little")
    oceDebug(debug, "profile data at buf[", header$offset, "] et seq.\n")
    oceDebug(debug, "matching bytes: 0x", buf[header$offset], " 0x", buf[header$offset+1], " 0x", buf[header$offset+2], '\n', sep="")
    profileStart <- .Call("match3bytes", buf, buf[header$offset], buf[header$offset+1], buf[header$offset+2])
    profilesInFile <- length(profileStart)
    if (is.na(to))
        to <- profilesInFile
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
        fromPair <- bisectAdpNortek(buf, from, -1, debug-1)
        from <- fromIndex <- fromPair$index
        toPair <- bisectAdpNortek(buf, to, 1, debug-1)
        to <- toIndex <- toPair$index
        oceDebug(debug, "  from=", format(fromPair$t), " yields profileStart[", fromIndex, "]\n",
                  "  to  =", format(toPair$t),   " yields profileStart[", toIndex, "]\n",
                  "  by=", by, "s\n",
                  "profileStart[1:10]=", profileStart[1:10], "\n",
                  "profileStart[", fromPair$index, "]=", profileStart[fromPair$index], "at time", format(fromPair$t), "\n",
                  "profileStart[",   toPair$index, "]=", profileStart[  toPair$index], "at time", format(  toPair$t), "\n")
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
        oceDebug(debug, "dt=", dt, "s; at this stage, by=", by, "(not interpreted yet)\n")
        profileStart <- profileStart[profileStart[fromIndex] < profileStart & profileStart < profileStart[toIndex]]
        if (is.character(by))
            by <- floor(0.5 + ctimeToSeconds(by) / dt)
        oceDebug(debug, "by=", by, "profiles (after change)\n")
        profileStart <- profileStart[seq(1, length(profileStart), by=by)]
        oceDebug(debug, 'dt=', dt, '\n', 'by=', by, "profileStart[1:10] after indexing:", profileStart[1:10], "\n")
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
    oceDebug(debug, "profilesToRead=", profilesToRead, "\n")
    profileStart2 <- sort(c(profileStart, profileStart+1)) # use this to subset for 2-byte reads
    numberOfCells <- header$user$numberOfCells
    numberOfBeams <- header$head$numberOfBeams
    oceDebug(debug, "numberOfCells=", numberOfCells, "\n")
    oceDebug(debug, "numberOfBeams=", numberOfBeams, "\n")
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
    ## aquadopp error: see table 5.4 (p40) and table 5.10 (p53) of system-integrator-manual_jan2011.pdf
    error <- readBin(buf[profileStart2 + 10], what="integer", n=profilesToRead, size=2, endian="little", signed=FALSE)
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
    velocityScale <- 1e-3              # FIXME: why not use the value in user$velocityScale?
    ## FIXME: why does 54 work, given 53 in docs? [see 38 of System Integrator Guide]
    oShift <- switch(type, aquadoppHR=54, aquadoppProfiler=30, aquadopp=30)
    for (i in 1:profilesToRead) {
        o <- profileStart[i] + oShift
        ##oceDebug(debug, 'getting data chunk',i,' at file position',o,'\n')
        v[i, , ] <- velocityScale * matrix(readBin(buf[o + seq(0, 2*items-1)], "integer", n=items, size=2, endian="little", signed=TRUE), ncol=numberOfBeams, byrow=FALSE)
        o <- o + items * 2
        a[i, , ] <- matrix(buf[o + seq(0, items-1)], ncol=items, byrow=TRUE)
        o <- o + items
        q[i, , ] <- matrix(buf[o + seq(0, items-1)], ncol=items, byrow=TRUE) # FIXME: this is correlation, not quality
        if (monitor) {
            cat(".", ...)
            if (!(i %% 50)) cat(i, "\n", ...)
        }
    }
    if (monitor) cat("\nRead", profilesToRead,  "of the", profilesInFile, "profiles in", filename, "\n", ...)

    if (missing(distance)) {
        distance <- seq(header$user$blankingDistance+header$user$cellSize, by=header$user$cellSize, length.out=header$user$numberOfCells)
    } else {
        if (length(distance) != dim(v)[2])
            stop("the argument distance is of length ", length(distance), ", which does not match the second dimension of the velocity matrix, ", dim(v)[2])
    }

    ## get diagnostic data, if any, and trim them to same index range as conventional data
    if (type == "aquadopp") {
        ##warning("read.aquadopp() is still in development.  BUG: vDiag mismatch to ascii file is up to 3 times the rounding error of the ascii (.dia) file.")
        diaStart <- .Call("match3bytes", buf, 0xa5, 0x80, 0x15)
        oceDebug(debug, "diaStart range:", range(diaStart), "\n")
        diaStart <- subset(diaStart, diaStart >= profileStart[fromIndex])
        diaStart <- subset(diaStart, diaStart <= profileStart[toIndex])
        oceDebug(debug, "LATER diaStart range:", range(diaStart), "\n")
        diaToRead <- length(diaStart)
        diaStart2 <- sort(c(diaStart, diaStart+1))
        timeDia <- ISOdatetime(2000+bcdToInteger(buf[diaStart+8]),
                               bcdToInteger(buf[diaStart+9]), # month
                               bcdToInteger(buf[diaStart+6]), # day
                               bcdToInteger(buf[diaStart+7]), # hour
                               bcdToInteger(buf[diaStart+4]), # min
                               bcdToInteger(buf[diaStart+5]), # sec
                               tz=tz)
        ## aquadopp error: see table 5.4 (p40) and table 5.10 (p53) of system-integrator-manual_jan2011.pdf
        errorDia <- readBin(buf[diaStart2 + 10], what="integer", n=diaToRead, size=2, endian="little", signed=FALSE)
        headingDia <- 0.1 * readBin(buf[diaStart2 + 18], what="integer", n=diaToRead, size=2, endian="little", signed=TRUE)
        pitchDia <- 0.1 * readBin(buf[diaStart2 + 20], what="integer", n=diaToRead, size=2, endian="little", signed=TRUE)
        rollDia <- 0.1 * readBin(buf[diaStart2 + 22], what="integer", n=diaToRead, size=2, endian="little", signed=TRUE)
        pressureMSB <- readBin(buf[diaStart + 24], what="integer", n=diaToRead, size=1, endian="little", signed=FALSE)
        pressureLSW <- readBin(buf[diaStart2 + 26], what="integer", n=diaToRead, size=2, endian="little", signed=FALSE)
        pressureDia <- (as.integer(pressureMSB)*65536 + pressureLSW) * 0.001
        temperatureDia <- 0.01 * readBin(buf[diaStart2 + 28], what="integer", n=diaToRead, size=2, endian="little")
        vDia <- array(double(), dim=c(diaToRead,  1,  3))
        vDia[, , 1] <- 0.001 * readBin(buf[diaStart2 + 30], what="integer", n=diaToRead, size=2, endian="little", signed=TRUE)
        vDia[, , 2] <- 0.001 * readBin(buf[diaStart2 + 32], what="integer", n=diaToRead, size=2, endian="little", signed=TRUE)
        vDia[, , 3] <- 0.001 * readBin(buf[diaStart2 + 34], what="integer", n=diaToRead, size=2, endian="little", signed=TRUE)
        aDia <- array(raw(), dim=c(diaToRead,  1,  3))
        aDia[, , 1] <- buf[diaStart + 36]
        aDia[, , 2] <- buf[diaStart + 37]
        aDia[, , 3] <- buf[diaStart + 38]
    }

    res@data <- list(v=v, a=a, q=q,
                     distance=distance,
                     time=time,
                     pressure=pressure,
                     error=error,
                     temperature=temperature,
                     heading=heading,
                     pitch=pitch,
                     roll=roll)
    ## Sometimes there can be an extra sample, with a time of
    ## NA. Because the times are continuous without the extra sample,
    ## for now I'll just remove the entire sample
    tNA <- which(is.na(time))
    if (length(tNA) > 0) {
        for (field in names(res@data)) {
            if (!(field %in% 'distance')) {
                if (field %in% c('v', 'a', 'q')) {
                    res@data[[field]] <- res@data[[field]][-tNA, , , drop=FALSE]
                } else {
                    res@data[[field]] <- res@data[[field]][-tNA]
                }
            }
        }
        warning(paste('Found and removed', length(tNA), 'NAs in the time vector.'))
    }

    if (type == "aquadopp" && diaToRead > 0) {
        ## FIXME: there may be other things here, e.g. does it try to measure salinity?
        res@data$timeDia <- timeDia
        res@data$errorDia <- errorDia
        res@data$headingDia <- headingDia
        res@data$pitchDia <- pitchDia
        res@data$rollDia <- rollDia
        res@data$pressureDia <- pressureDia
        res@data$temperatureDia <- temperatureDia
        res@data$vDia <- vDia
        res@data$aDia <- aDia
    }

    if (missing(orientation)) {
        orientation <- header$head$configTiltSensorOrientation # FIXME: is there a 'header$head'?
    } else {
        orientation <- match.arg(orientation, c("sideward", "upward", "downward"))
    }
    res@metadata$manufacturer <- "nortek"
    res@metadata$instrumentType <- type #"aquadopp-hr"
    res@metadata$filename <- filename
    res@metadata$latitude <- latitude
    res@metadata$longitude <- longitude
    res@metadata$numberOfSamples <- dim(v)[1]
    res@metadata$numberOfCells <- dim(v)[2]
    res@metadata$numberOfBeams <- dim(v)[3]
    res@metadata$numberOfBeamSequencesPerBurst <- header$user$numberOfBeamSequencesPerBurst
    res@metadata$measurementStart <- measurementStart
    res@metadata$measurementEnd <- measurementEnd
    res@metadata$measurementDeltat <- measurementDeltat
    res@metadata$subsampleStart <- time[1]
    res@metadata$subsampleEnd <- time[length(time)]
    res@metadata$subsampleDeltat <- as.numeric(time[2]) - as.numeric(time[1])
    res@metadata$size <- header$head$size
    res@metadata$serialNumber <- header$hardware$serialNumber
    res@metadata$internalCodeVersion <- header$hardware$picVersion
    res@metadata$hardwareRevision <- header$hardware$hwRevision
    res@metadata$recSize <- header$hardware$recSize
    res@metadata$velocityRange <- header$hardware$velocityRange # FIXME: should check against velocityMaximum
    res@metadata$firmwareVersion <- header$hardware$fwVersion
    res@metadata$config <- header$hardware$config
    res@metadata$configPressureSensor <- header$head$configPressureSensor
    res@metadata$configMagnetometerSensor <- header$head$configMagnetometerSensor
    res@metadata$configTiltSensor <- header$head$configTiltSensor
    res@metadata$beamAngle <- 25     # FIXME: may change with new devices
    res@metadata$tiltSensorOrientation <- header$head$tiltSensorOrientation
    res@metadata$orientation <- orientation
    res@metadata$frequency <- header$head$frequency
    res@metadata$headSerialNumber <- header$head$headSerialNumber
    res@metadata$bin1Distance <- header$user$blankingDistance # FIXME: is this right?
    res@metadata$blankingDistance <- header$user$blankingDistance
    res@metadata$measurementInteres <- header$user$measurementInteres
    res@metadata$transformationMatrix <- header$head$transformationMatrix
    res@metadata$deploymentName <- header$user$deploymentName
    res@metadata$cellSize <- header$user$cellSize
    res@metadata$velocityResolution <- velocityScale
    res@metadata$velocityMaximum <- velocityScale * 2^15
    res@metadata$originalCoordinate <- header$user$originalCoordinate
    res@metadata$oceCoordinate <- header$user$originalCoordinate
    res@metadata$oceBeamUnspreaded <- FALSE
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

    if (missing(processingLog))
        processingLog <- paste("read.adp.nortek(file=\"", filename, "\", from=", from, ", to=", to, ", by=", by, ")", sep="")
    res@processingLog <- processingLogItem(processingLog)
    res
}                                       # read.adp.nortek()
