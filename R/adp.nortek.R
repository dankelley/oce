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
#' This function is incomplete in several ways, and is still in active
#' development. Users should not expect a stable version until the
#' summer of 2019.
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
#' @param plan Optional integer specifying which 'plan' to focus on (see [1]
#' for the meaning of 'plan').  If this is not given, it defaults to the most
#' common plan in the requested subset of the data.
#' @param despike if \code{TRUE}, \code{\link{despike}} will be used to clean
#' anomalous spikes in heading, etc.
#' @template adpTemplate
#'
#' @examples
#' \dontrun{
#' f <- "/Users/kelley/Dropbox/oce_ad2cp/labtestsig3.ad2cp"
#' d <- read.ad2cp(f, to=100) # or read.oce()
#'}
#'
#' @author Dan Kelley
#'
#' @references
#' 1. Nortek, 2017. Signature Integration (55|250|500|1000kHz),
#' available as a file named "\code{N3015-007 Integrators Guide AD2CP.pdf}".
#'
#' @family things related to \code{adp} data
read.ad2cp <- function(file, from=1, to, by=1, tz=getOption("oceTz"),
                       longitude=NA, latitude=NA,
                       orientation, distance, plan,
                       monitor=FALSE, despike=FALSE, processingLog,
                       debug=getOption("oceDebug"), ...)
{
    oceDebug(debug, "read.ad2cp(...,from=", format(from), ",to=", if (missing(to)) "(missing)" else format(to), ", by=", by, ", plan=", if(missing(plan)) "(missing)" else plan, ",...)\n", sep="", unindent=1)
    if (missing(to))
        stop("Must supply 'to'. (This is a temporary constraint, whilst read.ad2cp() is being developed.)")
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
    oceDebug(debug, "ID: 0x", ID, " (NB: 0x15=burst data record; 0x16=avg data record; 0x17=bottom track record; 0x18=interleaved data record; 0xa0=string data record, e.g. GPS NMEA, comment from the FWRITE command)\n", sep="")
    dataSize <- readBin(buf[5:6], what="integer", n=1, size=2, endian="little", signed=FALSE)
    oceDebug(debug, "dataSize:", dataSize, "\n")
    oceDebug(debug, "buf[1+headerSize+dataSize=", 1+headerSize+dataSize, "]=0x", buf[1+headerSize+dataSize], " (expect 0xa5)\n", sep="")
    nav <- do_ldc_ad2cp_in_file(filename, from, to, by)
    d <- list(buf=buf, index=nav$index, length=nav$length, id=nav$id)
    res <- new("adp")
    if (0x10 == d$buf[d$index[1]+1]) { # 0x10 = AD2CP (p38 integrators guide)
        res <- oceSetMetadata(res, "instrumentType", "AD2CP")
    } else {
        stop("this file is not in AD2CP format, since the first byte is not 0x10")
    }
    oceDebug(debug, "focussing on ", length(d$index), " data records\n")
    if (by != 1)
        stop("must have by=1 for this preliminary version of read.ad2cp() (FIXME: check whether this is still required)")
    N <- 1 + as.integer((to - from) / by)
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
    blankingDistance <- vector("numeric", N)
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
    activeConfiguration <- vector("integer", N)
    ensemble <- vector("numeric", N)
    beam2xyzAverage <- NULL
    beam2xyzBurst  <- NULL
    ## Serial number. (It may also be in text records, but it is hard
    ## to imagine data without non-text records, and I know for sure this is
    ## in both average and burst records.
    ## FIXME: see if next will always work for serial number.
    firstData <- which(d$id != 160)[1]
    serialNumber <- readBin(d$buf[d$index[firstData]+5:8], "integer", size=4, endian="little")

    ## Create pointers for accessing 1-byte, 2-byte, and 4-byte chunks
    pointer1 <- d$index
    pointer2 <- as.vector(t(cbind(pointer1, 1 + pointer1))) # rbind() would be fine, too.
    pointer4 <- as.vector(t(cbind(pointer1, 1 + pointer1, 2 + pointer1, 3 + pointer1)))
    oceDebug(debug, "focussing on ", length(pointer1), " data records\n")

    ## {{{
    ## Handle multiple plans (FIXME: this is limited to a single plan, at present)
    status <- readBin(d$buf[pointer4 + 69], "integer", size=4, n=N, endian="little")
    statusBits <- intToBits(status)
    ## Construct an array to store the bits within th 'status' vector. The nortek
    ## docs refer to the first bit as 0, which becomes [1,] in this array.
    dim(statusBits) <- c(32, N)
    ## Nortek docs say bit 17 indicates the active configuration
    activeConfiguration <- as.integer(statusBits[18, ])
    ## If the 'plan' argument is missing, we select the most common one in the data subset.
    if (missing(plan)) {
        u <- unique(activeConfiguration)
        nu <- length(u)
        if (nu == 1) {
            plan <- activeConfiguration[1]
        } else {
            ## tabulate() might be handy here, but the following may be simpler to read.
            plan <- u[which.max(unlist(lapply(u,function(x)sum(activeConfiguration==x))))]
        }
        warning("since 'plan' was not given, using the most common value, namely ", plan, "\n")
    }
    keep <- activeConfiguration == plan
    if (sum(keep) < length(keep)) {
        message("Note: this plan has ", sum(keep), " data records, out of a total of ", length(keep), " in the file")
        d$index <- d$index[keep]
        d$length <- d$length[keep]
        d$id <- d$id[keep]
        statusBits <- statusBits[, keep]
        activeConfiguration <- activeConfiguration[keep]
        N <- sum(keep)
        pointer1 <- d$index
        pointer2 <- as.vector(t(cbind(pointer1, 1 + pointer1))) # rbind() would be fine, too.
        pointer4 <- as.vector(t(cbind(pointer1, 1 + pointer1, 2 + pointer1, 3 + pointer1)))
        oceDebug(debug, "focussing on ", length(pointer1), " data records (after subsetting for plan=", plan, ")\n")
    }
    if (sum(keep) == 0) {
        warning("there are no data for this plan. Below is a table() of the plans in this subset of the file:\n")
        print(table(activeConfiguration))
        return(res)
    }
    if (debug > 0) {
        oceDebug(debug, "below is table() of the 'plan' values in this subset of the file:\n")
        print(table(activeConfiguration))
    }
    ## }}}


    ## "Version" in nortek docs [1 page 48]. FIXME: is this present in other data types?
    version <- as.integer(d$buf[pointer1 + 1])

    ## 1. get some things in fast vector-based form.

    ## 'Configuration' in nortek docs [1 page 48, table 6.1.2]. Put bits in a
    ## logical matrix with rows corresponding to data records. In the docs,
    ## bits are counted from 0, so e.g. 'bit 0' indicates whether pressure is
    ## valid, in the docs.
    configuration <- rawToBits(d$buf[pointer2 + 3]) == 0x01
    dim(configuration) <- c(16, N)
    configuration <- t(configuration)
    ## Extract columns as simply-named flags, for convenience.
    pressureValid <- configuration[, 1]
    temperatureValid <- configuration[, 2]
    compassValid <- configuration[, 3]
    tiltValid <- configuration[, 4]
    ## configuration[, 5] -
    velocityIncluded <- configuration[, 6]
    amplitudeIncluded <- configuration[, 7]
    correlationIncluded <- configuration[, 8]
    altimeterIncluded <- configuration[, 9]
    altimeterRawIncluded <- configuration[,10]
    ASTIncluded <- configuration[,11]
    echoSounderIncluded <- configuration[,12]
    AHRSIncluded <- configuration[,13]
    percentGoodIncluded<- configuration[,14]
    stdDevIncluded <- configuration[,15]
    ## configuration[, 16] Unused
    rm(configuration)                  # prune namespace for clarity
    oceDebug(debug, "velocityIncluded:     ", paste(head(velocityIncluded), collapse=" "), "\n")
    oceDebug(debug, "amplitudeIncluded:    ", paste(head(amplitudeIncluded), collapse=" "), "\n")
    oceDebug(debug, "correlationIncluded:  ", paste(head(correlationIncluded), collapse=" "), "\n")
    oceDebug(debug, "altimeterIncluded:    ", paste(head(altimeterIncluded), collapse=" "), "\n")
    oceDebug(debug, "altimeterRawIncluded: ", paste(head(altimeterRawIncluded), collapse=" "), "\n")
    oceDebug(debug, "ASTIncluded:          ", paste(head(ASTIncluded), collapse=" "), "\n")
    oceDebug(debug, "echoSounderIncluded:  ", paste(head(echoSounderIncluded), collapse=" "), "\n")
    oceDebug(debug, "AHRSIncluded:         ", paste(head(AHRSIncluded), collapse=" "), "\n")
    oceDebug(debug, "percentGoodIncluded:  ", paste(head(percentGoodIncluded), collapse=" "), "\n")
    oceDebug(debug, "stdDevIncluded:       ", paste(head(stdDevIncluded), collapse=" "), "\n")


    ## Now, start decoding actual data.

    ## Decode time. Note that the 100usec part sometimes exceeds 1s, when
    ## multiplied by 1e4.  But we get the same result as nortek-supplied matlab
    ## code in a test file, so I won't worry about this, assuming instead that
    ## this is a quirk of the nortek setup.
    time <- ISOdatetime(year=1900+ as.integer(d$buf[pointer1 + 9]),
                        month=1+as.integer(d$buf[pointer1 + 10]),
                        day=as.integer(d$buf[pointer1 + 11]),
                        hour=as.integer(d$buf[pointer1 + 12]),
                        min=as.integer(d$buf[pointer1 + 13]),
                        sec=as.integer(d$buf[pointer1 + 14]) +
                        1e-4 * readBin(d$buf[pointer2 + 15],
                                       "integer", size=2, n=N, signed=FALSE, endian="little"),
                        tz="UTC")
    soundSpeed <- 0.1 * readBin(d$buf[pointer2 + 17], "integer", size=2, n=N, signed=FALSE, endian="little")
    temperature <- 0.01 * readBin(d$buf[pointer2 + 19], "integer", size=2, n=N, signed=FALSE, endian="little")
    pressure <- 0.001 * readBin(d$buf[pointer2 + 21], "integer", size=2, n=N, signed=FALSE, endian="little")
    heading <- 0.01 * readBin(d$buf[pointer2 + 25], "integer", size=2, n=N, endian="little")
    pitch <- 0.01 * readBin(d$buf[pointer2 + 27], "integer", size=2, n=N, endian="little")
    roll <- 0.01 * readBin(d$buf[pointer2 + 29], "integer", size=2, n=N, endian="little")
    ## BCC uses packed bits to hold info on # beams, coordinate-system, and # cells.
    ## [1 page 49] indicates 2 cases:
    ## case 1: Standard bit 9-0 ncell; bit 11-10 coord (00=enu, 01=xyz, 10=beam, 11=NA); bit 15-12 nbeams
    ## case 2: bit 15-0 number of echo sounder cells
    ## As for 'configuration' above, we set this up as a matrix of 0s and 1s,
    ## with rows corresponding to times, for easy transformation into integers.
    BCC <- ifelse(0x01 == rawToBits(d$buf[pointer2 + 31]), 1, 0)
    dim(BCC) <- c(16, N)
    BCC <- t(BCC)
    ## Use Horner's rule for clarity (for lispers, anyway!)
    ncells <- BCC[,1]+2*(BCC[,2]+2*(BCC[,3]+2*(BCC[,4]+2*(BCC[,5]+2*(BCC[,6]+2*(BCC[,7]+2*(BCC[,8]+2*(BCC[,9]+2*BCC[,10]))))))))
    nbeams <- BCC[,13]+2*(BCC[,14]+2*(BCC[,15]+2*BCC[,16]))
    coordinateSystem <- c("enu", "xyz", "beam", "?")[BCC[,11]+2*BCC[,12]]
    ## cell size is recorded in mm [1, table 6.1.2, page 47]
    cellSize <- 0.001 * readBin(d$buf[pointer2 + 33], "integer", size=2, n=N, signed=FALSE, endian="little")
    ## blanking distance is recorded in cm [1, table 6.1.2, page 47]
    ## NB. blanking may be altered later, if statusBits[2]==0x01
    blankingDistance <- 0.01 * readBin(d$buf[pointer2 + 35], "integer", size=2, n=N, signed=FALSE, endian="little")
    nominalCorrelation <- readBin(d$buf[pointer1 + 37], "integer", size=1, n=N, signed=FALSE, endian="little")
    accelerometerx <- 1.0/16384.0 * readBin(d$buf[pointer2 + 47], "integer", size=2, n=N, signed=TRUE, endian="little")
    accelerometery <- 1.0/16384.0 * readBin(d$buf[pointer2 + 49], "integer", size=2, n=N, signed=TRUE, endian="little")
    accelerometerz <- 1.0/16384.0 * readBin(d$buf[pointer2 + 51], "integer", size=2, n=N, signed=TRUE, endian="little")
    transmitEnergy <- readBin(d$buf[pointer2 + 57], "integer", size=2, n=N, signed=FALSE, endian="little")
    velocityFactor <- 10^readBin(d$buf[pointer1 + 59], "integer", size=1, n=N, signed=TRUE, endian="little")
    powerLevel <- readBin(d$buf[pointer1 + 60], "integer", size=1, n=N, signed=TRUE, endian="little")
    temperatureMagnetometer <- 0.001 * readBin(d$buf[pointer2 + 61], "integer", size=2, n=N, signed=TRUE, endian="little")
    temperatureRTC <- 0.01 * readBin(d$buf[pointer2 + 63], "integer", size=2, n=N, endian="little")
    error <- readBin(d$buf[pointer2 + 65], "integer", size=4, n=N, endian="little") # FIXME: UNUSED

    ## Nortek docs say bit 1 in 'status' indicats blanking scale factor.
    blankingDistance <- blankingDistance * ifelse(statusBits[2, ] == 0x01, 1, 10)

    ensemble <- readBin(d$buf[pointer4+73], "integer", size=4, n=N, endian="little")

    ## Limitations
    nconfiguration <- length(unique(activeConfiguration))
    if (1 < nconfiguration) {
        cat("developer-aimed information:\n")
        print(unique(activeConfiguration))
        print(table(activeConfiguration))
        browser()
        stop("This file has ",
             nconfiguration, " active configurations, but read.ad2cp() can only handle one. Please contact the oce developers if you need to work with this file.")
    }

    ## Record-type codes [1, sec 6.1, page 47]:
    ## 0x15 – Burst Data Record.
    ## 0x16 – Average Data Record.
    ## 0x17 – Bottom Track Data Record.
    ## 0x18 – Interleaved Burst Data Record (beam 5).
    ## 0x1A - Burst Altimeter Raw Record.
    ## 0x1B - DVL Bottom Track Record.
    ## 0x1C - Echo Sounder Record.
    ## 0x1D - DVL Water Track Record.
    ## 0x1E - Altimeter Record.
    ## 0x1F - Avg Altimeter Raw Record.
    ## 0xA0 - String Data Record, eg. GPS NMEA data, comment from the FWRITE command.
    p <- list(burst=which(d$id==0x15),
              average=which(d$id==0x16),
              bottomTrack=which(d$id==0x17),
              interleavedBurst=which(d$id==0x18),
              burstAltimeter=which(d$id==0x1a),
              DVLBottomTrack=which(d$id==0x1b),
              echosounder=which(d$id==0x1c),
              waterTrack=which(d$id==0x1d),
              altimeter=which(d$id==0x1e),
              averageAltimeter=which(d$id==0x1f),
              text=which(d$id==0xa0))
    res@metadata$recordCount <- lapply(p, length)
    ## Inform the user of things we don't attempt to read, and invite them to ask for new capabilities.
    for (n in c("bottomTrack", "interleavedBurst", "burstAltimeter",
                "DVLBottomTrack", "echosounder", "waterTrack", "altimeter",
                "averageAltimeter")) {
        if (res@metadata$recordCount[[n]] > 0) {
            warning("skipped ", res@metadata$recordCount[[n]], " '", n, "' data records; only 'average', 'burst' and 'text' are handled in this version of oce\n", sep="")
        }
    }
    ## 2. get some things in slow index-based form.
    if (length(p$burst) > 0) {
        if (any(version[p$burst] != 3))
            stop("can only decode 'burst' data records that are in 'version 3' format")
        nbeamsBurst <- nbeams[p$burst[1]]
        ncellsBurst <- ncells[p$burst[1]]
        oceDebug(debug, "burst data records: nbeams:", nbeamsBurst, ", ncells:", ncellsBurst, "\n")
        if (any(ncells[p$burst] != ncellsBurst))
            stop("the 'burst' data records do not all have the same number of cells")
        if (any(nbeams[p$burst] != nbeamsBurst))
            stop("the 'burst' data records do not all have the same number of beams")
        ## FIXME: read other fields to the following list.
        burst <- list(i=1,
                      numberOfCells=ncellsBurst,
                      numberOfBeams=nbeamsBurst,
                      originalCoordinate=coordinateSystem[p$burst[1]],
                      oceCoordinate=coordinateSystem[p$burst[1]],
                      cellSize=cellSize[p$burst[1]],
                      blankingDistance=blankingDistance[p$burst[1]],
                      ensemble=ensemble[p$burst],
                      time=time[p$burst],
                      heading=heading[p$burst],
                      pitch=pitch[p$burst],
                      roll=roll[p$burst],
                      pressure=pressure[p$burst],
                      temperature=temperature[p$burst],
                      temperatureMagnetometer=temperatureMagnetometer[p$burst],
                      temperatureRTC=temperatureRTC[p$burst],
                      soundSpeed=soundSpeed[p$burst],
                      accelerometerx=accelerometerx[p$burst],
                      accelerometery=accelerometery[p$burst],
                      accelerometerz=accelerometerz[p$burst],
                      nominalCorrelation=nominalCorrelation[p$burst],
                      v=array(double(), dim=c(length(p$burst), ncellsBurst, nbeamsBurst)),
                      a=array(raw(), dim=c(length(p$burst), ncellsBurst, nbeamsBurst)),
                      q=array(raw(), dim=c(length(p$burst), ncellsBurst, nbeamsBurst)))
    } else {
        burst <- NULL
    }
    if (length(p$average) > 0) {
        if (any(version[p$average] != 3))
            stop("can only decode 'average' data records that are in 'version 3' format")
        nbeamsAverage <- nbeams[p$average[1]]
        ncellsAverage <- ncells[p$average[1]]
        oceDebug(debug, "average data records: nbeams:", nbeamsAverage, ", ncells:", ncellsAverage, "\n")
        if (any(ncells[p$average] != ncellsAverage))
            stop("the 'average' data records do not all have the same number of cells")
        if (any(nbeams[p$average] != nbeamsAverage))
            stop("the 'average' data records do not all have the same number of beams")
        ## FIXME: read other fields to the following list.
        average <- list(i=1,
                        numberOfCells=ncellsAverage,
                        numberOfBeams=nbeamsAverage,
                        originalCoordinate=coordinateSystem[p$average[1]],
                        oceCoordinate=coordinateSystem[p$average[1]],
                        cellSize=cellSize[p$average[1]],
                        blankingDistance=blankingDistance[p$average[1]],
                        ensemble=ensemble[p$average],
                        time=time[p$average],
                        heading=heading[p$average],
                        pitch=pitch[p$average],
                        roll=roll[p$average],
                        pressure=pressure[p$average],
                        temperature=temperature[p$average],
                        temperatureMagnetometer=temperatureMagnetometer[p$average],
                        temperatureRTC=temperatureRTC[p$average],
                        soundSpeed=soundSpeed[p$average],
                        accelerometerx=accelerometerx[p$average],
                        accelerometery=accelerometery[p$average],
                        accelerometerz=accelerometerz[p$average],
                        nominalCorrelation=nominalCorrelation[p$average],
                        v=array(double(), dim=c(length(p$average), ncellsAverage, nbeamsAverage)),
                        a=array(raw(), dim=c(length(p$average), ncellsAverage, nbeamsAverage)),
                        q=array(raw(), dim=c(length(p$average), ncellsAverage, nbeamsAverage)))
    } else {
        average <- NULL
    }
    if (length(p$text) > 0) {
        text <- list(i=1, text=list()) # vector("character", length(p$text)))
    } else {
        text <- list()
    }
    ## Fill up th arrays in a loop. This could also be vectorized, if it proves slow.
    id <- d$id
    for (ch in 1:N) {
        oceDebug(debug>2, "  d$id[", ch, "]=", d$id[[ch]], "\n", sep="")
        if (d$id[ch] == 0xa0) {        # text (0xa0 = 160)
            chars <- rawToChar(d$buf[seq.int(2+d$index[ch], by=1, length.out=-1+d$length[ch])])
            text$text[[text$i]] <- strsplit(chars, "\r\n")[[1]]
            ##text$text[text$i] <- chars
            text$i <- text$i + 1
            ##oceDebug(debug, "added to text; now, text$i=", text$i, "\n")
        } else if (d$id[ch] == 0x15) { # burst (0x15 = 21)
            i <- d$index[ch]
            ncol <- burst$numberOfBeams
            nrow <- burst$numberOfCells
            n <- ncol * nrow
            n2 <- 2 * n
            i0 <- 77
            if (velocityIncluded[ch]) {
                v <- velocityFactor[ch]*readBin(d$buf[i+i0+seq(0,n2-1)],"integer",size=2,n=n,endian="little")
                burst$v[burst$i, , ] <- matrix(v, ncol=ncol, nrow=nrow, byrow=FALSE)
                i0 <- i0 + n2
            } else {
                stop("at ch=", ch, " how can velocityIncluded be false?")
            }
            if (amplitudeIncluded[ch]) {
                a <- d$buf[i + i0 + seq(0, n-1)]
                burst$a[burst$i, ,] <- matrix(a, ncol=ncol, nrow=nrow, byrow=FALSE)
                i0 <- i0 + n
            } else {
                stop("at ch=", ch, " how can amplitudeIncluded be false?")
            }
            if (correlationIncluded[ch]) {
                q <- d$buf[i + i0 + seq(0, n-1)]
                burst$q[burst$i, ,] <- matrix(q, ncol=ncol, nrow=nrow, byrow=FALSE)
                i0 <- i0 + n
            } else {
                stop("at ch=", ch, " how can correlationIncluded be false?")
            }
            burst$i <- burst$i + 1
            ## FIXME: read other fields
        } else if (d$id[ch] == 0x16) { # average (0x16 = 22)
            i <- d$index[ch]
            ncol <- average$numberOfBeams
            nrow <- average$numberOfCells
            n <- ncol * nrow
            n2 <- 2 * n
            i0 <- 77
            if (velocityIncluded[ch]) {
                v <- velocityFactor[ch]*readBin(d$buf[i+i0+seq(0,n2-1)],"integer",size=2,n=n,endian="little")
                average$v[average$i, , ] <- matrix(v, ncol=ncol, nrow=nrow, byrow=FALSE)
                i0 <- i0 + n2
            } else {
                stop("at ch=", ch, " how can velocityIncluded be false?")
            }
            if (amplitudeIncluded[ch]) {
                a <- d$buf[i + i0 + seq(0, n-1)]
                average$a[average$i, ,] <- matrix(a, ncol=ncol, nrow=nrow, byrow=FALSE)
                i0 <- i0 + n
            } else {
                stop("at ch=", ch, " how can amplitudeIncluded be false?")
            }
            if (correlationIncluded[ch]) {
                q <- d$buf[i + i0 + seq(0, n-1)]
                average$q[average$i, ,] <- matrix(q, ncol=ncol, nrow=nrow, byrow=FALSE)
                i0 <- i0 + n
            } else {
                stop("at ch=", ch, " how can correlationIncluded be false?")
            }
            average$i <- average$i + 1
        } else if (d$id[ch] == 0x18) { # interleaved burst (0x18 = 24 )
            if (debug > 0)
                message("ignoring interleaved burst at ch=", ch)
        } else {
            ## FIXME: read other fields
        }
    }
    ## Clean lists of temporary indices.
    if (length(text))
        text$i <- NULL
    if (length(burst))
        burst$i <- NULL
    if (length(average))
        average$i <- NULL
    data <- list(powerLevel=powerLevel,
                 status=status,
                 activeConfiguration=activeConfiguration,
                 text=text)
    if (!is.null(average))
        data$average <- average
    if (!is.null(burst))
        data$burst <- burst
    res@metadata$serialNumber <- serialNumber
    ##? res@metadata$header <- text
    res@metadata$id <- id
    ## Remove the overall coordiante (created by initializer) since it has no meaning here.
    res@metadata$oceCoordinate <- NULL

    res@data <- data
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
