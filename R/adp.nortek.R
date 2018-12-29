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

## private function
ad2cpDefaultDataItem <- function(x, j=NULL, order=c("average", "burst", "interleavedBurst", "bottomTrack"))
{
    if (!is.ad2cp(x))
        stop("x is not an AD2CP object")
    dataNames <- names(x@data)
    if (is.null(j) || nchar(j) == 0) {
        i <- which(order %in% dataNames)
        if (length(i)) order[i[1]] else stop("ad2cp object does not contain any of '", paste(order, collapse="', '"), "'")
    } else {
        if (j %in% dataNames) j else stop("ad2cp object does not contain data item '", j, "'")
    }
}


#' Decode an item from a Nortek AD2CP file header
#'
#' @param x Adp object of the ad2cp variety, i.e. an object created
#' by \code{\link{read.adp.ad2cp}}.
#'
#' @param key Character value that identifies a particular line in \code{x[["text"]]}.
#'
#' @param item Character value indicating the name of the item sought.
#'
#' @param numeric Logical value indicating whether to convert the return value
#' from a string to a numerical value.
#'
#' @param default Optional value to be used if the item is not found in the
#' header, or if the header is \code{NULL} (as in the case of a split-up file
#' that lacks the initial header information)
#'
#' @return String or number interpreted from the \code{x[["text"]]}, or \code{NULL},
#' if the desired item is not found there, or if \code{x} is not of the required
#' class and variety.
#'
#' @examples
#'\dontrun{
#' d <- read.oce("a.ad2cp")
#' # The examples start with the line in x[["text"]][[1]]; note that in the second
#' # example, it would be insuficient to use a key of "BEAMCFGLIST", because that will
#' # yield 4 lines, and the function is not designed to handle that.
#'
#' # ID,STR=\"Signature1000\",SN=123456
#' type <- ad2cpHeaderValue(d, "ID", "STR", numeric=FALSE)
#' serialNumber <- ad2cpHeaderValue(d, "ID", "SN")
#'
#' # BEAMCFGLIST,BEAM=1,THETA=25.00,PHI=0.00,FREQ=1000,BW=25,BRD=1,HWBEAM=1,ZNOM=60.00
#' beam1Angle <- ad2cpHeaderValue(d, "BEAMCFGLIST,BEAM=1", "THETA")
#' frequency <- ad2cpHeaderValue(d, "BEAMCFGLIST,BEAM=1", "FREQ", default=NA)
#'}
#'
#' @family things related to \code{adp} data
ad2cpHeaderValue <- function(x, key, item, numeric=TRUE, default)
{
    if (missing(x))
        stop("must provide x")
    if (is.character(x)) {
        header <- x
    } else if (is.ad2cp(x)) {
        header <- x[["header"]]
    } else {
        stop("x must be either a character value or an AD2CP object")
    }
    if (missing(key))
        stop("must provide key")
    if (missing(item))
        stop("must provide item")
    if (is.null(header))
        return(if (missing(default)) NULL else default)
    key2 <- paste("^", key, ",", sep="")
    ##message("key2='", key2, "'")
    hline <- header[grep(key2, header)]
    ##message("hline='",hline,"'")
    if (length(hline) > 1)
        stop("header line is not distinct; try using a comma at the end of key")
    if (0 == length(hline))
        return(if (missing(default)) NULL else default)
    if (0 == length(grep(item, hline))) {
        return(if (missing(default)) NULL else default)
    }
    res <- gsub(paste("^.*", item, "=([^,]*).*$", sep=""), "\\1", hline)
    if (nchar(res)) {
        res <- if (numeric) as.numeric(res) else gsub('"', '', res)
    } else {
        res <- if (missing(default)) NULL else default
    }
    res
}

#' Test whether object is an AD2CP type
#'
#' @param x An item
#'
#' @return Logical value indicating whether the object inherits from the
#' \code{\link{adp-class}} and has \code{fileType} in its
#' \code{metadata} slot equal to \code{"AD2CP"}.
#'
#' @family things related to \code{adp} data
is.ad2cp <- function(x)
{
    if (!inherits(x, "adp")) {
        FALSE
    } else {
        fileType <- x[["fileType"]]
        !is.null(fileType) && fileType == "AD2CP"
    }
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
#'
#' @param distance Optional vector holding the distances of bin centres from the
#' sensor.  This argument is ignored except for Nortek profilers, and need not
#' be given if the function determines the distances correctly from the data.
#' The problem is that the distance is poorly documented in the Nortek System
#' Integrator Guide (2008 edition, page 31), so the function must rely on
#' word-of-mouth formulae that do not work in all cases.
#'
#' @param plan Optional integer specifying which 'plan' to focus on (see [1]
#' for the meaning of 'plan').  If this is not given, it defaults to the most
#' common plan in the requested subset of the data.
#'
#' @param type Optional character value indicating the type of Nortek instrument.
#' If this is not provided, an attempt is made to infer it
#' from the file header (if there is one), and \code{"Signature1000"}
#' is used, otherwise. The importance
#' of knowing the type is for inferring the beam angle, which is usd in the
#' conversion from beam coordinates to xyz or enu coordinates. If \code{type} is
#' provided, it must be one of \code{"Signature250"}, \code{"Signature500"},
#' or \code{"Signature1000"}. The slant-beam angle is 20 degrees for Signature250,
#' and 25 degrees for the other types (see [2], section 2 on page 6).
#'
#' @param despike Ignored by this function, and provided only for similarity
#' to other adp-reading functions.
#'
#' @template adpTemplate
#'
#' @section Beam-to-xyz conversion:
#' The device orientation is not inferred correctly from the file, and
#' so a default is assumed -- that the orientation is upwards.
#'
#' @examples
#' \dontrun{
#' d <- read.adp.ad2cp("~/test.ad2cp", to=100) # or read.oce()
#'}
#'
#' @author Dan Kelley
#'
#' @references
#' 1. Nortek AS. “Signature Integration 55|250|500|1000kHz.” Nortek AS, 2017.
#'
#' 2. Nortek AS. “Operations Manual - Signature250, 500 and 1000.” Nortek AS, September 21, 2018.
#'
#' @family things related to \code{adp} data
read.adp.ad2cp <- function(file, from=1, to, by=1, tz=getOption("oceTz"),
                           longitude=NA, latitude=NA,
                           orientation, distance, plan, type,
                           monitor=FALSE, despike=FALSE, processingLog,
                           debug=getOption("oceDebug"), ...)
{
    planGiven <- !missing(plan)
    typeGiven <- !missing(type)
    oceDebug(debug, "read.adp.ad2cp(...,from=", format(from),
             ",to=", if (missing(to)) "(missing)" else format(to),
             ", by=", by,
             ", plan=", if (planGiven) plan else "(missing)",
             ", type=\"", if (typeGiven) type else "(missing)",
             "\",...)\n", sep="", unindent=1)
    if (typeGiven) {
        typeAllowed <- c("Signature1000", "Signature500", "Signature250")
        typei <- pmatch(type, typeAllowed)
        if (is.na(typei))
            stop("type must be \"Signature1000\", \"Signature500\", or \"Signature250\", but it is \"", type, "\"")
        type <- typeAllowed[typei]
    }
    if (by != 1)
        stop("must have by=1, since skipping makes no sense in complex ad2cp files")
    if (by < 1)
        stop("cannot have by < 1")
    if (missing(to))
        to <- 1e9                      # this should be enough to read any file
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
    if (0x10 != d$buf[d$index[1]+1]) # 0x10 = AD2CP (p38 integrators guide)
        stop("this file is not in AD2CP format, since the first byte is not 0x10")
    oceDebug(debug, "focussing on ", length(d$index), " data records\n")
    Nmax <- length(d$index)
    if (to > Nmax) {
        warning("using to=", Nmax, " based on file contents")
        to <- Nmax
    }
    focusIndex <- seq(from, to, by=by)
    N <- length(focusIndex)
    if (N <= 0)
        stop("must have to > from")

    ## Set up object, with key metadata to allow other functions to work.
    res <- new("adp")
    ## Set up data storage
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
    status <- intToBits(readBin(d$buf[pointer4 + 69], "integer", size=4, n=N, endian="little"))
    ## Construct an array to store the bits within th 'status' vector. The nortek
    ## docs refer to the first bit as 0, which becomes [1,] in this array.
    dim(status) <- c(32, N)
    ## Nortek docs say bit 16 indicates the active configuration, but they
    ## count from 0, so it is bit 17 here.
    activeConfiguration <- as.integer(status[17, ])
    ## Nortek docs say bits 25-27 (0-offset notation) store orientation, so 26-28 here.
    ## Table 1 tells how to interpret the bits:
    ## 0=xup 1=xdown 4=zup 5=zdown
    ## but I am not getting such values in my test files. Maybe . Am I looking
    ## at
    orientation1 <- as.integer(status[26, ])
    orientation2 <- as.integer(status[27, ])
    orientation3 <- as.integer(status[28, ])
    O1 <- orientation1 + 2*orientation2 + 4*orientation3
    O2 <- 4*orientation1 + 2*orientation2 + orientation3
    if (debug > 0) message(vectorShow(O1))
    if (debug > 0) message(vectorShow(O2))
    ## DEBUG message("table(activeConfiguration):")
    ## DEBUG print(table(activeConfiguration))
    ## DEBUG browser()
    ## If the 'plan' argument is missing, we select the most common one in the data subset.
    if (!planGiven) {
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
    ## Try to find a header, as the first record-type that has id=0xa0.
    header <- NULL
    idHeader <- which(d$id == 0xa0)[1]
    if (length(idHeader)) {
        oceDebug(debug, "this file has a header at id=", idHeader, "\n")
        chars <- rawToChar(d$buf[seq.int(2+d$index[idHeader], by=1, length.out=-1+d$length[idHeader])])
        header <- strsplit(chars, "\r\n")[[1]]
        if (!typeGiven) {
            type <- gsub('.*STR="([^"]*)".*$', '\\1', header[grep("^ID,", header)])
            typeGiven <- TRUE
        }
    }
    keep <- activeConfiguration == plan
    if (sum(keep) == 0) {
        warning("there are no data for plan=", plan, "; try one of the following values instead: ", paste(unique(activeConfiguration), collapse=" "))
        return(res)
    }
     if (sum(keep) < length(keep)) {
        oceDebug(debug, "this plan has ", sum(keep), " data records, out of a total of ", length(keep), " in the file subset\n")
        d$index <- d$index[keep]
        d$length <- d$length[keep]
        d$id <- d$id[keep]
        status <- status[, keep, drop=FALSE]
        activeConfiguration <- activeConfiguration[keep]
        N <- sum(keep)
        pointer1 <- d$index
        pointer2 <- as.vector(t(cbind(pointer1, 1 + pointer1))) # rbind() would be fine, too.
        pointer4 <- as.vector(t(cbind(pointer1, 1 + pointer1, 2 + pointer1, 3 + pointer1)))
        oceDebug(debug, "focussing on ", length(pointer1), " data records (after subsetting for plan=", plan, ")\n")
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
    ## Extract columns as simply-named flags, for convenience. The variable
    ## names to which the assignments are made apply to average/burst data,
    ## while comments are used to indicate values for other data, e.g.
    ## bottomTrack.
    pressureValid <- configuration[, 1]
    temperatureValid <- configuration[, 2]
    compassValid <- configuration[, 3]
    tiltValid <- configuration[, 4]
    ## configuration[, 5] -
    velocityIncluded <- configuration[, 6]
    amplitudeIncluded <- configuration[, 7] # bottomTrack:-
    correlationIncluded <- configuration[, 8] # bottomTrack:-
    altimeterIncluded <- configuration[, 9] # bottomTrack:distanceIncluded
    altimeterRawIncluded <- configuration[,10] # bottomTrack:figureOfMeritIncluded
    ASTIncluded <- configuration[,11] # bottomTrack:-
    echosounderIncluded <- configuration[,12] # bottomTrack:-
    AHRSIncluded <- configuration[,13] # bottomTrack:-
    percentGoodIncluded<- configuration[,14] # bottomTrack:-
    stdDevIncluded <- configuration[,15] # bottomTrack:-
    ## configuration[, 16] Unused
    oceDebug(debug, vectorShow(velocityIncluded))
    oceDebug(debug, vectorShow(amplitudeIncluded))
    oceDebug(debug, vectorShow(correlationIncluded))
    oceDebug(debug, vectorShow(altimeterIncluded))
    oceDebug(debug, vectorShow(altimeterRawIncluded))
    oceDebug(debug, vectorShow(ASTIncluded))
    oceDebug(debug, vectorShow(echosounderIncluded))
    oceDebug(debug, vectorShow(AHRSIncluded))
    oceDebug(debug, vectorShow(percentGoodIncluded))
    oceDebug(debug, vectorShow(stdDevIncluded))


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
    ## b00=enu, b01=xyz, b10=beam, b11=- [1 page 49]
    coordinateSystem <- c("enu", "xyz", "beam", "?")[1 + BCC[,11] + 2*BCC[,12]]
    ## cell size is recorded in mm [1, table 6.1.2, page 49]
    cellSize <- 0.001 * readBin(d$buf[pointer2 + 33], "integer", size=2, n=N, signed=FALSE, endian="little")
    ## blanking distance is recorded in cm [1, table 6.1.2, page 49]
    ## NB. blanking may be altered later, if status[2]==0x01
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

    ## status0, byte 67:68, skipped
    ## status,  byte 69:71, already read above so we could infer activeConfiguration

    ## Nortek docs [2 p51] say bit 1 (in 0-offset notation) in 'status' indicates blankingDistance
    ## unit, either 0 for m or 1 for cm. (Above, it was read and converted to m, assuming cm.)
    if (debug > 0) {
        cat(vectorShow(status[2,]))
        cat(vectorShow(blankingDistance))
    }
    blankingDistance <- blankingDistance * ifelse(status[2, ] == 0x01, 1, 0.1)
    if (debug > 0)
        cat(vectorShow(blankingDistance))

    ensemble <- readBin(d$buf[pointer4+73], "integer", size=4, n=N, endian="little")

    ## Limitations
    nconfiguration <- length(unique(activeConfiguration))
    if (1 < nconfiguration) {
        cat("developer-aimed information:\n")
        print(unique(activeConfiguration))
        print(table(activeConfiguration))
        stop("This file has ",
             nconfiguration, " active configurations, but read.adp.ad2cp() can only handle one. Please contact the oce developers if you need to work with this file.")
    }

    ## Record-type codes [1, sec 6.1, page 47], listed with -- if coded:
    ## 0x15 -- Burst Data Record.
    ## 0x16 -- Average Data Record.
    ## 0x17 -  Bottom Track Data Record.
    ## 0x18 -- Interleaved Burst Data Record (beam 5).
    ## 0x1A -  Burst Altimeter Raw Record.
    ## 0x1B -  DVL Bottom Track Record.
    ## 0x1C -- Echo Sounder Record.
    ## 0x1D -  DVL Water Track Record.
    ## 0x1E -  Altimeter Record.
    ## 0x1F -  Avg Altimeter Raw Record.
    ## 0xA0 -- String Data Record, eg. GPS NMEA data, comment from the FWRITE command.
    p <- list(burst=which(d$id==0x15), # coded
              average=which(d$id==0x16), # coded
              bottomTrack=which(d$id==0x17), # coded, but no sample-data test and no plot()
              interleavedBurst=which(d$id==0x18), # coded
              burstAltimeter=which(d$id==0x1a),
              DVLBottomTrack=which(d$id==0x1b),
              echosounder=which(d$id==0x1c), # coded, but no sample-data test and no plot()
              waterTrack=which(d$id==0x1d),
              altimeter=which(d$id==0x1e),
              averageAltimeter=which(d$id==0x1f),
              text=which(d$id==0xa0)) # coded
    recordCount <- lapply(p, length)

    ## Inform the user of things we don't attempt to read
    for (n in c("burstAltimeter", "DVLBottomTrack", "waterTrack", "altimeter", "averageAltimeter")) {
        if (recordCount[[n]] > 0) {
            warning("skipped ", recordCount[[n]], " '", n, "' data records; only 'average', 'burst', 'interleavedBurst', 'echosounder' and 'text' are handled\n", sep="")
        }
    }
    ## 2. get some things in slow index-based form. (Items are alphabetized.)
    ## FIXME: _AD2CPrecordtype_ update [["recordTypes"]] etc if new record types are handled here
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
                        transmitEnergy=transmitEnergy[p$average],
                        powerLevel=powerLevel[p$average])
        if (any(velocityIncluded[p$average])) {
            if (1 < length(unique(velocityIncluded[p$average])))
                stop("velocityIncluded values non-unique across 'average' data records")
            average$v <- array(double(), dim=c(length(p$average), ncellsAverage, nbeamsAverage))
        }
        if (any(amplitudeIncluded[p$average])) {
            if (1 < length(unique(amplitudeIncluded[p$average])))
                stop("amplitudeIncluded values non-unique across 'average' data records")
            average$a <- array(raw(), dim=c(length(p$average), ncellsAverage, nbeamsAverage))
        }
        if (any(correlationIncluded[p$average])) {
            if (1 < length(unique(correlationIncluded[p$average])))
                stop("correlationIncluded values non-unique across 'average' data records")
            average$q <- array(raw(), dim=c(length(p$average), ncellsAverage, nbeamsAverage))
        }
        if (any(altimeterIncluded[p$average])) {
            if (1 < length(unique(altimeterIncluded[p$average])))
                stop("altimeterIncluded values non-unique across 'average' data records")
            average$altimeterDistance <- vector("numeric", length(p$average))
        }
        if (any(ASTIncluded[p$average])) {
            average$ASTDistance <- vector("numeric", length(p$average))
            average$ASTPressure <- vector("numeric", length(p$average))
        }
        if (any(echosounderIncluded[p$average])) {
            average$echosounder <- matrix(double(), ncol=length(p$average), nrow=ncellsAverage)
        }
        if (any(AHRSIncluded[p$average])) {
            average$AHRS <- matrix(numeric(), nrow=length(p$average), ncol=9)
        }
    } else {
        average <- NULL
    }
    if (length(p$bottomTrack) > 0) {
        if (any(version[p$bottomTrack] != 3))
            stop("can only decode 'bottomTrack' data records that are in 'version 3' format")
        nbeamsBottomTrack <- nbeams[p$bottomTrack[1]]
        ncellsBottomTrack <- ncells[p$bottomTrack[1]]
        oceDebug(debug, "bottomTrack data records: nbeams:", nbeamsBottomTrack, ", ncells:", ncellsBottomTrack, "\n")
        if (any(ncells[p$bottomTrack] != ncellsBottomTrack))
            stop("the 'bottomTrack' data records do not all have the same number of cells")
        if (any(nbeams[p$bottomTrack] != nbeamsBottomTrack))
            stop("the 'bottomTrack' data records do not all have the same number of beams")
        ## FIXME: read other fields to the following list.
        bottomTrack <- list(i=1,
                            numberOfCells=ncellsBottomTrack,
                            numberOfBeams=nbeamsBottomTrack,
                            originalCoordinate=coordinateSystem[p$bottomTrack[1]],
                            oceCoordinate=coordinateSystem[p$bottomTrack[1]],
                            cellSize=cellSize[p$bottomTrack[1]],
                            blankingDistance=blankingDistance[p$bottomTrack[1]],
                            ensemble=ensemble[p$bottomTrack],
                            time=time[p$bottomTrack],
                            heading=heading[p$bottomTrack],
                            pitch=pitch[p$bottomTrack],
                            roll=roll[p$bottomTrack],
                            pressure=pressure[p$bottomTrack],
                            temperature=temperature[p$bottomTrack],
                            temperatureMagnetometer=temperatureMagnetometer[p$bottomTrack],
                            temperatureRTC=temperatureRTC[p$bottomTrack],
                            soundSpeed=soundSpeed[p$bottomTrack],
                            accelerometerx=accelerometerx[p$bottomTrack],
                            accelerometery=accelerometery[p$bottomTrack],
                            accelerometerz=accelerometerz[p$bottomTrack],
                            nominalCorrelation=nominalCorrelation[p$bottomTrack],
                            transmitEnergy=transmitEnergy[p$bottomTrack],
                            powerLevel=powerLevel[p$bottomTrack])
        if (any(velocityIncluded[p$bottomTrack])) {
            if (1 < length(unique(velocityIncluded[p$bottomTrack])))
                stop("velocityIncluded values non-unique across 'bottomTrack' data records")
            bottomTrack$v <- array(double(), dim=c(length(p$bottomTrack), nbeamsBottomTrack))
        }
        if (any(altimeterIncluded[p$bottomTrack])) { # note name-shift from average/burst data
            if (1 < length(unique(altimeterIncluded[p$bottomTrack])))
                stop("altimeterIncluded values non-unique across 'bottomTrack' data records")
            bottomTrack$altimeterDistance <- array(double(), dim=c(length(p$bottomTrack), nbeamsBottomTrack))
        }
        if (any(altimeterRawIncluded[p$bottomTrack])) { # note name-shift from average/burst data
            bottomTrack$altimeterFigureOfMerit <- array(double(), dim=c(length(p$bottomTrack), nbeamsBottomTrack))
        }
    } else {
        bottomTrack <- NULL
    }

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
                      transmitEnergy=transmitEnergy[p$burst],
                      powerLevel=powerLevel[p$burst])
        if (any(velocityIncluded[p$burst])) {
            if (1 < length(unique(velocityIncluded[p$burst])))
                stop("velocityIncluded values non-unique across 'burst' data records")
            burst$v <- array(double(), dim=c(length(p$burst), ncellsBurst, nbeamsBurst))
        }
        if (any(amplitudeIncluded[p$burst])) {
            if (1 < length(unique(amplitudeIncluded[p$burst])))
                stop("amplitudeIncluded values non-unique across 'burst' data records")
            burst$a <- array(raw(), dim=c(length(p$burst), ncellsBurst, nbeamsBurst))
        }
        if (any(correlationIncluded[p$burst])) {
            if (1 < length(unique(correlationIncluded[p$burst])))
                stop("correlationIncluded values non-unique across 'burst' data records")
            burst$q <- array(raw(), dim=c(length(p$burst), ncellsBurst, nbeamsBurst))
        }
        if (any(altimeterIncluded[p$burst])) {
            if (1 < length(unique(altimeterIncluded[p$burst])))
                stop("altimeterIncluded values non-unique across 'burst' data records")
            burst$altimeterDistance <- vector("numeric", length(p$burst))
        }
        if (any(ASTIncluded[p$burst])) {
            burst$ASTDistance <- vector("numeric", length(p$burst))
            burst$ASTPressure <- vector("numeric", length(p$burst))
        }
        if (any(echosounderIncluded[p$burst])) {
            burst$echosounder <- matrix(double(), ncol=length(p$burst), nrow=ncellsBurst)
        }
        if (any(AHRSIncluded[p$burst])) {
            burst$AHRS <- matrix(numeric(), nrow=length(p$burst), ncol=9)
        }
    } else {
        burst <- NULL
    }

    if (length(p$echosounder) > 0) {
        if (any(version[p$echosounder] != 3))
            stop("can only decode 'echosounder' data records that are in 'version 3' format")
        nbeamsEchosounder<- nbeams[p$echosounder[1]]
        ncellsEchosounder <- ncells[p$echosounder[1]]
        oceDebug(debug, "echosounder data records: nbeams:", nbeamsEchosounder, ", ncells:", ncellsEchosounder, "\n")
        if (any(ncells[p$echosounder] != ncellsEchosounder))
            stop("the 'echosounder' data records do not all have the same number of cells")
        if (any(nbeams[p$echosounder] != nbeamsEchosounder))
            stop("the 'echosounder' data records do not all have the same number of beams")
        ## FIXME: read other fields to the following list.
        echosounder <- list(i=1,
                        numberOfCells=ncellsEchosounder,
                        numberOfBeams=nbeamsEchosounder,
                        originalCoordinate=coordinateSystem[p$echosounder[1]],
                        oceCoordinate=coordinateSystem[p$echosounder[1]],
                        cellSize=cellSize[p$echosounder[1]],
                        blankingDistance=blankingDistance[p$echosounder[1]],
                        ensemble=ensemble[p$echosounder],
                        time=time[p$echosounder],
                        heading=heading[p$echosounder],
                        pitch=pitch[p$echosounder],
                        roll=roll[p$echosounder],
                        pressure=pressure[p$echosounder],
                        temperature=temperature[p$echosounder],
                        temperatureMagnetometer=temperatureMagnetometer[p$echosounder],
                        temperatureRTC=temperatureRTC[p$echosounder],
                        soundSpeed=soundSpeed[p$echosounder],
                        accelerometerx=accelerometerx[p$echosounder],
                        accelerometery=accelerometery[p$echosounder],
                        accelerometerz=accelerometerz[p$echosounder],
                        nominalCorrelation=nominalCorrelation[p$echosounder],
                        transmitEnergy=transmitEnergy[p$echosounder],
                        powerLevel=powerLevel[p$echosounder])
        if (any(velocityIncluded[p$echosounder])) {
            if (1 < length(unique(velocityIncluded[p$echosounder])))
                stop("velocityIncluded values non-unique across 'echosounder' data records")
            echosounder$v <- array(double(), dim=c(length(p$echosounder), ncellsEchosounder, nbeamsEchosounder))
        }
        if (any(amplitudeIncluded[p$echosounder])) {
            if (1 < length(unique(amplitudeIncluded[p$echosounder])))
                stop("amplitudeIncluded values non-unique across 'echosounder' data records")
            echosounder$a <- array(raw(), dim=c(length(p$echosounder), ncellsEchosounder, nbeamsEchosounder))
        }
        if (any(correlationIncluded[p$echosounder])) {
            if (1 < length(unique(correlationIncluded[p$echosounder])))
                stop("correlationIncluded values non-unique across 'echosounder' data records")
            echosounder$q <- array(raw(), dim=c(length(p$echosounder), ncellsEchosounder, nbeamsEchosounder))
        }
        if (any(altimeterIncluded[p$echosounder])) {
            if (1 < length(unique(altimeterIncluded[p$echosounder])))
                stop("altimeterIncluded values non-unique across 'echosounder' data records")
            echosounder$altimeterDistance <- vector("numeric", length(p$echosounder))
        }
        if (any(ASTIncluded[p$echosounder])) {
            echosounder$ASTDistance <- vector("numeric", length(p$echosounder))
            echosounder$ASTPressure <- vector("numeric", length(p$echosounder))
        }
        if (any(echosounderIncluded[p$echosounder])) {
            echosounder$echosounder <- matrix(double(), ncol=length(p$echosounder), nrow=ncellsAverage)
        }
        if (any(AHRSIncluded[p$average])) {
            echosounder$AHRS <- matrix(numeric(), nrow=length(p$echosounder), ncol=9)
        }
    } else {
        echosounder <- NULL
    }
    if (length(p$interleavedBurst) > 0) {
        if (any(version[p$interleavedBurst] != 3))
            stop("can only decode 'interleavedBurst' data records that are in 'version 3' format")
        nbeamsInterleavedBurst <- nbeams[p$interleavedBurst[1]]
        ncellsInterleavedBurst <- ncells[p$interleavedBurst[1]]
        oceDebug(debug, "interleavedBurst data records: nbeams:", nbeamsInterleavedBurst, ", ncells:", ncellsInterleavedBurst, "\n")
        if (any(ncells[p$interleavedBurst] != ncellsInterleavedBurst))
            stop("the 'interleavedBurst' data records do not all have the same number of cells")
        if (any(nbeams[p$interleavedBurst] != nbeamsInterleavedBurst))
            stop("the 'interleavedBurst' data records do not all have the same number of beams")
        ## FIXME: read other fields to the following list.
        interleavedBurst <- list(i=1,
                      numberOfCells=ncellsInterleavedBurst,
                      numberOfBeams=nbeamsInterleavedBurst,
                      originalCoordinate=coordinateSystem[p$interleavedBurst[1]],
                      oceCoordinate=coordinateSystem[p$interleavedBurst[1]],
                      cellSize=cellSize[p$interleavedBurst[1]],
                      blankingDistance=blankingDistance[p$interleavedBurst[1]],
                      ensemble=ensemble[p$interleavedBurst],
                      time=time[p$interleavedBurst],
                      heading=heading[p$interleavedBurst],
                      pitch=pitch[p$interleavedBurst],
                      roll=roll[p$interleavedBurst],
                      pressure=pressure[p$interleavedBurst],
                      temperature=temperature[p$interleavedBurst],
                      temperatureMagnetometer=temperatureMagnetometer[p$interleavedBurst],
                      temperatureRTC=temperatureRTC[p$interleavedBurst],
                      soundSpeed=soundSpeed[p$interleavedBurst],
                      accelerometerx=accelerometerx[p$interleavedBurst],
                      accelerometery=accelerometery[p$interleavedBurst],
                      accelerometerz=accelerometerz[p$interleavedBurst],
                      nominalCorrelation=nominalCorrelation[p$interleavedBurst],
                      transmitEnergy=transmitEnergy[p$interleavedBurst],
                      powerLevel=powerLevel[p$interleavedBurst])
        if (any(velocityIncluded[p$interleavedBurst])) {
            if (1 < length(unique(velocityIncluded[p$interleavedBurst])))
                stop("velocityIncluded values non-unique across 'interleavedBurst' data records")
            interleavedBurst$v <- array(double(), dim=c(length(p$interleavedBurst), ncellsInterleavedBurst, nbeamsInterleavedBurst))
        }
        if (any(amplitudeIncluded[p$interleavedBurst])) {
            if (1 < length(unique(amplitudeIncluded[p$interleavedBurst])))
                stop("amplitudeIncluded values non-unique across 'interleavedBurst' data records")
            interleavedBurst$a <- array(raw(), dim=c(length(p$interleavedBurst), ncellsInterleavedBurst, nbeamsInterleavedBurst))
        }
        if (any(correlationIncluded[p$interleavedBurst])) {
            if (1 < length(unique(correlationIncluded[p$interleavedBurst])))
                stop("correlationIncluded values non-unique across 'interleavedBurst' data records")
            interleavedBurst$q <- array(raw(), dim=c(length(p$interleavedBurst), ncellsInterleavedBurst, nbeamsInterleavedBurst))
        }
        if (any(altimeterIncluded[p$interleavedBurst])) {
            if (1 < length(unique(altimeterIncluded[p$burst])))
                stop("altimeterIncluded values non-unique across 'interleavedBurst' data records")
            interleavedBurst$altimeterDistance <- vector("numeric", length(p$interleavedBurst))
        }
        if (any(ASTIncluded[p$interleavedBurst])) {
            interleavedBurst$ASTDistance <- vector("numeric", length(p$interleavedBurst))
            interleavedBurst$ASTPressure <- vector("numeric", length(p$interleavedBurst))
        }
        if (any(echosounderIncluded[p$interleavedBurst])) {
            interleavedBurst$echosounder <- matrix(double(), ncol=length(p$interleavedBurst), nrow=ncellsInterleavedBurst)
        }
        if (any(AHRSIncluded[p$interleavedBurst])) {
            interleavedBurst$AHRS <- matrix(numeric(), nrow=length(p$interleavedBurst), ncol=9)
        }
    } else {
        interleavedBurst <- NULL
    }
    if (length(p$text) > 0) {
        text <- list(i=1, text=list()) # vector("character", length(p$text)))
    } else {
        text <- list()
    }
    ## Fill up th arrays in a loop. This could also be vectorized, if it proves slow.
    id <- d$id
    for (ch in 1:N) {
        oceDebug(debug>2, "d$id[", ch, "]=", d$id[[ch]], "\n", sep="")
        if (d$id[ch] == 0xa0) {        # text (0xa0 = 160)
            chars <- rawToChar(d$buf[seq.int(2+d$index[ch], by=1, length.out=-1+d$length[ch])])
            t <- strsplit(chars, "\r\n")[[1]]
            if (!typeGiven) {
                type <- gsub('.*STR="([^"]*)".*$', '\\1', t[grep("^ID,",t)])
                message("inferred type as '", type, "' from a text record")
                typeGiven <- TRUE
            }
            text$text[[text$i]] <- t
            text$i <- text$i + 1
            ##oceDebug(debug, "added to text; now, text$i=", text$i, "\n")
        } else if (d$id[ch] == 0x15) { # burst (0x15 = 21)
            ## FIXME FIXME FIXME put new code here, then insert in other blocks
            i <- d$index[ch]
            ncol <- burst$numberOfBeams
            nrow <- burst$numberOfCells
            n <- ncol * nrow
            n2 <- 2 * n
            i0 <- 77
            if (velocityIncluded[ch]) {
                v <- velocityFactor[ch]*readBin(d$buf[i+i0+seq(0,n2-1)], "integer",size=2,n=n,endian="little")
                burst$v[burst$i, , ] <- matrix(v, ncol=ncol, nrow=nrow, byrow=FALSE)
                i0 <- i0 + n2
            }
            if (amplitudeIncluded[ch]) {
                a <- d$buf[i + i0 + seq(0, n-1)]
                burst$a[burst$i, ,] <- matrix(a, ncol=ncol, nrow=nrow, byrow=FALSE)
                i0 <- i0 + n
            }
            if (correlationIncluded[ch]) {
                q <- d$buf[i + i0 + seq(0, n-1)]
                burst$q[burst$i, ,] <- matrix(q, ncol=ncol, nrow=nrow, byrow=FALSE)
                i0 <- i0 + n
            }
            if (altimeterIncluded[ch]) {
                burst$altimeterDistance[burst$i] <- readBin(d$buf[i + i0 + seq(0,3)], "numeric", size=4, n=1, endian="little")
                ## FIXME: perhaps save altimeterQuality from next 2 bytes
                ## FIXME: perhaps save altimeterStatus from next 2 bytes
                i0 <- i0 + 8
            }
            if (ASTIncluded[ch]) {
                ## bytes: 4(distance)+2(quality)+2(offset)+4(pressure)+8(spare)
                burst$ASTDistance <- readBin(d$buf[i + i0 + seq(0,3)], "numeric", size=4, n=1, endian="little")
                i0 <- i0 + 8 # advance past distance (4 bytes), then skip skip quality (2 bytes) and offset (2 bytes)
                burst$ASTPressure <- readBin(d$buf[i + i0 + seq(0,3)], "numeric", size=4, n=1, endian="little")
                i0 <- i0 + 12 # skip spare (8 bytes)
            }
            if (altimeterRawIncluded[ch]) {
                ## FIXME: perhaps read the data
                i0 <- i0 + 8           # 4(number)+2(distance)+2(samples)
            }
            if (echosounderIncluded[ch]) {
                burst$echosounder[burst$i, ] <- readBin(d$buf[i + i0 + seq(0,nrow-1)], size=2, n=nrow, endian="little")
                i0 <- i0 + 2 * nrow
            }
            if (AHRSIncluded[ch]) {
                burst$AHRS[burst$i,] <- readBin(d$buf[i + i0 + seq(0,35)], "numeric", size=4, n=9, endian="little")
            }
            burst$i <- burst$i + 1
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
            }
            if (amplitudeIncluded[ch]) {
                a <- d$buf[i + i0 + seq(0, n-1)]
                average$a[average$i, ,] <- matrix(a, ncol=ncol, nrow=nrow, byrow=FALSE)
                i0 <- i0 + n
            }
            if (correlationIncluded[ch]) {
                q <- d$buf[i + i0 + seq(0, n-1)]
                average$q[average$i, ,] <- matrix(q, ncol=ncol, nrow=nrow, byrow=FALSE)
                i0 <- i0 + n
            }
            if (altimeterIncluded[ch]) {
                average$altimeterDistance[average$i] <- readBin(d$buf[i + i0 + seq(0,3)],"numeric", size=4,n=1,endian="little")
                ## FIXME: perhaps save altimeterQuality from next 2 bytes
                ## FIXME: perhaps save altimeterStatus from next 2 bytes
                i0 <- i0 + 8
            }
            if (ASTIncluded[ch]) {
                ## bytes: 4(distance)+2(quality)+2(offset)+4(pressure)+8(spare)
                average$ASTDistance <- readBin(d$buf[i + i0 + seq(0,3)], "numeric", size=4, n=1, endian="little")
                i0 <- i0 + 8 # advance past distance (4 bytes), then skip skip quality (2 bytes) and offset (2 bytes)
                average$ASTPressure <- readBin(d$buf[i + i0 + seq(0,3)], "numeric", size=4, n=1, endian="little")
                i0 <- i0 + 12 # skip spare (8 bytes)
            }
            if (altimeterRawIncluded[ch]) {
                ## FIXME: perhaps read the data
                i0 <- i0 + 8           # 4(number)+2(distance)+2(samples)
            }
            if (echosounderIncluded[ch]) {
                average$echosounder[average$i, ] <- readBin(d$buf[i + i0 + seq(0,nrow-1)], size=2, n=nrow, endian="little")
                i0 <- i0 + 2 * nrow
            }
            if (AHRSIncluded[ch]) {
                average$AHRS[average$i,] <- readBin(d$buf[i + i0 + seq(0,35)],"numeric", size=4, n=9, endian="little")
            }
            average$i <- average$i + 1
        } else if (d$id[ch] == 0x17) { # bottom track (0x17 = 23)
            i <- d$index[ch]
            ncol <- bottomTrack$numberOfBeams
            nrow <- bottomTrack$numberOfCells
            message("i=", i, ", ch=", ch, ", bottomTrack$i=", bottomTrack$i, " ...")
            ## distance: uses variable name that makes sense for average/burst data
            i0 <- 77
            if (velocityIncluded[ch]) { # configuration[,9]=bit8 [1 pages 60 and 62]
                bottomTrack$v[i$bottomTrack, ] <- 0.001*readBin(buf[i + i0 + seq(0,4*ncol-1)], "numeric", size=4, n=ncol, endian="little")
                i0 <- i0 + 4*ncol
                message(" ... stored bottomTrack$altimeterDistance")
            }
             if (altimeterIncluded[ch]) { # configuration[,9]=bit8 [1 pages 60 and 62]
                bottomTrack$altimeterDistance[i$bottomTrack, ] <- readBin(buf[i + i0 + seq(0,4*ncol-1)], "numeric", size=4, n=ncol, endian="little")
                i0 <- i0 + 4*ncol
                message(" ... stored bottomTrack$altimeterDistance")
            }
            ## figureOfMerit: uses variable name that makes sense for average/burst data
            if (altimeterRawIncluded[ch]) { # configuration[,10]=bit9 [1 pages 60 and 62]
                bottomTrack$altimeterFigureOfMerit[i$bottomTrack, ] <- readBin(buf[i + i0 + seq(0,2*ncol-1)], "numeric", size=2, n=ncol, endian="little")
                i0 <- i0 + 4*ncol
                message(" ... stored bottomTrack$altimeterFigureOfMerit")
            }
            bottomTrack$i <- bottomTrack$i + 1
        } else if (d$id[ch] == 0x18) { # interleaved burst (0x18 = 24 )
            i <- d$index[ch]
            ncol <- interleavedBurst$numberOfBeams
            nrow <- interleavedBurst$numberOfCells
            n <- ncol * nrow
            n2 <- 2 * n
            i0 <- 77
            if (velocityIncluded[ch]) {
                v <- velocityFactor[ch]*readBin(d$buf[i+i0+seq(0,n2-1)],"integer",size=2,n=n,endian="little")
                interleavedBurst$v[interleavedBurst$i, , ] <- matrix(v, ncol=ncol, nrow=nrow, byrow=FALSE)
                i0 <- i0 + n2
            }
            if (amplitudeIncluded[ch]) {
                a <- d$buf[i + i0 + seq(0, n-1)]
                interleavedBurst$a[interleavedBurst$i, ,] <- matrix(a, ncol=ncol, nrow=nrow, byrow=FALSE)
                i0 <- i0 + n
            }
            if (correlationIncluded[ch]) {
                q <- d$buf[i + i0 + seq(0, n-1)]
                interleavedBurst$q[interleavedBurst$i, ,] <- matrix(q, ncol=ncol, nrow=nrow, byrow=FALSE)
                i0 <- i0 + n
            }
            if (altimeterIncluded[ch]) {
                interleavedBurst$altimeterDistance[interleavedBurst$i] <- readBin(d$buf[i + i0 + seq(0,3)],"numeric", size=4,n=1,endian="little")
                ## FIXME: perhaps save altimeterQuality from next 2 bytes
                ## FIXME: perhaps save altimeterStatus from next 2 bytes
                i0 <- i0 + 8
            }
            if (ASTIncluded[ch]) {
                ## bytes: 4(distance)+2(quality)+2(offset)+4(pressure)+8(spare)
                interleavedBurst$ASTDistance <- readBin(d$buf[i + i0 + seq(0,3)], "numeric", size=4, n=1, endian="little")
                i0 <- i0 + 8 # advance past distance (4 bytes), then skip skip quality (2 bytes) and offset (2 bytes)
                interleavedBurst$ASTPressure <- readBin(d$buf[i + i0 + seq(0,3)], "numeric", size=4, n=1, endian="little")
                i0 <- i0 + 12 # skip spare (8 bytes)
            }
            if (altimeterRawIncluded[ch]) {
                ## FIXME: perhaps read the data
                i0 <- i0 + 8           # 4(number)+2(distance)+2(samples)
            }
            if (echosounderIncluded[ch]) {
                interleavedBurst$echosounder[interleavedBurst$i, ] <- readBin(d$buf[i + i0 + seq(0,nrow-1)], size=2, n=nrow, endian="little")
                i0 <- i0 + 2 * nrow
            }
            if (AHRSIncluded[ch]) {
                interleavedBurst$AHRS[interleavedBurst$i,] <- readBin(d$buf[i + i0 + seq(0,35)], "numeric", size=4, n=9, endian="little")
            }
            interleavedBurst$i <- interleavedBurst$i + 1
        } else if (d$id[ch] == 0x1c) { # echosounder (0x1c)
            i <- d$index[ch]
            ncol <- echosounder$numberOfBeams
            nrow <- echosounder$numberOfCells
            n <- ncol * nrow
            n2 <- 2 * n
            i0 <- 77
            if (velocityIncluded[ch]) {
                v <- velocityFactor[ch]*readBin(d$buf[i+i0+seq(0,n2-1)],"integer",size=2,n=n,endian="little")
                echosounder$v[echosounder$i, , ] <- matrix(v, ncol=ncol, nrow=nrow, byrow=FALSE)
                i0 <- i0 + n2
            }
            if (amplitudeIncluded[ch]) {
                a <- d$buf[i + i0 + seq(0, n-1)]
                echosounder$a[echosounder$i, ,] <- matrix(a, ncol=ncol, nrow=nrow, byrow=FALSE)
                i0 <- i0 + n
            }
            if (correlationIncluded[ch]) {
                q <- d$buf[i + i0 + seq(0, n-1)]
                echosounder$q[echosounder$i, ,] <- matrix(q, ncol=ncol, nrow=nrow, byrow=FALSE)
                i0 <- i0 + n
            }
            if (altimeterIncluded[ch]) {
                echosounder$altimeterDistance[echosounder$i] <- readBin(d$buf[i + i0 + seq(0,3)],"numeric", size=4,n=1,endian="little")
                ## FIXME: perhaps save altimeterQuality from next 2 bytes
                ## FIXME: perhaps save altimeterStatus from next 2 bytes
                i0 <- i0 + 8
            }
            if (ASTIncluded[ch]) {
                ## bytes: 4(distance)+2(quality)+2(offset)+4(pressure)+8(spare)
                echosounder$ASTDistance <- readBin(d$buf[i + i0 + seq(0,3)], "numeric", size=4, n=1, endian="little")
                i0 <- i0 + 8 # advance past distance (4 bytes), then skip skip quality (2 bytes) and offset (2 bytes)
                echosounder$ASTPressure <- readBin(d$buf[i + i0 + seq(0,3)], "numeric", size=4, n=1, endian="little")
                i0 <- i0 + 12 # skip spare (8 bytes)
            }
            if (altimeterRawIncluded[ch]) {
                ## FIXME: perhaps read the data
                i0 <- i0 + 8           # 4(number)+2(distance)+2(samples)
            }
            if (echosounderIncluded[ch]) {
                echosounder$echosounder[echosounder$i, ] <- readBin(d$buf[i + i0 + seq(0,nrow-1)], size=2, n=nrow, endian="little")
                i0 <- i0 + 2 * nrow
            }
            if (AHRSIncluded[ch]) {
                echosounder$AHRS[echosounder$i,] <- readBin(d$buf[i + i0 + seq(0,35)], "numeric", size=4, n=9, endian="little")
            }
            echosounder$i <- echosounder$i + 1
        } else {
            ## FIXME: read other fields
        }
    }

    ## Prepare data
    data <- list(powerLevel=powerLevel, # FIXME: put in individual items?
                 status=status,
                 activeConfiguration=activeConfiguration)
    if (!is.null(average)) {
        average$i <- NULL
        data$average <- average
    }
    if (!is.null(bottomTrack)) {
        bottomTrack$i <- NULL
        data$bottomTrack <- bottomTrack
    }
    if (!is.null(burst)) {
        burst$i <- NULL
        data$burst <- burst
    }
    if (!is.null(echosounder)) {
        echosounder$i <- NULL
        data$echosounder <- echosounder
    }
    if (!is.null(interleavedBurst)) {
        interleavedBurst$i <- NULL
        data$interleavedBurst <- interleavedBurst
    }
    if (length(text)) {
        text$i <- NULL
        data$text <- text
    }

    ## Insert metadata
    res@metadata$id <- id
    res@metadata$manufacturer <- "nortek"
    res@metadata$fileType <- "AD2CP"
    res@metadata$serialNumber <- serialNumber
    res@metadata$header <- header
    ## Warn if we had to guess the type
    if (!typeGiven) {
        type <- "Signature1000"
        warning("defaulting 'type' to '", type, "', since no header was found in the file, and the 'type' argument was not provided")
    }
    res@metadata$type <- type
    res@metadata$frequency <- ad2cpHeaderValue(x=header, key="BEAMCFGLIST,BEAM=1", item="FREQ", default=NA)
    res@metadata$beamAngle <- switch(type, "Signature1000"=25, "Signature500"=25, "Signature250"=20)
    theta <- res@metadata$beamAngle * atan2(1,1) / 45
    ## The creation of a transformation matrix is covered in Section 5.3 of
    ## RD Instruments. “ADCP Coordinate Transformation.” RD Instruments, July 1998.
    TMc <- 1 # for convex beam stup; use -1 for concave
    TMa <- 1 / (2 * sin(theta))
    TMb <- 1 / (4 * cos(theta))
    TMd <- TMa / sqrt(2)
    res@metadata$transformationMatrix <- rbind(c(TMc*TMa, -TMc*TMa,        0,       0),
                                               c(      0,        0, -TMc*TMa, TMc*TMa),
                                               c(    TMb,      TMb,      TMb,     TMb),
                                               c(    TMd,      TMd,     -TMd,    -TMd))
    ## Remove the overall coordinate (created by initializer) since it has no meaning here.
    res@metadata$oceCoordinate <- NULL
    ## Insert data
    res@data <- data
    ## Insert processingLog
    if (missing(processingLog))
        processingLog <- paste("read.adp.ad2cp(file=\"", filename, "\", from=", from, ", to=", to, ", by=", by, ")", sep="")
    res@processingLog <- processingLogItem(processingLog)
    oceDebug(debug, "} # read.adp.ad2cp()\n", unindent=1)
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
    res@metadata$fileType <- type
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
