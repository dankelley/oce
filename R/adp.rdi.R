# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

# byte sequences at start of items
# FLH 00 00; VLH 00 80; vel 00 01; Cor 00 02;  echo 00 03; percent 00 04; bottom-track 00 06

# References
#
# teledyne2005wcao: Workhorse Commands and Output Data format. March 2005
# Teledyne RD Instruments, 2005.
# ("WorkHorse Commands and Output Data Format_Mar05.pdf" in Dan Kelley's collection)
#
# teledyne2007wcao: Workhorse Commands and Output Data format. November 2007
# Teledyne RD Instruments, 2007.
# ("WorkHorse Technical Manual_Nov07.pdf" in Dan Kelley's collection)
#
# teledyne2010wcao: Workhorse Commands and Output Data format. August 2010
# Teledyne RD Instruments, 2010.
# ("WorkHorse_commands_data_format_AUG10.PDF" in Dan Kelley's collection)
#
# teledyne2014ostm: Ocean Surveyor / Ocean Observer technical manual
# Teledyne RD Instruments, 2014.
# ("OS_TM_Apr14.pdf" in Dan Kelley's collection)
#
# teledyne2015vsod: V Series output data format
# Teledyne RD Instruments, 2015.
# ("SV_ODF_May15.pdf")

#' Trim an RDI adp File
#'
#' Create an RDI adp file by copying the first `n` data chunks (starting with
#' byte 0x7f 0x7f) of another such file. This can be useful in supplying small
#' sample files for bug reports.
#'
#' @param infile name of an RDI file.
#'
#' @param n integer indicating the number of data chunks to keep. The default is
#' to keep 100 chunks, a common choice for sample files.
#'
#' @param outfile optional name of the new RDI file to be created. If this is not
#' supplied, a default is used, by adding `_trimmed` to the base filename, e.g.
#' if `infile` is `"a.000"` then `outfile` will be `a_trimmed.000`.
#'
#' @param debug an integer value indicating the level of debugging. If
#' this is 0, then [read.adp.rdi()] proceeds quietly, except for
#' issuing warnings and errors if necessary.  If it is 1, then the R
#' code of [read.adp.rdi()] produces some messages.  If it is 2, then also
#' the underlying C/C++ code produces a message each time a possible
#' ensemble is detected.  If it is 3, then the C/C++ code also produces
#' information on some details of the ensemble.  Levels 2 and 3 are
#' mainly for use by the developers.
#'
#' @return `adpRdiFileTrim()` returns the name of the output file, `outfile`, as
#' provided or constructed.
#'
#' @family things related to adp data
#' @family functions that trim data files
#' @section Sample of Usage:
#' \preformatted{
#' # Can only be run by the developer, since it uses a private file.
#' file  <- "~/data/archive/sleiwex/2008/moorings/m09/adp/rdi_2615/raw/adp_rdi_2615.000"
#' if (file.exists(file)) {
#'     adpRdiFileTrim(file, 9L, "test.000")
#' }
#' }
#' @author Dan Kelley
adpRdiFileTrim <- function(infile, n = 100L, outfile, debug = getOption("oceDebug")) {
    oceDebug(debug, "adpRdiFileTrim(infile=\"", infile, "\", n=", n, ", debug=", debug, ") START\n", unindent = 1)
    debug <- ifelse(debug < 1, 0L, ifelse(debug < 2, 1, 2))
    if (missing(infile)) {
        stop("must provide 'infile'")
    }
    n <- as.integer(n)
    if (n < 1L) {
        stop("'n' must be a positive number, but it is ", n)
    }
    if (missing(outfile)) {
        outfile <- gsub("^(.*)\\.([^.]*)$", "\\1_trimmed.\\2", infile)
        oceDebug(debug, "created outfile value \"", outfile, "\"")
    }
    r <- read.oce(infile, which = "??")
    nmax <- length(r$start)
    if (n >= nmax) {
        stop("maximum allowed 'n' for this file is ", nmax)
    }
    # add 1 to profile count; go back 1 char before that
    last <- r$start[n + 1L] - 1L
    buf <- readBin(infile, "raw", n = last)
    writeBin(buf, outfile, useBytes = TRUE)
    oceDebug(debug, "END adpRdiFileTrim()\n", unindent = 1)
    outfile
}


decodeHeaderRDI <- function(buf, debug = getOption("oceDebug"), tz = getOption("oceTz"), ...) {
    byte1 <- as.raw(0x7f)
    byte2 <- as.raw(0x7f)
    #<<>> byte2 <- as.raw(0x79)
    # header length 6+2*numberOfDataTypes bytes (see e.g. Figure 44, page 160 of Surveyor docs)
    oceDebug(debug, "decodeHeaderRDI(buf, debug=", debug, ") START\n", unindent = 1)
    if (buf[1] != byte1 || buf[2] != byte2) {
        stop("first two bytes in file must be 0x", byte1, " 0x", byte2, ", but they are 0x", buf[1], " 0x", buf[2])
    }
    # FIXME: for sentinel files bytesPerEnsemble isn't the same for all ensembles
    bytesPerEnsemble <- readBin(buf[3:4], "integer", n = 1, size = 2, endian = "little", signed = FALSE)
    oceDebug(debug, "bytesPerEnsemble=", bytesPerEnsemble, "\n")
    # byte5 not used
    numberOfDataTypes <- readBin(buf[6], "integer", n = 1, size = 1)
    if (numberOfDataTypes < 1 || 200 < numberOfDataTypes) {
        stop("cannot have ", numberOfDataTypes, " data types, as header indicates")
    }
    oceDebug(debug, "numberOfDataTypes=", numberOfDataTypes, "\n")
    haveActualData <- numberOfDataTypes > 2 # will be 2 if just have headers
    oceDebug(debug, "haveActualData=", haveActualData, "\n")
    dataOffset <- readBin(buf[7 + 0:(2 * numberOfDataTypes)], "integer", n = numberOfDataTypes, size = 2, endian = "little", signed = FALSE)
    if (dataOffset[1] != 6 + 2 * numberOfDataTypes) {
        warning("dataOffset and numberOfDataTypes are inconsistent -- this dataset seems damaged")
    }
    oceDebug(debug, "head(dataOffset)=", paste(head(dataOffset), collapse = " "), "\n")
    oceDebug(debug, "head(sort(diff(dataOffset)))=", paste(head(sort(diff(dataOffset))), collapse = " "), "\n")
    # Set up codes
    codes <- cbind(buf[1 + c(0, dataOffset)], buf[1 + c(0, dataOffset) + 1])
    oceDebug(debug, "set up codes starting at buf[1:10]=", paste("0x", paste(buf[1:10], sep = ", "), collapse = " ", sep = ""), "\n")
    oceDebug(debug, " codes[,1]=", paste("0x", paste(codes[, 1], sep = ""), collapse = " ", sep = ""), "\n")
    oceDebug(debug, " codes[,2]=", paste("0x", paste(codes[, 2], sep = ""), collapse = " ", sep = ""), "\n")
    # Determine whether this is a Sentinel V file.
    isSentinel <- FALSE
    isSentinel <- any(codes[, 1] == 0x00 & codes[, 2] == 0x70)
    oceDebug(debug, "isSentinel =", isSentinel, " (inferred from whether we have 0x00 0x70 ID)\n")
    # Fixed Leader Data, abbreviated FLD, pointed to by the dataOffset
    FLD <- buf[dataOffset[1] + 1:(dataOffset[2] - dataOffset[1])]
    oceDebug(debug, "Fixed Leader Data:", paste(FLD, collapse = " "), "\n")
    if (FLD[1] != 0x00 && FLD[1] != 0x01) {
        stop("first byte of fixed leader header must be 0x00 or 0x01 but it is ", FLD[1])
    }
    if (FLD[2] != 0x00) {
        stop("second byte of fixed leader header must be a0x00 but it is ", FLD[2])
    }
    firmwareVersionMajor <- readBin(FLD[3], "integer", n = 1, size = 1, signed = FALSE)
    firmwareVersionMinor <- readBin(FLD[4], "integer", n = 1, size = 1, signed = FALSE)
    firmwareVersion <- paste(firmwareVersionMajor, firmwareVersionMinor, sep = ".")
    firmwareVersionNumeric <- as.numeric(firmwareVersion)
    oceDebug(debug, "firmwareVersion=", firmwareVersion, " (numerically, it is ", firmwareVersionNumeric, ")\n")
    # if (firmwareVersion < 16.28) warning("firmwareVersion ", firmwareVersion, " is less than 16.28, and so read.adp.rdi() may not work properly")
    # If no actual data, return something minimal
    if (!haveActualData) {
        return(list(
            instrumentType = "adcp",
            firmwareVersionMajor = firmwareVersionMajor,
            firmwareVersionMinor = firmwareVersionMinor,
            firmwareVersion = firmwareVersion,
            haveActualData = haveActualData
        ))
    }
    # FLD[5] = SYSTEM CONFIGURATION LSB (Table 5.2, page 126, System Integrator Guide, Nov 2007)
    # FLD[6] = SYSTEM CONFIGURATION MSB
    systemConfiguration <- paste(byteToBinary(FLD[5], endian = "big"), byteToBinary(FLD[6], endian = "big"), sep = "-")
    oceDebug(debug, "systemConfiguration=\"", systemConfiguration, "\"\n")
    oceDebug(debug, "FLD[4]=", byteToBinary(FLD[4], endian = "big"), "(looking near the systemConfiguration bytes to find a problem)\n")
    oceDebug(debug, "FLD[5]=", byteToBinary(FLD[5], endian = "big"), "(should be one of the systemConfiguration bytes)\n")
    oceDebug(debug, "FLD[6]=", byteToBinary(FLD[6], endian = "big"), "(should be one of the systemConfiguration bytes)\n")
    oceDebug(debug, "FLD[7]=", byteToBinary(FLD[7], endian = "big"), "(looking near the systemConfiguration bytes to find a problem)\n")
    bits <- substr(systemConfiguration, 6, 8)
    bitsFLD5 <- rawToBits(FLD[5])
    oceDebug(debug, "LSB=", paste(bitsFLD5, collapse = " "), "\n")
    bitsFLD6 <- rawToBits(FLD[6])
    oceDebug(debug, "MSB=", paste(bitsFLD6, collapse = " "), "\n")
    # NOTE: the nearby code should perhaps use .Call("get_bit", ...) for speed and clarity
    if (isSentinel) {
        if (bits == "010") {
            frequency <- 250
        } # kHz
        else if (bits == "011") {
            frequency <- 500
        } else if (bits == "100") {
            frequency <- 1000
        } else {
            stop("unknown freq. bit code:", bits, " (expect 010 for 250kHz, 010 for 500kHz, etc)")
        }
    } else {
        if (bits == "000") {
            frequency <- 75
        } # kHz
        else if (bits == "001") {
            frequency <- 150
        } else if (bits == "010") {
            frequency <- 300
        } else if (bits == "011") {
            frequency <- 600
        } else if (bits == "100") {
            frequency <- 1200
        } else if (bits == "101") {
            frequency <- 2400
        } else if (bits == "110") {
            frequency <- 38
        } else {
            stop("unknown freq. bit code:", bits, " (expect 000 for 75kHz, 001 for 150kHz, etc)")
        }
    }
    oceDebug(debug, "bits:", bits, "so frequency=", frequency, "\n")
    bits <- substr(systemConfiguration, 16, 17)
    if (isSentinel) {
        oceDebug(debug, "systemConfiguration:", systemConfiguration, "\n")
        oceDebug(debug, "bits:", bits, " (Expect 111 for 25 degrees)\n")
        bits <- substr(systemConfiguration, 15, 17)
        if (bits != "111") message("Assuming beam angle of 25deg, but SysCon bits aren't 111")
        beamAngle <- 25
    } else {
        oceDebug(debug, "systemConfiguration:", systemConfiguration, "\n")
        oceDebug(debug, "bits:", bits, "00 is 15deg, 01 is 20deg, 02 is 30deg, 11 is 'other'\n")
        if (bits == "00") {
            beamAngle <- 15
        } else if (bits == "01") {
            beamAngle <- 20
        } else if (bits == "10") {
            beamAngle <- 30
        } else if (bits == "11") beamAngle <- NA # means 'other'
    }
    oceDebug(debug, "bits=", bits, "so beamAngle=", beamAngle, "\n")
    bits <- substr(systemConfiguration, 5, 5)
    if (isSentinel) {
        beamPattern <- "convex" # is always convex for a SentinelV
    } else {
        if (bits == "0") {
            beamPattern <- "concave"
        } else {
            beamPattern <- "convex"
        }
    }
    oceDebug(debug, "bits=", bits, "so beamPattern=", beamPattern, "\n")
    beamConfig <- "?"
    bits <- substr(systemConfiguration, 10, 13)
    if (isSentinel) {
        if (bits == "0100") {
            beamConfig <- "4 beam janus"
        } else if (bits == "0101") {
            beamConfig <- "5 beam janus"
        } else {
            beamConfig <- "unknown"
        }
    } else {
        if (bits == "0100") {
            beamConfig <- "janus"
        } else if (bits == "0101") {
            beamConfig <- "janus demod"
        } else if (bits == "1111") {
            beamConfig <- "janus 2 demd"
        } else {
            beamConfig <- "unknown"
        }
    }
    bits <- substr(systemConfiguration, 1, 1)
    # if (bits == "1") orientation <- "upward" else orientation <- "downward"
    # oceDebug(debug, "bits=", bits, "so that orientation=", orientation, "\n")
    # real.sim.flag <- readBin(FLD[7], "integer", n=1, size=1)
    # lagLength <- readBin(FLD[8], "integer", n=1, size=1, signed=FALSE) # unused
    numberOfBeams <- readBin(FLD[9], "integer", n = 1, size = 1, signed = FALSE)
    oceDebug(debug, "numberOfBeams", numberOfBeams, "\n")
    numberOfCells <- abs(readBin(FLD[10], "integer", n = 1, size = 1, signed = FALSE)) # WN
    oceDebug(debug, "numberOfCells", numberOfCells, "\n")
    pingsPerEnsemble <- readBin(FLD[11:12], "integer", n = 1, size = 2, endian = "little")
    cellSize <- readBin(FLD[13:14], "integer", n = 1, size = 2, endian = "little") / 100 # WS in m
    if (cellSize < 0 || cellSize > 64) {
        stop("cellSize of ", cellSize, "m is not in the allowed range of 0m to 64m")
    }
    # blank.after.transmit <- readBin(FLD[15:16], "integer", n=1, size=2, endian="little") / 100 # in m
    profilingMode <- readBin(FLD[17], "integer", n = 1, size = 1) # WM
    lowCorrThresh <- readBin(FLD[18], "integer", n = 1, size = 1)
    numberOfCodeReps <- readBin(FLD[19], "integer", n = 1, size = 1)
    percentGdMinimum <- readBin(FLD[20], "integer", n = 1, size = 1)
    errorVelocityMaximum <- readBin(FLD[21:22], "integer", n = 1, size = 2, endian = "little")
    # tpp.minutes <- readBin(FLD[23], "integer", n=1, size=1)
    # tpp.seconds <- readBin(FLD[24], "integer", n=1, size=1)
    # tpp.hundredths <- readBin(FLD[25], "integer", n=1, size=1)
    coordTransform <- byteToBinary(FLD[26], endian = "big")
    bits <- substr(byteToBinary(FLD[26], endian = "big"), 4, 5)
    originalCoordinate <- "???"
    if (bits == "00") {
        originalCoordinate <- "beam"
    } else if (bits == "01") {
        originalCoordinate <- "xyz"
    } else if (bits == "10") {
        originalCoordinate <- "sfm"
    } else if (bits == "11") {
        originalCoordinate <- "enu"
    }
    bits <- substr(byteToBinary(FLD[26], endian = "big"), 6, 6)
    tiltUsed <- bits == "1"
    bits <- substr(byteToBinary(FLD[26], endian = "big"), 7, 7)
    threeBeamUsed <- bits == "1"
    bits <- substr(byteToBinary(FLD[26], endian = "big"), 8, 8)
    binMappingUsed <- bits == "1"
    headingAlignment <- 0.01 * readBin(FLD[27:28], "integer", n = 1, size = 2, endian = "little") # WCODF p 130
    headingBias <- 0.01 * readBin(FLD[29:30], "integer", n = 1, size = 2, endian = "little") # WCODF p 130
    oceDebug(debug, "headingAlignment=", headingAlignment, "; headingBias=", headingBias, "\n")
    sensorSource <- byteToBinary(FLD[31], endian = "big")
    sensorsAvailable <- byteToBinary(FLD[32], endian = "big")
    bin1Distance <- readBin(FLD[33:34], "integer", n = 1, size = 2, endian = "little", signed = FALSE) * 0.01
    # cat("bin1Distance being inferred from 0x", FLD[33:34], " as ", bin1Distance, "\n", sep="", ...)
    xmitPulseLength <- readBin(FLD[35:36], "integer", n = 1, size = 2, endian = "little", signed = FALSE) * 0.01
    # cat("xmitPulseLength being inferred from 0x", FLD[35:36], " as ", xmitPulseLength, "\n", sep="", ...)
    wpRefLayerAverage <- readBin(FLD[37:38], "integer", n = 1, size = 2, endian = "little")
    falseTargetThresh <- readBin(FLD[39], "integer", n = 1, size = 1)
    # FLD[40] spare
    transmitLagDistance <- readBin(FLD[41:42], "integer", n = 1, size = 2, endian = "little", signed = FALSE)
    cpuBoardSerialNumber <- c(
        readBin(FLD[43], "integer", n = 1, size = 1, signed = FALSE),
        readBin(FLD[44], "integer", n = 1, size = 1, signed = FALSE),
        readBin(FLD[45], "integer", n = 1, size = 1, signed = FALSE),
        readBin(FLD[46], "integer", n = 1, size = 1, signed = FALSE),
        readBin(FLD[47], "integer", n = 1, size = 1, signed = FALSE),
        readBin(FLD[48], "integer", n = 1, size = 1, signed = FALSE),
        readBin(FLD[49], "integer", n = 1, size = 1, signed = FALSE),
        readBin(FLD[50], "integer", n = 1, size = 1, signed = FALSE)
    )
    oceDebug(debug, "cpuBoardSerialNumber=\"", paste(cpuBoardSerialNumber, collapse = ""), "\"\n", sep = "")
    systemBandwidth <- readBin(FLD[51:52], "integer", n = 1, size = 2, endian = "little")
    # systemPower <- readBin(FLD[53], "integer", n=1, size=1)
    # FLD[54] spare
    # "WorkHorse Commands and Output Data Format_Mar05.pdf" p130: bytes 55:58 = serialNumber only for REMUS, else spare
    # "WorkHorse Commands and Output Data Format_Nov07.pdf" p127: bytes 55:58 = serialNumber
    serialNumber <- readBin(FLD[55:58], "integer", n = 1, size = 4, endian = "little")
    oceDebug(
        debug, "serialNumber=", serialNumber, ", based on bytes 55:58 of the Fixed Leader Header, ",
        " which are: 0x", FLD[55], " 0x", FLD[56], " 0x", FLD[57], " 0x", FLD[58], "\n"
    )
    if (serialNumber == 0) {
        serialNumber <- "unknown"
    }
    # beamAngle <- readBin(FLD[59], "integer", n=1, size=1) # NB 0 in first test case
    # cat("BEAM ANGLE=", FLD[59], "or", beamAngle, "\n", ...)
    #
    # VLD (variable leader data)
    #   The VLD length varies (see below) so infer its position from dataOffset[1].
    #
    # teledyne2005wcao and teledyne2007wcao (Figure 9 on page 122):
    #       HEADER (6+2*num.types bytes) bytes
    #       FLD 59 bytes
    #       VLD 65 bytes
    # "Ocean Surveyor Technical Manual.pdf" table D-3 on page D-5 (pdf-page 139):
    #       HEADER (6+2*num.types) bytes
    #       FLD 50 bytes
    #       VLD 58 bytes
    # teledyne2014ostm figure 45 p144 with WP *or* NP command
    #       HEADER  (6+2*num.types) bytes
    #       FLD 50 bytes
    #       VLD 60 bytes
    # teledyne2014ostm figure 46 p145 with WP *and* NP command
    #       HEADER  (6+2*num.types) bytes
    #       FLD 50 bytes
    #       VLD 60 bytes
    # Ocean Surveyor Technical Manual_Dec20.pdf figure 44 page 160
    #       HEADER (6+2*num.types) bytes
    #       FLD 50 bytes
    #       VLD 60 bytes
    # dataOffset[1] = within-ensemble byte offset for FLD (e.g. Table D-1 of Surveyor manual)
    # dataOffset[2] = within-ensemble byte offset for VLD (e.g. Table D-1 of Surveyor manual)
    # thus, length of FLD is dataOffset[2]-dataOffset[1]
    FLDLength <- dataOffset[2] - dataOffset[1]
    oceDebug(
        debug, "FLDLength=", FLDLength, " as inferred from dataOffset[2]-dataOffset[1]. ",
        "Is this right? See GH issue #1861. I have sometimes seen 59 for Workhorse, ",
        "or 50 for Surveyor/Observer.\n"
    )
    # ----------
    # RISKY CODE
    # ----------
    #
    # There really seems to be nothing specific in the file to tell instrument type, so, in an act of
    # desparation (or is that hope) I'm going to flag on the one thing that was clearly stated, and
    # clearly different, in the two documentation entries. The exception is the 'sentinel V' class,
    # which is recognized by a code sequence 0x00 0x70 (see near line 96), so we capture that first.
    if (isSentinel) {
        instrumentSubtype <- "sentinelV"
        # 5 beam (4 beam workhorse + a vertical centre beam)
    } else if (FLDLength == 59) {
        # "WorkHorse Commands and Output Data Format_Mar05.pdf" (and Nov07 version) Figure 9 on page 122 (pdf-page 130)
        instrumentSubtype <- "workhorse"
    } else if (FLDLength == 50) {
        # "Ocean Surveyor Technical Manual.pdf" table D-3 on page D-5 (pdf-page 139)
        # also teledyne2014ostm page 144 says could be Surveyor or Observer
        instrumentSubtype <- "surveyor/observer"
    } else {
        # FIXME: I think this is a poor way to determine the instrument type. Why do we even try?
        #> warning("unexpected length ", FLDLength, " of fixed-leader-data header; expecting 50 for
        #>         'surveyor/observor' or 59 for 'workhorse'.")
        instrumentSubtype <- "unknown"
    }
    oceDebug(debug, "instrumentSubtype=\"", instrumentSubtype, "\"\n")
    nVLD <- 65 # FIXME: should use the proper length, but we won't use it all anyway
    VLD <- buf[dataOffset[2] + 1:nVLD]
    oceDebug(debug, "Variable Leader Data (", length(VLD), "bytes):", paste(VLD, collapse = " "), "\n")
    # ensure that header is not ill-formed
    if (VLD[1] != 0x80) {
        stop("byte 1 of variable leader data should be 0x80, but it is ", VLD[1])
    }
    if (VLD[2] != 0x00) {
        stop("byte 2 of variable leader data should be 0x00, but it is ", VLD[2])
    }
    # ensemble.number <- readBin(VLD[3:4], "integer", n=1, size=2, endian="little")
    # Assemble the time.  This follows section 5.3 (paper 132, file page 140) of "Workhorse Commands and Output Data Format_Nov07.pdf"
    # FIXME: probably would save time to read all elements at once.  Instrument to check
    RTC.year <- unabbreviateYear(readBin(VLD[5], "integer", n = 1, size = 1))
    RTC.month <- readBin(VLD[6], "integer", n = 1, size = 1)
    RTC.day <- readBin(VLD[7], "integer", n = 1, size = 1)
    RTC.hour <- readBin(VLD[8], "integer", n = 1, size = 1)
    RTC.minute <- readBin(VLD[9], "integer", n = 1, size = 1)
    RTC.second <- readBin(VLD[10], "integer", n = 1, size = 1)
    RTC.hundredths <- readBin(VLD[11], "integer", n = 1, size = 1)
    time <- ISOdatetime(RTC.year, RTC.month, RTC.day, RTC.hour, RTC.minute, RTC.second + RTC.hundredths / 100, tz = tz)
    oceDebug(
        debug, "profile time=", format(time), "(year=", RTC.year,
        "month=", RTC.month, "day-", RTC.day, "hour=", RTC.hour,
        "minute=", RTC.minute, "second=", RTC.second, "hundreds=", RTC.hundredths, ")\n"
    )
    # ensembleNumberMSB <- readBin(VLD[12], "integer", n=1, size=1)
    # bitResult <- readBin(VLD[13:14], "integer", n=1, size=2, endian="little")
    soundSpeed <- readBin(VLD[15:16], "integer", n = 1, size = 2, endian = "little")
    oceDebug(debug, "soundSpeed= ", soundSpeed, "\n") # FIXME possibly wrong
    transducerDepth <- readBin(VLD[17:18], "integer", n = 1, size = 2, endian = "little")
    oceDebug(debug, "transducerDepth = ", transducerDepth, "\n")
    if (soundSpeed < 1400 || soundSpeed > 1600) {
        warning("soundSpeed is ", soundSpeed, ", which is outside the permitted range of 1400 m/s to
            1600 m/s.  Something may be wrong in decoding the data.")
    }
    oceDebug(debug, "about to create the list to be returned\n")
    res <- list(
        instrumentType = "adcp",
        instrumentSubtype = instrumentSubtype,
        firmwareVersionMajor = firmwareVersionMajor,
        firmwareVersionMinor = firmwareVersionMinor,
        firmwareVersion = firmwareVersion,
        bytesPerEnsemble = bytesPerEnsemble,
        systemConfiguration = systemConfiguration,
        frequency = frequency,
        beamAngle = beamAngle,
        beamPattern = beamPattern,
        beamConfig = beamConfig,
        numberOfDataTypes = numberOfDataTypes,
        dataOffset = dataOffset,
        codes = codes,
        numberOfBeams = numberOfBeams,
        numberOfCells = numberOfCells,
        pingsPerEnsemble = pingsPerEnsemble,
        cellSize = cellSize,
        transducerDepth = transducerDepth,
        profilingMode = profilingMode,
        lowCorrThresh = lowCorrThresh,
        numberOfCodeReps = numberOfCodeReps,
        percentGdMinimum = percentGdMinimum,
        errorVelocityMaximum = errorVelocityMaximum,
        coordTransform = coordTransform,
        originalCoordinate = originalCoordinate,
        tiltUsed = tiltUsed,
        threeBeamUsed = threeBeamUsed,
        binMappingUsed = binMappingUsed,
        headingAlignment = headingAlignment,
        headingBias = headingBias,
        sensorSource = sensorSource,
        sensorsAvailable = sensorsAvailable,
        bin1Distance = bin1Distance,
        xmitPulseLength = xmitPulseLength,
        wpRefLayerAverage = wpRefLayerAverage,
        falseTargetThresh = falseTargetThresh,
        transmitLagDistance = transmitLagDistance,
        cpuBoardSerialNumber = cpuBoardSerialNumber,
        systemBandwidth = systemBandwidth,
        serialNumber = serialNumber,
        haveBinaryFixedAttitudeHeader = any(codes[, 1] == 0x00 & codes[, 2] == 0x30),
        haveActualData = haveActualData
    )
    oceDebug(debug, "END decodeHeaderRDI()\n", unindent = 1)
    res
} # decodeHeaderRDI


#' Read an adp File in Teledyne/RDI Format
#'
#' Read a Teledyne/RDI ADCP file (called 'adp' in oce). This can
#' handle a variety of file/instrument types, by recognizing telltale
#' byte sequences in the data. The scope is limited to types that are
#' documented adequately in Teledyne/RDI manuals. In some instances,
#' the manuals provide some information but not enough to enable
#' inclusion here, for example in the case for wave data (see
#' \url{https://github.com/dankelley/oce/issues/2216}).
#'
#' If a heading bias had been set with the `EB` command during the
#' setup for the deployment, then a heading bias will have been stored
#' in the file's header.  This value is stored in the object's
#' metadata as `metadata$heading.bias`.  **Importantly**, this value
#' is subtracted from the headings stored in the file, and the result
#' of this subtraction is stored in the objects heading value (in
#' `data$heading`). It should be noted that `read.adp.rdi()` was
#' tested for firmware version 16.30.  For other versions, there may
#' be problems.  For example, the serial number is not recognized
#' properly for version 16.28.
#'
#' In Teledyne/RDI ADP data files, velocities are coded to signed
#' 2-byte integers, with a scale factor being used to convert to
#' velocity in metres per second.  These two facts control the maximum
#' recordable velocity and the velocity resolution, values that may be
#' retrieved for an ADP object name `d` with `d[["velocityMaximum"]]`
#' and `d[["velocityResolution"]]`.
#'
#' @section Handling of old file formats:
#'
#' Early PD0 file formats stored the year of sampling with a different
#' base year than that used in modern files.  To accommodate this,
#' `read.adp.rdi` examines the inferred year, and if it is greater
#' than 2050, then 100 years are subtracted from the time. This offset
#' was inferred by tests with sample files, but *not* from RDI
#' documentation, so it is somewhat risky.  If the authors can find
#' RDI documentation that indicates the condition in which this
#' century offset is required, then a change will be made to the code.
#' Even if not, the method should not cause problems for a long time.
#'
#' @template adpTemplate
#'
#' @param type character string indicating the type of instrument.
#'
#' @param which optional character value.  If this is `"??"` then the
#' only other parameters that are examined are `file` and `debug`,
#' [read.adp.rdi()] works by locating the indices in `file` at which
#' data segments begin, and storing them as `index` in a list that is
#' returned. The other entry of the list is `time`, the time of the
#' observation.
#'
#' @template encodingIgnoredTemplate
#'
#' @param despike if `TRUE`, [despike()] will be used to clean
#' anomalous spikes in heading, etc.
#'
#' @param testing logical value (IGNORED).
#'
#' @section Names of items in data slot:
#'
#' The names of items in the `data` slot are below. Not all items are
#' present for ll file varieties; use e.g. `names(d[["data"]])` to
#' determine the names used in an object named `d`. In this list,
#' items are either a vector (with one sample per time of
#' measurement), a [matrix] with first index for time and second for
#' bin number, or an [array] with first index for time, second for bin
#' number, and third for beam number. Items are of vector type, unless
#' otherwise indicated.
#'
#' \tabular{rr}{
#' **Item** \tab **Meaning** \cr
#' `a`                                    \tab signal amplitude array (units?)\cr
#' `ambientTemp`                          \tab ambient temperature (degC)\cr
#' `attitude`                             \tab attitude (deg)\cr
#' `attitudeTemp`                         \tab (FIXME add a description here)\cr
#' `avgMagnitudeVelocityEast`             \tab (FIXME add a description here)\cr
#' `avgMagnitudeVelocityNorth`            \tab (FIXME add a description here)\cr
#' `avgSpeed`                             \tab (FIXME add a description here)\cr
#' `avgTrackMagnetic`                     \tab (FIXME add a description here)\cr
#' `avgTrackTrue`                         \tab (FIXME add a description here)\cr
#' `avgTrueVelocityEast`                  \tab (FIXME add a description here)\cr
#' `avgTrueVelocityNorth`                 \tab (FIXME add a description here)\cr
#' `br`                                   \tab bottom range matrix (m)\cr
#' `bv`                                   \tab bottom velocity matrix (m/s)\cr
#' `contaminationSensor`                  \tab (FIXME add a description here)\cr
#' `depth`                                \tab depth (m)\cr
#' `directionMadeGood`                    \tab (FIXME add a description here)\cr
#' `distance`                             \tab (FIXME add a description here)\cr
#' `firstLatitude`                        \tab latitude at start of profile (deg)\cr
#' `firstLongitude`                       \tab longitude at start of profile (deg)\cr
#' `firstTime`                            \tab (FIXME add a description here)\cr
#' `g`                                    \tab data goodness matrix (units?)\cr
#' `heading`                              \tab instrument heading (degrees)\cr
#' `headingStd`                           \tab instrument heading std-dev (deg)\cr
#' `lastLatitude`                         \tab latitude at end of profile (deg)\cr
#' `lastLongitude`                        \tab longitude at end of profile (deg)\cr
#' `lastTime`                             \tab (FIXME add a description here)\cr
#' `numberOfHeadingSamplesAveraged`       \tab (FIXME add a description here)\cr
#' `numberOfMagneticTrackSamplesAveraged` \tab (FIXME add a description here)\cr
#' `numberOfPitchRollSamplesAveraged`     \tab (FIXME add a description here)\cr
#' `numberOfSpeedSamplesAveraged`         \tab (FIXME add a description here)\cr
#' `numberOfTrueTrackSamplesAveraged`     \tab (FIXME add a description here)\cr
#' `pitch`                                \tab instrument pitch (deg)\cr
#' `pitchStd`                             \tab instrument pitch std-dev (deg)\cr
#' `pressure`                             \tab pressure (dbar)\cr
#' `pressureMinus`                        \tab (FIXME add a description here)\cr
#' `pressurePlus`                         \tab (FIXME add a description here)\cr
#' `pressureStd`                          \tab pressure std-dev (dbar)\cr
#' `primaryFlags`                         \tab (FIXME add a description here)\cr
#' `q`                                    \tab data quality array\cr
#' `roll`                                 \tab instrument roll (deg)\cr
#' `rollStd`                              \tab instrument roll std-dev (deg)\cr
#' `salinity`                             \tab salinity\cr
#' `shipHeading`                          \tab ship heading (deg)\cr
#' `shipPitch`                            \tab ship pitch (deg)\cr
#' `shipRoll`                             \tab ship roll (deg)\cr
#' `soundSpeed`                           \tab sound speed (m/s)\cr
#' `speedMadeGood`                        \tab speed over ground (?) (m/s)\cr
#' `speedMadeGoodEast`                    \tab (FIXME add a description here)\cr
#' `speedMadeGoodNorth`                   \tab (FIXME add a description here)\cr
#' `temperature`                          \tab temperature (degC)\cr
#' `time`                                 \tab profile time (POSIXct)\cr
#' `v`                                    \tab velocity array (m/s)\cr
#' `xmitCurrent`                          \tab transmit current (unit?)\cr
#' `xmitVoltage`                          \tab transmit voltage\cr
#' }
#'
#' @section Memory considerations:
#'
#' For `RDI` files only, and only in the case where `by` is not
#' specified, an attempt is made to avoid running out of memory by
#' skipping some profiles in large input files. This only applies if
#' `from` and `to` are both integers; if they are times, none of the
#' rest of this section applies.
#'
#' A key issue is that RDI files store velocities in 2-byte values,
#' which is not a format that R supports. These velocities become
#' 8-byte (numeric) values in R. Thus, the R object created by
#' `read.adp.rdi` will require more memory than that of the data file.
#' A scale factor can be estimated by ignoring vector quantities (e.g.
#' time, which has just one value per profile) and concentrating on
#' matrix properties such as velocity, backscatter, and correlation.
#' These three elements have equal dimensions. Thus, each 4-byte slide
#' in the data file (2 bytes + 1 byte + 1 byte) corresponds to 10
#' bytes in the object (8 bytes + 1 byte + 1 byte). Rounding up the
#' resultant 10/4 to 3 for safety, we conclude that any limit on the
#' size of the R object corresponds to a 3X smaller limit on file
#' size.
#'
#' Various things can limit the size of objects in R, but a strong
#' upper limit is set by the space the operating system provides to R.
#' The least-performant machines in typical use appear to be
#' Microsoft-Windows systems, which limit R objects to about 2e6 bytes
#' (see `?Memory-limits`).  Since R routinely duplicates objects for
#' certain tasks (e.g. for call-by-value in function evaluation),
#' `read.adp.rdi` uses a safety factor in its calculation of when to
#' auto-decimate a file. This factor is set to 3, based partly on the
#' developers' experience with datasets in their possession.
#' Multiplied by the previously stated safety factor of 3, this
#' suggests that the 2 GB limit on R objects corresponds to
#' approximately a 222 MB limit on file size. In the present version
#' of `read.adp.rdi`, this value is lowered to 200 MB for simplicity.
#' Larger files are considered to be "big", and are decimated unless
#' the user supplies a value for the `by` argument.
#'

#' The decimation procedure has two cases.
#'
#' 1. If `from=1` and `to=0` (or if neither `from` or `to` is given),
#' then the intention is to process the full span of the data.  If the
#' input file is under 200 MB, then `by` defaults to 1, so that all
#' profiles are read. For larger files, `by` is set to the [ceiling()]
#' of the ratio of input file size to 200 MB.
#'
#' 2. If `from` exceeds 1, and/or `to` is nonzero, then the intention
#' is to process only an interior subset of the file. In this case,
#' `by` is calculated as the [ceiling()] of the ratio of
#' `bbp*(1+to-from)` to 200 MB, where `bbp` is the number of file
#' bytes per profile. Of course, `by` is set to 1, if this ratio is
#' less than 1.
#'
#' If the result of these calculations is that `by` exceeds 1, then
#' messages are printed to alert the user that the file will be
#' decimated, and also `monitor` is set to `TRUE`, so that a textual
#' progress bar is shown (if the session is interactive).
#'
#' @author Dan Kelley and Clark Richards
#'
#' @examples
#' adp <- read.adp.rdi(system.file("extdata", "adp_rdi.000", package = "oce"))
#' summary(adp)
#'
#' @references
#'
#' 1. Teledyne-RDI, 2007. *WorkHorse commands and output data
#' format.* P/N 957-6156-00 (November 2007).  (Section 5.3 h details the binary
#' format, e.g. the file should start with the byte `0x7f` repeated twice,
#' and each profile starts with the bytes `0x80`, followed by `0x00`,
#' followed by the sequence number of the profile, represented as a
#' little-endian two-byte short integer.  `read.adp.rdi` uses these
#' sequences to interpret data files.)
#'
#' 2. Teledyne RD Instruments, 2015. *V Series monitor, sentinel Output Data Format.*
#' P/N 95D-6022-00 (May 2015). `SV_ODF_May15.pdf`
#'
#' 3. Teledyne RD Instruments, 2014. *Ocean Surveyor / Ocean Observer Technical Manual.*
#' P/N 95A-6012-00 (April 2014). `OS_TM_Apr14.pdf`
#'
#' 4. Teledyne RD Instruments, 2001. *WinRiver User's Guide International Version.*
#' P/N 957-6171-00 (June 2001) `WinRiver User Guide International Version.pdf.pdf`
#'
#' @section Development Notes:
#'
#' An important part of the work of this function is to recognize what
#' will be called "data chunks" by two-byte ID sequences. This
#' function is developed in a practical way, with emphasis being
#' focussed on data files in the possession of the developers. Since
#' Teledyne-RDI tends to introduce new ID codes with new instruments,
#' that means that `read.adp.rdi` may not work on recently-developed
#' instruments.
#'
#' The following two-byte ID codes are recognized by `read.adp.rdi`
#' at this time (with bytes listed in natural order, LSB byte before
#' MSB). Items preceded by an asterisk are recognized, but not handled,
#' and so produce a warning.
#' \tabular{rrrr}{
#'   \tab **Byte 1** \tab **Byte 2** \tab **Meaning**\cr
#'   \tab 0x00 \tab 0x01 \tab velocity\cr
#'   \tab 0x00 \tab 0x01 \tab velocity\cr
#'   \tab 0x00 \tab 0x02 \tab correlation\cr
#'   \tab 0x00 \tab 0x03 \tab echo intensity\cr
#'   \tab 0x00 \tab 0x04 \tab percent good\cr
#'   \tab 0x00 \tab 0x06 \tab bottom track\cr
#'   \tab 0x00 \tab 0x0a \tab Sentinel vertical beam velocity\cr
#'   \tab 0x00 \tab 0x0b \tab Sentinel vertical beam correlation\cr
#'   \tab 0x00 \tab 0x0c \tab Sentinel vertical beam amplitude\cr
#'   \tab 0x00 \tab 0x0d \tab Sentinel vertical beam percent good\cr
#'   \tab 0x00 \tab 0x20 \tab VMDASS\cr
#'   \tab 0x00 \tab 0x30 \tab Binary Fixed Attitude header\cr
#'   \tab 0x00 \tab 0x32 \tab Sentinel transformation matrix\cr
#'   \tab 0x00 \tab 0x0a \tab Sentinel data\cr
#'   \tab 0x00 \tab 0x0b \tab Sentinel correlation\cr
#'   \tab 0x00 \tab 0x0c \tab Sentinel amplitude\cr
#'   \tab 0x00 \tab 0x0d \tab Sentinel percent good\cr
#'   \tab 0x01 \tab 0x0f \tab ?? something to do with V series and 4-beam\cr
#' }
#'
#' Lacking a comprehensive Teledyne-RDI listing of ID codes,
#' the authors have cobbled together a listing from documents to which they
#' have access, as follows.
#'
#' * Table 33 of reference 3 lists codes as follows:
#' \tabular{rrr}{
#' **Standard ID**  \tab  **Standard plus 1D** \tab **DESCRIPTION**\cr
#'  MSB    LSB  \tab        MSB    LSB\tab\cr
#'  ---    ---  \tab        ---    ---\tab\cr
#'   7F     7F  \tab         7F     7F \tab   Header\cr
#'   00     00  \tab         00     01 \tab   Fixed Leader\cr
#'   00     80  \tab         00     81 \tab   Variable Leader\cr
#'   01     00  \tab         01     01 \tab   Velocity Profile Data\cr
#'   02     00  \tab         02     01 \tab   Correlation Profile Data\cr
#'   03     00  \tab         03     01 \tab   Echo Intensity Profile Data\cr
#'   04     00  \tab         04     01 \tab   Percent Good Profile Data\cr
#'   05     00  \tab         05     01 \tab   Status Profile Data\cr
#'   06     00  \tab         06     01 \tab   Bottom Track Data\cr
#'   20     00  \tab         20     00 \tab   Navigation\cr
#'   30     00  \tab         30     00 \tab   Binary Fixed Attitude\cr
#'   30  40-F0  \tab         30  40-F0 \tab   Binary Variable Attitude\cr
#' }
#'
#' * Table 6 on p90 of reference 4 lists "Fixed Leader Navigation" ID
#' codes (none of which are handled by `read.adp.rdi` yet)
#' as follows (the format is reproduced literally; note that
#' e.g. 0x2100 is 0x00,0x21 in the oce notation):
#' \tabular{rr}{
#' **ID** \tab **Description**\cr
#' 0x2100 \tab  $xxDBT\cr
#' 0x2101 \tab  $xxGGA\cr
#' 0x2102 \tab  $xxVTG\cr
#' 0x2103 \tab  $xxGSA\cr
#' 0x2104 \tab  $xxHDT, $xxHGD or $PRDID\cr
#' }
#' and following pages in that manual reveal the following meanings
#' \tabular{rr}{
#' **Symbol** \tab **Meaning**\cr
#' `DBT`   \tab depth below transducer\cr
#' `GGA`   \tab global positioning system\cr
#' `VTA`   \tab track made good and ground speed\cr
#' `GSA`   \tab GPS DOP and active satellites\cr
#' `HDT`   \tab heading, true\cr
#' `HDG`   \tab heading, deviation, and variation\cr
#' `PRDID` \tab heading, pitch and roll\cr
#' }
#'
#' @section Error recovery:
#'
#' Files can sometimes be corrupted, and `read.adp.rdi` has ways to
#' handle two types of error that have been noticed in files supplied
#' by users.
#'
#' 1. There are two bytes within each ensemble that indicate the
#' number of bytes to check within that ensemble, to get the checksum.
#' Sometimes, those two bytes can be erroneous, so that the wrong
#' number of bytes are checked, leading to a failed checksum. As a
#' preventative measure, `read.adp.rdi` checks the stated ensemble
#' length, whenever it detects a failed checksum. If that length
#' agrees with the length of the most recent ensemble that had a good
#' checksum, then the ensemble is declared as faulty and is ignored.
#' However, if the length differs from that of the most recent
#' accepted ensemble, then `read.adp.rdi` goes back to just after the
#' start of the ensemble, and searches forward for the next two-byte
#' pair, namely `0x7f 0x7f`, that designates the start of an ensemble.
#' Distinct notifications are given about these two cases, and they
#' give the byte numbers in the original file, as a way to help
#' analysts who want to look at the data stream with other tools.
#'
#' 2. At the end of an ensemble, the next two characters ought to be
#' `0x7f 0x7f`, and if they are not, then the next ensemble is faulty.
#' If this error occurs, `read.adp.rdi` attempts to recover by
#' searching forward to the next instance of this two-byte pair,
#' discarding any information that is present in the mangled ensemble.
#'
#' In each of these cases, warnings are printed about ensembles that
#' seem problematic. Advanced users who want to diagnose the problem
#' further might find it helpful to examine the original data file
#' using other tools. To this end, `read.adp.rdi` inserts an element
#' named `ensembleInFile` into the `metadata` slot. This gives the
#' starting byte number of each inferred ensemble within the original
#' data file.  For example, if `d` is an object read with
#' `read.adp.rdi`, then using
#'
#' ```
#' plot(d[["time"]][-1], diff(d[["ensembleInFile"]]))
#' ```
#' can be a good way to narrow in on problems.
#'
#' @section Changes:
#' * The `bq` (bottom-track quality) field was called `bc` until 2023-02-09.
#' See https://github.com/dankelley/oce/issues/2039 for discussion.
#'
#' @template adReadingMethodTemplate
#'
#' @family things related to adp data
#' @family functions that read adp data
read.adp.rdi <- function(
    file, from, to, by, tz = getOption("oceTz"),
    longitude = NA, latitude = NA, type = c("workhorse"), which, encoding = NA,
    monitor = FALSE, despike = FALSE, processingLog, testing = FALSE,
    debug = getOption("oceDebug"), ...) {
    oceDebug(debug, "read.adp.rdi() START\n", unindent = 1)
    byte1 <- as.raw(0x7f)
    byte2 <- as.raw(0x7f)
    #<<>> byte2 <- as.raw(0x79)
    if (missing(file)) {
        stop("must supply 'file'")
    }
    if (is.character(file)) {
        if (!file.exists(file)) {
            stop("cannot find file \"", file, "\"")
        }
        if (0L == file.info(file)$size) {
            stop("empty file \"", file, "\"")
        }
    }
    fileSize <- file.info(file)$size
    oceDebug(debug, "fileSize=", fileSize, "\n")
    largeFile <- fileSize >= .Machine$integer.max
    if (largeFile) {
        warning("This file is large; please report any errors you find\n")
    }
    if (!interactive()) {
        monitor <- FALSE
    }
    warningUnknownCode <- list()
    fromGiven <- !missing(from) # FIXME document THIS
    toGiven <- !missing(to) # FIXME document THIS
    byGiven <- !missing(by) # FIXME document THIS
    oceDebug(debug, "read.adp.rdi(\"", file, "\"",
        ", from=", if (fromGiven) format(from) else "(missing)",
        ", to=", if (toGiven) format(to) else "(missing)",
        ", by=", if (byGiven) format(by) else "(missing)",
        "...) START\n",
        unindent = 1, sep = ""
    )
    if (!fromGiven) {
        from <- 1
    }
    if (!byGiven) {
        by <- 1
    }
    if (!toGiven) {
        to <- 0
    }
    profileStart <- NULL # prevent scope warning from rstudio; defined later anyway
    if (is.character(file)) {
        filename <- fullFilename(file)
        file <- file(file, "rb")
        on.exit(close(file))
    }
    if (!inherits(file, "connection")) {
        stop("argument `file' must be a character string or connection")
    }
    if (!isOpen(file)) {
        filename <- "(connection)"
        open(file, "rb")
        on.exit(close(file))
    }
    type <- match.arg(type)
    # Determine file size
    seek(file, 0, "start")
    seek(file, where = 0, origin = "end")
    fileSize <- seek(file, where = 0)
    if (fileSize < 1) {
        stop("empty file \"", file, "\"")
    }
    startIndex <- 1L # index of byte pair 0x7f 0x7f
    # 2024-02-19: examine 200,000 bytes.  This was 2,000 in the early
    # days of this code, but that switched to 10,000 bytes on 2017-01-07.
    # I don't really want to check the whole file (because of time it
    # might take) but potentially we'll have to do that.
    NCHECK <- min(fileSize, 200000L)
    buf <- readBin(file, what = "raw", n = NCHECK, endian = "little")
    if (buf[1] != byte1 || buf[2] != byte2) {
        message(
            "Expecting file to start with bytes 0x", byte1, " and 0x", byte2, ", but got 0x",
            buf[1], " and 0x", buf[2], ", so will try skipping ahead."
        )
        startIndex <- matchBytes(buf, byte1, byte2)[1] # FIXME: what if file is large?
        if (0 == length(startIndex) || is.na(startIndex)) {
            stop(
                "cannot find a 0x", byte1, " 0x", byte2, " byte sequence in the first ",
                NCHECK, " bytes of this file"
            )
        }
        message(
            "This file does not start with a 0x", byte1, " 0x", byte2,
            " byte sequence, so skipped to index ", startIndex,
            ", where this sequence is first found."
        )
        buf <- buf[seq(startIndex, length(buf))]
    }
    header <- decodeHeaderRDI(buf, debug = debug - 1)
    oceDebug(debug, "have Binary Fixed Attitude Header: ", header$haveBinaryFixedAttitudeHeader, "\n")
    if (header$haveActualData) {
        numberOfBeams <- header$numberOfBeams
        numberOfCells <- header$numberOfCells
        bin1Distance <- header$bin1Distance
        xmitPulseLength <- header$xmitPulseLength
        cellSize <- header$cellSize
        # message("1. isSentinel=", isSentinel)
        isSentinel <- header$instrumentSubtype == "sentinelV"
        oceDebug(debug, "isSentinel=", isSentinel, " near adp.rdi.R line 829\n")
        if (is.numeric(from) && is.numeric(to) && is.numeric(by)) {
            # check for large files
            byteMax <- 200e6 # for reasoning, see the help file
            if (!byGiven) {
                if (to == 0) { # whole file
                    by <- if (fileSize < byteMax) 1L else fileSize / byteMax
                } else {
                    byteEstimate <- header$bytesPerEnsemble * (to - from)
                    by <- if (byteEstimate < byteMax) 1L else byteEstimate / byteMax
                }
                by <- max(1L, as.integer(by))
                if (by > 1) {
                    warning("setting by=", by, " for a large RDI file\n")
                    if (!monitor && interactive()) {
                        warning("setting monitor=TRUE for a large RDI file\n")
                        monitor <- TRUE
                    }
                }
            }
            oceDebug(debug, "calling ldc_rdi_in_file() w/ numeric values of from etc\n")
            tC <- system.time({
                ldc <- do_ldc_rdi_in_file(filename = filename, from = from, to = to, by = by, startIndex = startIndex, mode = 0L, debug = debug - 1)
            })
            tCpp <- system.time({
                ldcNew <- do_ldc_rdi_in_file_new(filename = filename, from = from, to = to, by = by, startIndex = startIndex, mode = 0L, debug = debug - 1)
            })
            oceDebug(debug, sprintf(
                "ensemble detection in old (C) code took %fs (user), %fs (system), %fs (total)\n",
                tC[1], tC[2], tC[3]
            ))
            oceDebug(debug, sprintf(
                "ensemble detection in new (C++) code took %fs (user), %fs (system), %fs (total)\n",
                tCpp[1], tCpp[2], tCpp[3]
            ))
            if (!identical(ldc, ldcNew)) {
                message("IMPORTANT: read.adp.rdi/ldc (numeric from, to) problem -- please report at github.com/dankelley/oce/issues")
                warning("IMPORTANT: read.adp.rdi/ldc (numeric from, to) problem -- please report at github.com/dankelley/oce/issues")
                # browser()
            }
            oceDebug(debug, "old (C) and new (C++) methods agree on ensemble-detection results\n")
            rm(ldcNew)
            oceDebug(debug, "completed ensemble detection with numeric from, by, to values\n")
        } else {
            if (is.character(from)) {
                from <- as.POSIXct(from, tz = "UTC")
            }
            if (is.character(to)) {
                to <- as.POSIXct(to, tz = "UTC")
            }
            if (is.character(by)) {
                by <- ctimeToSeconds(by)
            }
            oceDebug(debug, "calling ldc_rdi_in_file() w/ POSIXt values of from etc\n")
            tC <- system.time({
                ldc <- do_ldc_rdi_in_file(filename = filename, from = from, to = to, by = by, startIndex = startIndex, mode = 1L, debug = debug - 1)
            })
            tCpp <- system.time({
                ldcNew <- do_ldc_rdi_in_file_new(filename = filename, from = from, to = to, by = by, startIndex = startIndex, mode = 1L, debug = debug - 1)
            })
            oceDebug(debug, sprintf(
                "ensemble detection in old (C) code took %fs (user), %fs (system), %fs (total)\n",
                tC[1], tC[2], tC[3]
            ))
            oceDebug(debug, sprintf(
                "ensemble detection in new (C++) code took %fs (user), %fs (system), %fs (total)\n",
                tCpp[1], tCpp[2], tCpp[3]
            ))
            if (!identical(ldc, ldcNew)) {
                message("IMPORTANT: read.adp.rdi/ldc (non-numeric from, to) problem -- please report at github.com/dankelley/oce/issues")
                warning("IMPORTANT: read.adp.rdi/ldc (non-numeric from, to) problem -- please report at github.com/dankelley/oce/issues")
            }
            oceDebug(debug, "old (C) and new (C++) methods agree on ensemble-detection results\n")
            rm(ldcNew)
            oceDebug(debug, "completed ensemble detection with non-numeric from, by, to values\n")
        }
        if (!missing(which)) {
            if (which[1] == "??") {
                oceDebug(debug, "handling which=\"??\"\n")
                return(list(index = ldc$ensembleStart, time = numberAsPOSIXct(ldc$time)))
            } else {
                stop("read.adp.rdi() cannot handle which=\"?\"")
            }
        }
        ensembleStart <- ldc$ensembleStart
        if (largeFile) {
            ensembleStart <- ldc$ensembleStart64
        }
        buf <- ldc$buf # FIXME: this doubles memory pressure for no good reason
        bufSize <- length(buf)
        time <- as.POSIXct(ldc$time + 0.01 * as.numeric(ldc$sec100), origin = "1970-01-01")
        oceDebug(debug, "next is str(ldc).  I think we can shrink this after extracting things\n")
        if (debug > 0) {
            print(str(ldc))
        }
        # Now, 'buf' contains *only* the profiles we want, so we may
        # redefine 'from', 'to' and 'by' to specify each and every profile.
        from <- 1
        to <- length(ensembleStart)
        by <- 1
        oceDebug(debug, "setting from=", from, ", by=", by, ", to=", to, "\n")
        if (isSentinel) {
            oceDebug(debug, "skipping first ensemble (trial ad-hoc measure for SentinelV files)\n")
            warning("skipping the first ensemble (a temporary solution that eases reading of SentinelV files)\n")
            ensembleStart <- ensembleStart[-1] # remove the first ensemble to simplify parsing
            # ensembleStart64 <- ensembleStart64[-1] # remove the first ensemble to simplify parsing
            to <- to - 1
            header$numberOfDataTypes <- readBin(buf[ensembleStart[1] + 5], "integer", n = 1, size = 1)
            header$dataOffset <- readBin(buf[ensembleStart[1] + 6 + 0:(2 * header$numberOfDataTypes)],
                "integer",
                n = header$numberOfDataTypes, size = 2, endian = "little", signed = FALSE
            )
            oceDebug(
                debug, "header$dataOffset=",
                paste(header$dataOffset, collapse = " "),
                " (reread because a sentinelV file)\n"
            )
        }
        # Profiles start at the VARIABLE LEADER DATA, since there is no point in
        # re-interpreting the HEADER and the FIXED LEADER DATA over and over,
        # but we need the VLD to get times, etc. I *think* we can assume a fixed
        # location for these, based on the "Always Output" indication in Fig 46
        # on page 145 of teledyne2014ostm.
        profileStart <- ensembleStart + as.numeric(buf[ensembleStart[1] + 8]) + 256 * as.numeric(buf[ensembleStart[1] + 9])
        oceDebug(debug, "check whether file is large FIXME: delete\n")
        if (any(profileStart < 1)) {
            cat("Next is table(profileStart<1):\n")
            print(table(profileStart < 1))
            cat("range(profileStart) = ", paste(range(profileStart), collapse = " to "), "\n")
            firstBad <- base::which(profileStart < 1)[1]
            cat("first bad profileStart (value < 1) is at index firstBad=", firstBad, "\n")
            cat("profileStart[firstBad]=", profileStart[firstBad], "\n", sep = "")
            cat("ensembleStart[firstBad]=", ensembleStart[firstBad], "\n", sep = "")
            if (debug > 0) { # FIXME: remove when largeFile is supported
                # x11()
                # par(mfrow = c(3, 1))
                # plot(ensembleStart < 1, type = "s", xlab = "index")
                # mtext("is ensembleStart<1?", adj = 0)
                # abline(v = firstBad, col = 2, lty = 2)
                # mtext(firstBad, at = firstBad, col = 2)

                # plot(ensembleStart64 == ensembleStart, type = "s", xlab = "index")
                # mtext("does ensembleStart==ensembleStart64?", adj = 0)
                # abline(v = firstBad, col = 2, lty = 2)
                # mtext(firstBad, at = firstBad, col = 2)

                # plot(diff(ensembleStart64), type = "s", xlab = "index")
                # mtext("diff(ensembleStart64)", adj = 0)
                # abline(v = firstBad, col = 2, lty = 2)
                # mtext(firstBad, at = firstBad, col = 2)
            }
            # ensembleStart <- ensembleStart64
            # profileStart <- ensembleStart + as.numeric(buf[ensembleStart[1] + 8]) + 256 * as.numeric(buf[ensembleStart[1] + 9])
            warning("Caution: using provisional support for large RDI file; if problems arise, chop up your file\n")
        }
        # offset for data type 1 (velocity)
        oceDebug(debug, vectorShow(profileStart, "profileStart before trimming"))
        profilesInFile <- length(profileStart)
        oceDebug(debug, "profilesInFile=", profilesInFile, "\n")
        if (profilesInFile > 0) {
            profilesToRead <- length(profileStart)
            oceDebug(debug, "filename: \"", filename, "\"\n")
            oceDebug(debug, "profilesToRead:", profilesToRead, "\n")
            oceDebug(debug, "numberOfBeams:", numberOfBeams, "\n")
            oceDebug(debug, "numberOfCells:", numberOfCells, "\n")
            items <- numberOfBeams * numberOfCells
            codes <- header$codes
            oceDebug(debug, "codes[,1]=", paste("0x", paste(codes[, 1], sep = ""), collapse = " "), "\n")
            oceDebug(debug, "codes[,2]=", paste("0x", paste(codes[, 2], sep = ""), collapse = " "), "\n")
            oceDebug(debug, "NOTE: only the following codes are recognized. See e.g. Table 45 of the\n")
            oceDebug(debug, "Teledyn-RDI document entitled `Ocean Surveyor Technical Manual_Dec20.pdf`,\n")
            oceDebug(debug, "P/N 95A-6012-00 (December 2020)\n")
            # the comments indicate the variable names used below
            oceDebug(debug, "  0x00 0x01 velocity\n") # vFound
            oceDebug(debug, "  0x00 0x02 correlation\n") # qfound
            oceDebug(debug, "  0x00 0x03 echo intensity\n") # aFoiund
            oceDebug(debug, "  0x00 0x04 percent good\n") # gFound
            oceDebug(debug, "  0x00 0x06 bottom track\n") # bFound; br, bv, bq, ba, bg
            oceDebug(debug, "  0x00 0x0a Sentinel vertical beam velocity\n") # vv
            oceDebug(debug, "  0x00 0x0b Sentinel vertical beam correlation\n") # vv
            oceDebug(debug, "  0x00 0x0c Sentinel vertical beam amplitude\n") # vv
            oceDebug(debug, "  0x00 0x0d Sentinel vertical beam percent good\n") # vv
            oceDebug(debug, "  0x00 0x20 VMDASS\n") # VMDASSStorageInitialized; many other variables
            oceDebug(debug, "  0x00 0x30 Variable Fixed Attitude header\n")
            oceDebug(debug, "  0x00 0x32 Sentinel transformation matrix\n") # tmFound
            oceDebug(debug, "  0x00 0x0a Sentinel data\n") # vvFound
            oceDebug(debug, "  0x00 0x0b Sentinel correlation\n") # vqFound
            oceDebug(debug, "  0x00 0x0c Sentinel amplitude\n") # vaFound
            oceDebug(debug, "  0x00 0x0d Sentinel percent good\n") # vgFoiund
            oceDebug(debug, "  0x01 0x0f ? something to do with V series and 4-beam\n")
            # oceDebug(debug, "buf[1:10] near line 745: ", paste("0x", paste(buf[1:10], sep=" "), sep=""), "\n")
            vFound <- sum(codes[, 1] == 0x00 & codes[, 2] == 0x01) # velo
            qFound <- sum(codes[, 1] == 0x00 & codes[, 2] == 0x02) # corr
            aFound <- sum(codes[, 1] == 0x00 & codes[, 2] == 0x03) # echo intensity
            gFound <- sum(codes[, 1] == 0x00 & codes[, 2] == 0x04) # percent good
            bFound <- sum(codes[, 1] == 0x00 & codes[, 2] == 0x06) # bottom-track
            # next is commented out because we are not (yet) using the
            # information
            # nFound <- sum(codes[, 1]==0x00 & codes[, 2]==0x20) # navigation
            tmFound <- sum(codes[, 1] == 0x00 & codes[, 2] == 0x32) # transformation matrix
            if (vFound) {
                v <- array(numeric(), dim = c(profilesToRead, numberOfCells, numberOfBeams))
                oceDebug(
                    debug, "set up 'v' (velocity) storage for", profilesToRead, "profiles,",
                    numberOfCells, "cells, and", numberOfBeams, "beams\n"
                )
            } else {
                v <- NULL
            }
            if (qFound) {
                q <- array(raw(), dim = c(profilesToRead, numberOfCells, numberOfBeams))
                oceDebug(
                    debug, "set up 'q' (correlation) storage for", profilesToRead, "profiles,",
                    numberOfCells, "cells, and", numberOfBeams, "beams\n"
                )
            } else {
                q <- NULL
            }
            if (aFound) {
                a <- array(raw(), dim = c(profilesToRead, numberOfCells, numberOfBeams))
                oceDebug(
                    debug, "set up 'a' (echo intensity) storage for", profilesToRead, "profiles,",
                    numberOfCells, "cells, and", numberOfBeams, "beams\n"
                )
            } else {
                a <- NULL
            }
            if (gFound) {
                g <- array(raw(), dim = c(profilesToRead, numberOfCells, numberOfBeams))
                oceDebug(
                    debug, "set up 'g' (percent good) storage for", profilesToRead, "profiles,",
                    numberOfCells, "cells, and", numberOfBeams, "beams\n"
                )
            } else {
                g <- NULL
            }
            # Read Binary Fixed Attitude Header (signalled by code 0x00 0x30) from first ensemble
            # (skipping others, because I think this is unchangeable, based on the names and the
            # docs), and store in the metadata.
            ii <- base::which(codes[, 1] == 0x00 & codes[, 2] == 0x30)
            if (length(ii)) {
                oceDebug(debug, "As a test, displaying 'Binary Fixed Attitude Header' in first 10 ensembles...\n")
                for (ensemble in seq_len(10)) {
                    J <- ensembleStart[ensemble] + header$dataOffset[ii]
                    oceDebug(debug, "ensembleStart[", ensemble, "]=", ensembleStart[ensemble],
                        ", header$dataOffset[", ii, "]=", header$dataOffset[ii],
                        "; buf[", J, "+1:34]=", paste(buf[J + 1:34], collapse = " "), "\n",
                        sep = ""
                    )
                }
                header$binaryFixedAttitudeHeaderRaw <- buf[J + 1:34]
                warning("provisionally, metadata$binaryFixedAttitudeHeaderRaw holds the 34 raw bytes of the Binary Fixed Attitude header\n")
            }
            ii <- base::which(codes[, 1] == 0x01 & codes[, 2] == 0x0f)
            if (isSentinel && length(ii) < 1) {
                warning("Didn't find V series leader data ID, treating as a 4 beam ADCP\n")
                isSentinel <- FALSE
            }
            if (isSentinel) {
                # Look for sentinel-related codes
                # FIXME: Fields to look for:
                # 0x00 0x70: V series Config
                # 0x01 0x70: V series ping setup
                # 0x02 0x70: V series ADC data
                # 0x04 0x70: V series event log
                # 0x01 0x0f: V beam data leader
                # 0x00 0x0a: V beam data
                # 0x00 0x0b: V beam correlation
                # 0x00 0x0c: V beam amplitude
                # 0x00 0x0d: V beam percent good
                # 0x00 0x32: Transformation Matrix
                tmFound <- sum(codes[, 1] == 0x00 & codes[, 2] == 0x32) # transformation matrix
                vvFound <- sum(codes[, 1] == 0x00 & codes[, 2] == 0x0a) # v beam data
                vqFound <- sum(codes[, 1] == 0x00 & codes[, 2] == 0x0b) # v beam correlation
                vaFound <- sum(codes[, 1] == 0x00 & codes[, 2] == 0x0c) # v beam amplitude
                vgFound <- sum(codes[, 1] == 0x00 & codes[, 2] == 0x0d) # v beam percent good
                # Read the relevant V series metadata
                # remove the first row from codes (7f7f) because it is the header (always has to be there)
                codes <- codes[-1, ]
                # transformation matrix
                if (tmFound) {
                    ii <- base::which(codes[, 1] == 0x00 & codes[, 2] == 0x32)
                    oceDebug(debug, "Reading transformation matrix\n")
                    tmx <- 0.0001 * readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 0:7 + 2], "integer", n = 4, size = 2, signed = TRUE, endian = "little")
                    tmy <- 0.0001 * readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 0:7 + 10], "integer", n = 4, size = 2, signed = TRUE, endian = "little")
                    tmz <- 0.0001 * readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 0:7 + 18], "integer", n = 4, size = 2, signed = TRUE, endian = "little")
                    tme <- 0.0001 * readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 0:7 + 26], "integer", n = 4, size = 2, signed = TRUE, endian = "little")
                    transformationMatrix <- matrix(c(tmx, tmy, tmz, tme), nrow = 4, byrow = TRUE)
                    if (debug > 0) {
                        cat("Transformation matrix:\n")
                        oceDebug(debug, vectorShow(tmx, paste("tmx", sep = "")))
                        oceDebug(debug, vectorShow(tmy, paste("tmy", sep = "")))
                        oceDebug(debug, vectorShow(tmz, paste("tmz", sep = "")))
                        oceDebug(debug, vectorShow(tme, paste("tme", sep = "")))
                    }
                }
                # Read the V beam data leader
                ii <- base::which(codes[, 1] == 0x01 & codes[, 2] == 0x0f)
                oceDebug(debug, "Reading V series data leader\n")
                numberOfVCells <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 2:3], "integer", size = 2, endian = "little")
                verticalPings <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 4:5], "integer", size = 2, endian = "little")
                depthCellSize <- 0.01 * readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 6:7], "integer", size = 2, endian = "little")
                firstCellRange <- 0.01 * readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 8:9], "integer", size = 2, endian = "little")
                verticalMode <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 10:11], "integer", size = 2, endian = "little")
                verticalTransmit <- 0.01 * readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 12:13], "integer", size = 2, endian = "little")
                verticalLagLength <- 0.01 * readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 14:15], "integer", size = 2, endian = "little")
                transmitCodeElements <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 16:17], "integer", size = 2, endian = "little")
                verticalRssiThreshold <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 18:19], "integer", size = 2, endian = "little")
                verticalShallowBin <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 20:21], "integer", size = 2, endian = "little")
                verticalStartBin <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 22:23], "integer", size = 2, endian = "little")
                verticalShallowRssiBin <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 24:25], "integer", size = 2, endian = "little")
                maxCoreThreshold <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 26:27], "integer", size = 2, endian = "little")
                minCoreThreshold <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 28:29], "integer", size = 2, endian = "little")
                pingOffsetTime <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 30:31], "integer", size = 2, endian = "little")
                surfSpare1 <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 32:33], "integer", size = 2, endian = "little")
                depthScreen <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 34:35], "integer", size = 2, endian = "little")
                percentGoodThreshold <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 36:37], "integer", size = 2, endian = "little")
                verticalDOproofing <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 38:39], "integer", size = 2, endian = "little")
                vBeamHeader <- list(
                    numberOfVCells = numberOfVCells,
                    verticalPings = verticalPings,
                    depthCellSize = depthCellSize,
                    firstCellRange = firstCellRange,
                    verticalMode = verticalMode,
                    verticalTransmit = verticalTransmit,
                    verticalLagLength = verticalLagLength,
                    transmitCodeElements = transmitCodeElements,
                    verticalRssiThreshold = verticalRssiThreshold,
                    verticalShallowBin = verticalShallowBin,
                    verticalStartBin = verticalStartBin,
                    verticalShallowRssiBin = verticalShallowRssiBin,
                    maxCoreThreshold = maxCoreThreshold,
                    minCoreThreshold = minCoreThreshold,
                    minCoreThreshold = minCoreThreshold,
                    pingOffsetTime = pingOffsetTime,
                    surfSpare1 = surfSpare1,
                    depthScreen = depthScreen,
                    percentGoodThreshold = percentGoodThreshold,
                    verticalDOproofing = verticalDOproofing
                )
                vItems <- numberOfVCells
                # V series config
                ii <- base::which(codes[, 1] == 0x00 & codes[, 2] == 0x70)
                if (length(ii) < 1) {
                    stop("Didn't find V series Configuration data ID")
                }
                oceDebug(debug, "Reading V series configuration\n")
                firmwareVersionPrimary <- as.numeric(buf[ensembleStart[1] + header$dataOffset[ii] + 2])
                firmwareVersionSecondary <- as.numeric(buf[ensembleStart[1] + header$dataOffset[ii] + 3])
                firmwareVersionBuild <- as.numeric(buf[ensembleStart[1] + header$dataOffset[ii] + 4])
                firmwareVersionService <- as.numeric(buf[ensembleStart[1] + header$dataOffset[ii] + 5])
                firmwareVersion <- paste(firmwareVersionPrimary, firmwareVersionSecondary,
                    firmwareVersionBuild, firmwareVersionService,
                    sep = "."
                )
                oceDebug(debug, "V series firmware version", firmwareVersion, "\n")
                systemFrequency <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 6:9], "integer", endian = "little")
                pressureRating <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 10:11], "integer", size = 2, endian = "little")
                schemaMajor <- as.numeric(buf[ensembleStart[1] + header$dataOffset[ii] + 12])
                schemaMinor <- as.numeric(buf[ensembleStart[1] + header$dataOffset[ii] + 13])
                schemaRev <- as.numeric(buf[ensembleStart[1] + header$dataOffset[ii] + 14])
                vBeamHeader$vSeriesConfig <- list(
                    firmwareVersion = firmwareVersion,
                    systemFrequency = systemFrequency,
                    pressureRating = pressureRating,
                    schemaMajor = schemaMajor,
                    schemaMinor = schemaMinor,
                    schemaRev = schemaRev
                )
                # Read V series ping setup
                ii <- base::which(codes[, 1] == 0x01 & codes[, 2] == 0x70)
                if (length(ii) < 1) {
                    stop("Didn't find V series ping setup data ID")
                }
                oceDebug(debug, "Reading V series ping setup\n")
                ensembleInterval <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 4:7], "integer", endian = "little")
                numberOfPings <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 8:9], "integer", size = 2, endian = "little")
                timeBetweenPings <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 10:13], "integer", endian = "little")
                offsetBetweenPingGroups <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 14:17], "integer", endian = "little")
                pingSequenceNumber <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 22:23], "integer", size = 2, endian = "little")
                ambiquityVelocity <- 0.001 * readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 24:25], "integer", size = 2, endian = "little")
                rxGain <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 26], "integer", size = 1, endian = "little")
                rxBeamMask <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 27], "integer", size = 1, endian = "little")
                txBeamMask <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 28], "integer", size = 1, endian = "little")
                ensembleCount <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 34:35], "integer", size = 2, endian = "little")
                deploymentStartCentury <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 36], "integer", size = 1, endian = "little")
                deploymentStartYear <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 37], "integer", size = 1, endian = "little")
                deploymentStartMonth <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 38], "integer", size = 1, endian = "little")
                deploymentStartDay <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 39], "integer", size = 1, endian = "little")
                deploymentStartHour <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 40], "integer", size = 1, endian = "little")
                deploymentStartMinute <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 41], "integer", size = 1, endian = "little")
                deploymentStartSecond <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 42], "integer", size = 1, endian = "little")
                deploymentStartHundredths <- readBin(buf[ensembleStart[1] + header$dataOffset[ii] + 43], "integer", size = 1, endian = "little")
                deploymentStart <- ISOdatetime(deploymentStartCentury * 100 + deploymentStartYear,
                    deploymentStartMonth, deploymentStartDay, deploymentStartHour,
                    deploymentStartMinute, deploymentStartSecond + deploymentStartHundredths / 100,
                    tz = tz
                )
                vBeamHeader$vSeriesPingSetup <- list(
                    ensembleInterval = ensembleInterval,
                    numberOfPings = numberOfPings,
                    timeBetweenPings = timeBetweenPings,
                    offsetBetweenPingGroups = offsetBetweenPingGroups,
                    pingSequenceNumber = pingSequenceNumber,
                    ambiquityVelocity = ambiquityVelocity,
                    rxGain = rxGain,
                    rxBeamMask = rxBeamMask,
                    txBeamMask = txBeamMask,
                    ensembleCount = ensembleCount,
                    deploymentStart = deploymentStart
                )
                header$vBeamHeader <- vBeamHeader
                # Handle vertical beam, if one exists.  This creates va, vg, vq and vv.
                if (vvFound) {
                    vv <- array(numeric(), dim = c(profilesToRead, numberOfVCells))
                    oceDebug(
                        debug, "set up 'vv' (vertical velocity) storage for", profilesToRead, "profiles, and",
                        numberOfVCells, "cells.\n"
                    )
                } else {
                    vv <- NULL
                }
                if (vaFound) {
                    va <- array(raw(), dim = c(profilesToRead, numberOfVCells))
                    oceDebug(
                        debug, "set up 'va' (vertical backscatter amplitude) storage for", profilesToRead, "profiles, and",
                        numberOfVCells, "cells.\n"
                    )
                } else {
                    va <- NULL
                }
                if (vqFound) {
                    vq <- array(raw(), dim = c(profilesToRead, numberOfVCells))
                    oceDebug(
                        debug, "set up 'vq' (vertical correlation) storage for", profilesToRead, "profiles, and",
                        numberOfVCells, "cells.\n"
                    )
                } else {
                    vq <- NULL
                }
                if (vgFound) {
                    vg <- array(raw(), dim = c(profilesToRead, numberOfVCells))
                    oceDebug(
                        debug, "set up 'vg' (vertical percent good) storage for", profilesToRead, "profiles, and",
                        numberOfVCells, "cells.\n"
                    )
                } else {
                    vg <- NULL
                }
            }
            if (bFound) {
                br <- array(double(), dim = c(profilesToRead, numberOfBeams))
                bv <- array(double(), dim = c(profilesToRead, numberOfBeams))
                # bq called bc until 2023-02-09 https://github.com/dankelley/oce/issues/2039
                bq <- array(double(), dim = c(profilesToRead, numberOfBeams)) # correlation
                ba <- array(double(), dim = c(profilesToRead, numberOfBeams)) # amplitude
                bg <- array(double(), dim = c(profilesToRead, numberOfBeams)) # percent good
                oceDebug(
                    debug, "set up 'br', etc. (bottom data) storage for", profilesToRead, "profiles,",
                    numberOfCells, "cells, and", numberOfBeams, "beams\n"
                )
            } else {
                br <- bv <- bq <- ba <- bg <- NULL
            }
            badProfiles <- NULL
            # haveBottomTrack <- FALSE          # FIXME maybe we can determine this from the header
            oceDebug(debug, "length(profileStart):", length(profileStart), "\n")
            if (profilesToRead < 1) {
                stop("no profilesToRead")
            }
            velocityScale <- 1e-3
            isVMDAS <- FALSE # flag for file type
            badVMDAS <- NULL # erroneous VMDAS profiles
            VMDASStorageInitialized <- FALSE # flag for whether we have VMDAS storage set up yet
            #> We do some things differently based on the firmware.
            # FIXME: the docs say not to assume an order in data chunks but the following works in all
            # FIXME: data seen to date.
            oceDebug(debug, "header$numberOfDataTypes: ", header$numberOfDataTypes, "\n")
            # profilesToShow <- 2 # only if debug>0
            if (monitor) {
                progressBar <- txtProgressBar(max = profilesToRead, style = 3, title = "Reading profiles")
            }
            oceDebug(debug, "profilesToRead=", profilesToRead, "\n")
            unhandled <- list(xxGGA = 0, xxVTA = 0, xxGSA = 0)
            # . unknownWarningCount <- 0
            nmea <- NULL
            nmeaLen <- 0
            orientation <- vector("character", profilesToRead)
            ensembleNumber <- vector("numeric", profilesToRead)
            # cat("** got space for orientation\n")
            for (i in 1:profilesToRead) {
                # update the data descriptions, after realizing, while working on
                # [issue 1401](https://github.com/dankelley/oce/issues/1401), that files
                # can have interlaced data types.
                header$numberOfDataTypes <- readBin(buf[ensembleStart[i] + 5], "integer", n = 1, size = 1)
                header$dataOffset <- readBin(buf[ensembleStart[i] + 6 + seq(0, 2 * header$numberOfDataTypes)],
                    "integer",
                    n = header$numberOfDataTypes, size = 2
                )
                # if (i < 30) {
                #     cat("i=", i, ", header$numberOfDataTypes=", header$numberOfDataTypes,
                #         ", header$dataOffset=", paste(header$dataOffset, collapse=" "), "\n")
                # }
                for (chunk in 1:header$numberOfDataTypes) {
                    o <- ensembleStart[i] + header$dataOffset[chunk]
                    if (buf[o] == 0x00 && buf[1 + o] == 0x00) {
                        orientation[i] <- if ("1" == substr(byteToBinary(buf[o + 4], endian = "big"), 1, 1)) "upward" else "downward"
                    } else if (buf[o] == 0x80 && buf[1 + o] == 0x00) {
                        ensembleNumber[i] <- readBin(buf[o + 2:3], "integer",
                            n = 1, size = 2,
                            endian = "little", signed = TRUE
                        ) + as.integer(buf[o + 11]) * 65535L
                    } else if (buf[o] == 0x00 && buf[1 + o] == 0x01) {
                        vtmp <- readBin(buf[o + 1 + seq(1, 2 * items)], "integer", n = items, size = 2, endian = "little", signed = TRUE)
                        vtmp[-32768 == vtmp] <- NA # blank out bad data
                        v[i, , ] <- matrix(velocityScale * vtmp, ncol = numberOfBeams, byrow = TRUE)
                    } else if (buf[o] == 0x00 && buf[1 + o] == 0x02) {
                        q[i, , ] <- matrix(buf[o + 1 + seq(1, items)], ncol = numberOfBeams, byrow = TRUE)
                    } else if (buf[o] == 0x00 && buf[1 + o] == 0x03) {
                        a[i, , ] <- matrix(buf[o + 1 + seq(1, items)], ncol = numberOfBeams, byrow = TRUE)
                    } else if (buf[o] == 0x00 && buf[1 + o] == 0x04) {
                        g[i, , ] <- matrix(buf[o + 1 + seq(1, items)], ncol = numberOfBeams, byrow = TRUE)
                    } else if (buf[o] == 0x00 && buf[1 + o] == 0x05) {
                        # FIXME: do something here (what is code 0x00 0x05?)
                    } else if (buf[o] == 0x00 && buf[1 + o] == 0x06) {
                        rangeLSB <- readBin(buf[o + c(16:23)], "integer", n = 4, size = 2, signed = FALSE, endian = "little")
                        rangeMSB <- readBin(buf[o + 77:80], "integer", n = 4, size = 1, signed = FALSE, endian = "little")
                        if (is.null(br)) {
                            warning(
                                "cannot store bottom-track data for profile ", i,
                                " because profile 1 lacked such data so no storage was set up\n"
                            )
                        } else {
                            br[i, ] <- 0.01 * (65536 * rangeMSB + rangeLSB)
                            bv[i, ] <- 0.001 * readBin(buf[o + c(24:31)], "integer", n = 4, size = 2, signed = TRUE, endian = "little")
                            bq[i, ] <- as.integer(buf[o + 32:35])
                            ba[i, ] <- as.integer(buf[o + 36:39])
                            bg[i, ] <- as.integer(buf[o + 40:43])
                        }
                    } else if (buf[o] == 0x00 && buf[1 + o] == 0x20) {
                        # On the first profile, we set up space.
                        if (!VMDASStorageInitialized) {
                            VMDASStorageInitialized <- TRUE
                            oceDebug(debug, "This is a VMDAS file\n")
                            isVMDAS <- TRUE
                            # FIXME: set up space; this c() method is slow and ugly
                            firstTime <- NULL
                            firstLongitude <- NULL
                            firstLatitude <- NULL
                            lastTime <- NULL
                            lastLongitude <- NULL
                            lastLatitude <- NULL
                            avgSpeed <- NULL
                            avgTrackTrue <- NULL
                            avgTrackMagnetic <- NULL
                            speedMadeGood <- NULL
                            speedMadeGoodNorth <- NULL
                            speedMadeGoodEast <- NULL
                            directionMadeGood <- NULL
                            shipPitch <- NULL
                            shipRoll <- NULL
                            shipHeading <- NULL
                            numberOfSpeedSamplesAveraged <- NULL
                            numberOfTrueTrackSamplesAveraged <- NULL
                            numberOfMagneticTrackSamplesAveraged <- NULL
                            numberOfHeadingSamplesAveraged <- NULL
                            numberOfPitchRollSamplesAveraged <- NULL
                            avgTrueVelocityNorth <- NULL
                            avgTrueVelocityEast <- NULL
                            avgMagnitudeVelocityNorth <- NULL
                            avgMagnitudeVelocityEast <- NULL
                            primaryFlags <- NULL
                        } else {
                            if (!isVMDAS) {
                                badVMDAS <- c(badVMDAS, i)
                            }
                        }
                        tmpTime <- as.numeric(ISOdatetime(as.integer(buf[o + 4]) + 256 * as.integer(buf[o + 5]), # year
                            as.integer(buf[o + 3]), # month
                            as.integer(buf[o + 2]), # day
                            0, 0, 0,
                            tz = tz
                        ))
                        clockOffset <- 0.001 * readBin(buf[o + 10:13], "integer", n = 1, size = 4, endian = "little")
                        firstTime <- c(firstTime, tmpTime + clockOffset + readBin(buf[o + 6:9], "integer", n = 1, size = 4, endian = "little") / 10000)
                        cfac <- 180 / 2^31 # from rdradcp.m line 825
                        firstLatitude <- c(firstLatitude, readBin(buf[o + 14:17], "integer", n = 1, size = 4, endian = "little") * cfac)
                        firstLongitude <- c(firstLongitude, readBin(buf[o + 18:21], "integer", n = 1, size = 4, endian = "little") * cfac)
                        lastTime <- c(lastTime, tmpTime + clockOffset + readBin(buf[o + 22:25], "integer", n = 1, size = 4, endian = "little") / 10000)
                        lastLatitude <- c(lastLatitude, readBin(buf[o + 26:29], "integer", n = 1, size = 4, endian = "little") * cfac)
                        lastLongitude <- c(lastLongitude, readBin(buf[o + 30:33], "integer", n = 1, size = 4, endian = "little") * cfac)
                        # FIXME: DK: I need to figure out the difference between eNavTime and navTime
                        # FIXME: CR: what you are calling navTime should be the same as the "ADCP time"
                        avgSpeed <- c(avgSpeed, 0.001 * readBin(buf[o + 34:35], "integer", n = 1, size = 2, endian = "little"))
                        avgTrackTrue <- c(avgTrackTrue, readBin(buf[o + 36:37], "integer", n = 1, size = 2, endian = "little"))
                        avgTrackMagnetic <- c(avgTrackMagnetic, readBin(buf[o + 38:39], "integer", n = 1, size = 2, endian = "little"))
                        speedMadeGood <- c(speedMadeGood, 0.001 * readBin(buf[o + 40:41], "integer", n = 1, size = 2, endian = "little"))
                        directionMadeGood <- c(directionMadeGood, (360 / 2^16) * readBin(buf[o + 42:43], "integer", n = 1, size = 2, endian = "little"))
                        shipPitch <- c(
                            shipPitch,
                            (360 / 2^16) * readBin(buf[o + 62:63], "integer", n = 1, size = 2, endian = "little")
                        )
                        shipRoll <- c(
                            shipRoll,
                            (360 / 2^16) * readBin(buf[o + 64:65], "integer", n = 1, size = 2, endian = "little")
                        )
                        shipHeading <- c(
                            shipHeading,
                            (360 / 2^16) * readBin(buf[o + 66:67], "integer", n = 1, size = 2, endian = "little")
                        )
                        numberOfSpeedSamplesAveraged <- c(
                            numberOfSpeedSamplesAveraged,
                            readBin(buf[o + 68:69], "integer", n = 1, size = 2, endian = "little")
                        )
                        numberOfTrueTrackSamplesAveraged <- c(
                            numberOfTrueTrackSamplesAveraged,
                            readBin(buf[o + 70:71], "integer", n = 1, size = 2, endian = "little")
                        )
                        numberOfMagneticTrackSamplesAveraged <- c(
                            numberOfMagneticTrackSamplesAveraged,
                            readBin(buf[o + 72:73], "integer", n = 1, size = 2, endian = "little")
                        )
                        numberOfHeadingSamplesAveraged <- c(
                            numberOfHeadingSamplesAveraged,
                            readBin(buf[o + 74:75], "integer", n = 1, size = 2, endian = "little")
                        )
                        numberOfPitchRollSamplesAveraged <- c(
                            numberOfPitchRollSamplesAveraged,
                            readBin(buf[o + 76:77], "integer", n = 1, size = 2, endian = "little")
                        )
                        avgTrueVelocityNorth <- c(
                            avgTrueVelocityNorth,
                            readBin(buf[o + 78:79], "integer", n = 1, size = 2, endian = "little")
                        )
                        avgTrueVelocityEast <- c(
                            avgTrueVelocityEast,
                            readBin(buf[o + 80:81], "integer", n = 1, size = 2, endian = "little")
                        )
                        avgMagnitudeVelocityNorth <- c(
                            avgMagnitudeVelocityNorth,
                            readBin(buf[o + 82:83], "integer", n = 1, size = 2, endian = "little")
                        )
                        avgMagnitudeVelocityEast <- c(
                            avgMagnitudeVelocityEast,
                            readBin(buf[o + 84:85], "integer", n = 1, size = 2, endian = "little")
                        )
                        speedMadeGoodNorth <- c(
                            speedMadeGoodNorth,
                            0.001 * readBin(buf[o + 86:87], "integer", n = 1, size = 2, endian = "little")
                        )
                        speedMadeGoodEast <- c(
                            speedMadeGoodEast,
                            0.001 * readBin(buf[o + 88:89], "integer", n = 1, size = 2, endian = "little")
                        )
                        primaryFlags <- c(
                            primaryFlags,
                            readBin(buf[o + 90:91], "integer", n = 1, size = 2, endian = "little")
                        )
                    } else if (buf[o] == 0x00 && buf[1 + o] == 0x0a) {
                        # vertical beam data
                        if (isSentinel) {
                            vtmp <- readBin(buf[o + 1 + seq(1, 2 * vItems)], "integer", n = vItems, size = 2, endian = "little", signed = TRUE)
                            vtmp[-32768 == vtmp] <- NA # blank out bad data
                            vv[i, ] <- velocityScale * vtmp
                        } else {
                            warning(
                                "Detected vertical beam data chunk, i.e. code ",
                                "0x00 0x0a at o=", o, " (profile ", i, "), but this is not a SentinelV\n"
                            )
                        }
                    } else if (buf[o] == 0x00 && buf[1 + o] == 0x0b) {
                        # vertical beam correlation
                        if (isSentinel) {
                            vq[i, ] <- buf[o + 1 + seq(1, vItems)]
                        } else {
                            warning(
                                "Detected vertical beam correlation chunk, i.e. code 0x00 0x0b at o=",
                                o, " (profile ", i, "), but this is not a SentinelV\n"
                            )
                        }
                    } else if (buf[o] == 0x00 && buf[1 + o] == 0x0c) {
                        # vertical beam amplitude
                        if (isSentinel) {
                            va[i, ] <- buf[o + 1 + seq(1, vItems)]
                        } else {
                            warning(
                                "Detected vertical beam amplitude chunk, i.e. code ",
                                "0x00 0x0c at o=", o, " (profile ", i, "), but this is not a SentinelV\n"
                            )
                        }
                    } else if (buf[o] == 0x00 && buf[1 + o] == 0x0d) {
                        # vertical beam percent good
                        if (isSentinel) {
                            vg[i, ] <- buf[o + 1 + seq(1, vItems)]
                        } else {
                            warning(
                                "Detected vertical beam percent-good chunk, i.e. code ",
                                "0x00 0x0d at o=", o, " (profile ", i, "), but this is not a SentinelV\n"
                            )
                        }
                    } else if (buf[o] == 0x00 && buf[1 + o] == 0x21) {
                        # 38 bytes (table 5 [WinRiver User Guide International Verion.pdf.pdf])
                        end <- .C("nmea_len", buf[o + 2 + 0:35], 36L, integer(1))[[3]]
                        nmeaTmp <- rawToChar(buf[o + 2 + 0:(end - 1)])
                        if (nchar(nmeaTmp)) {
                            nmeaLen <- nmeaLen + 1
                            nmea[nmeaLen] <- nmeaTmp
                        }
                    } else if (buf[o] == 0x00 && buf[1 + o] == 0x30) {
                        # already handled above
                    } else if (buf[o] == 0x01 && buf[1 + o] == 0x21) {
                        # 94 bytes (table 5 [WinRiver User Guide International Verion.pdf.pdf])
                        end <- .C("nmea_len", buf[o + 2 + 0:91], 92L, integer(1))[[3]]
                        nmeaTmp <- rawToChar(buf[o + 2 + 0:(end - 1)])
                        if (nchar(nmeaTmp)) {
                            nmeaLen <- nmeaLen + 1
                            nmea[nmeaLen] <- nmeaTmp
                        }
                    } else if (buf[o] == 0x02 && buf[1 + o] == 0x21) {
                        # 45 bytes (table 5 [WinRiver User Guide International Verion.pdf.pdf])
                        end <- .C("nmea_len", buf[o + 2 + 0:42], 43L, integer(1))[[3]]
                        nmeaTmp <- rawToChar(buf[o + 2 + 0:(end - 1)])
                        if (nchar(nmeaTmp)) {
                            nmeaLen <- nmeaLen + 1
                            nmea[nmeaLen] <- nmeaTmp
                        }
                    } else if (buf[o] == 0x03 && buf[1 + o] == 0x21) {
                        # 38 bytes (table 5 [WinRiver User Guide International Verion.pdf.pdf])
                        end <- .C("nmea_len", buf[o + 2 + 0:35], 36L, integer(1))[[3]]
                        nmeaTmp <- rawToChar(buf[o + 2 + 0:(end - 1)])
                        if (nchar(nmeaTmp)) {
                            nmeaLen <- nmeaLen + 1
                            nmea[nmeaLen] <- nmeaTmp
                        }
                    } else {
                        key <- paste("0x", as.character(buf[o]), " 0x", as.character(buf[o + 1]), sep = "")
                        if (0 == length(warningUnknownCode[[key]])) {
                            warningUnknownCode[[key]] <- 1
                        }
                        warningUnknownCode[[key]] <- warningUnknownCode[[key]] + 1
                    }
                    if (monitor) {
                        setTxtProgressBar(progressBar, i)
                    }
                }
                if (monitor) {
                    close(progressBar)
                }
                if (o >= bufSize) {
                    warning("got to end of file; o=", o, ", fileSize=", bufSize, "\n")
                    break
                }
            }
            if (debug > 0) {
                oceDebug(debug, "Recognized but unhandled ID codes:\n")
                print(unhandled)
            }
            if (length(warningUnknownCode) > 0) {
                warning(
                    "Encountered some unhandled segment codes, as listed in the following warnings.\n",
                    "Note that several Teledyne RDI manuals\n",
                    "describe such codes; see e.g. Table 33 of Teledyne RD Instruments, 2014.\n",
                    "Ocean Surveyor/Ocean Observer Technical Manual.\n",
                    "P/N 95A-6012-00 April 2014 (OS_TM_Apr14.pdf)\n"
                )
                for (name in names(warningUnknownCode)) {
                    # Recognize some cases:
                    # 0x40 0x30 through 0xFC 0x30: Binary Variable Attitude Data
                    testBytes <- as.raw(strsplit(name, " ")[[1]])
                    info <- ""
                    if (testBytes[2] == as.raw(0x30) &&
                        (as.raw(0x40) <= testBytes[1] && testBytes[1] <= as.raw(0xFC))) {
                        info <- " (a Variable Attitude ID; see Table 47 of Teledyne-RDI 'Ocean Surveyor Technical Manual_Dec20.pdf')"
                    }
                    warning("code ", name, " occurred ", warningUnknownCode[[name]], " times", info, "\n", sep = "")
                }
            }
            if (1 != length(unique(orientation))) {
                warning(
                    "the instrument orientation is not constant. The user is advised ",
                    "to determine the orientation during the relevant measurement phase, ",
                    "and to set this into the object with e.g.",
                    "adp <- oceSetMetadata(adp,\"orientation\",rep(\"upward\", length(adp[[\"time\"]]))) ",
                    "in case conversion to ENU is to be done later."
                )
            }
            oceDebug(debug, "length(time)=", length(time), "\n")
            if (length(time) > length(profileStart)) {
                warning("length(time)=", length(time), " exceeds length(profileStart)=", length(profileStart), " so trimming time\n")
                time <- time[seq_len(length(profileStart))]
            }
            oceDebug(debug, "length(time)=", length(time), " after possible shortening to match length(profileStart)\n")
            # Identify "junk" profiles by NA times
            junkProfiles <- base::which(is.na(time))
            if (isVMDAS) {
                firstTime <- firstTime + as.POSIXct("1970-01-01 00:00:00", tz = tz)
                lastTime <- lastTime + as.POSIXct("1970-01-01 00:00:00", tz = tz)
            }
            if (length(badProfiles) > 0) {
                # remove NAs in time (not sure this is right, but it prevents other problems)
                # FIXME: won't we need to handle VmDas here, also?
                t0 <- time[match(1, !is.na(time))] # FIXME: should test if any
                time <- fillGap(as.numeric(time) - as.numeric(t0)) + t0
                nbad <- length(badProfiles)
                if (nbad == 1) {
                    warning(
                        "Interpolated across a bad profile at time ", format(time[badProfiles]),
                        ".  (\"Bad\" means that the expected byte code for a velocity segment, ",
                        "0x00 0x01, was not found 65 bytes after the start of a profile, ",
                        "the latter indicated by the byte sequence 0x80 0x00.)"
                    )
                } else {
                    warning(
                        "Interpolated across ", length(badProfiles), " bad profile(s) at times: ",
                        paste(format(time[badProfiles]), collapse = ", "),
                        ".  (\"Bad\" means that the expected byte code for a velocity segment, ",
                        "0x00 0x01, was not found 65 bytes after the start of a profile, ",
                        "the latter indicated by the byte sequence 0x80 0x00.)"
                    )
                }
            }
            profileStart2 <- sort(c(profileStart, profileStart + 1)) # lets us index two-byte chunks
            profileStart4 <- sort(c(profileStart, profileStart + 1, profileStart + 2, profileStart + 3)) # lets us index four-byte chunks
            oceDebug(debug, "length(profileStart)=", length(profileStart), " after checking for bad profiles)\n")
            soundSpeed <- readBin(buf[profileStart2 + 14], "integer", n = profilesToRead, size = 2, endian = "little", signed = FALSE)
            depth <- 0.1 * readBin(buf[profileStart2 + 16], "integer", n = profilesToRead, size = 2, endian = "little")
            # Note that the headingBias needs to be removed
            heading <- 0.01 * readBin(buf[profileStart2 + 18], "integer",
                n = profilesToRead, size = 2, endian = "little", signed = FALSE
            ) - header$headingBias
            oceDebug(debug, vectorShow(heading, "heading"))
            if (header$headingBias != 0) {
                cat("read.adp.rdi(): subtracted a headingBias of ", header$headingBias, " degrees\n")
            }
            pitch <- 0.01 * readBin(buf[profileStart2 + 20], "integer", n = profilesToRead, size = 2, endian = "little", signed = TRUE)
            oceDebug(debug, vectorShow(profileStart2, "profileStart2"))
            oceDebug(debug, vectorShow(pitch, "pitch"))
            roll <- 0.01 * readBin(buf[profileStart2 + 22], "integer", n = profilesToRead, size = 2, endian = "little", signed = TRUE)
            oceDebug(debug, vectorShow(roll, "roll"))
            # tmp <- pitch
            oceDebug(debug, "will adjust the pitch as explained on page 14 of 'adcp coordinate transformation.pdf'\n")
            oceDebug(debug, vectorShow(pitch, "pitch, before correction"))
            pitch <- 180 / pi * atan(tan(pitch * pi / 180) / cos(roll * pi / 180)) # correct the pitch (see ACT page 14)
            oceDebug(debug, vectorShow(pitch, "pitch, after correction"))
            # oceDebug(debug, "RMS change in pitch:", sqrt(mean((pitch - tmp)^2, na.rm=TRUE)), "\n")
            # rm(tmp)
            salinity <- readBin(buf[profileStart2 + 24], "integer", n = profilesToRead, size = 2, endian = "little", signed = TRUE)
            temperature <- 0.01 * readBin(buf[profileStart2 + 26], "integer", n = profilesToRead, size = 2, endian = "little", signed = TRUE)
            pressure <- 0.001 * readBin(buf[profileStart4 + 48], "integer", n = profilesToRead, size = 4, endian = "little")
            if (despike) {
                temperature <- despike(temperature, reference = "trim", min = -3, max = 101)
                pressure <- despike(pressure, reference = "trim", min = 1, max = 10000)
            }
            headingStd <- as.numeric(buf[profileStart + 31]) # p142 WorkHorse_commands_data_format_AUG10.PDF
            pitchStd <- 0.1 * as.numeric(buf[profileStart + 32])
            rollStd <- 0.1 * as.numeric(buf[profileStart + 33])
            # read ADC channels [CR's code]
            xmitCurrent <- as.numeric(buf[profileStart + 34])
            xmitVoltage <- as.numeric(buf[profileStart + 35])
            ambientTemp <- as.numeric(buf[profileStart + 36])
            pressurePlus <- as.numeric(buf[profileStart + 37])
            pressureMinus <- as.numeric(buf[profileStart + 38])
            attitudeTemp <- as.numeric(buf[profileStart + 39])
            attitude <- as.numeric(buf[profileStart + 40])
            contaminationSensor <- as.numeric(buf[profileStart + 41])
            pressureStd <- readBin(buf[profileStart4 + 52], "integer", n = profilesToRead, size = 4, endian = "little")
            oceDebug(debug, vectorShow(temperature, "temperature"))
            oceDebug(debug, vectorShow(pressure, "pressure"))
            res <- new("adp")
            for (name in names(header)) {
                res@metadata[[name]] <- header[[name]]
            }
            res@metadata$orientation <- orientation # overrides a single value fromd decodeHeader()
            res@metadata$ensembleNumber <- ensembleNumber
            res@metadata$manufacturer <- "rdi"
            res@metadata$instrumentType <- "adcp"
            res@metadata$filename <- filename
            res@metadata$longitude <- longitude
            res@metadata$latitude <- latitude
            res@metadata$ensembleInFile <- ldc$ensemble_in_file
            res@metadata$velocityResolution <- velocityScale
            res@metadata$velocityMaximum <- velocityScale * 2^15
            res@metadata$numberOfSamples <- dim(v)[1]
            res@metadata$numberOfCells <- dim(v)[2]
            res@metadata$numberOfBeams <- dim(v)[3]
            res@metadata$bin1Distance <- bin1Distance
            res@metadata$xmitPulseLength <- xmitPulseLength
            res@metadata$oceBeamUnspreaded <- FALSE
            res@metadata$oceCoordinate <- header$originalCoordinate
            res@metadata$depthMean <- mean(depth, na.rm = TRUE)
            if (isSentinel) {
                if (exists("transformationMatrix")) {
                    res@metadata$transformationMatrix <- transformationMatrix
                } else {
                    warning("Sentinel file detected but no transformation matrix found.")
                }
            } else {
                # Transformation matrix
                # FIXME Dal people use 'a' in last row of matrix, but both
                # RDI and CODAS use as we have here.  (And I think RDI
                # may have two definitions...)
                #
                # Notes on coordinate transformationMatrix.
                # From figure 3 on page 12 of ACT (adcp coordinate transformation.pdf)
                # we have
                #
                #    x defined to run from beam 1 to beam 2
                #    y defined to run from beam 4 to beam 3
                #    z right-handed from these.
                #
                # and the upward-looking orientation (viewed from above) is
                #
                #        B3
                #    B2      B1
                #        B4
                #
                # so we have coords
                #
                #            y
                #            ^
                #            |
                #            |
                #    x <-----*   (z into page, or downward)
                #
                # The matrix below is from section 5.3 of the ACT.
                #
                # As a check on coding, see the python software at
                #   http://currents.soest.hawaii.edu/hg/pycurrents/file/3175207488bb/adcp/transform.py
                tm.c <- if (res@metadata$beamPattern == "convex") 1 else -1 # control sign of first 2 rows of transformationMatrix
                tm.a <- 1 / (2 * sin(res@metadata$beamAngle * pi / 180))
                tm.b <- 1 / (4 * cos(res@metadata$beamAngle * pi / 180))
                tm.d <- tm.a / sqrt(2)
                res@metadata$transformationMatrix <- matrix(
                    c(
                        tm.c * tm.a, -tm.c * tm.a, 0, 0,
                        0, 0, -tm.c * tm.a, tm.c * tm.a,
                        tm.b, tm.b, tm.b, tm.b,
                        tm.d, tm.d, -tm.d, -tm.d
                    ),
                    nrow = 4, byrow = TRUE
                )
            }
            if (monitor) {
                cat("\nFinished reading ", profilesToRead, " profiles from \"", filename, "\"\n", sep = "")
            }
            # Sometimes a non-VMDAS file will have some profiles that have the VMDAS flag.
            # It is not clear why this happens, but in any case, provide a warning.
            nbadVMDAS <- length(badVMDAS)
            if (nbadVMDAS > 0) {
                if (1 == nbadVMDAS) {
                    warning("erroneous VMDAS flag in profile ", badVMDAS)
                } else if (nbadVMDAS < 4) {
                    warning("erroneous VMDAS flag in profiles: ", paste(badVMDAS, collapse = " "))
                } else {
                    warning(
                        "erroneous VMDAS flag in ", nbadVMDAS, " profiles, including: ", badVMDAS[1], " ",
                        badVMDAS[2], " ... ", badVMDAS[nbadVMDAS - 1], " ",
                        badVMDAS[nbadVMDAS], "\n"
                    )
                }
            }
            class(time) <- c("POSIXct", "POSIXt")
            attr(time, "tzone") <- getOption("oceTz")
            if (bFound && !isVMDAS) {
                oceDebug(debug, "creating data slot for a file with bFound&&!isVMDAS\n")
                br[br == 0.0] <- NA # clean up (not sure if needed)
                # issue1228
                res@data <- list(
                    v = v, q = q, a = a, g = g,
                    br = br, bv = bv, bq = bq, ba = ba, bg = bg,
                    distance = seq(bin1Distance, by = cellSize, length.out = numberOfCells),
                    time = time,
                    pressure = pressure,
                    temperature = temperature,
                    salinity = salinity,
                    depth = depth,
                    soundSpeed = soundSpeed,
                    heading = heading,
                    pitch = pitch,
                    roll = roll,
                    headingStd = headingStd,
                    pitchStd = pitchStd,
                    rollStd = rollStd,
                    pressureStd = pressureStd,
                    xmitCurrent = xmitCurrent,
                    xmitVoltage = xmitVoltage,
                    ambientTemp = ambientTemp,
                    pressurePlus = pressurePlus,
                    pressureMinus = pressureMinus,
                    attitudeTemp = attitudeTemp,
                    attitude = attitude,
                    contaminationSensor = contaminationSensor,
                    nmea = nmea
                )
            } else if (bFound && isVMDAS) {
                oceDebug(debug, "creating data slot for a file with bFound&&isVMDAS\n")
                br[br == 0.0] <- NA # clean up (not sure if needed)
                res@data <- list(
                    v = v, q = q, a = a, g = g,
                    br = br, bv = bv, bq = bq, ba = ba, bg = bg,
                    distance = seq(bin1Distance, by = cellSize, length.out = numberOfCells),
                    time = time,
                    pressure = pressure,
                    temperature = temperature,
                    salinity = salinity,
                    depth = depth,
                    soundSpeed = soundSpeed,
                    heading = heading,
                    pitch = pitch,
                    roll = roll,
                    headingStd = headingStd,
                    pitchStd = pitchStd,
                    rollStd = rollStd,
                    pressureStd = pressureStd,
                    xmitCurrent = xmitCurrent,
                    xmitVoltage = xmitVoltage,
                    ambientTemp = ambientTemp,
                    pressurePlus = pressurePlus,
                    pressureMinus = pressureMinus,
                    attitudeTemp = attitudeTemp,
                    attitude = attitude,
                    contaminationSensor = contaminationSensor,
                    # Next are as described starting on p77 of VmDas_Users_Guide_May12.pdf
                    avgSpeed = avgSpeed,
                    avgMagnitudeVelocityEast = avgMagnitudeVelocityEast,
                    avgMagnitudeVelocityNorth = avgMagnitudeVelocityNorth,
                    avgTrackMagnetic = avgTrackMagnetic,
                    avgTrackTrue = avgTrackTrue,
                    avgTrueVelocityEast = avgTrueVelocityEast,
                    avgTrueVelocityNorth = avgTrueVelocityNorth,
                    directionMadeGood = directionMadeGood,
                    firstLatitude = firstLatitude,
                    firstLongitude = firstLongitude,
                    firstTime = firstTime,
                    lastLatitude = lastLatitude,
                    lastLongitude = lastLongitude,
                    lastTime = lastTime,
                    numberOfHeadingSamplesAveraged = numberOfHeadingSamplesAveraged,
                    numberOfMagneticTrackSamplesAveraged = numberOfMagneticTrackSamplesAveraged,
                    numberOfPitchRollSamplesAveraged = numberOfPitchRollSamplesAveraged,
                    numberOfSpeedSamplesAveraged = numberOfSpeedSamplesAveraged,
                    numberOfTrueTrackSamplesAveraged = numberOfTrueTrackSamplesAveraged,
                    primaryFlags = primaryFlags,
                    shipHeading = shipHeading,
                    shipPitch = shipPitch,
                    shipRoll = shipRoll,
                    speedMadeGood = speedMadeGood,
                    speedMadeGoodEast = speedMadeGoodEast,
                    speedMadeGoodNorth = speedMadeGoodNorth
                )
            } else if (!bFound && isVMDAS) {
                oceDebug(debug, "creating data slot for a file with !bFound&&isVMDAS\n")
                res@data <- list(
                    v = v, q = q, a = a, g = g,
                    distance = seq(bin1Distance, by = cellSize, length.out = numberOfCells),
                    time = time,
                    pressure = pressure,
                    temperature = temperature,
                    salinity = salinity,
                    depth = depth,
                    soundSpeed = soundSpeed,
                    heading = heading,
                    pitch = pitch,
                    roll = roll,
                    headingStd = headingStd,
                    pitchStd = pitchStd,
                    rollStd = rollStd,
                    pressureStd = pressureStd,
                    xmitCurrent = xmitCurrent,
                    xmitVoltage = xmitVoltage,
                    ambientTemp = ambientTemp,
                    pressurePlus = pressurePlus,
                    pressureMinus = pressureMinus,
                    attitudeTemp = attitudeTemp,
                    attitude = attitude,
                    contaminationSensor = contaminationSensor,
                    # Next are as described starting on p77 of VmDas_Users_Guide_May12.pdf
                    avgSpeed = avgSpeed,
                    avgMagnitudeVelocityEast = avgMagnitudeVelocityEast,
                    avgMagnitudeVelocityNorth = avgMagnitudeVelocityNorth,
                    avgTrackMagnetic = avgTrackMagnetic,
                    avgTrackTrue = avgTrackTrue,
                    avgTrueVelocityEast = avgTrueVelocityEast,
                    avgTrueVelocityNorth = avgTrueVelocityNorth,
                    directionMadeGood = directionMadeGood,
                    firstLatitude = firstLatitude,
                    firstLongitude = firstLongitude,
                    firstTime = firstTime,
                    lastLatitude = lastLatitude,
                    lastLongitude = lastLongitude,
                    lastTime = lastTime,
                    numberOfHeadingSamplesAveraged = numberOfHeadingSamplesAveraged,
                    numberOfMagneticTrackSamplesAveraged = numberOfMagneticTrackSamplesAveraged,
                    numberOfPitchRollSamplesAveraged = numberOfPitchRollSamplesAveraged,
                    numberOfSpeedSamplesAveraged = numberOfSpeedSamplesAveraged,
                    numberOfTrueTrackSamplesAveraged = numberOfTrueTrackSamplesAveraged,
                    primaryFlags = primaryFlags,
                    shipHeading = shipHeading,
                    shipPitch = shipPitch,
                    shipRoll = shipRoll,
                    speedMadeGood = speedMadeGood,
                    speedMadeGoodEast = speedMadeGoodEast,
                    speedMadeGoodNorth = speedMadeGoodNorth
                )
            } else if (isSentinel) {
                oceDebug(debug, "creating data slot for a SentinelV file\n")
                res@data <- list(
                    v = v, q = q, a = a, g = g,
                    vv = vv, vq = vq, va = va, vg = vg,
                    vdistance = seq(firstCellRange, b = depthCellSize, length.out = numberOfVCells),
                    distance = seq(bin1Distance, by = cellSize, length.out = numberOfCells),
                    time = time,
                    pressure = pressure,
                    temperature = temperature,
                    salinity = salinity,
                    depth = depth,
                    soundSpeed = soundSpeed,
                    heading = heading,
                    pitch = pitch,
                    roll = roll,
                    headingStd = headingStd,
                    pitchStd = pitchStd,
                    rollStd = rollStd,
                    pressureStd = pressureStd,
                    xmitCurrent = xmitCurrent,
                    xmitVoltage = xmitVoltage,
                    ambientTemp = ambientTemp,
                    pressurePlus = pressurePlus,
                    pressureMinus = pressureMinus,
                    attitudeTemp = attitudeTemp,
                    attitude = attitude,
                    contaminationSensor = contaminationSensor
                )
            } else {
                oceDebug(debug, "creating data slot for a non-SentinelV file\n")
                res@data <- list(
                    v = v, q = q, a = a, g = g,
                    distance = seq(bin1Distance, by = cellSize, length.out = numberOfCells),
                    time = time,
                    pressure = pressure,
                    temperature = temperature,
                    salinity = salinity,
                    depth = depth,
                    soundSpeed = soundSpeed,
                    heading = heading,
                    pitch = pitch,
                    roll = roll,
                    headingStd = headingStd,
                    pitchStd = pitchStd,
                    rollStd = rollStd,
                    pressureStd = pressureStd,
                    xmitCurrent = xmitCurrent,
                    xmitVoltage = xmitVoltage,
                    ambientTemp = ambientTemp,
                    pressurePlus = pressurePlus,
                    pressureMinus = pressureMinus,
                    attitudeTemp = attitudeTemp,
                    attitude = attitude,
                    contaminationSensor = contaminationSensor
                )
            }
        } else {
            warning("There are no profiles in this file.")
            for (name in names(header)) {
                res@metadata[[name]] <- header[[name]]
            }
            res@metadata$filename <- filename
            res@data <- NULL
        }
    } else {
        warning("The header indicates that there are no profiles in this file.")
        for (name in names(header)) {
            res@metadata[[name]] <- header[[name]]
        }
        res@metadata$filename <- filename
        res@data <- NULL
    }
    # Remove "junk" profiles
    if (length(junkProfiles) > 0) {
        # remove all data from the profile
        for (field in names(res@data)) {
            if (!(field %in% c("distance", "vdistance"))) {
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
        if (njunk == 1) {
            warning(
                "Removed a junk profile at time ",
                format(time[junkProfiles - 1]),
                ".  (\"Junk\" means that the decoded time was NA and all data fields were garbage)"
            )
        } else {
            warning(
                "Removed ", length(junkProfiles),
                " junk profile(s) at times: ",
                paste(format(time[junkProfiles - 1]), collapse = ", "),
                ".  (\"Junk\" means that the decoded time was NA and all data fields were garbage)"
            )
        }
    }
    res@metadata$manufacturer <- "teledyne rdi"
    if (missing(processingLog)) {
        processingLog <- paste(deparse(match.call()), sep = "", collapse = "")
    }
    hitem <- processingLogItem(processingLog)
    res@processingLog <- unclass(hitem)
    res@metadata$units$v <- list(unit = expression(m / s), scale = "")
    res@metadata$units$distance <- list(unit = expression(m), scale = "")
    res@metadata$units$pressure <- list(unit = expression(dbar), scale = "")
    res@metadata$units$salinity <- list(unit = expression(), scale = "PSS-78")
    res@metadata$units$temperature <- list(unit = expression(degree * C), scale = "ITS-90")
    res@metadata$units$soundSpeed <- list(unit = expression(m / s), scale = "")
    res@metadata$units$heading <- list(unit = expression(degree), scale = "")
    res@metadata$units$pitch <- list(unit = expression(degree), scale = "")
    res@metadata$units$roll <- list(unit = expression(degree), scale = "")
    res@metadata$units$headingStd <- list(unit = expression(degree), scale = "")
    res@metadata$units$pitchStd <- list(unit = expression(degree), scale = "")
    res@metadata$units$rollStd <- list(unit = expression(degree), scale = "")
    res@metadata$units$attitude <- list(unit = expression(degree), scale = "")
    res@metadata$units$depth <- list(unit = expression(m), scale = "")
    oceDebug(debug, "END read.adp.rdi()\n", unindent = 1)
    res
}
