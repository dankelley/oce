# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4:foldmethod=marker
#
# Analyse Configuration byte-pair Bottom-Track data. This format differs
# from that used in other data types. The code below was developed
# in ~/git/oce-issues/23xx/2368/ad2cp_snippet_1.R
dataAvailableBottomTrack <- function(twoBytes) {
    configuration <- ifelse(rawToBits(twoBytes) == 0x01, TRUE, FALSE)
    valid <- list()
    valid$pressure <- configuration[1] # Ref 1 calls this bit 0, etc for rest
    valid$temperature <- configuration[2]
    valid$compass <- configuration[3]
    valid$tilt <- configuration[4]
    # bit 5 (called bit 4 in Ref 1) is empty
    valid$velocity <- configuration[6]
    valid$amplitude <- configuration[7]
    valid$correlation <- configuration[8]
    valid$distance <- configuration[9]
    valid$figureOfMerit <- configuration[10]
    valid$AHRS <- configuration[11]
    valid$aux <- configuration[12]
    valid
}

# Read bottom-track AD2CP data
#
# Much of this code follows section 6.1.3 of Ref 1. I think Nortek
# also released a similar document in 2018, but the manual
# I have for 2026 has nothing about the format. Therefore,
# this code may be somewhat brittle. Some elements, e.g.
# the coordinate-system and number-of-beams field, are entirely
# different from Ref 1, instead coming from an email sent by
# a Nortek person to Clark Richards and Dan Kelley on 2026-03-24.
#
# @references
# 1. Nortek AS. "Signature Integration 55|250|500|1000kHz (2017)."
# Nortek AS, February 10, 2017.
# https://www.nortekgroup.com/assets/software/N3015-007-Integrators-Guide-AD2CP_1018.pdf.
#
# @author Dan Kelley
readBottomTrack <- function(d, configText, debug = getOption("oceDebug")) # uses global 'd' and 'configuration'
{
    if (any(d$id != d$id[1])) {
        stop("the 'id' field varies -- did read.adp.nortek.ad2cp() sieve the data?")
    }
    if (!(d$id[1] %in% c(0x17, 0x1d))) { # FIXME: check whether this code could work for 0x1d
        # stop("all d$id values must equal 0x17 or 0x1d -- there is a problem with read.adp.nortek.ad2cp().")
        stop("all d$id values must equal 0x17 -- there is a problem with read.adp.nortek.ad2cp().")
    }
    type <- gsub(".*=", "", ad2cpCodeToName(d$id[1]))
    oceDebug(debug, "readBottomTrack(id=0x", as.raw(d$id[1]), " or ", d$id[1], " decimal) # i.e. type=", type, " START\n", unindent = 1)
    offsetOfData <- as.integer(d$buf[d$index[1] + 2L])
    oceDebug(debug, vectorShow(offsetOfData))
    badRowCount <- checkRowConsistency(d$configuration)
    if (badRowCount > 0) {
        stop("Problem with bottomTrack 'configuration' matrix: ", badRowCount, " rows do not match row #1")
    }
    oceDebug(debug, "d$configuration: ", paste(ifelse(d$configuration[1, ], "1", "0"), collapse = ""), " (shown as a bitmask)\n")

    # Determine which data types are recorded. Nortek indicates that
    # bottom-track data should always have `v` (reflecter velocity), `distance`
    # ( distance to reflecter) and `figureOfMerit` (a measure of data quality, I
    # assume), and so we issue a warning if these are not all present.
    dataAvailable <- dataAvailableBottomTrack(d$buf[d$index[1] + 3:4])
    oceDebug(debug, vectorShow(dataAvailable))
    if (!dataAvailable$velocity) warning("no velocity data found -- this is likely an error")
    if (!dataAvailable$distance) warning("no velocity data found -- this is likely an error")
    if (!dataAvailable$figureOfMerit) warning("no velocity data found -- this is likely an error")
    serialNumber <- readBin(d$buf[d$index[1] + 5:8], "integer", size = 4L, endian = "little")
    oceDebug(debug, vectorShow(serialNumber))
    # {{{ timeseries data
    # FIXME: these would be useful generally, so maybe compute at higher level
    pointer1 <- d$index
    pointer2 <- gappyIndex(d$index, 0, 2)
    pointer4 <- gappyIndex(d$index, 0, 4)
    oceDebug(debug, vectorShow(pointer1, n = 8))
    oceDebug(debug, vectorShow(pointer2, n = 8))
    oceDebug(debug, vectorShow(pointer4, n = 8))
    nprofiles <- length(pointer1)
    oceDebug(debug, vectorShow(nprofiles))
    year <- 1900 + as.integer(d$buf[pointer1 + 9])
    oceDebug(debug, vectorShow(year))
    month <- 1 + as.integer(d$buf[pointer1 + 10])
    oceDebug(debug, vectorShow(month))
    day <- as.integer(d$buf[pointer1 + 11])
    oceDebug(debug, vectorShow(day))
    hour <- as.integer(d$buf[pointer1 + 12])
    oceDebug(debug, vectorShow(hour))
    min <- as.integer(d$buf[pointer1 + 13])
    oceDebug(debug, vectorShow(min))
    sec <- as.integer(d$buf[pointer1 + 14])
    oceDebug(debug, vectorShow(sec))
    # Time is in in units of 100 microseconds, hence the factor 1e-4.
    secFraction <- 1e-4 * readBin(d$buf[pointer2 + 15], "integer", size = 2L, n = nprofiles, signed = FALSE, endian = "little")
    oceDebug(debug, vectorShow(secFraction))
    time <- ISOdatetime(year, month, day, hour, min, sec + secFraction, tz = "UTC")
    oceDebug(debug, vectorShow(time))
    soundSpeed <- 0.1 * readBin(d$buf[pointer2 + 17], "integer", size = 2L, n = nprofiles, signed = FALSE, endian = "little")
    oceDebug(debug, vectorShow(soundSpeed))
    temperature <- 0.01 * readBin(d$buf[pointer2 + 19], "integer", size = 2L, n = nprofiles, signed = TRUE, endian = "little")
    oceDebug(debug, vectorShow(temperature))
    pressure <- 0.001 * readBin(d$buf[pointer4 + 21L], "integer", size = 4L, n = nprofiles, endian = "little")
    oceDebug(debug, vectorShow(pressure))
    heading <- 0.01 * readBin(d$buf[pointer2 + 25L], "integer", size = 2L, n = nprofiles, signed = FALSE, endian = "little")
    oceDebug(debug, vectorShow(heading))
    pitch <- 0.01 * readBin(d$buf[pointer2 + 27L], "integer", size = 2L, n = nprofiles, signed = TRUE, endian = "little")
    oceDebug(debug, vectorShow(pitch))
    roll <- 0.01 * readBin(d$buf[pointer2 + 29L], "integer", size = 2L, n = nprofiles, signed = TRUE, endian = "little")
    oceDebug(debug, vectorShow(roll))
    # }}}
    # {{{ #beams,coord-sys,#cells
    # DELETE beamsCoords <- d$buf[pointer1[1] + 31] # this ought never to change
    # DELETE oceDebug(debug, vectorShow(beamsCoords))
    # DELETE Decode a 2-byte sequence. Note that some items cross
    # DELETE byte boundaries, so we cannot simply read with readBin(),
    # DELETE and must instead expand bit by bit.
    # DELETE BCCraw <- readBin(d$buf[pointer1[1] + 31:32], "raw", size = 1, n = 2, endian = "little")
    # NOTE: email from Nortek says that we do NOT have a 2-byte sequence
    # here, in contradiction to the 2017 manual. We have 1-byte beams_cy
    # and then 1-byte pad1, and after 2-byte for ncells.
    beamsCyByte <- readBin(d$buf[pointer1[1] + 31], "raw", size = 1, n = 1, endian = "little")
    beamsCy <- ifelse(rawToBits(beamsCyByte) == 0x01, 1, 0)
    oceDebug(debug, vectorShow(beamsCy, n = 10))
    oceDebug(debug, "In the above value of beamsCy, expect as follows, based on a Nortek email\n")
    oceDebug(debug, "  bits 7-5: nbeams\n")
    oceDebug(debug, "  bits 4-3: coordinate system (00 for 'enu', 01 for 'xyz', 10 for 'beam' and 11 for '-')\n")
    oceDebug(debug, "where, I think the number is 0 for the first bit etc\n")
    oceDebug(debug, "but are they counting from left-to-right or from right-to-left?\n")
    # From email (I think they count from 0): bit 7-5 = number of beams")
    nbeams <- 4L * beamsCy[8] + 2L * beamsCy[7] + beamsCy[6]
    oceDebug(debug, vectorShow(nbeams))
    if (nbeams != 4) {
        tmp <- nbeams
        nbeams <- findInConfig(configText[[1]], "GETBT", "NB")
        warning("Using nbeams=", nbeams, " from file header, instead of suspicious value ", tmp, " in data record. Plan: revisit if Nortek documents bottom-track format.")
    }
    # From email (I think they count from 0): bit 4-3 = coordinate system (b00 : ENU b01 : XYZ b10 : BEAM)")
    oceDebug(debug, vectorShow(beamsCy[5:4]))
    CStmp <- 2L * beamsCy[5] + beamsCy[4]
    oceDebug(debug, vectorShow(CStmp))
    coordinateSystem <- switch(CStmp + 1L,
        "enu", # CStmp=0
        "xyz", # CStmp=1
        "beam", # CStmp=2
        "-" # CStmp=3
    )
    if (coordinateSystem == "-") {
        tmp <- coordinateSystem
        coordinateSystem <- tolower(findInConfig(configText[[1]], "", "CY", numeric = FALSE))
        warning("Using coordinateSystem=\"", coordinateSystem, "\" from file header, instead of suspicious value \"", tmp, "\" in data record. Plan: revisit if Nortek documents bottom-track format.")
    }
    oceDebug(debug, vectorShow(coordinateSystem))
    cellSize <- 1.0e-3 * readBin(d$buf[pointer2[1:2] + 33L], "integer", size = 2L, n = 1, signed = FALSE, endian = "little")
    oceDebug(debug, "in readBottomTrack() ", vectorShow(pointer2[1:2] + 33))
    oceDebug(debug, "in readBottomTrack() ", vectorShow(cellSize))
    blankingDistance <- 1.0e-3 * readBin(d$buf[pointer2[1:2] + 35L], "integer", size = 2L, n = 1, signed = FALSE, endian = "little")
    oceDebug(debug, vectorShow(blankingDistance))
    # skip 1 byte called 'padUnused' in Nortek email
    # skip 1 byte called 'pressureTemp' in Nortek email
    batteryVoltage <- 0.1 * readBin(d$buf[pointer2 + 39L], "integer", size = 2L, n = nprofiles, signed = FALSE, endian = "little")
    oceDebug(debug, vectorShow(batteryVoltage))
    # {{{ magnetometer FIXME: is there a factor to get to physical units?
    magnetometer <- matrix(0.0, nrow = nprofiles, ncol = 3)
    magnetometer[, 1] <- readBin(d$buf[pointer2 + 41L], "integer", size = 2L, n = nprofiles, signed = TRUE, endian = "little")
    oceDebug(debug, vectorShow(magnetometer[, 1]))
    magnetometer[, 2] <- readBin(d$buf[pointer2 + 43L], "integer", size = 2L, n = nprofiles, signed = TRUE, endian = "little")
    oceDebug(debug, vectorShow(magnetometer[, 2]))
    magnetometer[, 3] <- readBin(d$buf[pointer2 + 45L], "integer", size = 2L, n = nprofiles, signed = TRUE, endian = "little")
    oceDebug(debug, vectorShow(magnetometer[, 3]))
    # }}}
    # {{{ accelerometer
    accelerometer <- matrix(0.0, nrow = nprofiles, ncol = 3L)
    accelerometer[, 1] <- 1.0 / 16384.0 * readBin(d$buf[pointer2 + 47], "integer", size = 2L, n = nprofiles, signed = TRUE, endian = "little")
    accelerometer[, 2] <- 1.0 / 16384.0 * readBin(d$buf[pointer2 + 49], "integer", size = 2L, n = nprofiles, signed = TRUE, endian = "little")
    accelerometer[, 3] <- 1.0 / 16384.0 * readBin(d$buf[pointer2 + 51], "integer", size = 2L, n = nprofiles, signed = TRUE, endian = "little")
    # }}}
    velocityScaling <- readBin(d$buf[pointer1[1] + 61], "integer", size = 1L, endian = "little", signed = TRUE)
    oceDebug(debug, vectorShow(velocityScaling))
    velocityFactor <- 10^velocityScaling
    oceDebug(debug, vectorShow(velocityFactor))
    oceDebug(debug, "offsetOfData: ", offsetOfData, ", is this 78?\n")
    # Next is wrong; it cycles from 1 to 60, like a seconds or minutes field
    ensembleCounter <- readBin(d$buf[pointer4 + 75L],
        "integer",
        size = 4L, n = nprofiles, endian = "little"
    )
    # {{{ Velocity. FIXME: perhaps handle 5-beam case
    v <- array(double(), dim = c(nprofiles, 1L, nbeams))
    v[, 1L, 1L] <- velocityFactor * readBin(d$buf[pointer4 + 79L],
        "integer",
        size = 4L, n = nprofiles, endian = "little"
    )
    v[, 1L, 2L] <- velocityFactor * readBin(d$buf[pointer4 + 83L],
        "integer",
        size = 4L, n = nprofiles, endian = "little"
    )
    v[, 1L, 3L] <- velocityFactor * readBin(d$buf[pointer4 + 87L],
        "integer",
        size = 4L, n = nprofiles, endian = "little"
    )
    v[, 1L, 4L] <- velocityFactor * readBin(d$buf[pointer4 + 91L],
        "integer",
        size = 4L, n = nprofiles, endian = "little"
    )
    oceDebug(debug, vectorShow(v))
    # }}}
    # {{{ Distance.
    distance <- array(double(), dim = c(nprofiles, 1L, nbeams))
    distance[, 1, 1] <- 0.001 * readBin(d$buf[pointer4 + 95], "integer", size = 4L, n = nprofiles, endian = "little")
    distance[, 1, 2] <- 0.001 * readBin(d$buf[pointer4 + 99], "integer", size = 4L, n = nprofiles, endian = "little")
    distance[, 1, 3] <- 0.001 * readBin(d$buf[pointer4 + 103], "integer", size = 4L, n = nprofiles, endian = "little")
    distance[, 1, 4] <- 0.001 * readBin(d$buf[pointer4 + 107], "integer", size = 4L, n = nprofiles, endian = "little")
    oceDebug(debug, vectorShow(distance))
    # }}}
    figureOfMerit <- readBin(d$buf[pointer2 + 111], "integer", size = 2L, endian = "little", n = nprofiles, signed = FALSE)
    oceDebug(debug, vectorShow(d$buf[pointer2 + 111]))
    oceDebug(debug, "Something is definitely wrong with this -- ", vectorShow(figureOfMerit))
    oceDebug(debug, vectorShow(nprofiles))
    oceDebug(debug, vectorShow(pointer1))
    oceDebug(debug, vectorShow(pointer2))
    oceDebug(debug, vectorShow(pointer4))
    rval <- list(
        # Scalars
        numberOfBeams = nbeams, # renaming so calling function can move to metadata
        numberOfCells = 1, # I don't know what the value read above means, but plot.adp() needs 1 to plot timeseries
        oceCoordinate = coordinateSystem, # renaming so calling function can move to metadata
        cellSize = cellSize,
        blankingDistance = blankingDistance,
        # Time-series vectors
        soundSpeed = soundSpeed,
        time = time, pressure = pressure, temperature = temperature,
        heading = heading, pitch = pitch, roll = roll,
        batteryVoltage = batteryVoltage,
        magnetometer = magnetometer,
        accelerometer = accelerometer,
        ensembleCounter = ensembleCounter,
        figureOfMerit = figureOfMerit,
        # arrays (dimension NP x 1 x NB)
        v = v,
        distance = distance # Not to be confused with @metadata$distance for profile-data!
    )
    oceDebug(debug, "END readBottomTrack()\n", unindent = 1)
    rval
} # readBottomTrack
