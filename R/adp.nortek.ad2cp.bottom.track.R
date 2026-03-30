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

# Nortek (2022 page 93 ) "6.7 _DF20BottomTrack"
readBottomTrack <- function(d, debug = getOption("oceDebug")) # uses global 'd' and 'configuration'
{
    id <- 0x17 # bottomTrack

    if (any(d$id != id)) {
        stop("the 'id' field varies -- have the data been seived in the calling function?")
    }
    type <- gsub(".*=", "", ad2cpCodeToName(id))
    oceDebug(debug, "\n\n\nreadBottomTrackNEW(id=0x", as.raw(id), " or ", id, " decimal) # i.e. type=", type, " START\n", unindent = 1)
    look <- which(d$id == id)
    if (length(look) < 1) {
        stop("There are no records with id=0x", as.raw(a), " (i.e. with id=", a, " base 10)")
    }
    oceDebug(debug, vectorShow(look))
    lookIndex <- d$index[look]
    oceDebug(debug, vectorShow(lookIndex))
    offsetOfData <- as.integer(d$buf[d$index[look[1]] + 2L])
    oceDebug(debug, vectorShow(offsetOfData))
    badRowCount <- checkRowConsistency(d$configuration[look, ])
    if (badRowCount > 0) {
        stop("Problem with bottomTrack 'configuration' matrix: ", badRowCount, " rows do not match row #1")
    }
    oceDebug(debug, "d$configuration: ", paste(ifelse(d$configuration[look[1], ], "1", "0"), collapse = ""), " (shown as a bitmask)\n")

    # Determine which data types are recorded. Nortek indicates that
    # bottom-track data should always have `v` (reflecter velocity), `distance`
    # ( distance to reflecter) and `figureOfMerit` (a measure of data quality, I
    # assume), and so we issue a warning if these are not all present.
    dataAvailable <- dataAvailableBottomTrack(d$buf[d$index[look[1]] + 3:4])
    oceDebug(debug, vectorShow(dataAvailable))
    if (!dataAvailable$velocity) warning("no velocity data found -- this is likely an error")
    if (!dataAvailable$distance) warning("no velocity data found -- this is likely an error")
    if (!dataAvailable$figureOfMerit) warning("no velocity data found -- this is likely an error")
    serialNumber <- readBin(d$buf[d$index[look[1]] + 5:8], "integer", size = 4L, endian = "little")
    oceDebug(debug, vectorShow(serialNumber))
    # {{{ FIXME: these would be useful generally, so maybe compute at higher level
    pointer1 <- d$index[look] # FIXME: shouldn't we be using look here?
    pointer2 <- gappyIndex(d$index[look], 0, 2)
    pointer4 <- gappyIndex(d$index[look], 0, 4)
    oceDebug(debug, vectorShow(pointer1[1:8], n = 8))
    oceDebug(debug, vectorShow(pointer2[1:8], n = 8))
    oceDebug(debug, vectorShow(pointer4[1:8], n = 8))
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
    # {{{ FIXME remove these trial sample plots
    pdf("~/ad2cp_teaching.pdf")
    par(mfrow = c(4, 1))
    oce.plot.ts(time, pressure)
    oce.plot.ts(time, heading, drawTimeRange = FALSE)
    oce.plot.ts(time, pitch, drawTimeRange = FALSE)
    abline(h = 0, col = 2)
    oce.plot.ts(time, roll, drawTimeRange = FALSE)
    abline(h = 0, col = 2)
    dev.off()
    # }}}
    beamsCoords <- d$buf[pointer1[1] + 31] # this ought never to change
    oceDebug(debug, vectorShow(beamsCoords))
    # FIXME use
    beamsCoordsBits <- as.integer(strsplit(byteToBinary(beamsCoords[1]), "")[[1]])
    oceDebug(debug, vectorShow(beamsCoordsBits, n = 16))
    beamsCoordsBitsNEW <- ifelse(rawToBits(beamsCoords[1]) == 0x01, 1, 0)
    oceDebug(debug, vectorShow(beamsCoordsBitsNEW, n = 16))
    # Called bits 15-13 in Ref. 1
    nbeams <- beamsCoordsBits[3] + 2 * beamsCoordsBits[2] + 4 * beamsCoordsBits[1]
    oceDebug(debug, vectorShow(nbeams))
    # pad1 32
    # ncells 33:34
    ncells <- readBin(d$buf[pointer2[1:2] + 33L], "integer", size = 2L, n = 1, signed = FALSE, endian = "little")
    oceDebug(debug, vectorShow(ncells))
    cellSize <- 1.0e-3 * readBin(d$buf[pointer2[1:2] + 35L], "integer", size = 2L, n = 1, signed = FALSE, endian = "little")
    oceDebug(debug, vectorShow(cellSize))
    blankingDistance <- 1.0e-3 * readBin(d$buf[pointer2[1:2] + 37L], "integer", size = 2L, n = 1, signed = FALSE, endian = "little")
    oceDebug(debug, vectorShow(blankingDistance))

    velocityScaling <- readBin(d$buf[pointer1[1] + 61], "integer", size = 1L, endian = "little", signed = TRUE)
    oceDebug(debug, vectorShow(velocityScaling))
    velocityFactor <- 10^velocityScaling
    oceDebug(debug, vectorShow(velocityFactor))

    oceDebug(debug, "offsetOfData: ", offsetOfData, ", is this 78?\n")

    ensembleCounter <- readBin(d$buf[pointer4 + 75L],
        "integer",
        size = 4L, n = nprofiles, endian = "little"
    )

    # {{{ read velocities, beam by beam (FIXME: hard-wired for 4 beams, at the moment)
    v <- matrix(nrow = nprofiles, ncol = nbeams)
    v[, 1] <- velocityFactor * readBin(d$buf[pointer4 + 79L],
        "integer",
        size = 4L, n = nprofiles, endian = "little"
    )
    v[, 2] <- velocityFactor * readBin(d$buf[pointer4 + 83L],
        "integer",
        size = 4L, n = nprofiles, endian = "little"
    )
    v[, 3] <- velocityFactor * readBin(d$buf[pointer4 + 87L],
        "integer",
        size = 4L, n = nprofiles, endian = "little"
    )
    v[, 4] <- velocityFactor * readBin(d$buf[pointer4 + 91L],
        "integer",
        size = 4L, n = nprofiles, endian = "little"
    )
    # }}}
    oceDebug(debug, vectorShow(v))
    distance <- matrix(nrow = nprofiles, ncol = nbeams)
    message("The 'distance' value seems wrong")
    tmp <- readBin(d$buf[pointer4 + 95:98],
        "integer",
        size = 4L, n = nprofiles, endian = "little"
    )
    if (any(tmp<0.0)) message("NEGATIVE distances.......")
    tmp <- ifelse(tmp < 0.0, 2^32 + abs(tmp), tmp)
    distance[, 1] <- 0.001 * tmp

    tmp <- readBin(d$buf[pointer4 + 99:102],
        "integer",
        size = 4L, n = nprofiles, endian = "little"
    )
    tmp <- ifelse(tmp < 0.0, 2^32 + abs(tmp), tmp)
    distance[, 2] <- 0.001 * tmp

    tmp <- readBin(d$buf[pointer4 + 103:106],
        "integer",
        size = 4L, n = nprofiles, endian = "little"
    )
    tmp <- ifelse(tmp < 0.0, 2^32 + abs(tmp), tmp)
    distance[, 3] <- 0.001 * tmp

    tmp <- readBin(d$buf[pointer4 + 107:110],
        "integer",
        size = 4L, n = nprofiles, endian = "little"
    )
    tmp <- ifelse(tmp < 0.0, 2^32 + abs(tmp), tmp)
    distance[, 4] <- 0.001 * tmp
    oceDebug(debug, vectorShow(distance))


    message("The 'figureOfMerit' value seems wrong")
    figureOfMerit <- readBin(d$buf[pointer2 + 111], "integer", size = 2L, endian = "little", n = nprofiles, signed = FALSE)
    oceDebug(debug, vectorShow(d$buf[pointer2 + 111]))
    oceDebug(debug, vectorShow(figureOfMerit))

    if (debug) {
        par(mfrow = c(2, 1))
        oce.plot.ts(time, v[, 1])
        oce.plot.ts(time, distance[, 1], drawTimeRange = FALSE)
    }
    # FIXME: add coordinates, orientation, magnetometer, accelerometer, etc
    rval <- list(
        nbeams = nbeams, ncells = ncells, cellSize = cellSize,
        blankingDistance = blankingDistance,
        soundSpeed = soundSpeed,
        time = time, pressure = pressure, temperature = temperature,
        heading = heading, pitch = pitch, roll = roll,
        ensembleCounter = ensembleCounter,
        v = v, distance = distance,
        figureOfMerit = figureOfMerit
    )
    rval
} # readBottomTrack
