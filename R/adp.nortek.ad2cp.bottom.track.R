# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4:foldmethod=marker

# Nortek (2022 page 93 ) "6.7 _DF20BottomTrack"
readBottomTrackNEW <- function(d, debug = getOption("oceDebug")) # uses global 'd' and 'configuration'
{
    # id will be 0x17 for bottomTrack
    id <- 0x17
    type <- gsub(".*=", "", ad2cpCodeToName(id))
    oceDebug(debug, "readBottomTrackNEW(id=0x", as.raw(id), " or ", id, " decimal) # i.e. type=", type, " START\n", unindent = 1)
    message("next is names(d) ... is configuration there?")
    print(names(d))
    look <- which(d$id == id)
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
    # {{{ interpretation of configuration START
    # Determine what is included. The variables with names ending in
    # 'Included' are taken from the bits withing configuration0, following
    # the C code Nortek sent on 2026-03-24 to CR and DK.
    configuration0 <- d$configuration[look[1], ]
    oceDebug(debug, "local configuration: ", paste(ifelse(configuration0, "1", "0"), collapse = ""), " (shown as a bitmask)\n")
    pressureIncluded <- configuration0[1] # NOTE: Nortek code calls this bit 0, etc for rest
    temperatureIncluded <- configuration0[2]
    compassIncluded <- configuration0[3]
    tiltIncluded <- configuration0[4]
    # bit 5 (called bit 4 in Nortek code) is empty
    velocityIncluded <- configuration0[6]
    amplitudeIncluded <- configuration0[7]
    correlationIncluded <- configuration0[8]
    distanceIncluded <- configuration0[9]
    figureOfMeritIncluded <- configuration0[10]
    AHRSIncluded <- configuration0[11]
    auxIncluded <- configuration0[12]
    # Last 4 bits of this 16-bit cluster are ignored
    oceDebug(debug, "Analysis of 'configuration' bits, proceeding left-to-right:\n")
    oceDebug(debug, "  ", vectorShow(pressureIncluded, postscript = "based on configuration[1]"))
    oceDebug(debug, "  ", vectorShow(temperatureIncluded, postscript = "based on configuration[2]"))
    oceDebug(debug, "  ", vectorShow(compassIncluded, postscript = "based on configuration[3]"))
    oceDebug(debug, "  ", vectorShow(tiltIncluded, postscript = "based on configuration[4]"))
    oceDebug(debug, "  ", vectorShow(velocityIncluded, postscript = "based on configuration[6]"))
    oceDebug(debug, "  ", vectorShow(amplitudeIncluded, postscript = "based on configuration[7]"))
    oceDebug(debug, "  ", vectorShow(correlationIncluded, postscript = "based on configuration[8]"))
    oceDebug(debug, "  ", vectorShow(distanceIncluded, postscript = "based on configuration[9]"))
    oceDebug(debug, "  ", vectorShow(figureOfMeritIncluded, postscript = "based on configuration[10]"))
    oceDebug(debug, "  ", vectorShow(AHRSIncluded, postscript = "based on configuration[11]"))
    oceDebug(debug, "  ", vectorShow(auxIncluded, postscript = "based on configuration[12]"))
    # }}} END interpretation of configuration
    # The serial number is already known from calling code, but let's read it again
    # so we can isolate this function better
    serialNumber <- readBin(d$buf[d$index[look[1]] + 5:8], "integer", size = 4L, endian = "little")
    # {{{ FIXME: these would be useful generally, so maybe compute at higher level
    pointer1 <- d$index
    pointer2 <- gappyIndex(d$index, 0, 2)
    pointer4 <- gappyIndex(d$index, 0, 4)
    # }}}
    N <- length(pointer1)
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
    hsec <- 1e-4 * readBin(d$buf[pointer2 + 15], "integer", size = 2L, n = N, signed = FALSE, endian = "little")
    oceDebug(debug, vectorShow(hsec))
    time <- ISOdatetime(year, month, day, hour, min, sec + 0.01 * hsec, tz = "UTC")
    oceDebug(debug, vectorShow(time))
    soundSpeed <- 0.1 * readBin(d$buf[pointer2 + 17], "integer", size = 2L, n = N, signed = FALSE, endian = "little")
    oceDebug(debug, vectorShow(soundSpeed))
    temperature <- 0.01 * readBin(d$buf[pointer2 + 19], "integer", size = 2L, n = N, signed = TRUE, endian = "little")
    oceDebug(debug, vectorShow(temperature))
    pressure <- 0.001 * readBin(d$buf[pointer4 + 21L], "integer", size = 4L, n = N, endian = "little")
    oceDebug(debug, vectorShow(pressure))
    heading <- 0.01 * readBin(d$buf[pointer2 + 25L], "integer", size = 2L, n = N, signed = FALSE, endian = "little")
    oceDebug(debug, vectorShow(heading))
    pitch <- 0.01 * readBin(d$buf[pointer2 + 27L], "integer", size = 2L, n = N, signed = TRUE, endian = "little")
    oceDebug(debug, vectorShow(pitch))
    roll <- 0.01 * readBin(d$buf[pointer2 + 29L], "integer", size = 2L, n = N, signed = TRUE, endian = "little")
    oceDebug(debug, vectorShow(roll))
    # {{{ FIXME remove these trial sample plots
    pdf("~/ad2cp_teaching.pdf")
    par(mfrow = c(2, 2))
    oce.plot.ts(time, pressure)
    oce.plot.ts(time, heading, drawTimeRange = FALSE)
    oce.plot.ts(time, pitch, drawTimeRange = FALSE)
    abline(h = 0, col = 2)
    oce.plot.ts(time, roll, drawTimeRange = FALSE)
    abline(h = 0, col = 2)
    dev.off()
    # }}}
    beamsCoords <- d$buf[pointer1[1] + 31] # this ought never to change
    beamsCoordsBits <- as.integer(strsplit(byteToBinary(beamsCoords[1]), "")[[1]])
    nbeams <- beamsCoordsBits[3] + 2 * beamsCoordsBits[2] + 4 * beamsCoordsBits[1]
    oceDebug(debug, vectorShow(nbeams))
    stop("EARLY STOP DURING DEVELOPMENT")
    # Questions for nortek:
    #   1. Please document bottom-track as the others. Otherwise we cannot know units and scale factors.
    #   2. Please tell us more about that block at the end (window-start etc). Are all fields always present?

    # {{{ FIXME: remove next
    temperature <- 0.1 * readBin(d$buf[d$index + 20:23], "integer", size = 2L, endian = "little")
    oceDebug(debug, vectorShow(temperature))
    pressure <- 0.1 * readBin(d$buf[d$index + 24:27], "integer", size = 4L, endian = "little")
    oceDebug(debug, vectorShow(pressure))
    heading <- 0.01 * readBin(d$buf[d$index + 28:31], "integer", size = 2L, endian = "little")
    oceDebug(debug, vectorShow(heading))
    pitch <- 0.01 * readBin(d$buf[d$index + 32:35], "integer", size = 4L, endian = "little")
    oceDebug(debug, vectorShow(pitch))
    roll <- 0.01 * readBin(d$buf[d$index + 36:39], "integer", size = 2L, endian = "little")
    oceDebug(debug, vectorShow(roll))
    beamsCoords <- buf[d$index + 40]
    print(byteToBinary(beamsCoords))
    # }}}


    message("FIXME: early return from here in readBottomTrackNEW")

    return()

    rval <- list(
        configuration = configuration0,
        numberOfBeams = nbeams[look[1]],
        numberOfCells = ncells[look[1]],
        originalCoordinate = coordinateSystem[look[1]],
        oceCoordinate = coordinateSystem[look[1]],
        cellSize = cellSize[look[1]],
        nominalCorrelation = nominalCorrelation[look],
        blankingDistance = blankingDistance[look[1]],
        ensemble = ensemble[look],
        time = time[look],
        orientation = orientation[look],
        soundSpeed = soundSpeed[look],
        temperature = temperature[look], # "temperature pressure sensor"
        pressure = pressure[look],
        heading = heading[look], pitch = pitch[look], roll = roll[look],
        magnetometer = magnetometer[look, ],
        accelerometer = accelerometer[look, ],
        datasetDescription = datasetDescription[look],
        temperatureMagnetometer = temperatureMagnetometer[look],
        temperatureRTC = temperatureRTC[look],
        transmitEnergy = transmitEnergy[look],
        powerLevel = powerLevel[look]
    )
    i <- d$index[look] # pointers to "average" chunks in buf
    oceDebug(debug, vectorShow(i))
    # message(vectorShow(commonData$offsetOfData))
    # IMOS https://github.com/aodn/imos-toolbox/blob/e19c8c604cd062a7212cdedafe11436209336ba5/Parser/readAD2CPBinary.m#L561
    #  IMOS_pointer = oce_pointer - 3
    #  Q: is IMOS taking ambiguity-velocity to
    #  be 2 bytes, as for currents?  My reading
    #  of Nortek (2022 page 80) is that for
    #  _DF20BottomTrack, ambiguity-velocity is 4 bytes, whereas it is 2
    #  bytes for _currentProfileData.  See
    # https://github.com/dankelley/oce/issues/1980#issuecomment-1188992788
    # for more context on this.
    rval$velocityFactor <- 10^readBin(d$buf[lookIndex[1] + 61L], "integer", size = 1L, n = N, signed = TRUE, endian = "little")
    oceDebug(debug, vectorShow(rval$velocityFactor))
    # message(vectorShow(rval$velocityFactor))
    # Nortek (2022 page 94, 52 in zero-indexed notation)
    # IMOS uses idx+52 for ambiguityVelocity
    #   https://github.com/aodn/imos-toolbox/blob/e19c8c604cd062a7212cdedafe11436209336ba5/Parser/readAD2CPBinary.m#L558
    #   IMOS_pointer = oce_pointer - 1
    rval$ambiguityVelocity <- rval$velocityFactor * readBin(d$buf[lookIndex[1] + 53:56], "integer", size = 4L, n = 1)
    oceDebug(debug, vectorShow(rval$ambiguityVelocity))
    # message(vectorShow(rval$ambiguityVelocity))
    # NOTE: pointer is 2 bytes past pointer for e.g. burst/average
    NP <- length(i) # number of profiles of this type
    NB <- rval$numberOfBeams # number of beams for v,a,q
    oceDebug(debug, vectorShow(NP))
    oceDebug(debug, vectorShow(NB))
    # NOTE: imos uses idx+72 for ensembleCounter
    # https://github.com/aodn/imos-toolbox/blob/e19c8c604cd062a7212cdedafe11436209336ba5/Parser/readAD2CPBinary.m#L567
    # oce_pointer = imos_pointer - 3
    i0v <<- 75L
    # ensemble counter Nortek (2017) p62
    iv <- gappyIndex(i, i0v, 4L)
    rval$ensemble <- readBin(d$buf[iv], "integer", size = 4L, n = NP, endian = "little")
    # message(vectorShow(rval$ensemble))
    #<> #message(vectorShow(commonData$offsetOfData[look]))
    #<> offsetOfData <- commonData$offsetOfData[look]
    #<> #message(vectorShow(offsetOfData))
    #<> if (any(offsetOfData != offsetOfData[1])) {
    #<>     print(offsetOfData)
    #<>     stop("offsetOfData for bottom-track (printed above) are non-uniform")
    #<> }
    i0v <<- i0v + 4L
    # velocity [Nortek 2017 p60 table 6.1.3]
    if (configuration0[6]) {
        oceDebug(debug, "configuration[6] is non-zero, meaning that dataset has velocity\n")
        # message("reading v with i0v=", i0v, " (NB=", NB, ")")
        # message("FIXME: only read velo if flag is set")
        # message("about to read velo with i[1]=", i[1], ", i0v=",i0v,", NB=", NB)
        # message("configuration0: ", paste(configuration0, collapse=" "))
        # ! i0v <- i0v - 2L # test (gives v ~ -14,000 m/s)
        # ! i0v <- i0v + 2L # test (gives v ~ -1,500 and -15,000 m/s)
        oceDebug(debug, vectorShow(rval$velocityFactor))
        oceDebug(debug, vectorShow(i0v))
        if (NB < 3) {
            # if (debug > 0) {
            #    message("#beams has been read as ", NB, ", indicating a problem with the file or with oce; below is configuration")
            #    message(vectorShow(configuration0, n = 100))
            #    browser()
            # }
            NB <- findInConfig(configText[[1]], "GETBT", "NB")
            if (is.finite(NB)) {
                warning("nbeams is zero according to the Nortek 2017 file format, so we are reading it (as ", NB, ") from the TEXT block instead")
            } else {
                stop("cannot infer `nbeams` from the data chunks or the TEXT block")
            }
        }
        iv <- gappyIndex(i, i0v, 4L * NB)
        tmp <- readBin(d$buf[iv], "integer", size = 4L, n = NB * NP, endian = "little")
        # rval$v <- rval$velocityFactor * matrix(tmp, ncol = NB, byrow = FALSE)
        rval$v <- rval$velocityFactor * matrix(tmp, ncol = NB, byrow = TRUE)
        i0v <<- i0v + 4L * NB
    }
    # distance.  See configuration information at Nortek (2017, Table 6.1.3,
    # p60-62) and Nortek (2022, Table 6.7, p93-94).
    if (configuration0[8]) {
        # message("read distance with i0v=", i0v)
        iv <- gappyIndex(i, i0v, 4L * NB)
        oceDebug(debug, "reading bottom-track distance\n")
        tmp <- readBin(d$buf[iv], "integer", size = 4L, n = NB * NP, endian = "little")
        rval$distance <- 1e-3 * matrix(tmp, ncol = NB, byrow = FALSE)
        # message("FIXME DAN 2")
        i0v <<- i0v + 4L * NB
    }
    # figure-of-merit [Nortek 2017, Table 6.1.3, pages 60 and 62]
    if (configuration0[9]) {
        # message("read figure-of-merit with i0v=", i0v)
        iv <- gappyIndex(i, i0v, 2L * NB)
        oceDebug(debug, "reading bottom-track figureOfMerit: ", vectorShow(i0v))
        tmp <- readBin(d$buf[iv], "integer", size = 2L, n = NB * NP, endian = "little", signed = FALSE)
        rval$figureOfMerit <- matrix(tmp, ncol = NB, byrow = FALSE)
        i0v <<- i0v + 2L * NB
    }
    oceDebug(debug, "readBottomTrack() END\n", unindent = 1)
    rval
} # readBottomTrack
