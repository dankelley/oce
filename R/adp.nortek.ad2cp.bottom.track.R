# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4:foldmethod=marker

# Nortek (2022 page 93 ) "6.7 _DF20BottomTrack"
readBottomTrackNEW <- function(d, debug = getOption("oceDebug")) # uses global 'd' and 'configuration'
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
    # {{{ interpretation of configuration START
    # Determine what is included. The variables with names ending in
    # 'Included' are taken from the bits withing configuration0, following
    # the C code Nortek sent on 2026-03-24 to CR and DK.
    configuration0 <- d$configuration[look[1], ]
    dataAvailable <- list()
    dataAvailable$pressure <- configuration0[1] # NOTE: Nortek code calls this bit 0, etc for rest
    dataAvailable$temperature <- configuration0[2]
    dataAvailable$compass <- configuration0[3]
    dataAvailable$tilt <- configuration0[4]
    # bit 5 (called bit 4 in Nortek code) is empty
    dataAvailable$velocity <- configuration0[6]
    dataAvailable$amplitude <- configuration0[7]
    dataAvailable$correlation <- configuration0[8]
    dataAvailable$distance <- configuration0[9]
    dataAvailable$figureOfMerit <- configuration0[10]
    dataAvailable$AHRS <- configuration0[11]
    dataAvailable$aux <- configuration0[12]
    oceDebug(debug, "based on configuration, ", vectorShow(dataAvailable))
    # }}} END interpretation of configuration
    
    # The serial number is already known from calling code, but let's read it again
    # so we can isolate this function better
    serialNumber <- readBin(d$buf[d$index[look[1]] + 5:8], "integer", size = 4L, endian = "little")
    # {{{ FIXME: these would be useful generally, so maybe compute at higher level
    pointer1 <- d$index[look] # FIXME: shouldn't we be using look here?
    pointer2 <- gappyIndex(d$index[look], 0, 2)
    pointer4 <- gappyIndex(d$index[look], 0, 4)
    # {{{ check pointers
    oceDebug(debug, vectorShow(pointer1[1:8]))
    oceDebug(debug, vectorShow(pointer2[1:8]))
    oceDebug(debug, vectorShow(pointer4[1:8]))
    # }}}
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
    # Time is in in units of 100 microseconds, hence the factor
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
    # pad1 32
    # ncells 33:34
    ncells <- readBin(d$buf[pointer2[1:2] + 33L], "integer", size = 2L, n = 1, signed = FALSE, endian = "little")
    oceDebug(debug, vectorShow(ncells))
    cellSize <- 1.0e-3 * readBin(d$buf[pointer2[1:2] + 35L], "integer", size = 2L, n = 1, signed = FALSE, endian = "little")
    oceDebug(debug, vectorShow(cellSize))
    blanking <- 1.0e-3 * readBin(d$buf[pointer2[1:2] + 37L], "integer", size = 2L, n = 1, signed = FALSE, endian = "little")
    oceDebug(debug, vectorShow(blanking))
    for (dan in seq(-10, 10)) {
        cat("dan=", dan, "\n")
        velocityScaling <- as.integer(d$buf[pointer1 + 63L + dan])
        oceDebug(debug, vectorShow(velocityScaling))
    }
    # browser()
    # Skipping quite a few things, but if ensembleCounter makes sense then we'll
    # be ok to add those things later without changing the code below.
    for (DAN in seq(-5, 5)) {
        cat("DAN=", DAN, "\n")
        ensembleCounter <- readBin(d$buf[pointer4 + 74L + DAN], "integer", size = 4L, n = nprofiles, endian = "little")
        cat(vectorShow(ensembleCounter, n = 10))
    }
    # velocity should follow this.
    oceDebug(debug, "offsetOfData: ", offsetOfData, ", is this 78?\n")

    # iv <- gappyIndex(lookIndex, offsetOfData[1] + 1L, 4L * nbeams) # 4 bytes per beam entry
    # Trial near offsetOfData
    oceDebug(debug, vectorShow(pressure[2100:2110]))
    # iv <- gappyIndex(pointer1, offsetOfData + 1, 4L * nbeams) # 4 bytes per beam entry
    # velocity <- 1.0e-3 * matrix(
    #    readBin(d$buf[iv], "integer",
    #        size = 4L, endian = "little",
    #        n = nbeams * nprofiles
    #    ),
    #    ncol = nbeams, byrow = TRUE
    # )
    # cat("  ", vectorShow(velocity[2100:2110, 1]))
    # cat("  ", vectorShow(velocity[2100:2110, 2]))
    # cat("  ", vectorShow(velocity[2100:2110, 3]))
    # cat("  ", vectorShow(velocity[2100:2110, 4]))
    oceDebug(debug, vectorShow(nbeams * nprofiles))
    oceDebug(debug, vectorShow(pointer1[1:4]))
    #browser()
    if (TRUE) {
        for (trial in seq(-30, 30)) {
            cat("making a gappyIndex with trial adjustments to starting point of", trial, "\n")
            # assuming 4 beams
            iv0 <- gappyIndex(pointer1, offsetOfData + trial, 4L * nbeams) # 4 bytes per beam entry
            v <- 1.0e-3 * matrix(
                readBin(d$buf[iv0], "integer",
                    size = 4L, endian = "little",
                    n = nbeams * nprofiles
                ),
                ncol = nbeams, byrow = TRUE
            )
            vf <- 1.0e-3 * matrix(
                readBin(d$buf[iv0], "numeric",
                    size = 4L, endian = "little",
                    n = nbeams * nprofiles
                ),
                ncol = nbeams, byrow = TRUE
            )
            # v1 <- 1e-3 * readBin(d$buf[iv0 + 0], "integer", size = 4L, endian = "little", n = nprofiles)
            # v2 <- 1e-3 * readBin(d$buf[iv0 + 4], "integer", size = 4L, endian = "little", n = nprofiles)
            # v3 <- 1e-3 * readBin(d$buf[iv0 + 8], "integer", size = 4L, endian = "little", n = nprofiles)
            # v4 <- 1e-3 * readBin(d$buf[iv0 + 12], "integer", size = 4L, endian = "little", n = nprofiles)
            oce.plot.ts(time, v[, 1], drawTimeRange = FALSE)
            mtext(paste0("trial=", trial))
            # cat("  trial ", trial, " v indices: ", paste(iv0[1:20], collapse=" "), "\n")
            # cat(" * ", vectorShow(iv0 + 4, n = 10))
            # cat(" * ", vectorShow(iv0 + 8, n = 10))
            # cat(" * ", vectorShow(iv0 + 12, n = 10))

            # oceDebug(debug, vectorShow(ivtrial, n = 8))
            # velocity <- 1.0e-3 * matrix(
            #    readBin(d$buf[iv], "integer",
            #        size = 4L, endian = "little",
            #        n = nbeams * nprofiles
            #    ),
            #    ncol = nbeams, byrow = TRUE
            # )
            print(v[2100:2103, ])
            print(vf[2100:2103, ])
            # cat("  trial=", trial, " ", vectorShow(v[2100:2200,1], n = 10))
            # cat("  trial=", trial, " ", vectorShow(v[2100:2200,2], n = 10))
            # cat("  trial=", trial, " ", vectorShow(v[2100:2200,3], n = 10))
            # cat("  trial=", trial, " ", vectorShow(v[2100:2200,4], n = 10))
            # velocity <- cbind(v1, v2, v3, v4)
            # cat("  ", vectorShow(velocity[1, ]))
            # cat("  ", vectorShow(velocity[3540, ]))
            # cat("Tests across pointer pointer offsets (iii value)\n")
            # for (iii in seq(-10, 10)) {
            #    cat("  iii: ", iii, " -> v[1,]:")
            #    cat(" ", 1e-3 * readBin(d$buf[iv[1:4] + iii], "integer", size = 4L, endian = "little", n = 1))
            #    cat(" ", 1e-3 * readBin(d$buf[iv[1:4 + 4] + iii], "integer", size = 4L, endian = "little", n = 1))
            #    cat(" ", 1e-3 * readBin(d$buf[iv[1:4 + 8] + iii], "integer", size = 4L, endian = "little", n = 1))
            #    cat(" ", 1e-3 * readBin(d$buf[iv[1:4 + 12] + iii], "integer", size = 4L, endian = "little", n = 1))
            #    cat("\n")
            # }
            # for (bark in 2400:2500) {
            #    cat("THU trial decoding d$buf[jj], where jj=pointer1[", bark, "]=", pointer1[bark], "+offsetOfData=", offsetOfData, "+j+0:3\n", sep = "")
            #    cat("Note: pressure[", bark, "]=", pressure[bark], "\n", sep = "")
            #    for (j in seq(-20, 20)) {
            #        # jj <- pointer1[bark] + offsetOfData + j + 0:3
            #        jj <- pointer1[bark] + 21 + j + 0:3 # pressure
            #        v <- readBin(d$buf[jj], "integer", size = 4, endian = "little", n = 1)
            #        # vf <- 1e-3*readBin(d$buf[jj], "numeric", size = 4, endian = "little", n = 1)
            #        # cat("j:", j, ", jj: c(", paste(jj, collapse=", "), "), v: ", v, ", vf:, ", vf, "\n", sep = "")
            #        cat("j:", j, ", jj: c(", paste(jj, collapse = ", "), "), v: ", v, "\n", sep = "")
            #    }
            # }
        }
        # message("trial=-3 looks promising for v1")
        # message("trial=-7 looks promising for v2 BUT isn't that just reexamining from trial=-3 though")
        # message("trial=-11 looks promising for v3")
    }
    distance <- 1.0e-3 * matrix(readBin(d$buf[iv], "integer", size = 4L, endian = "little", n = nbeams * nprofiles), ncol = nbeams, byrow = TRUE)
    # par(mfrow=c(2,1))
    if (FALSE) {
        if (!interactive()) pdf("ad2cp.pdf")
        oce.plot.ts(time, velocity[, 1])
        if (!interactive()) dev.off()
        message("plotted to ad2cp.pdf")
    }

    #    browser()

    stop("FIXME: early stop")



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
    # FIXME: is velocityFactor at 59 or 61?
    rval$velocityFactor <- 10^readBin(d$buf[lookIndex[1] + 59L], "integer", size = 1L, n = nprofiles, signed = TRUE, endian = "little")
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
    # NP <- length(i) # number of profiles of this type
    # NB <- rval$numberOfBeams # number of beams for v,a,q
    # oceDebug(debug, vectorShow(NP))
    # oceDebug(debug, vectorShow(NB))
    # NOTE: imos uses idx+72 for ensembleCounter
    # https://github.com/aodn/imos-toolbox/blob/e19c8c604cd062a7212cdedafe11436209336ba5/Parser/readAD2CPBinary.m#L567
    # oce_pointer = imos_pointer - 3
    i0v <<- 75L
    # ensemble counter Nortek (2017) p62
    iv <- gappyIndex(i, i0v, 4L)
    rval$ensemble <- readBin(d$buf[iv], "integer", size = 4L, n = nprofiles, endian = "little")
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
        if (nbeams < 3) {
            # if (debug > 0) {
            #    message("#beams has been read as ", NB, ", indicating a problem with the file or with oce; below is configuration")
            #    message(vectorShow(configuration0, n = 100))
            #    browser()
            # }
            nbeams <- findInConfig(configText[[1]], "GETBT", "NB")
            if (is.finite(nbeams)) {
                warning("nbeams is zero according to the Nortek 2017 file format, so we are reading it (as ", nbeams, ") from the TEXT block instead")
            } else {
                stop("cannot infer `nbeams` from the data chunks or the TEXT block")
            }
        }
        iv <- gappyIndex(i, i0v, 4L * nbeams)
        tmp <- readBin(d$buf[iv], "integer", size = 4L, n = nbeams * nprofiles, endian = "little")
        # rval$v <- rval$velocityFactor * matrix(tmp, ncol = nbeams, byrow = FALSE)
        rval$v <- rval$velocityFactor * matrix(tmp, ncol = nbeams, byrow = TRUE)
        i0v <<- i0v + 4L * nbeams
    }
    # distance.  See configuration information at Nortek (2017, Table 6.1.3,
    # p60-62) and Nortek (2022, Table 6.7, p93-94).
    if (configuration0[8]) {
        # message("read distance with i0v=", i0v)
        iv <- gappyIndex(i, i0v, 4L * nbeams)
        oceDebug(debug, "reading bottom-track distance\n")
        tmp <- readBin(d$buf[iv], "integer", size = 4L, n = nbeams * nprofiles, endian = "little")
        rval$distance <- 1e-3 * matrix(tmp, ncol = nbeams, byrow = FALSE)
        # message("FIXME DAN 2")
        i0v <<- i0v + 4L * nbeams
    }
    # figure-of-merit [Nortek 2017, Table 6.1.3, pages 60 and 62]
    if (configuration0[9]) {
        # message("read figure-of-merit with i0v=", i0v)
        iv <- gappyIndex(i, i0v, 2L * nbeams)
        oceDebug(debug, "reading bottom-track figureOfMerit: ", vectorShow(i0v))
        tmp <- readBin(d$buf[iv], "integer", size = 2L, n = nbeams * nprofiles, endian = "little", signed = FALSE)
        rval$figureOfMerit <- matrix(tmp, ncol = nbeams, byrow = FALSE)
        i0v <<- i0v + 2L * nbeams
    }
    oceDebug(debug, "readBottomTrack() END\n", unindent = 1)
    rval
} # readBottomTrack
