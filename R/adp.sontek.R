## vim: tw=120 shiftwidth=4 softtabstop=4 expandtab:

#' Read a Sontek ADP File
#'
#' Read a Sontek acoustic-Dopplerprofiler file [1].
#"
#' @param despike if \code{TRUE}, \code{\link{despike}} will be used to clean
#' anomalous spikes in heading, etc.
#' @param type A character string indicating the type of instrument.
#'
#' @template adpTemplate
#'
#' @references
#' 1. Information about Sontek profilers is available at
#'
#' \url{http://www.sontek.com}.
#'
#' @author Dan Kelley and Clark Richards
#'
#' @family things related to \code{adp} data
read.adp.sontek <- function(file, from=1, to, by=1, tz=getOption("oceTz"),
                            longitude=NA, latitude=NA,
                            type=c("adp", "pcadp"),
                            monitor=FALSE, despike=FALSE, processingLog,
                            debug=getOption("oceDebug"),
                            ...)
{
    missing.to <- missing(to)
    ## In this function, comments in [] refer to logical page number of ADPManual_v710.pd; add 14 for file page number
    profileStart <- NULL # prevent scope warning from rstudio; defined later anyway
    bisectSontekAdp<- function(buf, t.find, add=0, debug=0) {
        oceDebug(debug, "bisectSontekAdp(t.find=", format(t.find), ", add=", add, ", debug=", debug, ")\n")
        len <- length(profileStart)
        lower <- 1
        upper <- len
        passes <- floor(10 + log(len, 2)) # won't need this many; only do this to catch coding errors
        for (pass in 1:passes) {
            middle <- floor( (upper + lower) / 2 )
            oceDebug(debug-1, "pass=", pass, "middle=", middle, "\n")
            oceDebug(debug-1, "data=", buf[profileStart[middle]+seq(0, 100, 1)], "\n")
            ##oceDebug(debug-1, "profileStart=", profileStart[1:5], "...; profileStart2=", profileStart2[1:5], "...\n")
            oceDebug(debug-1, "profileStart=", profileStart[1:5], "\n")
            ##year <- readBin(buf[profileStart2[middle]+18], "integer", size=2, signed=FALSE, endian="little")
            year <- readBin(buf[profileStart[middle]+18:19], "integer", size=2, signed=FALSE, endian="little")
            day <- as.integer(buf[profileStart[middle]+20])
            month <- as.integer(buf[profileStart[middle]+21])
            min <- as.integer(buf[profileStart[middle]+22])
            hour <- as.integer(buf[profileStart[middle]+23])
            ## SIG p82 C code suggests sec100 comes before second.
            sec100 <- as.integer(buf[profileStart[middle]+24])     # FIXME: determine whether this is 1/100th second
            sec <- as.integer(buf[profileStart[middle]+25])
            t <- ISOdatetime(year, month, day, hour, min, sec+sec100/100, tz=tz)
            oceDebug(debug, "t=", format(t),
                      " [year=", year, " month=", month, " day=", day, " hour=", hour, " sec=", sec, "sec100=", sec100, "]\n")
            if (t.find < t)
                upper <- middle
            else
                lower <- middle
            if (upper - lower < 2)
                break
            oceDebug(debug, "middle=", middle, " lower=", lower, " upper=", upper, " pass=", pass, " of max=", passes, "\n")
        }
        middle <- middle + add          # may use add to extend before and after window
        if (middle < 1)
            middle <- 1
        if (middle > len)
            middle <- len
        t <- ISOdatetime(readBin(buf[profileStart[middle]+18:19], "integer", size=2, signed=FALSE, endian="little"),  # year
                         as.integer(buf[profileStart[middle]+21]), # month
                         as.integer(buf[profileStart[middle]+20]), # day
                         as.integer(buf[profileStart[middle]+23]), # hour
                         as.integer(buf[profileStart[middle]+22]), # min
                         as.integer(buf[profileStart[middle]+25]), # sec
                         tz=tz)
        oceDebug(debug, "result: t=", format(t), " at profileStart[", middle, "]=", profileStart[middle], "\n")
        return(list(index=middle, time=t)) # index is within vsd
    }
    oceDebug(debug, "read.adp.sontek(...,from=", from, ",to=", if (missing.to) "(missing)" else to, ",by=", by, "type=", type, "...)\n")
    ##parameters <- list(profile.byte1 = 0xa5, profile.byte2=0x10, profile.headerLength=80)
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
    res <- new("adp")
    seek(file, 0, "end")
    fileSize <- seek(file, 0, "start")
    oceDebug(debug, "file", filename, "has", fileSize, "bytes\n")
    buf <- readBin(file, what="raw", n=fileSize, endian="little")
    ## See if there is a header here.  (May not be, since the file may have been chopped
    ## into parts by a deck unit.)
    frequency <- NA
    if (buf[1] == 0x10 && buf[2] == 0x02 && 96 == readBin(buf[3:4], "integer", n=1, size=2, signed=FALSE, endian="little")) {
        oceDebug(debug, "have a header, but ignoring it for now\n");
        ## Comments like [p83] refer to logical page number of ADPManual_v710.pd; add 14 for file page number
        cpuSoftwareVerNum <- as.integer(buf[13]) / 10 # CPUSoftwareVerNum [p83]
        dspSoftwareVerNum <- as.integer(buf[14]) / 10 # DSPSoftwareVerNum [p83]
        boardRev <- readBin(buf[15], "character", n=1, size=1, signed=TRUE) # BoardRev [p83]
        serialNumber <- readBin(buf[16:25], "character")
        oceDebug(debug, "serialNumber=", serialNumber, "\n")
        adp.type <- readBin(buf[26], what="integer", n=1, size=1) # 0-3; 1=1.5; 2-750; 3-500 [p83]
        oceDebug(debug, "adp.type=", adp.type, "\n")
        frequency <- switch(adp.type+1, 3000, 1500, 750, 500, 250)
        oceDebug(debug, "frequency=", frequency, "\n")
        nbeams <- as.integer(buf[27])
        oceDebug(debug, "nbeams=", nbeams, "\n")
        beam.geometry <- as.integer(buf[28])
        oceDebug(debug, "beam.geometry=", beam.geometry,
                  "; 0 (2 beams); 1 (3 beams), 2 (4 beams with 1 vertical), 3 (4 beams, Janus)\n")
        slant.angle <- readBin(buf[29:30], "integer", n=1, size=2, signed=FALSE) / 10
        oceDebug(debug, "slant.angle=", slant.angle, "\n")
        orientation <- switch(as.integer(buf[31]) + 1, "down", "up", "side")
        oceDebug(debug, "orientation=", orientation, "\n")
        compass.installed <- switch(as.integer(buf[32]) + 1, FALSE, TRUE) # nolint (variable not used)
        recorder.installed <- switch(as.integer(buf[33]) + 1, FALSE, TRUE) # nolint (variable not used)
        temp.installed <- switch(as.integer(buf[34]) + 1, FALSE, TRUE) # nolint (variable not used)
        press.installed <- switch(as.integer(buf[35]) + 1, FALSE, TRUE) # nolint (variable not used)
        ## 36 = spare
        ## 37 int[16], so I guess 2-byte ints, signed?
    } else {
        cpuSoftwareVerNum <- dspSoftwareVerNum <- boardRev <-
            adp.type <- nbeams <- slant.angle <- orientation <-
                compass.installed <- recorder.installed <- temp.installed <- press.installed <- "?"
    }
    ##profileStart <- .Call("match2bytes", buf, parameters$profile.byte1, parameters$profile.byte2, FALSE)
    profileStart <- .Call("ldc_sontek_adp", buf, 0, 0, 0, 1, -1) # no ctd, no gps, no bottom-track; pcadp; all data
    profileStart2 <- sort(c(profileStart, profileStart+1)) # use this to subset for 2-byte reads
    oceDebug(debug, "first 10 profileStart:", profileStart[1:10], "\n")
    oceDebug(debug, "first 100 bytes of first profile:", paste(buf[profileStart[1]:(99+profileStart[1])], collapse=" "), "\n")
    ## Examine the first profile to get numberOfBeams, etc.
    headerLength <- 80                 # FIXME: should this really be hard-wired??
    s <- profileStart[1]
    ## Only read (important) things that don't change profile-by-profile
    numberOfBeams <- as.integer(buf[s+26])
    oceDebug(debug, "numberOfBeams=", numberOfBeams, "\n")
    if (numberOfBeams != 3)
        stop("there should be 3 beams, but the file indicates ", numberOfBeams)
    orientation <- as.integer(buf[s+27])
    oceDebug(debug, "orientation=", orientation, "\n")
    temp.mode <- as.integer(buf[s+28])
    oceDebug(debug, "temp.mode=", temp.mode, "\n")
    originalCoordinate <- as.integer(buf[s+29])
    oceDebug(debug, "originalCoordinate=", originalCoordinate, "\n")
    numberOfCells <- readBin(buf[s+30:31], "integer", n=1, size=2, endian="little", signed=FALSE)
    oceDebug(debug, "numberOfCells=", numberOfCells, "\n")
    cellSize <- readBin(buf[s+32:33], "integer", n=1, size=2, endian="little", signed=FALSE) / 100 # metres
    oceDebug(debug, "cellSize=", cellSize, "m\n")
    blankingDistance <- readBin(buf[s+34:35], "integer", n=1, size=2, endian="little", signed=FALSE) / 100 # metres
    oceDebug(debug, "blankingDistance=", blankingDistance, "m\n")
    soundSpeed <- readBin(buf[s+60:61], "integer", n=1, size=2, endian="little", signed=FALSE) / 10
    oceDebug(debug, "soundSpeed=", soundSpeed, "m/s\n")
    profilesInFile <- length(profileStart)
    ##id <- buf[profileStart]
    bytesPerProfile <- diff(profileStart[1:2])
    oceDebug(debug, "bytesPerProfile=", bytesPerProfile, "\n")

    ## File time range and deltat
    measurementStart <- ISOdatetime(readBin(buf[profileStart[1]+18:19], "integer", n=1, size=2, signed=FALSE, endian="little"), # year
                                    as.integer(buf[profileStart[1]+21]), # month
                                    as.integer(buf[profileStart[1]+20]), # day
                                    as.integer(buf[profileStart[1]+23]), # hour
                                    as.integer(buf[profileStart[1]+22]), # min
                                    as.integer(buf[profileStart[1]+25])+0.01*as.integer(buf[profileStart[1]+24]), # sec (decimal)
                                    tz=tz)
    oceDebug(debug, "measurementStart=", format(measurementStart), "\n")
    oceDebug(debug, "length(profileStart)=", length(profileStart), " [FIXME: if to not given, use this??]\n")

    measurementEnd <- ISOdatetime(readBin(buf[profileStart[profilesInFile]+18:19], "integer", n=1, size=2, signed=FALSE, endian="little"), # year
                                   as.integer(buf[profileStart[profilesInFile]+21]), # month
                                   as.integer(buf[profileStart[profilesInFile]+20]), # day
                                   as.integer(buf[profileStart[profilesInFile]+23]), # hour
                                   as.integer(buf[profileStart[profilesInFile]+22]), # min
                                   as.integer(buf[profileStart[profilesInFile]+25])+0.01*as.integer(buf[profileStart[1]+24]), # sec (decimal)
                                   tz=tz)
    oceDebug(debug, "sampling.end=", format(measurementEnd), "\n")
    measurementDeltat <- as.numeric(ISOdatetime(readBin(buf[profileStart[2]+18:19], "integer", n=1, size=2, signed=FALSE, endian="little"), # year
                                                 as.integer(buf[profileStart[2]+21]), # month
                                                 as.integer(buf[profileStart[2]+20]), # day
                                                 as.integer(buf[profileStart[2]+23]), # hour
                                                 as.integer(buf[profileStart[2]+22]), # min
                                                 as.integer(buf[profileStart[2]+25])+0.01*as.integer(buf[profileStart[1]+24]), # sec
                                                 tz=tz)) - as.numeric(measurementStart)
    oceDebug(debug, "sampling.deltat=", format(measurementDeltat), "\n")

    ## Window data buffer, using bisection in case of a variable number of vd between sd pairs.
    if (inherits(from, "POSIXt")) {
        if (!inherits(to, "POSIXt"))
            stop("if 'from' is POSIXt, then 'to' must be, also")
        fromPair <- bisectSontekAdp(buf, from, -1, debug-1)
        from <- fromIndex <- fromPair$index
        toPair <- bisectSontekAdp(buf, to, 1, debug-1)
        to <- to.index <- toPair$index
        oceDebug(debug, "  from=", format(fromPair$t), " yields profileStart[", fromIndex, "]\n",
                  "  to  =", format(toPair$t),   " yields profileStart[", to.index, "]\n",
                  "  by=", by, "s\n",
                  "profileStart[1:10]=", profileStart[1:10], "\n",
                  "profileStart[", fromPair$index, "]=", profileStart[fromPair$index], "at time", format(fromPair$t), "\n",
                  "profileStart[",   toPair$index, "]=", profileStart[  toPair$index], "at time", format(  toPair$t), "\n")
        ## FIXME next line reads year incorrectly
        two.times <- ISOdatetime(readBin(buf[profileStart[1:2]+18:19], "integer", size=2, signed=FALSE, endian="little"),  # year
                                 as.integer(buf[profileStart[1:2]+21]), # month
                                 as.integer(buf[profileStart[1:2]+20]), # day
                                 as.integer(buf[profileStart[1:2]+23]), # hour
                                 as.integer(buf[profileStart[1:2]+22]), # min
                                 as.integer(buf[profileStart[1:2]+25])+0.01*as.integer(buf[profileStart[1]+24]), # sec
                                 tz=tz)
        dt <- as.numeric(difftime(two.times[2], two.times[1], units="secs"))
        oceDebug(debug, "dt=", dt, "s; at this stage, by=", by, "(not interpreted yet)\n")
        profileStart <- profileStart[profileStart[fromIndex] < profileStart & profileStart < profileStart[to.index]]
        if (is.character(by))
            by <- floor(0.5 + ctimeToSeconds(by) / dt)
        oceDebug(debug, "by=", by, "profiles (after change)\n")
        profileStart <- profileStart[seq(1, length(profileStart), by=by)]
        oceDebug(debug, "dt=", dt, "\n", "by=", by, "profileStart[1:10] after indexing:", profileStart[1:10], "\n")
    } else {
        fromIndex <- from
        if (missing.to)
            to <- length(profileStart)
        to.index <- to
        if (to.index < 1 + fromIndex)
            stop("need more separation between from and to")
        if (is.character(by))
            stop("cannot have string for 'by' if 'from' and 'to' are integers")
        profileStart <- profileStart[seq(from=from, to=to, by=by)]
        oceDebug(debug, "profileStart[1:10] after indexing:", profileStart[1:10], "\n")
    }
    profilesToRead <- length(profileStart)
    oceDebug(debug, "profilesInFile=", profilesInFile, "; profilesToRead=", profilesToRead, "\n")
    profileStart2 <- sort(c(profileStart, profileStart+1)) # use this to subset for 2-byte reads
    year   <- readBin(buf[profileStart2 + 18], "integer", n=profilesToRead, size=2, endian="little", signed=FALSE)
    day    <- as.integer(buf[profileStart + 20])
    month  <- as.integer(buf[profileStart + 21])
    minute <- as.integer(buf[profileStart + 22])
    hour   <- as.integer(buf[profileStart + 23])
    sec100 <- as.integer(buf[profileStart + 24])
    second <- as.integer(buf[profileStart + 25])
    time <- ISOdatetime(year, month, day, hour, minute, second+sec100/100, tz=tz)
    rm(year, day, month, minute, hour, sec100, second)
    temperature <- readBin(buf[profileStart2 + 46], "integer", n=profilesToRead, size=2, endian="little", signed=TRUE) / 100
    oceDebug(debug, "temperature[1:10]=", temperature[1:10], "\n")
    pressure <- readBin(buf[profileStart2 + 48], "integer", n=profilesToRead, size=2, endian="little", signed=FALSE) / 100
    ## FIXME: pressure (+else?) is wrong.  Need to count bytes on p84 of ADPManual to figure out where to look [UGLY]
    oceDebug(debug, "pressure[1:10]=", pressure[1:10], "\n")
    heading <- readBin(buf[profileStart2 + 40], "integer", n=profilesToRead, size=2, endian="little", signed=TRUE) / 10
    pitch <- readBin(buf[profileStart2 + 42], "integer", n=profilesToRead, size=2, endian="little", signed=TRUE) / 10
    roll <- readBin(buf[profileStart2 + 44], "integer", n=profilesToRead, size=2, endian="little", signed=TRUE) / 10

    oceDebug(debug, "time[1:10]=", format(time[1:10]), "\n")
    v <- array(numeric(), dim=c(profilesToRead, numberOfCells, numberOfBeams))
    a <- array(raw(), dim=c(profilesToRead, numberOfCells, numberOfBeams))
    q <- array(raw(), dim=c(profilesToRead, numberOfCells, numberOfBeams))
    nd <- numberOfCells * numberOfBeams
    oceDebug(debug, "nd=", nd, ";  headerLength=", headerLength, "\n")
    if (type == "pcadp") {
        nbeamMax <- 4                 # Max numberOfBeams, not actual number
        headerLength <- headerLength + 2 * (8 + nbeamMax) + 2 * nbeamMax + nbeamMax
        ## Below is C code from Sontek, for pulse-coherent adp (2-byte little-endian
        ## integers).  FIXME: should perhaps read these things, but this is not a
        ## high priority, since in the data file for which the code was originally
        ## developed, all distances were set to 123 mm and all velocities to
        ## 9999 mm/s, suggestive of insignificant, place-holder values.
        ##
        ##typedef struct
        ##{
        ##  unsigned int  ResLag;             /* in mm     Used for single cell    */
        ##  unsigned int  ResUa;              /* in mm/s   Ambiguity resolution    */
        ##  unsigned int  ResStart;           /* in mm     Position of resolve     */
        ##  unsigned int  ResLength;          /* in mm     cell                    */
        ##  unsigned int  PrfLag;             /* in mm     Used for full profile   */
        ##  unsigned int  PrfUa;              /* in mm/s                           */
        ##  unsigned int  PrfStart;           /* in mm     Position/Length of first*/
        ##  unsigned int  PrfLength;          /* in mm     cell in profile         */
        ##  unsigned int  Range[MAX_BEAMS];   /* in mm     Range to boundary       */
        ##           int  Ures[MAX_BEAMS];    /* in mm/s   Velocities from Resolve */
        ##                                    /*           lag                     */
        ##  unsigned char Cres[MAX_BEAMS];    /* in %      Correlations from       */
        ##                                    /*           resolve lag             */
        ##} PCrecordType;
    }
    velocityScale <- 1e-3
    if (profilesToRead > 0) {
        for (i in 1:profilesToRead) {
            v_ <- velocityScale * matrix(readBin(buf[profileStart[i] + headerLength + seq(0, 2*nd-1)],
                                                 "integer", n=nd, size=2, signed=TRUE, endian="little"),
                                         ncol=numberOfBeams, byrow=FALSE)
            a_ <- matrix(buf[profileStart[i] + headerLength + 2*nd + seq(0, nd-1)], ncol=numberOfBeams, byrow=FALSE)
            q_ <- matrix(buf[profileStart[i] + headerLength + 3*nd + seq(0, nd-1)], ncol=numberOfBeams, byrow=FALSE)
            for (b in 1:numberOfBeams) {
                ## FIXME: probably could be speeded up
                v[i, , b] <- v_[, b]
                a[i, , b] <- a_[, b]
                q[i, , b] <- q_[, b]
            }
            if (monitor) {
                cat(".")
                if (!(i %% 50))
                    cat(i, "\n")
            }
        }
        rm(v_, a_, q_)
        if (monitor)
            cat("\nRead", profilesToRead,  "of the", profilesInFile, "profiles in", filename, "\n")
        if (type == "pcadp")
            v <- v / 10                # it seems pcadp is in 0.1mm/s
    } else {
        stop("please request to read *some* profiles")
    }
    ## interpolate headings (which may be less frequent than profiles ... FIXME: really???)
    nheading <- length(heading)
    nv <- dim(v)[1]
    if (nheading != nv) {
        warning("read.adp.sontek() interpolating ", nheading, " heading/pitch/roll values to the ", nv, " velocity profiles")
        oceDebug(debug, "BEFORE: length(heading)=", nheading, ", nv=", nv, "\n")
        ##xout <- seq(1, nheading, length.out=nv)
        heading <- approx(1:nheading, heading, seq(1, nheading, length.out=nv))$y
        ##print(data.frame(xout=xout, heading=heading))
        pitch <- approx(1:nheading, pitch, seq(1, nheading, length.out=nv))$y
        roll <- approx(1:nheading, roll, seq(1, nheading, length.out=nv))$y
        oceDebug(debug, "AFTER:  length(heading)=", length(heading), "\n")
    }
    res@data <- list(v=v, a=a, q=q,
                     distance=seq(blankingDistance, by=cellSize, length.out=numberOfCells),
                     time=time,
                     temperature=temperature,
                     pressure=pressure,
                     heading=heading, pitch=pitch, roll=roll)
    oceDebug(debug, "slant.angle=", slant.angle, "; type=", type, "\n")
    beamAngle <- if (slant.angle == "?") 25 else slant.angle
    res@metadata$manufacturer <- "sontek"
    res@metadata$type <- type
    res@metadata$filename <- filename
    res@metadata$serialNumber <- if (exists('serialNumber')) serialNumber else "?"
    res@metadata$longitude <- longitude
    res@metadata$latitude <- latitude
    res@metadata$numberOfSamples <- dim(v)[1]
    res@metadata$numberOfCells <- dim(v)[2]
    res@metadata$numberOfBeams <- dim(v)[3]
    res@metadata$velocityResolution <- velocityScale
    res@metadata$velocityMaximum <- velocityScale * 2^15
    res@metadata$measurementStart <- measurementStart
    res@metadata$measurementEnd <- measurementEnd
    res@metadata$measurementDeltat <- measurementDeltat
    res@metadata$frequency <- frequency
    res@metadata$cpuSoftwareVerNum <- cpuSoftwareVerNum
    res@metadata$dspSoftwareVerNum <- dspSoftwareVerNum
    res@metadata$boardRev <- boardRev
    res@metadata$originalCoordinate <- c("beam", "xyz", "enu", "other")[originalCoordinate+1]
    res@metadata$oceCoordinate <- c("beam", "xyz", "enu", "other")[originalCoordinate+1]
    res@metadata$beamAngle <- beamAngle
    res@metadata$oceBeamUnspreaded <- FALSE
    res@metadata$orientation <- if (1==orientation) "upward" else "downward"
    if (numberOfBeams == 3) {
        if (res@metadata$orientation == "upward") {
            ##S  <- 1 / (3 * sin(25 * pi / 180))             # 0.7887339
            ##CS <- 1 / cos(30*pi/180) / sin(25*pi/180) / 2  # 1.366127 (30deg from 3-beam pattern)
            ##C  <- 1 / (3 * cos(25 * pi / 180))             # 0.3677926
            S  <- 1 / (3 * sin(beamAngle * pi / 180)) # 0.7887339
            CS <- 1 / cos(30*pi/180) / sin(beamAngle*pi/180) / 2 # 1.366127 (30deg from 3-beam pattern)
            C  <- 1 / (3 * cos(beamAngle * pi / 180))             # 0.3677926
            ## FIXME: check up and down; also read it and check
            res@metadata$transformationMatrix <- matrix(c(2*S,  -S,  -S,
                                                            0, -CS,  CS,
                                                            C,   C,   C),
                                                    nrow=3, byrow=TRUE)
        } else {
            S  <- 1 / (3 * sin(beamAngle * pi / 180)) # 0.7887339
            CS <- 1 / cos(30*pi/180) / sin(beamAngle*pi/180) / 2 # 1.366127 (30deg from 3-beam pattern)
            C  <- 1 / (3 * cos(beamAngle * pi / 180))             # 0.3677926
            ## warning("*****FIXME: check up and down; also read it and check*****")
            res@metadata$transformationMatrix <- matrix(c(2*S,  -S,  -S,
                                                            0,  CS, -CS,
                                                           -C,  -C,  -C),
                                                    nrow=3, byrow=TRUE)
        }
        ## For later use, RC says that the PC-ADP uses
        ## T =  2.576  -1.288  -1.288
        ##      0.000  -2.230   2.230
        ##      0.345   0.345   0.345
        ## and these are by the same formulae, with 25 switched to 15 (different beamAngle)
    } else
        stop("can only handle 3-beam devices")
    res@metadata$units <- list(v="m/s", distance="m")
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    hitem <- processingLogItem(processingLog)
    res@processingLog <- hitem
    res
}

sontek.time <- function(t, tz=getOption("oceTz"))
{
    minute <- bcdToInteger(t[1])
    second <- bcdToInteger(t[2])
    day <- bcdToInteger(t[3])
    hour <- bcdToInteger(t[4])
    year <- bcdToInteger(t[5])
    year <- year + if (year >= 90) 1900 else 2000 # page 51 of System Integrator Guide
    month <- bcdToInteger(t[6])
    milliseconds <- readBin(t[7:8], "integer", n=1, size=2, endian="little", signed=FALSE)
    ISOdatetime(year, month, day, hour, minute, second+milliseconds/1000, tz=tz)
}

#' Read a serial Sontek ADP file
#'
#' Read a Sontek acoustic-Dopplerprofiler file, in a serial form that
#' is possibly unique to Dalhousie University.
#'
#' @param beamAngle angle between instrument axis and beams, in degrees.
#' @param type A character string indicating the type of instrument.
#' @param orientation Ooptional character string specifying the orientation of the
#' sensor, provided for those cases in which it cannot be inferred from the
#' data file.  The valid choices are \code{"upward"}, \code{"downward"}, and
#' \code{"sideward"}.
#'
#' @template adpTemplate
#'
#' @author Dan Kelley and Clark Richards
#'
#' @family things related to \code{adp} data
read.adp.sontek.serial <- function(file, from=1, to, by=1, tz=getOption("oceTz"),
                                   longitude=NA, latitude=NA,
                                   type=c("adp", "pcadp"),
                                   beamAngle=25, orientation,
                                   monitor=FALSE, processingLog,
                                   debug=getOption("oceDebug"))
{
    ## Data format is described in
    ##   SonTek/YSI
    ##   ADPManual_v710.pdf
    ## A3. Profile Header/CTD/GPS/Bottom Track,/SonWave/Profile Data Structures
    bisectAdpSontekSerial <- function(buf, t.find, add=0, tz="UTC", debug=0) {
        oceDebug(debug, "bisectAdpSontekSerial(t.find=", format(t.find), ", add=", add, "\n")
        len <- length(p)
        lower <- 1
        upper <- len
        passes <- floor(10 + log(len, 2)) # won't need this many; only do this to catch coding errors
        for (pass in 1:passes) {
            middle <- floor( (upper + lower) / 2 )
            year   <- readBin(buf[p[middle] + 18:19], what="integer", n=1, size=2, signed=FALSE, endian="little")
            day    <- readBin(buf[p[middle] + 20], what="integer", n=1, size=1, signed=FALSE)
            month  <- readBin(buf[p[middle] + 21], what="integer", n=1, size=1, signed=FALSE)
            min    <- readBin(buf[p[middle] + 22], what="integer", n=1, size=1, signed=FALSE)
            hour   <- readBin(buf[p[middle] + 23], what="integer", n=1, size=1, signed=FALSE)
            sec100 <- readBin(buf[p[middle] + 24], what="integer", n=1, size=1, signed=FALSE)
            sec    <- readBin(buf[p[middle] + 25], what="integer", n=1, size=1, signed=FALSE)
            t <- ISOdatetime(year=year, month=month, day=day, hour=hour, min=min, sec=sec + sec100/100, tz=tz)
            oceDebug(debug, "t=", format(t), " y=", year, " m=", month, " d=", format(day, width=2),
                      " h=", format(hour, width=2),
                      " m=", format(min, width=2),
                      " s=", format(sec+sec100/100, width=4),
                      "| pass", format(pass, width=2), "/", passes, "| middle=", middle,
                      "(", format(middle/upper*100, digits=4), "%)\n", sep="")
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
        t <- ISOdatetime(readBin(buf[p[middle]+18:19], "integer", size=2, signed=FALSE, endian="little"),
                         as.integer(buf[p[middle]+21]), # month
                         as.integer(buf[p[middle]+20]), # day
                         as.integer(buf[p[middle]+23]), # hour
                         as.integer(buf[p[middle]+22]), # min
                         as.integer(buf[p[middle]+25])+0.01*as.integer(buf[p[middle]+24]),
                         tz=tz)
        oceDebug(debug, "result: t=", format(t), " at d[", middle, "]=", p[middle], "\n")
        return(list(index=middle, time=t))
    }
    oceDebug(debug, paste("read.adp.sontek.serial(file[1]=\"", file[1],
                           "\", from=", from,
                           if (missing(to)) "to," else sprintf(", to=%s, ", format(to)),
                           ", by=", by,
                           ", latitude=", latitude, ", longitude=", longitude,
                           ", monitor=", monitor,
                           ", processingLog=(not shown), debug=", debug, ") {\n", sep=""), unindent=1)
    nfile <- length(file)
    if (nfile > 1) {
        ## handle multiple files
        oceDebug(debug, "handling multiple files\n")
        buf <- NULL
        for (i in 1:nfile) {
            if (monitor)
                cat("\"", file[i], "\" ", sep="")
            thisFile <- file(file[i], "rb")
            seek(thisFile, 0, "end", rw="read")
            fileSize <- seek(thisFile, 0, origin="start", rw="read")
            if (monitor)
                cat(fileSize, "bytes\n")
            buf <- c(buf, readBin(thisFile, what="raw", n=fileSize, endian="little"))
            close(thisFile)
        }
        filename <- paste("(\"", file[i], "\", ...)", sep="")
    } else {
        ## handle single file (which might be a connection, etc)
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
        ## read whole file into buffer
        seek(file, 0, "end", rw="read")
        fileSize <- seek(file, 0, origin="start", rw="read")
        oceDebug(debug, "filesize=", fileSize, "\n")
        buf <- readBin(file, what="raw", n=fileSize, endian="little")
    }
    p <- .Call("ldc_sontek_adp", buf, 0, 0, 0, 0, -1) # no ctd, no gps, no bottom-track; all data
    ## read some unchanging things from the first profile only
    serialNumber <- paste(readBin(buf[p[1]+4:13], "character", n=10, size=1), collapse="")
    numberOfBeams <- readBin(buf[p[1]+26], "integer", n=1, size=1, signed=FALSE)
    if (missing(orientation)) {
    orientation <- readBin(buf[p[1]+27], "integer", n=1, size=1, signed=FALSE)
    if (orientation == 0)
        orientation <- "upward"
    else if (orientation == 1)
        orientation  <- "downward"
    else if (orientation == 2)
        orientation <- "sideward"
    else
        stop("orientation=", orientation, "but must be 0 (upward), 1 (downward), or 2 (sideward)")
    } else {
        if (orientation != "upward" && orientation != "downward")
            stop("orientation \"", orientation, " is not allowed; try \"upward\" or \"downward\"")
    }
    ## 28 is tempmode
    ## coord.system 0=beam 1=XYZ 2=ENU
    originalCoordinate <- readBin(buf[p[1]+29], "integer", n=1, size=1, signed=FALSE)
    if (originalCoordinate == 0)
        originalCoordinate <- "beam"
    else if (originalCoordinate == 1)
        originalCoordinate <- "xyz"
    else if (originalCoordinate == 2)
        originalCoordinate <- "enu"
    else
        stop("originalCoordinate=", originalCoordinate, "but must be 0 (beam), 1 (xyz), or 2 (enu)")
    numberOfCells <- readBin(buf[p[1]+30:31], "integer", n=1, size=2, signed=FALSE, endian="little")
    cellSize <- 0.01*readBin(buf[p[1]+32:33], what="integer", n=1, size=2, signed=FALSE, endian="little")
    blankingDistance <- 0.01*readBin(buf[p[1]+34:35], what="integer", n=1, size=2, signed=FALSE)
    distance <- blankingDistance + cellSize * seq(from=0.5, by=cellSize, length.out=numberOfCells)
    ## trim, if from and to are integers
    if (!missing(to)) {
        if (is.numeric(from) && is.numeric(to) && is.numeric(by)) {
            if (from < 1)
                stop("from=", from, " but must exceed 1")
            if (to < 1)
                stop("to=", to, " but must exceed 1")
            if (by < 1)
                stop("by=", by, " but must exceed 1")
            if (to <= from)
                stop("from=", from, " must exceed to=", to)
            p <- p[seq(from=from, to=to, by=by)]
        } else {
            if (inherits(from, "POSIXt")) {
                if (!inherits(to, "POSIXt"))
                    stop("if 'from' is POSIXt, then 'to' must be, also")
                if (!is.numeric(by)) {
                    warning("'by' must be numeric")
                    by <- 1
                }
                fromPair <- bisectAdpSontekSerial(buf, from, add=-1, tz=tz, debug=debug-1)
                from <- fromIndex <- fromPair$index
                toPair <- bisectAdpSontekSerial(buf, to, add=1, tz=tz, debug=debug-1)
                to <- to.index <- toPair$index
                oceDebug(debug, "from=", format(fromPair$t), " yields p[", fromIndex, "]\n",
                          "  to  =", format(toPair$t), "yields p[", to.index, "]\n",
                          "  by=", by, "(not yet decoded)\n",
                          vectorShow(p, "p:"),
                          "  p[", fromPair$index, "]=", p[fromPair$index], "at time", format(fromPair$t), "\n",
                          "  p[",   toPair$index, "]=", p[  toPair$index], "at time", format(  toPair$t), "\n")
                p <- p[seq(from=from, to=to, by=by)]
            }
        }
    }
    np <- length(p)
    pp <- sort(c(p, p+1)) # for 2-byte addressing ('int' in the Sontek docs)
    ##pppp <- sort(c(p, p+1, p+2, p+3)) # for 4-byte addressing ('long' in the Sontek docs)

    ## read profile-specific things profile by profile
    ##profile.number <- readBin(buf[pppp+14], "integer", n=np, size=4)
    ## FIXME: should check that profile number is monotonic ... it may
    ## help us with daily blank-outs, also!
    year <- readBin(buf[pp+18], "integer", n=np, size=2, signed=FALSE)
    day <- readBin(buf[p+20], "integer", n=np, size=1, signed=FALSE)
    month <- readBin(buf[p+21], "integer", n=np, size=1, signed=FALSE)
    min <- readBin(buf[p+22], "integer", n=np, size=1, signed=FALSE)
    hour <- readBin(buf[p+23], "integer", n=np, size=1, signed=FALSE)
    sec100 <- readBin(buf[p+24], "integer", n=np, size=1, signed=FALSE)
    sec <- readBin(buf[p+25], "integer", n=np, size=1, signed=FALSE)
    time <- ISOdatetime(year, month, day, hour, min, sec+0.01*sec100, tz=tz)
    rm(year, day, month, min, hour, sec100, sec) # possibly this space will come in handy
    heading <- 0.1 * readBin(buf[pp+40], "integer", n=np, size=2, signed=TRUE)
    pitch <- 0.1 * readBin(buf[pp+42], "integer", n=np, size=2, signed=TRUE)
    roll <- 0.1 * readBin(buf[pp+44], "integer", n=np, size=2, signed=TRUE)
    temperature <- 0.01 * readBin(buf[pp+46], "integer", n=np, size=2, signed=TRUE)
    v <- array(numeric(), dim=c(np, numberOfCells, numberOfBeams))
    a <- array(raw(), dim=c(np, numberOfCells, numberOfBeams))
    q <- array(raw(), dim=c(np, numberOfCells, numberOfBeams))
    ndata <- numberOfCells * numberOfBeams
    i1 <- seq(1, ndata)
    i2 <- seq(1, 2*ndata)
    for (ip in 1:np) {
        p0 <- p[ip] + 79
        v_ <- matrix(0.001*readBin(buf[p0 + i2], "integer", endian="little", n=ndata, size=2, signed=TRUE),
                     ncol=numberOfBeams, byrow=FALSE)
        p0 <- p0 + 2 * ndata
        ## NOTE: q is std-dev; need to multiply by 0.001 to get in m/s
        q_ <- matrix(buf[p0 + i1], ncol=numberOfBeams, byrow=FALSE)
        p0 <- p0 + ndata
        a_ <- matrix(buf[p0 + i1], ncol=numberOfBeams, byrow=FALSE)
        for (b in 1:numberOfBeams) {
            v[ip, , b] <- v_[, b]
            a[ip, , b] <- a_[, b]
            q[ip, , b] <- q_[, b]
        }
        if (monitor) {
            if (ip %% 50)
                cat(".")
            else
                cat(".", ip, "\n")
        }
    }
    if (monitor)
        cat("\nRead", np,  "of the", np, "profiles in", filename[1], "\n")
    S  <- sin(beamAngle * pi / 180)
    C  <- cos(beamAngle * pi / 180)
    ## FIXME: use the transformation.matrix, if it has been discovered in a header
    if (orientation == "upward") {
        ## OAR explains the method of determining the matrix.
        transformationMatrix <- rbind(c( 2/3/S,       -1/3/S, -1/3/S),
                                      c(     0, -1/sqrt(3)/S, 1/sqrt(3)/S),
                                      c( 1/3/C,        1/3/C,  1/3/C))
    } else {
        transformationMatrix <- rbind(c( 2/3/S,       -1/3/S, -1/3/S),
                                      c(     0,  1/sqrt(3)/S, -1/sqrt(3)/S),
                                      c(-1/3/C,       -1/3/C, -1/3/C))
    }
    res <- new("adp")
    res@metadata$manufacturer <- "sontek"
    res@metadata$instrumentType <- "adp"
    res@metadata$serialNumber <- serialNumber
    res@metadata$filename <- filename
    res@metadata$latitude <- latitude
    res@metadata$longitude <- longitude
    res@metadata$transformationMatrix <- transformationMatrix
    res@metadata$measurementStart <- 0 # FIXME: should fill in
    res@metadata$measurementEnd <- np # FIXME: should fill in
    res@metadata$measurementDeltat <- mean(diff(as.numeric(time)))
    res@metadata$subsampleStart <- 0 # FIXME: should fill in
    res@metadata$subsampleEnd <- np
    res@metadata$subsampleDeltat <- mean(diff(as.numeric(time)))
    res@metadata$frequency <- NA # FIXME
    res@metadata$numberOfSamples <- np
    res@metadata$numberOfBeams <- numberOfBeams
    res@metadata$originalCoordinate <- originalCoordinate
    res@metadata$oceCoordinate <- originalCoordinate
    res@metadata$beamAngle <- beamAngle
    res@metadata$oceBeamUnspreaded <- FALSE
    res@metadata$orientation <- orientation
    res@metadata$units <- list(v="m/s", distance="m")
    res@data <- list(v=v, a=a, q=q,
                     distance=distance,
                     time=time,
                     heading=heading, pitch=pitch, roll=roll,
                     temperature=temperature,
                     pressure=rep(0, length(temperature)),
                     distance=distance)
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    hitem <- processingLogItem(processingLog)
    res@processingLog <- hitem
    res
}
