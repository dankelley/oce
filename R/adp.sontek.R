## vim: tw=120 shiftwidth=4 softtabstop=4 expandtab:
read.adp.sontek <- function(file, from=1, to, by=1, tz=getOption("oce.tz"),
                            latitude=NA, longitude=NA,
                            type=c("adp", "pcadp"),
                            debug=getOption("oce.debug"), monitor=TRUE, despike=FALSE, log.action, ...)
{
    missing.to <- missing(to)
    ## In this function, comments in [] refer to logical page number of ADPManual_v710.pd; add 14 for file page number
    bisect.sontek.adp <- function(t.find, add=0, debug=0) {
        oce.debug(debug, "bisect.sontek.adp(t.find=", format(t.find), ", add=", add, ", debug=", debug, ")\n")
        len <- length(profile.start)
        lower <- 1
        upper <- len
        passes <- floor(10 + log(len, 2)) # won't need this many; only do this to catch coding errors
        for (pass in 1:passes) {
            middle <- floor((upper + lower) / 2)
            oce.debug(debug-1, "pass=",pass,"middle=", middle, "\n")
            oce.debug(debug-1, "data=", buf[profile.start[middle]+seq(0,100,1)], "\n")
            oce.debug(debug-1, "profile.start=", profile.start[1:5], "...; profile.start2=", profile.start2[1:5], "...\n")
            ##year <- readBin(buf[profile.start2[middle]+18], "integer", size=2, signed=FALSE, endian="little")
            year <- readBin(buf[profile.start[middle]+18:19], "integer", size=2, signed=FALSE, endian="little")
            day <- as.integer(buf[profile.start[middle]+20])
            month <- as.integer(buf[profile.start[middle]+21])
            min <- as.integer(buf[profile.start[middle]+22])
            hour <- as.integer(buf[profile.start[middle]+23])
            ## SIG p82 C code suggests sec100 comes before second.
            sec100 <- as.integer(buf[profile.start[middle]+24])     # FIXME: determine whether this is 1/100th second
            sec <- as.integer(buf[profile.start[middle]+25])
            t <- ISOdatetime(year, month, day, hour, min, sec+sec100/100, tz=tz)
            oce.debug(debug, "t=", format(t), 
                      " [year=", year, " month=", month, " day=", day, " hour=", hour, " sec=", second, "sec100=", sec100, "]\n")
            if (t.find < t)
                upper <- middle
            else
                lower <- middle
            if (upper - lower < 2)
                break
            oce.debug(debug, "middle=", middle, " lower=", lower, " upper=", upper, " pass=", pass, " of max=", passes, "\n")
        }
        middle <- middle + add          # may use add to extend before and after window
        if (middle < 1)
            middle <- 1
        if (middle > len)
            middle <- len
        t <- ISOdatetime(readBin(buf[profile.start[middle]+18:19],"integer",size=2,signed=FALSE,endian="little"),  # year
                         as.integer(buf[profile.start[middle]+21]), # month
                         as.integer(buf[profile.start[middle]+20]), # day
                         as.integer(buf[profile.start[middle]+23]), # hour
                         as.integer(buf[profile.start[middle]+22]), # min
                         as.integer(buf[profile.start[middle]+25]), # sec
                         tz=tz)
        oce.debug(debug, "result: t=", format(t), " at vsd.start[", middle, "]=", profile.start[middle], "\n")
        return(list(index=middle, time=t)) # index is within vsd
    }
    oce.debug(debug, "read.adp.sontek(...,from=",from,",to=",if (missing.to) "(missing)" else to,",by=",by,"type=",type,"...)\n")
    parameters <- list(profile.byte1 = 0xa5, profile.byte2=0x10, profile.header.length=80)
    if (is.character(file)) {
        filename <- full.filename(file)
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
    seek(file, 0, "end")
    file.size <- seek(file, 0, "start")
    oce.debug(debug, "file", filename, "has", file.size, "bytes\n")
    buf <- readBin(file, what="raw", n=file.size, endian="little")
    ## See if there is a header here.  (May not be, since the file may have been chopped
    ## into parts by a deck unit.)
    frequency <- NA
    if (buf[1] == 0x10 && buf[2] == 0x02 && 96 == readBin(buf[3:4],"integer",n=1,size=2,signed=FALSE,endian="little")) {
        oce.debug(debug, "have a header, but ignoring it for now\n");
        ## Comments like [p83] refer to logical page number of ADPManual_v710.pd; add 14 for file page number
        cpu.software.ver.num <- as.integer(buf[13]) / 10 # CPUSoftwareVerNum [p83]
        dsp.software.ver.num <- as.integer(buf[14]) / 10 # DSPSoftwareVerNum [p83]
        board.rev <- readBin(buf[15],"character",n=1,size=1,signed=TRUE) # BoardRev [p83]
        serial.number <- readBin(buf[16:25], "character")
        oce.debug(debug, "serial.number=", serial.number, "\n")
        adp.type <- readBin(buf[26], what="integer", n=1, size=1) # 0-3; 1=1.5; 2-750; 3-500 [p83]
        oce.debug(debug, "adp.type=",adp.type,"\n")
        frequency <- switch(adp.type+1, 3000, 1500, 750, 500, 250)
        oce.debug(debug, "frequency=",frequency,"\n")
        nbeams <- as.integer(buf[27])
        oce.debug(debug, "nbeams=", nbeams, "\n")
        beam.geometry <- as.integer(buf[28])
        oce.debug(debug, "beam.geometry=", beam.geometry, 
                  "; 0 (2 beams); 1 (3 beams), 2 (4 beams with 1 vertical), 3 (4 beams, Janus)\n")
        slant.angle <- readBin(buf[29:30], "integer", n=1, size=2, signed=FALSE) / 10
        oce.debug(debug, "slant.angle=",slant.angle,"\n")
        orientation <- switch(as.integer(buf[31]) + 1, "down", "up", "side")
        oce.debug(debug, "orientation=", orientation,"\n")
        compass.installed <- switch(as.integer(buf[32]) + 1, FALSE, TRUE)
        recorder.installed <- switch(as.integer(buf[33]) + 1, FALSE, TRUE)
        temp.installed <- switch(as.integer(buf[34]) + 1, FALSE, TRUE)
        press.installed <- switch(as.integer(buf[35]) + 1, FALSE, TRUE)
        ## 36 = spare
        ## 37 int[16], so I guess 2-byte ints, signed?
    } else {
        cpu.software.ver.num <- dsp.software.ver.num <- board.rev <-
            adp.type <- nbeams <- slant.angle <- orientation <-
                compass.installed <- recorder.installed <- temp.installed <- press.installed <- "?"
    }
    ##profile.start <- .Call("match2bytes", buf, parameters$profile.byte1, parameters$profile.byte2, FALSE)
    profile.start <- .Call("ldc_sontek_adp", buf, 0, 0, 0, 1, -1) # no ctd, no gps, no bottom-track; pcadp; all data
    profile.start2 <- sort(c(profile.start, profile.start+1)) # use this to subset for 2-byte reads
    oce.debug(debug, "first 10 profile.start:", profile.start[1:10], "\n")
    oce.debug(debug, "first 100 bytes of first profile:", paste(buf[profile.start[1]:(99+profile.start[1])], collapse=" "), "\n")
    ## Examine the first profile to get number of beams, etc.
    header.length <- 80                 # FIXME: should this really be hard-wired??
    s <- profile.start[1]
    ## Only read (important) things that don't change profile-by-profile
    number.of.beams <- as.integer(buf[s+26])
    oce.debug(debug, "number.of.beams=", number.of.beams, "\n")
    if (number.of.beams != 3)
        stop("there should be 3 beams, but the file indicates ", number.of.beams)
    orientation <- as.integer(buf[s+27])
    oce.debug(debug, "orientation=", orientation, "\n")
    temp.mode <- as.integer(buf[s+28])
    oce.debug(debug, "temp.mode=", temp.mode, "\n")
    coordinate.system <- as.integer(buf[s+29])
    oce.debug(debug, "coordinate.system=", coordinate.system, "\n")
    number.of.cells <- readBin(buf[s+30:31], "integer", n=1, size=2, endian="little", signed=FALSE)
    oce.debug(debug, "number.of.cells=", number.of.cells, "\n")
    cell.size <- readBin(buf[s+32:33], "integer", n=1, size=2, endian="little", signed=FALSE) / 100 # metres
    oce.debug(debug, "cell.size=", cell.size, "m\n")
    blanking.distance <- readBin(buf[s+34:35], "integer", n=1, size=2, endian="little", signed=FALSE) / 100 # metres
    oce.debug(debug, "blanking.distance=", blanking.distance, "m\n")
    sound.speed <- readBin(buf[s+60:61], "integer", n=1, size=2, endian="little", signed=FALSE) / 10
    oce.debug(debug, "sound.speed=", sound.speed, "m/s\n")
    profiles.in.file <- length(profile.start)
    id <- buf[profile.start]
    bytes.per.profile <- diff(profile.start[1:2])
    oce.debug(debug, "bytes.per.profile=", bytes.per.profile, "\n")

    ## File time range and deltat
    measurement.start <- ISOdatetime(readBin(buf[profile.start[1]+18:19],"integer",n=1,size=2,signed=FALSE,endian="little"), # year
                                     as.integer(buf[profile.start[1]+21]), # month
                                     as.integer(buf[profile.start[1]+20]), # day
                                     as.integer(buf[profile.start[1]+23]), # hour
                                     as.integer(buf[profile.start[1]+22]), # min
                                     as.integer(buf[profile.start[1]+25])+0.01*as.integer(buf[profile.start[1]+24]), # sec (decimal)
                                     tz=tz)
    oce.debug(debug, "measurement.start=", format(measurement.start), "\n")
    oce.debug(debug, "length(profile.start)=", length(profile.start), " [FIXME: if to not given, use this??]\n")

    measurement.end <- ISOdatetime(readBin(buf[profile.start[profiles.in.file]+18:19],"integer",n=1,size=2,signed=FALSE,endian="little"), # year
                                   as.integer(buf[profile.start[profiles.in.file]+21]), # month
                                   as.integer(buf[profile.start[profiles.in.file]+20]), # day
                                   as.integer(buf[profile.start[profiles.in.file]+23]), # hour
                                   as.integer(buf[profile.start[profiles.in.file]+22]), # min
                                   as.integer(buf[profile.start[profiles.in.file]+25])+0.01*as.integer(buf[profile.start[1]+24]), # sec (decimal)
                                   tz=tz)
    oce.debug(debug, "sampling.end=", format(measurement.end), "\n")
    measurement.deltat <- as.numeric(ISOdatetime(readBin(buf[profile.start[2]+18:19],"integer",n=1,size=2,signed=FALSE,endian="little"), # year
                                                 as.integer(buf[profile.start[2]+21]), # month
                                                 as.integer(buf[profile.start[2]+20]), # day
                                                 as.integer(buf[profile.start[2]+23]), # hour
                                                 as.integer(buf[profile.start[2]+22]), # min
                                                 as.integer(buf[profile.start[2]+25])+0.01*as.integer(buf[profile.start[1]+24]), # sec
                                                 tz=tz)) - as.numeric(measurement.start)
    oce.debug(debug, "sampling.deltat=", format(measurement.deltat), "\n")

    ## Window data buffer, using bisection in case of a variable number of vd between sd pairs.
    if (inherits(from, "POSIXt")) {
        if (!inherits(to, "POSIXt"))
            stop("if 'from' is POSIXt, then 'to' must be, also")
        from.pair <- bisect.sontek.adp(from, -1, debug-1)
        from <- from.index <- from.pair$index
        to.pair <- bisect.sontek.adp(to, 1, debug-1)
        to <- to.index <- to.pair$index
        oce.debug(debug, "  from=", format(from.pair$t), " yields profile.start[", from.index, "]\n",
                  "  to  =", format(to.pair$t),   " yields profile.start[", to.index, "]\n",
                  "  by=", by, "s\n",
                  "profile.start[1:10]=", profile.start[1:10],"\n",
                  "profile.start[",from.pair$index, "]=", profile.start[from.pair$index], "at time", format(from.pair$t), "\n",
                  "profile.start[",  to.pair$index, "]=", profile.start[  to.pair$index], "at time", format(  to.pair$t), "\n")
        ## FIXME next line reads year incorrectly
        two.times <- ISOdatetime(readBin(buf[profile.start[1:2]+18:19],"integer",size=2,signed=FALSE,endian="little"),  # year
                                 as.integer(buf[profile.start[1:2]+21]), # month
                                 as.integer(buf[profile.start[1:2]+20]), # day
                                 as.integer(buf[profile.start[1:2]+23]), # hour
                                 as.integer(buf[profile.start[1:2]+22]), # min
                                 as.integer(buf[profile.start[1:2]+25])+0.01*as.integer(buf[profile.start[1]+24]), # sec
                                 tz=tz)
        dt <- as.numeric(difftime(two.times[2], two.times[1], units="secs"))
        oce.debug(debug, "dt=", dt, "s; at this stage, by=", by,"(not interpreted yet)\n")
        profile.start <- profile.start[profile.start[from.index] < profile.start & profile.start < profile.start[to.index]]
        if (is.character(by))
            by <- floor(0.5 + ctime.to.seconds(by) / dt)
        oce.debug(debug, "by=",by,"profiles (after change)\n")
        profile.start <- profile.start[seq(1, length(profile.start), by=by)]
        oce.debug(debug, 'dt=',dt,'\n', 'by=',by, "profile.start[1:10] after indexing:", profile.start[1:10], "\n")
    } else {
        from.index <- from
        if (missing.to)
            to <- length(profile.start)
        to.index <- to
        if (to.index < 1 + from.index)
            stop("need more separation between from and to")
        if (is.character(by))
            stop("cannot have string for 'by' if 'from' and 'to' are integers")
        profile.start <- profile.start[seq(from=from, to=to, by=by)]
        oce.debug(debug, "profile.start[1:10] after indexing:", profile.start[1:10], "\n")
    }
    profiles.to.read <- length(profile.start)
    oce.debug(debug, "profiles.in.file=",profiles.in.file,"; profiles.to.read=",profiles.to.read,"\n")
    profile.start2 <- sort(c(profile.start, profile.start+1)) # use this to subset for 2-byte reads
    year   <- readBin(buf[profile.start2 + 18], "integer", n=profiles.to.read, size=2, endian="little", signed=FALSE)
    day    <- as.integer(buf[profile.start + 20])
    month  <- as.integer(buf[profile.start + 21])
    minute <- as.integer(buf[profile.start + 22])
    hour   <- as.integer(buf[profile.start + 23])
    sec100 <- as.integer(buf[profile.start + 24])
    second <- as.integer(buf[profile.start + 25])
    time <- ISOdatetime(year, month, day, hour, minute, second+sec100/100, tz=tz)
    rm(year, day, month, minute, hour, sec100, second)
    temperature <- readBin(buf[profile.start2 + 46], "integer", n=profiles.to.read, size=2, endian="little", signed=TRUE) / 100
    oce.debug(debug, "temperature[1:10]=",temperature[1:10],"\n")
    pressure <- readBin(buf[profile.start2 + 48], "integer", n=profiles.to.read, size=2, endian="little", signed=FALSE) / 100
    ## FIXME: pressure (+else?) is wrong.  Need to count bytes on p84 of ADPManual to figure out where to look [UGLY]
    oce.debug(debug, "pressure[1:10]=",pressure[1:10],"\n")
    heading <- readBin(buf[profile.start2 + 40], "integer", n=profiles.to.read, size=2, endian="little", signed=TRUE) / 10
    pitch <- readBin(buf[profile.start2 + 42], "integer", n=profiles.to.read, size=2, endian="little", signed=TRUE) / 10
    roll <- readBin(buf[profile.start2 + 44], "integer", n=profiles.to.read, size=2, endian="little", signed=TRUE) / 10

    oce.debug(debug, "time[1:10]=",format(time[1:10]),"\n")
    v <- array(dim=c(profiles.to.read, number.of.cells, number.of.beams))
    a <- array(raw(), dim=c(profiles.to.read, number.of.cells, number.of.beams))
    q <- array(raw(), dim=c(profiles.to.read, number.of.cells, number.of.beams))
    nd <- number.of.cells * number.of.beams
    oce.debug(debug, "nd=",nd,";  header.length=", header.length,"\n")
    if (type == "pcadp") {
        nbeam.max <- 4                 # Max number of beams, not actual number
        header.length <- header.length + 2 * (8 + nbeam.max) + 2 * nbeam.max + nbeam.max
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
    if (profiles.to.read > 0) {
        for (i in 1:profiles.to.read) {
            v_ <- matrix(readBin(buf[profile.start[i] + header.length + seq(0, 2*nd-1)], "integer", n=nd, size=2, signed=TRUE, endian="little"),
                         ncol=number.of.beams, byrow=FALSE)/1000
            a_ <- matrix(buf[profile.start[i] + header.length + 2*nd + seq(0, nd-1)], ncol=number.of.beams, byrow=FALSE)
            q_ <- matrix(buf[profile.start[i] + header.length + 3*nd + seq(0, nd-1)], ncol=number.of.beams, byrow=FALSE)
            for (b in 1:number.of.beams) { # FIXME: probably could be speeded up
                v[i,,b] <- v_[,b]
                a[i,,b] <- a_[,b]
                q[i,,b] <- q_[,b]
            }
            if (monitor) {
                cat(".")
                if (!(i %% 50))
                    cat(i, "\n")
            }
        }
        rm(v_, a_, q_)
        if (monitor)
            cat("\nRead", profiles.to.read,  "of the", profiles.in.file, "profiles in", filename, "\n")
        if (type == "pcadp")
            v <- v / 10                # it seems pcadp is in 0.1mm/s
        ma <- list(v=v, a=a, q=q)
        rm(v, a, q)
    } else {
        ma <- NULL
    }
    ## interpolate headings (which may be less frequent than profiles ... FIXME: really???)
    nheading <- length(heading)
    nv <- dim(ma$v)[1]
    if (nheading != nv) {
        warning("read.adp.sontek() interpolating ", nheading, " heading/pitch/roll values to the ", nv, " velocity profiles")
        oce.debug(debug, "BEFORE: length(heading)=", nheading, ", nv=", nv, "\n")
        xout <- seq(1, nheading, length.out=nv)
        heading <- approx(1:nheading, heading, seq(1,nheading,length.out=nv))$y
        ##print(data.frame(xout=xout, heading=heading))
        pitch <- approx(1:nheading, pitch, seq(1,nheading,length.out=nv))$y
        roll <- approx(1:nheading, roll, seq(1,nheading,length.out=nv))$y
        oce.debug(debug, "AFTER:  length(heading)=", length(heading), "\n")
    }
    data <- list(ma=ma,
                 ss=list(distance=seq(blanking.distance, by=cell.size, length.out=number.of.cells)),
                 ts=list(time=time,
                         temperature=temperature,
                         pressure=pressure,
                         heading=heading, pitch=pitch, roll=roll))
    oce.debug(debug, "slant.angle=",slant.angle,"; type=", type, "\n")
    beam.angle <- if (slant.angle == "?") 25 else slant.angle
    metadata <- list(manufacturer="sontek",
                     type=type,
                     filename=filename,
                     serial.number=if (exists('serial.number')) serial.number else "?",
                     latitude=latitude,
                     longitude=longitude,
                     measurement.start=measurement.start,
                     measurement.end=measurement.end,
                     measurement.deltat=measurement.deltat,
                     frequency=frequency,
                     cpu.software.ver.num=cpu.software.ver.num,
                     dsp.software.ver.num=dsp.software.ver.num,
                     board.rev=board.rev,
                     number.of.samples=profiles.to.read,
                     coordinate.system=c("beam", "xyz", "enu", "other")[coordinate.system+1], # FIXME: check this
                     oce.coordinate=c("beam", "xyz", "enu", "other")[coordinate.system+1], # FIXME: check this
                     number.of.beams=number.of.beams,
                     beam.angle=beam.angle,
                     oce.beam.attenuated=FALSE,
                     orientation=if(orientation==1) "upward" else "downward")
    if (number.of.beams == 3) {
        S  <- 1 / (3 * sin(beam.angle * pi / 180)) # 0.7887339
        CS <- 1 / cos(30*pi/180) / sin(beam.angle*pi/180) / 2 # 1.366127 (30deg from 3-beam pattern)
        C  <- 1 / (3 * cos(beam.angle * pi / 180))             # 0.3677926
        if (metadata$orientation == "downward") {
            metadata$transformation.matrix <- matrix(c(2*S,  -S,  -S,
                                                       0  ,  CS, -CS,
                                                       -C ,  -C,  -C),
                                                     nrow=3, byrow=TRUE)
        } else if (metadata$orientation == "upward") {
            metadata$transformation.matrix <- matrix(c(2*S,  -S,  -S,
                                                       0  , -CS,  CS,
                                                       C  ,   C,   C),
                                                     nrow=3, byrow=TRUE)
        } else 
            stop("unknown orientientation ", metadata$orientation, "; must be \"upward\" or \"downward\"")
    } else
        stop("can only handle 3-beam devices")
    if (missing(log.action))
        log.action <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processing.log.item(log.action)
    res <- list(data=data, metadata=metadata, processing.log=log.item)
    class(res) <- c("sontek", "adp", "oce")
    res
}

sontek.time <- function(t, tz=getOption("oce.tz"))
{
    minute <- bcd2integer(t[1])
    second <- bcd2integer(t[2])
    day <- bcd2integer(t[3])
    hour <- bcd2integer(t[4])
    year <- bcd2integer(t[5])
    year <- year + if (year >= 90) 1900 else 2000 # page 51 of System Integrator Guide
    month <- bcd2integer(t[6])
    milliseconds <- readBin(t[7:8], "integer", n=1, size=2, endian="little", signed=FALSE)
    ISOdatetime(year, month, day, hour, minute, second+milliseconds/1000, tz=tz)
}

read.adp.sontek.serial <- function(file, from=1, to, by=1, tz=getOption("oce.tz"),
                                   latitude=NA, longitude=NA,
                                   type=c("adp", "pcadp"),
                                   beam.angle=25, orientation,
                                   monitor=TRUE, log.action,
                                   debug=getOption("oce.debug"))
{
    ## Data format is described in
    ##   SonTek/YSI
    ##   ADPManual_v710.pdf
    ## A3. Profile Header/CTD/GPS/Bottom Track,/SonWave/Profile Data Structures
    bisect.adp.sontek.serial <- function(t.find, add=0, tz="UTC", debug=0) {
        oce.debug(debug, "bisect.adp.sontek.serial(t.find=", format(t.find), ", add=", add, "\n")
        len <- length(p)
        lower <- 1
        upper <- len
        passes <- floor(10 + log(len, 2)) # won't need this many; only do this to catch coding errors
        for (pass in 1:passes) {
            middle <- floor((upper + lower) / 2)
            year   <- readBin(buf[p[middle] + 18:19], what="integer", n=1, size=2, signed=FALSE, endian="little")
            day    <- readBin(buf[p[middle] + 20], what="integer", n=1, size=1, signed=FALSE)
            month  <- readBin(buf[p[middle] + 21], what="integer", n=1, size=1, signed=FALSE)
            min    <- readBin(buf[p[middle] + 22], what="integer", n=1, size=1, signed=FALSE)
            hour   <- readBin(buf[p[middle] + 23], what="integer", n=1, size=1, signed=FALSE)
            sec100 <- readBin(buf[p[middle] + 24], what="integer", n=1, size=1, signed=FALSE)
            sec    <- readBin(buf[p[middle] + 25], what="integer", n=1, size=1, signed=FALSE)
            t <- ISOdatetime(year=year, month=month, day=day, hour=hour, min=min, sec=sec + sec100/100, tz=tz)
            oce.debug(debug, "t=", format(t), " y=", year, " m=", month, " d=", format(day, width=2),
                      " h=", format(hour, width=2),
                      " m=", format(min, width=2),
                      " s=", format(sec+sec100/100, width=4),
                      "| pass", format(pass, width=2), "/", passes, "| middle=", middle,
                      "(", format(middle/upper*100,digits=4), "%)\n", sep="")
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
        t <- ISOdatetime(readBin(buf[p[middle]+18:19],"integer",size=2,signed=FALSE,endian="little"),
                         as.integer(buf[p[middle]+21]), # month
                         as.integer(buf[p[middle]+20]), # day
                         as.integer(buf[p[middle]+23]), # hour
                         as.integer(buf[p[middle]+22]), # min
                         as.integer(buf[p[middle]+25])+0.01*as.integer(buf[p[middle]+24]),
                         tz=tz)
        oce.debug(debug, "result: t=", format(t), " at d[", middle, "]=", p[middle], "\n")
        return(list(index=middle, time=t))
    }
    oce.debug(debug, paste("\b\bread.adp.sontek.serial(file[1]=\"", file[1],
                           "\", from=", from,
                           if (missing(to)) "to," else sprintf(", to=%s, ", format(to)),
                           ", by=", by,
                           ", latitude=", latitude, ", longitude=", longitude,
                           ", monitor=", monitor,
                           ", log.action=(not shown), debug=", debug, ") {\n", sep=""))
    nfile <- length(file)
    if (nfile > 1) {                   # handle multiple files
        oce.debug(debug, "handling multiple files\n")
        buf <- NULL
        for (i in 1:nfile) {
            if (monitor)
                cat("\"", file[i], "\" ", sep="")
            this.file <- file(file[i], "rb")
            seek(this.file, 0, "end", rw="read")
            file.size <- seek(this.file, 0, origin="start", rw="read")
            if (monitor)
                cat(file.size,"bytes\n")
            buf <- c(buf, readBin(this.file, what="raw", n=file.size, endian="little"))
            close(this.file)
        }
        filename <- paste("(\"", file[i], "\", ...)", sep="")
    } else {                            # handle single file (which might be a connection, etc)
        if (is.character(file)) {
            filename <- full.filename(file)
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
        file.size <- seek(file, 0, origin="start", rw="read")
        oce.debug(debug, "filesize=",file.size,"\n")
        buf <- readBin(file, what="raw", n=file.size, endian="little")
    }
    p <- .Call("ldc_sontek_adp", buf, 0, 0, 0, 0, -1) # no ctd, no gps, no bottom-track; all data
    ## read some unchanging things from the first profile only
    serial.number <- paste(readBin(buf[p[1]+4:13], "character", n=10, size=1),collapse="")
    number.of.beams <- readBin(buf[p[1]+26], "integer", n=1, size=1, signed=FALSE)
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
    coordinate.system <- readBin(buf[p[1]+29], "integer", n=1, size=1, signed=FALSE)
    if (coordinate.system == 0)
        coordinate.system <- "beam"
    else if (coordinate.system == 1)
        coordinate.system <- "xyz"
    else if (coordinate.system == 2)
        coordinate.system <- "enu"
    else
        stop("coordinate.system=", coordinate.system, "but must be 0 (beam), 1 (xyz), or 2 (enu)")
    number.of.cells <- readBin(buf[p[1]+30:31], "integer", n=1, size=2, signed=FALSE, endian="little")
    cell.size <- 0.01*readBin(buf[p[1]+32:33], what="integer", n=1, size=2, signed=FALSE, endian="little")
    blanking.distance <- 0.01*readBin(buf[p[1]+34:35], what="integer", n=1, size=2, signed=FALSE)
    distance <- blanking.distance + cell.size * seq(from=0.5, by=cell.size, length.out=number.of.cells)
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
                from.pair <- bisect.adp.sontek.serial(from, add=-1, tz=tz, debug=debug-1)
                from <- from.index <- from.pair$index
                to.pair <- bisect.adp.sontek.serial(to, add=1, tz=tz, debug=debug-1)
                to <- to.index <- to.pair$index
                oce.debug(debug, "from=", format(from.pair$t), " yields p[", from.index, "]\n",
                          "  to  =", format(to.pair$t), "yields p[", to.index, "]\n",
                          "  by=", by, "(not yet decoded)\n",
                          vector.show(p, "p:"),
                          "  p[",from.pair$index, "]=", p[from.pair$index], "at time", format(from.pair$t), "\n",
                          "  p[",  to.pair$index, "]=", p[  to.pair$index], "at time", format(  to.pair$t), "\n")
                p <- p[seq(from=from, to=to, by=by)]
            }
        }
    }
    np <- length(p)
    pp <- sort(c(p, p+1)) # for 2-byte addressing ('int' in the Sontek docs)
    pppp <- sort(c(p, p+1, p+2, p+3)) # for 4-byte addressing ('long' in the Sontek docs)

    ## read profile-specific things profile by profile
    profile.number <- readBin(buf[pppp+14], "integer", n=np, size=4, signed=FALSE)
    ## FIXME: should check that profile number is monotonic ... it may
    ## help us with daily blank-outs, also!
    year <- readBin(buf[pp+18],"integer",n=np,size=2,signed=FALSE)
    day <- readBin(buf[p+20],"integer",n=np,size=1,signed=FALSE)
    month <- readBin(buf[p+21],"integer",n=np,size=1,signed=FALSE)
    min <- readBin(buf[p+22],"integer",n=np,size=1,signed=FALSE)
    hour <- readBin(buf[p+23],"integer",n=np,size=1,signed=FALSE)
    sec100 <- readBin(buf[p+24],"integer",n=np,size=1,signed=FALSE)
    sec <- readBin(buf[p+25],"integer",n=np,size=1,signed=FALSE)
    time <- ISOdatetime(year, month, day, hour, min, sec+0.01*sec100, tz=tz)
    rm(year, day, month, min, hour, sec100, sec) # possibly this space will come in handy
    heading <- 0.1 * readBin(buf[pp+40], "integer", n=np, size=2, signed=TRUE)
    pitch <- 0.1 * readBin(buf[pp+42], "integer", n=np, size=2, signed=TRUE)
    roll <- 0.1 * readBin(buf[pp+44], "integer", n=np, size=2, signed=TRUE)
    temperature <- 0.01 * readBin(buf[pp+46], "integer", n=np, size=2, signed=TRUE)
    v <- array(numeric(), dim=c(np, number.of.cells, number.of.beams))
    vstd <- array(raw(), dim=c(np, number.of.cells, number.of.beams))
    amp <- array(raw(), dim=c(np, number.of.cells, number.of.beams))
    ndata <- number.of.cells * number.of.beams
    i1 <- seq(1, ndata)
    i2 <- seq(1, 2*ndata)
    for (ip in 1:np) {
        p0 <- p[ip] + 79
        v_ <- matrix(0.001*readBin(buf[p0 + i2], "integer", endian="little", n=ndata, size=2, signed=TRUE),
                     ncol=number.of.beams, byrow=FALSE)
        p0 <- p0 + 2 * ndata
        ## NOTE: vstd is std-dev; need to multiply by 0.001 to get in m/s
        vstd_ <- matrix(buf[p0 + i1], ncol=number.of.beams, byrow=FALSE)
        p0 <- p0 + ndata
        amp_ <- matrix(buf[p0 + i1], ncol=number.of.beams, byrow=FALSE)
        for (b in 1:number.of.beams) {
            v[ip,,b] <- v_[,b]
            vstd[ip,,b] <- vstd_[,b]
            amp[ip,,b] <- amp_[,b]
        }
        if (monitor) {
            if ((ip %% 50))
                cat(".")
            else
                cat(".", ip, "\n")
        }
    }
    if (monitor)
        cat("\nRead", np,  "of the", np, "profiles in", filename[1], "\n")
    S  <- sin(beam.angle * pi / 180)
    C  <- cos(beam.angle * pi / 180)
    ## FIXME: use the transformation.matrix, if it has been discovered in a header
    if (orientation == "upward") {
        ## OAR explains the method of determining the matrix.
        transformation.matrix <- rbind(c( 2/3/S,       -1/3/S, -1/3/S),
                                       c(     0, -1/sqrt(3)/S, 1/sqrt(3)/S),
                                       c( 1/3/C,        1/3/C,  1/3/C))
    } else {
        transformation.matrix <- rbind(c( 2/3/S,       -1/3/S, -1/3/S),
                                       c(     0,  1/sqrt(3)/S, -1/sqrt(3)/S),
                                       c(-1/3/C,       -1/3/C, -1/3/C))
    }
    metadata <- list(manufacturer="sontek",
                     type="adp",
                     serial.number=serial.number,
                     filename=filename,
                     latitude=latitude, longitude=longitude,
                     transformation.matrix=transformation.matrix,
                     measurement.start=0, # FIXME: should fill in
                     measurement.end=np, # FIXME: should fill in
                     measurement.deltat=mean(diff(as.numeric(time))),
                     subsample.start=0, # FIXME: should fill in
                     subsample.end=np, 
                     subsample.deltat=mean(diff(as.numeric(time))),
                     frequency=NA, # FIXME
                     number.of.samples=np,
                     number.of.beams=number.of.beams,
                     coordinate.system=coordinate.system,
                     oce.coordinate=coordinate.system,
                     beam.angle=beam.angle,
                     oce.beam.attenuated=FALSE,
                     orientation=orientation)
    data <- list(ts=list(time=time,
                         heading=heading, pitch=pitch, roll=roll,
                         temperature=temperature,
                         pressure=rep(0, length(temperature))),
                 ss=list(distance=distance),
                 ma=list(v=v,vstd=vstd,amp=amp)) # velo, velo stddev, amplitude
    if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processing.log.item(log.action)
    res <- list(data=data, metadata=metadata, processing.log=log.item)
    class(res) <- c("sontek", "adp", "oce")
    res
}

