read.adp.sontek <- function(file, from=1, to, by=1, type=c("adp"), debug=getOption("oce.debug"), monitor=TRUE, log.action, ...)
{
    ## In this function, comments in [] refer to logical page number of ADPManual_v710.pd; add 14 for file page number

    bisect.sontek.adp <- function(t.find, add=0, debug=0) {
        oce.debug(debug, "bisect.sontek.adv(t.find=", format(t.find), ", add=", add, ", debug=", debug, ")\n")
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
            minute <- as.integer(buf[profile.start[middle]+22])
            hour <- as.integer(buf[profile.start[middle]+23])
            ## SIG p82 C code suggests sec100 comes before second.
            sec100 <- as.integer(buf[profile.start[middle]+24])     # FIXME: determine whether this is 1/100th second
            second <- as.integer(buf[profile.start[middle]+25])
            t <- ISOdatetime(year, month, day, hour, minute, second+sec100/100, tz="UTC")
            oce.debug(debug, "t=", format(t), " inferred from year=", year, " month=", month, " day=", day, " hour=", hour, " second=", second, "sec100=", sec100, "\n")
            if (t.find < t)
                upper <- middle
            else
                lower <- middle
            if (upper - lower < 2)
                break
            oce.debug(debug, "middle=", middle, " lower=", lower, " upper=", upper, " pass=", pass, " of max=", passes, "\n")
        }
        middle <- middle + add          # may use add to extend before and after window
        if (middle < 1) middle <- 1
        if (middle > len) middle <- len
        t <- ISOdatetime(readBin(buf[profile.start[middle]+18:19],"integer",size=2,signed=FALSE,endian="little"),  # year
                         as.integer(buf[profile.start[middle]+21]), # month
                         as.integer(buf[profile.start[middle]+20]), # day
                         as.integer(buf[profile.start[middle]+23]), # hour
                         as.integer(buf[profile.start[middle]+22]), # min
                         as.integer(buf[profile.start[middle]+25]), # sec
                         tz=getOption("oce.tz"))
        oce.debug(debug, "result: t=", format(t), " at vsd.start[", middle, "]=", profile.start[middle], "\n")
        return(list(index=middle, time=t)) # index is within vsd
    }
    oce.debug(debug, "read.adp.sontek(...,from=",from,",to=",if (missing(to)) "(missing)" else to,",by=",by,"type=",type,"...)\n")
    parameters <- list(profile.byte1 = 0xa5, profile.byte2=0x10, profile.header.length=80)
    if (is.character(file)) {
        filename <- full.filename(file)
        file <- file(file, "rb")
        on.exit(close(file))
    }
    if (!inherits(file, "connection")) stop("argument `file' must be a character string or connection")
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
        serial.number <- readBin(buf[16:26], "character")
        adp.type <- as.integer(buf[27]) # 0-3; 1=1.5; 2-750; 3-500 [p83]
        warning("known error: may interpret adp.type (frequency) incorrectly")
        ## FIXME "/data/archive/sleiwex/2008/moorings/m07/adp/sontek_h53/raw/DEF003.ADP" has adp.type=3 but the ctl file says 1500kHz
        oce.debug(debug, "adp.type=",adp.type,"\n")
        frequency <- switch(adp.type+1, 3000, 1500, 750, 500)
        oce.debug(debug, "frequency=",frequency," (BUG: wrong on m07/adp/sontek_h53)\n")
        nbeams <- as.integer(buf[28])
        beam.geometry <- as.integer(buf[28])
        oce.debug(debug, "beam.geometry=", beam.geometry, "; 0 means 2 beams; 1 means 3 beams, 2 means 4 beams with 1 verticl; 3 means 4 beams, Janus\n")
        slant.angle <- readBin(buf[29:30], "integer", n=1, size=2, signed=FALSE) / 10
        oce.debug(debug, "slant.angle=",slant.angle,"\n")
        sensor.orientation <- switch(as.integer(buf[31]) + 1, "down", "up", "side")
        oce.debug(debug, "sensor.orientation=",sensor.orientation,"\n")
        compass.installed <- switch(as.integer(buf[32]) + 1, FALSE, TRUE)
        recorder.installed <- switch(as.integer(buf[33]) + 1, FALSE, TRUE)
        temp.installed <- switch(as.integer(buf[34]) + 1, FALSE, TRUE)
        press.installed <- switch(as.integer(buf[35]) + 1, FALSE, TRUE)
        ## 36 = spare
        ## 37 int[16], so I guess 2-byte ints, signed?
    } else {
        cpu.software.ver.num <- dsp.software.ver.num <- board.rev <-
            adp.type <- nbeams <- slant.angle <- sensor.orientation <-
                compass.installed <- recorder.installed <- temp.installed <- press.installed <- "?"
    }
    profile.start <- .Call("match2bytes", buf, parameters$profile.byte1, parameters$profile.byte2, FALSE)
    profile.start2 <- sort(c(profile.start, profile.start+1)) # use this to subset for 2-byte reads
    oce.debug(debug, "first 10 profile.start:", profile.start[1:10], "\n")
    oce.debug(debug, "first 100 bytes of first profile:", paste(buf[profile.start[1]:(99+profile.start[1])], collapse=" "), "\n")
    ## Examine the first profile to get number of beams, etc.
    header.length <- 80                 # FIXME: should this really be hard-wired??
    s <- profile.start[1]
    ## Only read (important) things that don't change profile-by-profile
    number.of.beams <- as.integer(buf[s+26])
    oce.debug(debug, "number.of.beams=", number.of.beams, "\n")
    if (number.of.beams != 3) stop("there should be 3 beams, but the file indicates ", number.of.beams)
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
    sampling.start <- ISOdatetime(readBin(buf[profile.start[1]+18:19],"integer",n=1,size=2,signed=FALSE,endian="little"), # year
                                  as.integer(buf[profile.start[1]+21]), # month
                                  as.integer(buf[profile.start[1]+20]), # day
                                  as.integer(buf[profile.start[1]+23]), # hour
                                  as.integer(buf[profile.start[1]+22]), # min
                                  as.integer(buf[profile.start[1]+25])+0.01*as.integer(buf[profile.start[1]+24]), # sec (decimal)
                                  tz=getOption("oce.tz"))
    oce.debug(debug, "sampling.start=", format(sampling.start), "\n")
    sampling.end <- ISOdatetime(readBin(buf[profile.start[profiles.in.file]+18:19],"integer",n=1,size=2,signed=FALSE,endian="little"), # year
                             as.integer(buf[profile.start[profiles.in.file]+21]), # month
                             as.integer(buf[profile.start[profiles.in.file]+20]), # day
                             as.integer(buf[profile.start[profiles.in.file]+23]), # hour
                             as.integer(buf[profile.start[profiles.in.file]+22]), # min
                             as.integer(buf[profile.start[profiles.in.file]+25])+0.01*as.integer(buf[profile.start[1]+24]), # sec (decimal)
                             tz=getOption("oce.tz"))
    oce.debug(debug, "sampling.end=", format(sampling.end), "\n")
    sampling.deltat <- as.numeric(ISOdatetime(readBin(buf[profile.start[2]+18:19],"integer",n=1,size=2,signed=FALSE,endian="little"), # year
                                              as.integer(buf[profile.start[2]+21]), # month
                                              as.integer(buf[profile.start[2]+20]), # day
                                              as.integer(buf[profile.start[2]+23]), # hour
                                              as.integer(buf[profile.start[2]+22]), # min
                                              as.integer(buf[profile.start[2]+25])+0.01*as.integer(buf[profile.start[1]+24]), # sec
                                              tz=getOption("oce.tz"))) - as.numeric(sampling.start)
    oce.debug(debug, "sampling.deltat=", format(sampling.deltat), "\n")

    ## Window data buffer, using bisection in case of a variable number of vd between sd pairs.
    if (inherits(from, "POSIXt")) {
        if (!inherits(to, "POSIXt")) stop("if 'from' is POSIXt, then 'to' must be, also")
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
                                 tz=getOption("oce.tz"))
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
        to.index <- to
        if (to.index < 1 + from.index) stop("need more separation between from and to")
        if (is.character(by)) stop("cannot have string for 'by' if 'from' and 'to' are integers")
        profile.start <- profile.start[seq(from=from, to=to, by=by)]
        oce.debug(debug, "profile.start[1:10] after indexing:", profile.start[1:10], "\n")
    }
    profiles.to.read <- length(profile.start)
    oce.debug(debug, "profiles.in.file=",profiles.in.file,"; profiles.to.read=",profiles.to.read,"\n")
    profile.start2 <- sort(c(profile.start, profile.start+1)) # use this to subset for 2-byte reads
    temperature <- readBin(buf[profile.start2 + 46], "integer", n=profiles.to.read, size=2, endian="little", signed=TRUE) / 100
    oce.debug(debug, "temperature[1:10]=",temperature[1:10],"\n")
    pressure <- readBin(buf[profile.start2 + 48], "integer", n=profiles.to.read, size=2, endian="little", signed=FALSE) / 100
    ## FIXME: pressure (+else?) is wrong.  Need to count bytes on p84 of ADPManual to figure out where to look [UGLY]
    oce.debug(debug, "pressure[1:10]=",pressure[1:10],"\n")
    heading <- readBin(buf[profile.start2 + 40], "integer", n=profiles.to.read, size=2, endian="little", signed=TRUE) / 10
    pitch <- readBin(buf[profile.start2 + 42], "integer", n=profiles.to.read, size=2, endian="little", signed=TRUE) / 10
    roll <- readBin(buf[profile.start2 + 44], "integer", n=profiles.to.read, size=2, endian="little", signed=TRUE) / 10
    year   <- readBin(buf[profile.start2 + 18], "integer", n=profiles.to.read, size=2, endian="little", signed=FALSE)
    day    <- as.integer(buf[profile.start + 20])
    month  <- as.integer(buf[profile.start + 21])
    minute <- as.integer(buf[profile.start + 22])
    hour   <- as.integer(buf[profile.start + 23])
    sec100 <- as.integer(buf[profile.start + 24])     # FIXME: determine whether this is 1/100th second
    second <- as.integer(buf[profile.start + 25])
    time <- ISOdatetime(year, month, day, hour, minute, second+sec100/100, tz="UTC")
    oce.debug(debug, "time[1:10]=",format(time[1:10]),"\n")
    v <- array(dim=c(profiles.to.read, number.of.cells, number.of.beams))
    a <- array(dim=c(profiles.to.read, number.of.cells, number.of.beams))
    q <- array(dim=c(profiles.to.read, number.of.cells, number.of.beams))
    nd <- number.of.cells * number.of.beams
    oce.debug(debug, "nd=",nd,";  header.length=", header.length,"\n")
    if (profiles.to.read > 0) {
        for (i in 1:profiles.to.read) {
            ## Doug does:
            ##   fseek(fid,adpProfiles(1)-1+18+8+58+(ii-1)*numCells(1)*2+(jj-1)*2,'bof');
            ##   v = fread(fid,numSamples,'int16=>double',adpPacketSize-1)/10;
            ## (he offsets by 83 ... but he also says the profile chunk size is 80+4*Nb*Nc+2-1,
            ## or 561, but I see repeats on length 562, so I think the formula should be 80+4*Nb*Nc+2.
            ## From that, I infer that there are 2 bytes after the profile data.
            ## I've tried (note using 1000 to get m/s)
            ##vv <- matrix(readBin(buf[profile.start[i] + header.length + 1:(2*nd)],
            ##                    "integer", n=nd, size=2, signed=TRUE, endian="little"), ncol=number.of.beams, byrow=FALSE)/1000
            vv <- matrix(readBin(buf[profile.start[i] + header.length + seq(0, 2*nd-1)],
                                 "integer", n=nd, size=2, signed=TRUE, endian="little"), ncol=number.of.beams, byrow=FALSE)/1000
            ##if (i == 1) {
            ##    print(t(matrix(buf[profile.start[i] + header.length + seq(0, 2*nd-1)],ncol=number.of.beams,byrow=FALSE)))
            ##    print(t(vv))
            ##}
            aa <- matrix(as.numeric(buf[profile.start[i] + header.length + 2*nd + seq(0, nd-1)]),
                         ncol=number.of.beams, byrow=FALSE)
            qq <- matrix(as.numeric(buf[profile.start[i] + header.length + 3*nd + seq(0, nd-1)]),
                         ncol=number.of.beams, byrow=FALSE)
            for (b in 1:number.of.beams) {
                v[i,,b] <- vv[,b]
                a[i,,b] <- aa[,b]
                q[i,,b] <- qq[,b]
            }
            if (monitor) {
                cat(".", ...)
                if (!(i %% 50)) cat(i, "\n", ...)
            }
        }
        if (monitor) cat("\nRead", profiles.to.read,  "of the", profiles.in.file, "profiles in", filename, "\n", ...)
        ma <- list(v=v, a=a, q=q)
    } else {
        ma <- NULL
    }
    data <- list(ma=ma,
                 ss=list(distance=seq(blanking.distance, by=cell.size, length.out=number.of.cells)),
                 ts=list(time=time,
                 temperature=temperature,
                 pressure=pressure,
                 heading=heading, pitch=pitch, roll=roll))
    beam.angle <- if (slant.angle == "?") 25 else slant.angle
    metadata <- list(filename=filename,
                     instrument.type="sontek",
                     serial.number=serial.number,
                     sampling.start=sampling.start,
                     sampling.end=sampling.end,
                     sampling.deltat=sampling.deltat,
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
        ## FIXME: using 25 degrees, which is FLAT-OUT wrong
        ##S  <- 1 / (3 * sin(25 * pi / 180))             # 0.7887339
        ##CS <- 1 / cos(30*pi/180) / sin(25*pi/180) / 2 # 1.366127 (30deg from 3-beam pattern)
        ##C  <- 1 / (3 * cos(25 * pi / 180))             # 0.3677926
        S  <- 1 / (3 * sin(beam.angle * pi / 180))             # 0.7887339
        CS <- 1 / cos(30*pi/180) / sin(beam.angle*pi/180) / 2 # 1.366127 (30deg from 3-beam pattern)
        C  <- 1 / (3 * cos(beam.angle * pi / 180))             # 0.3677926
        metadata$transformation.matrix <- matrix(c(2*S,  -S,  -S,
                                                   0  , -CS,  CS,
                                                   C  ,   C,   C),
                                                 nrow=3, byrow=TRUE)
        ## For later use, RC says that the PC-ADP uses
        ## T =  2.576  -1.288  -1.288
        ##      0.000  -2.230   2.230
        ##      0.345   0.345   0.345
        ## and these are by the same formulae, with 25 switched to 15 (different beam angle)
    } else stop("can only handle 3-beam devices")
    if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processing.log.item(log.action)
    res <- list(data=data, metadata=metadata, processing.log=log.item)
    class(res) <- c("sontek", "adp", "oce")
    res
}

sontek.time <- function(t, tz="UTC")
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
