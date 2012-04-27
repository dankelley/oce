## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

findInOrdered <- function(x, f)
{
    if (missing(x))
        stop("'x' missing")
    if (missing(f))
        stop("'f' missing")
    .Call("bisect", x, f)
}

filterSomething <- function(x, filter)
{
    if (is.raw(x)) {
        x <- as.numeric(x)
        replace <- mean(x, na.rm=TRUE)
        x[is.na(x)] <- replace
        res <- as.integer(filter(x, filter))
        res <- ifelse(res < 0, 0, res)
        res <- ifelse(res > 255, 255, res)
        res <- as.raw(res)
    } else {
        replace <- mean(x, na.rm=TRUE)
        x[is.na(x)] <- replace
        res <- filter(x, filter)
    }
    res
}

formatPosition <- function(latlon, isLat=TRUE, type=c("list", "string", "expression"))
{
    type <- match.arg(type)
    signs <- sign(latlon)
    x <- abs(latlon)
    degrees <- floor(x)
    minutes <- floor(60 * (x - degrees))
    seconds <- 3600 * (x - degrees - minutes / 60)
    seconds <- round(seconds, 2)
    noSeconds <- all(seconds == 0)
    noMinutes <- noSeconds & all(minutes == 0)
    hemispheres <- if (isLat) ifelse(signs, "N", "S") else ifelse(signs, "E", "W")
    oceDebug(0, "noSeconds=", noSeconds, "noMinutes=", noMinutes, "\n")
    if (type == "list") {
        if (noMinutes)
            rval <- list(degrees, hemispheres)
        else if (noSeconds)
            rval <- list(degrees, minutes, hemispheres)
        else
            rval <- list(degrees, minutes, seconds, hemispheres)
    } else if (type == "string") {
        if (noMinutes)
            rval <- sprintf("%02d %s", degrees, hemispheres)
        else if (noSeconds)
            rval <- sprintf("%02d %02d %s", degrees, minutes, hemispheres)
        else
            rval <- sprintf("%02d %02d %04.2f %s", degrees, minutes, seconds, hemispheres)
    } else if (type == "expression") {
        n <- length(degrees)
        rval <- vector("expression", n)
        for (i in 1:n) {
            if (noMinutes) 
                rval[i] <- as.expression(substitute(d*degree,
                                                    list(d=degrees[i])))
            else if (noSeconds)
                rval[i] <- as.expression(substitute(d*degree*phantom(.)*m*minute,
                                                    list(d=degrees[i],m=minutes[i])))
            else
                rval[i] <- as.expression(substitute(d*degree*phantom(.)*m*minute*phantom(.)*s*second,
                                                    list(d=degrees[i],m=minutes[i],s=seconds[i])))
        }
    }
    rval
}

smoothSomething <- function(x, ...)
{
    if (is.raw(x)) {
        x <- as.numeric(x)
        replace <- mean(x, na.rm=TRUE)
        x[is.na(x)] <- replace
        res <- as.integer(smooth(x, ...))
        res <- ifelse(res < 0, 0, res)
        res <- ifelse(res > 255, 255, res)
        res <- as.raw(res)
    } else {
        replace <- mean(x, na.rm=TRUE)
        x[is.na(x)] <- replace
        res <- smooth(x, ...)
    }
    res
}

binAverage <- function(x, y, xmin, xmax, xinc)
{
    if (missing(y))
        stop("must supply 'y'")
    if (missing(xmin))
        xmin <- min(as.numeric(x), na.rm=TRUE)
    if (missing(xmax))
        xmax <- max(as.numeric(x), na.rm=TRUE)
    if (missing(xinc))
        xinc  <- (xmax - xmin) / 10 
    if (xmax <= xmin)
        stop("must have xmax > xmin")
    if (xinc <= 0)
        stop("must have xinc > 0")
    nb <- floor(1 + (xmax - xmin) / xinc)
    if (nb < 1)
        stop("must have (xmin, xmax, xinc) such as to yield more than 0 bins")
    xx <- seq(xmin, xmax, xinc) + xinc / 2
    yy <- .C("bin_average", length(x), as.double(x), as.double(y),
             as.double(xmin), as.double(xmax), as.double(xinc),
             means=double(nb), NAOK=TRUE, PACKAGE="oce")$means
    list(x=xx, y=yy)
}

rescale <- function(x, xlow, xhigh, rlow=0, rhigh=1, clip=TRUE)
{
    x <- as.numeric(x)
    r <- range(x, na.rm=TRUE)
    if (missing(xlow))
        xlow <- min(x, na.rm=TRUE)
    if (missing(xhigh))
        xhigh <- max(x, na.rm=TRUE)
    rval <- rlow + (rhigh - rlow) * (x - xlow) / (xhigh - xlow)
    if (clip) {
        rval <- ifelse(rval < min(rlow, rhigh), rlow, rval)
        rval <- ifelse(rval > max(rlow, rhigh), rhigh, rval)
    }
    rval
}

retime <- function(x, a, b, t0, debug=getOption("oceDebug"))
{
    if (missing(x))
        stop("must give argument 'x'")
    if (missing(a))
        stop("must give argument 'a'")
    if (missing(b))
        stop("must give argument 'b'")
    if (missing(t0))
        stop("must give argument 't0'")
    oceDebug(debug, paste("\b\bretime.adv(x, a=", a, ", b=", b, ", t0=\"", format(t0), "\")\n"),sep="")
    rval <- x
    oceDebug(debug, "retiming x@data$time")
    rval@data$time <- x@data$time + a + b * (as.numeric(x@data$time) - as.numeric(t0))
    if ("timeSlow" %in% names(x@data)) {
        oceDebug(debug, "retiming x@data$timeSlow\n")
        rval@data$timeSlow <- x@data$timeSlow + a + b * (as.numeric(x@data$timeSlow) - as.numeric(t0))
    }
    rval@processingLog <- processingLog(rval@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "\b\b} # retime.adv()\n")
    rval
}

threenum <- function(x)
{
    if (is.raw(x))
        x <- as.numeric(x)
    if (sum(!is.na(x))) {
        c(min(x, na.rm=TRUE), mean(x, na.rm=TRUE), max(x, na.rm=TRUE))
    } else {
        c(NA, NA, NA)
    }
}

normalize <- function(x)
{
    var <- var(x, na.rm=TRUE)
    if (var == 0)
        rep(0, length(x))
    else
        (x - mean(x, na.rm=TRUE)) / sqrt(var)
}

detrend <- function(x,y)
{
    if (missing(x))
        stop("must give x")
    n <- length(x)
    if (missing(y)) {
        y <- x
        x <- 1:length(y)
    } else {
        if (length(y) != n)
            stop("x and y must be of same length, but they are ", n, " and ", length(y))
    }
    y - (y[1] + (y[n]-y[1]) * (x-x[1])/(x[n]-x[1]))
}

despike <- function(x, reference=c("median", "smooth", "trim"), n=4, k=7, min, max,
                    replace=c("reference","NA"))
{
    reference <- match.arg(reference)
    replace <- match.arg(replace)
    gave.min <- !missing(min)
    gave.max <- !missing(max)
    nx <- length(x)
    ## degap
    na <- is.na(x)
    if (sum(na) > 0) {
        i <- 1:nx
        x.gapless <- approx(i[!na], x[!na], i)$y
    } else {
        x.gapless <- x
    }
    if (reference == "median" || reference == "smooth") {
        if (reference == "median")
            x.reference <- runmed(x.gapless, k=k)
        else
            x.reference <- as.numeric(smooth(x.gapless))
        distance <- abs(x.reference - x.gapless)
        stddev <- sqrt(var(distance))
        bad <- distance > n * stddev
        nbad <- sum(bad)
        if (nbad > 0) {
            if (replace == "reference")
                x[bad] <- x.reference[bad]
            else
                x[bad] <- rep(NA, nbad)
        }
    } else if (reference == "trim") {
        if (!gave.min || !gave.max)
            stop("must give min and max")
        bad <- !(min <= x & x <= max)
        nbad <- length(bad)
        if (nbad > 0) {
            i <- 1:nx
            if (replace == "reference")
                x[bad] <- approx(i[!bad], x.gapless[!bad], i[bad])$y
            else
                x[bad] <- rep(NA, nbad)
        }
    } else {
        stop("unknown reference ", reference)
    }
    x
}
rangeLimit <- function(x, min, max)
{
    if (missing(min) && missing(max)) {
        minmax <- quantile(x, 0.005, 0.995)
        min <- minmax[1]
        max <- minmax[2]
    }
    ifelse(max < x | x < min, NA, x)
}
unabbreviateYear <- function(year)
{
    ## handle e.g. 2008 as 2008 (full year), 8 (year-2000 offset), or 108 (year 1900 offset)
    ##cat("year[1]=",year[1])
    ##rval <- ifelse(year > 1800, year, ifelse(year > 100, year + 1900, year + 2000))
    ##cat(" became ", rval[1], "\n")
    ##rval
    ifelse(year > 1800, year, ifelse(year > 50, year + 1900, year + 2000))
}

loggerToc <- function(dir, from, to, debug=getOption("oceDebug"))
{
    if (missing(dir))
        stop("need a 'dir', naming a directory containing a file with suffix .TBL, and also data files named in that file")
    tbl.files <- list.files(path=dir, pattern="*.TBL$")
    if (length(tbl.files) < 1)
        stop("could not locate a .TBL file in direcory ", dir)
    tref <- as.POSIXct("2010-01-01", tz="UTC") # arbitrary time, to make integers
    file.code <- NULL
    startTime <- NULL
    for (tbl.file in tbl.files) {
        oceDebug(debug, tbl.file)
        lines <- readLines(paste(dir, tbl.file, sep="/"))
        if (length(lines) < 1)
            stop("found no data in file ", paste(dir, tbl.file, sep="/"))
        ## "File \\day179\\SL08A179.023 started at Fri Jun 27 22:00:00 2008"
        for (line in lines) {
            s <- strsplit(line, "[ \t]+")[[1]]
            if (length(s) > 2) {
                filename <- s[2]
                month <- s[6]
                day <- s[7]
                hms <- s[8]
                year <- s[9]
                t <- as.POSIXct(strptime(paste(year, month, day, hms), "%Y %b %d %H:%M:%S", tz="UTC"))
                len <- nchar(filename)
                code <- substr(filename, len-6, len)
                oceDebug(debug, s, "(", code, format(t), ")\n")
                file.code <- c(file.code, code)
                startTime <- c(startTime, as.numeric(t) - as.numeric(tref))
            }
        }
    }
    prefix <- list.files(dir, pattern=".*[0-9]$")[1]
    lprefix <- nchar(prefix)
    prefix <- substr(prefix, 1, lprefix-7)
    filename <- paste(dir, paste(prefix, file.code, sep=""), sep="/")
    startTime <- as.POSIXct(startTime + tref)
    oceDebug(debug, "from=", format(from), "\n")
    oceDebug(debug, "to=", format(to), "\n")
    if (!missing(from) && !missing(to)) {
        oceDebug(debug, "got", length(file.code), "candidate files")
        ok <- from <= startTime & startTime <= to
        oceDebug(debug, "ok=", ok, "\n")
        filename <- filename[ok]
        startTime <- startTime[ok]
        oceDebug(debug, "taking into account the times, ended up with", length(file.code), "files\n")
    }
    list(filename=filename, startTime=startTime)
}

angleRemap <- function(theta)
{
    toRad <- atan2(1, 1) / 45
    atan2(sin(toRad * theta), cos(toRad * theta)) / toRad
}

unwrapAngle <- function(angle)
{
    toRad <- atan2(1, 1) / 45
    angle <- angle * toRad
    S <- sin(angle)
    C <- cos(angle)
    Smean <- mean(S, na.rm=TRUE)
    Smedian <- median(S, na.rm=TRUE)
    Cmean <- mean(C, na.rm=TRUE)
    Cmedian <- median(C, na.rm=TRUE)
    resMean <- atan2(Smean, Cmean)/toRad
    resMedian <- atan2(Smedian, Cmedian)/toRad
    resMean <- if (resMean < 0) resMean + 360 else resMean
    resMedian <- if (resMedian < 0) resMedian + 360 else resMedian
    list(mean=resMean, median=resMedian)
}

oceSpectrum <- function(x, ...)
{
    args <- list(...)
    want.plot <- FALSE
    if ("plot" %in% names(args)) {
        want.plot <- args$plot
        args$plot <- FALSE
        args$x <- x
        rval <- do.call(spectrum, args)
    }
    dt <- diff(rval$freq[1:2])
    normalize <- var(x) / (sum(rval$spec) * dt)
    rval$spec <- normalize * rval$spec
    if (want.plot)
        plot(rval)
    invisible(rval)
}

vectorShow <- function(v, msg, digits=5)
{
    n <- length(v)
    if (missing(msg))
        msg <- deparse(substitute(v))
    if (n == 0) {
        paste(msg, "(empty vector)\n")
    } else {
        if (is.numeric(v)) {
            if (n > 4) {
                vv <- format(v[c(1, 2, n-1, n)], digits=digits)
                paste(msg, ": ", vv[1], ", ", vv[2], ", ..., ", vv[3], ", ", vv[4], " (length ", n, ")\n", sep="")
            } else {
                paste(msg, ": ", paste(format(v, digits=digits), collapse=", "), "\n", sep="")
            }
        } else {
            if (n > 4) {
                paste(msg, ": ", v[1], ", ", v[2], ", ..., ", v[n-1], ", ", v[n], " (length ", n, ")\n", sep="")
            } else {
                paste(msg, ": ", paste(v, collapse=", "), "\n", sep="")
            }
        }
    }
}

fullFilename <- function(filename)
{
    first.char <- substr(filename, 1, 1)
    if (first.char == '/' || first.char == '~')
        return(filename)
    if (substr(filename, 1, 5) == "http:")
        return(filename)
    return(paste(getwd(), filename, sep="/"))
}
matrixSmooth <- function(m)
{
    if (missing(m))
        stop("must provide matrix 'm'")
    storage.mode(m) <- "double"
    .Call("matrix_smooth", m)
}

matchBytes <- function(input, b1, ...)
{
    if (missing(input))
        stop("must provide \"input\"")
    if (missing(b1))
        stop("must provide at least one byte to match")
    n <- length(input)
    dots <- list(...)
    lb <- 1 + length(dots)
    if (lb == 2)
        .Call("match2bytes", as.raw(input), as.raw(b1), as.raw(dots[[1]]), FALSE)
    else if (lb == 3)
        .Call("match3bytes", as.raw(input), as.raw(b1), as.raw(dots[[1]]), as.raw(dots[[2]]))
    else
        stop("must provide 2 or 3 bytes")
}

resizableLabel <- function(item=c("S", "T", "theta", "sigmaTheta",
                                  "conservative temperature", "absolute salinity",
                                  "nitrate", "nitrite", "oxygen", "phosphate", "silicate", "tritium",
                                  "spice",
                                  "p", "z", "distance", "heading", "pitch", "roll",
                                  "u", "v", "w", "speed", "direction",
                                  "eastward", "northward",
                                  "elevation"), axis=c("x", "y"))
{
    item <- match.arg(item)
    axis <- match.arg(axis)
    if (item == "T") {
        full <- expression(paste("Temperature [", degree, "C]"))
        abbreviated <- expression(paste("T [", degree, "C]"))
    } else if (item == "conservative temperature") {
        full <- expression(paste("Conservative Temperature [", degree, "C]"))
        abbreviated <- expression(paste(Theta, "[", degree, "C]"))
    } else if (item == "sigmaTheta") {
        full <- expression(paste("Potential density anomaly [", kg/m^3, "]"))
        abbreviated <- expression(paste(sigma[theta], " [", kg/m^3, "]"))
    } else if (item == "theta") {
        full <- expression(paste("Potential Temperature [", degree, "C]"))
        abbreviated <- expression(paste(theta, " [", degree, "C]"))
    } else if (item == "tritium") {
        full <- "Tritium Concentration [Tu]"
        abbreviated <- "Tritium [Tu]"
    } else if (item ==  "nitrate") {
        full <- "Nitrate Concentration [umol/kg]"
        abbreviated <- "NO3 [umol/kg]"
    } else if (item ==  "nitrite") {
        full <- "Nitrite Concentration [umol/kg]"
        abbreviated <- "NO2 [umol/kg]"
    } else if (item ==  "oxygen") {
        full <- "Oxygen Concentration [ml/l]"
        abbreviated <- "O2 [ml/l]"
    } else if (item ==  "phosphate") {
        full <- "Phosphate Concentration [umol/kg]"
        abbreviated <- "PO4 [umol/kg]"
    } else if (item ==  "silicate") {
        full <- "Silicate Concentration [umol/kg]"
        abbreviated <- "Si [umol/kg]"
    } else if (item == "spice") {
        full <- expression(paste("Spice [", kg/m^3, "]"))
        abbreviated <- full
    } else if (item == "S") {
        full <- "Salinity [PSU]"
        abbreviated <- "S [PSU]"
    } else if (item == "absolute salinity") {
        full <- expression(paste("Absolute Salinity [g/kg]"))
        abbreviated <- expression(paste(S[A], " [g/kg]"))
    } else if (item == "p") {
        full <- "Pressure [dbar]"
        abbreviated <- "P [dbar]"
    } else if (item == "z") {
        full <- "z [ m ]"
        abbreviated <- "z [m]"
    } else if (item == "distance") {
        full <- "Distance [m]"
        abbreviated <- "Dist. [m]"
    } else if (item == "heading") {
        full <- "Heading [deg]"
        abbreviated <- "Heading"
    } else if (item == "pitch") {
        full <- "Pitch [deg]"
        abbreviated <- "Pitch"
    } else if (item == "roll") {
        full <- "Roll [deg]"
        abbreviated <- "Roll"
    } else if (item == "u") {
        full <- "u [m/s]"
        abbreviated <- "u [m/s]"
    } else if (item == "v") {
        full <- "v [m/s]"
        abbreviated <- "v [m/s]"
    } else if (item == "w") {
        full <- "w [m/s]"
        abbreviated <- "w [m/s]"
    } else if (item == "eastward") {
        full <- "Eastward wind [m/s]"
        abbreviated <- "u [m/s]"
    } else if (item == "northward") {
        full <- "Northward wind [m/s]"
        abbreviated <- "v [m/s]"
    } else if (item == "elevation") {
        full <- "Elevation [m]"
        abbreviated <- "Elevation [m/s]"
    } else if (item ==  "speed") {
        full <- "Speed [m/s]"
        abbreviated <- "Speed [m/s]"
    }
    spaceNeeded <- strwidth(full, "inches")
    whichAxis <- if (axis == "x") 1 else 2
    spaceAvailable <- abs(par("fin")[whichAxis])
    fraction <- spaceNeeded / spaceAvailable
    ##cat("pin=", par('pin'), "\n")
    ##cat("spaceNeeded: in inches:", spaceNeeded, "\n")
    ##cat("whichAxis=", whichAxis, "\n")
    ##cat("spaceAvailable=", spaceAvailable, "\n")
    ##cat("fraction=", fraction, "\n")
    if (fraction < 1) full else abbreviated
}

latlonFormat <- function(lat, lon, digits=max(6, getOption("digits") - 1))
{
    n <- length(lon)
    rval <- vector("character", n)
    if (!is.numeric(lat) || !is.numeric(lon))
        return(paste("non-numeric lat (", lat, ") or lon (", lon, ")", sep=""))
    for (i in 1:n) {
        if (is.na(lat[i]) || is.na(lon[i]))
            rval[i] <- ""
        else
            rval[i] <- paste(format(abs(lat[i]), digits=digits),
                             if (lat[i] > 0) "N  " else "S  ",
                             format(abs(lon[i]), digits=digits),
                             if (lon[i] > 0) "E" else "W",
                             sep="")
    }
    rval
}

latFormat <- function(lat, digits=max(6, getOption("digits") - 1))
{
    n <- length(lat)
    if (n < 1) return("")
    rval <- vector("character", n)
    for (i in 1:n) {
        if (is.na(lat[i]))
            rval[i] <-  ""
        else
            rval[i] <- paste(format(abs(lat[i]), digits=digits),
                             if (lat[i] > 0) "N" else "S", sep="")
    }
    rval
}

lonFormat <- function(lon, digits=max(6, getOption("digits") - 1))
{
    n <- length(lon)
    if (n < 1) return("")
    rval <- vector("character", n)
    for (i in 1:n)
        if (is.na(lon[i]))
            rval[i] <- ""
        else
            rval[i] <- paste(format(abs(lon[i]), digits=digits),
                             if (lon[i] > 0) "E" else "S",
                             sep="")
    rval
}

GMTOffsetFromTz <- function(tz)
{
    ## Data are from
    ##   http://www.timeanddate.com/library/abbreviations/timezones/
    ## and hand-edited, so there may be errors.  Also, note that some of these
    ## contradict ... I've commented out conflicting definitions that I think
    ## will come up most rarely in use, but perhaps something better should
    ## be devised.  (Maybe this is not a problem.  Maybe only MEDS uses these,
    ## as opposed to GMT offsets, and maybe they only work in 5 zones, anyway...)
    if (tz == "A"   )   return( -1  ) # Alpha Time Zone Military        UTC + 1 hour
    if (tz == "ACDT")   return(-10.5) # Australian Central Daylight Time   Australia    UTC + 10:30 hours
    if (tz == "ACST")   return( -9.5) # Australian Central Standard Time  Australia     UTC + 9:30 hours
    if (tz == "ADT" )   return( 3   ) # Atlantic Daylight Time  North America   UTC - 3 hours
    if (tz == "AEDT")   return(-11  ) # Aus. East. Day. Time or Aus. East Summer Time Aus. UTC + 11 hours
    if (tz == "AEST")   return(-10  ) # Australian Eastern Standard Time  Australia UTC + 10 hours
    if (tz == "AKDT")   return(  8  ) # Alaska Daylight Time    North America   UTC - 8 hours
    if (tz == "AKST")   return(  9  ) # Alaska Standard Time    North America   UTC - 9 hours
    if (tz == "AST" )   return(  4  ) # Atlantic Standard Time  North America   UTC - 4 hours
    if (tz == "AWDT")   return( -9  ) # Australian Western Daylight Time        Australia       UTC + 9 hours
    if (tz == "AWST")   return( -8  ) # Australian Western Standard Time        Australia       UTC + 8 hours
    if (tz == "B"   )   return( -2  ) # Bravo Time Zone Military        UTC + 2 hours
    if (tz == "BST" )   return( -1  ) # British Summer Time     Europe  UTC + 1 hour
    if (tz == "C"   )   return( -3  ) # Charlie Time Zone       Military        UTC + 3 hours
    ##if (tz == "CDT")  return(-10.5) # Central Daylight Time   Australia       UTC + 10:30 hours
    if (tz == "CDT" )   return(  5  ) # Central Daylight Time   North America   UTC - 5 hours
    if (tz == "CEDT")   return( -2  ) # Central European Daylight Time  Europe  UTC + 2 hours
    if (tz == "CEST")   return( -2  ) # Central European Summer Time    Europe  UTC + 2 hours
    if (tz == "CET" )   return( -1  ) # Central European Time   Europe  UTC + 1 hour
    ##if (tz == "CST" ) return(-10.5) # Central Summer Time     Australia       UTC + 10:30 hours
    ##if (tz == "CST" ) return( -9.5) # Central Standard Time   Australia       UTC + 9:30 hours
    if (tz == "CST" )   return(  6  ) # Central Standard Time   North America   UTC - 6 hours
    if (tz == "CXT" )   return( -7  ) # Christmas Island Time   Australia       UTC + 7 hours
    if (tz == "D"   )   return( -4  ) # Delta Time Zone Military        UTC + 4 hours
    if (tz == "E"   )   return( -5  ) # Echo Time Zone  Military        UTC + 5 hours
    ##if (tz == "EDT" ) return( -11 ) # Eastern Daylight Time   Australia       UTC + 11 hours
    if (tz == "EDT" )   return(  4  ) # Eastern Daylight Time   North America   UTC - 4 hours
    if (tz == "EEDT")   return( -3  ) # Eastern European Daylight Time  Europe  UTC + 3 hours
    if (tz == "EEST")   return( -3  ) # Eastern European Summer Time    Europe  UTC + 3 hours
    if (tz == "EET")    return( -2  ) # Eastern European Time   Europe  UTC + 2 hours
    ##if (tz == "EST")  return( -11 ) # Eastern Summer Time     Australia       UTC + 11 hours
    ##if (tz == "EST")  return( -10 ) # Eastern Standard Time   Australia       UTC + 10 hours
    if (tz == "EST" )   return(  5  ) # Eastern Standard Time   North America   UTC - 5 hours
    if (tz == "F"   )   return( -6  ) # Foxtrot Time Zone       Military        UTC + 6 hours
    if (tz == "G"   )   return( -7  ) # Golf Time Zone  Military        UTC + 7 hours
    if (tz == "GMT" )   return(  0  ) # Greenwich Mean Time     Europe  UTC
    if (tz == "H"   )   return( -8  ) # Hotel Time Zone Military        UTC + 8 hours
    if (tz == "HAA" )   return(  3  ) # Heure Avancée de l'Atlantique   North America   UTC - 3 hours
    if (tz == "HAC" )   return(  5  ) # Heure Avancée du Centre North America   UTC - 5 hours
    if (tz == "HADT")   return(  9  ) # Hawaii-Aleutian Daylight Time   North America   UTC - 9 hours
    if (tz == "HAE" )   return(  4  ) # Heure Avancée de l'Est  North America   UTC - 4 hours
    if (tz == "HAP" )   return(  7  ) # Heure Avancée du Pacifique      North America   UTC - 7 hours
    if (tz == "HAR" )   return(  6  ) # Heure Avancée des Rocheuses     North America   UTC - 6 hours
    if (tz == "HAST")   return( 10  ) # Hawaii-Aleutian Standard Time   North America   UTC - 10 hours
    if (tz == "HAT" )   return(  2.5) # Heure Avancée de Terre-Neuve    North America   UTC - 2:30 hours
    if (tz == "HAY" )   return(  8  ) # Heure Avancée du Yukon  North America   UTC - 8 hours
    if (tz == "HNA" )   return(  4  ) # Heure Normale de l'Atlantique   North America   UTC - 4 hours
    if (tz == "HNC" )   return(  6  ) # Heure Normale du Centre North America   UTC - 6 hours
    if (tz == "HNE" )   return(  5  ) # Heure Normale de l'Est  North America   UTC - 5 hours
    if (tz == "HNP" )   return(  8  ) # Heure Normale du Pacifique      North America   UTC - 8 hours
    if (tz == "HNR" )   return(  7  ) # Heure Normale des Rocheuses     North America   UTC - 7 hours
    if (tz == "HNT" )   return(  3.5) # Heure Normale de Terre-Neuve    North America   UTC - 3:30 hours
    if (tz == "HNY" )   return(  9  ) # Heure Normale du Yukon  North America   UTC - 9 hours
    if (tz == "I"   )   return( -9  ) # India Time Zone Military        UTC + 9 hours
    if (tz == "IST" )   return( -1  ) # Irish Summer Time       Europe  UTC + 1 hour
    if (tz == "K"   )   return(-10  ) # Kilo Time Zone  Military        UTC + 10 hours
    if (tz == "L"   )   return(-11  ) # Lima Time Zone  Military        UTC + 11 hours
    if (tz == "M"   )   return(-12  ) # Mike Time Zone  Military        UTC + 12 hours
    if (tz == "MDT" )   return(  6  ) # Mountain Daylight Time  North America   UTC - 6 hours
    if (tz == "MESZ")   return( -2  ) # Mitteleuroäische Sommerzeit     Europe  UTC + 2 hours
    if (tz == "MEZ" )   return( -1  ) # Mitteleuropäische Zeit  Europe  UTC + 1 hour
    if (tz == "MST" )   return(  7  ) # Mountain Standard Time  North America   UTC - 7 hours
    if (tz == "N"   )   return(  1  ) # November Time Zone      Military        UTC - 1 hour
    if (tz == "NDT" )   return(  2.5) # Newfoundland Daylight Time      North America   UTC - 2:30 hours
    if (tz == "NFT" )   return(-11.5) # Norfolk (Island) Time   Australia       UTC + 11:30 hours
    if (tz == "NST" )   return(  3.5) # Newfoundland Standard Time      North America   UTC - 3:30 hours
    if (tz == "O"   )   return(  1  ) # Oscar Time Zone Military        UTC - 2 hours
    if (tz == "P"   )   return(  3  ) # Papa Time Zone  Military        UTC - 3 hours
    if (tz == "PDT" )   return(  7  ) # Pacific Daylight Time   North America   UTC - 7 hours
    if (tz == "PST" )   return(  8  ) # Pacific Standard Time   North America   UTC - 8 hours
    if (tz == "Q"   )   return(  4  ) # Quebec Time Zone        Military        UTC - 4 hours
    if (tz == "R"   )   return(  4  ) # Romeo Time Zone Military        UTC - 5 hours
    if (tz == "S"   )   return(  6  ) # Sierra Time Zone        Military        UTC - 6 hours
    if (tz == "T"   )   return(  7  ) # Tango Time Zone Military        UTC - 7 hours
    if (tz == "U"   )   return(  8  ) # Uniform Time Zone       Military        UTC - 8 hours
    if (tz == "UTC" )   return(  0  ) # Coordinated Universal Time      Europe  UTC
    if (tz == "V"   )   return(  9  ) # Victor Time Zone        Military        UTC - 9 hours
    if (tz == "W"   )   return( 10  ) # Whiskey Time Zone       Military        UTC - 10 hours
    if (tz == "WDT" )   return( -9  ) # Western Daylight Time   Australia       UTC + 9 hours
    if (tz == "WEDT")   return( -1  ) # Western European Daylight Time  Europe  UTC + 1 hour
    if (tz == "WEST")   return( -1  ) # Western European Summer Time    Europe  UTC + 1 hour
    if (tz == "WET")    return(  0  ) # Western European Time   Europe  UTC
    ##if (tz == "WST")  return( -9  ) # Western Summer Time     Australia       UTC + 9 hours
    if (tz == "WST")    return( -8  ) # Western Standard Time   Australia       UTC + 8 hours
    if (tz == "X"  )    return( 11  ) # X-ray Time Zone Military        UTC - 11 hours
    if (tz == "Y"  )    return( 12  ) # Yankee Time Zone        Military        UTC - 12 hours
    if (tz == "Z"  )    return(  0  ) # Zulu Time Zone  Military        UTC
}

gravity <- function(latitude=45, degrees=TRUE)
{
    if (degrees) latitude <- latitude * 0.0174532925199433
    9.780318*(1.0+5.3024e-3*sin(latitude)^2-5.9e-6*sin(2*latitude)^2)
}

makeFilter <- function(type=c("blackman-harris", "rectangular", "hamming", "hann"), m, asKernel=TRUE)
{
    type <- match.arg(type)
    if (missing(m))
        stop("must supply 'm'")
    i <- seq(0, m - 1)
    if (type == "blackman-harris") {    # See Harris (1978) table on p65
        if (m == 2 * floor(m/2)) {
            m <- m + 1
            warning("increased filter length by 1, to make it odd")
        }
        a <- c(0.35875, 0.488829, 0.14128, 0.01168) # 4-term (-92dB) coefficients
        ff <- pi * i / (m - 1)
        coef <- a[1] - a[2]*cos(2*ff) + a[3]*cos(4*ff) - a[4]*cos(6*ff)
    } else if (type == "rectangular") {
        coef <- rep(1 / m, m)
    } else if (type == "hamming") {
        coef <- 0.54 - 0.46 * cos(2 * pi * i / (m-1))
    } else if (type == "hann") {
        coef <- 0.50 - 0.50 * cos(2 * pi * i / (m-1))
    }
    coef <- coef / sum(coef)           # ensure unit sum
    if (!asKernel)
        return(coef)
    if (m == 2 * floor(m/2))
        stop("m must be odd")
    middle <- ceiling(m / 2)
    coef <- coef[middle:m]
    return(kernel(coef=coef, name=paste(type, "(", m, ")", sep="")))
}

oceFilter <- function(x, a=1, b, zero.phase=FALSE)
{
    if (missing(x))
        stop("must supply x")
    if (missing(b))
        stop("must supply b")
    if (!zero.phase) {
        return(.Call("oce_filter", x, a, b))
    } else {
        rval <- .Call("oce_filter", x, a, b)
        rval <- rev(rval)
        rval <- .Call("oce_filter", rval, a, b)
        return(rev(rval))
    }
}

## Calculation of geodetic distance on surface of earth,
## based upon datum defined by
##       a = radius of major axis of earth
##       f = flattening factor.
## The answer is returned in the same units as a; here in meters.
##
## Patterned after R code donated by Darren Gillis
geodXy <- function(lat, lon, lat.ref, lon.ref, rotate=0)
{
    a <- 6378137.00          # WGS84 major axis
    f <- 1/298.257223563     # WGS84 flattening parameter
    if (missing(lat))
        stop("must provide lat")
    if (missing(lon))
        stop("must provide lat")
    if (missing(lat.ref))
        stop("must provide lat.ref")
    if (missing(lon.ref))
        stop("must provide lon.ref")
    if (!is.finite(lat.ref))
        stop("lat.ref must be finite")
    if (!is.finite(lon.ref))
        stop("lat.ref must be finite")
    n <- length(lat)
    if (length(lon) != n)
        stop("lat and lon must be vectors of the same length")
    x <- y <- vector("numeric", n)
    xy  <- .C("geod_xy",
              as.integer(n),
              as.double(lat),
              as.double(lon),
              as.double(lat.ref),
              as.double(lon.ref),
              as.double(a),
              as.double(f),
              x = double(n),
              y = double(n),
              PACKAGE = "oce")
    x <- xy$x
    y <- xy$y
    if (rotate != 0) {
        S <- sin(rotate * pi / 180)
        C <- cos(rotate * pi / 180)
        r <- matrix(c(C,S,-S,C),nrow=2)
        rxy <- r %*% rbind(x,y)
        x <- rxy[1,]
        y <- rxy[2,]
    }
    data.frame(x, y)
}

geodDist <- function (lat1, lon1=NULL, lat2=NULL, lon2=NULL, alongPath=FALSE)
{
    a <- 6378137.00          # WGS84 major axis
    f <- 1/298.257223563     # WGS84 flattening parameter
    if (inherits(lat1, "section")) {
        ##cat("\nSECTION\n")
        ##print(lat1[["latitude", "byStation"]])
        ##print(lat1[["longitude", "byStation"]])
        if (alongPath)
            return(.Call("geoddist_alongpath", lat1[["latitude", "byStation"]], lat1[["longitude", "byStation"]], a, f) / 1000)
        copy <- lat1
        n <- length(copy@data$station)
        lat1 <- vector("numeric", n)
        lon1 <- vector("numeric", n)
        for (i in 1:n) {
            lat1[i] <- copy@data$station[[i]]@metadata$latitude
            lon1[i] <- copy@data$station[[i]]@metadata$longitude
        }
        res <- vector("numeric", n)
        for (i in 1:n) {
            if (is.finite(lat1[1]) && is.finite(lon1[1]) && is.finite(lat1[i]) && is.finite(lon1[i])) {
                ## dist <- .Fortran("geoddist",
                dist <- .C("geoddist",
                           as.double(lat1[1]),
                           as.double(lon1[1]),
                           as.double(lat1[i]),
                           as.double(lon1[i]),
                           as.double(a),
                           as.double(f),
                           as.double(1),
                           as.double(1),
                           dist = double(1),
                           PACKAGE = "oce")$dist
            } else {
                dist <- NA
            }
            res[i] <- dist
        }
    } else {
        ##cat("\nCOMPONENTS\n")
        ##print(lat1)
        ##print(lon1)
        if (alongPath)
            return(.Call("geoddist_alongpath", lat1, lon1, a, f) / 1000)
        n1 <- length(lat1)
        if (length(lon1) != n1)
            stop("lat1 and lon1 must be vectors of the same length")
        n2 <- length(lat2)
        if (length(lon2) != n2)
            stop("lat2 and lon2 must be vectors of the same length")
        if (n2 < n1) { # take only first one
            if (n2 != 1) warning("Using just the first element of lat2 and lon2, even though it contains more elements")
            llat2 <- rep(lat2[1], n1)
            llon2 <- rep(lon2[1], n1)
        } else {
            llat2 <- lat2
            llon2 <- lon2
        }
                                        #subroutine geoddist(DLAT1,DLON1,DLAT2,DLON2,A,F,FAZ,BAZ,S)
        res <- vector("numeric", n1)
        for (i in 1:n1) {
                                        #cat("values=",lat1[i],lon1[i],llat2[i],llon2[i],"\n")
            if (is.finite(lat1[i]) && is.finite(lon1[i]) && is.finite(llat2[i]) && is.finite(llon2[i])) {
                ## res[i] <- .Fortran("geoddist",
                res[i] <- .C("geoddist",
                             as.double(lat1[i]),
                             as.double(lon1[i]),
                             as.double(llat2[i]),
                             as.double(llon2[i]),
                             as.double(a),
                             as.double(f),
                             as.double(1),
                             as.double(1),
                             dist = double(1),
                             PACKAGE = "oce")$dist
            } else {
                res[i] <- NA
            }
        }
    }
    res / 1000
}

interpBarnes <- function(x, y, z, w=NULL, xg=NULL, yg=NULL,
                         xr=NULL, yr=NULL, gamma=0.5, iterations=2)
{
    n <- length(x)
    if (length(y) != n)
        stop("lengths of x and y disagree; they are ", n, " and ", length(y))
    if (length(z) != n)
        stop("lengths of x and z disagree; they are ", n, " and ", length(z))
    if (is.null(w))
        w <- rep(1.0, length(x))
    if (is.null(xg))
        xg <- pretty(x, n=50)
    if (is.null(yg)) {
        if (0 == diff(range(y))) {
            yg <- y[1]
        } else {
            yg <- pretty(y, n=50)
        }
    }
    if (is.null(xr)) {
        xr <- diff(range(x)) / sqrt(n)
        if (xr == 0)
            xr <- 1
    }
    if (is.null(yr)) {
        yr <- diff(range(y)) / sqrt(n)
        if (yr == 0)
            yr <- 1
    }
    zg <- .Call("interp_barnes",
                as.double(x),
                as.double(y),
                as.double(z),
                as.double(w),
                as.double(xg),
                as.double(yg),
                as.double(xr),
                as.double(yr),
                as.double(gamma),
                as.integer(iterations))
    list(xg=xg, yg=yg, zg=zg)
}

coriolis <- function(lat, degrees=TRUE)
{
    ## Siderial day 86164.1 s.
    if (degrees) lat <- lat * 0.0174532925199433
    1.458423010785138e-4 * sin(lat)
}

undriftTime <- function(x, slowEnd = 0, tname="time")
{
    if (!inherits(x, "oce"))
        stop("method is only for oce objects")
    names <- names(x@data)
    if (!(tname %in% names))
        stop("no column named '", tname, "'; only found: ", paste(names, collapse=" "))
    rval <- x
    time <- rval@data[[tname]]
    nt <- length(time)
    if (nt < 2) warning("too few data to to undrift time; returning object unaltered")
    else {
        sampleInterval <- as.numeric(difftime(time[2], time[1], units="s"))
        nt <- length(time)
        nt.out <- floor(0.5 + nt + slowEnd / sampleInterval)
        time.out <- seq.POSIXt(from=time[1], by=sampleInterval, length.out=nt.out)
        i <- seq(from=1, by=1, length.out=nt)
        i.out <- seq(from=1, to=nt, length.out = nt.out)
        out <- data.frame(array(dim=c(nt.out, length(x@data))))
        names(out) <- names
        out[[tname]] <- time.out
        for (name in names) {
            if (name != tname) {
                yy <- approx(x=i, y=x@data[[name]], xout=i.out)$y
                out[[name]] <- yy
            }
        }
        rval@data <- out
    }
    rval@processingLog <- processingLog(rval@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    rval
}

fillGap <- function(x, method=c("linear"), rule=1)
{
    if (!is.numeric(x))
        stop("only works for numeric 'x'")
    if (!is.vector(x))
        stop("only works for vector 'x'")
    method <- match.arg(method)
    class <- class(x)
    x <- as.numeric(x)
    res <- .Call("fillgap", x, rule)
    class(res) <-  class
    res
}

oceColorsGebco <- function(n=9, region=c("water", "land", "both"), type=c("fill","line"))
{
    region <- match.arg(region)
    type <- match.arg(type)
    if (type == "fill") {
        ## generate land colors by e.g. rgb(t(col2rgb(land[5])-1*c(10,4,10))/255)
        land <- c("#FBC784","#F1C37A","#E6B670","#DCA865","#D19A5C",
                  "#C79652","#BD9248","#B38E3E","#A98A34")
        water <- c("#E1FCF7","#BFF2EC","#A0E8E4","#83DEDE","#68CDD4",
                   "#4FBBC9","#38A7BF","#2292B5","#0F7CAB")
    } else {
        land <- c("#FBC784","#F1C37A","#E6B670","#DCA865","#D19A5C",
                  "#C79652","#BD9248","#B38E3E","#A98A34")
        water <- c("#A4FCE3","#72EFE9","#4FE3ED","#47DCF2","#46D7F6",
                   "#3FC0DF","#3FC0DF","#3BB7D3","#36A5C3","#3194B4",
                   "#2A7CA4","#205081","#16255E","#100C2F")
    }
    if (region == "water") {
        rgb.list <- col2rgb(water) / 255
        l <- length(water)
        r <- approx(1:l, rgb.list[1,1:l], xout=seq(1, l, length.out=n))$y
        g <- approx(1:l, rgb.list[2,1:l], xout=seq(1, l, length.out=n))$y
        b <- approx(1:l, rgb.list[3,1:l], xout=seq(1, l, length.out=n))$y
    } else if (region == "land") {
        rgb.list <- col2rgb(land) / 255
        l <- length(land)
        r <- approx(1:l, rgb.list[1,1:l], xout=seq(1, l, length.out=n))$y
        g <- approx(1:l, rgb.list[2,1:l], xout=seq(1, l, length.out=n))$y
        b <- approx(1:l, rgb.list[3,1:l], xout=seq(1, l, length.out=n))$y
    } else {                            # both
        rgb.list <- col2rgb(c(land ,water)) / 255
        l <- length(land) + length(water)
        r <- approx(1:l, rgb.list[1,1:l], xout=seq(1, l, length.out=n))$y
        g <- approx(1:l, rgb.list[2,1:l], xout=seq(1, l, length.out=n))$y
        b <- approx(1:l, rgb.list[3,1:l], xout=seq(1, l, length.out=n))$y
    }
    rgb(r, g, b)
}

addColumn <- function (x, data, name)
{
    if (!inherits(x, "oce"))
        stop("method is only for oce objects")
    if (missing(data))
        stop("must supply data")
    if (missing(name))
        stop("must supply name")
    n <- length(data)
    nd <- length(x@data)
    if (n != length(data))
        stop("data length is ", n, " but it must be ", nd, " to match existing data")
    if (inherits(x, "ctd")) {
        rval <- ctdAddColumn(x, data, name)
    } else {
        rval <- x
        rval@data[[name]] <- data
    }
    rval@processingLog <- processingLog(rval@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    rval
}

decimate <- function(x, by=10, to, filter, debug=getOption("oceDebug"))
{
    if (!inherits(x, "oce"))
        stop("method is only for oce objects")
    oceDebug(debug, "in decimate(x,by=", by, ",to=", if (missing(to)) "unspecified" else to, "...)\n")
    res <- x
    do.filter <- !missing(filter)
    if (missing(to))
        to <- length(x@data$time[[1]])
    if (length(by) == 1) { # FIXME: probably should not be here
        select <- seq(from=1, to=to, by=by)
        oceDebug(debug, vectorShow(select, "select:"))
    }
    if (inherits(x, "adp")) {
        oceDebug(debug, "decimate() on an ADP object\n")
        warning("decimate(adp) not working yet ... just returning the adp unchanged")
        return(res) # FIXME
        nbeam <- dim(x@data$v)[3]
        for (name in names(x@data)) {
            oceDebug(debug, "decimating item named '", name, "'\n")
            if ("distance" == name)
                next
            if ("time" == name) {
                res@data[[name]] <- x@data[[name]][select]
            } else if (is.vector(x@data[[name]])) {
                oceDebug(debug, "subsetting x@data$", name, ", which is a vector\n", sep="")
                if (do.filter)
                    res@data[[name]] <- filterSomething(x@data[[name]], filter)
                res@data[[name]] <- res@data[[name]][select]
            } else if (is.matrix(x@data[[name]])) {
                dim <- dim(x@data[[name]])
                for (j in 1: dim[2]) {
                    oceDebug(debug, "subsetting x@data[[", name, ",", j, "]], which is a matrix\n", sep="")
                    if (do.filter) 
                        res@data[[name]][,j] <- filterSomething(x@data[[name]][,j], filter)
                    res@data[[name]][,j] <- res@data[[name]][,j][select]
                }
            } else if (is.array(x@data[[name]])) {
                dim <- dim(x@data[[name]])
                print(dim)
                for (k in 1:dim[2]) {
                    for (j in 1:dim[3]) {
                        oceDebug(debug, "subsetting x@data[[", name, "]][", j, ",", k, "], which is an array\n", sep="")
                        if (do.filter)
                            res@data[[name]][,j,k] <- filterSomething(x@data[[name]][,j,k], filter)
                        res@data[[name]][,j,k] <- res@data[[name]][,j,k][select]
                    }
                }
            }
        }
    } else if (inherits(x, "adv")) { # FIXME: the (newer) adp code is probably better than this ADV code
        oceDebug(debug, "decimate() on an ADV object\n")
        warning("decimate(adv) not working yet ... just returning the adv unchanged")
        return(res) # FIXME
        for (name in names(x@data)) {
            if ("time" == name) {
                res@data[[name]] <- x@data[[name]][select]
            } else if (is.vector(x@data[[name]])) {
                oceDebug(debug, "subsetting x@data$", name, ", which is a vector\n", sep="")
                if (do.filter)
                    res@data[[name]] <- filterSomething(x@data[[name]], filter)
                res@data[[name]] <- res@data[[name]][select]
            } else if (is.matrix(x@data[[name]])) {
                dim <- dim(x@data[[name]])
                for (j in 1: dim[2]) {
                    oceDebug(debug, "subsetting x@data[[", name, ",", j, "]], which is a matrix\n", sep="")
                    if (do.filter) 
                        res@data[[name]][,j] <- filterSomething(x@data[[name]][,j], filter)
                    res@data[[name]][,j] <- res@data[[name]][,j][select]
                }
            } else if (is.array(x@data[[name]])) {
                dim <- dim(x@data[[name]])
                for (k in 1:dim[2]) {
                    for (j in 1: dim[3]) {
                        oceDebug(debug, "subsetting x@data[[", name, ",", j, ",", k, "]], which is an array\n", sep="")
                        if (do.filter)
                            res@data[[name]][,j,k] <- filterSomething(x@data[[name]][,j,k], filter)
                        res@data[[name]][,j,k] <- res@data[[name]][,j,k][select]
                    }
                }
            } else {
                stop("item data[[", name, "]] is not understood; it must be a vector, a matrix, or an array")
            }
        }
    } else if (inherits(x, "ctd")) {
        warning("decimate(ctd) not working yet ... just returning the ctd unchanged")
        return(res) # FIXME
        if (do.filter)
            stop("cannot (yet) filter ctd data during decimation") # FIXME
        select <- seq(1, dim(x@data)[1], by=by)
        res@data <- x@data[select,]
    } else if (inherits(x, "pt")) {
        warning("decimate(pt) not working yet ... just returning the pt unchanged")
        return(res) # FIXME
        if (do.filter)
            stop("cannot (yet) filter pt data during decimation") # FIXME
        for (name in names(res@data))
            res@data[[name]] <- x@data[[name]][select]
    } else if (inherits(x, "echosounder")) {
        oceDebug(debug, "decimate() on an 'echosounder' object\n")
        ## use 'by', ignoring 'to' and filter'
        if (length(by) != 2)
            stop("length(by) must equal 2.  First element is width of boxcar in pings, second is width in depths")
        by <- as.integer(by)
        byPing <- by[1]
        kPing <- as.integer(by[1])
        if (0 == kPing%%2)
            kPing <- kPing + 1
        byDepth <- by[2]
        kDepth <- as.integer(by[2])
        if (0 == kDepth%%2)
            kDepth <- kDepth + 1
        if (byDepth > 1) {
            depth <- x[["depth"]]
            a <- x[["a"]]
            ncol <- ncol(a)
            nrow <- nrow(a)
            ii <- 1:ncol
            depth2 <- binAverage(ii, depth, 1, ncol, byDepth)$y
            a2 <- matrix(nrow=nrow(a), ncol=length(depth2))
            for (r in 1:nrow)
                a2[r,] <- binAverage(ii, runmed(a[r,], kDepth), 1, ncol, byDepth)$y
            res <- x
            res[["depth"]] <- depth2
            res[["a"]] <- a2
            x <- res # need for next step
        }
        if (byPing > 1) {
            time <- x[["time"]]
            a <- x[["a"]]
            ncol <- ncol(a)
            nrow <- nrow(a)
            jj <- 1:nrow
            time2 <- binAverage(jj, as.numeric(x[["time"]]), 1, nrow, byPing)$y + as.POSIXct("1970-01-01 00:00:00", tz="UTC")
            a2 <- matrix(nrow=length(time2), ncol=ncol(a))
            for (c in 1:ncol)
                a2[,c] <- binAverage(jj, runmed(a[,c], kPing), 1, nrow, byPing)$y
            res <- x
            res[["time"]] <- time2
            res[["latitude"]] <- binAverage(jj, x[["latitude"]], 1, nrow, byPing)$y
            res[["longitude"]] <- binAverage(jj, x[["longitude"]], 1, nrow, byPing)$y
            res[["a"]] <- a2
        }
        ## do depth, rows of matrix, time, cols of matrix
    } else {
        stop("decimation does not work (yet) for objects of class ", paste(class(x), collapse=" "))
    }
    if ("deltat" %in% names(x@metadata)) # FIXME: should handle for individual cases, not here
        res@metadata$deltat <- by * x@metadata$deltat
    res@processingLog <- processingLog(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}

oceSmooth <- function(x, ...)
{
    if (!inherits(x, "oce"))
        stop("method is only for oce objects")
    res <- x
    if (inherits(x, "adp")) {
        stop("cannot smooth ADP objects (feel free to request this from the author)")
    } else if (inherits(x, "adv")) {
        for (name in names(x@data)) {
            if (length(grep("^time", name)))
                next
            if (is.vector(x@data[[name]])) {
                oceDebug(debug, "smoothing x@data$", name, ", which is a vector\n", sep="")
                res@data[[name]] <- smooth(x@data[[name]], ...)
            } else if (is.matrix(x@data[[name]])) {
                for (j in 1: dim(x@data[[name]])[2]) {
                    oceDebug(debug, "smoothing x@data[[", name, ",", j, "]], which is a matrix\n", sep="")
                    res@data[[name,j]] <- smooth(x@data[[name,j]], ...)
                }
            } else if (is.array(x@data[[name]])) {
                dim <- dim(x@data[[name]])
                for (k in 1:dim[2]) {
                    for (j in 1: dim[3]) {
                        oceDebug(debug, "smoothing x@data[[", name, ",", j, "]], which is an arry \n", sep="")
                        res@data[[name,j,k]] <- smooth(x@data[[name,j,k]], ...)
                    }
                }
            }
        }
        warning("oceSmooth() has recently been recoded for 'adv' objects -- do not trust it yet!")
    } else if (inherits(x, "ctd")) {
        res <- x
        for (name in names(x@data))
            res@data[[name]] <- smooth(x@data[[name]], ...)
    } else {
        stop("smoothing does not work (yet) for objects of class ", paste(class(x), collapse=" "))
    }
    res@processingLog <- processingLog(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}

stickplot <- function(t, x, y, ...)
{
    ylim <- max(y, na.rm=TRUE) * c(-1, 1)
    plot(range(t), ylim, type="n")
    tstart <- t[1]
    t.isPOSIXlt <- inherits(t, "POSIXlt")
    t.isPOSIXct <- inherits(t, "POSIXct")
    if (t.isPOSIXct) t <- unclass(t)
    if (t.isPOSIXlt) t <- unclass(as.POSIXct(t))
    usr <- par("usr")
    pin <- par("pin")
    tx.scale <- (usr[2]-usr[1]) / (usr[4]-usr[3]) * pin[2] / pin[1]
    n <- length(x)
    xx <- array(dim = 3 * n)
    yy <- array(dim = 3 * n)
    ones <- seq(1, 3*n, 3)
    twos <- seq(2, 3*n, 3)
    threes <- seq(3, 3*n, 3)
    xx[ones] <- t
    yy[ones] <- 0
    xx[twos] <- t + x * tx.scale
    yy[twos] <- y
    xx[threes] <- NA
    yy[threes] <- NA
    lines(xx, yy, type='l', ...)
    ##points(xx[ones],yy[ones],col="red")
}

bcdToInteger <- function(x, endian=c("little", "big"))
{
    endian <- match.arg(endian)
    x <- as.integer(x)
    byte1 <- as.integer(floor(x / 16))
    byte2 <- x - 16 * byte1
    if (endian=="little") 10*byte1 + byte2 else byte1 + 10*byte2
}

byteToBinary <- function(x, endian=c("little", "big"))
{
    onebyte2binary <- function(x)
    {
        c("0000","0001","0010","0011",
          "0100","0101","0110","0111",
          "1000","1001","1010","1011",
          "1100","1101","1110","1111")[x+1]
    }
    endian <- match.arg(endian)
    rval <- NULL
    if (class(x) == "raw")
        x <- readBin(x, "int", n=length(x), size=1, signed=FALSE)
    for (i in 1:length(x)) {
        if (x[i] < 0) {
            rval <- c(rval, "??")
        } else {
            byte1 <- as.integer(floor(x[i] / 16))
            byte2 <- x[i] - 16 * byte1
            ##cat("input=",x[i],"byte1=",byte1,"byte2=",byte2,"\n")
            if (endian == "little")
                rval <- c(rval, paste(onebyte2binary(byte2), onebyte2binary(byte1), sep=""))
            else
                rval <- c(rval, paste(onebyte2binary(byte1), onebyte2binary(byte2), sep=""))
            ##cat(" rval=",rval,"\n")
        }
    }
    rval
}

formatCI <- function(ci, style=c("+/-", "parentheses"), model, digits=NULL)
{
    formatCI.one <- function(ci, style, digits=NULL)
    {
        debug <- FALSE
        if (missing(ci))
            stop("must supply ci")
        ci <- as.numeric(ci)
        if (length(ci) == 3) {
            x <- ci[2]
            ci <- ci[c(1,3)]
        } else if (length(ci) == 2) {
            x <- mean(ci)
        } else {
            stop("ci must contain 2 or 3 elements")
        }
        sign <- sign(x)
        x <- abs(x)
        if (style == "+/-") {
            pm <- abs(diff(ci)/2)
            if (is.null(digits))
                paste(format(sign * x, digits=getOption("digits")), "+/-", format(pm, digits=getOption("digits")), sep="")
            else
                paste(format(sign * x, digits=digits), "+/-", format(pm, digits=digits), sep="")
        } else {
            pm <- abs(diff(ci)/2)
            scale <- 10^floor(log10(pm))
            pmr <- round(pm / scale)
            if (pmr == 10) {
                pmr <- 1
                scale <- scale * 10
            }
            ##scale <- 10^floor(log10(x))
            x0 <- x / scale
            ci0 <- ci / scale
            if (pm > x) return(paste(sign*x, "+/-", pm, sep=""))
            digits <- floor(log10(scale) + 0.1)
            if (digits < 0)
                fmt <- paste("%.", abs(digits), "f", sep="")
            else
                fmt <- "%.f"
            oceDebug(debug, "pm=", pm, ";pmr=", pmr, "; scale=", scale, "pm/scale=", pm/scale, "round(pm/scale)=", round(pm/scale), "\n", " x=", x,  "; x/scale=", x/scale, "digits=",digits,"fmt=", fmt, "\n")
            paste(sprintf(fmt, sign*x), "(", pmr, ")", sep="")
        }
    }
    style <- match.arg(style)
    if (!missing(model)) {
        cm <- class(model)
        ## > qt(0.6914619, 100000)
        ## [1] 0.5
        if (cm == "lm" || cm == "nls") {
            ci <- confint(model, level=0.6914619)
            names <- dimnames(ci)[[1]]
            n <- length(names)
            rval <- matrix("character", nrow=n, ncol=1)
            rownames(rval) <- names
            colnames(rval) <- "value"
            for (row in 1:dim(ci)[1]) {
                rval[row,1] <- formatCI.one(ci=ci[row,], style=style, digits=digits)
            }
        }
        rval
    } else {
        if (missing(ci))
            stop("must give either ci or model")
        formatCI.one(ci=ci, style=style, digits=digits)
    }
}

integerToAscii <- function(i)
{
    c("", "\001", "\002", "\003", "\004", "\005", "\006", "\a", "\b",
      "\t", "\n", "\v", "\f", "\r", "\016", "\017", "\020", "\021",
      "\022", "\023", "\024", "\025", "\026", "\027", "\030", "\031",
      "\032", "\033", "\034", "\035", "\036", "\037", " ", "!", "\"",
      "#", "$", "%", "&", "'", "(", ")", "*", "+", ",", "-", ".", "/",
      "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", ":", ";", "<",
      "=", ">", "?", "@", "A", "B", "C", "D", "E", "F", "G", "H", "I",
      "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V",
      "W", "X", "Y", "Z", "[", "\\", "]", "^", "_", "`", "a", "b",
      "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o",
      "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "{", "|",
      "}", "~", "\177", "\x80", "\x81", "\x82", "\x83", "\x84", "\x85",
      "\x86", "\x87", "\x88", "\x89", "\x8a", "\x8b", "\x8c", "\x8d",
      "\x8e", "\x8f", "\x90", "\x91", "\x92", "\x93", "\x94", "\x95",
      "\x96", "\x97", "\x98", "\x99", "\x9a", "\x9b", "\x9c", "\x9d",
      "\x9e", "\x9f", "\xa0", "\xa1", "\xa2", "\xa3", "\xa4", "\xa5",
      "\xa6", "\xa7", "\xa8", "\xa9", "\xaa", "\xab", "\xac", "\xad",
      "\xae", "\xaf", "\xb0", "\xb1", "\xb2", "\xb3", "\xb4", "\xb5",
      "\xb6", "\xb7", "\xb8", "\xb9", "\xba", "\xbb", "\xbc", "\xbd",
      "\xbe", "\xbf", "\xc0", "\xc1", "\xc2", "\xc3", "\xc4", "\xc5",
      "\xc6", "\xc7", "\xc8", "\xc9", "\xca", "\xcb", "\xcc", "\xcd",
      "\xce", "\xcf", "\xd0", "\xd1", "\xd2", "\xd3", "\xd4", "\xd5",
      "\xd6", "\xd7", "\xd8", "\xd9", "\xda", "\xdb", "\xdc", "\xdd",
      "\xde", "\xdf", "\xe0", "\xe1", "\xe2", "\xe3", "\xe4", "\xe5",
      "\xe6", "\xe7", "\xe8", "\xe9", "\xea", "\xeb", "\xec", "\xed",
      "\xee", "\xef", "\xf0", "\xf1", "\xf2", "\xf3", "\xf4", "\xf5",
      "\xf6", "\xf7", "\xf8", "\xf9", "\xfa", "\xfb", "\xfc", "\xfd",
      "\xfe", "\xff")[i+1]
}

applyMagneticDeclination <- function(x, declination=0, debug=getOption("oceDebug"))
{
    oceDebug(debug, "\b\bapplyMagneticDeclination(x,declination=", declination, ") {\n", sep="")
    if (inherits(x, "cm")) {
        oceDebug(debug, "object is of type 'cm'\n")
        rval <- x
        S <- sin(-declination * pi / 180)
        C <- cos(-declination * pi / 180)
        r <- matrix(c(C, S, -S, C), nrow=2)
        uvr <- r %*% rbind(x@data$u, x@data$v)
        rval@data$u <- uvr[1,]
        rval@data$v <- uvr[2,]
        oceDebug(debug, "originally, first u:", x@data$u[1:3], "\n")
        oceDebug(debug, "originally, first v:", x@data$v[1:3], "\n")
        oceDebug(debug, "after application, first u:", rval@data$u[1:3], "\n")
        oceDebug(debug, "after application, first v:", rval@data$v[1:3], "\n")
    } else {
        stop("cannot apply declination to object of class ", paste(class(x), collapse=", "), "\n")
    }
    rval@processingLog <- processingLog(rval@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "\b\b} # applyMagneticDeclination\n")
    rval
}

magneticDeclination <- function(lat, lon, date)
{
    if (missing(lat) || missing(lon) || missing(date))
        stop("must provide lat, lon, and date")
    dim <- dim(lat)
    if (!all(dim == dim(lon)))
        stop("dimensions of lat and lon must agree")
    n <- length(lat)
    if (length(date) == 1) {
        date <- rep(date, n)
    } else {
        if (!all(dim == dim(date)))
            stop("dimensions of lat and date must agree")
    }
    if (!is.null(dim)) {
        dim(lat) <- n
        dim(lon) <- n
        dim(date) <- n
    }
    isv <- 0
    itype <- 1                          # geodetic
    alt <- 0.0                          # altitude in km
    colat <- 90 - lat
    elong <- ifelse(lon < 0, 360 + lon, lon)
    r <- .Fortran("md_driver", as.double(colat), as.double(elong), as.double(date),
                  as.integer(n), dev=double(n))
    rval <- r$dev
    if (!is.null(dim))
        dim(rval) <- dim
    rval
}

secondsToCtime <- function(sec)
{
    if (sec < 60)
        return(sprintf("00:00:%02d", sec))
    if (sec < 3600) {
        min <- floor(sec / 60)
        sec <- sec - 60 * min
        return(sprintf("00:%02d:%02d", min, sec))
    }
    hour <- floor(sec / 3600)
    sec <- sec - 3600 * hour
    min <- floor(sec / 60)
    sec <- sec - 60 * min
    return(sprintf("%02d:%02d:%02d", hour, min, sec))
}

ctimeToSeconds <- function(ctime)
{
    if (length(grep(":", ctime)) > 0) {
        parts <- as.numeric(strsplit(ctime, ":")[[1]])
        l <- length(parts)
        if (l == 1) s <- as.numeric(ctime)
        else if (l == 2) s <- parts[1] * 60 + parts[2]
        else if (l == 3) s <- parts[1] * 3600 + parts[2] * 60 + parts[3]
        else stop("cannot interpret \"time\"=", ctime, "as a time interval because it has more than 2 colons")
    } else {
        s <- as.numeric(ctime)
    }
    s
}

##showThrees <- function(x, indent="    ")
##{
##    if (!("threes" %in% names(x)))
##        stop("'x' has no item named 'threes'")
##    rownames <- rownames(x$threes)
##    colnames <- colnames(x$threes)
##    data.width <- max(nchar(colnames)) + 10
##    name.width <- max(nchar(rownames(x$threes))) + 4 # space for left-hand column
##    ncol <- length(colnames)
##    nrow <- length(rownames)
##    res <- indent
##    res <- paste(res, format(" ", width=1+name.width), collapse="")
##    res <- paste(res, paste(format(colnames, width=data.width, justify="right"), collapse=" "))
##    res <- paste(res, "\n", sep="")
##    digits <- max(5, getOption("digits") - 1)
##    for (irow in 1L:nrow) {
##        res <- paste(res, indent, format(rownames[irow], width=name.width), "  ", sep="") # FIXME: should not need the "  "
##        for (icol in 1L:ncol) {
##            res <- paste(res, format(x$threes[irow,icol], digits=digits, width=data.width, justify="right"), sep=" ")
##        }
##        res <- paste(res, "\n", sep="")
##    }
##    res
##}

oceDebug <- function(debug=0, ...)
{
    debug <- if (debug > 4) 4 else max(0, floor(debug + 0.5))
    if (debug > 0) {
        cat(paste(rep("  ", 5 - debug), collapse=""), ...)
        ##cat(paste(rep("  ", debug), collapse=""), ...)
    }
    flush.console()
    invisible()
}

showMetadataItem <- function(object, name, label="", postlabel="", isdate=FALSE, quote=FALSE)
{
    if (name %in% names(object@metadata)) {
        item <- object@metadata[[name]]
        if (isdate) item <- format(item)
        if (quote) item <- paste('"', item, '"', sep="")
        cat(paste("* ", label, item, postlabel, "\n", sep=""))
    }
}

integrateTrapezoid <- function(x, y, cumulative=FALSE)
{
    .Call("trap", x, y, cumulative)
}

