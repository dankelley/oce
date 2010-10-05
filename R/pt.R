plot.pt <- function (x, which=1:4, title=deparse(substitute(x)), adorn=NULL,
                     tlim, plim, Tlim,
                     xlab, ylab,
                     draw.time.range=getOption("oce.draw.time.range"),
                     abbreviate.time.range=getOption("oce.abbreviate.time.range"),
                     small=2000,
                     mgp=getOption("oce.mgp"),
                     mar=c(mgp[1], mgp[1]+1, 1, 1.5),
                     debug=getOption("oce.debug"),
                     ...)
{
    if (!inherits(x, "pt")) stop("method is only for pt objects")
    lw <- length(which)
    opar <- par(no.readonly = TRUE)
    if (lw > 1)
        on.exit(par(opar))
    adorn.length <- length(adorn)
    if (adorn.length == 1) {
        adorn <- rep(adorn, lw)
        adorn.length <- lw
    }
    if (lw == 2) {
        layout(cbind(c(1,2)))
    } else if (lw==3 || lw==4) {
        layout(rbind(c(1,2),
                     c(3,4)), widths=c(2,1))
    }
    par(mgp=mgp, mar=mar)

    ## decode string values of 'which'
    which2 <- vector("numeric", lw)
    for (w in 1:lw) {
        ww <- which[w]
        if (is.numeric(ww)) {
            which2[w] <- ww
        } else {
            if (     ww == "temperature") which2[w] <- 1
            else if (ww == "text") which2[w] <- 2
            else if (ww == "pressure") which2[w] <- 3
            else if (ww == "profile") which2[w] <- 4
            else stop("unknown 'which':", ww)
        }
    }
    which <- which2
    oce.debug(debug, "after nickname-substitution, which=c(", paste(which, collapse=","), ")\n")



    for (w in 1:lw) {
        if (which[w] == 1) {
            plot(x$data$ts$time, x$data$ts$temperature,
                 xlab=if (missing(xlab)) "" else xlab,
                 ylab=if (missing(ylab)) resizable.label("T", "y") else ylab,
                 xaxs="i", type='l',
                 xlim=if (missing(tlim)) range(x$data$ts$time, na.rm=TRUE) else tlim,
                 ylim=if (missing(Tlim)) range(x$data$ts$temperature, na.rm=TRUE) else Tlim,
                 axes=FALSE, ...)
            box()
            oce.axis.POSIXct(1, x=x$data$ts$time, draw.time.range=draw.time.range, abbreviate.time.range=abbreviate.time.range)
            draw.time.range <- FALSE    # only the first time panel gets the time indication
            axis(2)
        } else if (which[w] == 3) {     # pressure timeseries
            plot(x$data$ts$time, x$data$ts$pressure,
                 xlab=if (missing(xlab)) "" else xlab,
                 ylab=if (missing(ylab)) resizable.label("p", "y") else ylab,
                 xaxs="i", type='l',
                 xlim=if (missing(tlim)) range(x$data$ts$time, na.rm=TRUE) else tlim,
                 ylim=if (missing(plim)) range(x$data$ts$pressure, na.rm=TRUE) else plim,
                 axes=FALSE, ...)
            box()
            oce.axis.POSIXct(1, x=x$data$ts$time, draw.time.range=draw.time.range)
            draw.time.range <- FALSE
            axis(2)
        } else if (which[w] == 2) {
            text.item <- function(item, cex=4/5*par("cex")) {
                if (!is.null(item) && !is.na(item)) {
                    text(xloc, yloc, item, adj = c(0, 0), cex=cex);
                    yloc <<- yloc - d.yloc;
                }
            }
            xfake <- seq(0:10)
            yfake <- seq(0:10)
            mar <- par("mar")
            par(mar=c(0,0,0,0))

            plot(xfake, yfake, type = "n", xlab = "", ylab = "", axes = FALSE)
            xloc <- 1
            yloc <- 10
            d.yloc <- 0.7
            cex <- par("cex")
            text.item(title, cex=1.25*cex)
            if (!is.null(x$metadata$filename))
                text.item(x$metadata$filename, cex=cex)
            if (!is.null(x$metadata$serial.number))
                text.item(paste("Serial Number: ", x$metadata$serial.number),cex=cex)
            if (!(1 %in% which || 2 %in% which)) { # don't bother with these if already on a time-series panel
                text.item(paste("Start:", x$data$ts$time[1], attr(x$data$ts$time, "tzone")), cex=cex)
                text.item(paste("End:", x$data$ts$time[length(x$data$ts$time)], attr(x$data$ts$time, "tzone")), cex=cex)
                text.item(paste("Sampled interval:", difftime(x$data$ts$time[2], x$data$ts$time[1], units="secs"), "s"),cex=cex)
            }
            par(mar=mar)
        } else if (which[w] == 4) {     # temperature 'profile'
            args <- list(x=x$data$ts$temperature, y=x$data$ts$pressure,
                         xlab=resizable.label("T"),
                         ylab=resizable.label("p"),
                         xlim=if (missing(Tlim)) range(x$data$ts$temperature, na.rm=TRUE) else Tlim,
                         ylim=if (missing(plim)) rev(range(x$data$ts$pressure, na.rm=TRUE)) else plim,
                         ...)
            if (!("type" %in% names(list(...)))) args <- c(args, type="p")
            if (!("cex"  %in% names(list(...)))) args <- c(args, cex=3/4)
            np <- length(x$data$ts$pressure)
            if (np <= small)
                do.call(plot, args)
            else {
                args <- args[names(args) != "type"]
                do.call(smoothScatter, args)
            }
        }
        if (w <= adorn.length) {
            t <- try(eval(adorn[w]), silent=TRUE)
            if (class(t) == "try-error") warning("cannot evaluate adorn[", w, "]\n")
        }
    }
    invisible()
}

read.pt <- function(file,from=1,to,by=1,tz=getOption("oce.tz"),log.action,debug=getOption("oce.debug"))
{
    if (!missing(to))
        oce.debug(debug, 'to=', to, '\n')
    file <- full.filename(file)
    filename <- file
    if (is.character(file)) {
        file <- file(file, "r")
        on.exit(close(file))
    }
    if (!inherits(file, "connection"))
        stop("'file' must be a character string or connection")
    if (!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }
    oce.debug(debug, "from=", from, "\n")
    from.keep <- from
    measurement.deltat <- 0
    if (is.numeric(from) && from < 1)
        stop("from cannot be an integer less than 1")
    ##from.keep <- from
    if (!missing(to))
        to.keep <- to
    by.keep <- by
    host.time <- 0
    logger.time <- 0
    subsample.start <- 0
    subsample.end <- 0
    subsample.period <- 0
    number.channels <- 0
    ## Q: what ends the header? a blank line?  Line 21?
    ## calibration 1
    ## calibration 2
    ## correction.to.conductivity
    ## memory type
    ## Timestamp
    ## columns time, Temperature, p
    ##header <- scan(file, what='char', sep="\n", n=19, quiet=TRUE)
    header <- c()
    measurement.start <-measurement.end <- measurement.deltat <- NULL
    while (TRUE) {
        line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
        if (0 < (r<-regexpr("Temp[ \t]*Pres", line))) break
        header <- c(header, line)
        if (0 < (r<-regexpr("Logging[ \t]*start", line))) {
            l <- sub("[ ]*Logging[ \t]*start[ ]*", "", line)
            measurement.start <- as.POSIXct(strptime(l,"%y/%m/%d %H:%M:%S", tz=tz))
        }
        ## "Logging end" would seem to be the sensible thing to examine,
        ## but "Logger time" seems correct in SLEIWEX 2008 data.  I think
        ## the issue is that the devices were turned off manually, and
        ## that time (the relevant one) is in "Logger time".
        ##OLD if (0 < (r<-regexpr("Logging[ \t]*end", line))) {
        ##OLD    l <- sub("[ ]*Logging[ \t]*end[ ]*", "", line)
        ##OLD    measurement.end <- as.POSIXct(strptime(l,"%y/%m/%d %H:%M:%S", tz=tz))
        ##OLD }
        if (0 < (r<-regexpr("Logger[ \t]*time", line))) {
            l <- sub("[ ]*Logger[ \t]*time[ ]*", "", line)
            measurement.end <- as.POSIXct(strptime(l,"%y/%m/%d %H:%M:%S", tz=tz))
        }
        if (0 < (r<-regexpr("Sample[ \t]*period", line))) {
            l <- sub("[ ]*Sample[ \t]*period[ ]*", "", line)
            sp <- as.numeric(strsplit(l, ":")[[1]])
            measurement.deltat <- (sp[3] + 60*(sp[2] + 60*sp[1]))
        }
    }
    oce.debug(debug, "measurement.start =", format(measurement.start), "\n")
    oce.debug(debug, "measurement.end =", format(measurement.end), "\n")
    oce.debug(debug, "measurement.deltat  =", measurement.deltat, "\n")
    serial.number <- strsplit(header[1],"[\t ]+")[[1]][4]
    oce.debug(debug, "serial.number=", serial.number,"\n")
    ## Now that we know the logging times, we can work with 'from 'and 'to'
    if (inherits(from, "POSIXt") || inherits(from, "character")) {
        if (!inherits(to, "POSIXt") && !inherits(to, "character")) stop("if 'from' is POSIXt or character, then 'to' must be, also")
        from <- as.numeric(difftime(as.POSIXct(from, tz=tz), measurement.start, units="secs")) / measurement.deltat
        oce.debug(debug, "inferred from =", format(from, width=7), " based on 'from' arg", from.keep, "\n")
        to <- as.numeric(difftime(as.POSIXct(to, tz=tz), measurement.start, units="secs")) / measurement.deltat
        oce.debug(debug, "inferred   to =",   format(to, width=7), " based on   'to' arg", to.keep, "\n")
    }
    oce.debug(debug, "by=", by, "in argument list\n")
    by <- ctime.to.seconds(by)
    oce.debug(debug, "inferred by=", by, "s\n")

    col.names <- strsplit(gsub("[ ]+"," ", gsub("[ ]*$","",gsub("^[ ]+","",line))), " ")[[1]]

    ## Read a line to determine if there is a pair of columns for time
    line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
    pushBack(line, file)
    line <- gsub("[ ]+$", "", gsub("^[ ]+","", line))
    nvar <- length(strsplit(line, "[ ]+")[[1]])

    oce.debug(debug, " data line '", line, "' reveals ", nvar, " data per line\n", sep="")
    if (missing(to)) {
        oce.debug(debug, "reading whole file, since 'to' was not provided\n")
        d <- scan(file, character(), skip=from-1, quiet=TRUE) # whole file
    } else {
        oce.debug(debug, "skipping (from-1)=", from-1, "lines, then reading (to-from+1)=", to-from+1, "lines\n")
        d <- scan(file, character(), skip=from-1, quiet=TRUE, nlines=(to - from + 1))
    }
    oce.debug(debug, "got length(d)=", length(d), "during the scan()\n")
    ## FIXME: it is slow to read whole file and then subset ... would multiple calls to scan() be faster?
    n <- length(d) / nvar
    dim(d) <- c(nvar, n)

    ## subset
    look <- seq(from=1, to=n, by=by)    # BUG: why not using proper 'from'?
    d <- d[,look]
    n <- dim(d)[2]
    subsample.start <- measurement.start + (from - 1) * measurement.deltat # FIXME: check this
    subsample.deltat <- by * measurement.deltat
    if (nvar == 2) {
        time <- subsample.start + seq(from=1, to=n) * subsample.deltat
        oce.debug(debug, "nvar=2; setting time[1:2]=", time[1:2], "with subsample.deltat=", subsample.deltat,"\n")
        temperature <- as.numeric(d[1,])
        pressure <- as.numeric(d[2,])
    } else if (nvar == 4) {
        oce.debug(debug, "nvar=4; decoding data\n")
        time <- as.POSIXct(paste(d[1,], d[2,]), tz=tz)
        temperature <- as.numeric(d[3,])
        pressure <- as.numeric(d[4,])
    } else if (nvar == 5) {
        oce.debug(debug, "nvar=5; decoding data\n")
        ## 2008/06/25 10:00:00   18.5260   10.2225    0.0917
        time <- as.POSIXct(paste(d[1,], d[2,]),tz=tz)
        temperature <- as.numeric(d[3,])
        pressure <- as.numeric(d[4,])
        ## ignore column 5
    } else stop("wrong number of variables; need 2, 4, or 5, but got ", nvar)
    data <- list(ts=list(time=time, temperature=temperature, pressure=pressure))
    metadata <- list(filename=filename,
                     instrument.type="rbr",
                     serial.number=serial.number,
                     measurement.start=measurement.start,
                     measurement.end=measurement.end,
                     measurement.deltat=measurement.deltat,
                     subsample.start=time[1],
                     subsample.end=time[length(time)],
                     subsample.deltat=as.numeric(time[2])-as.numeric(time[1]))
    if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processing.log.item(log.action)
    rval <- list(data=data, metadata=metadata, processing.log=log.item)
    class(rval) <- c("pt", "oce")
    rval
}


summary.pt <- function(object, ...)
{
    if (!inherits(object, "pt")) stop("method is only for pt objects")
    time.range <- range(object$data$ts$time, na.rm=TRUE)
    fives <- matrix(nrow=2, ncol=5)
    fives[1,] <- fivenum(object$data$ts$temperature, na.rm=TRUE)
    fives[2,] <- fivenum(object$data$ts$pressure, na.rm=TRUE)
    colnames(fives) <- c("Min.", "1st Qu.", "Median", "3rd Qu.", "Max.")
    rownames(fives) <- c("Temperature", "Pressure")
    res <- list(filename=object$metadata$filename,
                serial.number=object$metadata$serial.number,
                measurement.start=object$metadata$measurement.start,
                measurement.end=object$metadata$measurement.end,
                measurement.deltat=object$metadata$measurement.deltat,
                subsample.start=object$metadata$subsample.start,
                subsample.end=object$metadata$subsample.end,
                subsample.deltat=object$metadata$subsample.deltat,
                samples=length(object$data$ts$temperature), # FIXME: do we need this?
                fives=fives,
                processing.log=processing.log.summary(object))
    class(res) <- "summary.pt"
    res
}

print.summary.pt <- function(x, digits=max(6, getOption("digits") - 1), ...)
{
    cat("PT Summary\n----------\n", ...)
    cat(paste("* Instrument:         RBR, serial number ``", x$serial.number, "``\n", sep=""), ...)
    cat(paste("* Source:             ``", x$filename, "``\n", sep=""), ...)
    cat(sprintf("* Measurements:       %s %s to %s %s sampled at %.4g Hz\n",
                format(x$measurement.start), attr(x$measurement.start, "tzone"),
                format(x$measurement.end), attr(x$measurement.end, "tzone"),
                1 / x$measurement.deltat), ...)
    cat(sprintf("* Subsample:          %s %s to %s %s sampled at %.4g Hz\n",
                format(x$subsample.start), attr(x$subsample.start, "tzone"),
                format(x$subsample.end),  attr(x$subsample.end, "tzone"),
                1 / x$subsample.deltat), ...)
    cat("* Statistics of subsample::\n\n", ...)
    cat(show.fives(x, indent='     '), ...)
    ##cat("\n* Processing log::\n\n", ...)
    cat("\n")
    print(x$processing.log, ...)
    invisible(x)
}

pt.patm <- function(x, dp=0.5)
{
    if (inherits(x, "pt")) p <- x$data$ts$pressure else p <- x
    sap <- 10.1325                      # standard atm pressure
    if (length(p) < 1) return(rep(sap, 4))
    p <- p[(sap - dp) <= p & p <= (sap + dp)] # window near sap
    w <- exp(-2*((p - sap) / dp)^2)
    if (length(p) < 4)
        rep(sap, 4)
    else
        c(sap, median(p), mean(p), weighted.mean(p, w))
}

pt.trim <- function(x, method="water", parameters=NULL, debug=getOption("oce.debug"))
{
    if (!inherits(x, "pt")) stop("method is only for pt objects")
    res <- x
    n <- length(x$data$ts$temperature)
    oce.debug(debug, "pt.trim() working on dataset with", n, "points\n")
    if (n < 2) {
        warning("too few data to pt.trim()")
    } else {
        which.method <- pmatch(method, c("water", "time", "index"), nomatch=0)
        oce.debug(debug, "using method", which.method, "\n")
        if (which.method == 1) {        # "water"
            keep <- rep(FALSE, n)
            air <- x$data$ts$pressure < 10.5 # NB. standard pressure is 10.1325
            water.indices <- which(!air)
            b <- 2                      # trim a few descending points
            i.start <- water.indices[1] + b
            i.stop <- water.indices[-b + length(water.indices)]
            keep[i.start:i.stop] <- TRUE
        } else if (which.method == 2) { # "time"
            oce.debug(debug, "trimming to time range ",as.character(parameters[1])," to ", as.character(parameters[2]), "\n")
            keep <- rep(TRUE, n)
            keep[x$data$ts$time < as.POSIXlt(parameters[1])] <- FALSE
            keep[x$data$ts$time > as.POSIXlt(parameters[2])] <- FALSE
        } else if (which.method == 3) { # "index"
            oce.debug(debug, "parameters:",parameters,"\n")
            if (min(parameters) < 1)
                stop("Cannot select indices < 1");
            if (max(parameters) > n)
                stop(paste("Cannot select past end of array, i.e. past ", n))
            keep <- rep(FALSE, n)
            keep[parameters[1]:parameters[2]] <- TRUE
        } else {
            stop("Unknown method")
        }
    }
    for (name in names(x$data$ts))
        res$data$ts[[name]] <- subset(x$data$ts[[name]], keep)
    res$data$ts$pressure <- res$data$ts$pressure - 10.1325 # remove avg sealevel pressure
    res$processing.log <- processing.log.add(res$processing.log,
                                             paste(deparse(match.call()), sep="", collapse=""))
    res
}
