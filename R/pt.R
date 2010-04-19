plot.pt <- function (x, which=1:4, title=deparse(substitute(x)), adorn=NULL,
                     tlim, plim, Tlim,
                     xlab, ylab,
                     draw.time.range=getOption("oce.draw.time.range"),
                     mgp=getOption("oce.mgp"),
                     mar=c(mgp[1], mgp[1]+1, 1, 1.5),
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
    for (w in 1:lw) {
        if (which[w] == 1) {
            plot(x$data$time, x$data$temperature,
                 xlab=if (missing(xlab)) "" else xlab,
                 ylab=if (missing(ylab)) resizable.label("T", "y") else ylab,
                 xaxs="i", type='l',
                 xlim=if (missing(tlim)) range(x$data$time, na.rm=TRUE) else tlim,
                 ylim=if (missing(Tlim)) range(x$data$temperature, na.rm=TRUE) else Tlim,
                 axes=FALSE, ...)
            box()
            oce.axis.POSIXct(1, x=x$data$time, draw.time.range=draw.time.range)
            draw.time.range <- FALSE
            axis(2)
        } else if (which[w] == 3) {     # pressure timeseries
            plot(x$data$time, x$data$pressure,
                 xlab=if (missing(xlab)) "" else xlab,
                 ylab=if (missing(ylab)) resizable.label("p", "y") else ylab,
                 xaxs="i", type='l',
                 xlim=if (missing(tlim)) range(x$data$time, na.rm=TRUE) else tlim,
                 ylim=if (missing(plim)) range(x$data$pressure, na.rm=TRUE) else plim,
                 axes=FALSE, ...)
            box()
            oce.axis.POSIXct(1, x=x$data$time, draw.time.range=draw.time.range)
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
                text.item(paste("Start:", x$data$time[1], attr(x$data$time, "tzone")), cex=cex)
                text.item(paste("End:", x$data$time[length(x$data$time)], attr(x$data$time, "tzone")), cex=cex)
                text.item(paste("Sampled interval:", difftime(x$data$time[2], x$data$time[1], units="secs"), "s"),cex=cex)
            }
            par(mar=mar)
        } else if (which[w] == 4) {     # temperature 'profile'
            args <- list(x=x$data$temperature, y=x$data$pressure,
                         xlab=resizable.label("T"),
                         ylab=resizable.label("p"),
                         xlim=if (missing(Tlim)) range(x$data$temperature, na.rm=TRUE) else Tlim,
                         ylim=if (missing(plim)) rev(range(x$data$pressure, na.rm=TRUE)) else plim,
                         ...)
            if (!("type" %in% names(list(...)))) args <- c(args, type="p")
            if (!("cex"  %in% names(list(...)))) args <- c(args, cex=3/4)
            do.call(plot, args)
        }
        if (w <= adorn.length) {
            t <- try(eval(adorn[w]), silent=TRUE)
            if (class(t) == "try-error") warning("cannot evaluate adorn[", w, "]\n")
        }
    }
    invisible()
}

read.pt <- function(file,from="start",to="end",by=1,tz=getOption("oce.tz"),log.action,debug=getOption("oce.debug"))
{
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
    if (is.numeric(from) && from < 0)
        stop("from cannot be an integer less than 1")
    if (is.character(from) && from == "start")
        from <- 1
    ##from.keep <- from
    to.keep <- to
    by.keep <- by
    host.time <- 0
    logger.time <- 0
    logging.start <- 0
    logging.end <- 0
    sample.period <- 0
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
    logging.start <- sample.period <- NULL
    while (TRUE) {
        line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
        if (0 < (r<-regexpr("Temp[ \t]*Pres", line))) break
        header <- c(header, line)
        if (0 < (r<-regexpr("Logging[ \t]*start", line))) {
            l <- sub("[ ]*Logging[ \t]*start[ ]*", "", line)
            logging.start <- as.POSIXct(strptime(l,"%y/%m/%d %H:%M:%S", tz=tz))
        }
        if (0 < (r<-regexpr("Logging[ \t]*end", line))) {
            l <- sub("[ ]*Logging[ \t]*end[ ]*", "", line)
            logging.end <- as.POSIXct(strptime(l,"%y/%m/%d %H:%M:%S", tz=tz))
        }
        if (0 < (r<-regexpr("Sample[ \t]*period", line))) {
            l <- sub("[ ]*Sample[ \t]*period[ ]*", "", line)
            sp <- as.numeric(strsplit(l, ":")[[1]])
            file.delta.t <- (sp[3] + 60*(sp[2] + 60*sp[1]))
        }
    }
    serial.number <- strsplit(header[1],"[\t ]+")[[1]][4]
    oce.debug(debug, "logging.start =", format(logging.start), "\n")
    oce.debug(debug, "file.delta.t  =", file.delta.t, "\n")
    ## Now that we know the logging times, we can work with 'from 'and 'to'
    if (inherits(from, "POSIXt") || inherits(from, "character")) {
        from <- as.numeric(difftime(as.POSIXct(from, tz=tz), logging.start, units="secs")) / file.delta.t
        oce.debug(debug, "inferred from =", format(from, width=7), " based on 'from' arg", from.keep, "\n")
    }
    if (!missing(to)) {
        if (inherits(to, "POSIXt") || length(grep(":", to))) {
            to <- as.numeric(difftime(as.POSIXct(to, tz=tz), logging.start, units="secs")) / file.delta.t
            oce.debug(debug, "inferred   to =",   format(to, width=7), " based on   'to' arg", to.keep, "\n")
        }
    }
    if (!missing(by)) {
        by <- ctime.to.seconds(by)
    }
    oce.debug(debug, "by inferred to be", by, "s\n")

    ## Handle time-based args 'from', 'to', and 'by'.
    col.names <- strsplit(gsub("[ ]+"," ", gsub("[ ]*$","",gsub("^[ ]+","",line))), " ")[[1]]

    ## Read a line to determine if there is a pair of columns for time
    line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
    pushBack(line, file)
    line <- gsub("[ ]+$", "", gsub("^[ ]+","", line))
    nvar <- length(strsplit(line, "[ ]+")[[1]])

    oce.debug(debug, "Data line '", line, "' reveals ", nvar, " data per line\n", sep="")
    if (missing(to) || to == "end")
        d <- scan(file, character(), skip=from-1, quiet=TRUE) # whole file
    else
        d <- scan(file, character(), skip=from-1, quiet=TRUE, nlines=(to - from))
    ## FIXME: it is slow to read whole file and then subset ... would multiple calls to scan() be faster?
    n <- length(d) / nvar
    dim(d) <- c(nvar, n)
    if (by != 1)  {
        look <- seq(from=1, to=n, by=by)
        d <- d[,look]
        n <- dim(d)[2]
    }
    if (nvar == 2) {
        oce.debug(debug, "2 elements per data line\n")
        time <- logging.start + seq(from=1, to=n) * by * file.delta.t
        temperature <- as.numeric(d[1,])
        pressure <- as.numeric(d[2,])
    } else if (nvar == 4) {
        oce.debug(debug, "4 elements per data line\n")
        time <- as.POSIXct(paste(d[1,], d[2,]), tz=tz)
        temperature <- as.numeric(d[3,])
        pressure <- as.numeric(d[4,])
    } else if (nvar == 5) {
        ## 2008/06/25 10:00:00   18.5260   10.2225    0.0917
        oce.debug(debug, "5 elements per data line\n")
        time <- as.POSIXct(paste(d[1,], d[2,]),tz=tz)
        temperature <- as.numeric(d[3,])
        pressure <- as.numeric(d[4,])
        ## ignore column 5
    } else stop("wrong number of variables; need 2, 4, or 5, but got ", nvar)
    data <- data.frame(time=time, temperature=temperature, pressure=pressure)
    metadata <- list(header=header,
                     filename=filename,
                     serial.number=serial.number,
                     logging.start=logging.start,
                     logging.end=logging.end,
                     file.delta.t=file.delta.t,
                     subsample.delta.t=as.numeric(difftime(time[2], time[1], units="secs")))
    if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processing.log.item(log.action)
    rval <- list(data=data, metadata=metadata, processing.log=log.item)
    class(rval) <- c("pt", "oce")
    rval
}


summary.pt <- function(object, ...)
{
    if (!inherits(object, "pt")) stop("method is only for pt objects")
    time.range <- range(object$data$time, na.rm=TRUE)
    fives <- matrix(nrow=2, ncol=5)
    res <- list(filename=object$metadata$filename,
                serial.number=object$metadata$serial.number,
                samples=length(object$data$temperature),
                logging.start=object$metadata$logging.start,
                logging.end=object$metadata$logging.end,
                file.delta.t=object$metadata$file.delta.t,
                subsample.delta.t=object$metadata$subsample.delta.t,
                start.time=time.range[1],
                end.time=time.range[2],
                fives=fives,
                processing.log=processing.log.summary(object))
    fives[1,] <- fivenum(object$data$temperature, na.rm=TRUE, ...)
    fives[2,] <- fivenum(object$data$pressure, na.rm=TRUE)
    colnames(fives) <- c("Min.", "1st Qu.", "Median", "3rd Qu.", "Max.")
    rownames(fives) <- c("Temperature", "Pressure")
    res$fives <- fives
    class(res) <- "summary.pt"
    res
}

print.summary.pt <- function(x, digits=max(6, getOption("digits") - 1), ...)
{
    cat("PT Summary\n", ...)
    cat("  Instrument:   RBR serial number", x$serial.number, "\n", ...)
    cat("  Source:      ", x$filename, "\n", ...)
    cat(sprintf("  Measurements: %s  to  %s  by  %s\n", as.character(x$logging.start), format(x$logging.end), seconds.to.ctime(x$file.delta.t)), ...)
    cat(sprintf("  Subsamples:   %s  to  %s  by  %s\n", as.character(x$start.time), format(x$end.time), seconds.to.ctime(x$subsample.delta.t)), ...)
    cat("\nStatistics:\n", ...)
    cat(show.fives(x, indent='  '), ...)
    cat("\n", ...)
    cat("Processing Log:\n", ...)
    cat(x$processing.log, ...)
    invisible(x)
}

pt.patm <- function(x, dp=0.5)
{
    if (inherits(x, "pt")) p <- x$data$pressure else p <- x
    sap <- 10.1325                      # standard atm pressure
    if (length(p) < 1) return(rep(sap, 4))
    p <- p[(sap - dp) <= p & p <= (sap + dp)] # window near sap
    w <- exp(-2*((p - sap) / dp)^2)
    if (length(p) < 4)
        rep(sap, 4)
    else
        c(sap, median(p), mean(p), weighted.mean(p, w))
}

pt.trim <- function(x, method="water", parameters=NULL, verbose=FALSE)
{
    if (!inherits(x, "pt")) stop("method is only for pt objects")
    res <- x
    n <- length(x$data$temperature)
    if (verbose) cat("pt.trim() working on dataset with", n, "points\n")
    if (n < 2) {
        warning("too few data to pt.trim()")
    } else {
        which.method <- pmatch(method, c("water", "time", "index"), nomatch=0)
        if (verbose) cat("using method", which.method,"\n")
        if (which.method == 1) {        # "water"
            keep <- rep(FALSE, n)
            air <- x$data$pressure < 10.5 # NB. standard pressure is 10.1325
            water.indices <- which(!air)
            b <- 2                      # trim a few descending points
            i.start <- water.indices[1] + b
            i.stop <- water.indices[-b + length(water.indices)]
            keep[i.start:i.stop] <- TRUE
            #message("The mean (deleted) air pressure is", mean(x$data$pressure[air]),"dbar\n")
        } else if (which.method == 2) { # "time"
            if (verbose) cat("trimming to time range ",as.character(parameters[1])," to ", as.character(parameters[2]), "\n");
            keep <- rep(TRUE, n)
            keep[x$data$time < as.POSIXlt(parameters[1])] <- FALSE
            keep[x$data$time > as.POSIXlt(parameters[2])] <- FALSE
        } else if (which.method == 3) { # "index"
            if (verbose)	cat("parameters:",parameters,"\n");
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
    res$data <- subset(x$data, keep)
    res$data$pressure <- res$data$pressure - 10.1325 # remove avg sealevel pressure
    res <- processing.log.append(res, paste(deparse(match.call()), sep="", collapse=""))
    res
}
