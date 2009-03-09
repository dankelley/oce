plot.tdr <- function (x, which=1:4, ...)
{
    if (!inherits(x, "tdr")) stop("method is only for tdr objects")
    show <- rep(FALSE, 4)
    show[which] <- TRUE
    oldpar <- par(no.readonly = TRUE)
    lw <- length(which)
    if (!"mgp" %in% names(list(...))) par(mgp = c(2, 2/3, 0))
    if (lw == 1) {
        ;
    } else if (lw == 2) {
        par(mar=c(3,3,1,1))
        par(mfcol=c(2,1))
    } else if (lw==3 || lw==4) {
        par(mar=c(3,3,1,1))
        par(mfcol=c(2,2))
    }
    if (show[1]) {
        plot(x$data$t, x$data$temperature,
             xlab="", ylab=expression(paste("Temperature [ ", degree, "C ]")), type='l', ...)
    }
    if (show[2]) {
        plot(x$data$t, x$data$pressure,
             xlab="", ylab="Pressure [dbar]", type='l',
             ylim=rev(range(x$data$pressure, na.rm=TRUE)),
             ...)
    }
    if (show[4]) {
        text.item <- function(item, cex=1.25) {
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
        text.item(paste("Serial Number: ", x$metadata$serial.number),cex=1.25)
        text.item(paste("Start:", x$data$t[1]), cex=1)
        text.item(paste("End:", x$data$t[length(x$data$t)]), cex=1)
        text.item(paste("Sampling interval:", difftime(x$data$t[2], x$data$t[1], units="s"), "s"),cex=1)
        par(mar=mar)
    }
    if (show[3]) {
        args <- list(x=x$data$temperature, y=x$data$pressure,
                     ylim=rev(range(x$data$pressure, na.rm=TRUE)),
                     xlab=expression(paste("Temperature [", degree, "C ]")),
                     ylab="Pressure [dbar]", ...)
        if (!("type" %in% names(list(...)))) args <- c(args, type="p")
        if (!("cex"  %in% names(list(...)))) args <- c(args, cex=0.5)
        do.call(plot, args)
    }
    if (lw != 1) par(oldpar)
    invisible()
}

read.tdr <- function(file, tz=getOption("oce.tz"), log.action, debug=FALSE)
{
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
    ## columns t, Temperature, p

##    header <- scan(file, what='char', sep="\n", n=19, quiet=TRUE)
    header <- c()
    logging.start <- sample.period <- NULL
    while (TRUE) {
        line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
        if (0 < (r<-regexpr("Temp[ \t]*Pres", line))) break
        header <- c(header, line)
        if (0 < (r<-regexpr("Logging[ \t]*start", line))) {
            l <- sub("[ ]*Logging[ \t]*start[ ]*", "", line)
            logging.start <- strptime(l,"%y/%m/%d %H:%M:%S")
        }
        if (0 < (r<-regexpr("Sample[ \t]*period", line))) {
            l <- sub("[ ]*Sample[ \t]*period[ ]*", "", line)
            sp <- as.numeric(strsplit(l, ":")[[1]])
            sample.period <- (sp[3] + 60*(sp[2] + 60*sp[1]))
        }
    }
    serial.number <- strsplit(header[1],"[\t ]+")[[1]][4]
    if (debug) {
        cat("logging.start:");print(logging.start)
        cat("sample.period:");print(sample.period)
    }

    col.names <- strsplit(gsub("[ ]+"," ", gsub("[ ]*$","",gsub("^[ ]+","",line))), " ")[[1]]

    ## Read a line to determine if there is a pair of columns for time
    line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
    pushBack(line, file)
    line <- gsub("[ ]+$", "", gsub("^[ ]+","", line))
    nvar <- length(strsplit(line, "[ ]+")[[1]])

    if (debug) cat("Data line '", line, "' reveals ", nvar, " data per line\n", sep="")

    d <- scan(file, character(), quiet=TRUE)
    n <- length(d) / nvar
    if (nvar == 4) {
        if (debug) cat("4 elements per data line\n")
        if (missing(tz))
            t <- as.POSIXct(paste(d[seq(1,4*n,4)], d[seq(2,4*n,4)]))
        else
            t <- as.POSIXct(paste(d[seq(1,4*n,4)], d[seq(2,4*n,4)]), tz=tz)
        temperature <- as.numeric(d[seq(3,4*n,4)])
        pressure <- as.numeric(d[seq(4,4*n,4)])
    } else if (nvar == 2) {
        if (debug) cat("2 elements per data line\n")
        t <- logging.start + seq(1:n) * sample.period
        temperature <- as.numeric(d[seq(1,2*n,2)])
        pressure <- as.numeric(d[seq(2,2*n,2)])
    } else if (nvar == 5) {
        ## 2008/06/25 10:00:00   18.5260   10.2225    0.0917
        if (debug) cat("5 elements per data line\n")
        if (missing(tz))
            t <- as.POSIXct(paste(d[seq(1,5*n,5)], d[seq(2,5*n,5)]))
        else
            t <- as.POSIXct(paste(d[seq(1,5*n,5)], d[seq(2,5*n,5)]),tz=tz)
        temperature <- as.numeric(d[seq(3,5*n,5)])
        pressure <- as.numeric(d[seq(4,5*n,5)])
        ## ignore column 5
    } else stop("wrong number of variables.  Expect 2, 4, or 5, but got ", nvar)

    data <- data.frame(t=t, temperature=temperature, pressure=pressure)
    metadata <- list(header=header,
                     serial.number=serial.number,
                     logging.start=logging.start,
                     sample.period=sample.period)
    if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processing.log.item(log.action)
    rval <- list(data=data, metadata=metadata, processing.log=log.item)
    class(rval) <- c("tdr", "oce")
    rval
}


summary.tdr <- function(object, ...)
{
    if (!inherits(object, "tdr")) stop("method is only for tdr objects")
    time.range <- range(object$data$t, na.rm=TRUE)
    fives <- matrix(nrow=2, ncol=5)
    res <- list(serial.number=object$metadata$serial.number,
                samples=length(object$data$temperature),
                logging.start=object$metadata$logging.start,
                sample.period=object$metadata$sample.period,
                start.time=time.range[1],
                end.time=time.range[2],
                fives=fives,
                processing.log="?")
    fives[1,] <- fivenum(object$data$temperature, na.rm=TRUE)
    fives[2,] <- fivenum(object$data$pressure, na.rm=TRUE)
    colnames(fives) <- c("Min.", "1st Qu.", "Median", "3rd Qu.", "Max.")
    rownames(fives) <- c("Temperature", "Pressure")
    res$fives <- fives
    res$processing.log <- processing.log.summary(object)
    class(res) <- "summary.tdr"
    res
}

print.summary.tdr <- function(x, digits=max(6, getOption("digits") - 1), ...)
{
    cat("\nTDR record\n")
    cat("Instrument Serial No. ", x$serial.number,  "\n")
    cat("No. of samples:      ", x$samples,  "\n")
    cat(sprintf("Logging start: %s (as reported in header)\n", as.character(x$logging.start)))
    cat(sprintf("Sample period: %s (as reported in header)\n", as.character(x$sample.period)))
    cat(sprintf("Time range: %s to %s\n", as.character(x$start.time), as.character(x$end.time)))
    cat("Statistics of data:\n")
    print(x$fives, digits=digits)
    print(x$processing.log)
    cat("\n")
    invisible(x)
}

tdr.patm <- function(x, dp=0.5)
{
    if (inherits(x, "tdr")) p <- x$data$pressure else p <- x
    sap <- 10.1325                      # standard atm pressure
    if (length(p) < 1) return(rep(sap, 4))
    p <- p[(sap - dp) <= p & p <= (sap + dp)] # window near sap
    w <- exp(-2*((p - sap) / dp)^2)
    if (length(p) < 4)
        rep(sap, 4)
    else
        c(sap, median(p), mean(p), weighted.mean(p, w))
}

tdr.trim <- function(x, method="water", parameters=NULL, verbose=FALSE)
{
    if (!inherits(x, "tdr")) stop("method is only for tdr objects")
    res <- x
    n <- length(x$data$temperature)
    if (verbose) cat("tdr.trim() working on dataset with", n, "points\n")
    if (n < 2) {
        warning("too few data to tdr.trim()")
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
            keep[x$data$t < as.POSIXlt(parameters[1])] <- FALSE
            keep[x$data$t > as.POSIXlt(parameters[2])] <- FALSE
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
    res <- processing.log.append(res, paste(deparse(match.call()), sep="", collapse=""))
    res
}
