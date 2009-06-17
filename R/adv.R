read.adv <- function(file, from=0, to, by=1,
                     type=c("sontek"),
                     withHeader=FALSE, sampling.start, deltat,
                     debug=0, monitor=TRUE, log.action)
{
    type = match.arg(type)
    if (type == "sontek")
        read.adv.sontek(file=file, from=from, to=to, by=by,
                        withHeader=withHeader, sampling.start=sampling.start, deltat=deltat,
                        debug=debug, monitor=monitor, log.action=log.action)
}

read.adv.sontek <- function(file, from=0, to, by=1,
                            type="default",
                            withHeader=FALSE, sampling.start, deltat,
                            debug=0, monitor=TRUE, log.action)
{
    if (is.character(file)) {
        filename <- file
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
    if (withHeader) {
        stop("cannot read with header yet")
    } else {
        if (missing(sampling.start)) stop("must give 'sampling.start' if withHeader is FALSE")
        if (missing(deltat)) stop("must give 'deltat' if withHeader is FALSE")
        seek(file, 0, "end")
        file.size <- seek(file, 0, "start")
        if (debug) cat("file", filename, "has", file.size, "bytes\n")
        buf <- readBin(file, "raw", n=file.size, endian="little")
        flag1 <- as.raw(0x85)           # id
        flag2 <- as.raw(0x16)           # number of bytes (22 in decimal)
        match.flag1 <- which(buf==flag1)
        sample.start <- match.flag1[buf[match.flag1 + 1] == flag2]
        sample.start <- sample.start[1:(-1 + length(sample.start))] # last may be partial
        if (buf[sample.start[1] + as.integer(flag2)] != flag1) stop("problem reading first sample")
        ## FIXME: should run along the data for a while, to confirm that it's ok
    }
    n <- length(sample.start)
    ## id <- buf[sample.start]
    ## number.of.bytes <- buf[sample.start + 1]
    sample.start2 <- sort(c(sample.start, sample.start+1)) # use this to subset for 2-byte reads
    ##print((sample.start2 + 2)[1:10])
    sample.number <- readBin(buf[sample.start2 + 2], "integer", signed=FALSE, endian="little", size=2, n=n)
##?##    temperature <- readBin(buf[sample.start2 + 4], "integer", signed=TRUE, endian="little", size=2, n=n) / 100.0
    ## in next, divide by 100 to get to cm/s, then by 100 to get to m/s
    x <- readBin(buf[sample.start2 +  6], "integer", signed=TRUE, endian="little", size=2, n=n) / 10000.0
    y <- readBin(buf[sample.start2 +  8], "integer", signed=TRUE, endian="little", size=2, n=n) / 10000.0
    z <- readBin(buf[sample.start2 + 10], "integer", signed=TRUE, endian="little", size=2, n=n) / 10000.0
    a1 <- as.numeric(buf[sample.start + 12])
    a2 <- as.numeric(buf[sample.start + 13])
    a3 <- as.numeric(buf[sample.start + 14])
    c1 <- as.numeric(buf[sample.start + 15])
    c2 <- as.numeric(buf[sample.start + 16])
    c3 <- as.numeric(buf[sample.start + 17])

    ##print(buf[sample.start2 + 18][1:10])

    temperature <- readBin(buf[sample.start2 + 18], "integer", signed=TRUE, endian="little", size=2, n=n) / 100.0
    pressure <- readBin(buf[sample.start2 + 20], "integer", signed=FALSE, endian="little", size=2, n=n) / 1000 # mbar?
    ## 21 and 22 are checksum

    ##class(time) <- c("POSIXt", "POSIXct")
    ##attr(time, "tzone") <- attr(p$header$time, "tzone")
    ##data <- list(time=time, u=u, v=v, w=w)
    data <- list(#buf=buf,
                 time=sample.number,
                 x=x, y=y, z=z,
                 a1=a1, a2=a2, a3=a3,
                 c1=c1, c2=c2, c3=c3,
                 temperature=temperature,
                 pressure=pressure)
    metadata <- list(filename=filename,
                     instrument.type="sontek",
                     number.of.samples=length(x),
                     sampling.start=sampling.start,
                     deltat=deltat)
    if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processing.log.item(log.action)
    res <- list(data=data, metadata=metadata, processing.log=log.item)
    class(res) <- c("adv", "sontek", "oce")
    res
}

summary.adv <- function(object, ...)
{
    if (!inherits(object, "adv")) stop("method is only for adv objects")
    if (inherits(object, "sontek")) {
        ;
    } else stop("can only summarize ADV objects of type \"sontek\" not class ", paste(class(object),collapse=","))
    names <- names(object$data)
    fives <- matrix(nrow=(-1+length(names)), ncol=5)
    ii <- 1
    for (i in 1:length(names)) {
        if (names(object$data)[i] != "time") {
            fives[ii,] <- fivenum(object$data[[names[i]]], na.rm=TRUE)
            ii <- ii + 1
        }
    }
    rownames(fives) <- names[names != "time"]
    colnames(fives) <- c("Min.", "1st Qu.", "Median", "3rd Qu.", "Max.")
    res <- list(#res.specific,
                filename=object$metadata$filename,
                sampling.start=object$metadata$sampling.start,
                sampling.end=object$metadata$sampling.start + object$metadata$number.of.samples*object$metadata$deltat,
                deltat=object$metadata$deltat,
                instrument.type=object$metadata$instrument.type,
                number.of.samples=length(object$data$x),
                fives=fives,
                processing.log=processing.log.summary(object))
    class(res) <- "summary.adv"
    res
}                                       # summary.adv()

print.summary.adv <- function(x, digits=max(6, getOption("digits") - 1), ...)
{
    cat("ADV summary\n")
    cat("  Instrument type:            ", x$instrument.type, "\n")
    cat("  Filename:                   ", x$filename, "\n")
##    cat("  Instrument serial number:   ", x$metadata$serial.number, "\n")
##    cat("  Coordinate system:          ", x$coordinate.system, "[originally],", x$oce.coordinate, "[presently]\n")
    cat("  Measurements at times:      ", format(x$sampling.start), attr(x$sampling.end, "tzone"),
        "to",
        format(x$sampling.end), attr(x$sampling.end, "tzone"),
        "at interval", x$deltat, "s\n")
##    cat("  Orientation:                ", x$orientation, "\n")
##    cat("  Beam angle:                 ", x$metadata$beam.angle, "\n")
    cat("  Number of samples:          ", x$number.of.samples, "\n")

    if (x$instrument.type == "sontek") {
        cat("  SonTek-specific\n")
        cat("    -na-\n")
    }
    cat("\nStatistics:\n")
    print(x$fives)
    cat("\n")
    print(x$processing.log)
    invisible(x)
}

plot.adv <- function(x,
                     which=1:3,
                     smooth=TRUE,
                     titles,
                     adorn=NULL,
                     draw.time.range=getOption("oce.draw.time.range"),
                     mgp=getOption("oce.mgp"),
                     mar=c(mgp[1],mgp[1]+1,1,1+par("cex")),
                     ...)
{
    if (!inherits(x, "adv")) stop("method is only for adv objects")
    if (!all(which %in% 1:3)) stop("\"which\" must be in the range 1:3")
    opar <- par(no.readonly = TRUE)
    lw <- length(which)
    if (lw > 1) lay <- layout(cbind(1:lw))
    if (!missing(titles) && length(titles) != lw) stop("length of 'titles' must equal length of 'which'")
    if (lw > 1) on.exit(par(opar))
    par(mgp=mgp, mar=mar)
    dots <- list(...)
    gave.ylim <- "ylim" %in% names(dots)

    ylim.given <- if (gave.ylim) dots[["ylim"]] else NULL

    adorn.length <- length(adorn)
    if (adorn.length == 1) {
        adorn <- rep(adorn, lw)
        adorn.length <- lw
    }
    time <- seq(from=x$metadata$sampling.start, by=x$metadata$deltat, length.out=x$metadata$number.of.samples)
    attr(time, "tzone") <- attr(x$metadata$sampling.start, "tzone")

    for (w in 1:lw) {
        ##cat("which[w]=", which[w], "smooth=",smooth,"\n")
        if (which[w] == 1) oce.plot.ts(time,
                 if (smooth) as.numeric(smooth(x$data$x)) else x$data$x,
                 ylab="u [m/s]", type='l', draw.time.range=draw.time.range, ...)
        if (which[w] == 2) oce.plot.ts(time,
                 if (smooth) as.numeric(smooth(x$data$y)) else x$data$y,
                 ylab="v [m/s]", type='l', draw.time.range=draw.time.range, ...)
        if (which[w] == 3) oce.plot.ts(time,
                 if (smooth) as.numeric(smooth(x$data$z)) else x$data$z,
                 ylab="w [m/s]", type='l', draw.time.range=draw.time.range, ...)
        draw.time.range <- FALSE
        if (w <= adorn.length) {
            t <- try(eval(adorn[w]), silent=TRUE)
            if (class(t) == "try-error") warning("cannot evaluate adorn[", w, "]\n")
        }
    }
}
