# vim: tw=120 shiftwidth=4 softtabstop=4 expandtab:

remove.ship.motion <- function(x)
{
    rval <- x
    if (!("bottom.range" %in% names(x$data$ma)))
        return(rval)
    number.of.beams <- dim(x$data$ma$v)[3] # could also get from metadata but this is less brittle
    for (beam in 1:number.of.beams) {
        rval$data$ma$v[,,beam] <- rval$data$ma$v[,,beam] - rval$data$ma$bottom.velocity[,beam]
    }
    rval$processing.log <- processing.log.add(rval$processing.log,
                                              paste(deparse(match.call()), sep="", collapse=""))
    rval
}

coordinate <- function(x)
{
    if (inherits(x, "adp") || inherits(x, "adv"))
        x$metadata$oce.coordinate
    else {
        warning("unknown file type; the object must inherit from either \"adv\" or \"adp\"")
        NULL
    }
}
is.beam <- function(x)
{
    if (inherits(x, "adp") || inherits(x, "adv"))
        return(x$metadata$oce.coordinate == "beam")
    else {
        warning("unknown file type; the object must inherit from either \"adv\" or \"adp\"")
        return(FALSE)
    }
}
is.xyz <- function(x)
{
    if (inherits(x, "adp") || inherits(x, "adv"))
        return(x$metadata$oce.coordinate == "xyz")
    else {
        warning("unknown file type; the object must inherit from either \"adv\" or \"adp\"")
        return(FALSE)
    }
}
is.enu <- function(x)
{
    if (inherits(x, "adp") || inherits(x, "adv"))
        return(x$metadata$oce.coordinate == "enu")
    else {
        warning("unknown file type; the object must inherit from either \"adv\" or \"adp\"")
        return(FALSE)
    }
}

ad.beam.name <- function(x, which)
{
    if (x$metadata$oce.coordinate == "beam")
        c("beam 1", "beam 2", "beam 3", "beam 4")[which]
    else if (x$metadata$oce.coordinate == "enu")
        c("east", "north", "up", "error")[which]
    else if (x$metadata$oce.coordinate == "xyz")
        c("u", "v", "w", "e")[which]
    else if (x$metadata$oce.coordinate == "other")
        c("u'", "v'", "w'", "e")[which]
    else " "
}

read.adp <- function(file, from=1, to, by=1, tz=getOption("oce.tz"),
                     latitude=NA, longitude=NA,
                     type=c("rdi", "nortek", "sontek"),
                     debug=getOption("oce.debug"),
                     monitor=TRUE, despike=FALSE,
                     log.action, ...)
{
    oce.debug(debug, "read.adp(...,from=",from,",to=",if (missing(to)) "(missing)" else to,",by=",by,"type=",type,",...)\n")
    type <- match.arg(type)
    if (monitor)
        cat(file, "\n", ...)
    if (type == "rdi")
        read.adp.rdi(file=file, from=from, to=to, by=by, tz=tz,
                     latitude=latitude, longitude=longitude,
                     debug=debug-1, monitor=monitor, despike=despike,
                     log.action=log.action, ...)
    else if (type == "nortek")
        read.adp.nortek(file=file, from=from, to=to, by=by, tz=tz,
                        latitude=latitude, longitude=longitude,
                        debug=debug-1, monitor=monitor, despike=despike,
                        log.action=log.action, ...)
    else if (type == "sontek")
        read.adp.sontek(file=file, from=from, to=to, by=by, tz=tz,
                        latitude=latitude, longitude=longitude,
                        debug=debug-1, monitor=monitor, despike=despike,
                        log.action=log.action, ...)
}

summary.adp <- function(object, ...)
{
    if (!inherits(object, "adp"))
        stop("method is only for adp objects")
    if (is.null(object$metadata$have.actual.data) || object$metadata$have.actual.data) {
        if (1 == length(agrep("nortek", object$metadata$manufacturer, ignore.case=TRUE))) {
            res.specific <- list(internal.code.version=object$metadata$internal.code.version,
                                 hardware.revision=object$metadata$hardware.revision,
                                 rec.size=object$metadata$rec.size*65536/1024/1024,
                                 velocity.range=object$metadata$velocity.range,
                                 firmware.version=object$metadata$firmware.version,
                                 config=object$metadata$config,
                                 config.pressure.sensor=object$metadata$config.pressure.sensor,
                                 config.magnetometer.sensor=object$metadata$config.magnetometer.sensor,
                                 config.tilt.sensor=object$metadata$config.pressure.sensor,
                                 config.pressure.sensor=object$metadata$config.tilt.sensor,
                                 serial.number.head=object$metadata$serial.number.head,
                                 blanking.distance=object$metadata$blanking.distance,
                                 measurement.interval=object$metadata$measurement.interval,
                                 deployment.name=object$metadata$deployment.name,
                                 velocity.scale=object$metadata$velocity.scale)
        } else if (1 == length(agrep("rdi", object$metadata$manufacturer, ignore.case=TRUE))) {
            res.specific <- list(instrument.subtype=object$metadata[["instrument.subtype"]],
                                 manufacturer=object$metadata$manufacturer,
                                 number.of.data.types=object$metadata$number.of.data.types,
                                 heading.alignment=object$metadata$heading.alignment,
                                 heading.bias=object$metadata$heading.bias,
                                 pings.per.ensemble=object$metadata$pings.per.ensemble,
                                 bin1.distance=object$metadata$bin1.distance,
                                 xmit.pulse.length=object$metadata$xmit.pulse.length,
                                 oce.beam.attenuated=object$metadata$oce.beam.attenuated,
                                 beam.config=object$metadata$beam.config)
        } else if (1 == length(agrep("sontek", object$metadata$manufacturer, ignore.case=TRUE))) {
            res.specific <- list(cpu.software.ver.num=object$metadata$cpu.software.ver.num,
                                 dsp.software.ver.num=object$metadata$dsp.software.ver.num,
                                 board.rev=object$metadata$board.rev,
                                 adp.type=object$metadata$adp.type,
                                 slant.angle=object$metadata$slant.angle,
                                 orientation=object$metadata$orientation)
        } else {
            stop("can only summarize ADP objects of sub-type \"rdi\", \"sontek\", or \"nortek\", not class ", paste(class(object),collapse=","))
        }

        ## start building res from the header information
        have.data <- !is.null(object$data)
        res <- res.specific
        res$have.data <- have.data
        res$latitude <- object$metadata$latitude
        res$longitude <- object$metadata$longitude
        res$filename <- object$metadata$filename
        res$instrument.type <- object$metadata$instrument.type
        res$serial.number <- object$metadata$serial.number
        res$measurement.start <- object$metadata$measurement.start
        res$measurement.end <- object$metadata$measurement.end
        res$measurement.deltat <- object$metadata$measurement.deltat
        res$frequency <- object$metadata$frequency
        res$number.of.data.types <- object$metadata$number.of.data.type
        res$bin1.distance <- object$metadata$bin1.distance
        res$cell.size <- object$metadata$cell.size
        res$xmit.pulse.length <- object$metadata$xmit.pulse.length
        res$oce.beam.attenuated <- object$metadata$oce.beam.attenuated
        res$beam.angle <- object$metadata$beam.angle
        res$beam.config <- object$metadata$beam.config
        res$transformation.matrix <- object$metadata$transformation.matrix
        res$orientation <- object$metadata$orientation
        res$coordinate.system <- object$metadata$coordinate.system
        res$oce.coordinate <- object$metadata$oce.coordinate
        res$processing.log <- processing.log.summary(object)
        if (have.data) {
            ts.names <- names(object$data$ts)
            ma.names <- names(object$data$ma)
            fives <- matrix(nrow=(-1+length(ts.names)+length(ma.names)), ncol=5)
            ii <- 1
            for (i in 1:length(ts.names)) {
                if (names(object$data$ts)[i] != "time") {
                    fives[ii,] <- fivenum(object$data$ts[[ts.names[i]]], na.rm=TRUE)
                    ii <- ii + 1
                }
            }
            for (i in 1:length(ma.names)) {
                fives[ii,] <- fivenum(as.numeric(object$data$ma[[ma.names[i]]]), na.rm=TRUE)
                ii <- ii + 1
            }
            rownames(fives) <- c(ts.names[ts.names != "time"], ma.names)
            colnames(fives) <- c("Min.", "1st Qu.", "Median", "3rd Qu.", "Max.")
            v.dim <- dim(object$data$ma$v)
            res$subsample.start <- object$data$ts$time[1]
            res$subsample.end.time <- object$data$ts$time[length(object$data$ts$time)]
            res$subsample.deltat <- mean(diff(as.numeric(object$data$ts$time)),na.rm=TRUE)
            res$distance <- object$data$ss$distance
            res$fives <- fives
            res$time <- object$data$ts$time
            res$number.of.profiles <- v.dim[1]
            res$number.of.cells <- v.dim[2]
            res$number.of.beams <- v.dim[3]
            res$ts.names <- names(object$data$ts)
            res$ma.names <- names(object$data$ma)
        }
    } else {
        res$instrument.type <- object$metadata$instrument.type
        res$filename <- object$metadata$filename
        res$serial.number <- "unknown"
    }
    res$metadata <- object$metadata # FIXME: lazy
    class(res) <- "summary.adp"
    res
}                                       # summary.adp()

print.summary.adp <- function(x, digits=max(6, getOption("digits") - 1), ...)
{
    cat("ADP Summary\n-----------\n\n", ...)
    cat(paste("* Instrument:         ", x$instrument.type, ", serial number ``", paste(x$metadata$serial.number, collapse=""), "``\n", sep=""), ...)
    cat(paste("* Source filename:   ``", x$filename, "``\n", sep=""), ...)
    if ("latitude" %in% names(x)) {
        cat(paste("* Location:           ", if (is.na(x$latitude)) "unknown latitude" else sprintf("%.5f N", x$latitude), ", ",
                  if (is.na(x$longitude)) "unknown longitude" else sprintf("%.5f E", x$longitude), "\n"))
    }
    have.data <- x$have.data
    if (have.data) {
        cat(sprintf("* Measurements:       %s %s to %s %s sampled at %.4g Hz\n",
                    format(x$measurement.start), attr(x$measurement.start, "tzone"),
                    format(x$measurement.end), attr(x$measurement.end, "tzone"),
                    1 / x$measurement.deltat), ...)
        cat(sprintf("* Subsample:          %s %s to %s %s sampled at %.4g Hz\n",
                    format(x$subsample.start), attr(x$subsample.start, "tzone"),
                    format(x$subsample.end),  attr(x$subsample.end, "tzone"),
                    1 / x$subsample.deltat), ...)
        cat(sprintf("* Cells:              %d, centered at %.3f m to %.3f m, spaced by %.3f m\n",
                    x$number.of.cells, x$distance[1],  x$distance[length(x$distance)], diff(x$distance[1:2])),  ...)
    }
    cat("* Coordinate system: ", x$coordinate.system, "[originally],", x$oce.coordinate, "[presently]\n", ...)
    cat("* Frequency:         ", x$frequency, "kHz\n", ...)
    if (have.data) {
        cat("* Beams:             ", x$number.of.beams, if (!is.null(x$oce.beam.attenuated) & x$oce.beam.attenuated) "beams (attenuated)" else "beams (not attenuated)",
            "oriented", x$orientation, "with angle", x$metadata$beam.angle, "deg to axis\n", ...)
        if (!is.null(x$transformation.matrix)) {
            cat("\n* Transformation matrix\n  ::\n\n")
            cat("  ", format(x$transformation.matrix[1,], width=digits+4, digits=digits, justify="right"), "\n")
            cat("  ", format(x$transformation.matrix[2,], width=digits+4, digits=digits, justify="right"), "\n")
            cat("  ", format(x$transformation.matrix[3,], width=digits+4, digits=digits, justify="right"), "\n")
            if (x$number.of.beams > 3)
                cat("  ", format(x$transformation.matrix[4,], width=digits+4, digits=digits, justify="right"), "\n")
        }
        cat("\n")
        if (1 == length(agrep("rdi", x$manufacturer, ignore.case=TRUE))) {
            cat("* Teledyne-specific\n\n", ...)
            cat("  * Instrument subtype:         ", x$instrument.subtype, "\n", ...)
            cat("  * System configuration:       ", x$metadata$system.configuration, "\n", ...)
            cat("  * Software version:           ", paste(x$metadata$program.version.major, x$metadata$program.version.minor, sep="."), "\n", ...)
            cat("  * CPU board serial number:    ", x$metadata$cpu.board.serial.number, "\n", ...)
            cat("  * Xmit pulse length:          ", x$metadata$xmit.pulse.length,"m\n", ...)
            cat("  * Beam pattern:               ", x$metadata$beam.pattern, "\n", ...)
            cat("  * Pings per ensemble:         ", x$metadata$pings.per.ensemble, "\n", ...)
            cat("  * Heading alignment:          ", x$metadata$heading.alignment, "\n", ...)
            cat("  * Heading bias:               ", x$metadata$heading.bias)
            if (x$metadata$heading.bias != 0)
                cat(" [note: was *subtracted* from the file's heading, to create the obect's heading]\n", ...)
            else
                cat("\n", ...)
        } else if (1 == length(agrep("aquadopp", x$instrument.type, ignore.case=TRUE))) {
            cat("* Nortek-aquadopp-specific:\n\n", ...)
            cat("  * Internal code version:       ", x$metadata$internal.code.version, "\n", ...)
            cat("  * Hardware revision:           ", x$metadata$hardware.revision, "\n", ...)
            cat("  * Head serial number:          ", x$metadata$head.serial.number, "\n", ...)
        } else if (1 == length(agrep("sontek", x$instrument.type, ignore.case=TRUE))) {
            cat("* Sontek-specific:\n\n", ...)
            cat("  * CPU software version:        ", x$metadata$cpu.software.ver.num, "\n", ...)
            cat("  * DSP software version:        ", x$metadata$dsp.software.ver.num, "\n", ...)
            cat("  * Board rev:                   ", x$metadata$board.rev, "\n", ...)
        }
        cat("\n",...)
        cat("* Statistics of subsample\n  ::\n\n", ...)
        cat(show.fives(x, indent='     '), ...)
        ##cat("\n* Processing log::\n\n", ...)
        cat("\n")
        print(x$processing.log, ...)
    } else {
        cat("* There are no profiles in this file\n")
        print(x$processing.log, ...)
    }
    invisible(x)
}

plot.adp <- function(x, which=1:dim(x$data$ma$v)[3],
                     col,
                     zlim,
                     titles,
                     lwd=par('lwd'),
                     type='l',
                     ytype=c("profile", "distance"),
                     adorn=NULL,
                     draw.time.range=getOption("oce.draw.time.range"),
                     use.smoothScatter,
                     mgp=getOption("oce.mgp"),
                     mar=c(mgp[1]+1.5,mgp[1]+1.5,1.5,1.5),
                     margins.as.image=FALSE,
                     cex=par("cex"), cex.axis=par("cex.axis"), cex.main=par("cex.main"),
                     xlim, ylim, 
                     control,
                     use.layout=FALSE,
                     main="",
                     debug=getOption("oce.debug"),
                     ...)
{
    debug <- max(0, min(debug, 4))
    oce.debug(debug, "\b\bplot.adp(x, which=", paste(which, collapse=","), ") {\n", sep="")
    oce.debug(debug, "early in plot.adp:\n")
    oce.debug(debug, "  par(mar)=", paste(par('mar'), collapse=" "), "\n")
    oce.debug(debug, "  par(mai)=", paste(par('mai'), collapse=" "), "\n")
    if (!missing(ylim))
        oce.debug(debug, "ylim=c(", paste(ylim, collapse=", "), ")\n")
    if (!inherits(x, "adp"))
        stop("method is only for adp objects")
    if (!(is.null(x$metadata$have.actual.data) || x$metadata$have.actual.data)) {
        warning("there are no profiles in this dataset")
        return
    }
    opar <- par(no.readonly = TRUE)
    nw <- length(which)
    nbeams  <- x$metadata$number.of.beams
    if (nw == 1) {
        pm <- pmatch(which, c("velocity","amplitude","quality","hydrography", "angles"))
        if (!is.na(pm)) {
            if (pm == 1)
                which <- 0 + seq(1, nbeams)
            else if (pm == 2)
                which <- 4 + seq(1, nbeams)
            else if (pm == 3)
                which <- 8 + seq(1, nbeams)
            else if (pm == 4)
                which <- 14:15
            else if (pm == 5)
                which <- 16:18
            nw <- length(which)
        }
    }
    if (!missing(titles) && length(titles) != nw)
        stop("length of 'titles' must equal length of 'which'")
    if (nw > 1)
        on.exit(par(opar))
    par(mgp=mgp, mar=mar, cex=cex)
    dots <- list(...)
    ytype <- match.arg(ytype)
    ## user may specify a matrix for xlim and ylim
    gave.ylim <- !missing(ylim)
    oce.debug(debug, 'gave.ylim=', gave.ylim, '\n')
    if (gave.ylim) {
        if (is.matrix(ylim)) {
            if (dim(ylim)[2] != nw) {
                ylim2 <- matrix(ylim, ncol=2, nrow=nw) # FIXME: is this what I want?
            }
        } else {
            ylim2 <- matrix(ylim, ncol=2, nrow=nw) # FIXME: is this what I want?
        }
        class(ylim2) <- class(ylim)
        ylim <- ylim2
    }
    gave.xlim <- !missing(xlim)
    if (gave.xlim) {
        if (is.matrix(xlim)) {
            if (dim(xlim)[2] != nw) {
                xlim2 <- matrix(xlim, ncol=2, nrow=nw) # FIXME: is this what I want?
            }
        } else {
            if (length(xlim) != 2)
                stop("xlim must be a vector of length 2, or a 2-column matrix")
            xlim2 <- matrix(xlim[1:2], ncol=2, nrow=nw, byrow=TRUE)
        }
        xlim <- xlim2
    }
    if (missing(zlim)) {
        gave.zlim <- FALSE
        zlim.given <- NULL
    } else {
        gave.zlim <- TRUE
        if (is.vector(zlim)) {
            if (length(zlim) == 2) {
                zlim.given <- matrix(rep(zlim, length(which)),ncol=2,byrow=TRUE)
            } else {
                stop("zlim must be a vector of length 2, or a matrix with 2 columns")
            }
        }
        zlim.given <- zlim
    }
    ylim.given <- if (gave.ylim) dots[["ylim"]] else NULL
    if (missing(lwd))
        lwd <- rep(par('lwd'), length.out=nw)
    else
        lwd <- rep(lwd, length.out=nw)
    if (missing(main))
        main <- rep('', length.out=nw)
    else
        main <- rep(main, length.out=nw)
    oce.debug(debug, "later on in plot.adp:\n")
    oce.debug(debug, "  par(mar)=", paste(par('mar'), collapse=" "), "\n")
    oce.debug(debug, "  par(mai)=", paste(par('mai'), collapse=" "), "\n")

    ## Translate word-style (FIXME: ugly coding)
    which2 <- vector("numeric", length(which))
    for (w in 1:nw) {
        ww <- which[w]
        if (is.numeric(ww) || 1 == length(grep("^[0-9]*$", ww))) {
            which2[w] <- as.numeric(ww)
        } else {
            if (     ww == "u1") which2[w] <- 1
            else if (ww == "u2") which2[w] <- 2
            else if (ww == "u3") which2[w] <- 3
            else if (ww == "u4") which2[w] <- 4
            else if (ww == "a1") which2[w] <- 5
            else if (ww == "a2") which2[w] <- 6
            else if (ww == "a3") which2[w] <- 7
            else if (ww == "a4") which2[w] <- 8
            else if (ww == "q1") which2[w] <- 9
            else if (ww == "q2") which2[w] <- 10
            else if (ww == "q3") which2[w] <- 11
            else if (ww == "q4") which2[w] <- 12
            else if (ww == "salinity") which2[w] <- 13
            else if (ww == "temperature") which2[w] <- 14
            else if (ww == "pressure") which2[w] <- 15
            else if (ww == "heading") which2[w] <- 16
            else if (ww == "pitch") which2[w] <- 17
            else if (ww == "roll") which2[w] <- 18
            ## 19 beam-1 correlation-amplitude diagnostic plot
            ## 20 beam-2 correlation-amplitude diagnostic plot
            ## 21 beam-3 correlation-amplitude diagnostic plot
            ## 22 beam-4 correlation-amplitude diagnostic plot
            else if (ww == "progressive vector") which2[w] <- 23
            else if (ww == "uv") which2[w] <- 28
            else if (ww == "uv+ellipse") which2[w] <- 29
            else if (ww == "uv+ellipse+arrow") which2[w] <- 30
            ## 40 to 44 only work for bottom-tracking devices
            else if (ww == "bottom.range" ) which2[w] <- 40 # average of all beams
            else if (ww == "bottom.range1") which2[w] <- 41 # beam1
            else if (ww == "bottom.range2") which2[w] <- 42 # beam2
            else if (ww == "bottom.range3") which2[w] <- 43 # beam3
            else if (ww == "bottom.range4") which2[w] <- 44 # beam4
            ## 50 to 54 only work for bottom-tracking devices
            else if (ww == "bottom.u" ) which2[w] <- 50 # average of all beams
            else if (ww == "bottom.u1") which2[w] <- 51 # beam1
            else if (ww == "bottom.u2") which2[w] <- 52 # beam1
            else if (ww == "bottom.u3") which2[w] <- 53 # beam1
            else if (ww == "bottom.u4") which2[w] <- 54 # beam1
            else stop("unknown 'which':", ww)
        }
    }
    which <- which2
    images <- 1:12
    timeseries <- 13:22
    spatial <- 23:27
    speed <- 28

    adorn.length <- length(adorn)
    if (adorn.length == 1) {
        adorn <- rep(adorn, nw)
        adorn.length <- nw
    }

    tt <- x$data$ts$time
    class(tt) <- "POSIXct"              # otherwise image() gives warnings
    if (gave.zlim && all(which %in% 5:8)) { # single scale for all
        zlim <- range(abs(as.numeric(x$data$ma[,,which[1]])), na.rm=TRUE)
        for (w in 2:length(which)) {
            zlim <- range(abs(c(zlim, x$data$ma[[which[w]]])), na.rm=TRUE)
        }
    }
    ##oce.debug(debug, "use.layout=", use.layout, "\n")
    show.bottom <- ("bottom.range" %in% names(x$data$ma)) && !missing(control) && !is.null(control["draw.bottom"])
    if (show.bottom)
        bottom <- apply(x$data$ma$bottom.range, 1, mean)
    oce.debug(debug, "show.bottom=", show.bottom, "\n")
    if (use.layout) {
        if (any(which %in% images) || margins.as.image) {
            w <- 1.5
            lay <- layout(matrix(1:(2*nw), nrow=nw, byrow=TRUE), widths=rep(c(1, lcm(w)), nw))
            oce.debug(debug, "calling layout(matrix...)\n")
            oce.debug(debug, "using layout, since this is an image, or has margins as image\n")
        } else {
            if (nw != 1 || which != 23) {
                lay <- layout(cbind(1:nw))
                oce.debug(debug, "calling layout(cbind(1:", nw, ")\n")
                oce.debug(debug, "using layout\n")
            }
        }
    } else {
        if (nw > 1) {
            par(mfrow=c(nw, 1))
            oce.debug(debug, "calling par(mfrow=c(", nw, ", 1)\n")
        }
    }
    flip.y <- ytype == "profile" && x$metadata$orientation == "downward"
    have.time.images <- any(which %in% images)
    oce.debug(debug, 'have.time.images=', have.time.images, '(if TRUE, it means any timeseries graphs get padding on RHS)\n')
    for (w in 1:nw) {
        oce.debug(debug, "which[", w, "]=", which[w], "; draw.time.range=", draw.time.range, "\n")
        if (which[w] %in% images) {                   # image types
            skip <- FALSE
            if (which[w] %in% 1:(0+x$metadata$number.of.beams)) {    #velocity
                z <- x$data$ma$v[,,which[w]]
                y.look <- if (gave.ylim)
                    ylim.given[1] <= x$data$ss$distance & x$data$ss$distance <= ylim.given[2]
                else rep(TRUE, length(x$data$ss$distance))
                zlim <- if (gave.zlim) zlim.given[w,] else max(abs(x$data$ma$v[,y.look,which[w]]), na.rm=TRUE) * c(-1,1)
                zlab <- if (missing(titles)) ad.beam.name(x, which[w]) else titles[w]
            } else if (which[w] %in% 5:(4+x$metadata$number.of.beams)) { # amplitude
                z <- as.numeric(x$data$ma$a[,,which[w]-4])
                dim(z) <- dim(x$data$ma$a)[1:2]
                y.look <- if (gave.ylim)
                    ylim.given[1] <= x$data$ss$distance & x$data$ss$distance <= ylim.given[2]
                else
                    rep(TRUE, length(x$data$ss$distance))
                zlim <- range(as.numeric(x$data$ma$a[,y.look,]), na.rm=TRUE)
                zlab <- c(expression(a[1]),expression(a[2]),expression(a[3]),expression(a[4]))[which[w]-4]
            } else if (which[w] %in% 9:(8+x$metadata$number.of.beams)) { # correlation
                if ("q" %in% names(x$data$ma)) {
                    z <- as.numeric(x$data$ma$q[,,which[w]-8])
                    dim(z) <- dim(x$data$ma$q)[1:2]
                    zlim <- c(0, 256)
                    zlab <- c(expression(q[1]),expression(q[2]),expression(q[3]))[which[w]-8]
                } else if ("amp" %in% names(x$data$ma)) {
                    z <- as.numeric(x$data$ma$amp[,,which[w]-8])
                    dim(z) <- dim(x$data$ma$amp)[1:2]
                    zlim <- c(0, max(as.numeric(x$data$ma$amp)))
                    zlab <- c(expression(amp[1]),expression(amp[2]),expression(amp[3]))[which[w]-8]
                }
            } else {
                skip <- TRUE
            }
            if (!skip) {
                imagep(x=tt, y=x$data$ss$distance, z=z,
                       zlim=zlim,
                       flip.y=flip.y,
                       col=if (missing(col)) oce.colors.palette(128, 1) else col,
                       ylab=resizable.label("distance"),
                       xlab="Time",
                       zlab=zlab,
                       draw.time.range=draw.time.range,
                       draw.contours=FALSE,
                       adorn=adorn[w],
                       mgp=mgp,
                       mar=mar,
                       cex=cex*(1 - min(nw / 8, 1/4)), # FIXME: should emulate par(mfrow)
                       main=main[w],
                       debug=debug-1,
                       ...)
            }
            if (show.bottom)
                lines(x$data$ts$time, bottom)
            draw.time.range <- FALSE
        } else if (which[w] %in% timeseries) { # time-series types
            if (missing(col)) col <- rep("black", length.out=nw) else col <- rep(col, length.out=nw)
            oce.debug(debug, "graph", w, "is a timeseries\n")
            par(mgp=mgp, mar=mar, cex=cex)
            tlim <- range(x$data$ts$time)
            if (which[w] == 13) {
                par(cex=cex*(1 - min(nw / 8, 1/4)))
                if (have.time.images)
                    drawpalette(debug=debug-1)
                oce.plot.ts(x$data$ts$time, x$data$ts$salinity,
                            xlim=if(gave.xlim) xlim[w,] else tlim,
                            ylim=if(gave.ylim) ylim[w,],
                            xaxs="i",
                            col=col[w],
                            lwd=lwd[w],
                            cex.axis=cex*(1 - min(nw / 8, 1/4)),
                            main=main[w],
                            ylab=resizable.label("S"),
                            type=type,
                            mgp=mgp,
                            mar=if(have.time.images) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                            draw.time.range=draw.time.range, adorn=adorn[w])
            }
            if (which[w] == 14) {
                par(cex=cex*(1 - min(nw / 8, 1/4)))
                if (have.time.images)
                    drawpalette(debug=debug-1)
                oce.plot.ts(x$data$ts$time, x$data$ts$temperature,
                            xlim=if(gave.xlim) xlim[w,] else tlim,
                            ylim=if(gave.ylim) ylim[w,],
                            xaxs="i",
                            col=col[w],
                            lwd=lwd[w],
                            cex=cex*(1 - min(nw / 8, 1/4)),
                            cex.axis=cex*(1 - min(nw / 8, 1/4)),
                            main=main[w],
                            ylab=expression(paste("T [ ", degree, "C ]")),
                            type=type,
                            mgp=mgp,
                            mar=if(have.time.images) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                            draw.time.range=draw.time.range,
                            adorn=adorn[w],
                            debug=debug-1)
            }
            if (which[w] == 15) {
                par(cex=cex*(1 - min(nw / 8, 1/4)))
                if (have.time.images)
                    drawpalette(debug=debug-1)
                oce.debug(debug, "pressure plot. col=", col[w], "\n")
                oce.plot.ts(x$data$ts$time, x$data$ts$pressure,
                            xlim=if(gave.xlim) xlim[w,] else tlim,
                            ylim=if(gave.ylim) ylim[w,],
                            xaxs="i",
                            col=col[w],
                            lwd=lwd[w],
                            cex.axis=cex*(1 - min(nw / 8, 1/4)),
                            main=main[w],
                            ylab=resizable.label("p"),
                            type=type,
                            mgp=mgp,
                            mar=if(have.time.images) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                            draw.time.range=draw.time.range, adorn=adorn[w])
            }
            if (which[w] == 16) {
                par(cex=cex*(1 - min(nw / 8, 1/4)))
                if (have.time.images)
                    drawpalette(debug=debug-1)
                oce.plot.ts(x$data$ts$time, x$data$ts$heading,
                            xlim=if(gave.xlim) xlim[w,] else tlim,
                            ylim=if(gave.ylim) ylim[w,],
                            xaxs="i",
                            col=col[w],
                            lwd=lwd[w],
                            cex.axis=cex*(1 - min(nw / 8, 1/4)),
                            main=main[w],
                            ylab=resizable.label("heading"),
                            type=type,
                            mgp=mgp, 
                            mar=if(have.time.images) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                            draw.time.range=draw.time.range, adorn=adorn[w])
            }
            if (which[w] == 17) {
                par(cex=cex*(1 - min(nw / 8, 1/4)))
                if (have.time.images)
                    drawpalette(debug=debug-1)
                oce.plot.ts(x$data$ts$time, x$data$ts$pitch,
                            xlim=if(gave.xlim) xlim[w,] else tlim,
                            ylim=if(gave.ylim) ylim[w,],
                            xaxs="i",
                            col=col[w],
                            lwd=lwd[w],
                            cex.axis=cex*(1 - min(nw / 8, 1/4)),
                            main=main[w],
                            ylab=resizable.label("pitch"),
                            type=type,
                            mgp=mgp,
                            mar=if(have.time.images) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                            draw.time.range=draw.time.range, adorn=adorn[w])
            }
            if (which[w] == 18) {
                par(cex=cex*(1 - min(nw / 8, 1/4)))
                if (have.time.images)
                    drawpalette(debug=debug-1)
                oce.plot.ts(x$data$ts$time, x$data$ts$roll,
                            xlim=if(gave.xlim) xlim[w,] else tlim,
                            ylim=if(gave.ylim) ylim[w,],
                            xaxs="i",
                            col=col[w],
                            lwd=lwd[w],
                            cex.axis=cex*(1 - min(nw / 8, 1/4)),
                            main=main[w],
                            ylab=resizable.label("roll"),
                            type=type,
                            mgp=mgp,
                            mar=if(have.time.images) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                            draw.time.range=draw.time.range, adorn=adorn[w])
            }
            if (which[w] == 19) {
                par(cex=cex*(1 - min(nw / 8, 1/4)))
                if (have.time.images)
                    drawpalette(debug=debug-1)
                if (x$metadata$number.of.beams > 0)
                    oce.plot.ts(x$data$ts$time, apply(x$data$ma$v[,,1], 1, mean, na.rm=TRUE),
                                xlim=if(gave.xlim) xlim[w,] else tlim,
                                ylim=if(gave.ylim) ylim[w,],
                                xaxs="i",
                                col=col[w],
                                lwd=lwd[w],
                                cex.axis=cex*(1 - min(nw / 8, 1/4)),
                                main=main[w],
                                ylab=ad.beam.name(x, 1),
                                type=type,
                                mgp=mgp,
                                mar=if(have.time.images) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                draw.time.range=draw.time.range,
                                adorn=adorn[w], ...)
                    else
                        warning("cannot plot beam/velo 1 because the device no beams")
            }
            if (which[w] == 20) {
                par(cex=cex*(1 - min(nw / 8, 1/4)))
                if (have.time.images)
                    drawpalette(debug=debug-1)
                if (x$metadata$number.of.beams > 1)
                    oce.plot.ts(x$data$ts$time, apply(x$data$ma$v[,,2], 1, mean, na.rm=TRUE),
                                xlim=if(gave.xlim) xlim[w,] else tlim,
                                ylim=if(gave.ylim) ylim[w,],
                                xaxs="i",
                                col=col[w],
                                lwd=lwd[w],
                                cex.axis=cex*(1 - min(nw / 8, 1/4)),
                                main=main[w],
                                ylab=ad.beam.name(x, 2),
                                type=type,
                                mgp=mgp,
                                mar=if(have.time.images) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                draw.time.range=draw.time.range,
                                adorn=adorn[w], ...)
                    else
                        warning("cannot plot beam/velo 2 because the device has only ", x$metadata$number.of.beams, " beams")
            }
            if (which[w] == 21) {
                par(cex=cex*(1 - min(nw / 8, 1/4)))
                if (have.time.images)
                    drawpalette(debug=debug-1)
                if (x$metadata$number.of.beams > 2)
                    oce.plot.ts(x$data$ts$time, apply(x$data$ma$v[,,3], 1, mean, na.rm=TRUE),
                                xlim=if(gave.xlim) xlim[w,] else tlim,
                                ylim=if(gave.ylim) ylim[w,],
                                xaxs="i",
                                col=col[w],
                                lwd=lwd[w],
                                cex.axis=cex*(1 - min(nw / 8, 1/4)),
                                main=main[w],
                                ylab=ad.beam.name(x, 3),
                                type=type,
                                mgp=mgp,
                                mar=if(have.time.images) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                draw.time.range=draw.time.range,
                                adorn=adorn[w], ...)
                    else
                        warning("cannot plot beam/velo 3 because the device has only", x$metadata$number.of.beams, "beams")
            }
            if (which[w] == 22) {
                par(cex=cex*(1 - min(nw / 8, 1/4)))
                if (have.time.images)
                    drawpalette(debug=debug-1)
                if (x$metadata$number.of.beams > 3)
                    oce.plot.ts(x$data$ts$time, apply(x$data$ma$v[,,4], 1, mean, na.rm=TRUE),
                                xlim=if(gave.xlim) xlim[w,] else tlim,
                                ylim=if(gave.ylim) ylim[w,],
                                xaxs="i",
                                col=col[w],
                                lwd=lwd[w],
                                cex.axis=cex*(1 - min(nw / 8, 1/4)),
                                main=main[w], 
                                ylab=ad.beam.name(x, 4),
                                type=type,
                                mgp=mgp,
                                mar=if(have.time.images) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                draw.time.range=draw.time.range,
                                adorn=adorn[w], ...)
                    else
                        warning("cannot plot beam/velo 4 because the device has only", x$metadata$number.of.beams, "beams")
            }
            draw.time.range <- FALSE
            if (margins.as.image && use.layout)  { # FIXME: I think this should be deleted
                ## blank plot, to get axis length same as for images
                omar <- par("mar")
                par(mar=c(mar[1], 1/4, mgp[2]+1/2, mgp[2]+1))
                plot(1:2, 1:2, type='n', axes=FALSE, xlab="", ylab="")
                par(mar=omar)
            }
        } else if (which[w] %in% spatial) {                   # various spatial types
            if (which[w] == 23) {    # progressive vector
                par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
                dt <- as.numeric(difftime(x$data$ts$time[2], x$data$ts$time[1],units="sec")) # FIXME: should not assume all equal
                m.per.km <- 1000
                if (!missing(control) && !is.null(control$bin)) {
                    if (control$bin < 1)
                        stop("cannot have control$bin less than 1, but got ", control$bin)
                    max.bin <- dim(x$data$ma$v)[2]
                    if (control$bin > max.bin)
                        stop("cannot have control$bin larger than ", max.bin," but got ", control$bin)
                    u <- x$data$ma$v[,control$bin,1]
                    v <- x$data$ma$v[,control$bin,2]
                } else {
                    u <- apply(x$data$ma$v[,,1], 1, mean, na.rm=TRUE)
                    v <- apply(x$data$ma$v[,,2], 1, mean, na.rm=TRUE)
                }
                u[is.na(u)] <- 0        # zero out missing
                v[is.na(v)] <- 0
                x.dist <- cumsum(u) * dt / m.per.km
                y.dist <- cumsum(v) * dt / m.per.km
                plot(x.dist, y.dist, xlab="km", ylab="km", type='l', asp=1, col=if (missing(col)) "black" else col, ...)
            } else if (which[w] == 24) {
                par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
                value <- apply(x$data$ma$v[,,1], 2, mean, na.rm=TRUE)
                plot(value, x$data$ss$distance, xlab=ad.beam.name(x, 1), ylab="Distance [m]", type='l', ...)
            } else if (which[w] == 25) {
                par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
                value <- apply(x$data$ma$v[,,2], 2, mean, na.rm=TRUE)
                plot(value, x$data$ss$distance, xlab=ad.beam.name(x, 2), ylab="Distance [m]", type='l', ...)
            } else if (which[w] == 26) {
                par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
                value <- apply(x$data$ma$v[,,3], 2, mean, na.rm=TRUE)
                plot(value, x$data$ss$distance, xlab=ad.beam.name(x, 3), ylab="Distance [m]", type='l', ...)
                ##grid()
            } else if (which[w] == 27) {
                if (x$metadata$number.of.beams > 3) {
                    par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
                    value <- apply(x$data$ma$v[,,4], 2, mean, na.rm=TRUE)
                    plot(value, x$data$ss$distance, xlab=ad.beam.name(x, 4), ylab="Distance [m]", type='l', ...)
                    ##grid()
                } else {
                    warning("cannot use which=27 because this device did not have 4 beams")
                }
            }
            if (w <= adorn.length) {
                t <- try(eval(adorn[w]), silent=TRUE)
                if (class(t) == "try-error") warning("cannot evaluate adorn[", w, "]\n")
            }
        } else if (which[w] %in% 28:30) { # "uv", "uv+ellipse", or "uv+ellipse+arrow"
            par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
            n <- dim(x$data$ma$v)[1]
            if (!missing(control) && !is.null(control$bin)) {
                if (control$bin < 1)
                    stop("cannot have control$bin less than 1, but got ", control$bin)
                max.bin <- dim(x$data$ma$v)[2]
                if (control$bin > max.bin)
                    stop("cannot have control$bin larger than ", max.bin," but got ", control$bin)
                u <- x$data$ma$v[,control$bin,1]
                v <- x$data$ma$v[,control$bin,2]
            } else {
                u <- apply(x$data$ma$v[,,1], 1, mean, na.rm=TRUE)
                v <- apply(x$data$ma$v[,,2], 1, mean, na.rm=TRUE)
            }
            oce.debug(debug, "uv type plot\n")
            if (n < 5000 || (!missing(use.smoothScatter) && !use.smoothScatter)) {
                if ("type" %in% names(dots)) {
                    plot(u, v, xlab="u [m/s]", ylab="v [m/s]", asp=1, col=if (missing(col)) "black" else col,
                         xlim=if(gave.xlim) xlim[w,] else range(u, na.rm=TRUE),
                         ylim=if(gave.ylim) ylim[w,] else range(v, na.rm=TRUE),
                         ...)
                } else {
                    plot(u, v, xlab="u [m/s]", ylab="v [m/s]", type='n', asp=1,
                         xlim=if(gave.xlim) xlim[w,] else range(u, na.rm=TRUE),
                         ylim=if(gave.ylim) ylim[w,] else range(v, na.rm=TRUE),
                         ...)
                    points(u, v, cex=cex/2, col=if (missing(col)) "black" else col)
                }
            } else {
                smoothScatter(u, v, xlab="u [m/s]", ylab="v [m/s]", asp=1,
                              xlim=if(gave.xlim) xlim[w,] else range(u, na.rm=TRUE),
                              ylim=if(gave.ylim) ylim[w,] else range(v, na.rm=TRUE),
                              ...)
            }
            if (main[w] != "")
                mtext(main[w], adj=1)
            if (which[w] >= 29) {
                ok <- !is.na(u) & !is.na(v)
                e <- eigen(cov(data.frame(u[ok],v[ok])))
                major <- sqrt(e$values[1])  # major
                minor <- sqrt(e$values[2])  # minor
                theta <- seq(0, 2*pi, length.out=360/5)
                xx <- major * cos(theta)
                yy <- minor * sin(theta)
                theta0 <- atan2(e$vectors[2,1], e$vectors[1,1])
                rotate <- matrix(c(cos(theta0), -sin(theta0), sin(theta0), cos(theta0)), nrow=2, byrow=TRUE)
                xxyy <- rotate %*% rbind(xx, yy)
                col <- if (!missing(col)) col else "darkblue"
                lines(xxyy[1,], xxyy[2,], lwd=5, col="yellow")
                lines(xxyy[1,], xxyy[2,], lwd=2, col=col)
                if (which[w] >= 30) {
                    if (!missing(control) && !is.null(control$bin)) {
                        if (control$bin < 1)
                            stop("cannot have control$bin less than 1, but got ", control$bin)
                        max.bin <- dim(x$data$ma$v)[2]
                        if (control$bin > max.bin)
                            stop("cannot have control$bin larger than ", max.bin," but got ", control$bin)
                        umean <- mean(x$data$ma$v[,control$bin,1], na.rm=TRUE)
                        vmean <- mean(x$data$ma$v[,control$bin,2], na.rm=TRUE)
                    } else {
                        umean <- mean(x$data$ma$v[,,1], na.rm=TRUE)
                        vmean <- mean(x$data$ma$v[,,2], na.rm=TRUE)
                    }
                    arrows(0, 0, umean, vmean, lwd=5, length=1/10, col="yellow")
                    arrows(0, 0, umean, vmean, lwd=2, length=1/10, col=col)
                }
            }
        } else if (which[w] %in% 40:44) { # bottom range
            par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
            n <- prod(dim(x$data$ma$v)[1:2])
            if ("bottom.range" %in% names(x$data$ma)) {
                if (which[w] == 40)
                    oce.plot.ts(x$data$ts$time, apply(x$data$ma$bottom.range, 1, mean, na.rm=TRUE), ylab="Range [m]")
                else if (which[w] == 41)
                    oce.plot.ts(x$data$ts$time, x$data$ma$bottom.range[,1], ylab="Beam 1 range [m]")
                else if (which[w] == 42)
                    oce.plot.ts(x$data$ts$time, x$data$ma$bottom.range[,2], ylab="Beam 1 range [m]")
                else if (which[w] == 43)
                    oce.plot.ts(x$data$ts$time, x$data$ma$bottom.range[,3], ylab="Beam 1 range [m]")
                else if (which[w] == 44)
                    oce.plot.ts(x$data$ts$time, x$data$ma$bottom.range[,4], ylab="Beam 1 range [m]")
            } else {
                warning("cannot handle which= ", which[w], " because this instrument lacked bottom tracking")
            }
        } else if (which[w] %in% 50:54) { # bottom velocity
            par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
            n <- prod(dim(x$data$ma$v)[1:2])
            if ("bottom.velocity" %in% names(x$data$ma)) {
                if (which[w] == 50)
                    oce.plot.ts(x$data$ts$time, apply(x$data$ma$bottom.velocity, 1, mean, na.rm=TRUE), ylab="Range [m]")
                else if (which[w] == 51)
                    oce.plot.ts(x$data$ts$time, x$data$ma$bottom.velocity[,1], ylab="Beam 1 velocity [m/s]")
                else if (which[w] == 52)
                    oce.plot.ts(x$data$ts$time, x$data$ma$bottom.velocity[,2], ylab="Beam 2 velocity [m/s]")
                else if (which[w] == 53)
                    oce.plot.ts(x$data$ts$time, x$data$ma$bottom.velocity[,3], ylab="Beam 3 velocity [m/s]")
                else if (which[w] == 54)
                    oce.plot.ts(x$data$ts$time, x$data$ma$bottom.velocity[,4], ylab="Beam 4 velocity [m/s]")
            } else {
                warning("cannot handle which= ", which[w], " because this instrument lacked bottom tracking")
            }
        } else {
            stop("unknown value of which (", which[w], ")")
        }
        if (w <= adorn.length) {
            t <- try(eval(adorn[w]), silent=TRUE)
            if (class(t) == "try-error")
                warning("cannot evaluate adorn[", w, "]\n")
        }
    }
    oce.debug(debug, "\b\b\b} # plot.adp()\n")
    invisible()
}

to.enu.adp <- function(x, declination=0, debug=getOption("oce.debug"))
{
    oce.debug(debug, "\b\badp.2enu() {\n")
    coord <- x$metadata$oce.coordinate
    if (coord == "beam") {
        x <- xyz.to.enu.adp(beam.to.xyz.adp(x, debug=debug-1), declination=declination, debug=debug-1)
    } else if (coord == "xyz") {
        x <- xyz.to.enu.adp(x, declination=declination, debug=debug-1)
    } else if (coord == "enu") {
        ;
    } else {
        warning("adp.2enu cannot convert from coordinate system ", coord, " to ENU, so returning argument as-is")
    }
    oce.debug(debug, "\b\b} # adp.2enu()\n")
    x
}

beam.attenuate.adp <- function(x, count2db=c(0.45, 0.45, 0.45, 0.45), debug=getOption("oce.debug"))
{
    oce.debug(debug, "\b\badp.beam.attenuate(...) {\n")
    if (!inherits(x, "adp"))
        stop("method is only for adp objects")
    if (x$metadata$oce.beam.attenuated)
        stop("the beams are already attenuated in this dataset")
    res <- x
    num.profiles <- dim(x$data$ma$a)[1]
    oce.debug(debug, "num.profiles=", num.profiles, "\n")
    correction <- matrix(rep(20 * log10(x$data$ss$distance), num.profiles),
                         nrow=num.profiles, byrow=TRUE)
    for (beam in 1:x$metadata$number.of.beams) {
        oce.debug(debug, "beam=",beam,"\n")
        tmp <- floor(count2db[beam] * as.numeric(x$data$ma$a[,,beam]) + correction)
        tmp[tmp < 0] <- 0
        tmp[tmp > 255] <- 255
        res$data$ma$a[,,beam] <- as.raw(tmp)
    }
    res$metadata$oce.beam.attenuated <- TRUE
    res$processing.log <- processing.log.add(res$processing.log,
                                             paste(deparse(match.call()), sep="", collapse=""))
    oce.debug(debug, "\b\b} # beamAttenuate.adp()\n")
    res
}

beam.to.xyz.adp <- function(x, debug=getOption("oce.debug"))
{
    debug <- if (debug > 0) 1 else 0
    oce.debug(debug, "\b\bbeam.to.xyz.adp(x, debug=", debug, ") {\n", sep="")
    if (!inherits(x, "adp"))
        stop("method is only for objects of class \"adp\"")
    if (x$metadata$oce.coordinate != "beam")
        stop("input must be in beam coordinates")
    if (inherits(x, "rdi")) {
        if (x$metadata$number.of.beams != 4)
            stop("can only handle 4-beam ADP units from RDI")
        res <- x
        if (is.null(x$metadata$transformation.matrix))
            stop("missing x$metadata$transformation.matrix")
        tm <- x$metadata$transformation.matrix
        if (!all.equal(dim(tm), c(4,4)))
            stop("x$metadata$transformation.matrix must be a 4x4 matrix")
        if (debug) {
            cat("Transformation matrix:\n")
            print(tm)
        }
        V <- x$data$ma$v[,,1:4]
        res$data$ma$v[,,1] <- tm[1,1] * V[,,1] + tm[1,2] * V[,,2] + tm[1,3] * V[,,3] + tm[1,4] * V[,,4]
        res$data$ma$v[,,2] <- tm[2,1] * V[,,1] + tm[2,2] * V[,,2] + tm[2,3] * V[,,3] + tm[2,4] * V[,,4]
        res$data$ma$v[,,3] <- tm[3,1] * V[,,1] + tm[3,2] * V[,,2] + tm[3,3] * V[,,3] + tm[3,4] * V[,,4]
        res$data$ma$v[,,4] <- tm[4,1] * V[,,1] + tm[4,2] * V[,,2] + tm[4,3] * V[,,3] + tm[4,4] * V[,,4]
    } else if (inherits(x, "nortek")) {
        if (x$metadata$number.of.beams != 3)
            stop("can only handle 3-beam ADP units from nortek")
        if (is.null(x$metadata$transformation.matrix))
            stop("missing x$metadata$transformation.matrix")
        tm <- x$metadata$transformation.matrix
        if (!all.equal(dim(tm), c(3, 3)))
            stop("x$metadata$transformation.matrix must be a 3x3 matrix")
        res <- x
        V <- x$data$ma$v[,,1:3]
        res$data$ma$v[,,1] <- tm[1,1] * V[,,1] + tm[1,2] * V[,,2] + tm[1,3] * V[,,3]
        res$data$ma$v[,,2] <- tm[2,1] * V[,,1] + tm[2,2] * V[,,2] + tm[2,3] * V[,,3]
        res$data$ma$v[,,3] <- tm[3,1] * V[,,1] + tm[3,2] * V[,,2] + tm[3,3] * V[,,3]
    } else if (inherits(x, "sontek")) {
        if (x$metadata$number.of.beams != 3)
            stop("can only handle 3-beam ADP units from sontek")
        if (is.null(x$metadata$transformation.matrix))
            stop("missing x$metadata$transformation.matrix")
        tm <- x$metadata$transformation.matrix
        if (!all.equal(dim(tm), c(3, 3)))
            stop("x$metadata$transformation.matrix must be a 3x3 matrix")
        res <- x
        V <- x$data$ma$v[,,1:3]
        res$data$ma$v[,,1] <- tm[1,1] * V[,,1] + tm[1,2] * V[,,2] + tm[1,3] * V[,,3]
        res$data$ma$v[,,2] <- tm[2,1] * V[,,1] + tm[2,2] * V[,,2] + tm[2,3] * V[,,3]
        res$data$ma$v[,,3] <- tm[3,1] * V[,,1] + tm[3,2] * V[,,2] + tm[3,3] * V[,,3]
    } else {
        stop("adp type must be either \"rdi\" or \"nortek\" or \"sontek\"")
    }
    res$metadata$oce.coordinate <- "xyz"
    res$processing.log <- processing.log.add(res$processing.log,
                                             paste(deparse(match.call()), sep="", collapse=""))
    oce.debug(debug, "\b\b\b} # adp.beam.to.xyz()\n")
    res
}

xyz.to.enu.adp <- function(x, declination=0, debug=getOption("oce.debug"))
{
    debug <- if (debug > 0) 1 else 0
    oce.debug(debug, "\b\bxyz.to.enu.adp(x, declination=", declination, ", debug=", debug, ") {\n", sep="")
    if (!inherits(x, "adp"))
        stop("method is only for adp objects")
    if (x$metadata$oce.coordinate != "xyz")
        stop("input must be in xyz coordinates")
    
    res <- x
    heading <- res$data$ts$heading
    pitch <- res$data$ts$pitch
    roll <- res$data$ts$roll
    ## Case-by-case alteration of heading, pitch and roll, so we can use one formula for all.
    ## There are three instrument.type values, ("teledyn rdi", "nortek", and "sontek"), and
    ## three orientation values ("upward", "downward", and "sideward").
    if (1 == length(agrep("rdi", x$metadata$manufacturer, ignore.case=TRUE))) { # "teledyn rdi"
        oce.debug(debug, "instrument: Teledyne-RDI adcp\n")
        ## h/p/r and s/f/m from Clark Richards pers. comm. 2011-03-14, revised 2011-03-15
        if (res$metadata$orientation == "upward") {
            oce.debug(debug, "configuration: upward-looking\n")
            ## As an alternative to the next three lines, could just add 180 degrees to roll
            starboard <- -res$data$ma$v[,,1] # p11 "RDI Coordinate Transformation Manual" (July 1998)
            forward <- res$data$ma$v[,,2] # p11 "RDI Coordinate Transformation Manual" (July 1998)
            mast <- -res$data$ma$v[,,3] # p11 "RDI Coordinate Transformation Manual" (July 1998)
            oce.debug(debug, "S=-X; F=Y; M=-Z\n")
        } else if (res$metadata$orientation == "downward") {
            oce.debug(debug, "configuration: downward-looking\n")
            roll <- -roll
            starboard <- res$data$ma$v[,,1] # p11 "RDI Coordinate Transformation Manual" (July 1998)
            forward <- res$data$ma$v[,,2] # p11 "RDI Coordinate Transformation Manual" (July 1998)
            mast <- res$data$ma$v[,,3] # p11 "RDI Coordinate Transformation Manual" (July 1998)
            oce.debug(debug, "roll=-roll; S=X; F=Y; M=Z\n")
        } else {
            stop("need metadata$orientation='upward' or 'downward', not '",x$metadata$orientation,"'")
        }
    } else if (1 == length(agrep("nortek", x$metadata$manufacturer))) { # "nortek"
        oce.debug(debug, "Nortek adp\n")
        ## h/p/r and s/f/m from Clark Richards pers. comm. 2011-03-14
        heading <- heading - 90
        if (res$metadata$orientation == "upward") {
            oce.debug(debug, "configuration: upward-looking\n")
            pitch <- -pitch
            roll <- -roll
            starboard <- res$data$ma$v[,,1] 
            forward <- -res$data$ma$v[,,2]
            mast <- -res$data$ma$v[,,3]
        } else if (res$metadata$orientation == "downward") {
            oce.debug(debug, "configuration: downward-looking\n")
            pitch <- -pitch
            roll <- -roll
            starboard <- res$data$ma$v[,,1]
            forward <- res$data$ma$v[,,2]
            mast <- res$data$ma$v[,,3]
        } else {
            stop("need metadata$orientation='upward' or 'downward', not '",x$metadata$orientation,"'")
        }
    } else if (1 == length(agrep("sontek", x$metadata$manufacturer))) { # "sontek"
        oce.debug(debug, "Sontek adp\n")
        ## h/p/r and s/f/m mimic Sontek from Clark Richards pers. comm. 2011-03-14
        heading <- heading - 90
        if (res$metadata$orientation == "upward") {
            oce.debug(debug, "configuration: upward-looking\n")
            pitch <- (-pitch)
            roll <- (-roll)
            starboard <- res$data$ma$v[,,1] 
            forward <- -res$data$ma$v[,,2]
            mast <- -res$data$ma$v[,,3]
            oce.debug(debug, "heading=heading=90; roll=-roll; pitch=-pitch; S=X; F=-Y; M=-Z\n")
        } else if (res$metadata$orientation == "downward") {
            oce.debug(debug, "configuration: downward-looking\n")
            pitch <- (-pitch)
            roll <- (-roll)
            starboard <- res$data$ma$v[,,1]
            forward <- res$data$ma$v[,,2]
            mast <- res$data$ma$v[,,3]
        } else {
            stop("need metadata$orientation='upward' or 'downward', not '",x$metadata$orientation,"'")
        }
    } else {
        stop("unrecognized manufacturer; should be 'teledyne rdi', 'sontek', or 'nortek', but is '",
             x$metadata$manufacturer, "'")
    }
    oce.debug(debug, vector.show(heading, "heading"))
    oce.debug(debug, vector.show(pitch, "pitch"))
    oce.debug(debug, vector.show(roll, "roll"))
    np <- dim(x$data$ma$v)[1]           # number of profiles
    nc <- dim(x$data$ma$v)[2]           # number of cells
    ## ADP and ADV calculations are both handled by sfm_enu
    for (c in 1:nc) {
        enu <- .C("sfm_enu",
                  as.integer(length(x$data$ts$heading)), # need not equal np
                  as.double(heading + declination),
                  as.double(pitch),
                  as.double(roll), 
                  as.integer(np),
                  as.double(starboard[,c]),
                  as.double(forward[,c]),
                  as.double(mast[,c]),
                  east = double(np),
                  north = double(np),
                  up = double(np),
                  NAOK=TRUE,
                  PACKAGE="oce")
        res$data$ma$v[,c,1] <- enu$east
        res$data$ma$v[,c,2] <- enu$north
        res$data$ma$v[,c,3] <- enu$up
    }
    res$metadata$oce.coordinate <- "enu"
    res$processing.log <- processing.log.add(res$processing.log,
                                             paste(deparse(match.call()), sep="", collapse=""))
    oce.debug(debug, "\b\b\b} # xyz.to.enu.adp()\n")
    res
}

enu.to.other.adp <- function(x, heading=0, pitch=0, roll=0)
{
    if (!inherits(x, "adp"))
        stop("method is only for adp objects")
    if (x$metadata$oce.coordinate != "enu")
        stop("input must be in enu coordinates, but it is in ", x$metadata$oce.coordinate, " coordinates")
    res <- x
    np <- dim(x$data$ma$v)[1]           # number of profiles
    nc <- dim(x$data$ma$v)[2]           # number of cells
    for (c in 1:nc) {
        other <- .C("sfm_enu",
                    as.integer(length(heading)),
                    as.double(heading),
                    as.double(pitch),
                    as.double(roll), 
                    as.integer(np),
                    as.double(x$data$ma$v[,c,1]),
                    as.double(x$data$ma$v[,c,2]),
                    as.double(x$data$ma$v[,c,3]),
                    v1new = double(np),
                    v2new = double(np),
                    v3new = double(np),
                    NAOK=TRUE,
                    PACKAGE="oce")
        res$data$ma$v[,c,1] <- other$v1new
        res$data$ma$v[,c,2] <- other$v2new
        res$data$ma$v[,c,3] <- other$v3new
    }
    res$metadata$oce.coordinate <- "other"
    log.action <- paste(deparse(match.call()), sep="", collapse="")
    res$processing.log <- processing.log.add(res$processing.log,
                                             paste(deparse(match.call()), sep="", collapse=""))
    res
}

peek.ahead <- function(file, bytes=2, debug=!TRUE)
{
    pos <- seek(file)
    res <- readBin(file, "raw", n=bytes, size=1)
    oce.debug(debug, "peeked at", paste("0x", paste(res, sep=" "), sep=""), "\n")
    seek(file, pos)
    res
}

display.bytes <- function(b, label="", ...)
{
    n <- length(b)
    cat("\n", label, " (", n, "bytes)\n", sep="", ...)
    print(b, ...)
}
