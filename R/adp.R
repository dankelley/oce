use.new.imagep <- TRUE
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
                     type=c("rdi", "nortek", "sontek"), debug=getOption("oce.debug"), monitor=TRUE, log.action, ...)
{
    oce.debug(debug, "read.adp(...,from=",from,",to=",if (missing(to)) "(missing)" else to,",by=",by,"type=",type,",...)\n")
    type <- match.arg(type)
    if (monitor) cat(file, "\n", ...)
    if (type == "rdi")
        read.adp.rdi(file=file, from=from, to=to, by=by, tz=tz, debug=debug-1, monitor=monitor, log.action=log.action, ...)
    else if (type == "nortek")
        read.adp.nortek(file=file, from=from, to=to, by=by, tz=tz, debug=debug-1, monitor=monitor, log.action=log.action, ...)
    else if (type == "sontek")
        read.adp.sontek(file=file, from=from, to=to, by=by, tz=tz, debug=debug-1, monitor=monitor, log.action=log.action, ...)
}

summary.adp <- function(object, ...)
{
    if (!inherits(object, "adp")) stop("method is only for adp objects")
    if (is.null(object$metadata$have.actual.data) || object$metadata$have.actual.data) {
        if (inherits(object, "aquadopp")) {
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
        } else if (inherits(object, "rdi")) {
            res.specific <- list(number.of.data.types=object$metadata$number.of.data.types,
                                 heading.alignment=object$metadata$heading.alignment,
                                 heading.bias=object$metadata$heading.bias,
                                 bin1.distance=object$metadata$bin1.distance,
                                 xmit.pulse.length=object$metadata$xmit.pulse.length,
                                 oce.beam.attenuated=object$metadata$oce.beam.attenuated,
                                 beam.config=object$metadata$beam.config)
        } else if (inherits(object, "sontek")) {
            res.specific <- list(cpu.software.ver.num=object$metadata$cpu.software.ver.num,
                                 dsp.software.ver.num=object$metadata$dsp.software.ver.num,
                                 board.rev=object$metadata$board.rev,
                                 adp.type=object$metadata$adp.type,
                                 slant.angle=object$metadata$slant.angle,
                                 orientation=object$metadata$orientation)
        } else if (inherits(object, "nortek")) {
            res.specific <- NULL
        } else stop("can only summarize ADP objects of sub-type \"rdi\", \"sontek\", or \"nortek\", not class ", paste(class(object),collapse=","))
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
        res <- list(res.specific,
                    filename=object$metadata$filename,
                    instrument.type=object$metadata$instrument.type,
                    serial.number=object$metadata$serial.number,
                    measurement.start=object$metadata$measurement.start,
                    measurement.end=object$metadata$measurement.end,
                    measurement.deltat=object$metadata$measurement.deltat,
                    subsample.start=object$data$ts$time[1],
                    subsample.end.time=object$data$ts$time[length(object$data$ts$time)],
                    subsample.deltat=mean(diff(as.numeric(object$data$ts$time)),na.rm=TRUE),
                    distance=object$data$ss$distance,
                    metadata=object$metadata,
                    frequency=object$metadata$frequency,
                    number.of.profiles=v.dim[1],
                    number.of.cells=v.dim[2],
                    number.of.beams=v.dim[3],
                    number.of.data.types=object$metadata$number.of.data.type,
                    bin1.distance=object$metadata$bin1.distance,
                    cell.size=object$metadata$cell.size,
                    xmit.pulse.length=object$metadata$xmit.pulse.length,
                    oce.beam.attenuated=object$metadata$oce.beam.attenuated,
                    beam.angle=object$metadata$beam.angle,
                    beam.config=object$metadata$beam.config,
                    transformation.matrix=object$metadata$transformation.matrix,
                    orientation=object$metadata$orientation,
                    coordinate.system=object$metadata$coordinate.system,
                    oce.coordinate=object$metadata$oce.coordinate,
                    fives=fives,
                    time=object$data$ts$time,
                    processing.log=processing.log.summary(object))
    } else {
        res <- list(instrument.type=object$metadata$instrument.type,
                    filename=object$metadata$filename,
                    serial.number="unknown",
                    have.actual.data=FALSE)
    }
    class(res) <- "summary.adp"
    res
}                                       # summary.adp()

print.summary.adp <- function(x, digits=max(6, getOption("digits") - 1), ...)
{
    cat("ADP Summary\n-----------\n\n", ...)
    cat(paste("* Instrument:         ", x$instrument.type, ", serial number ``", paste(x$metadata$serial.number, collapse=""), "``\n", sep=""), ...)
    cat(paste("* Source:             ``", x$filename, "``\n", sep=""), ...)
    if (is.null(x$have.actual.data) || x$have.actual.data) {
        cat(sprintf("* Measurements:       %s %s to %s %s sampled at %.4g Hz\n",
                    format(x$measurement.start), attr(x$measurement.start, "tzone"),
                    format(x$measurement.end), attr(x$measurement.end, "tzone"),
                    1 / x$measurement.deltat), ...)
        cat(sprintf("* Subsample:          %s %s to %s %s sampled at %.4g Hz\n",
                    format(x$subsample.start), attr(x$subsample.start, "tzone"),
                    format(x$subsample.end),  attr(x$subsample.end, "tzone"),
                    1 / x$subsample.deltat), ...)
        ##cat("  Number of profiles:", x$number.of.profiles, "\n", ...)
        cat(sprintf("* Cells:              %d, centered at %.3f m to %.3f m, spaced by %.3f m\n",
                    x$number.of.cells, x$distance[1],  x$distance[length(x$distance)], diff(x$distance[1:2])),  ...)
        cat("* Coordinate system: ", x$coordinate.system, "[originally],", x$oce.coordinate, "[presently]\n", ...)
        cat("* Frequency:         ", x$frequency, "kHz\n", ...)
        cat("* Beams:             ", x$number.of.beams, if (x$oce.beam.attenuated) "beams (attenuated)" else "beams (not attenuated)",
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
        if (x$instrument.type == "teledyne rdi") {
            cat("* Teledyne-specific\n\n", ...)
            cat("  * System configuration:       ", x$metadata$system.configuration, "\n", ...)
            cat("  * Software version:           ", paste(x$metadata$program.version.major, x$metadata$program.version.minor, sep="."), "\n", ...)
            cat("  * CPU board serial number:    ", x$metadata$cpu.board.serial.number, "\n", ...)
            cat("  * Xmit pulse length:          ", x$metadata$xmit.pulse.length,"m\n", ...)
            cat("  * Beam pattern:               ", x$metadata$beam.pattern, "\n", ...)
            cat("  * Pings per ensemble:         ", x$metadata$pings.per.ensemble, "\n", ...)
            cat("  * Heading alignment:          ", x$metadata$heading.alignment, "\n", ...)
            cat("  * Heading bias:               ", x$metadata$heading.bias, "\n", ...)
        } else if (x$instrument.type == "nortek aquadopp high resolution") {
            cat("* Nortek-specific:\n\n", ...)
            cat("  * Internal code version:       ", x$metadata$internal.code.version, "\n", ...)
            cat("  * Hardware revision:           ", x$metadata$hardware.revision, "\n", ...)
            cat("  * Head serial number:          ", x$metadata$head.serial.number, "\n", ...)
        } else if (x$instrument.type == "sontek") {
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
        invisible(x)
    } else {
        cat("* No profiles in file\n")
        invisible(x)
    }
}

plot.adp <- function(x,
                     which=1:dim(x$data$ma$v)[3],
                     col,
                     zlim,
                     titles,
                     ytype=c("profile", "distance"),
                     adorn=NULL,
                     draw.time.range=getOption("oce.draw.time.range"),
                     mgp=getOption("oce.mgp"),
                     mar=c(mgp[1],mgp[1]+1,1,1.5),
                     margins.as.image=FALSE,
                     cex=1,
                     control,
                     use.layout=FALSE,  # FIXME: remove from arg list if imagep gets working
                     debug=getOption("oce.debug"),
                     ...)
{
    oce.debug(debug, "\n")
    oce.debug(debug, "Entering plot.adp()\n")
    oce.debug(debug, "  par(mar)=", paste(par('mar'), collapse=" "), "\n")
    oce.debug(debug, "  par(mai)=", paste(par('mai'), collapse=" "), "\n")
    if (!inherits(x, "adp")) stop("method is only for adp objects")
    if (!(is.null(x$metadata$have.actual.data) || x$metadata$have.actual.data)) {
        warning("there are no profiles in this dataset")
        return
    }
    opar <- par(no.readonly = TRUE)
    lw <- length(which)
    if (!missing(titles) && length(titles) != lw) stop("length of 'titles' must equal length of 'which'")
    oce.debug(debug, "length(which) =", lw, "\n")
    if (lw > 1) on.exit(par(opar))
    par(mgp=mgp, mar=mar, cex=cex)
    dots <- list(...)
    ytype <- match.arg(ytype)
    ytype <- match.arg(ytype)
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
    gave.ylim <- "ylim" %in% names(dots)
    ylim.given <- if (gave.ylim) dots[["ylim"]] else NULL

    oce.debug(debug, "later on in plot.adp:\n")
    oce.debug(debug, "  par(mar)=", paste(par('mar'), collapse=" "), "\n")
    oce.debug(debug, "  par(mai)=", paste(par('mai'), collapse=" "), "\n")


    images <- 1:12
    timeseries <- 13:22
    spatial <- 23:27
    speed <- 28
    if (any(!which %in% c(images, timeseries, spatial, speed))) stop("unknown value of 'which'")

    adorn.length <- length(adorn)
    if (adorn.length == 1) {
        adorn <- rep(adorn, lw)
        adorn.length <- lw
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
    if (use.layout) {
        if (any(which %in% images) || margins.as.image) {
            w <- 1.5
            lay <- layout(matrix(1:(2*lw), nrow=lw, byrow=TRUE), widths=rep(c(1, lcm(w)), lw))
            oce.debug(debug, "calling layout(matrix...)\n")
            oce.debug(debug, "using layout, since this is an image, or has margins as image\n")
        } else {
            if (lw != 1 || which != 23) {
                lay <- layout(cbind(1:lw))
                oce.debug(debug, "calling layout(cbind(1:", lw, ")\n")
                oce.debug(debug, "using layout\n")
            }
        }
    } else {
        if (use.new.imagep) {
            if (lw > 1) {
                par(mfrow=c(lw, 1))
                oce.debug(debug, "calling par(mfrow=c(", lw, ", 1)\n")
            }
        } else {
            stop("cannot have use.layout=FALSE unless use.new.imagep=TRUE")
        }
    }
    flip.y <- ytype == "profile" && x$metadata$orientation == "downward"
    for (w in 1:lw) {
        ##oce.debug(debug, "which[", w, "]=", which[w], "; draw.time.range=", draw.time.range, "\n")
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
                z <- as.numeric(x$data$ma$q[,,which[w]-8])
                dim(z) <- dim(x$data$ma$q)[1:2]
                zlim <- c(0, 100)
                zlab <- c(expression(q[1]),expression(q[2]),expression(q[3]))[which[w]-8]
            } else skip <- TRUE
            if (!skip) {
                ##oce.debug(debug, "which[", w, "]=", which[w], "; draw.time.range=", draw.time.range, " (just about to plot)\n")
                if (use.new.imagep) {
                    imagepnew(x=tt, y=x$data$ss$distance, z=z,
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
                              cex=cex*(1 - min(lw / 8, 1/4)), # FIXME: should emulate par(mfrow)
                              debug=debug-1,
                              ...)
                } else {
                    imagep(x=tt, y=x$data$ss$distance, z=z,
                           zlim=zlim,
                           flip.y=flip.y,
                           col=if (missing(col)) oce.colors.palette(128, 1) else col,
                           ylab=resizable.label("distance"),
                           xlab="Time",
                           zlab=zlab,
                           draw.time.range=draw.time.range,
                           draw.contours=FALSE,
                           do.layout=FALSE,
                           adorn=adorn[w],
                           mgp=mgp,
                           mar=mar,
                           cex=1,
                           debug=debug-1,
                           ...)
                }
                draw.time.range <- FALSE
            }
        } else if (which[w] %in% timeseries) { # time-series types
            par(mgp=mgp, mar=mar, cex=cex)
            if (which[w] == 13)
                oce.plot.ts(x$data$ts$time, x$data$ts$salinity,    ylab=resizable.label("S"),       type='l',
                            draw.time.range=draw.time.range, adorn=adorn[w])
            if (which[w] == 14)
                oce.plot.ts(x$data$ts$time, x$data$ts$temperature, ylab= expression(paste("T [ ", degree, "C ]")), type='l',
                            draw.time.range=draw.time.range, adorn=adorn[w])
            if (which[w] == 15)
                oce.plot.ts(x$data$ts$time, x$data$ts$pressure,    ylab=resizable.label("p"),       type='l',
                            draw.time.range=draw.time.range, adorn=adorn[w])
            if (which[w] == 16)
                oce.plot.ts(x$data$ts$time, x$data$ts$heading,     ylab=resizable.label("heading"), type='l',
                            draw.time.range=draw.time.range, adorn=adorn[w])
            if (which[w] == 17)
                oce.plot.ts(x$data$ts$time, x$data$ts$pitch,       ylab=resizable.label("pitch"),   type='l',
                            draw.time.range=draw.time.range, adorn=adorn[w])
            if (which[w] == 18)
                oce.plot.ts(x$data$ts$time, x$data$ts$roll,        ylab=resizable.label("roll"),    type='l',
                            draw.time.range=draw.time.range, adorn=adorn[w])
            if (which[w] == 19) {
                if (x$metadata$number.of.beams > 0)
                    oce.plot.ts(x$data$ts$time, apply(x$data$ma$v[,,1], 1, mean, na.rm=TRUE),
                                ylab=ad.beam.name(x, 1),
                                type='l', draw.time.range=draw.time.range, cex.axis=cex,
                                adorn=adorn[w], ...)
                else warning("cannot plot beam/velo 1 because the device no beams")
            }
            if (which[w] == 20) {
                if (x$metadata$number.of.beams > 1)
                    oce.plot.ts(x$data$ts$time, apply(x$data$ma$v[,,2], 1, mean, na.rm=TRUE),
                                ylab=ad.beam.name(x, 2),
                                type='l', draw.time.range=draw.time.range,
                                adorn=adorn[w], ...)
                else warning("cannot plot beam/velo 2 because the device has only", x$metadata$number.of.beams, "beams")
            }
            if (which[w] == 21) {
                if (x$metadata$number.of.beams > 2)
                    oce.plot.ts(x$data$ts$time, apply(x$data$ma$v[,,3], 1, mean, na.rm=TRUE),
                                ylab=ad.beam.name(x, 3),
                                type='l', draw.time.range=draw.time.range,
                                adorn=adorn[w], ...)
                else warning("cannot plot beam/velo 3 because the device has only", x$metadata$number.of.beams, "beams")
            }
            if (which[w] == 22) {
                if (x$metadata$number.of.beams > 3)
                    oce.plot.ts(x$data$ts$time, apply(x$data$ma$v[,,4], 1, mean, na.rm=TRUE),
                                ylab=ad.beam.name(x, 4),
                                type='l', draw.time.range=draw.time.range,
                                adorn=adorn[w], ...)
                else warning("cannot plot beam/velo 4 because the device has only", x$metadata$number.of.beams, "beams")
            }
            draw.time.range <- FALSE
            if (margins.as.image)  {
                ## blank plot, to get axis length same as for images
                omar <- par("mar")
                par(mar=c(mar[1], 1/4, mgp[2]+1/2, mgp[2]+1))
                plot(1:2, 1:2, type='n', axes=FALSE, xlab="", ylab="")
                par(mar=omar)
            }
        } else if (which[w] %in% spatial) {                   # various spatial types
            if (which[w] == 23) {                             # progressive-vector
                par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
                dt <- as.numeric(difftime(x$data$ts$time[2], x$data$ts$time[1],units="sec")) # FIXME: should not assume all equal
                m.per.km <- 1000
                if (!missing(control) && !is.null(control$bin)) {
                    if (control$bin < 1) stop("cannot have control$bin less than 1, but got ", control$bin)
                    max.bin <- dim(x$data$ma$v)[2]
                    if (control$bin > max.bin) stop("cannot have control$bin larger than ", max.bin," but got ", control$bin)
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
                } else warning("cannot use which=27 because this device did not have 4 beams")
            }
            if (w <= adorn.length) {
                t <- try(eval(adorn[w]), silent=TRUE)
                if (class(t) == "try-error") warning("cannot evaluate adorn[", w, "]\n")
            }
        } else if (which[w] %in% speed) { # various speed types
            if (which[w] == 28) {         # current ellipse
                par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
                if (!missing(control) && !is.null(control$bin)) {
                    if (control$bin < 1) stop("cannot have control$bin less than 1, but got ", control$bin)
                    max.bin <- dim(x$data$ma$v)[2]
                    if (control$bin > max.bin) stop("cannot have control$bin larger than ", max.bin," but got ", control$bin)
                    u <- x$data$ma$v[,control$bin,1]
                    v <- x$data$ma$v[,control$bin,2]
                } else {
                    u <- apply(x$data$ma$v[,,1], 1, mean, na.rm=TRUE)
                    v <- apply(x$data$ma$v[,,2], 1, mean, na.rm=TRUE)
                }
                if ("type" %in% names(dots)) {
                    plot(u, v, xlab="u [m/s]", ylab="v [m/s]", asp=1, col=if (missing(col)) "black" else col, ...)
                } else {
                    plot(u, v, xlab="u [m/s]", ylab="v [m/s]", type='n', asp=1, ...)
                    points(u, v, cex=cex/3, col=if (missing(col)) "black" else col)
                }
            }
            if (w <= adorn.length) {
                t <- try(eval(adorn[w]), silent=TRUE)
                if (class(t) == "try-error") warning("cannot evaluate adorn[", w, "]\n")
            }
        }
    }
}

adp.beam.attenuate <- function(x, count2db=c(0.45, 0.45, 0.45, 0.45))
{
    if (!inherits(x, "adp")) stop("method is only for adp objects")
    if (x$metadata$oce.beam.attenuated) stop("the beams are already attenuated in this dataset")
    res <- x
    num.profiles <- dim(x$data$ma$a)[1]
    correction <- matrix(rep(20 * log10(x$data$ss$distance), num.profiles),
                         nrow=num.profiles, byrow=TRUE)
    for (beam in 1:x$metadata$number.of.beams)
        res$data$ma$a[,,beam] <- as.raw(count2db[1] * as.numeric(x$data$ma$a[,,beam]) + correction)
    res$metadata$oce.beam.attenuated <- TRUE
    log.action <- paste(deparse(match.call()), sep="", collapse="")
    processing.log.append(res, log.action)
}

adp.beam2xyz <- function(x, debug=getOption("oce.debug"))
{
    if (!inherits(x, "adp")) stop("method is only for objects of class \"adp\"")
    if (x$metadata$oce.coordinate != "beam") stop("input must be in beam coordinates")
    if (inherits(x, "rdi")) {
        if (x$metadata$number.of.beams != 4) stop("can only handle 4-beam ADP units from RDI")
        res <- x
        if (!is.null(x$metadata$transformation.matrix)) {
            tm <- x$metadata$transformation.matrix
        } else {
            tm <- matrix(c(-1.9318517,  1.9318517,  0.0000000,  0.0000000,
                           0.0000000 ,  0.0000000, -1.9318517,  1.9318517,
                           -0.2588190, -0.2588190, -0.2588190, -0.2588190,
                           1.3660254 ,  1.3660254, -1.3660254, -1.3660254), nrow=4, byrow=TRUE)
            warning("adp.beam2xyz() detected no metadata$transformation.matrix, so assuming the following:")
            print(tm)
        }
        if (x$metadata$orientation == "upward") { # change sign of u and w, since RDI is pointing upward
            tm[1,] <- -tm[1,]
            tm[3,] <- -tm[3,]
        }
        res$data$ma$v[,,1] <- tm[1,1] * x$data$ma$v[,,1] + tm[1,2] * x$data$ma$v[,,2] + tm[1,3] * x$data$ma$v[,,3] + tm[1,4] * x$data$ma$v[,,4]
        res$data$ma$v[,,2] <- tm[2,1] * x$data$ma$v[,,1] + tm[2,2] * x$data$ma$v[,,2] + tm[2,3] * x$data$ma$v[,,3] + tm[2,4] * x$data$ma$v[,,4]
        res$data$ma$v[,,3] <- tm[3,1] * x$data$ma$v[,,1] + tm[3,2] * x$data$ma$v[,,2] + tm[3,3] * x$data$ma$v[,,3] + tm[3,4] * x$data$ma$v[,,4]
        res$data$ma$v[,,4] <- tm[4,1] * x$data$ma$v[,,1] + tm[4,2] * x$data$ma$v[,,2] + tm[4,3] * x$data$ma$v[,,3] + tm[4,4] * x$data$ma$v[,,4]
    } else if (inherits(x, "nortek")) {
        if (x$metadata$number.of.beams != 3) stop("can only handle 3-beam ADP units from nortek")
        res <- x
        if (!is.null(x$metadata$transformation.matrix)) {
            tm <- x$metadata$transformation.matrix
            if (FALSE) {                                    # FIXME: decide whether to modify transformation matrix here
                if (x$metadata$orientation == "downward") { # flip sign of rows 2 and 3
                    ## http://woodshole.er.usgs.gov/pubs/of2005-1429/MFILES/AQDPTOOLS/beam2enu.m
                    tm[2,] <- -tm[2,]       # FIXME: shouldn't this be done in read.adp.nortek() ?
                    tm[3,] <- -tm[3,]
                } else if (x$metadata$orientation != "upward")
                    stop("beam orientation must be \"upward\" or \"downward\", but is \"", x$metadata$orientation, "\"")
            }
            res$data$ma$v[,,1] <- tm[1,1] * x$data$ma$v[,,1] + tm[1,2] * x$data$ma$v[,,2] + tm[1,3] * x$data$ma$v[,,3]
            res$data$ma$v[,,2] <- tm[2,1] * x$data$ma$v[,,1] + tm[2,2] * x$data$ma$v[,,2] + tm[2,3] * x$data$ma$v[,,3]
            res$data$ma$v[,,3] <- tm[3,1] * x$data$ma$v[,,1] + tm[3,2] * x$data$ma$v[,,2] + tm[3,3] * x$data$ma$v[,,3]
        } else {
            stop("adp.beam2xyz() needs metadata$transformation.matrix")
        }
    } else if (inherits(x, "sontek")) {
        if (x$metadata$number.of.beams != 3) stop("can only handle 3-beam ADP units from sontek")
        res <- x
        if (!is.null(x$metadata$transformation.matrix)) {
            tm <- x$metadata$transformation.matrix
        } else {
            tm <- matrix(c(1.577, -0.789, -0.789,
                           0.000, -1.366,  1.366,
                           0.368,  0.368,  0.368), nrow=4, byrow=TRUE)
            warning("adp.beam2xyz() detected no metadata$transformation.matrix, so assuming the following:")
            print(tm)
        }
        res$data$ma$v[,,1] <- tm[1,1] * x$data$ma$v[,,1] + tm[1,2] * x$data$ma$v[,,2] + tm[1,3] * x$data$ma$v[,,3]
        res$data$ma$v[,,2] <- tm[2,1] * x$data$ma$v[,,1] + tm[2,2] * x$data$ma$v[,,2] + tm[2,3] * x$data$ma$v[,,3]
        res$data$ma$v[,,3] <- tm[3,1] * x$data$ma$v[,,1] + tm[3,2] * x$data$ma$v[,,2] + tm[3,3] * x$data$ma$v[,,3]
    } else {
        stop("adp type must be either \"rdi\" or \"nortek\" or \"sontek\"")
    }
    res$metadata$oce.coordinate <- "xyz"
    log.action <- paste(deparse(match.call()), sep="", collapse="")
    processing.log.append(res, log.action)
}

adp.xyz2enu <- function(x, debug=getOption("oce.debug"))
{
    if (!inherits(x, "adp")) stop("method is only for adp objects")
    if (x$metadata$oce.coordinate != "xyz") stop("input must be in xyz coordinates")
    res <- x
    heading <- res$data$ts$heading
    pitch <- res$data$ts$pitch
    roll <- res$data$ts$roll
    if (x$metadata$instrument.type == "teledyne rdi") {
        heading <- heading + 2 * res$metadata$heading.bias
        if (res$metadata$orientation == "upward")
            roll <- roll + 180
    }
    to.radians <- pi / 180
    CH <- cos(to.radians * heading)
    SH <- sin(to.radians * heading)
    CP <- cos(to.radians * pitch)
    SP <- sin(to.radians * pitch)
    CR <- cos(to.radians * roll)
    SR <- sin(to.radians * roll)
    np <- dim(x$data$ma$v)[1]
    nc <- dim(x$data$ma$v)[2]
    tr.mat <- array(dim=c(3, 3, np))
    tr.mat[1,1,] <-  CH * CR + SH * SP * SR
    tr.mat[1,2,] <-  SH * CP
    tr.mat[1,3,] <-  CH * SR - SH * SP * CR
    tr.mat[2,1,] <- -SH * CR + CH * SP * SR
    tr.mat[2,2,] <-  CH * CP
    tr.mat[2,3,] <- -SH * SR - CH * SP * CR
    tr.mat[3,1,] <- -CP * SR
    tr.mat[3,2,] <-  SP
    tr.mat[3,3,] <-  CP * CR
    rotated <- array(unlist(lapply(1:np, function(p) tr.mat[,,p] %*% t(x$data$ma$v[p,,1:3]))), dim=c(3, nc, np))
    res$data$ma$v[,,1] <- t(rotated[1,,])
    res$data$ma$v[,,2] <- t(rotated[2,,])
    res$data$ma$v[,,3] <- t(rotated[3,,])
    res$metadata$oce.coordinate <- "enu"
    log.action <- paste(deparse(match.call()), sep="", collapse="")
    processing.log.append(res, log.action)
}

adp.enu2other <- function(x, heading=0, pitch=0, roll=0)
{
    if (!inherits(x, "adp")) stop("method is only for adp objects")
    if (x$metadata$oce.coordinate != "enu") stop("input must be in enu coordinates, but it is in ", x$metadata$oce.coordinate, " coordinates")
    res <- x
    to.radians <- pi / 180
    CH <- cos(to.radians * heading)
    SH <- sin(to.radians * heading)
    CP <- cos(to.radians * pitch)
    SP <- sin(to.radians * pitch)
    CR <- cos(to.radians * roll)
    SR <- sin(to.radians * roll)
    tr.mat <- matrix(c( CH * CR + SH * SP * SR,  SH * CP,  CH * SR - SH * SP * CR,
                       -SH * CR + CH * SP * SR,  CH * CP, -SH * SR - CH * SP * CR,
                       -CP * SR,                 SP,       CP * CR),               nrow=3, byrow=TRUE)
    np <- dim(x$data$ma$v)[1]
    nc <- dim(x$data$ma$v)[2]
    rotated <- array(unlist(lapply(1:np, function(p) tr.mat %*% t(x$data$ma$v[p,,1:3]))), dim=c(3, nc, np))
    res$data$ma$v[,,1] <- t(rotated[1,,])
    res$data$ma$v[,,2] <- t(rotated[2,,])
    res$data$ma$v[,,3] <- t(rotated[3,,])
    res$metadata$oce.coordinate <- "other"
    log.action <- paste(deparse(match.call()), sep="", collapse="")
    processing.log.append(res, log.action)
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
