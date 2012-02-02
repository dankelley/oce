## vim: tw=120 shiftwidth=4 softtabstop=4 expandtab:
clipmin <- function(x, min=0)
{
    ifelse(x < min, min, x)
}

imagep <- function(x, y, z,
                   xlim, ylim, zlim,
                   flip.y=FALSE,
                   xlab="", ylab="", zlab="",
                   breaks, col,
                   drawContours=FALSE,
                   drawTimeRange=getOption("oceDrawTimeRange"),
                   drawPalette=TRUE,
                   filledContour=FALSE,
                   mgp=getOption("oceMgp"),
                   mar=c(mgp[1]+if(nchar(xlab)>0) 1.5 else 1, mgp[1]+if(nchar(ylab)>0) 1.5 else 1, mgp[2]+1/2, 1/2),
                   xaxs = "i", yaxs = "i",
                   cex=par("cex"),
                   adorn,
                   axes=TRUE,
                   main="",
                   debug=getOption("oceDebug"),
                   ...)
{
    oceDebug(debug, "\b\bimagep() {\n")
    oceDebug(debug, paste("  xlab='", xlab, "'; ylab='", ylab, "'; zlab='", zlab, "'\n", sep=""))
    oceDebug(debug, "  par(mar)=", paste(par('mar'), collapse=" "), "\n")
    oceDebug(debug, "  par(mai)=", paste(par('mai'), collapse=" "), "\n")
    if (!missing(x) && is.list(x)) {
        names <- names(x)
        if (!missing(y))
            stop("may not give y, since x is a list")
        if (!missing(z))
            stop("may not give z, since x is a list")
        if (!("x" %in% names))
            stop("since x is a list, it must have an item named 'x'")
        if (!("y" %in% names))
            stop("since x is a list, it must have an item named 'y'")
        if (!("z" %in% names))
            stop("since x is a list, it must have an item named 'z'")
        y <- x$y
        z <- x$z
        x <- x$x
    } else if (!missing(x) && is.matrix(x)) {
        z <- x
        y <- seq(0, 1, length.out=ncol(x))
        x <- seq(0, 1, length.out=nrow(x))
    } else if (!missing(z) && is.matrix(z) && missing(x) && missing(y)) {
        x <- seq(0, 1, length.out=nrow(z))
        y <- seq(0, 1, length.out=ncol(z))
        z <- z
    } else {
        if (missing(y))
            stop("must supply y")
        if (missing(z))
            stop("must supply z")
    }
    dim <- dim(z)
    if (nrow(z) != length(x) && (1+nrow(z)) != length(x))
        stop("image width (", ncol(z), ") does not match length of x (", length(x), ")")
    if (ncol(z) != length(y) && (1+ncol(z)) != length(y))
        stop("image height (", nrow(z), ") does not match length of y (", length(y), ")")

    ## ensure that x and y increase (BUT, for now, no check on equal values, and also no xlim reversals FIXME)
    ox <- order(x)
    if (any(diff(ox) < 0)) {
        warning("reordered some x values")
        x <- x[ox]
        z <- z[ox, ]
    }
    oy <- order(y)
    if (any(diff(oy) < 0)) {
        warning("reordered some x values")
        y <- y[oy]
        z <- z[,oy]
    }

    omai <- par("mai")
    omar <- par("mar")
    ocex <- par("cex")
    ## set overall graphical parameters (note: get opai after setting mar)
    par(mgp=mgp, mar=mar, cex=cex)
    omai <- par("mai")
    device.width <- par("fin")[1]      # FIXME: should call this figureWidth
    oceDebug(debug, sprintf("paper width: %.2f inches\n", device.width))
    pc <- paletteCalculations()
    if (debug > 0)
        str(pc)
    gave.breaks <- !missing(breaks)
    if (!gave.breaks) {
        zrange <- range(z, na.rm=TRUE)
        if (missing(zlim)) {
            if (missing(col)) {
                breaks <- pretty(zrange)
                if (breaks[1] < zrange[1]) breaks[1] <- zrange[1]
                if (breaks[length(breaks)] > zrange[2]) breaks[length(breaks)] <- zrange[2]
            } else {
                breaks <- seq(zrange[1], zrange[2], length.out=if(is.function(col))128 else 1+length(col))
            }
            breaks.orig <- breaks
        } else {
            if (missing(col))
                breaks <- c(zlim[1], pretty(zlim), zlim[2])
            else
                breaks <- seq(zlim[1], zlim[2], length.out=if(is.function(col))128 else 1+length(col))
            breaks.orig <- breaks
            ##cat('range(z):', zrange, '\n')
            ##cat('ORIG  range(breaks):', range(breaks), '\n')
            breaks[1] <- min(zrange[1], breaks[1])
            breaks[length(breaks)] <- max(breaks[length(breaks)], zrange[2])
            ##cat('later range(breaks):', range(breaks), '\n')
        }
    } else {
        breaks.orig <- breaks
    }
    if (missing(col))
        col <- oceColorsPalette(n=length(breaks)-1)
    if (is.function(col))
        col <- col(n=length(breaks)-1)

    if (TRUE == drawPalette || "space" == drawPalette) {
        the.mai <- c(pc$omai[1],
                     pc$main + pc$marLHS + pc$paletteSeparation,
                     pc$omai[3],
                     pc$marRHS)
        oceDebug(debug, "PALETTE: setting  par(mai)=", format(the.mai, digits=2), " (before clipping)\n")
        the.mai <- clipmin(the.mai, 0.1) # do not let palettes collide if mar[1] or mar[3] are zero
        oceDebug(debug, "PALETTE: setting  par(mai)=", format(the.mai, digits=2), " (after clipping)\n")
        par(mai=the.mai, cex=cex)
        if (TRUE == drawPalette) {
            if (!gave.breaks) {
                if (missing(zlim)) {
                    palette <- seq(min(z, na.rm=TRUE), max(z, na.rm=TRUE), length.out=300)
                } else {
                    palette <- seq(zlim[1], zlim[2], length.out=300)
                }
                image(x=1, y=palette, z=matrix(palette, nrow=1), axes=FALSE, xlab="", ylab="",
                      breaks=breaks,
                      col=col,
                      zlim=if(missing(zlim))range(z,na.rm=TRUE) else zlim)
            } else {
                if (missing(zlim)) {
                    palette <- seq(breaks[1], breaks[length(breaks)], length.out=300)
                } else {
                    palette <- seq(zlim[1], zlim[2], length.out=300)
                }
                image(x=1, y=palette, z=matrix(palette, nrow=1), axes=FALSE, xlab="", ylab="",
                      breaks=breaks.orig,
                      col=col,
                      zlim=if(missing(zlim))range(z,na.rm=TRUE) else zlim)
            }
            if (drawContours)
                abline(h=breaks)
            box()
            axis(side=4, at=pretty(palette), cex.axis=cex) # FIXME: decide on font size
        } else {
            plot(x=c(0,1), y=c(0,1), type='n', axes=FALSE, xlab="", ylab="")
        }
    }

    ## main image
    if (TRUE == drawPalette || "space" == drawPalette) {
        the.mai <- c(pc$omai[1],
                     pc$marLHS,
                     pc$omai[3],
                     pc$paletteSeparation + pc$paletteWidth + pc$marRHS)
        the.mai <- clipmin(the.mai, 0)         # just in case
        if (debug > 0)
            str(pc)
        oceDebug(debug, "original value of par(mai)=", format(omai, digits=2), "\n")
        oceDebug(debug, "MAIN: setting     par(mai)=", format(the.mai, digits=2), "\n")
        ##browser()
        par(new=TRUE, mai=the.mai, cex=cex)
    }

    x.is.time <- inherits(x, "POSIXt") || inherits(x, "POSIXct") || inherits(x, "POSIXlt")
    xlim <- if (missing(xlim)) range(x,na.rm=TRUE) else xlim
    ylim <- if (missing(ylim)) range(y,na.rm=TRUE) else ylim
    zlim <- if (missing(zlim)) range(z,na.rm=TRUE) else zlim
    if (x.is.time) {
        if (filledContour) {
            if (!is.double(z))
                storage.mode(z) <- "double"
            plot.new()
            plot.window(xlim=xlim, ylim=ylim, xaxs=xaxs, yaxs=yaxs)
            .Internal(filledcontour(as.double(x), as.double(y), z, as.double(breaks), col=col))
            mtext(ylab, side=2, line=par('mgp')[1])
        } else {
            if (!gave.breaks) {
                image(x=x, y=y, z=z, axes=FALSE, xlab="", ylab=ylab, col=col,
                      xlim=xlim, ylim=ylim, zlim=zlim, ...)
            } else {
                image(x=x, y=y, z=z, axes=FALSE, xlab="", ylab=ylab, breaks=breaks, col=col,
                      xlim=xlim, ylim=ylim, zlim=zlim, ...)
            }
        }
        box()
        if (axes) {
            oce.axis.POSIXct(side=1, x=x, cex=cex, cex.axis=cex, cex.lab=cex, drawTimeRange=drawTimeRange, mar=mar, mgp=mgp)
            axis(2, cex.axis=cex, cex.lab=cex)
        }
    } else {                           # x is not a POSIXt
        if (filledContour) {
            storage.mode(z) <- "double"
            plot.new()
            plot.window(xlim=xlim, ylim=ylim, xaxs=xaxs, yaxs=yaxs)
            .Internal(filledcontour(as.double(x), as.double(y), z, as.double(breaks), col=col))
        } else {
            image(x=x, y=y, z=z, axes=FALSE, xlab=xlab, ylab=ylab, breaks=breaks, col=col,
                  xlim=xlim, ylim=ylim, ...)
            box()
        }
        if (axes) {
            axis(1, cex.axis=cex, cex.lab=cex)
            axis(2, cex.axis=cex, cex.lab=cex)
        }
    }
    if (main != "")
        mtext(main, at=mean(range(x), na.rm=TRUE), side=3, line=1/8, cex=par("cex"))
    if (drawContours)
        contour(x=x, y=y, z=z, levels=breaks, drawlabels=FALSE, add=TRUE, col="black")
    mtext(zlab, side=3, cex=par("cex"), adj=1, line=1/8)
    if (!missing(adorn)) {
        t <- try(eval.parent(adorn), silent=!TRUE)
        if (class(t) == "try-error")
            warning("cannot evaluate adorn='", adorn, "'\n")
    }
    par(cex=ocex)
    oceDebug(debug, "\b\b} # imagep()\n")
    invisible()
}
