## vim: tw=120 shiftwidth=4 softtabstop=4 expandtab:

clipmin <- function(x, min=0)
{
    ifelse(x < min, min, x)
}

paletteCalculations <- function(paletteSeparation=1/8, paletteWidth=1/4, label) # in inches
{
    omai <- par('mai')
    figureWidth <- par("fin")[1]
    lineHeight <- 1.5 * par("cin")[2]  # character height in inches (times a safety for superscripts etc)
    ticLength <- abs(par("tcl")) * lineHeight # inches (not sure on this)
    pc <- list()
    pc$marLHS <- omai[2]               # width of LHS margin for panel to left of palette
    pc$paletteSeparation <- paletteSeparation # space between palette and panel to its left
    pc$paletteWidth <- paletteWidth    # palette width
    pc$marRHS <- lineHeight + ticLength # space to right of palette
    if (!missing(label))
        pc$marRHS <- pc$marRHS + strheight(label)*par('cin')[1]
    pc$main <- figureWidth - pc$marLHS - pc$paletteSeparation - pc$paletteWidth - pc$marRHS # width of main panel
    pc$omai <- omai
    pc$figureWidth <- figureWidth      # total width of figure (main panel, palette, space between, and margins)
    pc 
}

drawPalette <- function(zlim,
                        zlab="",
                        breaks,
                        col,
                        top=0, bottom=0,
                        drawContours=FALSE,
                        debug=getOption("oceDebug"),
                        ...)
{
    debug <- min(1, max(debug, 0))
    zlimGiven <- !missing(zlim)
    breaksGiven <- !missing(breaks)
    if (zlimGiven)
        oceDebug(debug, "\b\bdrawPalette(zlim=c(", zlim[1], ",", zlim[2], "), zlab=", "\"", if (is.character(zlab)) zlab else
            "(expression)", "\", ...) {\n", sep="")
    else
        oceDebug(debug, "palette() with no arguments: set space to right of a graph\n")
    oceDebug(debug, if (breaksGiven) "gave breaks\n" else "did not give breaks\n")
    omai <- par("mai")
    oceDebug(debug, "original mai: omai=c(", paste(omai, sep=","), ")\n")
    omar <- par("mar")
    oceDebug(debug, "original mar: omar=c(", paste(omar, sep=","), ")\n")

    pc <- paletteCalculations()
    contours <- NULL
    if (zlimGiven) {
        if (breaksGiven) {
            breaksOrig <- breaks
        } else {
            zrange <- zlim
            if (missing(col)) {
                breaks <- pretty(zlim)
                contours <- breaks
            } else {
                if (is.function(col)) {
                    breaks <- seq(zlim[1], zlim[2], length.out=256) # smooth image colorscale
                    contours <- pretty(zlim)
                } else {
                    breaks <- seq(zlim[1], zlim[2], length.out=1+length(col))
                    contours <- seq(zlim[1], zlim[2], length.out=1+length(col))
                }
            }
            breaksOrig <- breaks
            breaks[1] <- zrange[1]
            breaks[length(breaks)] <- zrange[2]
        }
        if (missing(col))
            col <- oceColorsPalette(n=length(breaks)-1)
        if (is.function(col))
            col <- col(n=length(breaks)-1)
    }
    theMai <- c(pc$omai[1]+bottom,
                pc$main + pc$marLHS + pc$paletteSeparation,
                pc$omai[3]+top,
                pc$marRHS)
    oceDebug(debug, "setting  par(mai)=", format(theMai, digits=2), "\n")
    if (zlimGiven) {
        cat("theMai=", theMai, "\n")
        par(mai=theMai)
        if (!breaksGiven) {
            palette <- seq(zlim[1], zlim[2], length.out=300)
            image(x=1, y=palette, z=matrix(palette, nrow=1), axes=FALSE, xlab="", ylab="", col=col,
                  zlim=zlim)
        } else {
            palette <- seq(zlim[1], zlim[2], length.out=300)
            image(x=1, y=palette, z=matrix(palette, nrow=1), axes=FALSE, xlab="", ylab="",
                  breaks=breaksOrig,
                  col=col,
                  zlim=zlim)
        }
        if (drawContours)
            abline(h=contours)
        box()
        axis(side=4, at=if (is.null(contours)) contours else pretty(palette), mgp=c(2.5,0.7,0))
        if (nchar(zlab) > 0)
            mtext(zlab, side=4, line=2.0, cex=par('cex'))
    }
    theMai <- c(pc$omai[1],
                pc$marLHS,
                pc$omai[3],
                pc$paletteSeparation + pc$paletteWidth + pc$marRHS)
    oceDebug(debug, "original par(mai)=", format(omai, digits=2), "\n")
    oceDebug(debug, "setting  par(mai)=", format(theMai, digits=2), "\n")
    oceDebug(debug, "drawPalette orig mar=",par('mar'),"\n")
    if (zlimGiven)
        par(new=TRUE, mai=theMai)
    else
        par(mai=theMai)
    oceDebug(debug, "drawPalette at end mar=",par('mar'),"\n")
    oceDebug(debug, "\b\b} # drawPalette()\n")
    invisible()
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
    breaksGiven <- !missing(breaks)
    if (!breaksGiven) {
        zrange <- range(z, na.rm=TRUE)
        if (missing(zlim)) {
            if (missing(col)) {
                breaks <- pretty(zrange)
                if (breaks[1] < zrange[1]) breaks[1] <- zrange[1]
                if (breaks[length(breaks)] > zrange[2]) breaks[length(breaks)] <- zrange[2]
            } else {
                breaks <- seq(zrange[1], zrange[2], length.out=if(is.function(col))128 else 1+length(col))
            }
            breaksOrig <- breaks
        } else {
            if (missing(col))
                breaks <- c(zlim[1], pretty(zlim), zlim[2])
            else
                breaks <- seq(zlim[1], zlim[2], length.out=if(is.function(col))128 else 1+length(col))
            breaksOrig <- breaks
            ##cat('range(z):', zrange, '\n')
            ##cat('ORIG  range(breaks):', range(breaks), '\n')
            breaks[1] <- min(zrange[1], breaks[1])
            breaks[length(breaks)] <- max(breaks[length(breaks)], zrange[2])
            ##cat('later range(breaks):', range(breaks), '\n')
        }
    } else {
        breaksOrig <- breaks
    }
    if (missing(col))
        col <- oceColorsPalette(n=length(breaks)-1)
    if (is.function(col))
        col <- col(n=length(breaks)-1)

    if (drawPalette) {
        zlim <- if(missing(zlim)) range(z,na.rm=TRUE) else zlim
        drawPalette(zlim=zlim, zlab="", breaks=breaks, col=col, drawContours=drawContours)
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
            if (!breaksGiven) {
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
