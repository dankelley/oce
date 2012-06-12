## vim: tw=120 shiftwidth=4 softtabstop=4 expandtab:

prettyLocal <- function(x, digits=10)
{
    round(pretty(x), digits)
}

clipmin <- function(x, min=0)
{
    ifelse(x < min, min, x)
}

abbreviateTimeLabels <- function(t, ...)
{
    if (!inherits(t, "POSIXt"))
        return(t)                     # not a time, so just return the argument
    dots <- list(...)
    if (!is.na(dots$format))
        return(format(t, dots$format)) # a format was specified, so just return the argument
    t <- format(t, "%Y-%m-%d %H:%M:%S")
    n <- length(t)
    if (n < 2)
        return(t)
    year <- substr(t, 1, 4)
    ## strip years, if all the same
    for (i in 2:n) if (year[i] != year[1]) return(t)
    t <- substr(t, 6, nchar(t))
    ## strip months, if all the same
    month <- substr(t, 1, 2)
    for (i in 2:n) if (month[i] != month[1]) return(t)
    t <- substr(t, 4, nchar(t))
    ## strip seconds, if all the same
    seconds <- substr(t, nchar(t)-2, nchar(t))
    for (i in 2:n) if (seconds[i] != seconds[1]) return(t)
    t <- substr(t, 1, nchar(t)-3)
    ## strip minutes, if all the same (this may be confusing)
    minutes <- substr(t, nchar(t)-2, nchar(t))
    for (i in 2:n) if (minutes[i] != minutes[1]) return(t)
    t <- substr(t, 1, nchar(t)-3)
    return(t)
}

paletteCalculations <- function(paletteSeparation=1/8, paletteWidth=1/4, label,
                                mai=c(0, 1/8, 0, 1/4))
{
    omai <- par('mai')
    figureWidth <- par("fin")[1]
    lineHeight <- 1.5 * par("cin")[2]  # character height in inches (times a safety for superscripts etc)
    ticLength <- abs(par("tcl")) * lineHeight # inches (not sure on this)
    pc <- list()
    pc$maiLHS <- omai[2]               # width of LHS margin for panel to left of palette
    ##pc$paletteSeparation <- paletteSeparation # space between palette and panel to its left
    pc$paletteSeparation <- mai[2]     # space between palette and panel to its left
    pc$paletteWidth <- paletteWidth    # palette width
    #pc$marRHS <- lineHeight + ticLength # space to right of palette
    pc$maiRHS <- mai[4]                # space to right of palette
    ##if (!missing(label))
    ##    pc$marRHS <- pc$marRHS + strheight(label)*par('cin')[1]
    pc$main <- figureWidth - pc$maiLHS - pc$paletteSeparation - pc$paletteWidth - pc$maiRHS # width of main panel
    pc$omai <- omai
    pc$figureWidth <- figureWidth      # total width of figure (main panel, palette, space between, and margins)
    pc 
}

drawPalette <- function(zlim,
                        zlab="",
                        breaks,
                        col,
                        labels=NULL,
                        at=NULL,
                        mai=c(0, 1/8, 0, 3/8),
                        top=0, bottom=0,
                        drawContours=FALSE,
                        debug=getOption("oceDebug"),
                        ...)
{
    if (top != 0) {
        warning("drawPalette(): please use mai instead of top\n")
        mai[3] <- top
    }
    if (bottom != 0) {
        warning("drawPalette(): please use mai instead of bottom\n")
        mai[1] <- bottom
    }
    zlimGiven <- !missing(zlim)
    breaksGiven <- !missing(breaks)
    if (zlimGiven)
        oceDebug(debug, "\bdrawPalette(zlim=c(", zlim[1], ",", zlim[2], "), zlab=", "\"", if (is.character(zlab)) zlab else
            "(expression)", "\", ...) {\n", sep="")
    else
        oceDebug(debug, "palette() with no arguments: set space to right of a graph\n")
    zIsTime <- zlimGiven && inherits(zlim[1], "POSIXt")
    if (zIsTime) {
        zlimOrig <- zlim
        zlim <- as.numeric(zlim)
    }
    oceDebug(debug, if (breaksGiven) "gave breaks\n" else "did not give breaks\n")
    oceDebug(debug, "zIsTime=", zIsTime, "\n")
    omai <- par("mai")
    oceDebug(debug, "original mai: omai=c(", paste(format(omai, digits=3), collapse=","), ")\n")
    omar <- par("mar")
    oceDebug(debug, "original mar: omar=c(", paste(format(omar, digits=3), collapse=","), ")\n")

    pc <- paletteCalculations(mai=mai)

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
##    theMai <- c(pc$omai[1]+mai[1],
##                pc$main + pc$marLHS + pc$paletteSeparation + mai[2],
##                pc$omai[3]+mai[3],
##                pc$marRHS+mai[4])
    theMai <- c(pc$omai[1]+mai[1],
                pc$main + pc$maiLHS + mai[2],
                pc$omai[3]+mai[3],
                pc$maiRHS)
    oceDebug(debug, "setting par(mai)=", format(theMai, digits=2), "\n")
    if (zlimGiven) {
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
        if (zIsTime & is.null(at)) {
            at <- as.numeric(pretty(zlim))
        } else if (is.null(at)) {
            at <- if (!is.null(contours) & is.null(at)) prettyLocal(contours) else prettyLocal(palette) # FIXME: wrong on contours
        }
        if (is.null(labels)) labels <- if (zIsTime) abbreviateTimeLabels(numberAsPOSIXct(at), ...) else format(at)
        axis(side=4, at=at, labels=labels, mgp=c(2.5,0.7,0))
        if (nchar(zlab) > 0)
            mtext(zlab, side=4, line=2.0, cex=par('cex'))
    }
    theMai <- c(pc$omai[1],
                pc$maiLHS,
                pc$omai[3],
                pc$paletteSeparation + pc$paletteWidth + pc$maiRHS)
    oceDebug(debug, "original par(mai)=c(", paste(format(omai, digits=3), collapse=","), ")\n")
    oceDebug(debug, "setting  par(mai)=c(", paste(format(theMai, digits=3), collapse=","), ")\n")
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
                   flipy=FALSE,
                   xlab="", ylab="", zlab="",
                   breaks, col,
                   labels=NULL, at=NULL,
                   drawContours=FALSE,
                   drawTimeRange=getOption("oceDrawTimeRange"),
                   drawPalette=TRUE,
                   filledContour=FALSE,
                   missingColor=NULL,
                   mgp=getOption("oceMgp"),
                   mar=c(mgp[1]+if(nchar(xlab)>0) 1.5 else 1, mgp[1]+if(nchar(ylab)>0) 1.5 else 1, mgp[2]+1/2, 1/2),
                   mai.palette=c(0, 1/8, 0, 3/8),
                   xaxs = "i", yaxs = "i",
                   cex=par("cex"),
                   adorn,
                   axes=TRUE,
                   main="",
                   debug=getOption("oceDebug"),
                   ...)
{
    oceDebug(debug, "\b\bimagep(..., cex=", cex, ", flipy=", flipy,
             " xlab='", xlab, "'; ylab='", ylab, "'; zlab='", zlab,
             " filledContour='", filledContour,
             " missingColor='", missingColor,
             ", ...) {\n", sep="")
    oceDebug(debug, "par(mar)=", paste(format(par('mar'), digits=3), collapse=" "), "\n")
    oceDebug(debug, "par(mai)=", paste(format(par('mai'), digits=3), collapse=" "), "\n")
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
    } else if (!missing(x) && is.array(x) && missing(z)) {
        if (length(dim(x)) > 2)
            stop("x must be a matrix, not an array with dim(x) = c(", paste(dim(x), collapse=","), ")\n")
        z <- x
        z <- if (length(dim(x)) > 2) z <- x[,,1] else x
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
    x.is.time <- inherits(x, "POSIXt") || inherits(x, "POSIXct") || inherits(x, "POSIXlt")
    if (!inherits(x, "POSIXct") && !inherits(x, "POSIXct"))
        x <- as.vector(x)
    if (!inherits(y, "POSIXct") && !inherits(y, "POSIXct"))
        y <- as.vector(y)
    dim <- dim(z)
    if (nrow(z) != length(x) && (1+nrow(z)) != length(x))
        stop("image width (", ncol(z), ") does not match length of x (", length(x), ")")
    if (ncol(z) != length(y) && (1+ncol(z)) != length(y))
        stop("image height (", nrow(z), ") does not match length of y (", length(y), ")")
    
    ## Ensure that x and y increase
    ## FIXME: should check on equal values
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

    ## Adjust x and y, to match what image() does; save orig for filled contours
    xorig <- x
    yorig <- y
    tz <- attr(x[1],"tzone")
    if (length(x) > 1 && length(x) == nrow(z)) {
        dx <- 0.5 * diff(x)
        x <- c(x[1L] - dx[1L], x[-length(x)] + dx, x[length(x)] + dx[length(x) - 1])
    }
    if (length(y) > 1 && length(y) == ncol(z)) {
        dy <- 0.5 * diff(y)
        y <- c(y[1L] - dy[1L], y[-length(y)] + dy, y[length(y)] + dy[length(y) - 1])
    }
    attr(x,'tzone')<-tz
    omai <- par("mai")
    omar <- par("mar")
    ocex <- par("cex")
    par(mgp=mgp, mar=mar, cex=cex)
    breaksGiven <- !missing(breaks)
    if (!breaksGiven) {
        zrange <- range(z, na.rm=TRUE)
        if (missing(zlim)) {
            if (missing(col)) {
                breaks <- pretty(zrange, n=10)
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
        if (1 == length(breaks)) {
            breaks <- pretty(z, n=breaks)
        }
    }
    if (missing(col))
        col <- oceColorsPalette(n=length(breaks)-1)
    if (is.function(col))
        col <- col(n=length(breaks)-1)

    if (drawPalette == "space") {
        drawPalette()
    } else if (drawPalette) {
        zlim <- if(missing(zlim)) range(z,na.rm=TRUE) else zlim
        drawPalette(zlim=zlim, zlab="", breaks=breaks, col=col, 
                    labels=labels, at=at,
                    drawContours=drawContours,
                    mai=mai.palette, debug=debug-1)
    }

    xlim <- if (missing(xlim)) range(x,na.rm=TRUE) else xlim
    ylim <- if (missing(ylim)) range(y,na.rm=TRUE) else ylim
    zlim <- if (missing(zlim)) range(z,na.rm=TRUE) else zlim
    if (flipy) {
        ## nc <- ncol(z)
        ## z[, seq.int(nc, 1L)] <- z[, seq.int(1L, nc)]
        ylim <- rev(ylim)
    }
    if (x.is.time) {
        if (filledContour) {
            if (!is.double(z))
                storage.mode(z) <- "double"
            plot.new()
            plot.window(xlim=xlim, ylim=ylim, xaxs=xaxs, yaxs=yaxs)
            ## Filled contours became official in version 2.15.0 of R.
            .filled.contour(as.double(xorig), as.double(yorig), z, as.double(breaks), col=col)
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
            oce.axis.POSIXct(side=1, x=x, #cex=cex, cex.axis=cex, cex.lab=cex,
                             drawTimeRange=drawTimeRange,
                             mar=mar, mgp=mgp, debug=debug-1)
            axis(2)#, cex.axis=cex, cex.lab=cex)
        }
    } else {                           # x is not a POSIXt
        if (filledContour) {
            storage.mode(z) <- "double"
            plot.new()
            plot.window(xlim=xlim, ylim=ylim, xaxs=xaxs, yaxs=yaxs)
            ## Filled contours became official in version 2.15.0 of R.
            .filled.contour(as.double(xorig), as.double(yorig), z, as.double(breaks), col=col)
            mtext(xlab, side=1, line=mgp[1])
            mtext(ylab, side=2, line=mgp[1])
        } else {
            image(x=x, y=y, z=z, axes=FALSE, xlab=xlab, ylab=ylab, breaks=breaks, col=col,
                  xlim=xlim, ylim=ylim, ...)
        }
        box()
        if (axes) {
            axis(1)#, cex.axis=cex, cex.lab=cex)
            axis(2)#, cex.axis=cex, cex.lab=cex)
        }
    }
    if (!is.null(missingColor)) {
        ## FIXME: the negation on is.na is confusing, but it comes from col and breaks together
        image(x, y, !is.na(z), col=c(missingColor, "transparent"), breaks=c(0,1/2,1), add=TRUE)
        box()
    }
    if (!(is.character(main) && main == ""))
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
