## vim: tw=120 shiftwidth=4 softtabstop=4 expandtab:

prettyLocal <- function(x, n, digits=10)
{
    round(pretty(x, n), digits)
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

drawPalette <- function(zlim, zlab="", breaks, col,
                        labels=NULL, at=NULL, mai, fullpage=FALSE,
                        drawContours=FALSE, drawTriangles=FALSE,
                        debug=getOption("oceDebug"), ...)
{
    zlimGiven <- !missing(zlim)
    if (zlimGiven)
        zlim <- range(zlim, na.rm=TRUE)
    breaksGiven <- !missing(breaks)
    if (zlimGiven)
        oceDebug(debug, "\bdrawPalette(zlim=c(", zlim[1], ",",
                 zlim[2], "), zlab=", "\"", as.character(zlab), "\",
                 drawTriangles=", drawTriangles, "...) {\n", sep="")
    else
        oceDebug(debug, "\bdrawPalette() with no arguments: set space to right of a graph\n", sep="")
    maiGiven <- !missing(mai)
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
    if (!maiGiven)
        mai <- c(0, 1/8, 0, 3/8 + if (nchar(zlab)) 1.5*par('cin')[2] else 0)
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
    oceDebug(debug, "fullpage=", fullpage, "\n")
    oceDebug(debug, "maiGiven=", maiGiven, "\n")
    if (fullpage) {
        if (maiGiven) {
            theMai <- mai
        } else {
            width <- par('fin')[1] # width
            mai4 <- 3 * par('cin')[2]
            mai2 <- 1/8 #width - paletteWidth - mai4
            theMai <- c(omai[1], mai2, omai[3], mai4)
            oceDebug(debug, "width=", width, ", mai2=", mai2, ", mai4=", mai4, "\n")
        }
    } else {
        theMai <- c(pc$omai[1]+mai[1],
                    pc$main + pc$maiLHS + mai[2],
                    pc$omai[3]+mai[3],
                    pc$maiRHS)
    }
    oceDebug(debug, "theMai=", theMai, "\n")
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
        drawTriangles <- rep(drawTriangles, length.out=2)
        if (any(drawTriangles, na.rm=TRUE)) {
            mai <- par('mai')
            fin <- par('fin')
            paletteWidth <- fin[1] - mai[2] - mai[4] # inch
            paletteHeight <- fin[2] - mai[1] - mai[3] # inch
            usr <- par('usr')
            dx <- usr[2] - usr[1]      # user unit
            dy <- usr[4] - usr[3]      # user unit
            triangleHeight <- 1 / 3 * paletteWidth * dy / dx / paletteHeight
            oceDebug(debug, "triangleHeight=", triangleHeight, "(user units)\n")
            if (drawTriangles[2]) {
                polygon(c(usr[1], 0.5*(usr[1]+usr[2]), usr[2]),
                        usr[4] + c(0, triangleHeight, 0), col=col[length(col)], 
                        border=col[length(col)], xpd=TRUE)
                lines(c(usr[1], 0.5*(usr[1]+usr[2]), usr[2]),
                      usr[4] + c(0, triangleHeight, 0),
                      xpd=TRUE)
            }
            if (drawTriangles[1]) {
                polygon(c(usr[1], 0.5*(usr[1]+usr[2]), usr[2]),
                        usr[3] + c(0, -triangleHeight, 0), col=col[1], 
                        border=col[1], xpd=TRUE)
                lines(c(usr[1], 0.5*(usr[1]+usr[2]), usr[2]),
                      usr[3] + c(0, -triangleHeight, 0),
                      xpd=TRUE)
            }
        }
        if (zIsTime & is.null(at)) {
            at <- as.numeric(pretty(zlim))
        } else if (is.null(at)) {
            ## NB. in next line, the '10' matches a call to pretty() in imagep().
            at <- if (!is.null(contours) & is.null(at)) prettyLocal(contours, 10) else prettyLocal(palette, 10) # FIXME: wrong on contours
        }
        if (is.null(labels))
            labels <- if (zIsTime) abbreviateTimeLabels(numberAsPOSIXct(at), ...) else format(at)
        labels <- sub("^[ ]*", "", labels)
        labels <- sub("[ ]*$", "", labels)
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
    if (fullpage)
        par(mai=omai)
    oceDebug(debug, "drawPalette at end mar=",par('mar'),"\n")
    oceDebug(debug, "\b\b} # drawPalette()\n")
    invisible()
}

imagep <- function(x, y, z,
                   xlim, ylim, zlim,
                   zclip=FALSE,
                   flipy=FALSE,
                   xlab="", ylab="", zlab="", zlabPosition=c("top", "side"),
                   breaks, col,
                   labels=NULL, at=NULL,
                   drawContours=FALSE,
                   drawPalette=TRUE,
                   drawTriangles=FALSE,
                   tformat,
                   drawTimeRange=getOption("oceDrawTimeRange"),
                   filledContour=FALSE,
                   missingColor=NULL,
                   mgp=getOption("oceMgp"),
                   mar, mai.palette,
                   xaxs = "i", yaxs = "i",
                   cex=par("cex"),
                   adorn,
                   axes=TRUE,
                   main="",
                   debug=getOption("oceDebug"),
                   ...)
{
    zlabPosition <- match.arg(zlabPosition)
    oceDebug(debug, "\b\bimagep(..., cex=", cex, ", flipy=", flipy, ",", 
             " xlab='", xlab, "'; ylab='", ylab, "'; zlab=\"", as.character(zlab), "\", ", 
             " zlabPosition=\"", zlabPosition, "\", ",
             " filledContour=", filledContour, ", ",
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
        ##y <- seq(0, 1, length.out=ncol(x))
        ##x <- seq(0, 1, length.out=nrow(x))
        y <- seq.int(1L, ncol(x))
        x <- seq.int(1L, nrow(x))
    } else if (!missing(x) && inherits(x, "topo")) {
        ## NB. rewrites x, so put that last
        y <- x[["latitude"]]
        z <- x[["z"]]
        x <- x[["longitude"]]
        if (missing(xlab)) xlab <- "Longitude"
        if (missing(ylab)) ylab <- "Latitude"
    } else if (!missing(z) && is.matrix(z) && missing(x) && missing(y)) {
        ##x <- seq(0, 1, length.out=nrow(z))
        ##y <- seq(0, 1, length.out=ncol(z))
        x <- seq.int(1L, nrow(z))
        y <- seq.int(1L, ncol(z))
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
        ##warning("reordered some x values")
        x <- x[ox]
        z <- z[ox, ]
    }
    oy <- order(y)
    if (any(diff(oy) < 0)) {
        ##warning("reordered some y values")
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
    attr(x,'tzone') <- tz
    omai <- par("mai")
    omar <- par("mar")
    ocex <- par("cex")
    if (missing(mar))
        mar <- c(mgp[1]+if(nchar(xlab)>0) 1.5 else 1, mgp[1]+if(nchar(ylab)>0) 1.5 else 1, mgp[2]+1/2, 1/2)
    if (missing(mai.palette)) {
        mai.palette <- c(0, 1/8, 0, 3/8 + if (nchar(zlab) && zlabPosition=="side") 1.5*par('cin')[2] else 0)
        oceDebug(debug, "set mai.palette=", mai.palette, "\n")
    }

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

    #browser()
    if (drawPalette == "space") {
        drawPalette(zlab=if(zlabPosition=="side") zlab else "", debug=debug-1)
    } else if (drawPalette) {
        zlim <- if(missing(zlim)) range(z,na.rm=TRUE) else zlim
        drawTriangles <- rep(drawTriangles, length.out=2)
        drawTriangles[1] <- drawTriangles[1] || any(z < zlim[1], na.rm=TRUE)
        drawTriangles[2] <- drawTriangles[2] || any(z > zlim[2], na.rm=TRUE)
        drawPalette(zlim=zlim, zlab=if(zlabPosition=="side") zlab else "",
                    breaks=breaks, col=col, 
                    labels=labels, at=at,
                    drawContours=drawContours,
                    drawTriangles=drawTriangles,
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
    if (zclip) {
        oceDebug(debug, "using missingColour for out-of-range values")
        z[z < zlim[1]] <- NA
        z[z > zlim[2]] <- NA
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
        if (axes) {
            box()
            oce.axis.POSIXct(side=1, x=x, #cex=cex, cex.axis=cex, cex.lab=cex,
                             drawTimeRange=drawTimeRange,
                             mar=mar, mgp=mgp, tformat=tformat, debug=debug-1)
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
        if (axes) {
            box()
            axis(1)#, cex.axis=cex, cex.lab=cex)
            axis(2)#, cex.axis=cex, cex.lab=cex)
        }
    }
    if (!is.null(missingColor)) {
        ## FIXME: the negation on is.na is confusing, but it comes from col and breaks together
        image(x, y, !is.na(z), col=c(missingColor, "transparent"), breaks=c(0,1/2,1), add=TRUE)
        if (axes)
            box()
    }
    if (!(is.character(main) && main == ""))
        mtext(main, at=mean(range(x), na.rm=TRUE), side=3, line=1/8, cex=par("cex"))
    if (drawContours)
        contour(x=x, y=y, z=z, levels=breaks, drawlabels=FALSE, add=TRUE, col="black")
    if (zlabPosition == "top")
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
