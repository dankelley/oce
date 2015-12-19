## vim: tw=120 shiftwidth=4 softtabstop=4 expandtab:

PLEN <- 300 # palette has this many default levels

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
    if ("format" %in% names(dots$format))
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


paletteCalculations <- function(separation=par('cin')[2]/2,
                                width=par('cin')[2],
                                pos=4,
                                zlab, maidiff=c(0, 0, 0, 0),
                                debug=getOption("oceDebug"))
{
    ## This returns a list with the following entries:
    ##   mai0  = before this call
    ##   mai1  = just before plotting palette (i.e. lots of white space on one side)
    ##   mai1f = set before plotting fullpage palette
    ##   mai2  = ready for post-palette drawing (i.e. good for a diagram beside palette)
    if (!(pos %in% 1:4))
        stop("'pos' must be 1, 2, 3 or 4")
    oceDebug(debug, "paletteCalculations(separation=", separation,
             ", width=", width, ", pos=", pos,
             ", zlab=", if (missing(zlab)) "(missing)" else "(given)",
             ", maidiff=c(", paste(maidiff, collapse=","), ")",
             ", debug=", debug, ") {\n", sep="", unindent=1)
    haveZlab <- !missing(zlab) && !is.null(zlab) && sum(nchar(zlab)) > 0

    ## 2014-04-02 {
    ## Below, we will be using e.g. par('mai') to find margins.  If the user
    ## is employing layout(), the call will not give the right answer until the plot
    ## has been established or initialized (not sure on right term).  So, we use 
    ## a trick: call frame() to establish/initialize the plot, then call
    ## plot(new=TRUE) to prevent advancing to the next panel of the layout.
    ## A secondary trick is also required: we set to zero margins before
    ## calling frame(), because otherwise there can be a "figure margins
    ## too large" error from frame(), if the layout is tight.
    omar <- par('mar')
    par(mar=rep(0, 4))
    frame()
    par(mar=omar)
    par(new=TRUE)
    ## OK, done with the trick now.  PS: the long comments given here
    ## are a result of persistent problems with large-margin errors,
    ## and I don't want this new approach to get lost in code.
    ## } 2014-04-02

    lineHeight <- par("cin")[2]  # character height in inches
    tickSpace <- abs(par("tcl")) * lineHeight # inches (not sure on this)
    textSpace <- 1.25 * (lineHeight + if (haveZlab) lineHeight else 0)
    figureWidth <- par("fin")[1]
    figureHeight <- par("fin")[2]
    oceDebug(debug, "figureWidth:", format(figureWidth, digits=2), "in\n")
    oceDebug(debug, "figureHeight:", format(figureHeight, digits=2), "in\n")
    oceDebug(debug, "tickSpace:", tickSpace, "in\n")
    oceDebug(debug, "textSpace:", textSpace, "in\n")
    pc <- list(mai0=par('mai'))
    pc$mai1 <- pc$mai0
    pc$mai1f <- pc$mai0
    pc$mai2 <- pc$mai0
    ##P <- separation + width
    P <- width
    A <- tickSpace + textSpace
    if (pos == 1) {
        ## alter top and bottom margins
        pc$mai1[1] <- A
        pc$mai1[3] <- figureHeight - P - A
        pc$mai1f[2] <- 0
        pc$mai1f[4] <- A
        pc$mai2[1] <- P + A + pc$mai0[1]
        pc$mai2[3] <- pc$mai0[3]
    } else if (pos == 2) {
        ## alter left and right margins
        pc$mai1[2] <- A
        pc$mai1[4] <- figureWidth - P - A
        pc$mai1f[4] <- 0
        pc$mai1f[2] <- A
        pc$mai2[2] <- P + A + pc$mai0[2]
        pc$mai2[4] <- pc$mai0[4]
    } else if (pos == 3) {
        ## alter top and bottom margins
        pc$mai1[1] <- figureHeight - P - A
        pc$mai1[3] <- A
        pc$mai1f[1] <- 0
        pc$mai1f[3] <- A
        pc$mai2[1] <- pc$mai0[1]
        pc$mai2[3] <- P + A + pc$mai0[3]
    } else if (pos == 4) {
        ## DEVELOPER: work here first since it's the common case
        ## alter left and right margins
        pc$mai1[2] <- figureWidth - P - A
        pc$mai1[4] <- A
        pc$mai1f[2] <- 0
        pc$mai1f[4] <- A
        pc$mai2[2] <- pc$mai0[2]
        pc$mai2[4] <- P + A + pc$mai0[4]
    } else {
        stop("pos must be in 1:4") # never reached
    }
    ## Adjust palette margins (mai1); FIXME: should this also alter mai2?
    pc$mai1 <- pc$mai1 + maidiff
    pc$mai1f <- pc$mai1f + maidiff
    oceDebug(debug, "} # paletteCalculations\n", unindent=1)
    pc 
}

drawPalette <- function(zlim, zlab="",
                        breaks, col, colormap,
                        mai, cex.axis=par("cex.axis"), pos=4,
                        labels=NULL, at=NULL,
                        levels, drawContours=FALSE,
                        plot=TRUE, fullpage=FALSE, drawTriangles=FALSE,
                        axisPalette, tformat,
                        debug=getOption("oceDebug"), ...)
{
    zlimGiven <- !missing(zlim)
    colormapGiven <- !missing(colormap)
    oceDebug(debug, "colormapGiven =", colormapGiven, "\n")
    ##message("missing(col) ", missing(col))
    if (!zlimGiven && !colormapGiven)
        plot <- FALSE
    levelsGiven <- !missing(levels)
    if (zlimGiven)
        zlim <- range(zlim, na.rm=TRUE)
    breaksGiven <- !missing(breaks)
    pos <- as.integer(pos)
    if (!(pos %in% 1:4))
        stop("'pos' must be 1, 2, 3 or 4")
    if (zlimGiven)
        oceDebug(debug, "drawPalette(zlim=c(", zlim[1], ",",
                 zlim[2], "), zlab=", "\"", as.character(zlab), "\"",
                 ", pos=", pos, 
                 ", drawTriangles=c(", paste(drawTriangles, collapse=","), "), ...) {\n", 
                 unindent=1, sep="")
    else
        oceDebug(debug, "drawPalette() with no zlim argument\n", sep="", unindent=1)
    maiGiven <- !missing(mai)
    oceDebug(debug, "maiGiven =", maiGiven, "\n")
    if (maiGiven)
        oceDebug(debug, "mai = c(", paste(mai, collapse=","), ") = the argument, not the par() value\n")
    oceDebug(debug, "breaksGiven =", breaksGiven, "\n")
    oceDebug(debug, "fullpage =", fullpage, "\n")
    haveZlab <- !is.null(zlab) && sum(nchar(zlab)) > 0
    zIsTime <- zlimGiven && inherits(zlim[1], "POSIXt")
    if (zIsTime) {
        zlimOrig <- zlim
        zlim <- as.numeric(zlim)
    }
    oceDebug(debug, "zIsTime=", zIsTime, "\n")
    omai <- par("mai")
    oceDebug(debug, "original mai: omai=c(", paste(format(omai, digits=3), collapse=","), ")\n")
    if (!maiGiven)
        mai <- rep(0, 4)
    pc <- paletteCalculations(maidiff=mai, pos=pos, zlab=zlab, debug=debug-1)
    contours <- if (breaksGiven) breaks else NULL
    if (colormapGiven) {
        breaks <- colormap$breaks
        col <- colormap$col
        ## Trick the code below, to avoid auto-creating breaks
        breaksGiven <- TRUE
        colGiven <- TRUE
        if (!zlimGiven)
            zlim <- range(breaks)
        zlimGiven <- TRUE
        breaksOrig <- breaks
        contours <- breaks
    } else {
        if (zlimGiven && !is.null(zlim)) {
            if (breaksGiven) {
                breaksOrig <- breaks
                contours <- breaks
            } else {
                zrange <- zlim
                if (missing(col)) {
                    breaks <- pretty(zlim)
                    contours <- breaks
                } else {
                    if (is.function(col)) {
                        breaks <- seq(zlim[1], zlim[2], length.out=PLEN) # smooth image colorscale
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
                col <- oce.colorsPalette(n=length(breaks)-1)
            if (is.function(col))
                col <- col(n=length(breaks)-1)
        }
    }
    oceDebug(debug, "plot:", plot, "\n")
    if (plot) {
        if (fullpage)
            par(mai=ifelse(pc$mai1f>0, pc$mai1f, 0))
        else
            par(mai=ifelse(pc$mai1>0, pc$mai1, 0))
        oceDebug(debug, "A. par(mai=c(", paste(round(par('mai'),1), collapse=","), "))\n")
        oceDebug(debug, "A. par(mar=c(", paste(round(par('mar'),1), collapse=","), "))\n")
        if (!breaksGiven) {
            palette <- seq(zlim[1], zlim[2], length.out=PLEN)
            if (pos == 1 || pos == 3) {
                image(x=palette, y=1, z=matrix(palette, ncol=1), axes=FALSE, xlab="", ylab="",
                      col=col, zlim=zlim)
            } else if (pos == 2 || pos == 4) {
                FIN <- par('fin')
                PIN <- par('pin')
                MAI <- par('mai')

                oceDebug(debug, "mai[2] and mail[4] add to", MAI[2] + MAI[4], "fin[1]=", FIN[1], "so image will occupy ", FIN[1] - MAI[2] - MAI[4], "inches\n")
                oceDebug(debug, "mai[2] and mail[4] add to", MAI[2] + MAI[4], "pin[1]=", PIN[1], "so image will occupy ", FIN[1] - MAI[2] - MAI[4], "inches\n")

                image(x=1, y=palette, z=matrix(palette, nrow=1), axes=FALSE, xlab="", ylab="",
                      col=col, zlim=zlim)
            } else {
                stop("pos must be 1, 2, 3 or 4") # cannot be reached
            }
        } else {
            palette <- seq(zlim[1], zlim[2], length.out=PLEN)
            oceDebug(debug, "drawing palette image, with par('mai')=c(",
                     paste(round(par('mai'), 2), collapse=","), ")\n")
            oceDebug(debug, "palette image width =",
                     par('fin')[1] - par('mai')[2] - par('mai')[4], "in\n")
            oceDebug(debug, "palette image height =",
                     par('fin')[2] - par('mai')[1] - par('mai')[3], "in\n")
            oceDebug(debug, "par('pin')=c(", paste(format(par('pin'), 2), collapse=","), ") in\n")
            oceDebug(debug, "par('fin')=c(", paste(format(par('fin'), 2), collapse=","), ") in\n")
            if (pos == 1 || pos == 3) {
                image(x=palette, y=1, z=matrix(palette, ncol=1), axes=FALSE, xlab="", ylab="",
                      breaks=breaksOrig, col=col, zlim=zlim)
            } else if (pos == 2 || pos == 4) {
                ##message("in drawPalette(), breaks and col follow:");
                ##str(breaksOrig)
                ##str(col)
                oceDebug(debug, "B. par(mai=c(", paste(round(par('mai'),1), collapse=","), "))\n")
                oceDebug(debug, "B. par(mar=c(", paste(round(par('mar'),1), collapse=","), "))\n")
                image(x=1, y=palette, z=matrix(palette, nrow=1), axes=FALSE, xlab="", ylab="",
                      breaks=breaksOrig, col=col, zlim=zlim)
            } else {
                stop("pos must be 1, 2, 3 or 4") # cannot be reached
            }
        }
        if (drawContours) {
            if (pos == 1 || pos == 3) {
                if (levelsGiven) abline(v=levels) else abline(v=contours)
            } else if (pos == 2 || pos == 4){
                if (levelsGiven) abline(h=levels) else abline(h=contours)
            } else {
                stop("pos must be 1, 2, 3 or 4") # cannot be reached
            }
        }
        box()
        drawTriangles <- rep(drawTriangles, length.out=2)
        if (any(drawTriangles, na.rm=TRUE)) {
            mai <- par('mai')
            fin <- par('fin')
            width <- fin[1] - mai[2] - mai[4] # inch
            paletteHeight <- fin[2] - mai[1] - mai[3] # inch
            usr <- par('usr')
            dx <- usr[2] - usr[1]      # user unit
            dy <- usr[4] - usr[3]      # user unit
            triangleHeight <- 1 / 3 * width * dy / dx / paletteHeight
            oceDebug(debug, "triangleHeight=", triangleHeight, "(user units)\n")
            if (drawTriangles[2]) {
                if (pos == 1 || pos == 3) {
                    warning("horizontal triangles not working yet\n")
                } else if (pos == 2 || pos == 4) {
                    polygon(c(usr[1], 0.5*(usr[1]+usr[2]), usr[2]),
                            usr[4] + c(0, triangleHeight, 0), col=col[length(col)], 
                            border=col[length(col)], xpd=TRUE)
                    lines(c(usr[1], 0.5*(usr[1]+usr[2]), usr[2]),
                          usr[4] + c(0, triangleHeight, 0),
                          xpd=TRUE)
                }
            }
            if (drawTriangles[1]) {
                if (pos == 1 || pos == 3) {
                    warning("horizontal triangles not working yet\n")
                } else if (pos == 2 || pos == 4) {
                    polygon(c(usr[1], 0.5*(usr[1]+usr[2]), usr[2]),
                            usr[3] + c(0, -triangleHeight, 0), col=col[1], 
                            border=col[1], xpd=TRUE)
                    lines(c(usr[1], 0.5*(usr[1]+usr[2]), usr[2]),
                          usr[3] + c(0, -triangleHeight, 0),
                          xpd=TRUE)
                }
            }
        }
        if (zIsTime && is.null(at)) {
            at <- as.numeric(pretty(zlim))
        } else if (is.null(at)) {
            if (missing(axisPalette)) {
                ## NB. in next line, the '10' matches a call to pretty() in imagep().
                at <- if (!is.null(contours) & is.null(at)) prettyLocal(contours, 10) else prettyLocal(palette, 10)
            } else {
                ## Guess that the axis labels may need more space
                at <- if (!is.null(contours) & is.null(at)) prettyLocal(contours, 6) else prettyLocal(palette, 6)
            }
        }
        if (is.null(labels)) {
            if (zIsTime) {
                if (!missing(tformat)) {
                    labels <- format(numberAsPOSIXct(at), format=tformat)
                } else {
                    labels <- abbreviateTimeLabels(numberAsPOSIXct(at), ...)
                }
            } else {
                labels <- format(at)
            }
        }
        labels <- sub("^[ ]*", "", labels)
        labels <- sub("[ ]*$", "", labels)
        ## FIXME: just guessing on best 'line', used below
        if (!missing(axisPalette))
            axis <- axisPalette
        if (pos == 1) {
            axis(side=1, at=at, labels=labels, mgp=c(2.5,0.7,0), cex.axis=cex.axis)
            if (haveZlab) mtext(zlab, side=1, line=getOption("oceMgp")[1],
                                cex=par('cex'), cex.axis=cex.axis)
        } else if (pos == 2) {
            axis(side=2, at=at, labels=labels, mgp=c(2.5,0.7,0), cex.axis=cex.axis)
            if (haveZlab) mtext(zlab, side=2, line=getOption("oceMgp")[1],
                                cex=par('cex'), cex.axis=cex.axis)
        } else if (pos == 3) {
            axis(side=3, at=at, labels=labels, mgp=c(2.5,0.7,0), cex.axis=cex.axis)
            if (haveZlab) mtext(zlab, side=3, line=getOption("oceMgp")[1],
                                cex=par('cex'), cex.axis=cex.axis)
        } else if (pos == 4) {
            axis(side=4, at=at, labels=labels, mgp=c(2.5,0.7,0), cex.axis=cex.axis)
            if (haveZlab) mtext(zlab, side=4, line=getOption("oceMgp")[1],
                                cex=par('cex'), cex.axis=cex.axis)
        } else {
            stop("pos must be 1, 2, 3 or 4") # cannot be reached
        }
    }
    ## FIXME why the "new" in only one case? Generally, the graphic state is confusing!
    if (fullpage) {
        par(mai=pc$mai0) # reset to original
    } else {
        if (zlimGiven) {
            par(new=TRUE, mai=pc$mai2)
        } else {
            par(mai=pc$mai2)
        }
    }
    oceDebug(debug, "at end of drawPalette(), par('mai') yields c(",
             paste(format(par('mai'), digits=2), collapse=","), ")\n")
    oceDebug(debug, "} # drawPalette()\n", unindent=1)
    invisible()
}

imagep <- function(x, y, z,
                   xlim, ylim, zlim,
                   zclip=FALSE, flipy=FALSE,
                   xlab="", ylab="", zlab="", zlabPosition=c("top", "side"),
                   decimate=TRUE,
                   breaks, col, colormap, labels=NULL, at=NULL,
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
                   axisPalette,
                   debug=getOption("oceDebug"),
                   ...)
{
    zlabPosition <- match.arg(zlabPosition)
    oceDebug(debug, "imagep(x, y, z, ",
             argShow(cex),
             argShow(flipy),
             argShow(breaks),
             argShow(zlim),
             argShow(col),
             "colormap=", if (missing(colormap)) "(missing), " else "(provided), ",
             argShow(xlab),
             argShow(ylab),
             argShow(zlab),
             argShow(zlabPosition),
             argShow(filledContour),
             argShow(drawTriangles),
             argShow(missingColor),
             "...) {\n", sep="", unindent=1)
    oceDebug(debug, "par('mai'):", paste(format(par('mai'), digits=2)), "\n")
    oceDebug(debug, "par('mar'):", paste(format(par('mar'), digits=2)), "\n")
    zlimGiven <- !missing(zlim) && !is.null(zlim) # latter is used by plot.adp
    breaksGiven <- !missing(breaks)
    if (zlimGiven && breaksGiven && length(breaks) > 1)
        stop("cannot specify both zlim and breaks, unless length(breaks)==1")

    xat <- NULL
    yat <- NULL

    ## issue 674: permit POSIXlt in addition to POSIXct
    if (inherits(x, "POSIXt"))
        x <- as.POSIXct(x)

    haveZlab <- !is.null(zlab) && sum(nchar(zlab)) > 0
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
    z[!is.finite(z)] <- NA # so range(z, na.rm=TRUE) will not be thwarted Inf
    oceDebug(debug, "range(z):", range(z, na.rm=TRUE), "\n")
    xIsTime <- inherits(x, "POSIXt") || inherits(x, "POSIXct") || inherits(x, "POSIXlt")
    # Handle TRUE/FALSE decimation
    dim <- dim(z)
    if (is.logical(decimate) && decimate) {
        if (is.logical(decimate)) { # find value from image
            maxdim <- max(dim)
            decimate <- max(as.integer(round(maxdim / 400)), 1)
            oceDebug(debug, "set auto decimation=", decimate, "\n")
        }
    }
    if (decimate < 1)
        stop("decimate must be a positive integer or a logical value")
    oceDebug(debug, "decimate:", decimate, "\n")
    if (decimate > 1) {
        ilook <- seq.int(1, dim[1], by=decimate)
        jlook <- seq.int(1, dim[2], by=decimate)
        oceDebug(debug, "ilook:", paste(ilook[1:4], collapse=" "), "...\n")
        oceDebug(debug, "jlook:", paste(jlook[1:4], collapse=" "), "...\n")
        x <- x[ilook]
        y <- y[jlook]
        z <- z[ilook, jlook]
    }
    if (!inherits(x, "POSIXct") && !inherits(x, "POSIXct"))
        x <- as.vector(x)
    if (!inherits(y, "POSIXct") && !inherits(y, "POSIXct"))
        y <- as.vector(y)
    dim <- dim(z)
    if (nrow(z) != length(x) && (1+nrow(z)) != length(x))
        stop("nrow(image)=", nrow(z), " does not match length(x)=", length(x), sep="")
    if (ncol(z) != length(y) && (1+ncol(z)) != length(y))
        stop("ncol(image)=", ncol(z), " does not match length(y)=", length(y), sep="")
    
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
    ocex <- par("cex")
    if (missing(mar))
        mar <- c(mgp[1]+if(nchar(xlab)>0) 1.5 else 1, mgp[1]+if(nchar(ylab)>0) 1.5 else 1, mgp[2]+1/2, 1/2)
    if (missing(mai.palette)) {
        ##mai.palette <- c(0, 1/8, 0, 3/8 + if (haveZlab && zlabPosition=="side") 1.5*par('cin')[2] else 0)
        mai.palette <- rep(0, 4)
        oceDebug(debug, "set mai.palette=", mai.palette, "\n")
    }

    par(mgp=mgp, mar=mar, cex=cex)

    zlimHistogram <- zlimGiven && zlim == "histogram"
    breaksGiven <- !missing(breaks)
    colormapGiven <- !missing(colormap)
    if (colormapGiven && missing(missingColor))
        missingColor <- colormap$missingColor
    zrange <- range(z, na.rm=TRUE)

    if (colormapGiven) {
        oceDebug(debug, "colormap provided\n")
        breaks <- colormap$breaks
        breaks2 <- breaks
        col <- colormap$col
        col2 <- col
    } else {
        ## Determine breaks unless zlim=="histogram".
        if (zlimHistogram) {
            if (missing(col))
                col <- oce.colorsPalette(200) # FIXME: how many colours to use?
        } else {
            if (!breaksGiven) {
                nbreaks <- 128                 # smooth image colorscale
                if (missing(zlim)) {
                    if (missing(col)) {
                        breaks <- pretty(zrange, n=nbreaks)
                        if (breaks[1] < zrange[1])
                            breaks[1] <- zrange[1]
                        if (breaks[length(breaks)] > zrange[2])
                            breaks[length(breaks)] <- zrange[2]
                    } else {
                        breaks <- seq(zrange[1], zrange[2], length.out=if(is.function(col))PLEN else 1+length(col))
                    }
                    breaksOrig <- breaks
                } else {
                    ## zlim given, but breaks not given
                    if (missing(col)) {
                        ##breaks <- c(zlim[1], pretty(zlim, n=nbreaks), zlim[2])
                        breaks <- pretty(zlim, n=nbreaks)
                        oceDebug(debug, "zlim given but not breaks or col\n")
                        oceDebug(debug, "inferred breaks:", head(breaks), "...\n")
                    } else {
                        breaks <- seq(zlim[1], zlim[2],
                                      length.out=if(is.function(col))PLEN else 1+length(col))
                        oceDebug(debug, "zlim and col given but not breaks; inferred head(breaks)=", head(breaks), "\n")
                    }
                    breaksOrig <- breaks
                    oceDebug(debug, 'range(z):', zrange, '\n')
                    oceDebug(debug, 'ORIG  range(breaks):', range(breaks), '\n')
                    breaks[1] <- min(max(zlim[1], zrange[1]), breaks[1])
                    breaks[length(breaks)] <- max(breaks[length(breaks)], min(zlim[2], zrange[2]))
                    oceDebug(debug, 'later range(breaks):', range(breaks), '\n')
                }
            } else {
                breaksOrig <- breaks
                if (1 == length(breaks)) {
                    breaks <- if (missing(zlim)) pretty(z, n=breaks) else pretty(zlim, n=breaks)
                }
            }
            if (missing(col))
                col <- oce.colorsPalette(n=length(breaks)-1)
        }
        breaks2 <- if (missing(breaks)) NULL else breaks
        col2 <- if (missing(col)) NULL else col
        ## message("imagep() col:  ", paste(col, collapse=" "))
        ## If not z clipping, enlarge breaks/cols to avoid missing-colour regions
        oceDebug(debug, "zrange=c(", zrange[1], ",", zrange[2], ")\n", sep="")
        oceDebug(debug, "zclip:", zclip, "\n")
        oceDebug(debug, "zlimHistogram:", zlimHistogram, "\n")
        ## 2014-08-02: add zclip to test [issue 516]
        if (!zclip && !zlimHistogram) {
            db <- median(diff(breaks), na.rm=TRUE)
            breaks2 <- c(min(c(zrange[1], breaks, na.rm=TRUE))-db/100,
                         breaks,
                         max(c(zrange[2], breaks, na.rm=TRUE))+db/100)
            if (!is.function(col))
                col2 <- c(col[1], col, col[length(col)])
            oceDebug(debug, "USE breaks2 and col2 as calculated\n")
        } else {
            oceDebug(debug, "IGNORE breaks2 and col2 as calculated\n")
            ##20140801 warning("2014-07-17/#489 trial code: ignore breaks2 and col2")
            if (!missing(breaks))
                breaks2 <- breaks
            if (!missing(col))
                col2 <- col
        }
        if (is.function(col)) {
            if (zlimHistogram)
                col <- col(n=200)          # FIXME: decide on length
            else
                col <- col(n=length(breaks)-1)
        }
    }
    if (!missing(breaks))
        oceDebug(debug, "head(breaks): ", paste(head(breaks), collapse=" "), "\n")
    oceDebug(debug, "head(col): ", paste(head(col), collapse=" "), "\n")

    ## issue 542: move this out from the drawPalette part of the next block
    if (missing(zlim) || is.null(zlim)) {
        oceDebug(debug, "zlim is missing or NULL\n")
        ## use range of breaks preferably; otherwise use range z
        if (missing(breaks)) {
            zlim <- range(z, na.rm=TRUE)
            oceDebug(debug, "infer zlim=c(", zlim[1], ",", zlim[2], ") from range(z)\n", sep="")
        } else {
            zlim <- range(breaks, na.rm=TRUE)
            oceDebug(debug, "infer zlim=c(", zlim[1], ",", zlim[2], ") from range(breaks)\n", sep="")
        }
    }
    xlim <- if (missing(xlim)) range(x,na.rm=TRUE) else xlim
    ylim <- if (missing(ylim)) range(y,na.rm=TRUE) else ylim
    ## oceDebug(debug, "zlimGiven: ", zlimGiven, "\n")
    ## zlim <- if (missing(zlim)) range(z,na.rm=TRUE) else zlim
    ## oceDebug(debug, "zlim=c(", paste(zlim, collapse=","), ")\n", sep="")


    if (drawPalette == "space") {
        drawPalette(zlab=if(zlabPosition=="side") zlab else "", axisPalette=axisPalette, debug=debug-1)
    } else if (drawPalette) {
        ## issue 542: put this above the block
        ## if(missing(zlim)) {
        ##     ## use range of breaks preferably; otherwise use range z
        ##     if (missing(breaks)) {
        ##         zlim <- range(z, na.rm=TRUE)
        ##     } else {
        ##         zlim <- range(breaks, na.rm=TRUE)
        ##     }
        ## }
        drawTriangles <- rep(drawTriangles, length.out=2)
        drawTriangles[1] <- drawTriangles[1] && any(z < zlim[1], na.rm=TRUE)
        drawTriangles[2] <- drawTriangles[2] && any(z > zlim[2], na.rm=TRUE)
        oceDebug(debug, "mai.palette=c(", paste(mai.palette, collapse=", "), ")\n")
        if (zlimHistogram) {
            oceDebug(debug, "palette with zlim=\"histogram\"\n")
            ## CAUTION: change data in 'z'
            dim <- dim(z)
            z <- as.vector(z)
            n <- length(z)
            h <- hist(z, breaks=100, plot=FALSE)   # the 100 could be altered...
            mids <- h$mids
            density <- cumsum(h$counts)/n
            z <- approx(mids, density, z, rule=2)$y
            dim(z) <- dim
            labels <- round(approx(density, mids, seq(0, 1, 0.1), rule=2)$y, 3)
            drawPalette(zlim=c(0,1), zlab=if(zlabPosition=="side") zlab else "",
                        col=col, 
                        labels=labels, at=seq(0, 1, 0.1),
                        drawContours=drawContours,
                        drawTriangles=drawTriangles,
                        mai=mai.palette, debug=debug-1)
        } else {
            oceDebug(debug, "palette with zlim not \"histogram\"\n")
            drawPalette(zlim=zlim, zlab=if(zlabPosition=="side") zlab else "",
                        breaks=breaks, col=col, 
                        labels=labels, at=at,
                        drawContours=drawContours,
                        drawTriangles=drawTriangles,
                        mai=mai.palette,
                        axisPalette=axisPalette,
                        debug=debug-1)
        }
    }

    ## xlim <- if (missing(xlim)) range(x,na.rm=TRUE) else xlim
    ## ylim <- if (missing(ylim)) range(y,na.rm=TRUE) else ylim
    ## oceDebug(debug, "zlimGiven: ", zlimGiven, "\n")
    ## zlim <- if (missing(zlim)) range(z,na.rm=TRUE) else zlim
    ## oceDebug(debug, "zlim=c(", paste(zlim, collapse=","), ")\n", sep="")

    ## trim image to limits, so endpoint colours will indicate outliers
    if (!zclip && !zlimHistogram) {
        oceDebug(debug, "using zlim[1:2]=c(", zlim[1], ",", zlim[2], ") for out-of-range values\n")
        z[z < zlim[1]] <- zlim[1]
        z[z > zlim[2]] <- zlim[2]
    }

    if (flipy) {
        ## nc <- ncol(z)
        ## z[, seq.int(nc, 1L)] <- z[, seq.int(1L, nc)]
        ylim <- rev(ylim)
    }
    if (zclip && !zlimHistogram) {
        oceDebug(debug, "using missingColour for out-of-range values")
        z[z < zlim[1]] <- NA
        z[z > zlim[2]] <- NA
    }
    if (is.function(col2) && !is.null(breaks2)) {
        col2 <- col2(n=length(breaks2)-1)
    }
    if (xIsTime) {
        oceDebug(debug, "the x axis represents time\n")
        ## if (debug > 0) {
        ##     message("breaks:", paste(breaks, collapse=" "))
        ##     message("breaks2:", paste(breaks2, collapse=" "))
        ##     message("col:", paste(col, collapse=" "))
        ##     message("col2:", paste(col2, collapse=" "))
        ## }
        if (filledContour) {
            oceDebug(debug, "doing filled contours [1]\n")
            if (!is.double(z))
                storage.mode(z) <- "double"
            plot.new()
            plot.window(xlim=xlim, ylim=ylim, xaxs=xaxs, yaxs=yaxs, ...)
            ## Filled contours became official in version 2.15.0 of R.
            ## issue 489: use breaks/col instead of breaks2/col2
            #.filled.contour(as.double(xorig), as.double(yorig), z, as.double(breaks2), col=col2)
            .filled.contour(as.double(xorig), as.double(yorig), z, as.double(breaks), col=col)
            mtext(ylab, side=2, line=par('mgp')[1])
        } else {
            oceDebug(debug, "not doing filled contours [2]\n")
            if (zlimHistogram) {
                image(x=x, y=y, z=z, axes=FALSE, xlab="", ylab=ylab, col=col2,
                      xlim=xlim, ylim=ylim, zlim=c(0,1), ...)
            } else {
                ## issue 489: use breaks/col instead of breaks2/col2
                ##image(x=x, y=y, z=z, axes=FALSE, xlab="", ylab=ylab, breaks=breaks2, col=col2,
                image(x=x, y=y, z=z, axes=FALSE, xlab="", ylab=ylab, breaks=breaks, col=col,
                  xlim=xlim, ylim=ylim, zlim=zlim, ...)
            }
        }
        if (axes) {
            box()
            xat <- oce.axis.POSIXct(side=1, x=x, #cex=cex, cex.axis=cex, cex.lab=cex,
                                    drawTimeRange=drawTimeRange,
                                    mar=mar, mgp=mgp, tformat=tformat, debug=debug-1)
            yat <- axis(2)#, cex.axis=cex, cex.lab=cex)
        }
    } else {                           # x is not a POSIXt
        oceDebug(debug, "the x axis does not represent time\n")
        if (filledContour) {
            oceDebug(debug, "doing filled contours [3]\n")
            storage.mode(z) <- "double"
            plot.new()
            plot.window(xlim=xlim, ylim=ylim, xaxs=xaxs, yaxs=yaxs, ...)
            ## Filled contours became official in version 2.15.0 of R.
            ## issue 489: use breaks/col instead of breaks2/col2
            ##.filled.contour(as.double(xorig), as.double(yorig), z, as.double(breaks2), col=col2)
            .filled.contour(as.double(xorig), as.double(yorig), z, as.double(breaks), col=col)
            mtext(xlab, side=1, line=mgp[1])
            mtext(ylab, side=2, line=mgp[1])
        } else {
            oceDebug(debug, "not doing filled contours\n")
            if (zlimHistogram) {
                if (is.function(col2))
                    col2 <- col2(200)
                breaks2 <- seq(0, 1, length.out=length(col2) + 1)
            }
            oceDebug(debug, "length(x)", length(x), "length(y)", length(y), "\n")
            ## issue 489: use breaks/col instead of breaks2/col2
            ##image(x=x, y=y, z=z, axes=FALSE, xlab=xlab, ylab=ylab, breaks=breaks2, col=col2,
            image(x=x, y=y, z=z, axes=FALSE, xlab=xlab, ylab=ylab, breaks=breaks, col=col,
                  xlim=xlim, ylim=ylim, ...)
        }
        if (axes) {
            box()
            xat <- axis(1)#, cex.axis=cex, cex.lab=cex)
            yat <- axis(2)#, cex.axis=cex, cex.lab=cex)
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
    if (drawContours) {
        oceDebug(debug, "adding contours\n")
        contour(x=xorig, y=yorig, z=z, levels=breaks, drawlabels=FALSE, add=TRUE, col="black")
    }
    if (zlabPosition == "top")
        mtext(zlab, side=3, cex=par("cex"), adj=1, line=1/8)
    if (!missing(adorn)) {
        t <- try(eval.parent(adorn), silent=!TRUE)
        if (class(t) == "try-error")
            warning("cannot evaluate adorn='", adorn, "'\n")
    }
    par(cex=ocex)
    oceDebug(debug, "par('mai')=c(",
             paste(format(par('mai'), digits=2), collapse=","), "); par('mar')=c(",
             paste(format(par('mar'), digits=2), collapse=","), ")\n", sep='')
    oceDebug(debug, "} # imagep()\n", unindent=1)
    invisible(list(xat=xat, yat=yat))
}
