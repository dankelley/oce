## vim: tw=120 shiftwidth=4 softtabstop=4 expandtab:

PLEN <- 300 # palette has this many default levels

prettyLocal <- function(x, n, digits=10)
{
    if (is.numeric(x)) round(pretty(x, n), digits) else pretty(x, n)
}

clipmin <- function(x, min=0)
{
    ifelse(x < min, min, x)
}


#' Abbreviate a vector of times by removing commonalities
#'
#' Abbreviate a vector of times by removing commonalities (e.g. year)
#'
#' @param t vector of times.
#' @param \dots optional arguments passed to the \code{\link{format}}, e.g.
#' \code{format}.
#' @return None.
#' @author Dan Kelley, with help from Clark Richards
#' @seealso This is used by various functions that draw time labels on axes,
#' e.g.  \code{\link{plot,adp-method}}.
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
    oceDebug(debug, "lineHeight: ", lineHeight, " from cin\n")
    oceDebug(debug, "par('csi'):: ", par('csi'), "\n")
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
    oceDebug(debug, "pc$mail1: ", paste(round(pc$mai1, 2), sep=" "), "\n")
    oceDebug(debug, "pc$mailf: ", paste(round(pc$mai1f, 2), sep=" "), "\n")
    oceDebug(debug, "} # paletteCalculations\n", unindent=1)
    pc
}


#' Draw a palette, leaving margins suitable for accompanying plot
#'
#' Draw a palette, leaving margins suitable for accompanying plot.
#'
#' In the normal use, \code{drawPalette} draws an image palette near the
#' right-hand side of the plotting device, and then adjusts the global margin
#' settings in such a way as to cause the next plot to appear (with much larger
#' width) to the left of the palette. The function can also be used, if
#' \code{zlim} is not provided, to adjust the margin without drawing anything;
#' this is useful in lining up the x axes of a stack of plots, some some of
#' which will have palettes and others not.
#'
#' The plot positioning is done entirely with margins, not with
#' \code{par(mfrow)} or other R schemes for multi-panel plots.  This means that
#' the user is free to use those schemes without worrying about nesting or
#' conflicts.
#'
#' @param zlim two-element vector containing the lower and upper limits of z.
#' This may also be a vector of any length exceeding 1, in which case its range
#' is used.
#' @param zlab label for the palette scale.
#' @param breaks the z values for breaks in the colour scheme.
#' @param col either a vector of colours corresponding to the breaks, of length
#' 1 less than the number of breaks, or a function specifying colours, e.g.
#' \code{\link{oce.colorsJet}} for a rainbow.
#' @param colormap a colour map as created by \code{\link{colormap}}.  If
#' provided, this takes precedence over \code{breaks} and \code{col}.
#' @param mai margins for palette, as defined in the usual way; see
#' \code{\link{par}}.  If not given, reasonable values are inferred from the
#' existence of a non-blank \code{zlab}.
#' @param cex.axis character-expansion value for text labels
#' @param pos an integer indicating the location of the palette within the
#' plotting area, 1 for near the bottom, 2 for near the left-hand side, 3 for
#' near the top side, and 4 (the default) for near the right-hand side.
#' @param labels optional vector of labels for ticks on palette axis (must
#' correspond with \code{at})
#' @param at optional vector of positions for the \code{label}s
#' @param levels optional contour levels, in preference to \code{breaks}
#' values, to be added to the image if \code{drawContours} is \code{TRUE}.
#' @param drawContours logical value indicating whether to draw contours on the
#' palette, at the colour breaks.
#' @param plot logical value indicating whether to plot the palette, the
#' default, or whether to just alter the margins to make space for where the
#' palette would have gone.  The latter case may be useful in lining up plots,
#' as in example 1 of \dQuote{Examples}.
#' @param fullpage logical value indicating whether to draw the palette filling
#' the whole plot width (apart from \code{mai}, of course).  This can be
#' helpful if the palette panel is to be created with \code{\link{layout}}, as
#' illustrated in the \dQuote{Examples}.
#' @param drawTriangles logical value indicating whether to draw triangles on
#' the top and bottom of the palette.  If a single value is provide, it applies
#' to both ends of the palette.  If a pair is provided, the first refers to the
#' lower range of the palette, and the second to the upper range.
#' @param axisPalette optional replacement function for \code{axis()}, e.g.
#' for exponential notation on large or small values.
#' @param tformat optional format for axis labels, if the variable is a time
#' type (ignored otherwise).
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, or to 2 to get more.
#' @param \dots optional arguments passed to plotting functions.
#' @return None.
#' @section Use with multi-panel plots: An important consequence of the margin
#' adjustment is that multi-panel plots require that the initial margin be
#' stored prior to the first call to \code{drawPalette}, and reset after each
#' palette-plot pair.  This method is illustrated in \dQuote{Examples}.
#' @author Dan Kelley, with help from Clark Richards
#' @seealso This is used by \code{\link{imagep}}.
#' @examples
#'
#' library(oce)
#' par(mgp=getOption("oceMgp"))
#'
#' ## 1. A three-panel plot
#' par(mfrow=c(3, 1), mar=c(3, 3, 1, 1))
#' omar <- par('mar')                 # save initial margin
#'
#' ## 1a. top panel: simple case
#' drawPalette(zlim=c(0, 1), col=oce.colorsJet(10))
#' plot(1:10, 1:10, col=oce.colorsJet(10)[1:10],pch=20,cex=3,xlab='x', ylab='y')
#' par(mar=omar)                      # reset margin
#'
#' ## 1b. middle panel: colormap
#' cm <- colormap(name="gmt_globe")
#' drawPalette(colormap=cm)
#' icol <- seq_along(cm$col)
#' plot(icol, cm$breaks[icol], pch=20, cex=2, col=cm$col,
#'      xlab="Palette index", ylab="Palette breaks")
#' par(mar=omar)                      # reset margin
#'
#' ## 1c. bottom panel: space for palette (to line up graphs)
#' drawPalette(plot=FALSE)
#' plot(1:10, 1:10, col=oce.colorsJet(10)[1:10],pch=20,cex=3,xlab='x', ylab='y')
#' par(mar=omar)                      # reset margin
#'
#' # 2. Use layout to mimic the action of imagep(), with the width
#' # of the palette region being 14 percent of figure width.
#' d <- 0.14
#' layout(matrix(1:2,nrow=1), widths=c(1-d,d))
#' image(volcano, col=oce.colorsJet(100), zlim=c(90, 200))
#' contour(volcano, add=TRUE)
#' drawPalette(c(90, 200), fullpage=TRUE, col=oce.colorsJet)
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
    if (zlimGiven && length(zlim) != 2)
        stop("'zlim' must be of length 2")
    if (zlimGiven && zlim[2] < zlim[1])
        stop("'zlim' must be ordered")
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
    if (colormapGiven && !zlimGiven) {
        zlim <- colormap$zlim
        zlimGiven <- TRUE
        if (zlim[2] <= zlim[1])
            stop("colormap zlim values must be ordered and distinct")
    }
    zIsTime <- zlimGiven && inherits(zlim[1], "POSIXt")
    if (zIsTime) {
        ##zlimOrig <- zlim
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
        ##colGiven <- TRUE
        if (!zlimGiven)
            zlim <- range(breaks, na.rm=TRUE)
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
                        breaks <- seq(zlim[1], zlim[2], length.out=PLEN) # smooth image colour scale
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
    oceDebug(debug, "plot:", plot, "; fullpage:", fullpage, "\n")
    if (plot) {
        if (fullpage)
            par(mai=ifelse(pc$mai1f>0, pc$mai1f, 0))
        else
            par(mai=ifelse(pc$mai1>0, pc$mai1, 0))
        oceDebug(debug, "A. par(mai=c(", paste(round(par('mai'), 1), collapse=","), "))\n")
        oceDebug(debug, "A. par(mar=c(", paste(round(par('mar'), 1), collapse=","), "))\n")
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
                oceDebug(debug, "B. par(mai=c(", paste(round(par('mai'), 1), collapse=","), "))\n")
                oceDebug(debug, "B. x non-margin width: ", par('fin')[1] - par('mai')[2] - par('mai')[4], "\n")
                oceDebug(debug, "B. y non-margin height: ", par('fin')[2] - par('mai')[1] - par('mai')[3], "\n")
                oceDebug(debug, "B. par(mar=c(", paste(round(par('mar'), 1), collapse=","), "))\n")
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
                    warning("horizontal triangles not working yet")
                } else if (pos == 2 || pos == 4) {
                    polygon(c(usr[1], 0.5 * (usr[1]+usr[2]), usr[2]),
                            usr[4] + c(0, triangleHeight, 0), col=col[length(col)],
                            border=col[length(col)], xpd=TRUE)
                    lines(c(usr[1], 0.5 * (usr[1]+usr[2]), usr[2]),
                          usr[4] + c(0, triangleHeight, 0),
                          xpd=TRUE)
                }
            }
            if (drawTriangles[1]) {
                if (pos == 1 || pos == 3) {
                    warning("horizontal triangles not working yet")
                } else if (pos == 2 || pos == 4) {
                    polygon(c(usr[1], 0.5 * (usr[1]+usr[2]), usr[2]),
                            usr[3] + c(0, -triangleHeight, 0), col=col[1],
                            border=col[1], xpd=TRUE)
                    lines(c(usr[1], 0.5 * (usr[1]+usr[2]), usr[2]),
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
            axis(side=1, at=at, labels=labels, mgp=c(2.5, 0.7, 0), cex.axis=cex.axis)
            if (haveZlab) mtext(zlab, side=1, line=getOption("oceMgp")[1],
                                cex=par('cex'), cex.axis=cex.axis)
        } else if (pos == 2) {
            axis(side=2, at=at, labels=labels, mgp=c(2.5, 0.7, 0), cex.axis=cex.axis)
            if (haveZlab) mtext(zlab, side=2, line=getOption("oceMgp")[1],
                                cex=par('cex'), cex.axis=cex.axis)
        } else if (pos == 3) {
            axis(side=3, at=at, labels=labels, mgp=c(2.5, 0.7, 0), cex.axis=cex.axis)
            if (haveZlab) mtext(zlab, side=3, line=getOption("oceMgp")[1],
                                cex=par('cex'), cex.axis=cex.axis)
        } else if (pos == 4) {
            axis(side=4, at=at, labels=labels, mgp=c(2.5, 0.7, 0), cex.axis=cex.axis)
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


#' Plot an Image with a Color Palette
#'
#' Plot an image with a colour palette, in a way that does not conflict with
#' \code{\link{par}(mfrow)} or \code{\link{layout}}.  To plot just a palette,
#' e.g. to get an x-y plot with points coloured according to a palette, use
#' \code{\link{drawPalette}} and then draw the main diagram.
#'
#' @details
#' By default, creates an image with a colour palette to the right.  The effect is similar to
#' \code{\link{filled.contour}} except that with \code{imagep} it is possible to
#' set the \code{\link{layout}} outside the function, which enables the creation
#' of plots with many image-palette panels.  Note that the contour lines may not
#' coincide with the colour transitions, in the case of coarse images.
#'
#' Note that this does not use \code{\link{layout}} or any of the other screen
#' splitting methods.  It simply manipulates margins, and draws two plots
#' together.  This lets users employ their favourite layout schemes.
#'
#' NOTE: \code{imagep} is an analogue of \code{\link{image}}, and from that
#' it borrows a the convention that the number of rows in the matrix corresponds to
#' to \code{x} axis, not the \code{y} axis.  (Actually, \code{\link{image}} permits
#' the length of \code{x} to match either \code{nrow(z)} or \code{1+nrow(z)}, but
#' here only the first is permitted.)
#'
#' @param x,y These have different meanings in different modes of operation.
#'         \emph{Mode 1.} One
#'         mode has them meaning the locations of coordinates along which values
#'         matrix \code{z} are defined. In this case, both \code{x} and \code{y}
#'         must be supplied and, within each, the values must be finite and
#'         distinct; if values are out of order, they (and \code{z}) will be
#'         transformed to put them in order.
#'         ordered in a matching way).  \emph{Mode 2.}
#'         If \code{z} is provided but not \code{x} and \code{y}, then the latter
#'         are constructed to
#'         indicate the indices of the matrix, in contrast
#'         to the range of 0 to 1, as is the case for \code{\link{image}}.
#'         \emph{Mode 3.} If
#'         \code{x} is a list, its components \code{x$x} and \code{x$y} are used
#'         for \code{x} and \code{y}, respectively. If the list has component
#'         \code{z} this is used for \code{z}. (NOTE: these arguments are meant to
#'         mimic those of \code{\link{image}}, which explains the same description
#'         here.)  \emph{Mode 4.} There are also some special cases, e.g. if \code{x} is a
#'         topographic object such as can be created with \code{\link{read.topo}}
#'         or \code{\link{as.topo}}, then longitude and latitude are used for
#'         axes, and topographic height is drawn.
#'
#' @param z A matrix containing the values to be plotted (NAs are allowed). Note
#'         that x can be used instead of z for convenience. (NOTE: these arguments
#'         are meant to mimic those of \code{\link{image}}, which explains the same
#'         description here.)
#' @param  xlim,ylim Limits on x and y axes.
#' @param  zlim If missing, the z scale is determined by the range of the data.
#'         If provided, \code{zlim} may take several forms. First, it may be a pair
#'         of numbers that specify the limits for the colour scale.  Second,
#'         it could be the string \code{"histogram"}, to yield a flattened
#'         histogram (i.e. to increase contrast). Third, it could be the
#'         string \code{"symmetric"}, to yield limits that are symmetric
#'         about zero, which can be helpful in drawing velocity fields,
#'         for which a zero value has a particular meaning (in which case,
#'         a good colour scheme might be \code{col=\link{oceColorsTwo}}).
#' @param  zclip Logical, indicating whether to clip the colours to those
#'         corresponding to \code{zlim}. This only works if \code{zlim} is
#'         provided. Clipped regions will be coloured with \code{missingColor}.
#'         Thus, clipping an image is somewhat analogous to clipping in an xy
#'         plot, with clipped data being ignored, which in an image means to be be
#'         coloured with \code{missingColor}.
#' @param  flipy Logical, with \code{TRUE} indicating that the image
#'         should be flipped top to bottom (e.g. to produce a profile image
#'         for a downward-looking acoustic-doppler profile).
#' @param  xlab,ylab,zlab Names for x axis, y axis, and the image values.
#' @param  zlabPosition String indicating where to put the label for the z axis,
#'         either at the top-right of the main image, or on the side, in the axis
#'         for the palette.
#' @param  decimate Controls whether the image will be decimated before plotting,
#'         in three possible cases. \strong{Case 1.}
#'         If \code{decimate=FALSE} then every grid cell in the matrix will
#'         be represented by a pixel in the image. \strong{Case 2 (the default).}
#'         If \code{decimate=TRUE}, then decimation will be done
#'         in the horizontal or vertical direction (or both) if the length of the
#'         corresponding edge of the \code{z} matrix exceeds 800. (This also creates
#'         a warning message.) The decimation
#'         factor is computed as the integet just below the ratio of \code{z} dimension
#'         to 400. Thus, no decimation is done if the dimension is less than 800,
#'         but if the dimension s between 800 and 1199, only every second grid
#'         point is mapped to a pixel in the image.  \strong{Case 3.}
#'         If \code{decimate} is an integer, then that \code{z} is subsampled
#'         at \code{seq.int(1L, dim(z)[1], by=decimate)} (as is \code{x}), and
#'         the same is done for the \code{y} direction. \strong{Case 4.} If
#'         \code{decimate} is a vector of two integers, the first is used for
#'         the first index of \code{z}, and the second is used for the second
#'         index.
#' @param  breaks The z values for breaks in the colour scheme.  If this is of
#'         length 1, the value indicates the desired number of breaks, which is
#'         supplied to \code{\link{pretty}}, in determining clean break points.
#' @param  col Either a vector of colours corresponding to the breaks, of length
#'         1 plus the number of breaks, or a function specifying colours,
#'         e.g. \code{\link{oce.colorsJet}} for a rainbow.
#' @param  colormap A colour map as created by \code{\link{colormap}}.  If
#'         provided, then \code{colormap$breaks} and \code{colormap$col} take
#'         precedence over the present arguments \code{breaks} and \code{col}.
#'         (All of the other contents of \code{colormap} are ignored, though.)
#' @param  labels Optional vector of labels for ticks on palette axis (must
#'         correspond with \code{at}).
#' @param  at Optional vector of positions for the \code{label}s.
#' @param  drawContours Logical value indicating whether to draw contours on the
#'         image, and palette, at the colour breaks.  Images with a great deal of
#'         high-wavenumber variation look poor with contours.
#' @param  tformat Optional argument passed to \code{\link{oce.plot.ts}}, for
#'         plot types that call that function.  (See \code{\link{strptime}} for the
#'         format used.)
#' @param  drawTimeRange Logical, only used if the \code{x} axis is a
#'         time.  If \code{TRUE}, then an indication of the time range of the
#'         data (not the axis) is indicated at the top-left margin of the
#'         graph.  This is useful because the labels on time axes only indicate
#'         hours if the range is less than a day, etc.
#' @param  drawPalette Indication of the type of palette to draw, if any.  If
#'         \code{drawPalette=TRUE}, a palette is drawn at the right-hand side of the
#'         main image.  If \code{drawPalette=FALSE}, no palette is drawn, and the
#'         right-hand side of the plot has a thin margin.  If
#'         \code{drawPalette="space"}, then no palette is drawn, but space is put in
#'         the right-hand margin to occupy the region in which the palette would
#'         have been drawn.  This last form is useful for producing stacked plots
#'         with uniform left and right margins, but with palettes on only some of
#'         the images.
#' @param  drawTriangles Logical value indicating whether to draw
#'         triangles on the top and bottom of the palette.  This is passed to
#'         \code{\link{drawPalette}}.
#' @param  filledContour Boolean value indicating whether to use filled
#'         contours to plot the image.
#' @param  missingColor A colour to be used to indicate missing data, or
#'         \code{NULL} for transparent (to see this, try setting
#'         \code{par("bg")<-"red"}).
#' @param  mgp A 3-element numerical vector to use for \code{par(mgp)}, and
#'         also for \code{par(mar)}, computed from this.  The default is
#'         tighter than the R default, in order to use more space for the
#'         data and less for the axes.
#' @param  mar A 4-element Value to be used with \code{\link{par}("mar")}.  If not
#'         given, a reasonable value is calculated based on whether \code{xlab} and
#'         \code{ylab} are empty strings.
#' @param  mai.palette Palette margin corrections (in inches), added to the
#'         \code{mai} value used for the palette.  Use with care.
#' @param  xaxs Character indicating whether image should extend to edge
#'         of x axis (with value \code{"i"}) or not; see
#'         \code{\link[graphics]{par}}("xaxs").
#' @param  yaxs As \code{xaxs} but for y axis.
#' @param asp Aspect ratio of the plot, as for \code{\link{plot.default}}. If
#'        \code{x} inherits from \code{\link{topo-class}} and \code{asp=NA} (the
#'        default) then \code{asp} is redefined to be the reciprocal of the
#'        mean latitude in \code{x}, as a way to reduce geographical distortion.
#'        Otherwise, if \code{asp} is not \code{NA}, then it is used directly.
#' @param  cex Size of labels on axes and palette; see \code{\link[graphics]{par}}("cex").
#'
#' @template adornTemplate
#'
#' @param  axes Logical, set \code{TRUE} to get axes on the main image.
#' @param  main Title for plot.
#' @param  axisPalette Optional replacement function for \code{axis()}, passed to
#'         \code{\link{drawPalette}}.
#'
#' @param add Logical value indicating whether to add to an existing plot.
#' The default value, \code{FALSE} indicates that a new plot is to be created.
#' However, if \code{add} is \code{TRUE}, the idea is to add an image (but not
#' its palette or its axes) to an existing plot. Clearly, then, arguments
#' such \code{xlim} are to be ignored. Indeed, if \code{add=TRUE}, the only
#' arguments examined are \code{x} (which must be a vector; the mode of providing
#' a matrix or \code{oce} object does not work), \code{y}, \code{z},
#' \code{decimate}, plus either \code{colormap} or
#' both \code{breaks} and \code{col}.
#'
#' @param  debug A flag that turns on debugging.  Set to 1 to get a
#'         moderate amount of debugging information, or to 2 to get more.
#' @param  \dots Optional arguments passed to plotting functions.
#'
#' @return A list is silently returned, containing \code{xat} and \code{yat},
#'     values that can be used by \code{\link{oce.grid}} to add a grid to the
#'     plot.
#'
#' @seealso This uses \code{\link{drawPalette}}, and is used by \code{\link{plot,adp-method}},
#' \code{\link{plot,landsat-method}}, and other image-generating functions.
#'
#' @section Note for RStudio/OSX users:
#' On OSX computers, some versions of RStudio produce a margin-size error when
#' \code{imagep} is called. The problem is not isolated to \code{imagep};
#' it occurs with other packages, and a web
#' search reveals repeated bug reports submitted to RStudio.
#' The problem seems to come and go, as RStudio evolves. In the
#' \code{imagep} case, things worked properly for
#' RStudio version 0.99.451 (released late in 2015), but not
#' for version 0.99.878 (released early
#' in 2016). A bug report was sent to RStudio in
#' January 2016, with a minimal example that boiled the issue
#' down to a few lines of basic R code (not using \code{imagep}
#' or even \code{oce}).
#' Although communications with RStudio gave
#' reason for optimism, the problem persisted in version 0.99.892,
#' released March 4. New versions of RStudio will be checked as they
#' come out, with status updates here.
#' Pending an RStudio solution, users can avoid the error
#' simply by opening
#' a new (and separate) plotting window with \code{\link{dev.new}}.
#' In doing so, they may find that this is preferable generally,
#' given the limitations of one-window interfaces.
#'
#' @examples
#' library(oce)
#'
#' # 1. simplest use
#' imagep(volcano)
#'
#' # 2. something oceanographic (internal-wave speed)
#' h <- seq(0, 50, length.out=100)
#' drho <- seq(1, 3, length.out=200)
#' speed <- outer(h, drho, function(drho, h) sqrt(9.8 * drho * h / 1024))
#' imagep(h, drho, speed, xlab="Equivalent depth [m]",
#' ylab=expression(paste(Delta*rho, " [kg/m^3]")),
#' zlab="Internal-wave speed [m/s]")
#'
#' # 3. fancy labelling on atan() function
#' x <- seq(0, 1, 0.01)
#' y <- seq(0, 1, 0.01)
#' angle <- outer(x,y,function(x,y) atan2(y,x))
#' imagep(x, y, angle, filledContour=TRUE, breaks=c(0, pi/4, pi/2),
#'        col=c("lightgray", "darkgray"),
#'        at=c(0, pi/4, pi/2),
#'        labels=c(0, expression(pi/4), expression(pi/2)))
#'
#' # 4. a colormap case
#' data(topoWorld)
#' cm <- colormap(name="gmt_globe")
#' imagep(topoWorld, colormap=cm)
#'
#' @author Dan Kelley and Clark Richards
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
                   xaxs="i", yaxs="i",
                   asp=NA,
                   cex=par("cex"),
                   adorn=NULL,
                   axes=TRUE,
                   main="",
                   axisPalette,
                   add=FALSE,
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

    if (is.logical(add)) {
        if (add) {
            if (missing(x)) stop("must give 'x'")
            if (missing(y)) stop("must give 'y'")
            if (missing(z)) stop("must give 'z'")
            if (missing(colormap)) {
                if (missing(breaks)) stop("must give 'breaks'")
                if (missing(col)) stop("must give 'col'")
            } else {
                breaks <- colormap$breaks
                col <- colormap$col
            }
            oceDebug(debug, "decimate: ", paste(decimate, collapse=" "), " (before calculation)\n")
            if (is.logical(decimate)) {
                decimate <- as.integer(dim(z) / 400)
                decimate <- ifelse(decimate < 1, 1, decimate)
            } else {
                decimate <- rep(as.numeric(decimate), length.out=2)
            }
            oceDebug(debug, "decimate: ", paste(decimate, collapse=" "), " (after calculation)\n")
            ix <- seq(1L, length(x), by=decimate[1])
            iy <- seq(1L, length(y), by=decimate[2])
            if (is.function(col))
                col <- col(n=length(breaks)-1)
            image(x[ix], y[iy], z[ix, iy], breaks=breaks, col=col, add=TRUE)
            return(invisible(list(xat=NULL, yat=NULL, decimate=decimate)))
        }
    } else {
        stop("'add' must be a logical value")
    }

    if (!is.null(adorn))
        warning("In imagep() : the 'adorn' argument is defunct, and will be removed soon", call.=FALSE)
    xlimGiven <- !missing(xlim)
    ylimGiven <- !missing(ylim)
    zlimGiven <- !missing(zlim) && !is.null(zlim) # latter is used by plot,adp-method
    xlimGiven <- !missing(xlim)
    if (zlimGiven && is.character(zlim)) {
        if ("symmetric" == zlim) {
            zlim <- c(-1, 1) * max(abs(z), na.rm=TRUE)
        }
    }
    breaksGiven <- !missing(breaks)
    if (zlimGiven && breaksGiven && length(breaks) > 1)
        stop("cannot specify both zlim and breaks, unless length(breaks)==1")

    xat <- NULL
    yat <- NULL

    ## issue 674: permit POSIXlt in addition to POSIXct
    if (inherits(x, "POSIXt"))
        x <- as.POSIXct(x)

    ##haveZlab <- !is.null(zlab) && sum(nchar(zlab)) > 0
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
        z <- if (length(dim(x)) > 2) z <- x[, , 1] else x
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
        if (is.na(asp))
            asp <- 1 / cos(mean(y * pi / 180))
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
    decimateLogical <- is.logical(decimate)
    if (decimateLogical) { # this block makes decimate be a vector of length 2
        ## message("decimate is logical; decimate:", decimate)
        if (decimate) {
            cdim <- dim                    # (possibly) clipped dim
            ## issue 827: decide whether to decimate based on just the data
            ## within the plot window.
            if (xlimGiven) {
                nx <- cdim[1]
                if (length(x) > 1) {
                    nx <- if (x[2] > x[1]) sum(xlim[1] <= x & x <= xlim[2])
                        else sum(xlim[2] <= x & x <= xlim[1])
                }
                if (nx < cdim[1])
                    cdim[1] <- nx
            }
            if (ylimGiven) {
                ny <- cdim[2]
                if (length(y) > 1) {
                    ny <- if (y[2] > y[1]) sum(ylim[1] <= y & y <= ylim[2])
                        else sum(ylim[2] <= y & y <= ylim[1])
                }
                if (ny < cdim[2])
                    cdim[2] <- ny
            }
            ## message("nx: ", nx, ", ny: ", ny)
            ## message("cdim: ", cdim[1], " ", cdim[2])
            decimate <- as.integer(cdim / 400)
            decimate <- ifelse(decimate < 1, 1, decimate)
            ## message("decimate: ", decimate[1], " ", decimate[2])
            oceDebug(debug, "set auto decimation=", paste(decimate, collapse=" "), "\n")
        } else {
            decimate <- c(1L, 1L)
        }
    }
    if (1 == length(decimate))
        decimate <- rep(decimate, 2)
    ##> message("dim(z): ", paste(dim(z), collapse=" "))
    oceDebug(debug, "decimation: ", paste(decimate, collapse=" "), "\n")
    if (decimate[1] > 1) {
        ilook <- seq.int(1, dim[1], by=decimate[1])
        x <- x[ilook]
        z <- z[ilook, ]
        oceDebug(debug, "ilook:", paste(ilook[1:4], collapse=" "), "...\n")
        if (decimateLogical)
            warning("auto-decimating first index of large image by ", decimate[1], "; use decimate=FALSE to prevent this")
    }
    if (decimate[2] > 1) {
        jlook <- seq.int(1, dim[2], by=decimate[2])
        y <- y[jlook]
        z <- z[, jlook]
        oceDebug(debug, "jlook:", paste(jlook[1:4], collapse=" "), "...\n")
        if (decimateLogical)
            warning("auto-decimating second index of large image by ", decimate[2], "; use decimate=FALSE to prevent this")
    }
    ##> message("dim(z): ", paste(dim(z), collapse=" "))
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
        z <- z[, oy]
    }

    ## Adjust x and y, to match what image() does; save orig for filled contours
    xorig <- x
    yorig <- y
    tz <- attr(x[1], "tzone")
    if (length(x) > 1 && length(x) == nrow(z)) {
        dx <- 0.5 * diff(x)
        x <- c(x[1L] - dx[1L], x[-length(x)] + dx, x[length(x)] + dx[length(x) - 1])
    }
    if (length(y) > 1 && length(y) == ncol(z)) {
        dy <- 0.5 * diff(y)
        y <- c(y[1L] - dy[1L], y[-length(y)] + dy, y[length(y)] + dy[length(y) - 1])
    }
    attr(x, 'tzone') <- tz
    ##omai <- par("mai")
    ocex <- par("cex")
    if (missing(mar))
        mar <- c(mgp[1]+if (nchar(xlab)>0) 1.5 else 1, mgp[1]+if (nchar(ylab)>0) 1.5 else 1, mgp[2]+1/2, 1/2)
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
                nbreaks <- 128                 # smooth image colour scale
                if (missing(zlim)) {
                    if (missing(col)) {
                        breaks <- pretty(zrange, n=nbreaks)
                        if (breaks[1] < zrange[1])
                            breaks[1] <- zrange[1]
                        if (breaks[length(breaks)] > zrange[2])
                            breaks[length(breaks)] <- zrange[2]
                    } else {
                        breaks <- seq(zrange[1], zrange[2], length.out=if (is.function(col))PLEN else 1+length(col))
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
                                      length.out=if (is.function(col))PLEN else 1+length(col))
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
    xlim <- if (!xlimGiven) range(x, na.rm=TRUE) else xlim
    ylim <- if (!ylimGiven) range(y, na.rm=TRUE) else ylim
    ## oceDebug(debug, "zlimGiven: ", zlimGiven, "\n")
    ## zlim <- if (missing(zlim)) range(z, na.rm=TRUE) else zlim
    ## oceDebug(debug, "zlim=c(", paste(zlim, collapse=","), ")\n", sep="")


    if (drawPalette == "space") {
        drawPalette(zlab=if (zlabPosition=="side") zlab else "", axisPalette=axisPalette, debug=debug-1)
    } else if (drawPalette) {
        ## issue 542: put this above the block
        ## if (missing(zlim)) {
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
            drawPalette(zlim=c(0, 1), zlab=if (zlabPosition=="side") zlab else "",
                        col=col,
                        labels=labels, at=seq(0, 1, 0.1),
                        drawContours=drawContours,
                        drawTriangles=drawTriangles,
                        mai=mai.palette, debug=debug-1)
        } else {
            oceDebug(debug, "palette with zlim not \"histogram\"\n")
            drawPalette(zlim=zlim, zlab=if (zlabPosition=="side") zlab else "",
                        breaks=breaks, col=col,
                        labels=labels, at=at,
                        drawContours=drawContours,
                        drawTriangles=drawTriangles,
                        mai=mai.palette,
                        axisPalette=axisPalette,
                        debug=debug-1)
        }
    }

    ## xlim <- if (missing(xlim)) range(x, na.rm=TRUE) else xlim
    ## ylim <- if (missing(ylim)) range(y, na.rm=TRUE) else ylim
    ## oceDebug(debug, "zlimGiven: ", zlimGiven, "\n")
    ## zlim <- if (missing(zlim)) range(z, na.rm=TRUE) else zlim
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
            plot.window(xlim=xlim, ylim=ylim, xaxs=xaxs, yaxs=yaxs, asp=asp, ...)
            ## Filled contours became official in version 2.15.0 of R.
            ## issue 489: use breaks/col instead of breaks2/col2
            #.filled.contour(as.double(xorig), as.double(yorig), z, as.double(breaks2), col=col2)
            .filled.contour(as.double(xorig), as.double(yorig), z, as.double(breaks), col=col)
            mtext(ylab, side=2, line=par('mgp')[1])
        } else {
            oceDebug(debug, "not doing filled contours [2]\n")
            if (zlimHistogram) {
                image(x=x, y=y, z=z, axes=FALSE, xlab="", ylab=ylab, col=col2,
                      xlim=xlim, ylim=ylim, zlim=c(0, 1), asp=asp, add=add, ...)
            } else {
                ## issue 489: use breaks/col instead of breaks2/col2
                ##image(x=x, y=y, z=z, axes=FALSE, xlab="", ylab=ylab, breaks=breaks2, col=col2,
                image(x=x, y=y, z=z, axes=FALSE, xlab="", ylab=ylab, breaks=breaks, col=col,
                  xlim=xlim, ylim=ylim, zlim=zlim, asp=asp, add=add, ...)
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
            plot.window(xlim=xlim, ylim=ylim, xaxs=xaxs, yaxs=yaxs, asp=asp, ...)
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
                  xlim=xlim, ylim=ylim, asp=asp, ...)
        }
        if (axes) {
            box()
            xat <- axis(1)#, cex.axis=cex, cex.lab=cex)
            yat <- axis(2)#, cex.axis=cex, cex.lab=cex)
        }
    }
    if (!is.null(missingColor)) {
        ## FIXME: the negation on is.na is confusing, but it comes from col and breaks together
        image(x, y, !is.na(z), col=c(missingColor, "transparent"), breaks=c(0, 1/2, 1), add=TRUE)
        if (axes)
            box()
    }
    if (is.na(main)) {
        mtext("", at=mean(range(x), na.rm=TRUE), side=3, line=1/8, cex=par("cex"))
    } else if (!(is.character(main) && main == "")) {
        mtext(main, at=mean(range(x), na.rm=TRUE), side=3, line=1/8, cex=par("cex"))
    }
    if (drawContours) {
        oceDebug(debug, "adding contours\n")
        contour(x=xorig, y=yorig, z=z, levels=breaks, drawlabels=FALSE, add=TRUE, col="black")
    }
    if (zlabPosition == "top")
        mtext(zlab, side=3, cex=par("cex"), adj=1, line=1/8)
    if (!missing(adorn)) {
        t <- try(eval.parent(adorn), silent=!TRUE)
        if (class(t) == "try-error")
            warning("cannot evaluate adorn='", adorn, "'")
    }
    par(cex=ocex)
    oceDebug(debug, "par('mai')=c(",
             paste(format(par('mai'), digits=2), collapse=","), "); par('mar')=c(",
             paste(format(par('mar'), digits=2), collapse=","), ")\n", sep='')
    oceDebug(debug, "} # imagep()\n", unindent=1)
    invisible(list(xat=xat, yat=yat, decimate=decimate))
}
