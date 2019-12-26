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
#'
#' @param \dots optional arguments passed to the [format()], e.g.
#' `format`.
#'
#' @return None.
#'
#' @author Dan Kelley, with help from Clark Richards
#'
#' @seealso This is used by various functions that draw time labels on axes,
#' e.g.  [plot,adp-method()].
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
#' In the normal use, [drawPalette()] draws an image palette near the
#' right-hand side of the plotting device, and then adjusts the global margin
#' settings in such a way as to cause the next plot to appear (with much larger
#' width) to the left of the palette. The function can also be used, if
#' `zlim` is not provided, to adjust the margin without drawing anything;
#' this is useful in lining up the x axes of a stack of plots, some some of
#' which will have palettes and others not.
#'
#' The plot positioning is done entirely with margins, not with
#' `par(mfrow)` or other R schemes for multi-panel plots.  This means that
#' the user is free to use those schemes without worrying about nesting or
#' conflicts.
#'
#' @param zlim two-element vector containing the lower and upper limits of z.
#' This may also be a vector of any length exceeding 1, in which case its range
#' is used.
#'
#' @param zlab label for the palette scale.
#'
#' @param breaks the z values for breaks in the color scheme.
#'
#' @param col either a vector of colors corresponding to the breaks, of length
#' 1 less than the number of breaks, or a function specifying colors, e.g.
#' [oce.colorsJet()] for a rainbow.
#'
#' @param colormap a color map as created by [colormap()].  If
#' provided, this takes precedence over `breaks` and `col`.
#'
#' @param mai margins for palette, as defined in the usual way; see
#' [par()].  If not given, reasonable values are inferred from the
#' existence of a non-blank `zlab`.
#'
#' @param cex numeric character expansion value for text labels
#'
#' @param pos an integer indicating the location of the palette within the
#' plotting area, 1 for near the bottom, 2 for near the left-hand side, 3 for
#' near the top side, and 4 (the default) for near the right-hand side.
#'
#' @param las optional argument, passed to [axis()], to control the orientation
#' of numbers along the axis. As explained in the help for [par()], the
#' meaning of `las` is as follows: `las=0` (the default) means to put labels
#' parallel to the axis, `las=1` means horizontal (regardless of
#' axis orientation), `las=2` means perpendicular to the axis,
#' and `las=3` means to vertical (regardless of axis orientation).  Note that
#' the automatic computation of margin spacing parameter `mai`
#' assumes that `las=0`, and so for other cases, the user may need to
#' specify the `mai` argument directly.
#'
#' @param labels optional vector of labels for ticks on palette axis (must
#' correspond with `at`)
#'
#' @param at optional vector of positions for the `label`s
#'
#' @param levels optional contour levels, in preference to `breaks`
#' values, to be added to the image if `drawContours` is `TRUE`.
#'
#' @param drawContours logical value indicating whether to draw contours on the
#' palette, at the color breaks.
#'
#' @param plot logical value indicating whether to plot the palette, the
#' default, or whether to just alter the margins to make space for where the
#' palette would have gone.  The latter case may be useful in lining up plots,
#' as in example 1 of \dQuote{Examples}.
#'
#' @param fullpage logical value indicating whether to draw the palette filling
#' the whole plot width (apart from `mai`, of course).  This can be
#' helpful if the palette panel is to be created with [layout()], as
#' illustrated in the \dQuote{Examples}.
#'
#' @param drawTriangles logical value indicating whether to draw triangles on
#' the top and bottom of the palette.  If a single value is provide, it applies
#' to both ends of the palette.  If a pair is provided, the first refers to the
#' lower range of the palette, and the second to the upper range.
#'
#' @param axisPalette optional replacement function for `axis()`, e.g.
#' for exponential notation on large or small values.
#'
#' @param tformat optional format for axis labels, if the variable is a time
#' type (ignored otherwise).
#'
## @param cex.zlab character expansion factor for z label
#'
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, or to 2 to get more.
#'
#' @param \dots optional arguments passed to plotting functions.
#'
#' @return None.
#'
#' @section Use with multi-panel plots: An important consequence of the margin
#' adjustment is that multi-panel plots require that the initial margin be
#' stored prior to the first call to [drawPalette()], and reset after each
#' palette-plot pair.  This method is illustrated in \dQuote{Examples}.
#'
#' @author Dan Kelley, with help from Clark Richards
#'
#' @seealso This is used by [imagep()].
#'
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
                        mai, cex=par("cex"),
                        pos=4, las=0,
                        labels=NULL, at=NULL,
                        levels, drawContours=FALSE,
                        plot=TRUE, fullpage=FALSE, drawTriangles=FALSE,
                        axisPalette, tformat,
                        debug=getOption("oceDebug"), ...)
{
    oceDebug(debug, "drawPalette(",
             argShow(zlab),
             argShow(zlim),
             argShow(cex),
             argShow(mai),
             argShow(breaks),
             argShow(fullpage),
             "...) {\n", sep="", unindent=1, style="bold")
    zlimGiven <- !missing(zlim)
    if (zlimGiven && length(zlim) != 2)
        stop("'zlim' must be of length 2")
    if (zlimGiven && zlim[2] < zlim[1])
        stop("'zlim' must be ordered")
    colormapGiven <- !missing(colormap)
    ## oceDebug(debug, "colormapGiven=", colormapGiven, "\n", sep="")
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
    maiGiven <- !missing(mai)
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
                        breaks <- seq(zlim[1], zlim[2], length.out=PLEN) # smooth image color scale
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
            oceDebug(debug, "drawing palette image, with ", vectorShow(par('mai')))
            oceDebug(debug, "palette image width =",
                     par('fin')[1] - par('mai')[2] - par('mai')[4], "in\n")
            oceDebug(debug, "palette image height =",
                     par('fin')[2] - par('mai')[1] - par('mai')[3], "in\n")
            oceDebug(debug, vectorShow(par('pin')))
            oceDebug(debug, vectorShow(par('fin')))
            if (pos == 1 || pos == 3) {
                image(x=palette, y=1, z=matrix(palette, ncol=1), axes=FALSE, xlab="", ylab="",
                      breaks=breaksOrig, col=col, zlim=zlim)
            } else if (pos == 2 || pos == 4) {
                ##message("in drawPalette(), breaks and col follow:");
                ##str(breaksOrig)
                ##str(col)
                oceDebug(debug, vectorShow(par('mai')))
                oceDebug(debug, "B. x non-margin width: ", par('fin')[1] - par('mai')[2] - par('mai')[4], "\n")
                oceDebug(debug, "B. y non-margin height: ", par('fin')[2] - par('mai')[1] - par('mai')[3], "\n")
                oceDebug(debug, vectorShow(par('mar')))
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
        oceDebug(debug, "about to label palette zlab='", zlab, "' with cex=", cex, style="blue")
        if (pos == 1) {
            axis(side=1, at=at, labels=labels, mgp=c(2.5, 0.7, 0), cex.axis=cex, las=las)
            if (haveZlab) mtext(zlab, side=1, line=getOption("oceMgp")[1], cex=cex)
        } else if (pos == 2) {
            axis(side=2, at=at, labels=labels, mgp=c(2.5, 0.7, 0), cex.axis=cex, las=las)
            if (haveZlab) mtext(zlab, side=2, line=getOption("oceMgp")[1], cex=cex)
        } else if (pos == 3) {
            axis(side=3, at=at, labels=labels, mgp=c(2.5, 0.7, 0), cex.axis=cex, las=las)
            if (haveZlab) mtext(zlab, side=3, line=getOption("oceMgp")[1], cex=cex)
        } else if (pos == 4) {
            axis(side=4, at=at, labels=labels, mgp=c(2.5, 0.7, 0), cex.axis=cex, las=las)
            if (haveZlab) mtext(zlab, side=4, line=getOption("oceMgp")[1], cex=cex)
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
    oceDebug(debug, "just before drawPalette() returns\n")
    oceDebug(debug, vectorShow(par("mar")), style="blue")
    oceDebug(debug, vectorShow(par("mai")), style="blue")
    oceDebug(debug, "} # drawPalette()\n", unindent=1, style="bold")
    invisible()
}


#' Plot an Image with a Color Palette
#'
#' Plot an image with a color palette, in a way that does not conflict with
#' [`par`]`("mfrow")` or [layout()].  To plot just a palette,
#' e.g. to get an x-y plot with points colored according to a palette, use
#' [drawPalette()] and then draw the main diagram.
#'
#' @details
#' By default, creates an image with a color palette to the right.  The effect is similar to
#' [filled.contour()] except that with `imagep` it is possible to
#' set the [layout()] outside the function, which enables the creation
#' of plots with many image-palette panels.  Note that the contour lines may not
#' coincide with the color transitions, in the case of coarse images.
#'
#' Note that this does not use [layout()] or any of the other screen
#' splitting methods.  It simply manipulates margins, and draws two plots
#' together.  This lets users employ their favourite layout schemes.
#'
#' NOTE: `imagep` is an analogue of [image()], and from that
#' it borrows a the convention that the number of rows in the matrix corresponds to
#' to `x` axis, not the `y` axis.  (Actually, [image()] permits
#' the length of `x` to match either `nrow(z)` or `1+nrow(z)`, but
#' here only the first is permitted.)
#'
#' @param x,y These have different meanings in different modes of operation.
#'         *Mode 1.* One
#'         mode has them meaning the locations of coordinates along which values
#'         matrix `z` are defined. In this case, both `x` and `y`
#'         must be supplied and, within each, the values must be finite and
#'         distinct; if values are out of order, they (and `z`) will be
#'         transformed to put them in order.
#'         ordered in a matching way).  *Mode 2.*
#'         If `z` is provided but not `x` and `y`, then the latter
#'         are constructed to
#'         indicate the indices of the matrix, in contrast
#'         to the range of 0 to 1, as is the case for [image()].
#'         *Mode 3.* If
#'         `x` is a list, its components `x$x` and `x$y` are used
#'         for `x` and `y`, respectively. If the list has component
#'         `z` this is used for `z`. (NOTE: these arguments are meant to
#'         mimic those of [image()], which explains the same description
#'         here.)  *Mode 4.* There are also some special cases, e.g. if `x` is a
#'         topographic object such as can be created with [read.topo()]
#'         or [as.topo()], then longitude and latitude are used for
#'         axes, and topographic height is drawn.
#'
#' @param z A matrix containing the values to be plotted (NAs are allowed). Note
#'         that x can be used instead of z for convenience. (NOTE: these arguments
#'         are meant to mimic those of [image()], which explains the same
#'         description here.)
#' @param xlim,ylim Limits on x and y axes.
#' @param zlim If missing, the z scale is determined by the range of the data.
#'         If provided, `zlim` may take several forms. First, it may be a pair
#'         of numbers that specify the limits for the color scale.  Second,
#'         it could be the string `"histogram"`, to yield a flattened
#'         histogram (i.e. to increase contrast). Third, it could be the
#'         string `"symmetric"`, to yield limits that are symmetric
#'         about zero, which can be helpful in drawing velocity fields,
#'         for which a zero value has a particular meaning (in which case,
#'         a good color scheme might be `col=oceColorsTwo`).
#' @param zclip Logical, indicating whether to clip the colors to those
#'         corresponding to `zlim`. This only works if `zlim` is
#'         provided. Clipped regions will be colored with `missingColor`.
#'         Thus, clipping an image is somewhat analogous to clipping in an xy
#'         plot, with clipped data being ignored, which in an image means to be be
#'         colored with `missingColor`.
#'
#' @param flipy Logical, with `TRUE` indicating that the graph
#' should have the y axis reversed, i.e. with smaller values at
#' the bottom of the page. (*Historical note:* until 2019 March 26,
#' the meaning of `flipy` was different; it meant to reverse the
#' range of the y axis, so that if `ylim` were given as a reversed
#' range, then setting `flipy=TRUE` would reverse the flip, yielding
#' a conventional axis with smaller values at the bottom.)
#'
#' @param xlab,ylab,zlab Names for x axis, y axis, and the image values.
#'
#' @param zlabPosition String indicating where to put the label for the z axis,
#'         either at the top-right of the main image, or on the side, in the axis
#'         for the palette.
#'
#' @param las.palette Parameter controlling the orientation of labels on the
#' image palette, passed as the `las` argument to [drawPalette()].  See the
#' documentation for [drawPalette()] for details.
#'
#' @param decimate Controls whether the image will be decimated before plotting,
#'         in three possible cases.
#'
#' 1. If `decimate=FALSE` then every grid cell in the matrix will
#'    be represented by a pixel in the image.
#'
#' 2. If `decimate=TRUE` (the default), then decimation will be done
#'    in the horizontal or vertical direction (or both) if the length of the
#'    corresponding edge of the `z` matrix exceeds 800. (This also creates
#'    a warning message.) The decimation
#'    factor is computed as the integer just below the ratio of `z` dimension
#'    to 400. Thus, no decimation is done if the dimension is less than 800,
#'    but if the dimension s between 800 and 1199, only every second grid
#'    point is mapped to a pixel in the image.
#'
#' 3. If `decimate` is an integer, then that `z` is subsampled
#'    at `seq.int(1L, dim(z)[1], by=decimate)` (as is `x`), and
#'    the same is done for the `y` direction.
#'
#' 4. If `decimate` is a vector of two integers, the first is used for
#'    the first index of `z`, and the second is used for the second
#'    index.
#'
#' @param breaks The z values for breaks in the color scheme.  If this is of
#'         length 1, the value indicates the desired number of breaks, which is
#'         supplied to [pretty()], in determining clean break points.
#'
#' @param col Either a vector of colors corresponding to the breaks, of length
#'         1 plus the number of breaks, or a function specifying colors,
#'         e.g. [oce.colorsJet()] for a rainbow.
#'
#' @param colormap A color map as created by [colormap()].  If
#'         provided, then `colormap$breaks` and `colormap$col` take
#'         precedence over the present arguments `breaks` and `col`.
#'         (All of the other contents of `colormap` are ignored, though.)
#'
#' @param labels Optional vector of labels for ticks on palette axis (must
#'         correspond with `at`).
#'
#' @param at Optional vector of positions for the `label`s.
#'
#' @param drawContours Logical value indicating whether to draw contours on the
#'         image, and palette, at the color breaks.  Images with a great deal of
#'         high-wavenumber variation look poor with contours.
#'
#' @param tformat Optional argument passed to [oce.plot.ts()], for
#'         plot types that call that function.  (See [strptime()] for the
#'         format used.)
#'
#' @param drawTimeRange Logical, only used if the `x` axis is a
#'         time.  If `TRUE`, then an indication of the time range of the
#'         data (not the axis) is indicated at the top-left margin of the
#'         graph.  This is useful because the labels on time axes only indicate
#'         hours if the range is less than a day, etc.
#'
#' @param drawPalette Indication of the type of palette to draw, if any.  If
#'         `drawPalette=TRUE`, a palette is drawn at the right-hand side of the
#'         main image.  If `drawPalette=FALSE`, no palette is drawn, and the
#'         right-hand side of the plot has a thin margin.  If
#'         `drawPalette="space"`, then no palette is drawn, but space is put in
#'         the right-hand margin to occupy the region in which the palette would
#'         have been drawn.  This last form is useful for producing stacked plots
#'         with uniform left and right margins, but with palettes on only some of
#'         the images.
#'
#' @param drawTriangles Logical value indicating whether to draw
#'         triangles on the top and bottom of the palette.  This is passed to
#'         [drawPalette()].
#'
#' @param filledContour Boolean value indicating whether to use filled
#'         contours to plot the image.
#'
#' @param missingColor A color to be used to indicate missing data, or
#'         `NULL` for transparent (to see this, try setting
#'         `par("bg")<-"red"`).
#'
#' @param useRaster A logical value passed to [image()], in cases
#'        where `filledContour` is `FALSE`. Setting `useRaster=TRUE`
#'        can alleviate some anti-aliasing effects on some plot devices;
#'        see the documentation for [image()].
#'
#' @param mgp A 3-element numerical vector to use for `par(mgp)`, and
#'         also for `par(mar)`, computed from this.  The default is
#'         tighter than the R default, in order to use more space for the
#'         data and less for the axes.
#'
#' @param mar A 4-element Value to be used with [`par`]`("mar")`.  If not
#'         given, a reasonable value is calculated based on whether `xlab` and
#'         `ylab` are empty strings.
#'
#' @param mai.palette Palette margin corrections (in inches), added to the
#'         `mai` value used for the palette.  Use with care.
#'
#' @param xaxs Character indicating whether image should extend to edge
#'         of x axis (with value `"i"`) or not; see
#'         [`par`]`("xaxs")`.
#'
#' @param yaxs As `xaxs` but for y axis.
#'
#' @param asp Aspect ratio of the plot, as for [plot.default()]. If
#'        `x` inherits from [topo-class] and `asp=NA` (the
#'        default) then `asp` is redefined to be the reciprocal of the
#'        mean latitude in `x`, as a way to reduce geographical distortion.
#'        Otherwise, if `asp` is not `NA`, then it is used directly.
#'
#' @param cex numeric character expansion factor, used for `cex.axis`, `cex.lab` and `cex.main`,
#' if those values are not supplied.
#'
#' @param cex.axis,cex.lab,cex.main numeric character expansion factors for axis numbers,
#' axis names and plot titles; see [par()].
#'
#' @param  axes Logical, set `TRUE` to get axes on the main image.
#'
#' @param  main Title for plot.
#'
#' @param  axisPalette Optional replacement function for [axis()], passed to
#'         [drawPalette()].
#'
#' @param add Logical value indicating whether to add to an existing plot.
#' The default value, `FALSE` indicates that a new plot is to be created.
#' However, if `add` is `TRUE`, the idea is to add an image (but not
#' its palette or its axes) to an existing plot. Clearly, then, arguments
#' such `xlim` are to be ignored. Indeed, if `add=TRUE`, the only
#' arguments examined are `x` (which must be a vector; the mode of providing
#' a matrix or `oce` object does not work), `y`, `z`,
#' `decimate`, plus either `colormap` or
#' both `breaks` and `col`.
#'
#' @param  debug A flag that turns on debugging.  Set to 1 to get a
#'         moderate amount of debugging information, or to 2 to get more.
#' @param  \dots Optional arguments passed to plotting functions.
#'
#' @return A list is silently returned, containing `xat` and `yat`,
#'     values that can be used by [oce.grid()] to add a grid to the
#'     plot.
#'
#' @seealso This uses [drawPalette()], and is used by [plot,adp-method()],
#' [plot,landsat-method()], and other image-generating functions.
#'
## @section Note for RStudio/OSX users:
## On OSX computers, some versions of RStudio produce a margin-size error when
## `imagep` is called. The problem is not isolated to `imagep`;
## it occurs with other packages, and a web
## search reveals repeated bug reports submitted to RStudio.
## The problem seems to come and go, as RStudio evolves. In the
## `imagep` case, things worked properly for
## RStudio version 0.99.451 (released late in 2015), but not
## for version 0.99.878 (released early
## in 2016). A bug report was sent to RStudio in
## January 2016, with a minimal example that boiled the issue
## down to a few lines of basic R code (not using `imagep`
## or even `oce`).
## Although communications with RStudio gave
## reason for optimism, the problem persisted in version 0.99.892,
## released March 4. New versions of RStudio will be checked as they
## come out, with status updates here.
## Pending an RStudio solution, users can avoid the error
## simply by opening
## a new (and separate) plotting window with [dev.new()].
## In doing so, they may find that this is preferable generally,
## given the limitations of one-window interfaces.
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
#' # 5. y-axis flipping
#' par(mfrow=c(2,2))
#' data(adp)
#' d <- adp[["distance"]]
#' t <- adp[["time"]]
#' u <- adp[["v"]][ , ,1]
#' imagep(t, d, u, drawTimeRange=FALSE)
#' mtext("normal")
#' imagep(t, d, u, flipy=TRUE, drawTimeRange=FALSE)
#' mtext("flipy")
#' imagep(t, d, u, ylim=rev(range(d)), drawTimeRange=FALSE)
#' mtext("ylim")
#' imagep(t, d, u, ylim=rev(range(d)), flipy=TRUE, drawTimeRange=FALSE)
#' mtext("flipy and ylim")
#' par(mfrow=c(1,1))
#'
#' # 6. a colormap case
#' data(topoWorld)
#' cm <- colormap(name="gmt_globe")
#' imagep(topoWorld, colormap=cm)
#'
#' @author Dan Kelley and Clark Richards
imagep <- function(x, y, z,
                   xlim, ylim, zlim,
                   zclip=FALSE, flipy=FALSE,
                   xlab="", ylab="", zlab="", zlabPosition=c("top", "side"),
                   las.palette=0,
                   decimate=TRUE,
                   breaks, col, colormap, labels=NULL, at=NULL,
                   drawContours=FALSE,
                   drawPalette=TRUE,
                   drawTriangles=FALSE,
                   tformat,
                   drawTimeRange=getOption("oceDrawTimeRange"),
                   filledContour=FALSE,
                   missingColor=NULL,
                   useRaster,
                   mgp=getOption("oceMgp"),
                   mar, mai.palette,
                   xaxs="i", yaxs="i",
                   asp=NA,
                   cex=par("cex"),
                   cex.axis=cex, cex.lab=cex, cex.main=cex,
                   axes=TRUE,
                   main="",
                   axisPalette,
                   add=FALSE,
                   debug=getOption("oceDebug"),
                   ...)
{
    oceDebug(debug, "imagep(x,y,z,",
             argShow(xlab),
             argShow(ylab),
             argShow(ylab),
             argShow(zlim),
             argShow(flipy),
             argShow(cex),
             argShow(cex.axis),
             argShow(cex.lab),
             argShow(cex.main),
             argShow(mgp),
             argShow(mar),
             argShow(mai.palette),
             argShow(breaks),
             style="bold")
    if ("adorn" %in% names(list(...)))
        warning("the 'adorn' argument was removed in November 2017")
    zlabPosition <- match.arg(zlabPosition)
    if (!is.logical(flipy))
        stop("flipy must be TRUE or FALSE")

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
                zlim <- colormap$zlim
                ## FIXME: need to check zclip here too
                zclip <- colormap$zclip
            }
            if (!zclip) {
                oceDebug(debug, "using zlim[1:2]=c(", zlim[1], ",", zlim[2], ") for out-of-range values\n")
                z[z < zlim[1]] <- zlim[1]
                z[z > zlim[2]] <- zlim[2]
            } else {
              oceDebug(debug, "using missingColor for out-of-range values")
                z[z < zlim[1]] <- NA
                z[z > zlim[2]] <- NA
            }
            oceDebug(debug, "decimate: ", paste(decimate, collapse=" "), " (before calculation)\n")
            if (is.logical(decimate)) {
                if (decimate) {
                    decimate <- as.integer(dim(z) / 400)
                    decimate <- ifelse(decimate < 1, 1, decimate)
                } else {
                    decimate <- c(1, 1)
                }
            } else {
                decimate <- rep(as.numeric(decimate), length.out=2)
            }
            oceDebug(debug, "decimate: ", paste(decimate, collapse=" "), " (after calculation)\n")
            ix <- seq(1L, length(x), by=decimate[1])
            iy <- seq(1L, length(y), by=decimate[2])
            if (is.function(col))
                col <- col(n=length(breaks)-1)
            image(x[ix], y[iy], z[ix, iy], breaks=breaks, col=col, useRaster=useRaster, #why useRaster?
                  add=TRUE)
            return(invisible(list(xat=NULL, yat=NULL, decimate=decimate)))
        }
    } else {
        stop("'add' must be a logical value")
    }

    xlimGiven <- !missing(xlim)
    ylimGiven <- !missing(ylim)
    zlimGiven <- !missing(zlim) && !is.null(zlim) # latter is used by plot,adp-method
    ## Guard against poor setup
    if (xlimGiven && length(xlim) != 2) stop("length of xlim must be 2")
    if (ylimGiven && length(ylim) != 2) stop("length of ylim must be 2")
    if (zlimGiven && !(length(zlim) %in% 1:2)) stop("length of zlim must be 1 or 2")

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
    if (decimateLogical) {
        ## this block makes decimate be a vector of length 2
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
        mar <- c(mgp[1]+if (nchar(xlab[1])>0) 1.5 else 1, mgp[1]+if (nchar(ylab[1])>0) 1.5 else 1, mgp[2]+1/2, 1/2)
    oceDebug(debug, "after possibly recalculating, mar=c(", paste(mar, collapse=", "), "); based on mgp=c(", paste(mgp, collapse=","),")\n", sep="")
    if (missing(mai.palette)) {
        ##mai.palette <- c(0, 1/8, 0, 3/8 + if (haveZlab && zlabPosition=="side") 1.5*par('cin')[2] else 0)
        mai.palette <- rep(0, 4)
        oceDebug(debug, "set mai.palette=", mai.palette, "\n")
    }

    oceDebug(debug, "imagep.R:1125 cex=", cex, ", par('cex')=", par('cex'), style="blue")
    par(mgp=mgp, mar=mar)# , cex=cex)

    if (zlimGiven && is.character(zlim)) {
        if ("symmetric" == zlim) {
            zlim <- c(-1, 1) * max(abs(z), na.rm=TRUE)
        }
    }

    zlimHistogram <- zlimGiven && length(zlim) == 1 && zlim == "histogram"
    breaksGiven <- !missing(breaks)
    colormapGiven <- !missing(colormap)
    if (colormapGiven && missing(missingColor))
        missingColor <- colormap$missingColor
    zrange <- range(z, na.rm=TRUE)

    if (colormapGiven) {
        oceDebug(debug, "colormap provided\n", sep="")
        breaks <- colormap$breaks
        breaks2 <- breaks
        col <- colormap$col
        col2 <- col
    } else {
        ## Determine breaks unless zlim=="histogram".
        if (zlimHistogram) {
            if (missing(col))
                col <- oce.colorsPalette(200) # FIXME: how many colors to use?
        } else {
            if (!breaksGiven) {
                nbreaks <- 128                 # smooth image color scale
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
        ## If not z clipping, enlarge breaks/cols to avoid missing-color regions
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
        drawPalette(zlab=if (zlabPosition=="side") zlab else "", axisPalette=axisPalette, debug=debug-1, las=las.palette)
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
                        mai=mai.palette, las=las.palette,
                        cex=1,
                        debug=debug-1)
        } else {
            oceDebug(debug, "palette with zlim not \"histogram\"\n")
            drawPalette(zlim=zlim, zlab=if (zlabPosition=="side") zlab else "",
                        breaks=breaks, col=col,
                        labels=labels, at=at,
                        drawContours=drawContours,
                        drawTriangles=drawTriangles,
                        mai=mai.palette,
                        las=las.palette, 
                        axisPalette=axisPalette,
                        cex=cex,
                        debug=debug-1)
        }
    }

    ## xlim <- if (missing(xlim)) range(x, na.rm=TRUE) else xlim
    ## ylim <- if (missing(ylim)) range(y, na.rm=TRUE) else ylim
    ## oceDebug(debug, "zlimGiven: ", zlimGiven, "\n")
    ## zlim <- if (missing(zlim)) range(z, na.rm=TRUE) else zlim
    ## oceDebug(debug, "zlim=c(", paste(zlim, collapse=","), ")\n", sep="")

    ## trim image to limits, so endpoint colors will indicate outliers
    if (!zclip && !zlimHistogram) {
        oceDebug(debug, "using zlim[1:2]=c(", zlim[1], ",", zlim[2], ") for out-of-range values\n")
        z[z < zlim[1]] <- zlim[1]
        z[z > zlim[2]] <- zlim[2]
    }

    if (flipy) {
        ## nc <- ncol(z)
        ## z[, seq.int(nc, 1L)] <- z[, seq.int(1L, nc)]
        ylim <- rev(sort(ylim))
        if (ylimGiven)
           warning("The interaction of ylim and flipy changed on 2018 Mar 26.")
    }
    if (zclip && !zlimHistogram) {
        oceDebug(debug, "using missingColor for out-of-range values")
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
            mtext(ylab, side=2, line=par('mgp')[1], cex=cex.lab*par("cex"))
        } else {
            oceDebug(debug, "not doing filled contours [2]\n")
            if (zlimHistogram) {
                image(x=x, y=y, z=z, axes=FALSE, xlab="", ylab="", col=col2,
                      xlim=xlim, ylim=ylim, zlim=c(0, 1), asp=asp, add=add, useRaster=useRaster, ...)
            } else {
                ## issue 489: use breaks/col instead of breaks2/col2
                ##image(x=x, y=y, z=z, axes=FALSE, xlab="", ylab=ylab, breaks=breaks2, col=col2,
                image(x=x, y=y, z=z, axes=FALSE, xlab="", ylab="", breaks=breaks, col=col,
                  xlim=xlim, ylim=ylim, zlim=zlim, asp=asp, add=add, useRaster=useRaster, cex=cex, ...)
            }
        }
        if (axes) {
            box()
            oceDebug(debug,"about to call oce.axis.POSIXct() with par('mar')=c(", paste(par('mar'),collapse=","), ", mar=c(", paste(mar,collapse=","), ") and mgp=c(",paste(mgp,collapse=","),")\n")
            xat <- oce.axis.POSIXct(side=1, x=x, cex.axis=cex, cex.lab=cex, cex.main=cex,
                                    drawTimeRange=drawTimeRange,
                                    mar=mar, mgp=mgp, tformat=tformat, debug=debug-1)
            ##yat <- axis(2, cex.axis=cex.axis*par("cex"))
            oceDebug(debug, "doing y axis with cex.axis=", cex.axis, " then naming it with cex=", cex.lab*par('cex'), " (note par('cex')=", par('cex'), style="blue")
            ##yat <- axis(2, cex.axis=cex.axis)
            yat <- axis(2, cex.axis=cex.axis)
            mtext(side=2, ylab, line=mgp[1], cex=cex.lab*par("cex"))
        }
    } else {
        ## x is not a POSIXt
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
            mtext(xlab, side=1, cex=cex.lab*par("cex"), line=mgp[1])
            mtext(ylab, side=2, cex=cex.lab*par("cex"), line=mgp[1])
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
                  xlim=xlim, ylim=ylim, asp=asp, useRaster=useRaster, ...)
        }
        if (axes) {
            box()
            xat <- axis(1, cex.axis=cex)#, cex.axis=cex, cex.lab=cex)
            yat <- axis(2, cex.axis=cex)#, cex.axis=cex, cex.lab=cex)
        }
    }
    if (!is.null(missingColor)) {
        ## FIXME: the negation on is.na is confusing, but it comes from col and breaks together
        image(x, y, !is.na(z), col=c(missingColor, "transparent"), breaks=c(0, 1/2, 1), useRaster=useRaster, add=TRUE)
        if (axes)
            box()
    }
    if (is.na(main)) {
        mtext("", at=mean(range(x), na.rm=TRUE), side=3, line=1/8, cex=cex.main*par("cex"))
    } else if (!(is.character(main) && main == "")) {
        mtext(main, at=mean(range(x), na.rm=TRUE), side=3, line=1/8, cex=cex.main*par("cex"))
    }
    if (drawContours) {
        oceDebug(debug, "adding contours\n")
        contour(x=xorig, y=yorig, z=z, levels=breaks, drawlabels=FALSE, add=TRUE, col="black")
    }
    if (zlabPosition == "top")
        mtext(zlab, side=3, adj=1, line=1/8, cex=cex.main*par("cex"))
    par(cex=ocex)
    oceDebug(debug, "par('mai')=c(",
             paste(format(par('mai'), digits=2), collapse=","), "); par('mar')=c(",
             paste(format(par('mar'), digits=2), collapse=","), ")\n", sep='')
    oceDebug(debug, "} # imagep()\n", unindent=1, style="bold")
    invisible(list(xat=xat, yat=yat, decimate=decimate))
}                                      # imagep()
