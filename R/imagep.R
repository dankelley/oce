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

colorize <- function(z, breaks, colors=oceColorsJet)
{
    ## FIXME: colors could be e.g. "gmt_relief" etc
    ## FIXME: add pre-defined palettes for e.g. topography
    if (missing(z))
        stop("must supply z")
    if (is.character(colors)) {
        stop("'colors' may not be a character string in this early version of colorize()\n")
    } else {
        if (is.function(colors)) {
            if (missing(breaks)) { # Won't be doing it this way if e.g. colors="gmt"
                breaks <- pretty(z, n=10)
            }
            if (length(breaks) == 1)
                breaks <- pretty(z, n=breaks)
            col <- colors(length(breaks) - 1)
        } else {
            stop("'colors' must be a function in this early version of colorize()\n")
        }
    }
    list(zlim=range(z, na.rm=TRUE),
         breaks=breaks, col=col, zcol=col[findInterval(z, breaks)])
}

makePalette <- function(style=c("gmt_relief", "gmt_ocean", "oce_shelf"),
                        file, breaksPerLevel=20,
                        region=c("water", "land", "both"))
{
    readGMT <- function(file, text)
    {
        if (missing(file) && missing(text))
            stop("must give either 'file' or 'text'\n")
        if (missing(file)) {
            text <- strsplit(text, '\\n')[[1]]
        } else {
            text <- readLines(file)
        }
        text1 <- text[grep("^[ ]*[-0-9]", text)]
        d <- read.table(text=text1, col.names=c("l", "lr", "lg", "lb", "u", "ur", "ug", "ub"))
        if (length(grep("^[ ]*F", text))) {
            f <- as.numeric(strsplit(text[grep("^[ ]*F",text)], '\\t')[[1]][-1])
            f <- rgb(f[1]/255, f[2]/255, f[3]/255)
        } else {
            f <- "#FFFFFF"
        }
        if (length(grep("^[ ]*B", text))) {
            b <- as.numeric(strsplit(text[grep("^[ ]*B",text)], '\\t')[[1]][-1])
            b <- rgb(b[1]/255, b[2]/255, b[3]/255)
        } else {
            b <- "#000000"
        }
        if (length(grep("^[ ]*N", text))) {
            n <- as.numeric(strsplit(text[grep("^[ ]*N",text)], '\\t')[[1]][-1])
            n <- rgb(n[1]/255, n[2]/255, n[3]/255)
        } else {
            n <- "#FFFFFF"
        }
        list(l=d$l, lr=d$lr, lg=d$lg, lb=d$lb, u=d$u, ur=d$ur, ug=d$ug, ub=d$ub, f=f, b=b, n=n)
    }
    style <- match.arg(style)
    region <- match.arg(region)
    if (!missing(file)) {
        d <- readGMT(file)
    } else {
        if (style == "gmt_relief") {
            text <- "
#	$Id: GMT_relief.cpt,v 1.1 2001/09/23 23:11:20 pwessel Exp $
#
# Colortable for whole earth relief used in Wessel topomaps
# Designed by P. Wessel and F. Martinez, SOEST
# COLOR_MODEL = RGB
-8000	0	0	0	-7000	0	5	25
-7000	0	5	25	-6000	0	10	50
-6000	0	10	50	-5000	0	80	125
-5000	0	80	125	-4000	0	150	200
-4000	0	150	200	-3000	86	197	184
-3000	86	197	184	-2000	172	245	168
-2000	172	245	168	-1000	211	250	211
-1000	211	250	211	0	250	255	255
0	70	120	50	500	120	100	50
500	120	100	50	1000	146	126	60
1000	146	126	60	2000	198	178	80
2000	198	178	80	3000	250	230	100
3000	250	230	100	4000	250	234	126
4000	250	234	126	5000	252	238	152
5000	252	238	152	6000	252	243	177
6000	252	243	177	7000	253	249	216
7000	253	249	216	8000	255	255	255
F	255	255	255				
B	0	0	0
N	255	255	255"
        } else if (style == "gmt_ocean") {
            text <- "
#	$Id: GMT_ocean.cpt,v 1.1 2001/09/23 23:11:20 pwessel Exp $
#
# Colortable for oceanic areas as used in Wessel maps
# Designed by P. Wessel and F. Martinez, SOEST.
# COLOR_MODEL = RGB
-8000	0	0	0	-7000	0	5	25
-7000	0	5	25	-6000	0	10	50
-6000	0	10	50	-5000	0	80	125
-5000	0	80	125	-4000	0	150	200
-4000	0	150	200	-3000	86	197	184
-3000	86	197	184	-2000	172	245	168
-2000	172	245	168	-1000	211	250	211
-1000	211	250	211	0	250	255	255
F	255	255	255
B	0	0	0"
        } else if (style == "oce_shelf") {
            text <- "
-500	0	0	0	-200	0	10	55
-200	0	10	55	-175	0	40	80
-175	0	40	80	-150	0	80	125
-150	0	80	125	-125	0	115     162	
-125	0	150	200	-100	43	173     192	
-100	86	197	184	-75	129     221     176
-75	172     245     168	-50	191     247     189
-50	211     250     211	-25     220     250     240	
-25	220	250	240	0	250	255	255"
        }
        d <- readGMT(text=text)
    }
    nlevel <- length(d$l)
    breaks <- NULL
    col <- NULL
    for (l in 1:nlevel) {
        lowerColor <- rgb(d$lr[l]/255, d$lg[l]/255, d$lb[l]/255)
        upperColor <- rgb(d$ur[l]/255, d$ug[l]/255, d$ub[l]/255)
        breaks <- c(breaks, seq(d$l[l], d$u[l], length.out=1+breaksPerLevel))
        col <- c(col, colorRampPalette(c(lowerColor, upperColor))(1+breaksPerLevel))
    }
    if (region == "water") {
        wet <- breaks <= 0
        breaks <- breaks[wet]
        col <- col[wet]
    } else if (region == "land") {
        dry <- breaks >= 0
        breaks <- breaks[dry]
        col <- col[dry]
    }
    ## drop a colour for length match with breaks
    col <- col[-1]                     
    list(breaks=breaks, col=col, f=d$f, b=d$b, n=d$n)
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
    oceDebug(debug, "figureWidth = ", figureWidth, "(inches)\n")
    oceDebug(debug, "figureHeight = ", figureHeight, "(inches)\n")
    oceDebug(debug, "tickSpace = ", tickSpace, "(inches)\n")
    oceDebug(debug, "textSpace = ", textSpace, "(inches)\n")
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
                        breaks, col, mai, cex.axis=par("cex.axis"), pos=4,
                        labels=NULL, at=NULL,
                        levels, drawContours=FALSE,
                        fullpage=FALSE, drawTriangles=FALSE,
                        axisPalette, tformat,
                        debug=getOption("oceDebug"), ...)
{
    zlimGiven <- !missing(zlim)
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
        oceDebug(debug, "drawPalette() with no zlim argument: set space to right of a graph\n", sep="", unindent=1)
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
    if (zlimGiven) {
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
                    breaks <- seq(zlim[1], zlim[2], length.out=128) # smooth image colorscale
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
    if (zlimGiven) {
        if (fullpage)
            par(mai=pc$mai1f)
        else
            par(mai=pc$mai1)
        if (!breaksGiven) {
            palette <- seq(zlim[1], zlim[2], length.out=300)
            if (pos == 1 || pos == 3) {
                image(x=palette, y=1, z=matrix(palette, ncol=1), axes=FALSE, xlab="", ylab="",
                      col=col, zlim=zlim)
            } else if (pos == 2 || pos == 4) {
                FIN <- par('fin')
                PIN <- par('pin')
                MAI <- par('mai')

                oceDebug(debug, "mai[2] and mail[4] add to", MAI[2] + MAI[4], "fin[1]=", FIN[1], "so image will occupy ", FIN[1] - MAI[2] - MAI[4], "inches\n")
                oceDebug(debug, "mai[2] and mail[4] add to", MAI[2] + MAI[4], "pin[1]=", PIN[1], "so image will occupy ", FIN[1] - MAI[2] - MAI[4], "inches\n")

                #browser()
                image(x=1, y=palette, z=matrix(palette, nrow=1), axes=FALSE, xlab="", ylab="",
                      col=col, zlim=zlim)
            } else {
                stop("pos must be 1, 2, 3 or 4") # cannot be reached
            }
        } else {
            palette <- seq(zlim[1], zlim[2], length.out=300)
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
             paste(par('mai'), collapse=","), ")\n")
    oceDebug(debug, "} # drawPalette()\n", unindent=1)
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
                   axisPalette,
                   debug=getOption("oceDebug"),
                   ...)
{
    zlabPosition <- match.arg(zlabPosition)
    oceDebug(debug, "imagep(..., cex=", cex, ", flipy=", flipy, ",", 
             " xlab='", xlab, "'; ylab='", ylab, "'; zlab=\"", as.character(zlab), "\", ", 
             " zlabPosition=\"", zlabPosition, "\", ",
             " filledContour=", filledContour, ", ",
             " missingColor='", missingColor,
             ", ...) {\n", sep="", unindent=1)
    oceDebug(debug, "par(mar)=", paste(format(par('mar'), digits=3), collapse=" "), "\n")
    oceDebug(debug, "par(mai)=", paste(format(par('mai'), digits=3), collapse=" "), "\n")
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
    ocex <- par("cex")
    if (missing(mar))
        mar <- c(mgp[1]+if(nchar(xlab)>0) 1.5 else 1, mgp[1]+if(nchar(ylab)>0) 1.5 else 1, mgp[2]+1/2, 1/2)
    if (missing(mai.palette)) {
        ##mai.palette <- c(0, 1/8, 0, 3/8 + if (haveZlab && zlabPosition=="side") 1.5*par('cin')[2] else 0)
        mai.palette <- rep(0, 4)
        oceDebug(debug, "set mai.palette=", mai.palette, "\n")
    }

    par(mgp=mgp, mar=mar, cex=cex)

    zlimHistogram <- !missing(zlim) && zlim == "histogram"
    breaksGiven <- !missing(breaks)
    zrange <- range(z, na.rm=TRUE)

    ## Determine breaks unless zlim=="histogram".
    if (zlimHistogram) {
        if (missing(col))
            col <- oceColorsPalette(200) # FIXME: how many colours to use?
    } else {
        if (!breaksGiven) {
            oceDebug(debug, "breaks were not given\n")
            nbreaks <- 128                 # smooth image colorscale
            if (missing(zlim)) {
                if (missing(col)) {
                    breaks <- pretty(zrange, n=nbreaks)
                    if (breaks[1] < zrange[1])
                        breaks[1] <- zrange[1]
                    if (breaks[length(breaks)] > zrange[2])
                        breaks[length(breaks)] <- zrange[2]
                } else {
                    breaks <- seq(zrange[1], zrange[2], length.out=if(is.function(col))128 else 1+length(col))
                }
                breaksOrig <- breaks
            } else {
                ## zlim given, but breaks not given
                if (missing(col)) {
                    ##breaks <- c(zlim[1], pretty(zlim, n=nbreaks), zlim[2])
                    breaks <- pretty(zlim, n=nbreaks)
                    oceDebug(debug, "zlim given but not breaks or col; inferred breaks=", breaks, "\n")
                } else {
                    breaks <- seq(zlim[1], zlim[2],
                                  length.out=if(is.function(col))128 else 1+length(col))
                    oceDebug(debug, "zlim and col given but not breaks; inferred breaks=", breaks, "\n")
                }
                breaksOrig <- breaks
                oceDebug(debug, 'range(z):', zrange, '\n')
                oceDebug(debug, 'ORIG  range(breaks):', range(breaks), '\n')
                breaks[1] <- min(max(zlim[1], zrange[1]), breaks[1])
                breaks[length(breaks)] <- max(breaks[length(breaks)], min(zlim[2], zrange[2]))
                oceDebug(debug, 'later range(breaks):', range(breaks), '\n')
            }
        } else {
            oceDebug(debug, "breaks were given\n")
            breaksOrig <- breaks
            if (1 == length(breaks)) {
                breaks <- if (missing(zlim)) pretty(z, n=breaks) else pretty(zlim, n=breaks)
            }
        }
        if (missing(col))
            col <- oceColorsPalette(n=length(breaks)-1)
    }
    breaks2 <- if (missing(breaks)) NULL else breaks
    col2 <- if (missing(col)) NULL else col
    ## If not z clipping, enlarge breaks/cols to avoid missing-colour regions
    if (!zclip && !zlimHistogram) {
        db <- median(diff(breaks), na.rm=TRUE)
        breaks2 <- c(min(c(zrange[1], breaks, na.rm=TRUE))-db/100,
                         breaks,
                         max(c(zrange[2], breaks, na.rm=TRUE))+db/100)
        if (!is.function(col))
            col2 <- c(col[1], col, col[length(col)])
    }

    if (is.function(col)) {
        if (zlimHistogram)
            col <- col(n=200)          # FIXME: decide on length
        else
            col <- col(n=length(breaks)-1)
    }
    if (drawPalette == "space") {
        oceDebug(debug, "not drawing a palette, since drawPalette=\"space\"\n")
        drawPalette(zlab=if(zlabPosition=="side") zlab else "", axisPalette=axisPalette, debug=debug-1)
    } else if (drawPalette) {
        oceDebug(debug, "drawPalette=", drawPalette, "\n")
        oceDebug(debug, "drawing a palette\n")
        if(missing(zlim)) {
            ## use range of breaks preferably; otherwise use range z
            if (missing(breaks)) {
                zlim <- range(z, na.rm=TRUE)
            } else {
                zlim <- range(breaks, na.rm=TRUE)
            }
        }
        drawTriangles <- rep(drawTriangles, length.out=2)
        drawTriangles[1] <- drawTriangles[1] || any(z < zlim[1], na.rm=TRUE)
        drawTriangles[2] <- drawTriangles[2] || any(z > zlim[2], na.rm=TRUE)
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

    xlim <- if (missing(xlim)) range(x,na.rm=TRUE) else xlim
    ylim <- if (missing(ylim)) range(y,na.rm=TRUE) else ylim
    zlim <- if (missing(zlim)) range(z,na.rm=TRUE) else zlim

    ## trim image to limits, so endpoint colours will indicate outliers
    if (!zlimHistogram) {
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
        if (filledContour) {
            oceDebug(debug, "doing filled contours [1]\n")
            if (!is.double(z))
                storage.mode(z) <- "double"
            plot.new()
            plot.window(xlim=xlim, ylim=ylim, xaxs=xaxs, yaxs=yaxs, ...)
            ## Filled contours became official in version 2.15.0 of R.
            .filled.contour(as.double(xorig), as.double(yorig), z, as.double(breaks2), col=col2)
            mtext(ylab, side=2, line=par('mgp')[1])
        } else {
            oceDebug(debug, "not doing filled contours [2]\n")
            if (zlimHistogram) {
                image(x=x, y=y, z=z, axes=FALSE, xlab="", ylab=ylab, col=col2,
                      xlim=xlim, ylim=ylim, zlim=c(0,1), ...)
            } else {
                image(x=x, y=y, z=z, axes=FALSE, xlab="", ylab=ylab, breaks=breaks2, col=col2,
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
        oceDebug(debug, "the x axis does not represent time\n")
        if (filledContour) {
            oceDebug(debug, "doing filled contours [3]\n")
            storage.mode(z) <- "double"
            plot.new()
            plot.window(xlim=xlim, ylim=ylim, xaxs=xaxs, yaxs=yaxs, ...)
            ## Filled contours became official in version 2.15.0 of R.
            .filled.contour(as.double(xorig), as.double(yorig), z, as.double(breaks2), col=col2)
            mtext(xlab, side=1, line=mgp[1])
            mtext(ylab, side=2, line=mgp[1])
        } else {
            oceDebug(debug, "not doing filled contours\n")
            if (zlimHistogram) {
                if (is.function(col2))
                    col2 <- col2(200)
                breaks2 <- seq(0, 1, length.out=length(col2) + 1)
            }
            image(x=x, y=y, z=z, axes=FALSE, xlab=xlab, ylab=ylab, breaks=breaks2, col=col2,
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
    oceDebug(debug, "at end of imagep(), par('mai') yields c(", paste(par('mai'), collapse=","), ")\n")
    oceDebug(debug, "at end of imagep(), par('mar') yields c(", paste(par('mar'), collapse=","), ")\n")
    oceDebug(debug, "} # imagep()\n", unindent=1)
    invisible()
}
