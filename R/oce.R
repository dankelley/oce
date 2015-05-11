## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
useHeading <- function(b, g, add=0)
{
    if (!"heading" %in% names(b@data))
        stop("'from' does not have any heading data (in b@data$heading)")
    if (!"time" %in% names(b@data))
        stop("'b' does not have any time data (in b@data$time)")
    if (!"heading" %in% names(g@data))
        stop("'g' does not have any heading data (in g@data$heading)")
    if (!"time" %in% names(g@data))
        stop("'g' does not have any time data (in g@data$time)")
    res <- b
    t0 <- as.numeric(g@data$time[1])
    if (is.na(t0))
        stop("need first element of from@data$time to be non-NA")
    b.t <- as.numeric(b@data$time) - t0 # FIXME: what if heading in tsSlow?
    g.t <- as.numeric(g@data$time) - t0 # FIXME: what if heading in tsSlow?
    res@data$heading <- approx(x=g.t, y=g@data$heading, xout=b.t)$y + add
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}

window.oce <- function(x, start = NULL, end = NULL, frequency = NULL, deltat = NULL, extend = FALSE,
                       which=c("time","distance"), indexReturn=FALSE,
                       debug=getOption("oceDebug"), ...)
{
    oceDebug(debug, "window.oce(...,start=",
             paste(format(start),collapse=","), ",end=",
             paste(format(end),collapse=","),
             ",indexReturn=",indexReturn,",...) {\n", unindent=1)
    if (extend)
        stop("cannot handle extend=TRUE yet")
    if (!is.null(frequency))
        stop("cannot handle frequency yet")
    if (!is.null(deltat))
        stop("cannot handle deltat yet")
    if (is.null(start))
        stop("must provide start")
    if (is.null(end))
        stop("must provide end")
    oceDebug(debug, "class of (x) is: ", paste(class(x), collapse=","), "\n")
    res <- x
    which <- match.arg(which)
    nstart <- length(start)
    if (nstart != length(end))
        stop("lengths of 'start' and 'end' must match")
    if (which == "time") {
        oceDebug(debug, "windowing by time\n")
        if (!("time" %in% names(x@data))) {
            warning("oce object has no @data$time vector, so window is returning it unaltered")
            return(x)
        }
        if (is.character(start))
            start <- as.POSIXct(start, tz=getOption("oceTz"))
        if (is.character(end))
            end <- as.POSIXct(end, tz=getOption("oceTz"))
        oceDebug(debug, "tz of start:", attr(start, "tzone"), "\n")
        oceDebug(debug, "tz of end:", attr(end, "tzone"), "\n")
        oceDebug(debug, "tz of data$time:", attr(res@data$time, "tzone"), "\n")
        nstart <- length(start)
        ntime <- length(x@data$time)
        keep <- rep(FALSE, ntime)
        haveSlow <- "timeSlow" %in% names(x@data)
        keepSlow <- if (haveSlow) rep(FALSE, length(x@data$timeSlow)) else NULL
        for (w in 1:nstart) {
            keep <- keep | (start[w] <= res@data$time & res@data$time <= end[w])
            if (haveSlow)
                keepSlow <- keepSlow | (start[w] <= res@data$timeSlow & res@data$timeSlow <= end[w])
            oceDebug(debug, "data window (start=", format(start[w]), ", end=", format(end[w]), ") retains", sum(keep)/ntime*100, "percent\n")
        }
        if (indexReturn) {
            res <- list(index=keep, indexSlow=keepSlow)
            return(res)
        } else {
            for (name in names(res@data)) {
                if ("distance" == name)
                    next
                if (length(grep("^time", name)) || is.vector(res@data[[name]])) {
                    if (1 == length(agrep("Slow$", name))) {
                        oceDebug(debug, "windowing 'slow' variable data$", name, "\n", sep="")
                        res@data[[name]] <- x@data[[name]][keepSlow]
                    } else {
                        oceDebug(debug, "windowing data@", name, "\n", sep="")
                        res@data[[name]] <- x@data[[name]][keep]
                    }
                } else if (is.matrix(res@data[[name]])) {
                    oceDebug(debug, "windowing data@", name, ", which is a matrix\n", sep="")
                    res@data[[name]] <- x@data[[name]][keep,]
                } else if (is.array(res@data[[name]])) {
                    oceDebug(debug, "windowing data@", name, ", which is an array\n", sep="")
                    res@data[[name]] <- x@data[[name]][keep,,,drop=FALSE]
                }
            }
        }
    } else if (which == "distance") {
        if (!inherits(x, "adp")) {
            warning("window(..., which=\"distance\") only works for objects of class adp")
            return(x)
        }
        if (!("distance" %in% names(x@data))) {
            warning("oce object has no @data$s$distance vector, so window is returning it unaltered")
            return(x)
        }
        oceDebug(debug, "windowing an ADP object by distance\n")
        ## FIXME: make it work on sections, on CTD, etc.
        keep <- start <= res@data$distance & res@data$distance < end
        res@data$distance <- x@data$distanc[keep]
        for (name in names(res@data)) {
            if (is.array(res@data[[name]]) && 3 == length(dim(x@data[[name]]))){
                res@data[[name]] <- res@data[[name]][,keep,]
            }
        }
    } else {
        stop("unknown value of which \"", which, "\"") # cannot get here
    }
    if (inherits(x, "adp") || inherits(x, "adv")) {
        res@metadata$numberOfSamples <- dim(res@data$v)[1]
        res@metadata$numberOfCells <- dim(res@data$v)[2]
    }
    oceDebug(debug, "} # window.oce()\n", unindent=1)
    res
}

plotPolar <- function(r, theta, debug=getOption("oceDebug"), ...)
{

    oceDebug(debug, "plotPolar(...)\n", unindent=1)
    if (missing(r)) stop("must supply 'r'")
    if (missing(theta)) stop("must supply 'theta'")
    thetaRad <- theta * atan2(1, 1) / 45
    x <- r * cos(thetaRad)
    y <- r * sin(thetaRad)
    R <- 1.2 * max(r, na.rm=TRUE)
    Rpretty <- pretty(c(0, R))
    plot.new()
    plot.window(c(-R, R), c(-R, R), asp=1)
    points(x, y, ...)
    xa <- axis(1, pos=0)
    abline(v=0)
    th <- seq(0, 2 * atan2(1, 1) * 4, length.out=100)
    for (radius in xa[xa>0]) {
        lines(radius * cos(th), radius * sin(th))
    }
    abline(h=0)
    abline(v=0)
    oceDebug(debug, "} # plotPolar()\n", unindent=1)
}

oceApprox <- function(x, y, xout, method=c("rr", "unesco"))
{
    method <- match.arg(method)
    if (missing(x))
        stop("must supply x")
    if (missing(y))
        stop("must supply y")
    lx <- length(x)
    ly <- length(y)
    if (lx != ly)
        stop("length of x (", lx, ") and y (", ly, ") must agree")
    ok <- !is.na(x) & !is.na(y)
    x <- x[ok]
    y <- y[ok]
    o <- order(x)
    x <- x[o]
    y <- y[o]
    keep <- c(TRUE, 0 != diff(x))
    x <- x[keep]
    y <- y[keep]
    if (missing(xout))
        xout <- seq(min(x, na.rm=TRUE), max(x, na.rm=TRUE), length.out=lx)
    else
        if (any(is.na(xout)))
            stop("must not have any NA values in xout")
    .Call("oce_approx", x=x, y=y, xout=xout,
          method=pmatch(method, c("unesco", "rr")))
}
oce.approx <- oceApprox

plotSticks <- function(x, y, u, v, yscale=1, add=FALSE, length=1/20,
                       mgp=getOption("oceMgp"),
                       mar=c(mgp[1]+1,mgp[1]+1,1,1+par("cex")),
                       ...)
{
    pin <- par("pin")
    page.ratio <- pin[2]/pin[1]
    if (missing(x))
        stop("must supply x")
    nx <- length(x)
    if (missing(y))
        y <- rep(0, nx)
    if (length(y) < nx)
        y <- rep(y[1], nx)
    if (missing(u))
        stop("must supply u")
    if (missing(v))
        stop("must supply v")
    n <- length(x)
    if (length(y) != n)
        stop("lengths of x and y must match, but they are ", n, " and ", length(y))
    if (length(u) != n)
        stop("lengths of x and u must match, but they are ", n, " and ", length(u))
    if (length(v) != n)
        stop("lenghts of x and v must match, but they are ", n, " and ", length(v))
    par(mar=mar, mgp=mgp)
    if (!add)
        plot(range(x), range(y), type='n', ...)
    usr <- par("usr")
    yr.by.xr <- (usr[4] - usr[3]) / (usr[2] - usr[1])
    warn <- options("warn")$warn # FIXME: fails to quieten arrows()
    options(warn=0)
    ok <- !is.na(u) & !is.na(v) & (u^2+v^2) > 0
    arrows(as.numeric(x[ok]),
           y[ok],
           (as.numeric(x[ok]) + u[ok] / yscale / yr.by.xr * page.ratio),
           (y[ok] + v[ok] / yscale),
           length=length, ...)
    options(warn=warn)
}

#' Add a grid to an existing plot, with special abilities for those created by oce
#'
#' @details
#' For plots not created by oce functions, or for missing \code{xat} and \code{yat},
#' this is the same as a call to \code{\link{grid}} with missing \code{nx} and
#' \code{ny}. However, if \code{xat} is the return value from certain oce functions,
#' a more sophisticated grid is constructed. The problem with \code{\link{grid}} is
#' that it cannot handle axes with non-uniform grids, e.g. those with time axes
#' that span months of differing lengths.
#'
#' As of early February 2015, \code{oce.grid} handles \code{xat} produced as the
#' return value from the following functions: \code{\link{imagep}} and
#' \code{\link{oce.plot.ts}}, \code{\link{plot.adp}}, \code{\link{plot.echosounder}},
#' and \code{\link{plotTS}}.
#' It makes no sense to try to use \code{oce.grid} for multiplanel oce plots,
#' e.g. the default plot from \code{\link{plot.adp}}.
#'
#' @examples
#' library(oce)
#' i <- imagep(volcano)
#' oce.grid(i, lwd=2)
#' 
#' data(sealevel)
#' i <- oce.plot.ts(sealevel[["time"]], sealevel[["elevation"]])
#' oce.grid(i, col='red')
#' 
#' data(ctd)
#' i <- plotTS(ctd)
#' oce.grid(i, col='red')
#'
#' data(adp)
#' i <- plot(adp, which=1)
#' oce.grid(i, col='gray', lty=1)
#'
#' data(echosounder)
#' i <- plot(echosounder)
#' oce.grid(i, col='pink', lty=1)
#'
#' @param xat either a list of x values at which to draw the grid, or the return value from an oce plotting function
#' @param yat a list of y values at which to plot the grid (ignored if \code{gx} was a return value from an oce plotting function)
#' @param col colour of grid lines (see \code{\link{par}})
#' @param lty type for grid lines (see \code{\link{par}})
#' @param lwd width for grid lines (see \code{\link{par}})
#' @return nothing
oce.grid <- function(xat, yat, col="lightgray", lty="dotted", lwd=par("lwd")) 
{
    if (missing(xat) && missing(yat)) {
        grid(col=col, lty=lty, lwd=lwd)
    } else {
        if (is.list(xat)) {
            ## following over-rides the args
            yat <- xat$yat
            xat <- xat$xat
        }
        if (!missing(xat)) abline(v=xat, col=col, lty=lty, lwd=lwd)
        if (!missing(yat)) abline(h=yat, col=col, lty=lty, lwd=lwd)
    }
}

oce.plot.ts <- function(x, y, type="l", xlim, ylim, xlab, ylab,
                        drawTimeRange=getOption("oceDrawTimeRange"),
                        adorn=NULL, fill=FALSE,
                        xaxs=par("xaxs"), yaxs=par("yaxs"),
                        cex=par("cex"), cex.axis=par("cex.axis"), cex.main=par("cex.main"),
                        mgp=getOption("oceMgp"),
                        mar=c(mgp[1]+if(nchar(xlab)>0) 1.5 else 1, mgp[1]+1.5, mgp[2]+1, mgp[2]+3/4),
                        main="",
                        despike=FALSE,
                        axes=TRUE, tformat,
                        marginsAsImage=FALSE,
                        grid=FALSE, grid.col="darkgray", grid.lty="dotted", grid.lwd=1,
                        debug=getOption("oceDebug"),
                        ...)
{
    if (is.function(x))
        stop("x cannot be a function")
    if (missing(xlab))
        xlab <- ""
    if (missing(ylab))
        ylab  <- deparse(substitute(y))
    ocex <- par("cex")
    #par(cex=cex)
    debug <- min(debug, 4)
    oceDebug(debug, "oce.plot.ts(..., debug=", debug, ", type=\"", type, "\", \n", sep="", unindent=1)
    oceDebug(debug, "  mar=c(", paste(mar, collapse=", "), "),\n", sep="")
    oceDebug(debug, "  mgp=c(",paste(mgp, collapse=", "),"),\n", sep="")
    oceDebug(debug, "  ...) {\n", sep="")
    oceDebug(debug, "length(x)", length(x), "; length(y)", length(y), "\n")
    oceDebug(debug, "cex=",cex," cex.axis=", cex.axis, " cex.main=", cex.main, "\n")
    oceDebug(debug, "mar=c(",paste(mar, collapse=","), ")\n")
    oceDebug(debug, "marginsAsImage=",marginsAsImage, ")\n")
    oceDebug(debug, "x has timezone", attr(x[1], "tzone"), "\n")
    pc <- paletteCalculations(maidiff=rep(0, 4))
    par(mgp=mgp, mar=mar)
    args <- list(...)
    xlimGiven <- !missing(xlim)
    if (xlimGiven) {
        if (2 != length(xlim))
            stop("'xlim' must be of length 2")
        if (xlim[2] <= xlim[1])
            stop("the elements of xlim must be in order")
        ends <- .Call("trim_ts", as.numeric(x), as.numeric(xlim), as.numeric(0.04))
        x <- x[seq.int(ends$from, ends$to)]
        y <- y[seq.int(ends$from, ends$to)]
    }
    if (length(y) == 1)
        y <- rep(y, length(x))
    if (despike)
        y <- despike(y)
    if (marginsAsImage) {
        ## FIXME: obey their mar?
        the.mai <- pc$mai0
        the.mai <- clipmin(the.mai, 0)         # just in case
        oceDebug(debug, "the.mai=", the.mai, "\n")

        par(mai=the.mai, cex=cex)
        drawPalette(mai=rep(0, 4))
    }
    if (fill) {
        xx <- c(x[1], x, x[length(x)])
        yy <- c(0, y, 0)
        plot(x, y, axes=FALSE, xaxs=xaxs, yaxs=yaxs,
             xlim=if (xlimGiven) xlim else range(x, na.rm=TRUE),
             xlab=xlab, ylab=ylab,
             type=type, cex=cex, ...)
        fillcol <- if ("col" %in% names(args)) args$col else "lightgray" # FIXME: should be a formal argument
        do.call(polygon, list(x=xx, y=yy, col=fillcol))
    } else {
        plot(x, y, axes=FALSE, xaxs=xaxs, yaxs=yaxs,
             xlim=if (missing(xlim)) NULL else xlim,
             ylim=if (missing(ylim)) NULL else ylim,
             xlab=xlab, ylab=ylab,
             type=type, cex=cex, ...)
    }
    xat <- NULL
    yat <- NULL
    if (axes) {
        xaxt <- list(...)["xaxt"]
        drawxaxis <- !is.null(xaxt) && xaxt != 'n'
        yaxt <- list(...)["yaxt"]
        drawyaxis <- !is.null(yaxt) && yaxt != 'n'
        if (drawxaxis) {
            xlabs <- oce.axis.POSIXct(1, x=x, drawTimeRange=drawTimeRange, main=main,
                                      mgp=mgp,
                                      xlim=if(missing(xlim)) range(x) else xlim,
                                      cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                      tformat=tformat,
                                      debug=debug-1)
            xat <- xlabs
            oceDebug(debug, "drawing x axis; set xat=c(", paste(xat, collapse=","),")", "\n", sep="")
        }
        if (grid) {
            lwd <- par("lwd")
            if (drawxaxis)
                abline(v=xlabs, col="lightgray", lty="dotted", lwd=lwd)
            yaxp <- par("yaxp")
            abline(h=seq(yaxp[1], yaxp[2], length.out=1+yaxp[3]),
                   col="lightgray", lty="dotted", lwd=lwd)
        }
        box()
        ##cat("cex.axis=",cex.axis,"; par('cex.axis') is", par('cex.axis'), "; par('cex') is", par('cex'), "\n")
        if (drawyaxis)
            axis(2, cex.axis=cex.axis, cex=cex.axis)
        yat <- axis(4, labels=FALSE)
    }
    if (grid)
        grid(col=grid.col, lty=grid.lty, lwd=grid.lwd)
    if (!is.null(adorn)) {
        t <- try(eval(adorn, enclos=parent.frame()), silent=TRUE)
        if (class(t) == "try-error")
            warning("cannot evaluate adorn {", format(adorn), "}\n")
    }
    ##par(cex=ocex)
    oceDebug(debug, "} # oce.plot.ts()\n", unindent=1)
    invisible(list(xat=xat, yat=yat))
}

oce.as.POSIXlt <- function (x, tz = "")
{
    fromchar <- function(x)
    {
        xx <- x[1]
        if (is.na(xx)) {
            j <- 1
            while (is.na(xx) && (j <- j + 1) <= length(x)) xx <- x[j]
            if (is.na(xx))
                f <- "%Y-%m-%d"
        }
        ## year day hhmm
        tokens <- strsplit(xx, " +")[[1]]
        if (length(tokens) == 3 && nchar(tokens[3]) == 4) { # the nchar check skips [year month day]
            return(strptime(x, format="%Y %j %H%M"))
        }
        if (is.na(xx) ||
                                        # additions ...
            ((nchar(xx) == 8) && !is.na(strptime(xx, f <- "%Y%m%d"))) || # 20020823
            !is.na(strptime(xx, f <- "%B %d %Y %H:%M:%OS")) || # Aug 23 2002 or August 23 2002
            !is.na(strptime(xx, f <- "%Y %B %d %H:%M:%OS")) || # 2002 Aug 23
            !is.na(strptime(xx, f <- "%d %B %Y %H:%M:%OS")) || # 23 Aug 2002
                                        # ... and now back to the standard
            !is.na(strptime(xx, f <- "%Y-%m-%d %H:%M:%OS")) ||
            !is.na(strptime(xx, f <- "%Y/%m/%d %H:%M:%OS")) ||
            !is.na(strptime(xx, f <- "%Y-%m-%d %H:%M")) ||
            !is.na(strptime(xx, f <- "%Y/%m/%d %H:%M")) ||
            !is.na(strptime(xx, f <- "%Y-%m-%d")) ||
            !is.na(strptime(xx, f <- "%Y/%m/%d"))) {
            res <- strptime(x, f)
            if (nchar(tz))
                attr(res, "tzone") <- tz
            return(res)
        }
        warning("The string \"", x, "\" is not in a known date format")
        return(NA)
    }
    if (inherits(x, "POSIXlt"))
        return(x)
    if (inherits(x, "Date"))
        return(as.POSIXlt(x))
    tzone <- attr(x, "tzone")
    if (inherits(x, "date") || inherits(x, "dates"))
        x <- as.POSIXct(x)
    if (is.character(x))
        return(fromchar(unclass(x)))
    if (is.factor(x))
        return(fromchar(as.character(x)))
    if (is.logical(x) && all(is.na(x)))
        x <- as.POSIXct.default(x)
    if (!inherits(x, "POSIXct"))
        stop(gettextf("do not know how to convert '%s' to class \"POSIXlt\"", deparse(substitute(x))))
    if (missing(tz) && !is.null(tzone))
        tz <- tzone[1]
    as.POSIXlt(x, tz)
}

oce.edit <- function(x, item, value, action, reason="", person="",
                     debug=getOption("oceDebug"))
{
    oceDebug(debug, "oce.edit() {\n", unindent=1)
    if (!inherits(x, "oce"))
        stop("method is only for oce objects")
    if (!missing(item)) {
        if (missing(value))
            stop("must supply a 'value' for this 'item'")
        ##if (!(item %in% names(x@metadata)))
        ## stop("no item named '", item, "' in object's  metadata")
        if (inherits(x, "adv")) {
            oceDebug(debug, "object is an ADV\n")
            hpr <- 0 < length(grep("heading|pitch|roll", item))
            if (hpr) {
                x@data[[item]] <- value
            } else {
                if (item %in% names(x@metadata)) {
                    oceDebug(debug, "changing metadata[[", item, "]]\n")
                    x@metadata[[item]] <- value
                } else
                    stop("do not know how to handle this item")
            }
        } else if (inherits(x, "adp")) {
            oceDebug(debug, "object is an ADP\n")
            hpr <- 0 < length(grep("heading|pitch|roll", item))
            if (hpr) {
                oceDebug(debug, "changing data$ts[[", item, "]] of a non-nortek\n")
                x@data[[item]] <- value
            } else {
                if (item %in% names(x@metadata)) {
                    oceDebug(debug, "changing metadata[[", item, "]]\n")
                    x@metadata[[item]] <- value
                } else
                    stop("do not know how to handle this item")
            }
        } else if (inherits(x, "ctd")) {
            if (item %in% names(x@metadata)) {
                x@metadata[[item]] <- value
            } else if (item %in% names(x@data)) {
                x@data[[item]] <- value
            } else {
                stop("cannot find that item")
            }
        } else if (inherits(x, "section")) {
             if (item %in% names(x@metadata)) {
                x@metadata[[item]] <- value
            } else if (item %in% names(x@data)) {
                x@data[[item]] <- value
            } else {
                stop("cannot find that item")
            }
        } else if ("instrumentType" %in% names(x@metadata) && x@metadata$instrumentType == "aquadopp-hr") { ## FIXME: what if S4?
            oceDebug(debug, "About to try editing AQUADOPP ...\n")
            hpr <- 0 < length(grep("heading|pitch|roll", item)) # FIXME: possibly aquadopp should have tsSlow
            x@data[[item]] <- value
            if (hpr) {
                x@data[[item]] <- value
                oceDebug(debug, " edited x$ts[", item, "]\n", sep="")
            } else {
                if (item %in% names(x@metadata)) {
                    oceDebug(debug, " edited x@metadata[", item, "]\n", sep="")
                    x@metadata[item] <- value
                } else {
                    stop("do not know how to handle this item, named \"", item, "\"\n", sep="")
                }
            }
            oceDebug(debug, "...AQUADOPP edited\n")
        } else {
            if (item %in% names(x@metadata))
                x@metadata[item] <- value
            else
                stop("do not know how to handle this item")
        } 
    } else if (!missing(action)) {
        warning("the 'action' method may not work -- this needs testing!")
        eval(parse(text=action))        # FIXME: should check if it worked
    } else {
        stop("must supply either an 'item' plus a 'value', or an 'action'")
    }
    x@processingLog <- processingLogAppend(x@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # oce.edit()\n", unindent=1)
    x
}
oceEdit <- oce.edit

oce.write.table <- function (x, file="", ...)
{
    if (!inherits(x, "oce"))
        stop("method is only for oce objects")
    if (!("row.names" %in% names(list(...))))
        write.table(x@data, file, row.names=FALSE, ...)
    else
        write.table(x@data, file, ...)
}

standardDepths <- function()
{
    c(0,   10,   20,   30,   50,   75,  100,  125,  150,  200,
      250,  300,  400,  500,  600,  700,  800,  900, 1000, 1100,
      1200, 1300, 1400, 1500, 1750, 2000, 2500, 3000, 3500, 4000,
      4500, 5000, 5500)
}

summary.oce <- function(object, ...)
{
    if (!inherits(object, "oce"))
        stop("method is only for oce objects")
    cat("Data summary:\n")
    print(summary(object@data))
    cat("\nMetadata:\n")
    print(object@metadata)
    print(summary(object))
    return(invisible(object))
}

oceMagic <- function(file, debug=getOption("oceDebug"))
{
    filename <- file
    oceDebug(debug, paste("oceMagic(file=\"", filename, "\") {\n", sep=""), unindent=1)
    isdir<- file.info(file)$isdir
    if (is.finite(isdir) && isdir) {
        tst <- file.info(paste(file, "/", file, "_MTL.txt", sep=""))$isdir
        if (!is.na(tst) && !tst)
            return("landsat")
        stop("please supply a file name, not a directory name")
    }
    if (is.character(file)) {
        oceDebug(debug, "checking filename to see if it matches known patterns\n")
        if (length(grep(".asc$", filename))) {
            someLines <- readLines(file, encoding="UTF-8", n=1)
            if (42 == length(strsplit(someLines[1], ' ')[[1]]))
                return("lisst")
        } else if (length(grep(".adr$", filename))) {
            oceDebug(debug, "file names ends in .adr, so this is an adv/sontek/adr file.\n")
            return("adv/sontek/adr")
        } else if (length(grep(".rsk$", filename))) {
            oceDebug(debug, "file names ends with \".rsk\", so this is an RBR/rsk file.\n")
            return("RBR/rsk")
        } else if (length(grep(".s4a.", filename))) {
            oceDebug(debug, "file names contains \".s4a.\", so this is an interocean S4 file.\n")
            return("interocean/s4")
        } else if (length(grep(".ODF$", filename, ignore.case=TRUE))) {
            ## in BIO files, the data type seems to be on line 14.  Read more, for safety.
            someLines <- readLines(file, n=100, encoding="UTF-8")
            dt <- grep("DATA_TYPE=", someLines)
            if (length(dt) < 1)
                stop("cannot infer type of ODF file")
            subtype <- gsub("[',]", "", tolower(strsplit(someLines[dt[1]], "=")[[1]][2]))
            subtype <- gsub("^\\s*", "", subtype)
            subtype <- gsub("\\s*$", "", subtype)
            rval <- paste(subtype, "odf", sep="/")
            oceDebug(debug, "file type:", rval, "\n")
            return(rval)
        } else if (length(grep(".WCT$", filename, ignore.case=TRUE))) { # old-style WOCE
            return("ctd/woce/other") # e.g. http://cchdo.ucsd.edu/data/onetime/atlantic/a01/a01e/a01ect.zip
        } else if (length(grep(".nc$", filename, ignore.case=TRUE))) { # argo drifter?
            if (requireNamespace("ncdf4", quietly=TRUE)) {
                if (substr(filename, 1, 5) == "http:") {
                    stop("cannot open netcdf files over the web; try doing as follows\n    download.file(\"",
                         filename, "\", \"", gsub(".*/", "", filename), "\")")
                }
                ## NOTE: need to name ncdf4 package because otherwise R checks give warnings.
                f <- ncdf4::nc_open(filename)
                if ("DATA_TYPE" %in% names(f$var) && grep("argo", ncdf4::ncvar_get(f, "DATA_TYPE"), ignore.case=TRUE))
                    return("drifter/argo")
            } else {
                stop('must install.packages("ncdf4") to read a netCDF file')
            }
        } else if (length(grep(".osm.xml$", filename, ignore.case=TRUE))) { # openstreetmap
            return("openstreetmap")
        } else if (length(grep(".osm$", filename, ignore.case=TRUE))) { # openstreetmap
            return("openstreetmap")
        } else if (length(grep(".gpx$", filename, ignore.case=TRUE))) { # gpx (e.g. Garmin GPS)
            return("gpx")
        }
        file <- file(file, "r")
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file))
        open(file, "r")
    ## Grab text at start of file.
    lines <- readLines(file, n=2, skipNul=TRUE)
    line <- lines[1]
    line2 <- lines[2]
    oceDebug(debug, "first line of file: ", line, "\n", sep="")
    oceDebug(debug, "second line of file: ", line2, "\n", sep="")
    close(file)
    file <- file(filename, "rb")
    bytes <- readBin(file, what="raw", n=4)
    oceDebug(debug, paste("first two bytes in file: 0x", bytes[1], " and 0x", bytes[2], "\n", sep=""))
    on.exit(close(file))
    ##read.index()  ## check for an ocean index file e.g.
    ##read.index()  # http://www.esrl.noaa.gov/psd/data/correlation/ao.data
    ##read.index()  tokens <- scan(text=line, what='integer', n=2, quiet=TRUE)
    ##read.index()  if (2 == length(tokens)) {
    ##read.index()      tokens2 <- scan(text=line2, what='integer', quiet=TRUE)
    ##read.index()      if (tokens[1] == tokens2[1])
    ##read.index()          return("index")
    ##read.index()  }
    if (bytes[1] == 0x00 && bytes[2] == 0x00 && bytes[3] == 0x27 && bytes[4] == 0x0a) {
        oceDebug(debug, "this is a shapefile; see e.g. http://en.wikipedia.org/wiki/Shapefile\n  }\n")
        return("shapefile")
    }
    if (bytes[3] == 0xff && bytes[4] == 0xff) {
        oceDebug(debug, "this is a biosonics echosounder file")
        return("echosounder")
    }
    if (bytes[1] == 0x10 && bytes[2] == 0x02) {
        ## 'ADPManual v710.pdf' p83
        if (96 == readBin(bytes[3:4], "integer", n=1, size=2,endian="little"))
            oceDebug(debug, "this is adp/sontek (4 byte match)\n  }\n")
        else
            oceDebug(debug, "this is adp/sontek (2 byte match, but bytes 3 and 4 should become integer 96)\n  }\n")
        return("adp/sontek")
    }
    if (bytes[1] == 0x7f && bytes[2] == 0x7f) {
        oceDebug(debug, "this is adp/rdi\n  }\n")
        return("adp/rdi")
    }
    if (bytes[1] == 0xa5 && bytes[2] == 0x05) {
        ## NorTek files require deeper inspection.  Here, SIG stands for "System Integrator Guide",
        ## Dated Jue 2008 (Nortek Doc No PS100-0101-0608)
        seek(file, 0)
        oceDebug(debug, "This is probably a nortek file of some sort.  Reading further to see for sure ...\n")
        hardware.configuration <- readBin(file, what="raw", n=48) # FIXME: this hard-wiring is repeated elsewhere
        if (hardware.configuration[1] != 0xa5 || hardware.configuration[2] != 0x05) return("unknown")
        oceDebug(debug, "hardware.configuration[1:2]", hardware.configuration[1:2], "(expect 0xa5 0x05)\n")
        head.configuration <- readBin(file, what="raw", n=224)
        oceDebug(debug, "head.configuration[1:2]", head.configuration[1:2], "(expect 0xa5 0x04)\n")
        if (head.configuration[1] != 0xa5 || head.configuration[2] != 0x04) return("unknown")
        user.configuration <- readBin(file, what="raw", n=512)
        oceDebug(debug, "user.configuration[1:2]", user.configuration[1:2], "(expect 0xa5 0x00)\n")
        if (user.configuration[1] != 0xa5 || user.configuration[2] != 0x00) return("unknown")
        next.two.bytes <- readBin(file, what="raw", n=2)
        oceDebug(debug, "next.two.bytes:", paste("0x", next.two.bytes[1], sep=''),
                 paste("0x", next.two.bytes[2], sep=''),
                 "(e.g. 0x5 0x12 is adv/nortek/vector)\n")
        if (next.two.bytes[1] == 0xa5 && next.two.bytes[2] == 0x12) {
            oceDebug(debug, "these two bytes imply this is adv/nortek/vector\n")
            return("adv/nortek/vector")
        }
        if (next.two.bytes[1] == 0xa5 && next.two.bytes[2] == 0x01) {
            oceDebug(debug, "these two bytes imply this is adp/nortek/aqudopp (see system-integrator-manual_jan2011.pdf Table 5.2)\n")
            return("adp/nortek/aquadopp")
        }
        if (next.two.bytes[1] == 0xa5 && next.two.bytes[2] == 0x21)  {
            oceDebug(debug, "these two bytes imply this is adp/nortek/aqudoppProfiler\n")
            return("adp/nortek/aquadoppProfiler") # p37 SIG
        }
        if (next.two.bytes[1] == 0xa5 && next.two.bytes[2] == 0x2a)  {
            oceDebug(debug, "these two bytes imply this is adp/nortek/aqudoppHR\n")
            return("adp/nortek/aquadoppHR") # p38 SIG
        }
        stop("some sort of nortek ... two bytes are 0x", next.two.bytes[1], " and 0x", next.two.bytes[2], " but cannot figure out what the type is")
        ##} else if (as.integer(bytes[1]) == 81) {
        ##    warning("possibly this file is a sontek ADV (first byte is 81)")
        ##} else if (as.integer(bytes[1]) == 83) {
        ##    warning("possibly this file is a sontek ADV (first byte is 83)")
        ##} else if (as.integer(bytes[1]) == 87) {
        ##    warning("possibly this file is a sontek ADV (first byte is 87)")
    }

    ##if (substr(line, 1, 2) == "\177\177")            return("adp")
    if (substr(line, 1, 3) == "CTD") {
        oceDebug(debug, "this is ctd/woce/exchange\n")
        return("ctd/woce/exchange")
    }
    if ("* Sea-Bird" == substr(line, 1, 10))  {
        oceDebug(debug, "this is ctd/sbe/19\n")
        return("ctd/sbe/19")
    }
    if ("%ITP" == substr(line, 1, 4)) {
        oceDebug(debug, "this is ice-tethered profile\n")
        return("ctd/itp")
    }
    if ("# -b" == substr(line, 1, 4)) {
        oceDebug(debug, "this is coastline\n")
        return("coastline")
    }
    if ("# Station_Name," == substr(line, 1, 15)) {
        oceDebug(debug, "this is sealevel\n")
        return("sealevel")
    }
    if ("Station_Name," == substr(line, 1, 13)) {
        oceDebug(debug, "this is sealevel\n")
        return("sealevel")
    }
    if ("%" == line && length(line2) && regexpr("^Observatory", line2)) {
        oceDebug(debug, "this is observatory\n")
        return("observatory")
    }
    if (0 < regexpr("^[0-9][0-9][0-9][A-Z] ", line)) {
        oceDebug(debug, "this is sealevel\n")
        return("sealevel")
    }
    if (0 < regexpr("^NCOLS[ ]*[0-9]*[ ]*$", line, ignore.case=TRUE)) {
        oceDebug(debug, "this is topo\n")
        return("topo")
    }
    if ("RBR TDR" == substr(line, 1, 7))  {
        oceDebug(debug, "this is logger\n")
        return("logger")
    }
    if ("BOTTLE"  == substr(line, 1, 6))  {
        oceDebug(debug, "this is section\n")
        return("section")
    }
    if (length(grep("^//SDN_parameter_mapping", line)) ||
        length(grep("^//SDN_parameter_mapping", line2))) {
        oceDebug(debug, "this is ODV\n")
        return("ctd/odv")
    }
    oceDebug(debug, "this is unknown\n")
    return("unknown")
}
oce.magic <- oceMagic

read.oce <- function(file, ...)
{
    type <- oceMagic(file)
    debug <- if ("debug" %in% names(list(...))) list(...)$debug else 0
    oceDebug(debug,
             "read.oce(\"", as.character(file), "\", ...) inferred type=\"", type, "\"\n",
             sep="", unindent=1)
    processingLog <- paste(deparse(match.call()), sep="", collapse="")
    ## read.index if (type == "index")
    ## read.index     return(read.index(file))
    if (type == "shapefile")
        return(read.coastline.shapefile(file, processingLog=processingLog, ...))
    if (type == "openstreetmap")
        return(read.coastline.openstreetmap(file, processingLog=processingLog, ...))
    if (type == "echosounder")
        return(read.echosounder(file, processingLog=processingLog, ...))
    if (type == "adp/rdi")
        return(read.adp.rdi(file, processingLog=processingLog, ...))
    if (type == "adp/sontek")
        return(read.adp.sontek(file, processingLog=processingLog, ...)) # FIXME is pcadcp different?
    if (type == "adp/nortek/aquadopp")
        return(read.aquadopp(file, processingLog=processingLog, ...))
    if (type == "adp/nortek/aquadoppProfiler")
        return(read.aquadoppProfiler(file, processingLog=processingLog, ...))
    if (type == "adp/nortek/aquadoppHR")
        return(read.aquadoppHR(file, processingLog=processingLog, ...))
    if (type == "adv/nortek/vector")
        return(read.adv.nortek(file, processingLog=processingLog, ...))
    if (type == "adv/sontek/adr")
        return(read.adv.sontek.adr(file, processingLog=processingLog, ...))
    ## FIXME need adv/sontek (non adr)
    if (type == "interocean/s4")
        return(read.cm.s4(file, processingLog=processingLog, ...))
    if (type == "ctd/sbe/19")
        return(read.ctd.sbe(file, processingLog=processingLog, ...))
    if (type == "ctd/woce/exchange")
        return(read.ctd.woce(file, processingLog=processingLog, ...))
    if (type == "ctd/odf" || type == "mctd/odf")
        return(read.ctd.odf(file, processingLog=processingLog, ...))
    if (type == "mtg/odf") {
        ## FIXME: document this data type
        ## Moored tide gauge: returns a data frame.
        fromHeader <- function(key)
        {
            i <- grep(key, lines)
            if (length(i) < 1)
                ""
            else
                gsub("\\s*$", "", gsub("^\\s*", "", gsub("'","", gsub(",","",strsplit(lines[i[1]], "=")[[1]][2]))))
        }
        lines <- readLines(file, encoding="UTF-8")
        nlines <- length(lines)
        headerEnd <- grep("-- DATA --", lines)
        if (1 != length(headerEnd))
            stop("found zero or multiple '-- DATA --' (end of header) lines in a mtg/odf file")
        header <- lines[1:headerEnd]
        data <- lines[seq.int(headerEnd+1, nlines)]
        d <- read.table(text=data, header=FALSE, col.names=c("time","temperature","ptotal","psea","depth"))
        d$time <- strptime(d$time,"%d-%B-%Y %H:%M:%S", tz="UTC") # guess on timezone
        missing_value <- -99.0 # FIXME: it's different for each column
        d[d==missing_value] <- NA
        attr(d, "scientist") <- fromHeader("CHIEF_SCIENTIST")
        attr(d, "latitude") <- fromHeader("INITIAL_LATITUDE")
        attr(d, "longitude") <- fromHeader("INITIAL_LONGITUDE")
        attr(d, "cruise_name") <- fromHeader("CRUISE_NAME")
        attr(d, "cruise_description") <- fromHeader("CRUISE_DESCRIPTION")
        attr(d, "inst_type") <- fromHeader("INST_TYPE")
        attr(d, "model") <- fromHeader("MODEL")
        attr(d, "serial_number") <- fromHeader("SERIAL_NUMBER")
        attr(d, "missing_value") <- missing_value
        warning("The format of mtg/odf objects is likely to change throughout April, 2015")
        return(d)
    }
    if (type == "ctd/odv")
        return(read.ctd.odv(file, processingLog=processingLog, ...))
    if (type == "ctd/itp")
        return(read.ctd.itp(file, processingLog=processingLog, ...))
    if (type == "gpx")
        return(read.gps(file, type="gpx", processingLog=processingLog, ...))
    if (type == "mvctd/odf")
        return(read.ctd.odf(file, processingLog=processingLog, ...))
    if (type == "coastline")
        return(read.coastline(file, type="mapgen", processingLog=processingLog, ...))
    if (type == "drifter/argo")
        return(read.drifter(file))
    if (type == "lisst")
        return(read.lisst(file))
    if (type == "sealevel")
        return(read.sealevel(file, processingLog=processingLog, ...))
    if (type == "topo")
        return(read.topo(file, processingLog=processingLog, ...))
    if (type == "logger")
        return(read.logger(file, processingLog=processingLog, ...))
    if (type == "RBR/rsk")
        return(read.logger(file, processingLog=processingLog, ...))
        ##return(read.logger(file, processingLog=processingLog, type='rsk'))
    if (type == "section")
        return(read.section(file, processingLog=processingLog, ...))
    if (type == "ctd/woce/other")
        return(read.ctd.woce.other(file, processingLog=processingLog, ...))
    if (type == "observatory")
        return(read.observatory(file, processingLog=processingLog, ...))
    if (type == "landsat") {
        return(read.landsat(file, ...))
    }
    stop("unknown file type \"", type, "\"")
}


oce.colorsGebco <- function(n=9, region=c("water", "land", "both"), type=c("fill","line"))
{
    region <- match.arg(region)
    type <- match.arg(type)
    if (type == "fill") {
        ## generate land colors by e.g. rgb(t(col2rgb(land[5])-1*c(10,4,10))/255)
        land <- c("#FBC784","#F1C37A","#E6B670","#DCA865","#D19A5C",
                  "#C79652","#BD9248","#B38E3E","#A98A34")
        water <- rev(c("#E1FCF7","#BFF2EC","#A0E8E4","#83DEDE","#68CDD4",
                       "#4FBBC9","#38A7BF","#2292B5","#0F7CAB"))
    } else {
        land <- c("#FBC784","#F1C37A","#E6B670","#DCA865","#D19A5C",
                  "#C79652","#BD9248","#B38E3E","#A98A34")
        water <- rev(c("#A4FCE3","#72EFE9","#4FE3ED","#47DCF2","#46D7F6",
                       "#3FC0DF","#3FC0DF","#3BB7D3","#36A5C3"))#,"#3194B4",
                       #"#2A7CA4","#205081","#16255E","#100C2F"))
    }
    if (region == "water") {
        rgb.list <- col2rgb(water) / 255
        l <- length(water)
        r <- approx(1:l, rgb.list[1,1:l], xout=seq(1, l, length.out=n))$y
        g <- approx(1:l, rgb.list[2,1:l], xout=seq(1, l, length.out=n))$y
        b <- approx(1:l, rgb.list[3,1:l], xout=seq(1, l, length.out=n))$y
    } else if (region == "land") {
        rgb.list <- col2rgb(land) / 255
        l <- length(land)
        r <- approx(1:l, rgb.list[1,1:l], xout=seq(1, l, length.out=n))$y
        g <- approx(1:l, rgb.list[2,1:l], xout=seq(1, l, length.out=n))$y
        b <- approx(1:l, rgb.list[3,1:l], xout=seq(1, l, length.out=n))$y
    } else {                            # both
        rgb.list <- col2rgb(c(water, land)) / 255
        l <- length(land) + length(water)
        r <- approx(1:l, rgb.list[1,1:l], xout=seq(1, l, length.out=n))$y
        g <- approx(1:l, rgb.list[2,1:l], xout=seq(1, l, length.out=n))$y
        b <- approx(1:l, rgb.list[3,1:l], xout=seq(1, l, length.out=n))$y
    }
    rgb(r, g, b)
}
oceColorsGebco <- oce.colorsGebco


oce.colorsTwo <- function (n, low=2/3, high=0, smax=1, alpha = 1)
{
    ## code borrows heavily from cm.color()
    if ((n <- as.integer(n[1])) > 0) {
        even.n <- n%%2 == 0
        k <- n%/%2
        l1 <- k + 1 - even.n
        l2 <- n - k + even.n
        c(if (l1 > 0) hsv(h = low,
                          s = seq.int(smax, ifelse(even.n, 0.5/k, 0), length.out = l1),
                          v = 1, alpha = alpha),
          if (l2 > 1) hsv(h = high,
                          s = seq.int(0, smax, length.out = l2)[-1],
                          v = 1, alpha = alpha))
    }
    else character(0)
}
oceColorsTwo <- oce.colorsTwo

oce.colorsJet <- function(n)
{
    if (missing(n) || n <= 0)
        colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                           "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
    else {
        colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                           "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))(n)
    }
}
oceColorsJet <- oce.colorsJet

oce.colors9A <- function(n)
{
    oce.colorsJet(n)
}
oceColors9A <- oce.colors9A

oce.colors9B <- function(n)
{
    if (missing(n) || n <= 0)
        colorRampPalette(c("#00007F", "blue", "#007FFF", "#22e4e7",
                           "white", "#ffe45e", "#FF7F00", "red", "#7F0000"))
    else {
        colorRampPalette(c("#00007F", "blue", "#007FFF", "#22e4e7",
                           "white", "#ffe45e", "#FF7F00", "red", "#7F0000"))(n)
    }
}
oceColors9B <- oce.colors9B


oce.colorsPalette <- function(n, which=1)
{
    if ((n <- as.integer(n[1])) > 0) {
        if (which == 1) {
            ## Started with http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer.html
            ## RdBu 11 divisions
            ## and then smoothed the components with smooth.spline(...,df=6)
            rgb <- matrix(c(
                            103, 000, 026,
                            178, 024, 046,
                            214, 096, 072,
                            244, 165, 136,
                            253, 219, 195,
                            247, 247, 247,
                            209, 229, 238,
                            146, 197, 226,
                            067, 147, 184,
                            033, 102, 179,
                            005, 048,  97), ncol=3, byrow=TRUE) / 255
            m <- dim(rgb)[1]
            i <- 1:m
            xout <- seq(1, m, length.out=n)
            rev(rgb(red=approx(i, rgb[,1], xout, rule=1)$y,
                    green=approx(i, rgb[,2], xout, rule=1)$y,
                    blue=approx(i, rgb[,3], xout, rule=1)$y,
                    alpha=1))
        } else if (which == 2) {
            ## http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer.html
            m <- 11                         # number of classes
            r <- c(165, 215, 244, 253, 254, 255, 224, 171, 116,  69,  49) / 255
            g <- c(  0,  48, 109, 174, 224, 255, 243, 217, 173, 117,  54) / 255
            b <- c( 38,  39,  67,  97, 144, 191, 248, 233, 209, 180, 149) / 255
            i <- 1:m
            xout <- seq(1, m, length.out=n)
            rev(rgb(approx(i, r, xout, rule=1)$y,
                    approx(i, g, xout, rule=1)$y,
                    approx(i, b, xout, rule=1)$y))
        } else if (which == 9.01 || which == "9A" || which == "jet") { # jet, also known as 9A or 9.01
            oce.colorsJet(n)
        } else if (which == 9.02 || which == "9B") {
            oce.colors9B(n)
        } else stop("unknown which")
    }
    else character(0)
}
oceColorsPalette <- oce.colorsPalette

oce.axis.POSIXct <- function (side, x, at, tformat, labels = TRUE,
                              drawTimeRange=getOption("oceDrawTimeRange"),
                              abbreviateTimeRange=FALSE, drawFrequency=FALSE,
                              cex=par("cex"), cex.axis=par("cex.axis"), cex.main=par("cex.main"),
                              mar=par("mar"),
                              mgp=par("mgp"),
                              main="",
                              debug=getOption("oceDebug"), ...)
{
    oceDebug(debug, "oce.axis.POSIXct(...,debug=", debug, ",...) {\n", sep="", unindent=1)
    oceDebug(debug,"mar=",mar,"\n")
    oceDebug(debug,"mgp=",mgp,"\n")
    oceDebug(debug,"cex=",cex," cex.axis=", cex.axis, " cex.main=", cex.main, "\n")
    oceDebug(debug,vectorShow(x, "x"))
    ## This was written because axis.POSIXt in R version 2.8.x did not obey the
    ## time zone in the data.  (Version 2.9.0 obeys the time zone.)
    if (missing(x))
        x <- numberAsPOSIXct(par('usr')[1:2])
    dots <- list(...)
    if ("xlim" %in% names(dots)) {
        ok <- dots$xlim[1] <= x & x <= dots$xlim[2]
        x <- x[ok]
    }
    mat <- missing(at) || is.null(at)
    if (!mat) x <- as.POSIXct(at) else x <- as.POSIXct(x)
    range <- par("usr")[if (side%%2) 1:2 else 3:4]
    d <- range[2] - range[1]            # time span, in seconds
    z <- c(range, x[is.finite(x)])
    attr(z, "tzone") <- attr(x, "tzone") # need this because c() makes it local time zone (!)
    rr <- range + as.POSIXct("2000-01-20") - as.numeric(as.POSIXct("2000-01-20"))
    attr(rr, "tzone") <- attr(x, "tzone")
    oceDebug(debug, "range=",
              format.POSIXct(rr[1], "%Y-%m-%d %H:%M:%S", tz="UTC"),
              "UTC to ",
              format.POSIXct(rr[2], "%Y-%m-%d %H:%M:%S", tz="UTC"),
              "UTC\n")
    z.sub <- NULL # unlabelled tics may be set in some time ranges, e.g. hours, for few-day plots
    oceDebug(debug, "d=", d, " (time range)\n")
    if (d < 2) {
        ## The time rounding will fail for very small time intervals;
        ## a wider range can be added easily.
        t.start <- rr[1]
        t.end <- rr[2]
        span <- as.numeric(t.end) - as.numeric(t.start)
        if (     span > 1     ) round <- 0.5
        else if (span > 0.1   ) round <- 0.05
        else if (span > 0.01  ) round <- 0.005
        else if (span > 0.001 ) round <- 0.0005
        else if (span > 0.0001) round <- 0.00005
        else round <- 0.00001
        t0 <- trunc(t.start, "sec")
        t.start <- t0+round*floor((as.numeric(t.start)-as.numeric(t0))/round)
        t.end <- t0+round*floor((as.numeric(t.end)-as.numeric(t0))/round)
        z <- seq(t.start, t.end, by=round)
        oceDebug(debug, "time range is under 5 seconds\n")
        oceDebug(debug, vectorShow(z, "z"))
        if (missing(tformat))
            tformat <- "%OS" # FIXME: not too useful if span is under 1ms
    } else if (d < 60) {                       # under a min
        t.start <- trunc(rr[1]-1, "secs")
        t.end <- trunc(rr[2]+1, "secs")
        z <- seq(t.start, t.end, by="1 sec")
        oceDebug(debug, "time range is under a minute\n")
        oceDebug(debug, vectorShow(z, "z"))
        if (missing(tformat))
            tformat <- "%S"
    } else if (d < 60 * 3) {                       # under 3 min
        t.start <- trunc(rr[1]-60, "mins")
        t.end <- trunc(rr[2]+60, "mins")
        z <- seq(t.start, t.end, by="10 sec")
        oceDebug(debug, "time range is under 3 minutes\n")
        oceDebug(debug, vectorShow(z, "z"))
        if (missing(tformat))
            tformat <- "%H:%M:%S"
    } else if (d < 60 * 30) {                  # under 30min
        t.start <- trunc(rr[1]-30, "mins")
        t.end <- trunc(rr[2]+30, "mins")
        z <- seq(t.start, t.end, by="min")
        oceDebug(debug, "time range is under 30 min\n")
        oceDebug(debug, vectorShow(z, "z"))
        if (missing(tformat))
            tformat <- "%H:%M"
    } else if (d < 60 * 60) {                  # under 1 hour
        t.start <- trunc(rr[1]-30, "mins")
        t.end <- trunc(rr[2]+30, "mins")
        z <- seq(t.start, t.end, by="10 min")
        oceDebug(debug, vectorShow(z, "Time range is under an hour; z="))
        if (missing(tformat))
            tformat <- "%H:%M"
    } else if (d < 60 * 60 * 2) {       # under 2 hours
        t.start <- trunc(rr[1]-30, "mins")
        t.end <- trunc(rr[2]+30, "mins")
        z <- seq(t.start, t.end, by="10 min")
        oceDebug(debug, vectorShow(z, "Time range is under 2 hours; z="))
        if (missing(tformat))
            tformat <- "%H:%M"
    } else if (d < 60 * 60 * 6) {       # under 6 hours, use HM
        t.start <- trunc(rr[1], "hour")
        t.end <- trunc(rr[2] + 3600, "hour")
        z <- seq(t.start, t.end, by="30 min")
        oceDebug(debug, vectorShow(z, "Time range is under 6 hours; z="))
        if (missing(tformat))
            tformat <- "%H:%M"
    } else if (d < 60 * 60 * 30) {       # under about a day
        t.start <- trunc(rr[1], "hour")
        t.end <- trunc(rr[2] + 3600, "hour")
        z <- seq(t.start, t.end, by="1 hour")
        oceDebug(debug, vectorShow(z, "Time range is under 30 hours, so z="))
        if (missing(tformat))
            tformat <- "%H"
    } else if (d <= 60 * 60 * 24 * 3) {        # under 3 days: label day; show 1-hour subticks
        t.start <- trunc(rr[1], "day")
        t.end <- trunc(rr[2] + 86400, "day")
        z <- seq(t.start, t.end, by="hour")
        z.sub <- seq(t.start, t.end, by="hour")
        oceDebug(debug, vectorShow(z, "Time range is under 3 days; z="))
        oceDebug(debug, vectorShow(z.sub, "Time range is under 3 days; z.sub="))
        if (missing(tformat))
            tformat <- "%H"             #b %d"
    } else if (d <= 60 * 60 * 24 * 5) {        # under 5 days: label day; show 2-h subticks
        t.start <- trunc(rr[1], "day")
        t.end <- trunc(rr[2] + 86400, "day")
        z <- seq(t.start, t.end, by="day")
        z.sub <- seq(t.start, t.end, by="2 hour")
        oceDebug(debug, vectorShow(z, "Time range is under 5 days; z="))
        oceDebug(debug, vectorShow(z.sub, "Time range is under 5 days; z.sub="))
        if (missing(tformat))
            tformat <- "%b %d"
    } else if (d <= 60 * 60 * 24 * 14) { # under 2 weeks: label day; show 12-h subticks
        t.start <- trunc(rr[1], "day")
        t.end <- trunc(rr[2] + 86400, "day")
        z <- seq(t.start, t.end, by="day")
        z.sub <- seq(t.start, t.end, by="12 hour")
        oceDebug(debug, vectorShow(z, "Time range is under 2 weeks; z="))
        if (missing(tformat))
            tformat <- "%b %d"
    } else if (d <= 60 * 60 * 24 * 31) { # under 1 month: label day
        t.start <- trunc(rr[1], "day")
        t.end <- trunc(rr[2] + 86400, "day")
        z <- seq(t.start, t.end, by="day")
        oceDebug(debug, vectorShow(z, "Time range is under a month; z="))
        if (missing(tformat))
            tformat <- "%b %d"
    } else if (d < 60 * 60 * 24 * 31 * 2) {        # under 2 months
        t.start <- trunc(rr[1], "day")
        t.end <- trunc(rr[2] + 86400, "day")
        z <- seq(t.start, t.end, by="day")
        oceDebug(debug, vectorShow(z, "Time range is under 2 months; z="))
        if (missing(tformat))
            tformat <- "%b %d"
    } else if (d < 60 * 60 * 24 * 31 * 4) {        # under 4 months
        t.start <- trunc(rr[1], "days")
        t.end <- trunc(rr[2] + 86400, "days")
        z <- seq(t.start, t.end, by="week")
        oceDebug(debug, vectorShow(z, "Time range is under 4 months; z="))
        if (missing(tformat))
            tformat <- "%b %d"
    } else if (d < 1.1 * 60 * 60 * 24 * 365) { # under about a year
        rrl <- as.POSIXlt(rr)
        rrl[1]$mday <- 1
        rrl[2] <- rrl[2] + 31 * 86400
        rrl[2]$mday <- 1
        t.start <- trunc(rrl[1], "day")
        t.end <- trunc(rrl[2] + 86400, "day")
        z <- seq(t.start, t.end, by="month")
        oceDebug(debug, vectorShow(z, "Time range is under a year or so; z="))
        if (missing(tformat))
            tformat <- "%b %d"
    } else if (d < 3.1 * 60 * 60 * 24 * 365) { # under about 3 years
        rrl <- as.POSIXlt(rr)
        rrl[1]$mday <- 1
        rrl[2] <- rrl[2] + 31 * 86400
        rrl[2]$mday <- 1
        t.start <- trunc(rrl[1], "days")
        t.end <- trunc(rrl[2], "days")
        z <- seq(t.start, t.end, by="1 month")
        if (missing(tformat))
            tformat <- "%b %Y"
    } else { # FIXME: do this as above.  Then remove the junk near the top.
        class(z) <- c("POSIXt", "POSIXct")
        attr(z, "tzone") <- attr(x, "tzone")
        zz <- as.POSIXlt(z)
        zz$mday <- zz$wday <- zz$yday <- 1
        zz$isdst <- -1
        zz$mon <- zz$hour <- zz$min <- zz$sec <- 0
        zz$year <- pretty(zz$year)
        M <- length(zz$year)
        zz <- lapply(zz, function(x) rep(x, length.out = M))
        class(zz) <- c("POSIXt", "POSIXlt")
        z <- as.POSIXct(zz)
        attr(z, "tzone") <- attr(x, "tzone")
        if (missing(tformat))
            tformat <- "%Y"
        oceDebug(debug, vectorShow(z, "z="))
    }
    if (!mat)
        z <- x[is.finite(x)]
    ##
    ## FIXME: I was twiddling the numbers, to get more labels, but xaxs="r" fixes that.
    twiddle <- 0.04 * diff(as.numeric(range))  # FIXME: do I need this anymore?
    oceDebug(debug, "range=",
              format.POSIXct(rr[1], "%Y-%m-%d %H:%M:%S", tz="UTC"),
              "UTC to ",
              format.POSIXct(rr[2], "%Y-%m-%d %H:%M:%S", tz="UTC"),
              "UTC\n")
    keep <- range[1] <= (z + twiddle) & (z - twiddle) <= range[2]
    ##oceDebug(debug, vectorShow(keep, "keep"))
    oceDebug(debug, vectorShow(z, "z before keep"))
    z <- z[keep]
    oceDebug(debug, vectorShow(z, "z after keep"))
    if (!is.logical(labels))
        labels <- labels[keep]
    else if (identical(labels, TRUE))
        labels <- format(z, format = tformat)
    else if (identical(labels, FALSE))
        labels <- rep("", length(z))
    if (drawTimeRange) {
        time.range <- par("usr")[1:2]   # axis, not data
        class(time.range) <- c("POSIXt", "POSIXct")
        attr(time.range, "tzone") <- attr(x, "tzone")[1]
        time.range <-  as.POSIXlt(time.range)
        ## time.range.data <- range(x, na.rm=TRUE)
        ## what was this for?# time.range[1] <- max(time.range[1], time.range.data[1], na.rm=TRUE)
        ## what was this for?# time.range[2] <- min(time.range[2], time.range.data[2], na.rm=TRUE)
        tr1 <- format(time.range[1], getOption("oceTimeFormat"))
        tr2 <- format(time.range[2], getOption("oceTimeFormat"))
        if (abbreviateTimeRange) {
            if (time.range[1]$year == time.range[2]$year) {
                tr2 <- substr(tr2, 6, nchar(tr2)) # remove the "YYYY-"
                if (time.range[1]$mon == time.range[2]$mon) {
                    tr2 <- substr(tr2, 4, nchar(tr2)) # remove the "MM-"
                    if (time.range[1]$mday == time.range[2]$mday) {
                        tr2 <- substr(tr2, 4, nchar(tr2)) # remove the "DD-"
                    }
                }
            }
            time.range <- as.POSIXct(time.range)
        }
        deltat <- mean(diff(as.numeric(x)), na.rm=TRUE)
        ## only show timezone if hours are shown
        oceDebug(debug, "time.range[1]:", format(time.range[1]), "\n")
        oceDebug(debug, "round(time.range[1], 'days'):", format(round(time.range[1],'days')), "\n")
        oceDebug(debug, "time.range[2]:", format(time.range[2]), "\n")
        oceDebug(debug, "round(time.range[2], 'days'):", format(round(time.range[2],'days')), "\n")
        ## The below is not fool-proof, depending on how xlim might have been supplied; see
        ##    https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=14449
        if (time.range[1] == round(time.range[1],"days") && time.range[2] == round(time.range[2],"days")) {
            label <- paste(tr1, tr2, sep=" to ")
        } else {
            label <- paste(tr1, attr(time.range[1], "tzone")[1], " to ", tr2,  attr(time.range[2], "tzone")[1], sep="")
        }
        if (drawFrequency && is.finite(1/deltat))
            label <- paste(label, "@", sprintf("%.4g Hz", 1/deltat), sep=" ")
        oceDebug(debug, "label=", label, "\n")
        mtext(label, side=if (side==1) 3 else 1, cex=0.9*cex.axis*par('cex'), adj=0)
        oceDebug(debug, "cex.axis=", cex.axis, "; par('cex')=", par('cex'), "\n")
    }
    if (nchar(main) > 0) {
        mtext(main, side=if(side==1) 3 else 1, cex=cex.axis*par('cex'), adj=1)
    }
    ## FIXME: why an axis() here and also in a dozen lines?
    oceDebug(debug, vectorShow(z, "z="))
    if (length(z.sub) > 0) {
        axis(side, at = z.sub, line=0, labels = FALSE, tcl=-0.25)
        oceDebug(debug, vectorShow(z.sub, "z.sub="))
    }
    oceDebug(debug, vectorShow(labels, "labels="))
    ocex <- par('cex')
    ocex.axis <- par('cex.axis')
    ocex.main <- par('cex.main')
    omgp <- par('mgp')
    par(cex.axis=cex.axis, cex.main=cex.main, mgp=mgp, tcl=-0.5)
    ##axis(side, at=z, line=0, labels=labels, cex=cex, cex.axis=cex.axis, cex.main=cex.main, mar=mar, mgp=mgp)
    axis(side, at=z, line=0, labels=labels, mgp=mgp, cex.main=cex.main, cex.axis=cex.axis, ...)
    par(cex.axis=ocex.axis, cex.main=cex.main, mgp=omgp)
    oceDebug(debug, "} # oce.axis.ts()\n", unindent=1)
    zzz <- as.numeric(z)
    par(xaxp=c(min(zzz, na.rm=TRUE), max(zzz, na.rm=TRUE), -1+length(zzz)))
    invisible(z)                       # FIXME: or z.sub?
}

numberAsHMS <- function(t, default=0)
{
    if ("factor" == class(t))
        t <- as.character(t)
    if (!is.character(t))
        stop("can only handle strings or factors")
    nc <- nchar(t)
    t[nc == 0] <- "0000"
    nc <- nchar(t)
    t[nc==1] <- paste("000", t[nc==1], sep="")
    t[nc==2] <- paste("00", t[nc==2], sep="")
    t[nc==3] <- paste("0", t[nc==3], sep="")
    nc <- nchar(t)
    warn <- options('warn')$warn
    options(warn=-1)
    try({
        hour <- as.numeric(substr(t, 1, 2))
        minute <- as.numeric(substr(t, 3, 4))
        second <- rep(0, length(hour))
    }, silent=TRUE)
    options(warn=warn)
    bad <- nc > 4 | grepl('[^[0-9]]*', t)
    hour[bad] <- default
    minute[bad] <- default
    second[bad] <- default
    list(hour=hour, minute=minute, second=second)
}

numberAsPOSIXct <- function(t, type=c("unix", "matlab", "gps", "argo", "sas", "spss", "yearday"), tz="UTC")
{
    type <- match.arg(type)
    if (type == "unix") {
        tref <- as.POSIXct("2000-01-01", tz=tz) # arbitrary
        return((as.numeric(t) - as.numeric(tref)) + tref)
    } else if (type == "matlab") {
        ## R won't take a day "0", so subtract one
        return(as.POSIXct(ISOdatetime(0000, 01, 01, 0, 0, 0, tz=tz) + 86400 * (t - 1)))
    } else if (type == "yearday") {
        if (2 != ncol(t))
            stop("'t' must have two columns, one for year, the other for yearday")
        return(ISOdatetime(t[,1], 1, 1, 0, 0, 0, tz=tz) + 1 + t[,2] * 24 * 3600)
    } else if (type == "argo") {
        return(t * 86400 + as.POSIXct("1900-01-01", tz="UTC"))
    } else if (type == "argo") {
        return(t * 86400 + as.POSIXct("1900-01-01", tz="UTC"))
    } else if (type == "gps") {
        if (!is.matrix(t) || dim(t)[2] != 2)
            stop("for GPS times, 't' must be a two-column matrix, with first col the week, second the second")

        ## Account for leap seconds since the GPS start time in 1980 (for the present week wraparound grouping).
        ## See http://en.wikipedia.org/wiki/Leap_second and other sources for a list.  Updates can happen
        ## on June 30 and December 31 of any given year.  The information below is correct as of 2014-07-01,
        ## which is the day after the June 2014 update possibility.
        leaps <- as.POSIXct(strptime(c("1981-07-01", "1982-07-01", "1983-07-01", "1985-07-01", "1987-01-01",
                                       "1989-01-01", "1990-01-01", "1992-07-01", "1993-07-01", "1994-07-01",
                                       "1995-01-01", "1997-07-01", "1998-01-01", "2005-01-01", "2008-01-01",
                                       "2012-07-01"),
                                     format="%Y-%m-%d", tz="UTC"))
        t <- as.POSIXct("1999-08-22 00:00:00",tz="UTC") + 86400*7*t[,1] + t[,2]
        for (l in 1:length(leaps)) {
            t <- t - ifelse(t >= leaps[l], 1, 0)
        }
    } else if (type == "spss") {
        t <- as.POSIXct(t, origin="1582-10-14", tz=tz)
    } else if (type == "sas") {
        t <- as.POSIXct(t, origin="1960-01-01", tz=tz)
    } else {
        stop("type must be \"unix\", \"matlab\" or \"GPS\"")
    }
    t
}

plotInset <- function(xleft, ybottom, xright, ytop, expr,
                      mar=c(2, 2, 1, 1),
                      debug=getOption("oceDebug"))
{
    omfg <- par('mfg')                 # original mfg
    xLog <- par('xlog')
    yLog <- par('ylog')
    x2in <- function(x) {
        if (xLog)
            mai[2] + (log10(x) - usr[1]) * (fin[1]-mai[2]-mai[4]) / (usr[2]-usr[1])
        else
            mai[2] + (x-usr[1]) * (fin[1]-mai[2]-mai[4]) / (usr[2]-usr[1])
    }
    y2in <- function(y) {
        if (yLog)
            mai[1] + (log10(y) - usr[3]) * (fin[2]-mai[1]-mai[3]) / (usr[4]-usr[3])
        else
            mai[1] + (y-usr[3]) * (fin[2]-mai[1]-mai[3]) / (usr[4]-usr[3])
    }
 
    usr <- par('usr')                  # xmin xmax ymin ymax
    if (is.character(xleft)) {
        if (xleft != "bottomleft")
            stop("only named position is \"bottomleft\"")
        f1 <- 0.02
        f2 <- 1/3
        if (xLog) {
            stop("cannot handle xlog yet")
        } else {
            xleft <- usr[1] + f1 * (usr[2] - usr[1])
            xright <- usr[1] + f2 * (usr[2] - usr[1])
        }
        if (yLog) {
            stop("cannot handle ylog yet")
        } else {
            ybottom <- usr[3] + f1 * (usr[4] - usr[3])
            ytop <- usr[3] + f2 * (usr[4] - usr[3])
        }
    } else {
        oceDebug(debug, "plotInset(xleft=", xleft, ", ybottom=", ybottom,
                 ", xright=", xright, ", ytop=", ytop,
                 ",  ...) {\n",
                 sep="", unindent=1)
    }
    oceDebug(debug, "par('mfg')=", par('mfg'), "\n")
    opar <- par(no.readonly=TRUE)
    mai <- par('mai')                  # bottom left top right
    oceDebug(debug, "par('mai')=", par('mai'), '\n')
    oceDebug(debug, "par('usr')=", par('usr'), '\n')
    fin <- par('fin') # figure width height
    oceDebug(debug, "par('fin')=", fin, "(figure width and height)\n")
    nmai <- c(y2in(ybottom), x2in(xleft), fin[2]-y2in(ytop), fin[1]-x2in(xright))
    oceDebug(debug, "nmai:", nmai, "\n")
    if (any(nmai < 0)) {
        warning("part of the inset is off the page")
    }
    nmai[nmai<0] <- 0
    if (nmai[1] < 0) nmai[1] <- {fin[1]}
    if (nmai[2] < 0) nmai[2] <- {fin[1]}
    if (nmai[3] > fin[2] - 0.2) {nmai[3] <- fin[2] - 0.2}
    if (nmai[4] > fin[1] - 0.2) {nmai[4] <- fin[1] - 0.2}
    oceDebug(debug, "nmai:", nmai, "(after trimming negatives)\n")
    mfg2 <- par('mfg')
    par(new=TRUE, mai=nmai)
    thismar <- par('mar')
    par(mar=thismar+mar)
    if (debug > 1) {
        cat("\n\nBEFORE expr, PAR IS:\n");
        str(par())
    }
    mfg <- par('mfg')
    oceDebug(debug, "BEFORE expr, mfg=", mfg, "\n")
    ## Draw the inset plot (or perform any action, actually)
    expr
    if (mfg[1] == mfg[3] && mfg[2] == mfg[4]) {
        oceDebug(debug, "setting new=FALSE; mfg=", mfg, "\n")
        par(new=FALSE)
    } else {
        oceDebug(debug, "setting new=TRUE; mfg=", mfg, "\n")
        par(new=TRUE)
    }
    ## Reset some things that could have been set in the inset, and
    ## then adjust 'new' appropriately.
    par(usr=opar$usr, mai=opar$mai, cex=opar$cex, lwd=opar$lwd, lty=opar$lty, bg=opar$bg)
    oceDebug(debug, "} # plotInset()\n", unindent=1)
    invisible()
}

decodeTime <- function(time, timeFormats, tz="UTC")
{
    if (missing(timeFormats))
        timeFormats <- c("%b %d %Y %H:%M:%S", "%b %d %Y", # Jul 1 2013
                         "%B %d %Y %H:%M:%S", "%B %d %Y", # July 1 2013
                         "%d %b %Y %H:%M:%S", "%d %b %Y", # 1 Jul 2013
                         "%d %B %Y %H:%M:%S", "%d %B %Y", # 1 July 2013
                         "%Y-%m-%d %H:%M:%S", "%Y-%m-%d", # 2013-07-01
                         "%Y-%b-%d %H:%M:%S", "%Y-%b-%d", # 2013-Jul-01
                         "%Y-%B-%d %H:%M:%S", "%Y-%B-%d", # 2013-July-01
                         "%d-%b-%Y %H:%M:%S", "%d-%b-%Y", # 01-Jul-2013
                         "%d-%B-%Y %H:%M:%S", "%d-%B-%Y", # 01-July-2013
                         "%Y/%m/%d %H:%M:%S", "%Y/%m/%d", # 2013/07/01
                         "%Y/%b/%d %H:%M:%S", "%Y/%b/%d", # 2013/Jul/01
                         "%Y/%B/%d %H:%M:%S", "%Y/%B/%d", # 2013/July/01
                         "%Y/%m/%d %H:%M:%S", "%Y/%m/%d") # 2013/07/01
    ## FIXME: permit time to be a vector
    rval <- NA
    for (format in timeFormats) {
        ##cat("TRYING FORMAT:", format, "\n")
        if (!is.na(rval <-  as.POSIXct(time, format=format, tz=tz))) {
            break
        }
    }
    rval
}

drawDirectionField <- function(x, y, u, v, scalex, scaley, add=FALSE,
                               type=1, debug=getOption("oceDebug"), ...)
{
    oceDebug(debug, "drawDirectionField(...) {\n", unindent=1)
    if (missing(x) || missing(y) || missing(u) || missing(v))
        stop("must supply x, y, u, and v")
    if ((missing(scalex) && missing(scaley)) || (!missing(scalex) && !missing(scaley)))
        stop("either 'scalex' or 'scaley' must be specified (but not both)")
    if (length(x) != length(y))
        stop("lengths of x and y must match")
    if (length(x) != length(u))
        stop("lengths of x and u must match")
    if (length(x) != length(v))
        stop("lengths of x and v must match")
    usr <- par('usr')
    pin <- par('pin')
    mai <- par('mai')
    xPerInch <- diff(usr[1:2]) / pin[1]
    yPerInch <- diff(usr[3:4]) / pin[2]
    oceDebug(debug, 'pin=', pin, 'usr=', usr, 'xPerInch=', xPerInch, 'yPerInch=', yPerInch, '\n')
    if (missing(scaley)) {
        oceDebug(debug, "scaling for x\n")
        uPerX <- 1 / scalex
        vPerY <- uPerX * xPerInch / yPerInch
    } else {
        vPerY <- 1 / scaley
        uPerX <- vPerY * yPerInch / xPerInch
        oceDebug(debug, "scaling for y\n")
    }
    oceDebug(debug, 'uPerX=', uPerX, '\n')
    oceDebug(debug, 'vPerY=', vPerY, '\n')
    if (type == 1) {
        if (add)
            points(x, y, ...)
        else 
            plot(x, y, ...) 
        segments(x, y, x + u / uPerX, y + v / vPerY, ...)
    } else if (type == 2) {
        if (!add)
            plot(x, y, ...)
        arrows(x, y, x + u / uPerX, y + v / vPerY, ...)
    } else {
        stop("unknown value of type ", type)
    }
    oceDebug(debug, "} # drawDirectionField\n", unindent=1)
}

oce.contour <- function(x, y, z, revx=FALSE, revy=FALSE, add=FALSE,
                       tformat, drawTimeRange=getOption("oceDrawTimeRange"),
                       debug=getOption("oceDebug"), ...)
{
    dots <- list(...)
    dotsNames <- names(dots)
    mustReverseX <- any(0 > diff(order(x)))
    mustReverseY <- any(0 > diff(order(y)))
    oceDebug(debug, "mustReverseX:", mustReverseX, '\n')
    oceDebug(debug, "mustReverseY:", mustReverseY, '\n')

    ## perhaps get (x,y,z) from x, etc., trying to emulate contour()
    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            } else {
                z <- x
                x <- seq.int(0, 1, length.out = nrow(z))
            }
        } else stop("no 'z' matrix specified")
    } else if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    zdim <- dim(z)


    ## store local values for the tricky cases of reversing axes etc
    xx <- x
    yy <- y
    zz <- z
    if (mustReverseX) {
        xx <- rev(xx)
        zz <- zz[seq.int(zdim[1], 1),]
    }
    if (mustReverseY) {
        yy <- rev(yy)
        zz <- zz[,seq.int(zdim[2], 1)]
    }
    if (add) {
        contour(xx, yy, zz, add=TRUE, ...)
    } else {
        if (revx) {
            xx <- rev(xx)
        }
        if (revy) {
            yy <- rev(yy)
        }
        contour(xx, yy, zz, axes=FALSE, ...)
        ## see src/library/graphics/R/contour.R 
        xaxp <- par('xaxp')
        xat <- seq(xaxp[1], xaxp[2], length.out=xaxp[3])
        xlabels <- format(xat)
        yaxp <- par('yaxp')
        yat <- seq(yaxp[1], yaxp[2], length.out=yaxp[3])
        ylabels <- format(yat)
        xIsTime <- inherits(x, "POSIXt") || inherits(x, "POSIXct") || inherits(x, "POSIXlt")

        if (revx) {
            if (xIsTime) {
                oce.axis.POSIXct(side=1, x=x,
                                 drawTimeRange=drawTimeRange,
                                 #mar=mar, mgp=mgp, 
                                 tformat=tformat, debug=debug-1)
            } else {
                Axis(xx, side=1, at=xat, labels=rev(xlabels))
            }
        } else {
            if (xIsTime) {
                oce.axis.POSIXct(side=1, x=x,
                                 drawTimeRange=drawTimeRange,
                                 #mar=mar, mgp=mgp,
                                 tformat=tformat, debug=debug-1)
            } else {
                Axis(xx, side=1)
            }
        }
        if (revy) {
            Axis(yy, side=2, at=yat, labels=rev(ylabels))
        } else {
            Axis(yy, side=2)
        }
        box()
    }
}
oceContour <- oce.contour

