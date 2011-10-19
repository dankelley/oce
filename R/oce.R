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
    res@processingLog <- processingLog(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}

window.oce <- function(x, start = NULL, end = NULL, frequency = NULL, deltat = NULL, extend = FALSE,
                       which=c("time","distance"), indexReturn=FALSE,
                       debug=getOption("oceDebug"), ...)
{
    oceDebug(debug, "\b\bwindow.oce(...,start=",
             paste(format(start),collapse=","), ",end=",
             paste(format(end),collapse=","),
             ",indexReturn=",indexReturn,",...) {\n")
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
                        oceDebug(debug, "subsetting 'slow' variable data$", name, "\n", sep="")
                        res@data[[name]] <- x@data[[name]][keepSlow]
                    } else {
                        oceDebug(debug, "subsetting data$", name, "\n", sep="")
                        res@data[[name]] <- x@data[[name]][keep]
                    }
                } else if (is.matrix(res@data[[name]])) {
                    oceDebug(debug, "subsetting data$", name, ", which is a matrix\n", sep="")
                    res@data[[name]] <- x@data[[name]][keep,]
                } else if (is.array(res@data[[name]])) {
                    oceDebug(debug, "subsetting data$", name, ", which is an array\n", sep="")
                    res@data[[name]] <- x@data[[name]][keep,,]
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
    oceDebug(debug, "\b\b} # window.oce()\n")
    res
}

oceApprox <- function(x, y, xout, method=c("reiniger-ross"))
{
    method <- match.arg(method)
    if (method != "reiniger-ross")
        stop("only available method is \"reiniger-ross\"")
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
    if (missing(xout))
        xout <- seq(min(x), max(x), length.out=lx)
    else
        if (any(is.na(xout)))
            stop("must not have any NA values in xout")
    .Call("oce_approx", x=x[o], y=y[o], xout=xout)
}

oce.plot.sticks <- function(x, y, u, v, yscale=1, add=FALSE, length=1/20,
                            mgp=getOption("oceMgp"),
                            mar=c(mgp[1]+1,mgp[1]+1,1,1+par("cex")),
                            ...)
{
    pin <- par("pin")
    page.ratio <- pin[2]/pin[1]
    if (missing(x))
        stop("must supply x")
    if (missing(y))
        stop("must supply y")
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


oce.plot.ts <- function(x, y, type="l", xlim, ylim, xlab="", ylab="",
                        drawTimeRange=TRUE, xaxs="r", grid=TRUE, adorn=NULL, fill=FALSE,
                        cex=par("cex"), cex.axis=par("cex.axis"), cex.main=par("cex.main"),
                        mgp=getOption("oceMgp"),
                        mar=c(mgp[1]+if(nchar(xlab)>0) 1 else 0.5,
                              mgp[1]+if(nchar(ylab)>0) 1.5 else 1,
                              mgp[2]+1,
                              mgp[2]+3/4),
                        main="",
                        despike=FALSE,
                        axes=TRUE,
                        debug=getOption("oceDebug"),
                        ...)
{
    ocex <- par("cex")
    #par(cex=cex)
    debug <- min(debug, 4)
    oceDebug(debug, "\boce.plot.ts(...,debug=", debug, ", type=\"", type, "\", mar=c(", paste(mar, collapse=", "), "), ...) {\n",sep="")
    oceDebug(debug, "mgp=",mgp,"\n")
    oceDebug(debug, "length(x)", length(x), "; length(y)", length(y), "\n")
    oceDebug(debug, "cex=",cex," cex.axis=", cex.axis, " cex.main=", cex.main, "\n")
    oceDebug(debug, "mar=c(",paste(mar, collapse=","), ")\n")
    oceDebug(debug, "x has timezone", attr(x[1], "tzone"), "\n")
    par(mgp=mgp, mar=mar)
    args <- list(...)
    xlimGiven <- !missing(xlim)
    if (xlimGiven) {
        if (2 != length(xlim))
            stop("'xlim' must be of length 2")
        if (xlim[2] <= xlim[1])
            stop("the elements of xlim must be in order")
        ok <- xlim[1] <= x & x <= xlim[2]
        x <- x[ok]
        y <- y[ok]
    }
    if (length(y) == 1)
        y <- rep(y, length(x))
    if (despike)
        y <- despike(y)
    if (fill) {
        xx <- c(x[1], x, x[length(x)])
        yy <- c(0, y, 0)
        plot(x, y, axes=FALSE, xaxs=xaxs, xlab=xlab,
             ylab=if (missing(ylab)) deparse(substitute(y)) else ylab,
             type=type, cex=cex, ...)
        fillcol <- if ("col" %in% names(args)) args$col else "lightgray" # FIXME: should be a formal argument
        do.call(polygon, list(x=xx, y=yy, col=fillcol))
    } else {
        plot(x, y, axes=FALSE, xaxs=xaxs, xlab=xlab,
             ylab=if (missing(ylab)) deparse(substitute(y)) else ylab,
             ylim=if (missing(ylim)) NULL else ylim,
             type=if (missing(type)) NULL else type, cex=cex, ...)
    }
    if (axes) {
        xlabs <- oce.axis.POSIXct(1, x=x, drawTimeRange=drawTimeRange, main=main,
                                  mgp=mgp,
                                  cex=cex.axis, cex.axis=cex.axis, cex.main=cex.main,
                                  debug=debug-1)#, ...)
        if (grid) {
            lwd <- par("lwd")
            abline(v=xlabs, col="lightgray", lty="dotted", lwd=lwd)
            yaxp <- par("yaxp")
            abline(h=seq(yaxp[1], yaxp[2], length.out=1+yaxp[3]),
                   col="lightgray", lty="dotted", lwd=lwd)
        }
        box()
        ##cat("cex.axis=",cex.axis,"; par('cex.axis') is", par('cex.axis'), "; par('cex') is", par('cex'), "\n")
        axis(2, cex.axis=cex.axis)
        axis(4, labels=FALSE)
    }
    if (!is.null(adorn)) {
        t <- try(eval(adorn, enclos=parent.frame()), silent=TRUE)
        if (class(t) == "try-error")
            warning("cannot evaluate adorn {", format(adorn), "}\n")
    }
    ##par(cex=ocex)
    oceDebug(debug, "\b\b} # oce.plot.ts()\n")
    invisible()
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
        return(.Internal(Date2POSIXlt(x)))
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
    .Internal(as.POSIXlt(x, tz))
}

oceEdit <- function(x, item, value, action, reason="", person="",
                     debug=getOption("oceDebug"))
{
    oceDebug(debug, "\b\boceEdit() {\n")
    if (!inherits(x, "oce") && !inherits(x, "noce")) # FIXME: remove noce
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
    warning("should update processingLog")
    ##x@processingLog <- processingLog(x@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "\b\b} # oceEdit() \n")
    x
}

oce.write.table <- function (x, file="", ...)
{
    if (!inherits(x, "oce"))
        stop("method is only for oce objects")
    if (!("row.names" %in% names(list(...))))
        write.table(x@data, file, row.names=FALSE, ...)
    else
        write.table(x@data, file, ...)
}

subset.oce <- function (x, subset, indices=NULL, debug=getOption("oceDebug"), ...)
{
    debug <- max(0, min(debug, 1))
    oceDebug(debug, "\b\bsubset.oce(..., debug=", debug, ", ...) {\n")
    if (!inherits(x, "oce") && !inherits(x, "noce")) # FIXME: drop noce later
        stop("method is only for oce objects")
    if (inherits(x, "cm")) {
        if (!is.null(indices)) {
            oceDebug(debug, vectorShow(keep, "keeping indices"))
            rval <- x
            keep <- (1:length(x@data$u))[indices]
            for (name in names(rval@data)) {
                rval@data[[name]] <- x@data[[name]][keep]
            }
        } else if (!missing(subset)) {
            subsetString <- deparse(substitute(subset))
            oceDebug(debug, "subsetString='", subsetString, "'\n")
            if (length(grep("time", subsetString))) {
                oceDebug(debug, "subsetting a cm by time\n")
                keep <- eval(substitute(subset), x@data, parent.frame())
                oceDebug(debug, vectorShow(keep, "keeping times at indices:"), "\n")
            } else {
                stop("it makes no sense to subset a \"cm\" object by anything other than index or time")
            }
        } else {
            stop("must supply either 'subset' or 'indices'")
        }
        rval <- x
        for (name in names(x@data)) {
            oceDebug(debug, "subsetting x@data[[", name, "]]", sep="")
            rval@data[[name]] <- x@data[[name]][keep]
        }
    } else if (inherits(x, "adp")) { # FIXME: should be able to select by time or space, maybe others
        if (!is.null(indices)) {
            rval <- x
            keep <- (1:x@metadata$numberOfProfiles)[indices]
            oceDebug(debug, vectorShow(keep, "keeping indices"))
            stop("this version of oce cannot subset adp data by index")
        } else if (!missing(subset)) {
            subsetString <- deparse(substitute(subset))
            oceDebug(debug, "subsetString='", subsetString, "'\n")
            if (length(grep("time", subsetString))) {
                oceDebug(debug, "subsetting an adp by time\n")
                keep <- eval(substitute(subset), x@data, parent.frame())
                oceDebug(debug, vectorShow(keep, "keeping bins:"))
                oceDebug(debug, "number of kept bins:", sum(keep), "\n")
                if (sum(keep) < 2)
                    stop("must keep at least 2 profiles")
                rval <- x
                ## FIXME: are we handling slow timescale data?
                for (name in names(x@data)) {
                    if (name == "time" || is.vector(x@data[[name]])) {
                        if ("distance" == name)
                            next
                        oceDebug(debug, "subsetting x@data$", name, ", which is a vector\n", sep="")
                        rval@data[[name]] <- x@data[[name]][keep] # FIXME: what about fast/slow
                    } else if (is.matrix(x@data[[name]])) {
                        oceDebug(debug, "subsetting x@data$", name, ", which is a matrix\n", sep="")
                        rval@data[[name]] <- x@data[[name]][keep,]
                    } else if (is.array(x@data[[name]])) {
                        oceDebug(debug, "subsetting x@data$", name, ", which is an array\n", sep="")
                        rval@data[[name]] <- x@data[[name]][keep,,]
                    }
                }
            } else if (length(grep("distance", subsetString))) {
                oceDebug(debug, "subsetting an adp by distance\n")
                keep <- eval(substitute(subset), x@data, parent.frame())
                oceDebug(debug, vectorShow(keep, "keeping bins:"), "\n")
                if (sum(keep) < 2)
                    stop("must keep at least 2 bins")
                rval <- x
                rval@data$distance <- x@data$distance[keep]
                for (name in names(x@data)) {
                    if (name == "time")
                        next
                    if (is.array(x@data[[name]]) && 3 == length(dim(x@data[[name]]))) {
                        oceDebug(debug, "subsetting array data[[", name, "]] by distance\n")
                        rval@data[[name]] <- x@data[[name]][,keep,]
                    }
                }
            } else {
                stop("should express the subset in terms of distance or time")
            }
        } else {
            stop("must supply either 'subset' or 'indices'")
        }
    } else if (inherits(x, "section")) {
        if (!is.null(indices)) {        # select a portion of the stations
            n <- length(indices)
            station <- vector("list", n)
            stn <- vector("character", n)
            lon <- vector("numeric", n)
            lat <- vector("numeric", n)
            for (i in 1:n) {
                ii <- indices[i]
                stn[i] <- x@metadata$stationId[ii]
                lat[i] <- x@metadata$latitude[ii]
                lon[i] <- x@metadata$longitude[ii]
                station[[i]] <- x@data$station[[ii]]
            }
            data <- list(station=station)
            metadata <- list(header=x@metadata$header,sectionId=x@metadata$sectionId,stationId=stn,latitude=lat,longitude=lon)
            rval <- new("section")
            rval@metadata <- metadata
            rval@data <- data
            rval@processingLog <- unclass(x@processingLog)
        } else {                        # subset within the stations
            subsetString <- deparse(substitute(subset))
            oceDebug(debug, "subsetString='", subsetString, "'\n")
            rval <- x
            if (length(grep("distance", subsetString))) {
                l <- list(distance=geodDist(rval))
                keep <- eval(substitute(subset), l, parent.frame())
                rval@metadata$latitude <- rval@metadata$latitude[keep]
                rval@metadata$longitude <- rval@metadata$longitude[keep]
                rval@metadata$stationId <- rval@metadata$stationId[keep]
                rval@data$station <- rval@data$station[keep]
            } else if (length(grep("latitude", subsetString)) || length(grep("longitude", subsetString))) {
                n <- length(x@data$station)
                keep <- vector(length=n)
                for (i in 1:n)
                    keep[i] <- eval(substitute(subset), x@data$station[[i]]@metadata, parent.frame())
                nn <- sum(keep)
                station <- vector("list", nn)
                stn <- vector("character", nn)
                lon <- vector("numeric", nn)
                lat <- vector("numeric", nn)
                j <- 1
                for (i in 1:n) {
                    if (keep[i]) {
                        stn[j] <- x@metadata$stationId[i]
                        lat[j] <- x@metadata$latitude[i]
                        lon[j] <- x@metadata$longitude[i]
                        station[[j]] <- x@data$station[[i]]
                        j <- j + 1
                    }
                }
                data <- list(station=station)
                metadata <- list(header=x@metadata$header,
                                 sectionId=x@metadata$sectionId,
                                 stationId=stn,
                                 latitude=lat,
                                 longitude=lon)
                rval <- new('section')
                rval@data <- data
                rval@metadata <- metadata
                rval@processingLog <- x@processingLog
            } else {
                n <- length(x@data$station)
                r <- eval(substitute(subset), x@data$station[[1]]@data, parent.frame())
                for (i in 1:n) {
                    rval@data$station[[i]]@data <- x@data$station[[i]]@data[r,]
                }
            }
        }
    } else if (inherits(x, "pt")) {
        r <- eval(substitute(subset), x@data, parent.frame())
        r <- r & !is.na(r)
        rval <- x
        for (name in names(rval@data)) {
            rval@data[[name]] <- x@data[[name]][r]
        }
    } else if (inherits(x, "sealevel")) {
        warning("not handling subset.oce(sealevel) yet.")
        rval <- x
    } else if (inherits(x, "adv")) {
        if (!is.null(indices))
            stop("cannot specify 'indices' for adv objects (not coded yet)")
        if (missing(subset))
            stop("must specify a 'subset'")
        subsetString <- paste(deparse(substitute(subset)), collapse=" ")
        oceDebug(debug, "subsetString='", subsetString, "'\n")
        if (length(grep("time", subsetString))) {
            oceDebug(debug, "subsetting an adv object by time\n")
            keep <- eval(substitute(subset), x@data, parent.frame()) # used for $ts and $ma, but $tsSlow gets another
            sum.keep <- sum(keep)
            if (sum.keep < 2)
                stop("must keep at least 2 profiles")
            oceDebug(debug, "keeping", sum.keep, "of the", length(keep), "time slots\n")
            oceDebug(debug, vectorShow(keep, "keeping bins:"))
            rval <- x
            names <- names(x@data)
            haveSlow <- "timeSlow" %in% names
            keep <- eval(substitute(subset), x@data, parent.frame()) # used for $ts and $ma, but $tsSlow gets another
            if (haveSlow) {
                subsetStringSlow <- gsub("time", "timeSlow", subsetString)
                keepSlow <-eval(parse(text=subsetStringSlow), x@data, parent.frame())
            }
            if ("timeBurst" %in% names) {
                subsetStringBurst <- gsub("time", "timeBurst", subsetString)
                keepBurst <-eval(parse(text=subsetStringBurst), x@data, parent.frame())
            }
            for (name in names(x@data)) {
                if ("distance" == name)
                    next
                if (length(grep("Burst$", name))) {
                    rval@data[[name]] = x@data[[name]][keepBurst]
                } else if (length(grep("^time", name)) || is.vector(rval@data[[name]])) {
                    if (1 == length(agrep("Slow$", name))) {
                        oceDebug(debug, "subsetting data$", name, " (using an interpolated subset)\n", sep="")
                        rval@data[[name]] <- x@data[[name]][keepSlow]
                    } else {
                        oceDebug(debug, "subsetting data$", name, "\n", sep="")
                        rval@data[[name]] <- x@data[[name]][keep]
                    }
                } else if (is.matrix(rval@data[[name]])) {
                    oceDebug(debug, "subsetting data$", name, ", which is a matrix\n", sep="")
                    rval@data[[name]] <- x@data[[name]][keep,]
                } else if (is.array(rval@data[[name]])) {
                    oceDebug(debug, "subsetting data$", name, ", which is an array\n", sep="")
                    rval@data[[name]] <- x@data[[name]][keep,,]
                }
            }
        } else {
            stop("only 'time' is permitted for subsetting")
        }
    } else if (inherits(x, "ctd")) {
        if (!isS4(x))
            x <- makeS4(x)
        rval <- new("ctd")
        rval@metadata <- x@metadata
        for (i in seq_along(x@data)) {
            ##cat("i=", i, "name=", names(x@data)[i], "\n")
            r <- eval(substitute(subset), x@data, parent.frame())
            r <- r & !is.na(r)
            rval@data[[i]] <- x@data[[i]][r]
        }
        names(rval@data) <- names(x@data)
    } else {
        if (isS4(x)) {
            r <- eval(substitute(subset), x@data, parent.frame())
        } else {
            r <- eval(substitute(subset), x@data, parent.frame())
        }
        r <- r & !is.na(r)
        rval <- x
        if (isS4(x)) {
            for (i in seq_along(x@data))
                rval@data[[i]] <- x@data[[i]][r]
        } else {
            rval@data <- x@data[r,]
        }
        names(rval@data) <- names(x@data)
    }
    if (inherits(x, "adp") || inherits(x, "adv")) {
        rval@metadata$numberOfSamples <- dim(rval@data$v)[1]
        rval@metadata$numberOfCells <- dim(rval@data$v)[2]
    }
    oceDebug(debug, "\b\b} # subset.oce\n")
    ##rval@processingLog <- processingLog(rval@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    ##FIXME: processingLog
    rval
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

magic <- function(file, debug=getOption("oceDebug"))
{
    filename <- file
    if (is.character(file)) {
        oceDebug(debug, "checking filename to see if it matches known patterns\n")
        if (length(grep(".adr$", filename))) {
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
            someLines <- readLines(file, encoding="UTF-8")
            dt <- grep("DATA_TYPE=", someLines)
            if (length(dt) < 1)
                stop("cannot infer type of ODF file")
            subtype <- gsub("[',]", "", tolower(strsplit(someLines[dt[1]], "=")[[1]][2]))
            subtype <- gsub("^\\s*", "", subtype)
            subtype <- gsub("\\s*$", "", subtype)
            return(paste("odf", subtype, sep="/"))
        }
        oceDebug(debug, " no, so not adv/sontek/adr.\n")
        file <- file(file, "r")
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file))
        open(file, "r")
    ## grab a single line of text, then some raw bytes (the latter may be followed by yet more bytes)
    line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
    oceDebug(debug, paste("magic(file=\"", filename, "\", debug=",debug,") found first line of file to be as follows:\n", line, "\n", sep=""))
    close(file)
    file <- file(filename, "rb")
    bytes <- readBin(file, what="raw", n=4)
    oceDebug(debug, paste("magic(file=\"", filename, "\", debug=",debug,") found two bytes in file: 0x", bytes[1], " and 0x", bytes[2], "\n", sep=""))
    on.exit(close(file))
    if (bytes[1] == 0x00 && bytes[2] == 0x00 && bytes[3] == 0x27 && bytes[4] == 0x0a) {
        oceDebug(debug, "this is a shapefile; see e.g. http://en.wikipedia.org/wiki/Shapefile\n")
        return("shapefile")
    }

    if (bytes[1] == 0x10 && bytes[2] == 0x02) {
        ## 'ADPManual v710.pdf' p83
        if (96 == readBin(bytes[3:4], "integer", n=1, size=2,endian="little"))
            oceDebug(debug, "this is adp/sontek (4 byte match)\n")
        else
            oceDebug(debug, "this is adp/sontek (2 byte match, but bytes 3 and 4 should become integer 96)\n")
        return("adp/sontek")
    }
    if (bytes[1] == 0x7f && bytes[2] == 0x7f) {
        oceDebug(debug, "this is adp/rdi\n")
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
        oceDebug(debug, "next.two.bytes:", next.two.bytes,"(e.g. 0x5 0x12 is adv/nortek/vector)\n")
        if (next.two.bytes[1] == 0xa5 && next.two.bytes[2] == 0x12) {
            oceDebug(debug, "this is adv/nortek/vector\n")
            return("adv/nortek/vector")
        }
        if (next.two.bytes[1] == 0xa5 && next.two.bytes[2] == 0x01) {
            oceDebug(debug, "this is adp/nortek/aqudopp\n")
            return("adp/nortek/aquadopp") # p33 SIG
        }
        if (next.two.bytes[1] == 0xa5 && next.two.bytes[2] == 0x2a)  {
            oceDebug(debug, "this is adp/nortek/aqudoppHR\n")
            return("adp/nortek/aquadoppHR") # p38 SIG
        } else
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
    if (0 < regexpr("^[0-9][0-9][0-9][A-Z] ", line)) {
        oceDebug(debug, "this is sealevel\n")
        return("sealevel")
    }
    if (0 < regexpr("^NCOLS[ ]*[0-9]*[ ]*$", line)) {
        oceDebug(debug, "this is topo\n")
        return("topo")
    }
    if ("RBR TDR" == substr(line, 1, 7))  {
        oceDebug(debug, "this is pt\n")
        return("pt")
    }
    if ("BOTTLE"  == substr(line, 1, 6))  {
        oceDebug(debug, "this is section\n")
        return("section")
    }
    oceDebug(debug, "this is unknown\n")
    return("unknown")
}

read.oce <- function(file, ...)
{
    type <- magic(file)
    processingLog <- paste(deparse(match.call()), sep="", collapse="")
    if (type == "shapefile")
        stop("cannot read shapefiles")
    if (type == "adp/rdi")
        return(read.adp.rdi(file, processingLog=processingLog, ...))
    if (type == "adp/sontek")
        return(read.adp.sontek(file, processingLog=processingLog, ...)) # FIXME is pcadcp different?
    if (type == "adp/nortek/aquadopp")
        stop("cannot read adp/nortek/aquadopp files (aquadoppHR is OK, though)")
    if (type == "adp/nortek/aquadoppHR")
        return(read.adp.nortek(file, processingLog=processingLog, ...))
    if (type == "adv/nortek/vector")
        return(read.adv.nortek(file, processingLog=processingLog, ...))
    if (type == "adv/sontek/adr")
        return(read.adv.sontek.adr(file, processingLog=processingLog, ...))
    ## FIXME need adv/sontek (non adr)
    if (type == "interocean/s4")
        return(read.cm.s4(file, processingLog=processingLog, ...))
    if (type == "ctd/sbe/19")
        return(read.ctd.sbe(file, processingLog=processingLog, ...))
    if (type == "ctd/woce/exchange")      return(read.ctd.woce(file, processingLog=processingLog, ...))
    if (type == "coastline")
        return(read.coastline(file, type="mapgen", processingLog=processingLog, ...))
    if (type == "sealevel")
        return(read.sealevel(file, processingLog=processingLog, ...))
    if (type == "topo")
        return(read.topo(file, processingLog=processingLog, ...))
    if (type == "pt")
        return(read.pt(file, processingLog=processingLog, ...))
    if (type == "RBR/rsk")
        return(read.pt(file, processingLog=processingLog, type='rsk'))
    if (type == "section")
        return(read.section(file, processingLog=processingLog, ...))
    if (type == "odf/ctd")
        return(read.ctd.odf(file, processingLog=processingLog, ...))
    if (type == "odf/mvctd")
        return(read.ctd.odf(file, processingLog=processingLog, ...))
    stop("unknown file type \"", type, "\"")
}

oceColorsTwo <- function (n, low=2/3, high=0, smax=1, alpha = 1)
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

oceColorsJet <- function(n)
{
    if (missing(n) || n <= 0)
        colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                           "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
    else {
        colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                           "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))(n)
    }
}

oceColorsPalette <- function(n, which=1)
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
            rev(rgb(approx(i, rgb[,1], xout, rule=1)$y,
                    approx(i, rgb[,2], xout, rule=1)$y,
                    approx(i, rgb[,3], xout, rule=1)$y))
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
        } else stop("unknown which")
    }
    else character(0)
}

oce.axis.POSIXct <- function (side, x, at, format, labels = TRUE,
                              drawTimeRange=TRUE, abbreviateTimeRange=FALSE, drawFrequency=FALSE,
                              cex=par("cex"), cex.axis=par("cex.axis"), cex.main=par("cex.main"),
                              mar=par("mar"),
                              mgp=par("mgp"),
                              main="",
                              debug=getOption("oceDebug"), ...)
{
    oceDebug(debug, "\boce.axis.POSIXct(...,debug=", debug, ",...) {\n", sep="")
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
    if (d < 60 * 3) {                       # under 3 min
        t.start <- trunc(rr[1]-60, "mins")
        t.end <- trunc(rr[2]+60, "mins")
        z <- seq(t.start, t.end, by="10 sec")
        oceDebug(debug, "time range is under 3 minutes\n")
        oceDebug(debug, vectorShow(z, "z"))
        if (missing(format))
            format <- "%H:%M:%S"
    } else if (d < 60 * 30) {                  # under 30min
        t.start <- trunc(rr[1]-30, "mins")
        t.end <- trunc(rr[2]+30, "mins")
        z <- seq(t.start, t.end, by="min")
        oceDebug(debug, "time range is under 30 min\n")
        oceDebug(debug, vectorShow(z, "z"))
        if (missing(format))
            format <- "%H:%M"
    } else if (d < 60 * 60) {                  # under 1 hour
        t.start <- trunc(rr[1]-30, "mins")
        t.end <- trunc(rr[2]+30, "mins")
        z <- seq(t.start, t.end, by="10 min")
        oceDebug(debug, vectorShow(z, "Time range is under an hour; z="))
        if (missing(format))
            format <- "%H:%M"
    } else if (d < 60 * 60 * 2) {       # under 2 hours
        t.start <- trunc(rr[1]-30, "mins")
        t.end <- trunc(rr[2]+30, "mins")
        z <- seq(t.start, t.end, by="10 min")
        oceDebug(debug, vectorShow(z, "Time range is under 2 hours; z="))
        if (missing(format))
            format <- "%H:%M"
    } else if (d < 60 * 60 * 6) {       # under 6 hours, use HM
        t.start <- trunc(rr[1], "hour")
        t.end <- trunc(rr[2] + 3600, "hour")
        z <- seq(t.start, t.end, by="30 min")
        oceDebug(debug, vectorShow(z, "Time range is under 6 hours; z="))
        if (missing(format))
            format <- "%H:%M"
    } else if (d < 60 * 60 * 30) {       # under about a day
        t.start <- trunc(rr[1], "hour")
        t.end <- trunc(rr[2] + 3600, "hour")
        z <- seq(t.start, t.end, by="1 hour")
        oceDebug(debug, vectorShow(z, "Time range is under 30 hours, so z="))
        if (missing(format))
            format <- "%H"
    } else if (d <= 60 * 60 * 24 * 3) {        # under 3 days: label day; show 1-hour subticks
        t.start <- trunc(rr[1], "day")
        t.end <- trunc(rr[2] + 86400, "day")
        z <- seq(t.start, t.end, by="day")
        z.sub <- seq(t.start, t.end, by="hour")
        oceDebug(debug, vectorShow(z, "Time range is under 3 days; z="))
        oceDebug(debug, vectorShow(z.sub, "Time range is under 3 days; z.sub="))
        if (missing(format))
            format <- "%b %d"
    } else if (d <= 60 * 60 * 24 * 5) {        # under 5 days: label day; show 2-h subticks
        t.start <- trunc(rr[1], "day")
        t.end <- trunc(rr[2] + 86400, "day")
        z <- seq(t.start, t.end, by="day")
        z.sub <- seq(t.start, t.end, by="2 hour")
        oceDebug(debug, vectorShow(z, "Time range is under 5 days; z="))
        oceDebug(debug, vectorShow(z.sub, "Time range is under 5 days; z.sub="))
        if (missing(format))
            format <- "%b %d"
    } else if (d <= 60 * 60 * 24 * 14) { # under 2 weeks: label day; show 12-h subticks
        t.start <- trunc(rr[1], "day")
        t.end <- trunc(rr[2] + 86400, "day")
        z <- seq(t.start, t.end, by="day")
        z.sub <- seq(t.start, t.end, by="12 hour")
        oceDebug(debug, vectorShow(z, "Time range is under 2 weeks; z="))
        if (missing(format))
            format <- "%b %d"
    } else if (d <= 60 * 60 * 24 * 31) { # under 1 month: label day
        t.start <- trunc(rr[1], "day")
        t.end <- trunc(rr[2] + 86400, "day")
        z <- seq(t.start, t.end, by="day")
        oceDebug(debug, vectorShow(z, "Time range is under a month; z="))
        if (missing(format))
            format <- "%b %d"
    } else if (d < 60 * 60 * 24 * 31 * 2) {        # under 2 months
        t.start <- trunc(rr[1], "day")
        t.end <- trunc(rr[2] + 86400, "day")
        z <- seq(t.start, t.end, by="day")
        oceDebug(debug, vectorShow(z, "Time range is under 2 months; z="))
        if (missing(format))
            format <- "%b %d"
    } else if (d < 60 * 60 * 24 * 31 * 4) {        # under 4 months
        t.start <- trunc(rr[1], "days")
        t.end <- trunc(rr[2] + 86400, "days")
        z <- seq(t.start, t.end, by="week")
        oceDebug(debug, vectorShow(z, "Time range is under 4 months; z="))
        if (missing(format))
            format <- "%b %d"
    } else if (d < 1.1 * 60 * 60 * 24 * 365) { # under about a year
        rrl <- as.POSIXlt(rr)
        rrl[1]$mday <- 1
        rrl[2] <- rrl[2] + 31 * 86400
        rrl[2]$mday <- 1
        t.start <- trunc(rrl[1], "day")
        t.end <- trunc(rrl[2] + 86400, "day")
        z <- seq(t.start, t.end, by="month")
        oceDebug(debug, vectorShow(z, "Time range is under a year or so; z="))
        if (missing(format))
            format <- "%b %d"
    } else if (d < 3.1 * 60 * 60 * 24 * 365) { # under about 3 years
        rrl <- as.POSIXlt(rr)
        rrl[1]$mday <- 1
        rrl[2] <- rrl[2] + 31 * 86400
        rrl[2]$mday <- 1
        t.start <- trunc(rrl[1], "day")
        t.end <- trunc(rrl[2], "day")
        z <- seq(t.start, t.end, by="month")
        if (missing(format))
            format <- "%b %d"
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
        if (missing(format))
            format <- "%Y"
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
        labels <- format(z, format = format)
    else if (identical(labels, FALSE))
        labels <- rep("", length(z))
    if (drawTimeRange) {
        time.range <- par("usr")[1:2]   # axis, not data
        class(time.range) <- c("POSIXt", "POSIXct")
        attr(time.range, "tzone") <- attr(x, "tzone")[1]
        time.range <-  as.POSIXlt(time.range)
        time.range.data <- range(x, na.rm=TRUE)
        time.range[1] <- max(time.range[1], time.range.data[1])
        time.range[2] <- min(time.range[2], time.range.data[2])
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
    par(cex=cex, cex.axis=cex.axis, cex.main=cex.main, mgp=mgp, tcl=-0.5)
    ##axis(side, at=z, line=0, labels=labels, cex=cex, cex.axis=cex.axis, cex.main=cex.main, mar=mar, mgp=mgp)
    axis(side, at=z, line=0, labels=labels, mgp=mgp, cex=cex, cex.main=cex.main, cex.axis=cex.axis, ...)
    par(cex=ocex, cex.axis=ocex.axis, cex.main=cex.main, mgp=omgp)
    oceDebug(debug, "\b\b} # oce.axis.ts()\n")
    invisible()
}

oceBisect <- function(f, xleft, xright, dx, debug=getOption("oceDebug"))
{
    if (xleft >= xright)
        stop("xright must exceed xleft")
    if (f(xleft) * f(xright) > 0)
        stop("xleft and xright do not bracket a root")
    if (missing(dx))
        dx <- (xright - xleft) / 1e5
    else {
        if (dx == 0)
            stop('cannot have dx == 0')
    if (dx < 0)
        stop('cannot have dx < 0')
    }
    xmiddle <- 0.5 * (xleft + xright)
    npass <- 4 + floor(1 + log2((xright - xleft) / dx))
    oceDebug(debug, "npass=", npass, "\n")
    for (pass in 1:npass) {
        if (f(xleft) * f(xmiddle) < 0) { # FIXME: should not need to do f(left)
            tmp <- xmiddle
            xmiddle <- 0.5 * (xleft + xmiddle)
            xright <- tmp
            oceDebug(debug, xleft, "|", xmiddle, "|", xright, "<\n")
        } else {
            tmp <- xmiddle
            xmiddle <- 0.5 * (xmiddle + xright)
            xleft <- tmp
            oceDebug(debug, ">", xleft, "|", xmiddle, "|", xright, "\n")
        }
        if ((xright - xleft) < dx){
            oceDebug(debug, "got root in pass", pass, "\n")
            break
        }
    }
    xmiddle
}

numberAsPOSIXct <- function(t, type=c("unix", "matlab", "gps", "argos"), tz="UTC")
{
    type <- match.arg(type)
    if (type == "unix") {
        tref <- as.POSIXct("2000-01-01", tz=tz) # arbitrary
        return((as.numeric(t) - as.numeric(tref)) + tref)
    }
    if (type == "matlab") {
        ## R won't take a day "0", so subtract one
        return(as.POSIXct(ISOdatetime(0000, 01, 01, 0, 0, 0, tz=tz) + 86400 * (t - 1)))
    }
    if (type == "argos") {
        return(t * 86400 + as.POSIXct("1900-01-01", tz="UTC"))
    }
    if (type == "gps") {
        if (!is.matrix(t) || dim(t)[2] != 2)
            stop("for GPS times, 't' must be a two-column matrix, with first col the week, second the second")
        ## Account for leap seconds since the GPS start time in 1980 (for the present week wraparound grouping).
        ## http://en.wikipedia.org/wiki/Leap_second
        leaps <- as.POSIXct(strptime(c("1981-07-01", "1982-07-01", "1983-07-01", "1985-07-01", "1987-01-01",
                                       "1989-01-01", "1990-01-01", "1992-07-01", "1993-07-01", "1994-07-01",
                                       "1995-01-01", "1997-07-01", "1998-01-01", "2005-01-01", "2008-01-01"),
                                     format="%Y-%m-%d", tz="UTC"))
        t <- as.POSIXct("1999-08-22 00:00:00",tz="UTC") + 86400*7*t[,1] + t[,2]
        for (l in 1:length(leaps)) {
            t <- t - ifelse(t >= leaps[l], 1, 0)
        }
        t
    } else {
        stop("type must be \"unix\", \"matlab\" or \"GPS\"")
    }
}

