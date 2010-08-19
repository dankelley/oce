use.heading <- function(b, g, add=0)
{
    if (!"heading" %in% names(b$data$ts))
        stop("'from' does not have any heading data (in b$data$ts$heading)")
    if (!"time" %in% names(b$data$ts))
        stop("'b' does not have any time data (in b$data$ts$time)")
    if (!"heading" %in% names(g$data$ts))
        stop("'g' does not have any heading data (in g$data$ts$heading)")
    if (!"time" %in% names(g$data$ts))
        stop("'g' does not have any time data (in g$data$ts$time)")
    rval <- b
    t0 <- as.numeric(g$data$ts$time[1])
    if (is.na(t0))
        stop("need first element of from$data$ts$time to be non-NA")
    b.t <- as.numeric(b$data$ts$time) - t0
    g.t <- as.numeric(g$data$ts$time) - t0
    rval$data$ts$heading <- approx(x=g.t, y=g$data$ts$heading, xout=b.t)$y + add * pi / 180
    processing.log.append(rval, paste(deparse(match.call()), sep="", collapse=""))
    rval
}

window.oce <- function(x, start = NULL, end = NULL, frequency = NULL, deltat = NULL, extend = FALSE, which=c("time","distance"), ...)
{
    if (extend) stop("cannot handle extend=TRUE yet")
    if (!is.null(frequency)) stop("cannot handle frequency yet")
    if (!is.null(deltat)) stop("cannot handle deltat yet")
    if (is.null(start)) stop("must provide start")
    if (is.null(end)) stop("must provide end")
    res <- x
    which <- match.arg(which)
    if (which == "time") {
        if (!("ts" %in% names(x$data))) {
            warning("oce object has no $data$ts vector, so window is returning it unaltered")
            return(x)
        }
        if (!("time" %in% names(x$data$ts))) {
            warning("oce object has no $data$ts$time vector, so window is returning it unaltered")
            return(x)
        }
        if (is.character(start))
            start <- as.POSIXct(start, tz=getOption("oce.tz"))
        if (is.character(end))
            end <- as.POSIXct(end, tz=getOption("oce.tz"))
        keep <- start <= res$data$ts$time & res$data$ts$time < end
        for (tsname in names(res$data$ts)) {
            res$data$ts[[tsname]] <- res$data$ts[[tsname]][keep]
        }
        if ("ma" %in% names(res$data)) {
            for (maname in names(res$data$ma)) {
                ldim <- length(dim(res$data$ma[[maname]]))
                if (ldim == 2)
                    res$data$ma[[maname]] <- res$data$ma[[maname]][keep,]
                else if (ldim == 3)
                    res$data$ma[[maname]] <- res$data$ma[[maname]][keep,,]
                else
                    stop("cannot handle data$ma item of dimension ", ldim)
            }
        }
    } else if (which == "distance") {
        if (!("ss" %in% names(x$data))) {
            warning("oce object has no $data$ss vector, so window is returning it unaltered")
            return(x)
        }
        if (!("distance" %in% names(x$data$ss))) {
            warning("oce object has no $data$s$distance vector, so window is returning it unaltered")
            return(x)
        }
        keep <- start <= res$data$ss$distance & res$data$ss$distance < end
        ## FIXME: make it work on sections, on CTD, etc.
        if (!inherits(x, "adp")) {
            warning("window(..., which=\"distance\") only works for objects of class adp")
            return(x)
        }
        res$data$ss$distance <- x$data$ss$distance[keep]
        if ("ma" %in% names(res$data)) {
            for (maname in names(res$data$ma)) {
                res$data$ma[[maname]] <- res$data$ma[[maname]][,keep,]
            }
        }
    } else {
        stop("unknown value of which \"", which, "\"") # cannot get here
    }
    res
}

oce.approx <- function(x, y, xout, method=c("reiniger-ross"))
{
    method <- match.arg(method)
    if (method != "reiniger-ross") stop("only available method is \"reiniger-ross\"")
    if (missing(x)) stop("must supply x")
    if (missing(y)) stop("must supply y")
    lx <- length(x)
    ly <- length(y)
    if (lx != ly) stop("length of x (", lx, ") and y (", ly, ") must agree")
    if (any(is.na(x))) stop("must not have any NA values in x")
    if (any(is.na(y))) stop("must not have any NA values in y")
    o <- order(x)
    if (missing(xout))
        xout <- seq(min(x), max(x), length.out=lx)
    else
        if (any(is.na(xout))) stop("must not have any NA values in xout")
    .Call("oce_approx", x=x[o], y=y[o], xout=xout)
}

oce.plot.sticks <- function(x, y, u, v, yscale=1, add=FALSE, length=1/20,
                            mgp=getOption("oce.mgp"),
                            mar=c(mgp[1]+1,mgp[1]+1,1,1+par("cex")),
                            ...)
{
    pin <- par("pin")
    page.ratio <- pin[2]/pin[1]
    if (missing(x)) stop("must supply x")
    if (missing(y)) stop("must supply y")
    if (missing(u)) stop("must supply u")
    if (missing(v)) stop("must supply v")
    n <- length(x)
    if (length(y) != n) stop("lengths of x and y must match, but they are ", n, " and ", length(y))
    if (length(u) != n) stop("lengths of x and u must match, but they are ", n, " and ", length(u))
    if (length(v) != n) stop("lenghts of x and v must match, but they are ", n, " and ", length(v))
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


oce.plot.ts <- function(x,
                        y,
                        draw.time.range=TRUE,
                        xaxs="r",       # was "i"
                        grid=TRUE,
                        adorn=NULL,
                        fill=FALSE,
                        xlab="",
                        ylab="",
                        cex=par("cex"), cex.axis=par("cex.axis"), cex.main=par("cex.main"),
                        mgp=getOption("oce.mgp"),
                        mar=c(mgp[1]+if(nchar(xlab)>0) 1 else 0.5, mgp[1]+if(nchar(ylab)>0) 1.5 else 1, mgp[2]+1, mgp[2]+3/4),
                        type="l",

                        main="",
                        debug=getOption("oce.debug"),
                        ...)
{
    oce.debug(debug, "\b\boce.plot.ts() {\n")
    oce.debug(debug, "cex=",cex," cex.axis=", cex.axis, " cex.main=", cex.main, "\n")
    oce.debug(debug, "mar=c(",paste(mar, collapse=","), ")\n")
    par(mgp=mgp, mar=mar, cex=cex)
    args <- list(...)
    if (fill) {
        xx <- c(x[1], x, x[length(x)])
        yy <- c(0, y, 0)
        plot(x, y, axes=FALSE, xaxs=xaxs, xlab=xlab, ylab=ylab,
             cex=cex, cex.lab=cex.axis, cex.axis=cex.axis, cex.main=cex.main,
             type=type, ...)
        fillcol <- if ("col" %in% names(args)) args$col else "lightgray" # FIXME: should be a formal argument
        do.call(polygon, list(x=xx, y=yy, col=fillcol))
    } else {
        plot(x, y, axes=FALSE, xaxs=xaxs, xlab=xlab, ylab=ylab,
             cex=cex, cex.lab=cex.axis, cex.axis=cex.axis, cex.main=cex.main,
             type=type, ...)
    }
    xlabs <- oce.axis.POSIXct(1, x=x, draw.time.range=draw.time.range, main=main, debug=debug-1, cex=cex, cex.axis=cex.axis, cex.main=cex.main, ...)
    if (grid) {
        lwd <- par("lwd")
        abline(v=xlabs, col="lightgray", lty="dotted", lwd=lwd)
        yaxp <- par("yaxp")
        abline(h=seq(yaxp[1], yaxp[2], length.out=1+yaxp[3]),
               col="lightgray", lty="dotted", lwd=lwd)
    }
    box()
    axis(2, cex.axis=cex.axis)
    axis(4, labels=FALSE, ...)
    if (!is.null(adorn)) {
        t <- try(eval(adorn, enclos=parent.frame()), silent=TRUE)
        if (class(t) == "try-error") warning("cannot evaluate adorn {", adorn, "}\n")
    }
    oce.debug(debug, "\b\b}\n")
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
    if (!inherits(x, "POSIXct")) stop(gettextf("do not know how to convert '%s' to class \"POSIXlt\"", deparse(substitute(x))))
    if (missing(tz) && !is.null(tzone))
        tz <- tzone[1]
    .Internal(as.POSIXlt(x, tz))
}

oce.edit <- function(x, item, value, action, reason="not specified", person="not specified")
{
    if (!inherits(x, "oce")) stop("method is only for oce objects")
    if (!missing(item)) {
        if (missing(value)) stop("must supply a 'value' for this 'item'")
        if (!(item %in% names(x$metadata))) stop("no item named '", item, "' in object's  metadata")
        x$metadata[item] <- value
    } else if (!missing(action)) {
        eval(parse(text=action))        # FIXME: should check if it worked
    } else {
        stop("must supply either an 'item' plus a 'value', or an 'action'")
    }
    processing.log.append(x, paste(deparse(match.call()), sep="", collapse=""))
}

oce.write.table <- function (x, file="", ...)
{
    if (!inherits(x, "oce")) stop("method is only for oce objects")
    if (!("row.names" %in% names(list(...)))) write.table(x$data, file, row.names=FALSE, ...)
    else write.table(x$data, file, ...)
}

subset.oce <- function (x, subset, indices=NULL, debug=getOption("oce.debug"), ...)
{
    if (!inherits(x, "oce")) stop("method is only for oce objects")
    if (inherits(x, "adp")) { # FIXME: should be able to select by time or space, maybe others
        if (!is.null(indices)) {
            rval <- x
            keep <- (1:x$metadata$number.of.profiles)[indices]
            print(keep)
            stop("this version of oce cannot subset adp data by index")
        } else if (!missing(subset)) {
            subset.string <- deparse(substitute(subset))
            oce.debug(debug, "subset.string='", subset.string, "'\n")
            if (length(grep("time", subset.string))) {
                oce.debug(debug, "subsetting an adp by time\n")
                ##stop("cannot understand the subset; it should be e.g. 'time < as.POSIXct(\"2008-06-26 12:00:00\", tz = \"UTC\")'")
                keep <- eval(substitute(subset), x$data$ts, parent.frame())
                oce.debug(debug, vector.show(keep, "keeping bins:"), "\n")
                if (sum(keep) < 2) stop("must keep at least 2 profiles")
                rval <- x
                for (name in names(x$data$ts)) {
                    rval$data$ts[[name]] <- x$data$ts[[name]][keep]
                }
                for (name in names(x$data$ma)) {
                    rval$data$ma[[name]] <- x$data$ma[[name]][keep,,]
                }
            } else if (length(grep("distance", subset.string))) {
                oce.debug(debug, "subsetting an adp by distance\n")
                keep <- eval(substitute(subset), x$data$ss, parent.frame())
                oce.debug(debug, vector.show(keep, "keeping bins:"), "\n")
                if (sum(keep) < 2) stop("must keep at least 2 bins")
                rval <- x
                for (name in names(x$data$ss)) {
                    rval$data$ss[[name]] <- x$data$ss[[name]][keep]
                }
                for (name in names(x$data$ma)) {
                    rval$data$ma[[name]] <- x$data$ma[[name]][,keep,]
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
                stn[i] <- x$metadata$station.id[ii]
                lat[i] <- x$metadata$latitude[ii]
                lon[i] <- x$metadata$longitude[ii]
                station[[i]] <- x$data$station[[ii]]
            }
            data <- list(station=station)
            metadata <- list(header=x$header,section.id=x$section.id,station.id=stn,latitude=lat,longitude=lon)
            rval <- list(data=data, metadata=metadata, processing.log=x$processing.log)
            class(rval) <- c("section", "oce")
        } else {                        # subset within the stations
            rval <- x
            n <- length(x$data$station)
            r <- eval(substitute(subset), x$data$station[[1]]$data, parent.frame())
            for (i in 1:n) {
                rval$data$station[[i]]$data <- x$data$station[[i]]$data[r,]
            }
        }
        rval <- processing.log.append(rval, paste(deparse(match.call()), sep="", collapse=""))
    } else if (inherits(x, "pt")) {
        r <- eval(substitute(subset), x$data$ts, parent.frame())
        r <- r & !is.na(r)
        rval <- x
        for (name in names(rval$data$ts))
            rval$data$ts[[name]] <- x$data$ts[[name]][r]
        rval <- processing.log.append(rval, paste(deparse(match.call()), sep="", collapse=""))
    } else if (inherits(x, "adv")) {
        if (!is.null(indices))
            stop("cannot specify 'indices' for adv objects (not coded yet)")
        if (missing(subset))
            stop("must specify a 'subset'")
        subset.string <- deparse(substitute(subset))
        oce.debug(debug, "subset.string='", subset.string, "'\n")
        if (length(grep("time", subset.string))) {
            oce.debug(debug, "subsetting an adp by time\n")
            keep <- eval(substitute(subset), x$data$ts, parent.frame())
            oce.debug(debug, vector.show(keep, "keeping bins:"), "\n")
            if (sum(keep) < 2)
                stop("must keep at least 2 profiles")
            rval <- x
            for (name in names(x$data$ts)) {
                rval$data$ts[[name]] <- x$data$ts[[name]][keep]
            }
            for (name in names(x$data$ma)) {
                rval$data$ma[[name]] <- x$data$ma[[name]][keep,]
            }
        } else {
            stop("only 'time' is permitted for subsetting")
        }
    } else {
        r <- eval(substitute(subset), x$data, parent.frame())
        r <- r & !is.na(r)
        rval <- x
        rval$data <- x$data[r,]
        rval <- processing.log.append(rval, paste(deparse(match.call()), sep="", collapse=""))
    }
    rval
}

summary.oce <- function(object, ...)
{
    if (!inherits(object, "oce")) stop("method is only for oce objects")
    cat("Data summary:\n")
    print(summary(object$data))
    cat("\nMetadata:\n")
    print(object$metadata)
    processing.log.summary(object)
    return(invisible(object))
}

magic <- function(file, debug=getOption("oce.debug"))
{
    filename <- file
    if (is.character(file)) {
        oce.debug(debug, "checking filename to see if it ends in .adr ...\n")
        if (length(grep(".adr$", file))) {
            oce.debug(debug, "yes, so this is adv/sontek/adr.\n")
            return("adv/sontek/adr")
        }
        oce.debug(debug, " no, so not adv/sontek/adr.\n")
        file <- file(file, "r")
    }
    if (!inherits(file, "connection")) stop("argument `file' must be a character string or connection")
    if (!isOpen(file))
    	open(file, "r")
    ## grab a single line of text, then some raw bytes (the latter may be followed by yet more bytes)
    line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
    oce.debug(debug, paste("magic(file=\"", filename, "\", debug=",debug,") found first line of file to be as follows:\n", line, "\n", sep=""))
    close(file)
    file <- file(filename, "rb")
    bytes <- readBin(file, what="raw", n=4)
    oce.debug(debug, paste("magic(file=\"", filename, "\", debug=",debug,") found two bytes in file: 0x", bytes[1], " and 0x", bytes[2], "\n", sep=""))
    on.exit(close(file))
    if (bytes[1] == 0x00 && bytes[2] == 0x00 && bytes[3] == 0x27 && bytes[4] == 0x0a) {
        oce.debug(debug, "this is a shapefile; see e.g. http://en.wikipedia.org/wiki/Shapefile\n")
        return("shapefile")
    }

    if (bytes[1] == 0x10 && bytes[2] == 0x02) {
        ## 'ADPManual v710.pdf' p83
        if (96 == readBin(bytes[3:4], "integer", n=1, size=2,endian="little"))
            oce.debug(debug, "this is adp/sontek (4 byte match)\n")
        else
            oce.debug(debug, "this is adp/sontek (2 byte match, but bytes 3 and 4 should become integer 96)\n")
        return("adp/sontek")
    }
    if (bytes[1] == 0x7f && bytes[2] == 0x7f) {
        oce.debug(debug, "this is adp/rdi\n")
        return("adp/rdi")
    }
    if (bytes[1] == 0xa5 && bytes[2] == 0x05) {
        ## NorTek files require deeper inspection.  Here, SIG stands for "System Integrator Guide",
        ## Dated Jue 2008 (Nortek Doc No PS100-0101-0608)
        seek(file, 0)
        oce.debug(debug, "This is probably a nortek file of some sort.  Reading further to see for sure ...\n")
        hardware.configuration <- readBin(file, what="raw", n=48) # FIXME: this hard-wiring is repeated elsewhere
        if (hardware.configuration[1] != 0xa5 || hardware.configuration[2] != 0x05) return("unknown")
        oce.debug(debug, "hardware.configuration[1:2]", hardware.configuration[1:2], "(expect 0xa5 0x05)\n")
        head.configuration <- readBin(file, what="raw", n=224)
        oce.debug(debug, "head.configuration[1:2]", head.configuration[1:2], "(expect 0xa5 0x04)\n")
        if (head.configuration[1] != 0xa5 || head.configuration[2] != 0x04) return("unknown")
        user.configuration <- readBin(file, what="raw", n=512)
        oce.debug(debug, "user.configuration[1:2]", user.configuration[1:2], "(expect 0xa5 0x00)\n")
        if (user.configuration[1] != 0xa5 || user.configuration[2] != 0x00) return("unknown")
        next.two.bytes <- readBin(file, what="raw", n=2)
        oce.debug(debug, "next.two.bytes:", next.two.bytes,"(e.g. 0x5 0x12 is adv/nortek/vector)\n")
        if (next.two.bytes[1] == 0xa5 && next.two.bytes[2] == 0x12) {
            oce.debug(debug, "this is adv/nortek/vector\n")
            return("adv/nortek/vector")
        }
        if (next.two.bytes[1] == 0xa5 && next.two.bytes[2] == 0x01) {
            oce.debug(debug, "this is adp/nortek/aqudopp\n")
            return("adp/nortek/aquadopp") # p33 SIG
        }
        if (next.two.bytes[1] == 0xa5 && next.two.bytes[2] == 0x2a)  {
            oce.debug(debug, "this is adp/nortek/aqudoppHR\n")
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
        oce.debug(debug, "this is ctd/woce/exchange\n")
        return("ctd/woce/exchange")
    }
    if ("* Sea-Bird" == substr(line, 1, 10))  {
        oce.debug(debug, "this is ctd/sbe/19\n")
        return("ctd/sbe/19")
    }
    if ("# -b" == substr(line, 1, 4)) {
        oce.debug(debug, "this is coastline\n")
        return("coastline")
    }
    if ("# Station_Name," == substr(line, 1, 15)) {
        oce.debug(debug, "this is sealevel\n")
        return("sealevel")
    }
    if ("Station_Name," == substr(line, 1, 13)) {
        oce.debug(debug, "this is sealevel\n")
        return("sealevel")
    }
    if (0 < regexpr("^[0-9][0-9][0-9][A-Z] ", line)) {
        oce.debug(debug, "this is sealevel\n")
        return("sealevel")
    }
    if (0 < regexpr("^NCOLS[ ]*[0-9]*[ ]*$", line)) {
        oce.debug(debug, "this is topo\n")
        return("topo")
    }
    if ("RBR TDR" == substr(line, 1, 7))  {
        oce.debug(debug, "this is pt\n")
        return("pt")
    }
    if ("BOTTLE"  == substr(line, 1, 6))  {
        oce.debug(debug, "this is section\n")
        return("section")
    }
    oce.debug(debug, "this is unknown\n")
    return("unknown")
}

read.oce <- function(file, ...)
{
    type <- magic(file)
    log.action <- paste(deparse(match.call()), sep="", collapse="")
    if (type == "shapefile")              stop("cannot read shapefiles")
    if (type == "adp/rdi")                return(read.adp.rdi(file, log.action=log.action, ...))
    if (type == "adp/sontek")             return(read.adp.sontek(file, log.action=log.action, ...)) # FIXME is pcadcp different?
    if (type == "adp/nortek/aquadopp")    stop("cannot read adp/nortek/aquadopp files (aquadoppHR is OK, though)")
    if (type == "adp/nortek/aquadoppHR")  return(read.adp.nortek(file, log.action=log.action, ...))
    if (type == "adv/nortek/vector")      return(read.adv.nortek(file, log.action=log.action, ...))
    if (type == "adv/sontek/adr")         return(read.adv.sontek.adr(file, log.action=log.action, ...))
    ## FIXME need adv/sontek (non adr)
    if (type == "ctd/sbe/19")             return(read.ctd.sbe(file, log.action=log.action, ...))
    if (type == "ctd/woce/exchange")      return(read.ctd.woce(file, log.action=log.action, ...))
    if (type == "coastline")              return(read.coastline(file, type="mapgen", log.action=log.action, ...))
    if (type == "sealevel")               return(read.sealevel(file, log.action=log.action, ...))
    if (type == "topo")                   return(read.topo(file, log.action=log.action, ...))
    if (type == "pt")                     return(read.pt(file, log.action=log.action, ...))
    if (type == "section")                return(read.section(file, log.action=log.action, ...))
    stop("unknown file type \"", type, "\"")
}

oce.colors.two <- function (n, low=2/3, high=0, smax=1, alpha = 1)
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

oce.colors.jet <- function(n)
{
    if ((n <- as.integer(n[1])) > 0) {
                                        # matlab::jet, cleaned of matlab:: calls
        n4 <- ceiling(n / 4)
        u <- c(seq(1, n4) / n4,
               if (n4 > 1) rep(1, n4-1) else NULL,
               seq(n4, 1, by = -1) / n4)
        g <- ceiling(n4 / 2) - (n%%4 == 1) + (1:length(u))
        r <- g + n4
        b <- g - n4
        g <- g[g <= n]
        r <- r[r <= n]
        b <- b[b >= 1]
        J <- matrix(0, nrow=n, ncol=3)
        J[r, 1] <- u[seq(along = r)]
        J[g, 2] <- u[seq(along = g)]
        J[b, 3] <- u[seq(length(u)-length(b)+1, length(u))]
        rgb(J)
    }
    else character(0)
}

oce.colors.palette <- function(n, which=1)
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

oce.axis.POSIXct <- function (side, x, at, format, labels = TRUE, draw.time.range=TRUE, abbreviate.time.range=FALSE, cex=par("cex"), cex.axis=par("cex.axis"), cex.main=par("cex.main"), main="", debug=getOption("oce.debug"), ...)
{
    oce.debug(debug, "\b\boce.axis.POSIXct() {\n")
    oce.debug(debug,"cex=",cex," cex.axis=", cex.axis, " cex.main=", cex.main, "\n")
    oce.debug(debug,vector.show(x, "x"))
    ## This was written because axis.POSIXt in R version 2.8.x did not obey the
    ## time zone in the data.  (Version 2.9.0 obeys the time zone.)
    if (missing(x))
        stop("must supply x")
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
    if (FALSE) {                         # will remove after testing
        if (d < 1.1 * 60) {              # under about a minute
            sc <- 1
            if (missing(format))
                format <- "%S"
        } else if (d < 1.1 * 60 * 60) { # under about an hour
            sc <- 60
            if (missing(format))
                format <- "%M:%S"
        } else if (d < 1.1 * 60 * 60 * 24) { # under about a day
            sc <- 60 * 60
            if (missing(format))
                format <- "%H:%M"
        } else if (d < 2 * 60 * 60 * 24) { # under 2 days
            sc <- 60 * 60
            if (missing(format))
                format <- "%a %H:%M"
        } else if (d < 7 * 60 * 60 * 24) { # under a week
            sc <- 60 * 60 * 24
            if (missing(format))
                format <- "%a"
        } else if (d < 3 * 7 * 60 * 60 * 24) { # under 3 weeks
            sc <- 60 * 60 * 24
            if (missing(format))
                format <- "%b %d"
        } else if (d < 32 * 60 * 60 * 24) { # under  a month
            sc <- 60 * 60 * 24 * 7
            if (missing(format))
                format <- "%a"
        } else {
            sc <- 60 * 60 * 24
            if (missing(format))
                format <- "%a"
        }
    }

    rr <- range + as.POSIXct("2000-01-20") - as.numeric(as.POSIXct("2000-01-20"))
    attr(rr, "tzone") <- attr(x, "tzone")
    oce.debug(debug, "range=",
              format.POSIXct(rr[1], "%Y-%m-%d %H:%M:%S", tz="UTC"),
              "UTC to ",
              format.POSIXct(rr[2], "%Y-%m-%d %H:%M:%S", tz="UTC"),
              "UTC\n")

    if (d < 60) {                       # under 1 min
        t.start <- trunc(rr[1]-30, "mins")
        t.end <- trunc(rr[2]+30, "mins")
        z <- seq(t.start, t.end, by="10 sec")
        oce.debug(debug, "time range is under a minute\n")
        oce.debug(debug, vector.show(z, "z"))
        if (missing(format)) format <- "%H:%M:%S"
    } else if (d < 60 * 30) {                  # under 30min
        t.start <- trunc(rr[1]-30, "mins")
        t.end <- trunc(rr[2]+30, "mins")
        z <- seq(t.start, t.end, by="min")
        oce.debug(debug, "time range is under 30 min\n")
        oce.debug(debug, vector.show(z, "z"))
        if (missing(format)) format <- "%H:%M"
    } else if (d < 60 * 60) {                  # under 1 hour
        t.start <- trunc(rr[1]-30, "mins")
        t.end <- trunc(rr[2]+30, "mins")
        z <- seq(t.start, t.end, by="10 min")
        oce.debug(debug, vector.show(z, "Time range is under an hour; z="))
        if (missing(format)) format <- "%H:%M"
    } else if (d < 60 * 60 * 2) {       # under 2 hours
        t.start <- trunc(rr[1]-30, "mins")
        t.end <- trunc(rr[2]+30, "mins")
        z <- seq(t.start, t.end, by="10 min")
        oce.debug(debug, vector.show(z, "Time range is under 2 hours; z="))
        if (missing(format)) format <- "%H:%M"
    } else if (d < 60 * 60 * 6) {       # under 6 hours, use HM
        t.start <- trunc(rr[1], "hour")
        t.end <- trunc(rr[2] + 3600, "hour")
        z <- seq(t.start, t.end, by="30 min")
        oce.debug(debug, vector.show(z, "Time range is under 6 hours; z="))
        if (missing(format)) format <- "%H:%M"
    } else if (d < 60 * 60 * 24) {        # under a day
        t.start <- trunc(rr[1], "hour")
        t.end <- trunc(rr[2] + 3600, "hour")
        z <- seq(t.start, t.end, by="hour")
        oce.debug(debug, vector.show(z, "Time range is under a day; z="))
        if (missing(format)) format <- "%H:%M"
    } else if (d < 60 * 60 * 24 * 2) {        # under 2 days
        t.start <- trunc(rr[1], "hour")
        t.end <- trunc(rr[2] + 86400, "hour")
        z <- seq(t.start, t.end, by="hour")
        oce.debug(debug, vector.show(z, "Time range is under 2 days; z="))
        if (missing(format)) format <- "%H"
    } else if (d < 60 * 60 * 24 * 32) {        # under 2 weeks
        t.start <- trunc(rr[1], "day")
        t.end <- trunc(rr[2] + 86400, "day")
        z <- seq(t.start, t.end, by="day")
        oce.debug(debug, vector.show(z, "Time range is under 2 weeks; z="))
        if (missing(format)) format <- "%b %d"
    } else if (d < 1.1 * 60 * 60 * 24 * 365) { # under about a year
        t.start <- trunc(rr[1], "day")
        t.end <- trunc(rr[2] + 86400, "day")
        z <- seq(t.start, t.end, by="month")
        oce.debug(debug, vector.show(z, "Time range is under a year or so; z="))
        if (missing(format)) format <- "%b %d"
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
        if (missing(format)) format <- "%Y"
        oce.debug(debug, vector.show(z, "z="))
    }
    if (!mat)
        z <- x[is.finite(x)]

    ##
    ## FIXME: I was twiddling the numbers, to get more labels, but xaxs="r" fixes that.
    twiddle <- 0*diff(as.numeric(range)) / 10 # FIXME: do I need this anymore?
    ##oce.debug(debug, "range:", format(range[1]), "to", format(range[2]), "\n")
    keep <- range[1] <= (z + twiddle) & (z - twiddle) <= range[2]
    ##oce.debug(debug, vector.show(keep, "keep"))
    ##oce.debug(debug, vector.show(z, "z before keep"))
    z <- z[keep]
    ##oce.debug(debug, vector.show(z, "z after keep"))
    if (!is.logical(labels))
        labels <- labels[keep]
    else if (identical(labels, TRUE))
        labels <- format(z, format = format)
    else if (identical(labels, FALSE))
        labels <- rep("", length(z))
    if (draw.time.range) {
        time.range <- par("usr")[1:2]   # axis, not data
        class(time.range) <- c("POSIXt", "POSIXct")
        attr(time.range, "tzone") <- attr(x, "tzone")[1]
        time.range <-  as.POSIXlt(time.range)
        time.range.data <- range(x, na.rm=TRUE)
        time.range[1] <- max(time.range[1], time.range.data[1])
        time.range[2] <- min(time.range[2], time.range.data[2])
        tr1 <- format(time.range[1])
        tr2 <- format(time.range[2])
        if (abbreviate.time.range) {
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
        label <- paste(tr1, attr(time.range[1], "tzone")[1], "to", tr2,  attr(time.range[2], "tzone")[1], "@", sprintf("%.4g Hz", 1/deltat), sep=" ")
        oce.debug(debug, "label=", label, "\n")
        mtext(label, side=if (side==1) 3 else 1, cex=0.7*cex.axis*par('cex'), adj=0)
        oce.debug(debug, "cex.axis=", cex.axis, "; par('cex')=", par('cex'), "\n")
    }
    if (nchar(main) > 0) {
        mtext(main, side=if(side==1) 3 else 1, cex=cex.axis*par('cex'), adj=1)
    }
    axis(side, at = z, line=0, labels = labels, cex=cex, cex.axis=cex.axis, cex.main=cex.main, ...)
    oce.debug(debug, "\b\b}\n")
}
