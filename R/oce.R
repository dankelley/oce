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
    .Call("oce_approx", x=x[o], y=y[o], xout=xout);
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
                        xaxs="i",
                        grid=TRUE,
                        adorn=NULL,
                        fill=FALSE,
                        ...)
{
    args <- list(...)
    have.ylab <- "ylab" %in% names(args)
    if (fill) {
        xx <- c(x[1], x, x[length(x)])
        yy <- c(0, y, 0)
        plot(x, y, axes=FALSE, xaxs=xaxs, ...)
        fillcol <- if ("col" %in% names(args)) args$col else "lightgray"
        if (have.ylab)
            do.call(polygon, list(x=xx, y=yy, col=fillcol, ylab=args$ylab))
        else
            do.call(polygon, list(x=xx, y=yy, col=fillcol))
    } else {
        if (have.ylab)
            plot(x, y, axes=FALSE, xaxs=xaxs, ...)
        else
            plot(x, y, axes=FALSE, xaxs=xaxs, ylab=paste(deparse(substitute(y))), ...)
    }
    xlabs <- oce.axis.POSIXct(1, x=x, draw.time.range=draw.time.range)
    if (grid) {
        lwd <- par("lwd")
        abline(v=xlabs, col="lightgray", lty="dotted", lwd=lwd)
        yaxp <- par("yaxp")
        abline(h=seq(yaxp[1], yaxp[2], length.out=1+yaxp[3]),
               col="lightgray", lty="dotted", lwd=lwd)
    }
    box()
    axis(2)
    axis(4, labels=FALSE)
    if (!is.null(adorn)) {
        t <- try(eval(adorn, enclos=parent.frame()), silent=TRUE)
        if (class(t) == "try-error") warning("cannot evaluate adorn {", adorn, "}\n")
    }
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

subset.oce <- function (x, subset, indices=NULL, ...)
{
    debug <- !TRUE
    if (!inherits(x, "oce")) stop("method is only for oce objects")
    if (inherits(x, "adp")) { # FIXME: should be able to select by time or space, maybe others
        if (!is.null(indices)) {
            rval <- x
            keep <- (1:x$metadata$number.of.profiles)[indices]
            print(keep)
            stop("this version of oce cannot subset adp data by index")
        } else if (!missing(subset)) {
            subset.string <- deparse(substitute(subset))
            if (length(grep("time", subset.string))) {
                ##stop("cannot understand the subset; it should be e.g. 'time < as.POSIXct(\"2008-06-26 12:00:00\", tz = \"UTC\")'")
                keep <- eval(substitute(subset), x$data$ts, parent.frame())
                if (sum(keep) < 2) stop("must keep at least 2 profiles")
                if (debug) {
                    cat("keeping profiles:\n")
                    print(keep)
                }
                rval <- x
                for (name in names(x$data$ts)) {
                    rval$data$ts[[name]] <- x$data$ts[[name]][keep]
                }
                for (name in names(x$data$ma)) {
                    rval$data$ma[[name]] <- x$data$ma[[name]][keep,,]
                }
            } else if (length(grep("distance", subset.string))) {
                keep <- eval(substitute(subset), x$data$ss, parent.frame())
                if (sum(keep) < 2) stop("must keep at least 2 bins")
                if (debug) {
                    cat("keeping bins:\n")
                    print(keep)
                }
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
        if (debug) cat("checking filename to see if it ends in .adr ...")
        if (length(grep(".adr$", file))) {
            if (debug) cat(" yes, so this is adv/sontek/adr.\n")
            return("adv/sontek/adr")
        }
        if (debug) cat(" no, so not adv/sontek/adr.\n")
        file <- file(file, "r")
    }
    if (!inherits(file, "connection")) stop("argument `file' must be a character string or connection")
    if (!isOpen(file))
    	open(file, "r")
    ## grab a single line of text, then some raw bytes (the latter may be followed by yet more bytes)
    line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
    if (debug > 0) cat(paste("magic(file=\"", filename, "\", debug=",debug,") found first line of file to be as follows:\n", line, "\n", sep=""))
    close(file)
    file <- file(filename, "rb")
    bytes <- readBin(file, what="raw", n=2)
    if (debug > 0) cat(paste("magic(file=\"", filename, "\", debug=",debug,") found two bytes in file: 0x", bytes[1], " and 0x", bytes[2], "\n", sep=""))
    on.exit(close(file))
    if (bytes[1] == 0x7f && bytes[2] == 0x7f) {
        if (debug) cat("this is adp/rdi\n")
        return("adp/rdi")
    }
    if (bytes[1] == 0xa5 && bytes[2] == 0x05) {
        ## NorTek files require deeper inspection.  Here, SIG stands for "System Integrator Guide",
        ## Dated Jue 2008 (Nortek Doc No PS100-0101-0608)
        seek(file, 0)
        if (debug) cat("This is probably a nortek file of some sort.  Reading further to see for sure ...\n")
        hardware.configuration <- readBin(file, what="raw", n=48) # FIXME: this hard-wiring is repeated elsewhere
        if (hardware.configuration[1] != 0xa5 || hardware.configuration[2] != 0x05) return("unknown")
        if (debug) cat("  hardware.configuration[1:2]", hardware.configuration[1:2], "(expect 0xa5 0x05)\n")
        head.configuration <- readBin(file, what="raw", n=224)
        if (debug) cat("  head.configuration[1:2]", head.configuration[1:2], "(expect 0xa5 0x04)\n")
        if (head.configuration[1] != 0xa5 || head.configuration[2] != 0x04) return("unknown")
        user.configuration <- readBin(file, what="raw", n=512)
        if (debug) cat("  user.configuration[1:2]", user.configuration[1:2], "(expect 0xa5 0x00)\n")
        if (user.configuration[1] != 0xa5 || user.configuration[2] != 0x00) return("unknown")
        next.two.bytes <- readBin(file, what="raw", n=2)
        if (debug) cat("  next.two.bytes:", next.two.bytes,"(e.g. 0x5 0x12 is adv/nortek/vector)\n")
        if (next.two.bytes[1] == 0xa5 && next.two.bytes[2] == 0x12) {
            if (debug) cat("this is adv/nortek/vector\n")
            return("adv/nortek/vector")
        }
        if (next.two.bytes[1] == 0xa5 && next.two.bytes[2] == 0x01) {
            if (debug) cat("this is adp/nortek/aqudopp\n")
            return("adp/nortek/aquadopp") # p33 SIG
        }
        if (next.two.bytes[1] == 0xa5 && next.two.bytes[2] == 0x2a)  {
            if (debug) cat("this is adp/nortek/aqudoppHR\n")
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
        if (debug) cat("this is ctd/woce/exchange\n")
        return("ctd/woce/exchange")
    }
    if ("* Sea-Bird" == substr(line, 1, 10))  {
        if (debug) cat("this is ctd/sbe/19\n")
        return("ctd/sbe/19")
    }
    if ("# -b" == substr(line, 1, 4)) {
        if (debug) cat("this is coastline\n")
        return("coastline")
    }
    if ("# Station_Name," == substr(line, 1, 15)) {
        if (debug) cat("this is sealevel\n")
        return("sealevel")
    }
    if ("Station_Name," == substr(line, 1, 13)) {
        if (debug) cat("this is sealevel\n")
        return("sealevel")
    }
    if (0 < regexpr("^[0-9][0-9][0-9][A-Z] ", line)) {
        if (debug) cat("this is sealevel\n")
        return("sealevel")
    }
    if (0 < regexpr("^NCOLS[ ]*[0-9]*[ ]*$", line)) {
        if (debug) cat("this is topo\n")
        return("topo")
    }
    if ("RBR TDR" == substr(line, 1, 7))  {
        if (debug) cat("this is pt\n")
        return("pt")
    }
    if ("BOTTLE"  == substr(line, 1, 6))  {
        if (debug) cat("this is section\n")
        return("section")
    }
    if (debug) cat("this is unknown\n")
    return("unknown")
}

read.oce <- function(file, ...)
{
    type <- magic(file)
    log.action <- paste(deparse(match.call()), sep="", collapse="")
    if (type == "adp/rdi")     return(read.adp.rdi(file,                              ..., log.action=log.action))
    if (type == "adp/nortek/aquadopp")  stop("Sorry, the oce package cannot read ADP/nortek/aquadopp files yet")
    if (type == "adp/nortek/aquadoppHR")  return(read.adp.nortek(file,               ..., log.action=log.action))
    if (type == "adv/nortek/vector")       return(read.adv.nortek(file,               ..., log.action=log.action))
    if (type == "adv/sontek/adr")          return(read.adv.sontek.adr(file,           ..., log.action=log.action))
    if (type == "ctd/sbe/19")              return(read.ctd.sbe(file,                  ..., log.action=log.action))
    if (type == "ctd/woce/exchange")       return(read.ctd.woce(file,                 ..., log.action=log.action))
    if (type == "coastline")               return(read.coastline(file, type="mapgen", ..., log.action=log.action))
    if (type == "sealevel")                return(read.sealevel(file,                 ..., log.action=log.action))
    if (type == "topo")                    return(read.topo(file,                     ..., log.action=log.action))
    if (type == "pt")                      return(read.pt(file,                       ..., log.action=log.action))
    if (type == "section")                 return(read.section(file,                  ..., log.action=log.action))
    stop("unknown file type")
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

oce.axis.POSIXct <- function (side, x, at, format, labels = TRUE, draw.time.range=TRUE, ...)
{
    debug <- !TRUE
    ## This was written because axis.POSIXt in R version 2.8.x did not obey the
    ## time zone in the data.  (Version 2.9.0 obeys the time zone.)
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

    if (d < 60) {                       # under 1 min
        rr <- range
        class(rr) <- c("POSIXt", "POSIXct")
        attr(rr, "tzone") <- attr(x, "tzone")
        if (debug) cat("range=", paste(format(rr), collapse=" to "), "\n")
        t.start <- trunc(rr[1]-30, "mins")
        t.end <- trunc(rr[2]+30, "mins")
        z <- seq(t.start, t.end, by="min")
        if (debug) {
            cat("z=")
            str(z)
        }
        if (missing(format)) format <- "%H:%M:%S"
    } else if (d < 60 * 30) {                  # under 30min
        rr <- range
        class(rr) <- c("POSIXt", "POSIXct")
        attr(rr, "tzone") <- attr(x, "tzone")
        if (debug) cat("range=", paste(format(rr), collapse=" to "), "\n")
        t.start <- trunc(rr[1]-30, "mins")
        t.end <- trunc(rr[2]+30, "mins")
        z <- seq(t.start, t.end, by="min")
        if (debug) {
            cat("(under 30 minutes) z=")
            str(z)
        }
        if (missing(format)) format <- "%H:%M"
    } else if (d < 60 * 60) {                  # under 1 hour
        rr <- range
        class(rr) <- c("POSIXt", "POSIXct")
        attr(rr, "tzone") <- attr(x, "tzone")
        if (debug) cat("range=", paste(format(rr), collapse=" to "), "\n")
        t.start <- trunc(rr[1]-30, "mins")
        t.end <- trunc(rr[2]+30, "mins")
        z <- seq(t.start, t.end, by="10 min")
        if (debug) {
            cat("(under an hour) z=")
            str(z)
        }
        if (missing(format)) format <- "%H:%M"
    } else if (d < 60 * 60 * 2) {       # under 2 hours
        rr <- range
        class(rr) <- c("POSIXt", "POSIXct")
        attr(rr, "tzone") <- attr(x, "tzone")
        if (debug) cat("range=", paste(format(rr), collapse=" to "), "\n")
        t.start <- trunc(rr[1]-30, "mins")
        t.end <- trunc(rr[2]+30, "mins")
        z <- seq(t.start, t.end, by="10 min")
        if (debug) {
            cat("(under an hour) z=")
            str(z)
        }
        if (missing(format)) format <- "%H:%M"
    } else if (d < 60 * 60 * 6) {       # under 6 hours, use HM
        rr <- range
        class(rr) <- c("POSIXt", "POSIXct")
        attr(rr, "tzone") <- attr(x, "tzone")
        t.start <- trunc(rr[1], "hour")
        t.end <- trunc(rr[2] + 3600, "hour")
        z <- seq(t.start, t.end, by="30 min")
        if (missing(format)) format <- "%H:%M"
        if (debug) {
            cat("(under 6 hours) z=")
            str(z)
        }
    } else if (d < 60 * 60 * 24) {        # under a day
        rr <- range
        class(rr) <- c("POSIXt", "POSIXct")
        attr(rr, "tzone") <- attr(x, "tzone")
        t.start <- trunc(rr[1], "hour")
        t.end <- trunc(rr[2] + 3600, "hour")
        z <- seq(t.start, t.end, by="hour")
        if (missing(format)) format <- "%H:%M"
        if (debug) cat("labels at", format(z), "\n")
    } else if (d < 60 * 60 * 24 * 2) {        # under 2 days
        rr <- range
        class(rr) <- c("POSIXt", "POSIXct")
        attr(rr, "tzone") <- attr(x, "tzone")
        t.start <- trunc(rr[1], "hour")
        t.end <- trunc(rr[2] + 86400, "hour")
        z <- seq(t.start, t.end, by="hour")
        if (missing(format)) format <- "%H"
        if (debug) cat("labels at", format(z), "\n")
    } else if (d < 60 * 60 * 24 * 32) {        # under 2 weeks
        rr <- range
        class(rr) <- c("POSIXt", "POSIXct")
        attr(rr, "tzone") <- attr(x, "tzone")
        t.start <- trunc(rr[1], "day")
        t.end <- trunc(rr[2] + 86400, "day")
        z <- seq(t.start, t.end, by="day")
        if (missing(format)) format <- "%b %d"
        if (debug) cat("labels at", format(z), "\n")
    } else if (d < 1.1 * 60 * 60 * 24 * 365) { # under about a year
        rr <- range
        class(rr) <- c("POSIXt", "POSIXct")
        attr(rr, "tzone") <- attr(x, "tzone")
        t.start <- trunc(rr[1], "day")
        t.end <- trunc(rr[2] + 86400, "day")
        z <- seq(t.start, t.end, by="month")
        if (missing(format)) format <- "%b %d"
        if (debug) cat("labels at", format(z), "\n")
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
        if (debug) cat("labels at", format(z), "\n")
    }
    if (!mat)
        z <- x[is.finite(x)]
    keep <- range[1] <= z & z <= range[2]
    z <- z[keep]
    if (!is.logical(labels))
        labels <- labels[keep]
    else if (identical(labels, TRUE))
        labels <- format(z, format = format)
    else if (identical(labels, FALSE))
        labels <- rep("", length(z))
    if (draw.time.range) {
        time.range <- par("usr")[1:2]
        class(time.range) <- c("POSIXt", "POSIXct")
        attr(time.range, "tzone") <- attr(x, "tzone")
        mtext(paste(paste(format(time.range), collapse=" to "), attr(time.range, "tzone")),
              side=if (side==1) 3 else 1, cex=4/5*par("cex"), adj=0)
    }
    axis(side, at = z, labels = labels, ...)
}
