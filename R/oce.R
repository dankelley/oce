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
    if (!inherits(x, "oce")) stop("method is only for oce objects")
    if (inherits(x, "adcp")) {
        if (!is.null(indices)) {
            rval <- x
            keep <- (1:x$metadata$number.of.profiles)[indices]
            print(keep)
            stop("testing")
        } else if (!missing(subset)) {
            ss <- substitute(subset)
            profiles.to.keep <- eval(substitute(subset), x$data, parent.frame())
            cat("keep profiles: ", paste(profiles.to.keep, collapse=" "), "\n")
            print(dim(x$data[[1]]))
            ## FIXME: ugly kludge; see adcp.R for rest of kludge
            rval <- x
            for (i in 1:12) {
                rval$data[[i]] <- x$data[[i]][profiles.to.keep,]
            }
            for (i in 13:21) {
                if (names(x$data)[i] != "distance")
                    rval$data[[i]] <- x$data[[i]][profiles.to.keep]
            }
            ## end of ugly kludge
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

magic <- function(file)
{
    filename <- file
    if (is.character(file))
        file <- file(file, "r")
    if (!inherits(file, "connection")) stop("argument `file' must be a character string or connection")
    if (!isOpen(file))
    	open(file, "r")
    line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
    close(file)
    file <- file(filename, "rb")
    bytes <- readBin(file, what="raw", n=2)
    close(file)
    if (bytes[1] == 0x7f && bytes[2] == 0x7f)        return("adcp")
    if (bytes[1] == 0xa5 && bytes[2] == 0x05)        return("aquadopp")
    ##if (substr(line, 1, 2) == "\177\177")            return("adcp")
    if (substr(line, 1, 3) == "CTD")                 return("ctd.woce")
    if ("* Sea-Bird" == substr(line, 1, 10))         return("ctd.seabird")
    if ("# -b" == substr(line, 1, 4))                return("coastline")
    if ("# Station_Name," == substr(line, 1, 15))    return("sealevel")
    if ("Station_Name," == substr(line, 1, 13))      return("sealevel")
    if (0 < regexpr("^[0-9][0-9][0-9][A-Z] ", line)) return("sealevel")
    ##275A Halifax            Canada              1920 44400N 063350W 0000 3 00000R MM
    if (0 < regexpr("^NCOLS[ ]*[0-9]*[ ]*$", line))  return("topo")
    if ("RBR TDR" == substr(line, 1, 7))             return("pt")
    if ("BOTTLE"  == substr(line, 1, 6))             return("section")
    return("unknown")
}

read.oce <- function(file, ...)
{
    type <- magic(file)
    log.action <- paste(deparse(match.call()), sep="", collapse="")
    if (type == "adcp")        return(read.adcp(file, ..., log.action=log.action))
    if (type == "aquadopp")    return(read.aquadopp(file, ..., log.action=log.action))
    if (type == "ctd.woce")    return(read.ctd(file, ..., log.action=log.action))
    if (type == "ctd.seabird") return(read.ctd(file, ..., log.action=log.action))
    if (type == "coastline")   return(read.coastline(file, type="mapgen", ..., log.action=log.action))
    if (type == "sealevel")    return(read.sealevel(file, ..., log.action=log.action))
    if (type == "topo")        return(read.topo(file, ..., log.action=log.action))
    if (type == "pt")          return(read.pt(file, ..., log.action=log.action))
    if (type == "section")     return(read.section(file, ..., log.action=log.action))
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

oce.axis.POSIXct <- function (side, x, at, format, labels = TRUE, ...)
{
    mat <- missing(at) || is.null(at)
    if (!mat)
        x <- as.POSIXct(at)
    else x <- as.POSIXct(x)
    range <- par("usr")[if (side%%2) 1:2 else 3:4]

    d <- range[2] - range[1]
    z <- c(range, x[is.finite(x)])
    attr(z, "tzone") <- attr(x, "tzone")

    if (d < 1.1 * 60) {
        sc <- 1
        if (missing(format))
            format <- "%S"
    }
    else if (d < 1.1 * 60 * 60) {
        sc <- 60
        if (missing(format))
            format <- "%M:%S"
    }
    else if (d < 1.1 * 60 * 60 * 24) {
        sc <- 60 * 60
        if (missing(format))
            format <- "%H:%M"
    }
    else if (d < 2 * 60 * 60 * 24) {
        sc <- 60 * 60
        if (missing(format))
            format <- "%a %H:%M"
    }
    else if (d < 7 * 60 * 60 * 24) {
        sc <- 60 * 60 * 24
        if (missing(format))
            format <- "%a"
    }
    else {
        sc <- 60 * 60 * 24
    }

    if (d < 60 * 60 * 24 * 50) {
        zz <- pretty(z/sc)
        z <- zz * sc
        class(z) <- c("POSIXt", "POSIXct")
        attr(z, "tzone") <- attr(x, "tzone")
        if (sc == 60 * 60 * 24)
            z <- as.POSIXct(round(z, "days"))
        attr(z, "tzone") <- attr(x, "tzone")
        if (missing(format))
            format <- "%b %d"
    }
    else if (d < 1.1 * 60 * 60 * 24 * 365) {
        class(z) <- c("POSIXt", "POSIXct")
        attr(z, "tzone") <- attr(x, "tzone")
        zz <- as.POSIXlt(z)
        zz$mday <- zz$wday <- zz$yday <- 1
        zz$isdst <- -1
        zz$hour <- zz$min <- zz$sec <- 0
        zz$mon <- pretty(zz$mon)
        m <- length(zz$mon)
        M <- 2 * m
        m <- rep.int(zz$year[1], m)
        zz$year <- c(m, m + 1)
        zz <- lapply(zz, function(x) rep(x, length.out = M))
        class(zz) <- c("POSIXt", "POSIXlt")
        z <- as.POSIXct(zz)
        attr(z, "tzone") <- attr(x, "tzone")
        if (missing(format))
            format <- "%b"
    }
    else {
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
    }
    if (!mat)
        z <- x[is.finite(x)]
    keep <- z >= range[1] & z <= range[2]
    z <- z[keep]
    if (!is.logical(labels))
        labels <- labels[keep]
    else if (identical(labels, TRUE))
        labels <- format(z, format = format)
    else if (identical(labels, FALSE))
        labels <- rep("", length(z))
    axis(side, at = z, labels = labels, ...)
}
