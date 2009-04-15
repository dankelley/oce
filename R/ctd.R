as.ctd <- function(S, t, p,
                   ship=NA,scientist=NA,institute=NA,address=NA,
                   cruise=NA,station=NA,date=NA,start.time=NA,
                   latitude=NA, longitude=NA,
                   recovery=NA,
                   water.depth=NA,
                   sample.interval=NA,
                   src="")
{
    if (length(p) == 1) # special case
        p = rep(p, length(S))
    data <- data.frame(salinity=S, temperature=t, pressure=p, sigma.theta=sw.sigma.theta(S, t, p))
    metadata <- list(
                     header=NULL,
                     filename=NULL,
                     filename.orig=NULL,
                     system.upload.time=NULL,
                     ship=ship,
                     scientist=scientist,
                     institute=institute,
                     address=address,
                     cruise=cruise,
                     station=station,
                     date=date,
                     start.time=start.time,
                     latitude=latitude,
                     longitude=longitude,
                     recovery=recovery,
                     water.depth=water.depth,
                     sample.interval=sample.interval,
                     src=src)
    log <- processing.log.item(paste(deparse(match.call()), sep="", collapse=""))
    res <- list(data=data, metadata=metadata, log=log)
    class(res) <- c("ctd", "oce")
    res
}

ctd.add.column <- function (x, column=NULL, column.name="", code="", name="", unit="", debug = FALSE)
{
    if (length(column) < 1) stop("must supply column data")
    if (column.name == "")  stop("must supply 'column.name'")
    if (code=="")           stop("must supply 'code'")
    if (name=="")           stop("must supply 'name'")
    if (unit=="")           stop("must supply 'unit'")
    result <- x
    insert.in.header <- function(h, flag, content, content.name)
    {
        last.was.flag <- FALSE
        after <- -1
        flags <- 0 # how many flagged lines exist on input
        pattern <- paste("^#[\t ]*", flag)
        n <- length(h)
        for (i in 1:n) {
            if (flag == "name") { # increment nquan (skip on e.g. "span")
                g <- grep("#[\t ]*nquan[\t ]*=[\t ]*", h[i], perl=TRUE, useBytes=TRUE)
                if (length(g)) {
                    nquan <- unlist(strsplit(h[i], "\\s"))[4]
                    nquan.new <- as.character(1 + as.integer(nquan))
                    h[i] <- sub(nquan, nquan.new, h[i])
                }
            }
            if (last.was.flag) {
                if (!length(grep(pattern, h[i], perl=TRUE, useBytes=TRUE))) {
                    after <- i
                    break
                }
            }
            if (debug) cat("grep(\"", pattern, "\", \"", h[i], "\",...) -> ", length(grep(pattern,h[i],perl=TRUE,useBytes=TRUE)), "\n",sep="")
            if (length(grep(pattern, h[i], perl=TRUE, useBytes=TRUE))) {
                last.was.flag <- TRUE
                flags <- flags + 1
            }
        }
        if (after < 1) stop("Cannot locate any flagged lines in input header")
        if (debug) cat("after=", after, "\n", "\t", h[after-1], "\n\t", h[after])
        return(c(h[1:(after-1)],
                 paste("# ", flag, " ", flags, " = ", content, sep=""),
                 h[after:n]))
    }
    h <- result$metadata$header
    h <- insert.in.header(h, "name", sprintf("%s: %s, [%s]", code, name, unit))
    r <- range(column)
    h <- insert.in.header(h, "span", sprintf("%f, %f",r[1],r[2]))
    if (debug) {
        cat("Original header:", result$header,sep="\n")
        cat("Modified header:", h,sep="\n")
    }
    result$metadata$header <- h
    result$data[,column.name] <- column
    log.action <- paste(deparse(match.call()), sep="", collapse="")
    result <- processing.log.append(result, log.action)
    return(result)
}

ctd.decimate <- function(x, p, method=c("approx", "boxcar","lm"), e=1.5)
    ## SHOULD ADD: BIO method; spline; supsmu; ...
{
    if (!inherits(x, "ctd")) stop("method is only for ctd objects")
    res <- x
    n <- length(x$data$pressure)
    if (n < 2) {
        warning("too few data to trim.decimate()")
        return(res)
    }
                                        # Figure out pressure targets, pt
    if (missing(p)) {
                                        # autoscale
        dp.exact <- median(abs(diff(x$data$pressure)))
        dp <- pretty(3 * dp.exact)[2] # try for 3 data at least
        pt <- seq(0, dp * floor(max(x$data$pressure) / dp), dp)
    } else {
        if (length(p) == 1) {
            pt <- seq(0, p * floor(max(x$data$pressure) / p), p)
        } else {
            pt <- p
        }
    }
    npt <- length(pt)
                                        # Step through each variable.
    data.names <- names(x$data)
    data.new <- as.data.frame(array(NA, dim=c(npt, dim(x$data)[2])))
    names(data.new) <- data.names
    method <- match.arg(method)
    if (method == "approx") {
        too.deep <- pt > max(x$data[["pressure"]], na.rm=TRUE)
        for (datum.name in data.names) {
            if (datum.name != "pressure") {
                data.new[[datum.name]] <- approx(x$data[["pressure"]], x$data[[datum.name]], pt, rule=2)$y
                data.new[[datum.name]][too.deep] <- NA
            }
        }
    } else {
        for (i in 1:npt) {
            if (i==1) {
                focus <- (x$data$pressure >= (pt[i] - e*(pt[i+1] - pt[ i ]))) & (x$data$pressure <= (pt[i] + e*(pt[i+1] - pt[ i ])))
            } else if (i == npt) {
                focus <- (x$data$pressure >= (pt[i] - e*(pt[ i ] - pt[i-1]))) & (x$data$pressure <= (pt[i] + e*(pt[ i ] - pt[i-1])))
            } else {
                focus <- (x$data$pressure >= (pt[i] - e*(pt[ i ] - pt[i-1]))) & (x$data$pressure <= (pt[i] + e*(pt[i+1] - pt[ i ])))
            }
            ##cat("i=",i,"pt[i]=",pt[i],"\n")
            if (sum(focus, na.rm=TRUE) > 0) {
                if (method == "boxcar") {
                    for (datum.name in data.names) {
                        if (datum.name != "pressure") {
                            ##cat("i=",i,"datum=",datum.name,"avg=",mean(x$data[[datum.name]][focus]),"\n")
                            data.new[[datum.name]][i] <- mean(x$data[[datum.name]][focus],na.rm=TRUE)
                        }
                    }
                } else if (method == "lm") { # FIXME: this is far too slow
                    xvar <- x$data[["pressure"]][focus]
                    for (datum.name in data.names) {
                        if (datum.name != "pressure") {
                            yvar <- x$data[[datum.name]][focus]
                            m <- lm(yvar ~ xvar)
                            data.new[[datum.name]][i] <- predict(m, newdata=list(xvar=pt[i]))
                        }
                    }
                } else {
                    stop("impossible to get here -- developer error")
                }
            } else {                    # No data in the focus region
                for (datum.name in data.names) {
                    ##cat("i=",i,"NO DATA IN focus =\n")
                    if (datum.name != "pressure") {
                        data.new[[datum.name]][i] <- NA
                    }
                }
            }
        }
    }
    data.new[["pressure"]] <- pt
    res$data <- data.new
    log.action <- paste(deparse(match.call()), sep="", collapse="")
    res <- processing.log.append(res, log.action)
    res
}

ctd.trim <- function(x, method="downcast", parameters, verbose=FALSE)
{
    if (!inherits(x, "ctd")) stop("method is only for ctd objects")
    result <- x
    n <- length(x$data$pressure)
    if (n < 2) {
        warning("too few data to ctd.trim()")
    } else {
        which.method <- pmatch(method, c("index", "downcast"), nomatch=0)
        ##if (verbose) cat("ctd.trim()\n  using method", which.method,"\n")
        keep <- rep(TRUE, n)
        if (which.method == 1) {        # "index"
            ##if (verbose)	cat("  parameters:",parameters,"\n");
            if (min(parameters) < 1)
                stop("Cannot select indices < 1");
            if (max(parameters) > n)
                stop(paste("Cannot select past end of array, i.e. past ", n))
            keep <- rep(FALSE, n)
            keep[parameters] <- TRUE
        } else if (which.method == 2) { # "downcast"
                                        # 1. despike to remove (rare) instrumental problems
            x$data$pressure <- smooth(x$data$pressure,kind="3R")
            pmin <- 0
            if (!missing(parameters)) {
                if ("pmin" %in% names(parameters)) pmin <- parameters$pmin else stop("parameter not understood for this method")
            }
            keep <- (x$data$pressure > pmin) # 2. in water (or below start depth)
            delta.p <- diff(x$data$pressure)  # descending
            delta.p <- c(delta.p[1], delta.p) # to get right length
            keep <- keep & (delta.p > 0)
                                        # 3. trim the upcast and anything thereafter (ignore beginning and end)
            trim.top <- as.integer(0.1*n)
            trim.bottom <- as.integer(0.9*n)
            max.spot <- which.max(smooth(x$data$pressure[trim.top:trim.bottom],kind="3R"))
            max.location <- trim.top + max.spot
            keep[max.location:n] <- FALSE
            ##if (verbose) cat("  pressure maximum at index=",max.spot,"\n")
            if (FALSE) {
                                        # deleted method: slowly-falling data
                delta.p.sorted <- sort(delta.p)
                if (!is.null(parameters)) {
                    dp.cutoff <- t.test(delta.p[keep], conf.level=0.5)$conf.int[1]
                    print(t.test(delta.p[keep], conf.level=0.05))#$conf.int[1]
                } else {
                    dp.cutoff <- delta.p.sorted[0.1*n]
                }
                keep[delta.p < dp.cutoff] <- FALSE
            }
                                        # 4. remove equilibration phase
            if (FALSE) {                # old method, prior to Feb 2008
                pp <- x$data$pressure[keep]
                ss <- x$data$scan[keep]
                equilibration <- (predict(m <- lm(pp ~ ss), newdata=list(ss=x$data$scan)) < 0)
                keep[equilibration] <- FALSE
            }
            if (TRUE) {                 # new method, after Feb 2008
                bilinear1 <- function(s, s0, dpds) {
                    ifelse(s < s0, 0, dpds*(s-s0))
                }
                pp <- x$data$pressure[keep]
                ss <- x$data$scan[keep]
                ##if (verbose) plot(ss,pp,ylim=rev(range(pp)))
                p0 <- 0
                s0 <- ss[0.25*length(ss)]
                p0 <- pp[1]
                p1 <- max(pp) #pp[0.9*length(pp)]
                dpds0 <-  diff(range(pp)) / diff(range(ss))
                t <- try(m <- nls(pp ~ bilinear1(ss, s0, dpds),
                                  start=list(s0=s0, dpds=dpds0)),
                         silent=TRUE)
                if (class(t) != "try-error") {
                    if (m$convInfo$isConv) {
                        s0 <- floor(coef(m)[[1]])
                        ##if (verbose) cat("  trimming scan numbers below", s0, "\n")
                        ##if (verbose) print(summary(m))
                        keep <- keep & (x$data$scan > (coef(m)[[1]]))
                    }
                } else {
                    warning("unable to complete step 5 of the trim operation (removal of initial equilibrium phase)")
                }
                if (verbose) cat("ctd.trim(read.oce(\"", x$metadata$filename, "\"), \"scan\", c(", min(x$data$scan[keep],na.rm=TRUE), ",", max(x$data$scan[keep],na.rm=TRUE),"))\n",sep="")
            }
        } else {
            if (verbose)	cat(paste("column",method,"; parameters ", parameters[1], parameters[2]))
            l <- length(parameters)
            if (l == 1) { 		# lower limit
                keep <- (x$data[[method]] > parameters[1]);
            } else if (l == 2) {	# lower and upper limits
                keep <- (x$data[[method]] > parameters[1]) & (x$data[[method]] < parameters[2])
            }
        }
    }
    result$data <- subset(x$data, keep)
    result <- processing.log.append(result, paste(deparse(match.call()), sep="", collapse=""))
    result
}

ctd.update.header <- function (x, debug = FALSE)
{
    if (length(x$metadata$header) < 1) stop("there is no header in this CTD object")
    if (length(x$data) < 1) stop("there are no data in this CTD object")
    replace.header.element <- function(h, match, new)
    {
        for (i in 1:length(h)) {
            if (length(grep(match, h[i], perl=TRUE, useBytes=TRUE))) {
                h[i] <- new;
                break;
            }
        }
        return(h)
    }
                                        # adjust nvalues
                                        # ... fill in ...
                                        # adjust column ranges
    nquan <- length(x$data)
    xret <- x
    h <- xret$metadata$header
    for (i in 1:nquan) {
        r <- range(x$data[[i]])
        prefix <- sprintf("^#[\t ]*span[\t ]*%d[\t ]*=", i)
        span <- sprintf("# span %d = %g, %g", i, r[1], r[2]);
        h <- replace.header.element(h, prefix, span);
    }
    xret$header <- h
    return(xret)
}

ctd.write <- function(object, file=stop("'file' must be specified"))
{
  	if (!inherits(object, "ctd")) stop("method is only for ctd objects")
	if (is.character(file)) {
		if (file == "") stop("'file' must be a non-empty string")
		con <- file(file, "w")
	} else if (inherits(file, "connection")) {
		con <- file
    }
	cat(object$header, sep="\n", file=con) # bug: ranges are wrong
	write.table(object$data, col.names=FALSE,row.names=FALSE, file=con)
	close(con)
}

plot.ctd <- function (x, ref.lat = NaN, ref.lon = NaN,
                      grid = TRUE, col.grid="lightgray", lty.grid="dotted",
                      which = 1:4,
                      coastline,
                      Slim, Tlim, plim, densitylim, dpdtlim, timelim,
                      lonlim, latlim,
                      latlon.pch=20, latlon.cex=1.5, latlon.col="red",
                      adorn=NULL,
                      ...)
{
    ## FIXME: plot.ctd() should not use par(new=TRUE); it should copy plot.ctd.scan()
    dec_deg <- function(x, code = "lat") {
        if (code == "lat") {
            if (x < 0) {
                x <- -x
                sprintf("%.0f %.2fS", floor(x), 60 * (x - floor(x)))
            }
            else {
                sprintf("%.0f %.2fN", floor(x), 60 * (x - floor(x)))
            }
        } else {
            if (x < 0) {
                x <- -x
                sprintf("% %.2fW", floor(x), 60 * (x - floor(x)))
            }
            else {
                sprintf("% %.2fE", floor(x), 60 * (x - floor(x)))
            }
        }
    }
    if (!inherits(x, "ctd")) stop("method is only for ctd objects")

    ## 1=S+T
    ## 2=density+N2
    ## 3=TS
    ## 4=text
    ## 5=map
    ## 6=density+dpdt
    ## 7=density+time
    ## 8=index
    ## 9=density
    if (any(!which %in% 1:9)) stop("which must be between 1 and 9")

    lw <- length(which)
    adorn.length <- length(adorn)
    if (adorn.length == 1) {
        adorn <- rep(adorn, lw)
        adorn.length <- lw
    }

    if (!"mgp" %in% names(list(...))) par(mgp = getOption("oce.mgp"))
    mgp <- par("mgp")
    par(mar=c(mgp[1]+1,mgp[1]+1,mgp[1]+1.5,mgp[1])) # 1.5 because density unit has superscript

    ##cat("mgp=",paste(par("mgp"), collapse=" "), "\n")
    ##cat("mar=",paste(par("mar"), collapse=" "), "\n")

    if (lw > 1) {
        oldpar <- par(no.readonly = TRUE)
#        par(mar=c(3,3,3.25,2))
#        if (!"mgp" %in% names(list(...))) par(mgp = c(2, 2/3, 0))
        if (lw > 2)
            lay <- layout(matrix(1:4, nrow=2, byrow=TRUE))
        else
            lay <- layout(matrix(1:2, nrow=2, byrow=TRUE))
        ##layout.show(lay)
        ##stop()
    }
    for (w in 1:length(which)) {
        if (which[w] == 1) plot.profile(x, type = "S+T", Slim=Slim, Tlim=Tlim, plim=plim,
                 grid=grid, col.grid=col.grid, lty.grid=lty.grid, ...)
        if (which[w] == 2) plot.profile(x, type = "density+N2", plim=plim,
                 grid=grid, col.grid=col.grid, lty.grid=lty.grid, ...)
        if (which[w] == 6) plot.profile(x, "density+dpdt", plim=plim, densitylim=densitylim, dpdtlim=dpdtlim,
                 grid=grid, col.grid=col.grid, lty.grid=lty.grid, ...)
        if (which[w] == 7) plot.profile(x, "density+time", plim=plim, densitylim=densitylim, timelim=timelim,
                 grid=grid, col.grid=col.grid, lty.grid=lty.grid, ...)
        if (which[w] == 8) plot.profile(x, "index", plim=plim,
                 grid=grid, col.grid=col.grid, lty.grid=lty.grid, ...)
        if (which[w] == 9) plot.profile(x, "density", plim=plim,
                 grid=grid, col.grid=col.grid, lty.grid=lty.grid, ...)
        if (which[w] == 3) {
##            par(mar=c(3.5,3,2,2))
            plot.TS(x, Slim=Slim, Tlim=Tlim,
                    grid=grid, col.grid=col.grid, lty.grid=lty.grid, ...)
        }
        if (which[w] == 4) {
            text.item <- function(item, label, cex=0.8) {
                if (!is.null(item) && !is.na(item)) {
                    text(xloc, yloc, paste(label, item), adj = c(0, 0), cex=cex);
                    yloc <<- yloc - d.yloc;
                }
            }
            par(mar=c(0,0,0,0))
            plot.new()
            plot.window(c(0,10), c(0,10))
            xloc <- 0
            yloc <- 8
            d.yloc <- 0.8
            cex <- 3/4
            text(xloc, yloc, paste("CTD Station"), adj = c(0, 0), cex=cex)
            yloc <- yloc - d.yloc
            xm <- x$metadata
            if (!is.null(xm$filename))    	text.item(xm$filename,    " File:     ", cex=cex)
            if (!is.null(xm$scientist))	text.item(xm$scientist,   " Scientist:", cex=cex)
            if (!is.null(xm$institute))	text.item(xm$institute,   " Institute:", cex=cex)
            if (!is.null(xm$date))    	text.item(xm$date,        " Date:     ", cex=cex)
            if (!is.null(xm$ship))		text.item(xm$ship,        " Ship:     ", cex=cex)
            if (!is.null(xm$cruise))    	text.item(xm$cruise,      " Cruise:   ", cex=cex)
            if (!is.null(xm$station))    	text.item(xm$station,     " Station:  ", cex=cex)
            if (!is.null(xm$water.depth))  	text.item(xm$water.depth, " Depth:    ", cex=cex)
            if (!is.na(xm$longitude) && !is.na(xm$latitude))
                text.item(latlon.format(xm$latitude, xm$longitude),   " Location: ", cex=cex)
            if (!is.na(ref.lat) && !is.na(ref.lon)) {
                dist <- geod.dist(xm$latitude, xm$longitude, ref.lat, ref.lon)
                kms <- sprintf("%.2f km", dist/1000)
                rlat <- text(xloc, yloc, paste(" Distance to (", dec_deg(ref.lon),
                                               ",", dec_deg(ref.lat), ") = ", kms), adj = c(0, 0), cex=cex)
                yloc <- yloc - d.yloc
            }
        }
        if (which[w] == 5) {
            if (missing(coastline)) stop("need a coastline file to draw a map")
            if (!missing(lonlim) && !missing(latlim))
                plot(coastline, xlim=lonlim, ylim=latlim)
            else
                plot(coastline)
            points(x$metadata$longitude, x$metadata$latitude, cex=latlon.cex, col=latlon.col, pch=latlon.pch)
            title(paste("Station", x$metadata$station),font.main=par("font"))
        }
        if (w <= adorn.length && nchar(adorn[w]) > 0) {
            t <- try(eval(adorn[w]), silent=TRUE)
            if (class(t) == "try-error") warning("cannot evaluate adorn[", w, "]\n")
        }
    }
#    if (lw > 1)
#        par(oldpar)
    invisible()
}

plot.ctd.scan <- function(x,
                          name = "scan",
                          S.col = "darkgreen",
                          T.col = "darkred",
                          p.col = "blue",
                          adorn=NULL,
                          ...)
{
    if (!inherits(x, "ctd")) stop("method is only for ctd objects")

    if (!"mgp" %in% names(list(...))) par(mgp = getOption("oce.mgp"))
    mgp <- par("mgp")
    par(mar=c(mgp[1], mgp[1]+1, 1, mgp[1]+2))

    adorn.length <- length(adorn)
    if (adorn.length == 1) {
        adorn <- rep(adorn, 2)
        adorn.length <- 2
    }
    layout(matrix(1:2, nrow=2))
    xx <- x$data[[name]];
    xxlen <- length(xx)
    ##if (xxlen < 1) stop(paste("this ctd has no data column named '", name, "'",sep=""))
    if (xxlen < 1) {
        xxlen <- length(x$data$pressure)
        xx <- seq(1, xxlen)             # fake a scan number
    }
    if (xxlen != length(x$data$pressure)) stop(paste("length mismatch.  '", name, "' has length ", xxlen, " but pressure has length ", length(x$data$pressure),sep=""))
    plot(x$data[[name]], x$data$pressure,
         xlab=name, ylab="Pressure [dbar]",
         type="l", col=p.col, axes=FALSE)
    mtext(paste("Station", x$metadata$station), side=3, adj=1)
    mtext(latlon.format(x$metadata$latitude, x$metadata$longitude, digits=5), side=3, adj=0)
    box()
    grid(col="brown")
    axis(1)
    axis(2,col=p.col, col.axis=p.col, col.lab = p.col)
    if (1 <= adorn.length) {
        t <- try(eval(adorn[1]), silent=TRUE)
        if (class(t) == "try-error") warning("cannot evaluate adorn[", 1, "]\n")
    }

##    par(mar=c(4,4,1,4)) # bot left top right
    Slen <- length(x$data$salinity)
    Tlen <- length(x$data$temperature)
    if (Slen != Tlen) stop(paste("length mismatch.  'salinity' has length ", Slen, " but 'temperature' has length ", Tlen, sep=""))
    plot(x$data[[name]], x$data$temperature, xlab="scan", ylab="", type="l", col = T.col, axes=FALSE)
    axis(1)
    axis(2,col=T.col, col.axis = T.col, col.lab = T.col)
    box()
    grid(NULL, NA, col="brown")
    mtext("Temperature [degC]", side = 2, line = 2, col = T.col)
    ## used to par(new=TRUE) here, but some say that is a bad idea
    usr <- par("usr")
    Sr <- range(x$data$salinity, na.rm=TRUE)
    usr[3:4] <- Sr + c(-1, 1) * 0.04 * diff(Sr)
    par(usr=usr)
    lines(x$data[[name]], x$data$salinity, col=S.col)
    mtext("Salinity [PSU]", side = 4, line = 2, col = S.col)
    axis(4,col=S.col, col.axis = S.col, col.lab = S.col)
    if (2 <= adorn.length) {
        t <- try(eval(adorn[2]), silent=TRUE)
        if (class(t) == "try-error") warning("cannot evaluate adorn[", 2, "]\n")
    }
    invisible(x)
}
##* Sea-Bird SBE 25 Data File:
##CTD,20060609WHPOSIODAM

read.ctd <- function(file, type=NULL, debug=FALSE, columns=NULL, station=NULL, check.human.headers=FALSE, log.action)
{
    if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
    filename <- NULL
    if (is.null(type)) {
        if (is.character(file)) {
            filename <- file
            file <- file(file, "r")
            on.exit(close(file))
        }
        if (!inherits(file, "connection")) stop("argument `file' must be a character string or connection")
        if (!isOpen(file)) {
            open(file, "r")
            on.exit(close(file))
        }
        line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE);
        pushBack(line, file)
        if ("CTD" == substr(line, 1, 3))              type <- "WOCE"
        else if ("* Sea-Bird" == substr(line, 1, 10)) type <- "SBE19"
        else stop("Cannot discover type in line", line, "\n")
    } else {
        if (!is.na(pmatch(type, "SBE19")))            type <- "SBE19"
        else if (!is.na(pmatch(type, "WOCE")))        type <- "WOCE"
        else stop("type must be SBE19 or WOCE, not ", type)
    }
    switch(type,
           SBE19 = read.ctd.SBE19(file, filename, debug, columns, station=station, check.human.headers=check.human.headers, log.action),
           WOCE  = read.ctd.WOCE(file, filename, debug, columns, station=station, missing.value=-999, log.action))
}

read.ctd.WOCE <- function(file, filename, debug=FALSE, columns=NULL, station=NULL, missing.value=-999, log.item)
{
    if (is.character(file)) {
        filename <- file
        file <- file(file, "r")
        on.exit(close(file))
    }
    if (!inherits(file, "connection")) stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }
                                        # Header
    scientist <- ship <- institute <- address <- NULL
    filename.orig <- NULL
    sample.interval <- NaN
    system.upload.time <- NULL
    latitude <- longitude <- NaN
    start.time <- NULL
    water.depth <- NaN
    date <- recovery <- NULL
    header <- c();
    col.names.inferred <- NULL
    found.scan <- FALSE
    found.temperature <- found.salinity <- found.pressure <- found.depth <- FALSE
    found.sigma.theta <- found.sigma.t <- found.sigma <- FALSE
    found.conductivity <- found.conductivity.ratio <- FALSE
    conductivity.standard <- 4.2914
    ## http://www.nodc.noaa.gov/woce_V2/disk02/exchange/exchange_format_desc.htm
    ## First line
    line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE);
    if(debug) cat(paste("examining header line '",line,"'\n"));
    header <- line
    ## CTD, 20000718WHPOSIOSCD
    if ("CTD" != substr(line, 1, 3)) stop("Can only read WOCE files of type CTD")
    tmp <- sub("(.*), ", "", line);
    date <- substr(tmp, 1, 8)
    diw <- substr(tmp, 9, nchar(tmp)) # really, divisionINSTITUTEwho
    institute <- diw # BUG: really, it is division, institute, who, strung together
                                        # Kludge: recognize some institutes
    if (0 < regexpr("SIO", diw)) institute <- "SIO"
    while (TRUE) {
        line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE);
        if(debug) cat(paste("examining header line '",line,"'\n"));
        header <- c(header, line);
        ## SAMPLE:
        ##      EXPOCODE = 31WTTUNES_3
        ##      SECTION_ID = P16C
        ##      STNNBR = 221
        ##      CAST = 1
        ##      DATE = 19910901
        ##      TIME = 0817
        ##      LATITUDE = -17.5053
        ##      LONGITUDE = -150.4812
        ##      BOTTOM = 3600
        if (!(0 < (r<-regexpr("^#", line)))) {
            ## NUMBER_HEADERS = 10
            nh <- as.numeric(sub("(.*)NUMBER_HEADERS = ", "", ignore.case=TRUE, line))
            for (i in 2:nh) {
                line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE);
                header <- c(header, line)
                if ((0 < (r<-regexpr("LATITUDE",  line))))
                    latitude  <- as.numeric(sub("[a-zA-Z =]*","", line))
                if ((0 < (r<-regexpr("LONGITUDE", line))))
                    longitude <- as.numeric(sub("(.*) =","", line))
                if ((0 < (r<-regexpr("DATE", line)))) {
                    d <- sub("[ ]*DATE[ ]*=[ ]*", "", line)
                    date <- oce.as.POSIXlt(d, "%Y%m%d")
                }
                if ((0 < (r<-regexpr("DEPTH", line))))
                    water.depth <- as.numeric(sub("[a-zA-Z =]*","", line))
                if ((0 < (r<-regexpr("DEPTH", line))))
                    water.depth <- as.numeric(sub("[a-zA-Z =]*","", line))
                if ((0 < (r<-regexpr("STNNBR", line))))
                    station <- as.numeric(sub("[a-zA-Z =]*","", line))
            }
            break
        }
    }
    while (TRUE) {                    # catch any remaining "#" lines
        line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
        if (!(0 < (r<-regexpr("^#", line)))) break
        header <- c(header, line)
    }
    ##CTDPRS,CTDPRS_FLAG_W,CTDTMP,CTDTMP_FLAG_W,CTDSAL,CTDSAL_FLAG_W,CTDOXY,CTDOXY_FLAG_W,
    var.names <- strsplit(line, split=",")[[1]]
    line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
    var.units <- strsplit(line, split=",")[[1]]
    pcol <- pmatch("CTDPRS", var.names)
    if (is.na(pcol)) stop("cannot find pressure column in list", paste(var.names,","))
    Scol <- pmatch("CTDSAL", var.names)
    if (is.na(Scol)) stop("cannot find salinity column in list", paste(var.names,","))
    Tcol <- pmatch("CTDTMP", var.names)
    if (is.na(Tcol)) stop("cannot find temperature column in list", paste(var.names,","))

    var.names <- strsplit(line, split=",")[[1]]
    line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
    var.units <- strsplit(line, split=",")[[1]]
    pressure <- NULL
    temperature <- NULL
    salinity <- NULL
    while (TRUE) {
        line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
        if (0 < (r<-regexpr("END_DATA", line))) break
        items <- strsplit(line, ",")[[1]]
        pressure    <- c(pressure,    as.numeric(items[pcol]))
        salinity    <- c(salinity,    as.numeric(items[Scol]))
        temperature <- c(temperature, as.numeric(items[Tcol]))
    }
    pressure[pressure == missing.value] <- NA
    salinity[salinity == missing.value] <- NA
    temperature[temperature == missing.value] <- NA
    sigma.theta <- sw.sigma.theta(salinity, temperature, pressure)
    data <- data.frame(pressure=pressure, salinity=salinity, temperature=temperature, sigma.theta=sigma.theta)
    metadata <- list(header=header,
                     filename=filename, # provided to this routine
                     filename.orig=filename.orig, # from instrument
                     system.upload.time=system.upload.time,
                     ship=ship,
                     scientist=scientist,
                     institute=institute,
                     address=address,
                     cruise=NULL,
                     station=station,
                     date=date,
                     start.time=start.time,
                     latitude=latitude,
                     longitude=longitude,
                     recovery=recovery,
                     water.depth=water.depth,
                     sample.interval=sample.interval,
                     src=filename)
    if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
    res <- list(data=data, metadata=metadata, processing.log=log.action)
    class(res) <- c("ctd", "oce")
    res
}

parse.latlon <- function(line, debug=FALSE)
{
    ## The following formats are understood (for, e.g. latitude)
    ## * NMEA Latitude = 47 54.760 N
    ## ** Latitude:      47 53.27 N
    x <- line
    positive <- TRUE
    if (debug) cat("parse.latlon() processing stages\n0. [", x, "]\n", sep="")
    x <- sub("(.*)latitude", "", ignore.case=TRUE, x);
    x <- sub("(.*)longitude", "", ignore.case=TRUE, x);
    x <- sub("[:=]", "", ignore.case=TRUE, x);
    if (debug) cat("1. [", x, "]\n", sep="")
    if (0 < (r <- regexpr("[NnEe]", x)))
        x <- sub("[NnEe]", "", ignore.case=TRUE, x)
    if (debug) cat("2. [", x, "]\n", sep="")
    if (0 < (r <- regexpr("[SsWw]", x))) {
        positive <- FALSE
        x <- sub("[SsWw]", "", ignore.case=TRUE, x)
    }
    if (debug) cat("3. [", x, "]\n", sep="")
    x <- sub("^[ \t]*", "", ignore.case=TRUE, x)
    if (debug) cat("4. [", x, "]\n", sep="")
    x <- sub("[ \t]*$", "", ignore.case=TRUE, x)
    if (debug) cat("5. [", x, "]\n", sep="")
    x <- strsplit(x, " ")
    if (length(x[[1]]) == 2) {
        x <- as.double(x[[1]][1]) + as.double(x[[1]][2]) / 60
        if (!positive)
            x <- (-x)
    } else {
        warning("cannot parse latitude or longitude in header since need 2 items but got ", length(x[[1]]), " items in '", line, "'\n")
    }
    if (debug) cat(sprintf("6. x = %f\n", x))
    x
}

read.ctd.SBE19 <- function(file, filename, debug=FALSE, columns=NULL, station=NULL, check.human.headers=TRUE, log.action)
{
    ## Read Seabird data file.  Note on headers: '*' is machine-generated,
    ## '**' is a user header, and '#' is a post-processing header.
    if (is.character(file)) {
        filename <- file
        file <- file(file, "r")
        on.exit(close(file))
    }
    if (!inherits(file, "connection")) stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }
                                        # Header
    scientist <- ship <- institute <- address <- cruise <- filename.orig <- ""
    sample.interval <- NA
    system.upload.time <- NULL
    latitude <- longitude <- NA
    start.time <- NULL
    water.depth <- NA
    date <- recovery <- NA
    header <- c();
    col.names.inferred <- NULL
    found.temperature <- found.salinity <- found.pressure <- found.depth <- found.scan <- found.time <- found.sigma.theta <- found.sigma.t <- found.sigma <- found.conductivity <- found.conductivity.ratio <- FALSE
    conductivity.standard <- 4.2914
    found.header.latitude <- found.header.longitude <- FALSE
    while (TRUE) {
        line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE);
        if(debug) cat(paste("examining header line '",line,"'\n"));
        header <- c(header, line);
        ##if (length(grep("\*END\*", line))) #BUG# why is this regexp no good (new with R-2.1.0)
        aline <- iconv(line, from="UTF-8", to="ASCII", sub="?");
        if (length(grep("END", aline, perl=TRUE, useBytes=TRUE))) break;
        lline <- tolower(aline);
        ## BUG: discovery of column names is brittle to format changes
        if (0 < (r <- regexpr("# name ", lline))) {
            if (debug) cat("lline: '",lline,"'\n",sep="")
            tokens <- strsplit(line, split=" ")
            name <- tokens[[1]][6]
            if (debug) cat("  name: '",name,"'\n",sep="")
            if (0 < regexpr("scan", lline)) {
                name <- "scan"
                found.scan <- TRUE
            }
            if (0 < regexpr("pressure", lline)) {
                name <- "pressure"
                found.pressure <- TRUE
            }
            if (0 < regexpr("time", lline)) {
                name <- "time"
                found.time <- TRUE
            }
            if (0 < regexpr("salinity", lline)) {
                name <- "salinity"
                found.salinity <- TRUE
            }
            if (0 < regexpr("temperature", lline)) {
                name <- "temperature"
                found.temperature <- TRUE
            }
            if (0 < regexpr("conductivity", lline)) {
                if (0 < regexpr("ratio", lline)) {
                    found.conductivity.ratio <- TRUE;
                    name <- "conductivityratio";
                } else {
                    found.conductivity <- TRUE;
                    name <- "conductivity";
                }
            }
            if (0 < regexpr("depth", lline) || 0 < regexpr("depSM", lline)) {
                name <- "depth"
                found.depth <- TRUE
            }
            if (0 < regexpr("fluorometer", lline)) name <- "fluorometer"
            if (0 < regexpr("oxygen, current", lline)) name <- "oxygen.current"
            if (0 < regexpr("oxygen, temperature", lline)) name <- "oxygen.temperature"
            if (0 < regexpr("flag", lline)) name <- "flag"
            if (0 < regexpr("sigma-theta", lline)) {
                name <- "sigma.theta"
                found.sigma.theta <- TRUE
            } else {
                if (0 < regexpr("sigma-t", lline)) {
                    name <- "sigma.t"
                    found.sigma.t <- TRUE
                }
            }
            col.names.inferred <- c(col.names.inferred, name)
        }
        if (0 < (r<-regexpr("date:", lline))) {
            d <- sub("(.*)date:([ ])*", "", lline);
            date <- oce.as.POSIXlt(d)
        }
        if (0 < (r<-regexpr("filename", lline))) {
                                        #cat("FileName... ",lline,"\n")
            filename.orig <- sub("(.*)FileName =([ ])*", "", ignore.case=TRUE, lline);
                                        #cat(" ... '",filename.orig,"'\n")
        }
        if (0 < (r<-regexpr("system upload time", lline))) {
                                        #cat(lline, "\n")
            d <- sub("([^=]*)[ ]*=[ ]*", "", ignore.case=TRUE, lline);
                                        #cat(d,"\n")
            system.upload.time <- oce.as.POSIXlt(d)
                                        #cat(paste("system upload time:", system.upload.time, "\n"))
        }
        ## Styles:
        ## * NMEA Latitude = 47 54.760 N
        ## ** Latitude:      47 53.27 N
        if (!found.header.latitude && (0 < (r<-regexpr("latitude*[0-8]*", lline, ignore.case=TRUE)))) {
            latitude <- parse.latlon(lline)
            found.header.latitude <- TRUE
        }
        if (!found.header.longitude && (0 < (r<-regexpr("longitude*[0-8]*", lline, ignore.case=TRUE)))) {
            longitude <- parse.latlon(lline)
            found.header.longitude <- TRUE
        }
        if (0 < (r<-regexpr("start_time =", lline))) {
            d <- sub("#[ ]*start_time[ ]*=[ ]*", "", lline)
            start.time <- oce.as.POSIXlt(d)
        }
        if (0 < (r<-regexpr("ship:", lline))) {
            ship <- sub("(.*)ship:([ \t])*", "", ignore.case=TRUE, line); # note: using full string
            ship <- sub("[ \t]*$", "", ship)
        }
        if (0 < (r<-regexpr("scientist:", lline)))
            scientist <- sub("(.*)scientist:([ ])*", "", ignore.case=TRUE, line); # full string
        if (0 < (r<-regexpr("institute:", lline)))
            institute <- sub("(.*)institute:([ ])*", "", ignore.case=TRUE, line); # full string
        if (0 < (r<-regexpr("address:", lline)))
            address <- sub("(.*)address:([ ])*", "", ignore.case=TRUE, line); # full string
        if (0 < (r<-regexpr("cruise:", lline)))
            cruise <- sub("(.*)cruise:([ ])*", "", ignore.case=TRUE, line); # full string
        if (is.null(station)) {
            if (0 < (r<-regexpr("station:", lline)))
                station <- sub("(.*)station:([ ])*", "", ignore.case=TRUE, line); # full string
        }
        if (0 < (r<-regexpr("recovery:", lline)))
            recovery <- sub("(.*)recovery:([ ])*", "", lline);
        if (0 < (r<-regexpr("water depth:", lline))) {
            linesplit <- strsplit(line," ")
            if (length(linesplit[[1]]) != 7)
                warning("cannot parse water depth in `",line,"' (expecting 7 tokens)");
            value <- linesplit[[1]][6]
            unit <- strsplit(lline," ")[[1]][7]
            if (!is.na(unit)) {
                if (unit == "m") {
                    water.depth <- as.numeric(value)
                } else {
                    if (rtmp[[1]][2] == "km") {
                        water.depth <- as.numeric(value) * 1000
                    }
                }
            }
        }
        if (0 < (r<-regexpr("^. sample rate =", lline))) {
                                        #* sample rate = 1 scan every 5.0 seconds
            rtmp <- lline;
            rtmp <- sub("(.*) sample rate = ", "", rtmp);
            rtmp <- sub("scan every ", "", rtmp);
            rtmp <- strsplit(rtmp, " ");
                                        #      if (length(rtmp[[1]]) != 3)
                                        #        warning("cannot parse sample-rate string in `",line,"'");
            sample.interval <- as.double(rtmp[[1]][2]) / as.double(rtmp[[1]][1])
            if (rtmp[[1]][3] == "seconds") {
                ;
            } else {
                if (rtmp[[1]][3] == "minutes") {
                    sample.interval <- sample.interval / 60;
                } else {
                    if (rtmp[[1]][3] == "hours") {
                        sample.interval <- sample.interval / 3600;
                    } else {
                        warning("cannot understand `",rtmp[[1]][2],"' as a unit of time for sample.interval");
                    }
                }
            }
        }
    }
    if (debug) cat("Finished reading header\n")
    if (check.human.headers) {
        if (is.nan(sample.interval)) warning("'* sample rate =' not found in header");
        if (is.nan(latitude))        warning("'** Latitude:' not found in header");
        if (is.nan(longitude))       warning("'** Longitude:' not found in header");
        if (is.null(date))           warning("'** Date:' not found in header");
        if (is.null(recovery))       warning("'** Recovery' not found in header");
    }
                                        # Require p,S,T data at least
    if (!found.temperature) stop("cannot find 'temperature' in this file")
    if (!found.pressure && !found.depth)    stop("no column named 'pressure', 'depth' or 'depSM'")

    ## Read the data as a table.
    ## FIXME: should we match to standardized names?
    ##col.names.forced <- c("scan","pressure","temperature","conductivity","descent","salinity","sigma.theta.unused","depth","flag");
    col.names.inferred <- tolower(col.names.inferred)
    if (debug) cat("About to read these names:", col.names.inferred,"\n");
    data <- read.table(file,col.names=col.names.inferred,colClasses="numeric");
    if (!found.scan) {
        newnames <- c("scan", names(data))
        data <- cbind(seq(1,dim(data)[1]), data)
        names(data) <- newnames
        warning("data file lacked a 'scan' column, so one was created");
    }
    metadata <- list(header=header,
                     filename=filename, # provided to this routine
                     filename.orig=filename.orig, # from instrument
                     system.upload.time=system.upload.time,
                     ship=ship,
                     scientist=scientist,
                     institute=institute,
                     address=address,
                     cruise=cruise,
                     station=station,
                     date=date,
                     start.time=start.time,
                     latitude=latitude,
                     longitude=longitude,
                     recovery=recovery,
                     water.depth=water.depth,
                     sample.interval=sample.interval,
                     src=filename)
    if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processing.log.item(log.action)
    res <- list(data=data, metadata=metadata, processing.log=log.item)
    class(res) <- c("ctd", "oce")
                                        # Add standard things, if missing
    if (!found.salinity) {
        if (found.conductivity.ratio) {
            warning("cannot find 'salinity' in this file; calculating from T, C, and p");
            S <- sw.S.C.T.p(data$conductivityratio, data$temperature, data$pressure)
        } else if (found.conductivity) {
            warning("cannot find 'salinity' in this file; calculating from T, C-ratio, and p");
            S <- sw.S.C.T.p(data$conductivity/conductivity.standard, data$temperature, data$pressure)
        } else {
            stop("cannot find salinity in this file, nor conductivity or conductivity ratio")
        }
        res <- ctd.add.column(res, S, "salinity", "sal", "salinity", "PSU")
    }
    if (found.depth && !found.pressure) { # BUG: this is a poor, nonrobust approximation of pressure
        g <- if (found.header.latitude) gravity(latitude) else 9.8
        rho0 <- sw.sigma.theta(median(res$data$salinity), median(res$data$temperature), rep(0, length(res$data$salinity)))
        res <- ctd.add.column(res, res$data$depth * g * rho0 / 1e4, "pressure", "pressure", "pressure", "dbar")
    }
    res <- ctd.add.column(res, sw.sigma.theta(res$data$salinity, res$data$temperature, res$data$pressure), "sigma.theta", "sigma.theta", "sigma.theta", "kg/m^3")
    return(res)
}

summary.ctd <- function(object, ...)
{
    if (!inherits(object, "ctd")) stop("method is only for ctd objects")
    dim <- dim(object$data)
    fives <- matrix(nrow=dim[2], ncol=5)
    res <- list(filename="?", system.upload.time="?", date="?", institute="?",
		scientist="?", ship="?", cruise="?", latitude=NA, longitude=NA,
		station="?", start.time="?", deployed="?", recovery="?", water.depth="?",
		levels="?",
                fives=fives,
                processing.log=processing.log.summary(object))
    if (!is.null(object$metadata$filename.orig))      res$filename <- object$metadata$filename.orig
    if (!is.null(object$metadata$system.upload.time)) res$upload.time <- object$metadata$system.upload.time
    if (!is.null(object$metadata$date))               res$date <- object$metadata$date
    if (!is.null(object$metadata$institute))          res$institute <- object$metadata$institute
    if (!is.null(object$metadata$scientist))          res$scientist <- object$metadata$scientist
    if (!is.null(object$metadata$ship))               res$ship <- object$metadata$ship
    if (!is.null(object$metadata$cruise))             res$cruise <- object$metadata$cruise
    if (is.finite(object$metadata$latitude))          res$latitude <- object$metadata$latitude
    if (is.finite(object$metadata$longitude))         res$longitude <- object$metadata$longitude
    if (!is.null(object$metadata$station))            res$station <- object$metadata$station
    if (!is.null(object$metadata$start.time))         res$start.time <- object$metadata$start.time
    if (!is.null(object$metadata$deployed))           res$deployed<- object$metadata$date
    if (!is.null(object$metadata$recovery))           res$recovery <- object$metadata$recovery
    if (!is.null(object$metadata$water.depth))        res$water.depth <- object$metadata$water.depth
    res$levels <- dim[1]
    for (v in 1:dim[2])
        fives[v,] <- fivenum(object$data[,v], na.rm=TRUE)
    rownames(fives) <- names(object$data)
    colnames(fives) <- c("Min.", "1st Qu.", "Median", "3rd Qu.", "Max.")
    res$fives <- fives
    class(res) <- "summary.ctd"
    res
}

print.summary.ctd <- function(x, digits=max(6, getOption("digits") - 1), ...)
{
    cat("CTD Profile\n")
    cat("  Raw file:           \"",     x$filename, "\"\n",sep="")
    cat(paste("  System upload time: ", x$system.upload.time, "\n"))
    cat(paste("  Date:               ", x$date, "\n"))
    cat("  Institute:          ",       x$institute, "\n")
    cat("  Scientist:          ",       x$scientist, "\n")
    cat("  Ship:               ",       x$ship, "\n")
    cat("  Cruise:             ",       x$cruise, "\n")
    cat("  Location:           ",       latlon.format(x$latitude, x$longitude, digits=digits), "\n")
    cat("  Station:            ",       x$station, "\n")
    cat(paste("  Start time:         ", as.POSIXct(x$start.time), "\n"))
    cat(paste("  Deployed:           ", x$date, "\n"))
    cat(paste("  Recovered:          ", x$recovery, "\n"))
    cat("  Water depth:        ",       x$water.depth, "\n")
    cat("  No. of levels:      ",       x$levels,  "\n")
    print(x$fives, digits=digits)
    print(x$processing.log)
    invisible(x)
}


plot.TS <- function (x,
                     rho.levels = 6,
                     grid = TRUE,
                     col.grid = "lightgray",
                     lty.grid = "dotted",
                     rho1000=FALSE,
                     col = par("col"),
                     col.rho = "darkgray",
                     cex.rho = 0.9 * par("cex"),
                     cex=par("cex"),
                     pch=21,
                     rotate.rho.labels=FALSE,
                     connect.points=FALSE,
                     xlab, ylab,
                     Slim, Tlim,
                     ...)
{
    if (!inherits(x, "ctd")) stop("method is only for ctd objects")
    if (missing(Slim)) Slim <- range(x$data$salinity, na.rm=TRUE)
    if (missing(Tlim)) Tlim <- range(x$data$temperature, na.rm=TRUE)

##    old.mgp <- par("mgp")
##    if (!"mgp" %in% names(list(...))) par(mgp = getOption("oce.mgp")) #par(mgp = c(2, 2/3, 0))

    axis.name.loc <- par("mgp")[1]
    old.mar <- par("mar")
###    if (!rotate.rho.labels && old.mar[4] < 3) par(mar=c(old.mar[1:3], 2))

    plot(x$data$salinity, x$data$temperature,
         xlab = if (missing(xlab)) "Salinity [ PSU ]" else xlab,
         ylab = if (missing(ylab)) expression(paste("Temperature [ ", degree, "C ]")) else ylab,
         xaxs = if (min(x$data$salinity,na.rm=TRUE)==0) "i" else "r", # avoid plotting S<0
         cex=cex, pch=pch, col=col, cex.axis=par("cex.axis"),
         xlim=Slim, ylim=Tlim,
         ...)
    if (connect.points) lines(x$data$salinity, x$data$temperature, col=col, ...)
    S.axis.min <- par()$usr[1]
    if (S.axis.min < 0.1) S.axis.min <- 0.1 # avoid NaN, which UNESCO density gives for freshwater
    S.axis.max <- par()$usr[2]
    T.axis.min <- par()$usr[3]
    T.axis.max <- par()$usr[4]
    ##if (is.null(args$xlab)) mtext("Salinity [ PSU ]", side = 1, line = 3)
    if (grid) grid(col=col.grid, lty=lty.grid)
    rho.corners <- sw.sigma(c(S.axis.min,
                              S.axis.max,
                              S.axis.min,
                              S.axis.max),
                            c(T.axis.min,
                              T.axis.min,
                              T.axis.max,
                              T.axis.max),
                            rep(0,4))
    rho.min <- min(rho.corners, na.rm=TRUE)
    rho.max <- max(rho.corners, na.rm=TRUE)
    if (length(rho.levels) == 1) {
        rho.list <- pretty(c(rho.min, rho.max), n=rho.levels)
                                        # Trim first and last values, since not in box
        rho.list <- rho.list[-1]
        rho.list <- rho.list[-length(rho.list)]
    } else {
        rho.list <- rho.levels
    }
    t.n <- 300
    t.line <- seq(T.axis.min, T.axis.max, length.out=t.n)
    cex.par <- par("cex")               # need to scale text() differently than mtext()
    for (rho in rho.list) {
        rho.label <- if (rho1000) 1000+rho else rho
        s.line <- sw.S.T.rho(t.line, rep(rho, t.n), rep(0, t.n))
        ok <- !is.na(s.line) # crazy T can give crazy S
        s.ok <- s.line[ok]
        t.ok <- t.line[ok]
        lines(s.ok, t.ok, col = col.rho)
        if (s.ok[length(s.ok)] > S.axis.max) { # to right of box
            i <- match(TRUE, s.ok > S.axis.max)
            if (rotate.rho.labels)
                mtext(rho.label, side=4, at=t.line[i], line=0.25, cex=cex.rho, col=col.rho)
            else
                text(par("usr")[2], t.line[i], rho.label, pos=4, cex=cex.rho/cex.par, col=col.rho, xpd=TRUE)
        } else { # above box ... if the line got there
            if (max(t.ok) > (T.axis.max - 0.05 * (T.axis.max - T.axis.min)))
                mtext(rho.label, side=3, at=s.line[t.n], line=0.25, cex=cex.rho, col=col.rho)
        }
    }
                                        # Freezing point
    Sr <- c(max(0, S.axis.min), S.axis.max)
    lines(Sr, sw.T.freeze(Sr, p=0), col="darkblue")
##    par(mar=old.mar)
##    par(mgp=old.mgp)
}

plot.profile <- function (x,
                          type = "S",
                          col.S = "darkgreen",
                          col.t = "red",
                          col.rho = "blue",
                          col.N2 = "brown",
                          col.dpdt = "darkgreen",
                          col.time = "darkgreen",
                          grid = TRUE,
                          col.grid = "lightgray",
                          lty.grid = "dotted",
                          Slim, Tlim, densitylim, N2lim, plim, dpdtlim, timelim,
                          lwd=par("lwd"),
                          ...)
{
    if (!inherits(x, "ctd")) stop("method is only for ctd objects")
    pname <- "Pressure [ dbar ]"
    if (missing(plim)) plim <- rev(range(x$data$pressure, na.rm=TRUE))
    plim <- sort(plim, decreasing=TRUE)
    axis.name.loc <- par("mgp")[1]
    know.time.unit <- FALSE
    if ("time" %in% names(x$data)) {
        know.time.unit <- TRUE
        time <- x$data$time
    } else {
        time <- 0:(length(x$data$pressure) - 1)
        if (!is.na(x$metadata$sample.interval)) {
            know.time.unit <- TRUE
            time <- time * x$metadata$sample.interval
        }
    }
    if (type == "index") {
        index <- 1:length(x$data$pressure)
        plot(index, x$data$pressure, ylim=plim, xlab = "index", ylab = pname, type='l')
    } else if (type == "density+time") {
        if (missing(densitylim)) densitylim <- range(x$data$sigma.theta, na.rm=TRUE)
	st <- sw.sigma.theta(x$data$salinity, x$data$temperature, x$data$pressure)
        plot(st, x$data$pressure,
             xlim=densitylim, ylim=plim,
             type = "n", xlab = "", ylab = pname, axes = FALSE)
        axis(3, col = col.rho, col.axis = col.rho, col.lab = col.rho)
        mtext(expression(paste(sigma[theta], " [ ", kg/m^3, " ]")), side = 3, line = axis.name.loc, col = col.rho, cex=par("cex"))
        axis(2)
        box()
        lines(st, x$data$pressure, col = col.rho, lwd=lwd)
        par(new = TRUE)
        if (missing(timelim)) timelim <- range(time, na.rm=TRUE)
        plot(time, x$data$pressure, xlim=timelim, ylim=plim, type='n', xlab="", ylab=pname, axes=FALSE, lwd=lwd, col=col.time)
        axis(1, col=col.dpdt, col.axis=col.dpdt, col.lab=col.time)
        lines(time, x$data$pressure, lwd=lwd, col=col.time)
        if (know.time.unit)
            mtext(expression(paste(Delta*t, " [ s ]")), side = 1, line = axis.name.loc, cex=par("cex"), col=col.time)
        else
            mtext(expression(paste(Delta*t, " [ unknown unit ]")), side = 1, line = axis.name.loc, cex=par("cex"), col=col.time)
        box()
        if (grid) {
            at <- par("yaxp")
            abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
        }
    } else if (type == "density+dpdt") {
        if (missing(densitylim)) densitylim <- range(x$data$sigma.theta, na.rm=TRUE)
	st <- sw.sigma.theta(x$data$salinity, x$data$temperature, x$data$pressure)
        plot(st, x$data$pressure,
             xlim=densitylim, ylim=plim,
             type = "n", xlab = "", ylab = pname, axes = FALSE)
        axis(3, col = col.rho, col.axis = col.rho, col.lab = col.rho)
        mtext(expression(paste(sigma[theta], " [ ", kg/m^3, " ]")), side = 3, line = axis.name.loc, col = col.rho, cex=par("cex"))
        axis(2)
        box()
        lines(st, x$data$pressure, col = col.rho, lwd=lwd)
        par(new = TRUE)
        dpdt <- diff(x$data$pressure) / diff(time)
        dpdt <- c(dpdt[1], dpdt)        # fake first point
        df <- min(max(x$data$pressure, na.rm=TRUE) / 5, length(x$data$pressure) / 10) # FIXME: adjust params
        dpdt.sm <- smooth.spline(x$data$pressure, dpdt, df=df)
        if (missing(dpdtlim)) dpdtlim <- range(dpdt.sm$y)
        plot(dpdt.sm$y, dpdt.sm$x, xlim=dpdtlim, ylim=plim, type='n', xlab="", ylab=pname, axes=FALSE, lwd=lwd, col=col.dpdt)
        axis(1, col=col.dpdt, col.axis=col.dpdt, col.lab=col.dpdt)
        lines(dpdt.sm$y, dpdt.sm$x, lwd=lwd, col=col.dpdt)
        if (know.time.unit)
            mtext(expression(paste(dp/dt, " [ dbar/s ]")), side = 1, line = axis.name.loc, cex=par("cex"), col=col.dpdt)
        else
            mtext(expression(paste(dp/dt, " [ dbar/(time-unit) ]")), side = 1, line = axis.name.loc, cex=par("cex"), col=col.dpdt)
        box()
        if (grid) {
            at <- par("yaxp")
            abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
            at <- par("xaxp")
            abline(v=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
        }
    } else if (type == "S") {
        if (missing(Slim)) Slim <- range(x$data$salinity, na.rm=TRUE)
        plot(x$data$salinity, x$data$pressure,
             xlim=Slim, ylim=plim,
             type = "n", xlab = "", ylab = pname, axes = FALSE)
        mtext("Salinity [ PSU ]", side = 3, line = axis.name.loc, col = col.S, cex=par("cex"))
        axis(2)
        axis(3, col = col.S, col.axis = col.S, col.lab = col.S)
        box()
        if (grid) {
            at <- par("yaxp")
            abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
            at <- par("xaxp")
            abline(v=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
        }
        lines(x$data$salinity, x$data$pressure, col = col.S, lwd=lwd)
    } else if (type == "T") {
        if (missing(Tlim)) Tlim <- range(x$data$temperature, na.rm=TRUE)
        plot(x$data$temperature, x$data$pressure,
             xlim=Tlim, ylim=plim,
             type = "n", xlab = "", ylab = pname, axes = FALSE)
        mtext(expression(paste("Temperature [ ", degree, "C ]")), side = 3, line = axis.name.loc, col = col.t, cex=par("cex"))
        axis(2)
        axis(3, col = col.t, col.axis = col.t, col.lab = col.t)
        box()
        if (grid) {
            at <- par("yaxp")
            abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
            at <- par("xaxp")
            abline(v=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
        }
        lines(x$data$temperature, x$data$pressure, col = col.t, lwd=lwd)
    } else if (type == "density") {
	st <- sw.sigma.theta(x$data$salinity, x$data$temperature, x$data$pressure)
        if (missing(densitylim)) densitylim <- range(st, na.rm=TRUE)
        plot(st, x$data$pressure,
             xlim=densitylim, ylim=plim,
             type = "n", xlab = "", ylab = pname, axes = FALSE)
        mtext(expression(paste(sigma[theta], " [ ", kg/m^3, " ]")), side = 3, line = axis.name.loc, col = col.rho, cex=par("cex"))
        axis(2)
        axis(3, col = col.rho, col.axis = col.rho, col.lab = col.rho)
        box()
        if (grid) {
            at <- par("yaxp")
            abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
            at <- par("xaxp")
            abline(v=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
        }
        lines(x$data$sigma.theta, x$data$pressure, col = col.rho, lwd=lwd)
    } else if (type == "density+N2") {
        if (missing(densitylim)) densitylim <- range(x$data$sigma.theta, na.rm=TRUE)
	st <- sw.sigma.theta(x$data$salinity, x$data$temperature, x$data$pressure)
        plot(st, x$data$pressure,
             xlim=densitylim, ylim=plim,
             type = "n", xlab = "", ylab = pname, axes = FALSE)
        axis(3, col = col.rho, col.axis = col.rho, col.lab = col.rho)
        mtext(expression(paste(sigma[theta], " [ ", kg/m^3, " ]")), side = 3, line = axis.name.loc, col = col.rho, cex=par("cex"))
        axis(2)
        box()
        lines(st, x$data$pressure, col = col.rho, lwd=lwd)
        par(new = TRUE)
        N2 <- sw.N2(x$data$pressure, st, ...)
        if (missing(N2lim)) N2lim <- range(N2, na.rm=TRUE)
        plot(N2, x$data$pressure,
             xlim=N2lim, ylim=plim,
             type = "n", xlab = "", ylab = "", axes = FALSE, lwd=lwd)
        axis(1, col = col.N2, col.axis = col.N2, col.lab = col.N2)
        lines(N2, x$data$pressure, col = col.N2, lwd=lwd)
        mtext(expression(paste(N^2, " [ ", s^-2, " ]")), side = 1, line = axis.name.loc, col = col.N2, cex=par("cex"))
        box()
        if (grid) {
            at <- par("yaxp")
            abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
        }
    } else if (type == "N2") {
        N2 <- sw.N2(x$data$pressure, x$data$sigma.theta, ...)
        if (missing(N2lim)) N2lim <- range(N2, na.rm=TRUE)
        plot(N2, x$data$pressure,
             xlim=N2lim, ylim=plim,
             type = "n", xlab = "", ylab = pname, axes = FALSE)
        mtext(expression(paste(N^2, " [ ", s^-2, " ]")), side = 3, line = axis.name.loc, col = col.N2, cex=par("cex"))
        axis(2)
        axis(3, col = col.N2, col.axis = col.N2, col.lab = col.N2)
        box()
        if (grid) {
            at <- par("yaxp")
            abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
            at <- par("xaxp")
            abline(v=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
        }
        lines(N2, x$data$pressure, col = col.N2, lwd=lwd)
        abline(v = 0, col = col.N2)
    } else if (type == "S+T") {
        if (missing(Slim)) Slim <- range(x$data$salinity, na.rm=TRUE)
        if (missing(Tlim)) Tlim <- range(x$data$temperature, na.rm=TRUE)
        plot(x$data$temperature, x$data$pressure,
             xlim=Tlim, ylim=plim,
             type = "n", xlab = "", ylab = pname, axes = FALSE)
        axis(3, col = col.t, col.axis = col.t, col.lab = col.t)
        mtext(expression(paste("Temperature [ ", degree, "C ]")), side = 3, line = axis.name.loc, col = col.t, cex=par("cex"))
        axis(2)
        box()
        lines(x$data$temperature, x$data$pressure, col = col.t, lwd=lwd)
        par(new = TRUE)
        plot(x$data$salinity, x$data$pressure,
             xlim=Slim, ylim=plim,
             type = "n", xlab = "", ylab = "", axes = FALSE)
        axis(1, col = col.S, col.axis = col.S, col.lab = col.S)
        mtext("Salinity [ PSU ]", side = 1, line = axis.name.loc, col = col.S, cex=par("cex"))
        box()
        if (grid) {
            at <- par("yaxp")
            abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
        }
        lines(x$data$salinity, x$data$pressure, col = col.S, lwd=lwd)
    } else {
        stop("unknown type, ", type, ", given")
    }
}
