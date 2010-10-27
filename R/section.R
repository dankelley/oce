make.section <- function(item, ...)
{
    if (inherits(item, "ctd")) {
        extra.args <- list(...)
        num.stations <- 1 + length(extra.args)
        station <- vector("list", num.stations)
        stn <- vector("character", num.stations)
        lon <- vector("numeric", num.stations)
        lat <- vector("numeric", num.stations)
        stn[1] <- item$metadata$station
        lat[1] <- item$metadata$latitude
        lon[1] <- item$metadata$longitude
        station[[1]] <- item
        if (num.stations > 1)
            for (i in 2:num.stations) {
                stn[i] <- extra.args[[i-1]]$metadata$station
                lat[i] <- extra.args[[i-1]]$metadata$latitude
                lon[i] <- extra.args[[i-1]]$metadata$longitude
                station[[i]] <- extra.args[[i-1]]
            }
    } else if (inherits(item, "list")) {
        num.stations <- length(item)
        station <- vector("list", num.stations)
        stn <- vector("character", num.stations)
        lon <- vector("numeric", num.stations)
        lat <- vector("numeric", num.stations)
        if (num.stations > 1)
            for (i in 1:num.stations) {
                stn[i] <- item[[i]]$metadata$station
                lat[i] <- item[[i]]$metadata$latitude
                lon[i] <- item[[i]]$metadata$longitude
                station[[i]] <- item[[i]]
            }
    } else {
        stop("first argument must be of class \"ctd\" or a \"list\"")
    }
    data <- list(station=station)
    metadata <- list(header="",section.id="",station.id=stn,latitude=lat,longitude=lon)
    log.item <- processing.log.item(paste(deparse(match.call()), sep="", collapse=""))
    res <- list(data=data, metadata=metadata, processing.log=log.item)
    class(res) <- c("section", "oce")
    res
}

"+.section" <- function(section, station)
{
    if (missing(station)) return(section) # not sure this can happen
    if (!inherits(section, "section")) stop("'section' is not a section")
    if (!inherits(station, "ctd")) stop("'station' is not a station")
    res <- section
    n.orig <- length(section$data$station)
    s <- vector("list", n.orig + 1)
    for (i in 1:n.orig)
        s[[i]] <- section$data$station[[i]]
    s[[n.orig + 1]] <- station
    res$data$station <- s
    res$metadata$latitude <- c(res$metadata$latitude, station$metadata$latitude)
    res$metadata$longitude <- c(res$metadata$longitude, station$metadata$longitude)
    res$metadata$station.id <- c(res$metadata$station.id, station$metadata$station)
    res$processing.log <- processing.log.add(res$processing.log,
                                             paste(deparse(match.call()), sep="", collapse=""))
    res
}

plot.section <- function(x,
                         which=1:4,
                         at=NULL,
                         labels=TRUE,
                         grid = FALSE,
                         contour.levels=NULL,
                         contour.labels=NULL,
                         station.indices,
                         coastline=NULL,
                         xlim=NULL, ylim=NULL,
                         map.xlim=NULL,
                         xtype="distance",
                         ytype="depth",
                         legend.loc="bottomright",
                         adorn=NULL,
                         mgp=getOption("oce.mgp"),
                         mar=c(mgp[1]+1, mgp[1]+1, mgp[2], mgp[2]+0.5),
                         debug=getOption("oce.debug"),
                         ...)
{
    debug <- if (debug > 2) 2 else floor(0.5 + debug)
    oce.debug(debug, "\bplot.section() {\n")
    plot.subsection <- function(variable="temperature", title="Temperature",
                                indicate.stations=TRUE, contour.levels=NULL, contour.labels=NULL,
                                xlim=NULL,
                                ylim=NULL,
                                debug=0,
                                ...)
    {
        oce.debug(debug, "\bplot.subsection() {\n")
        if (variable == "map") {
            lat <- array(NA, num.stations)
            lon <- array(NA, num.stations)
            for (i in 1:num.stations) {
                lat[i] <- x$data$station[[station.indices[i]]]$metadata$latitude
                lon[i] <- x$data$station[[station.indices[i]]]$metadata$longitude
            }
            lon[lon<0] <- lon[lon<0] + 360
            asp <- 1 / cos(mean(range(lat,na.rm=TRUE))*pi/180)
            latm <- mean(lat, na.rm=TRUE)
            lonm <- mean(lon, na.rm=TRUE)
            lonr <- lonm + 1.2 * (range(lon, na.rm=TRUE) - mean(lon, na.rm=TRUE)) # expand range
            latr <- latm + 1.2 * (range(lat, na.rm=TRUE) - mean(lat, na.rm=TRUE))
            if (!is.null(map.xlim))
                plot(lonr, latr, xlim=map.xlim, asp=asp, type='n', xlab="Longitude", ylab="Latitude")
            else
                plot(lonr, latr, asp=asp, type='n', xlab="Longitude", ylab="Latitude")
            if (!is.null(coastline)) {
                lines(coastline$data$longitude, coastline$data$latitude, col="darkgray")
                lines(coastline$data$longitude + 360, coastline$data$latitude, col="darkgray")
            }
            lines(lon, lat, col="lightgray")
            ## FIXME: possibly should figure out the offset, instead of just replotting shifted lon
            points(lon, lat, pch=20)
            points(lon - 360, lat, pch=20)
            points(lon[1], lat[1], pch=22, cex=2*par("cex"))
            points(lon[1] - 360, lat[1], pch=22, cex=2*par("cex"))
            if (indicate.stations) {
                dx <- 5 * mean(diff(sort(x$metadata$longitude)),na.rm=TRUE)
                dy <- 5 * mean(diff(sort(x$metadata$latitude)),na.rm=TRUE)
                xlab <- x$metadata$longitude[1] - dx * sign(x$metadata$longitude[2] - x$metadata$longitude[1])
                ylab <- x$metadata$latitude[1]  - dy * sign(x$metadata$latitude[2]  - x$metadata$latitude[1])
                text(xlab, ylab, x$metadata$station.id[1])
                xlab <- x$metadata$longitude[num.stations] -
                    dx * sign(x$metadata$longitude[num.stations-1] - x$metadata$longitude[num.stations])
                ylab <- x$metadata$latitude[num.stations]  -
                    dy * sign(x$metadata$latitude[num.stations-1]  - x$metadata$latitude[num.stations])
                text(xlab, ylab, x$metadata$station.id[num.stations])
            }
        } else {                        # not a map
            if (!(variable %in% names(x$data$station[[1]]$data)))
                stop("this section does not contain a variable named '", variable, "'")
            ## FIXME: contours don't get to plot edges
            xxrange <- range(xx, na.rm=TRUE)
            yyrange <- range(yy, na.rm=TRUE)
            ##yyrange[1] <- -1

            ## Put x in order, if it's not already
            ox <- order(xx)
            if (any(xx[ox] != xx)) {
                xx <- xx[ox]
                zz <- zz[ox,]
                warning("plot.section() reordered the stations to make x monotonic")
            }
            ylim <- if (!is.null(ylim)) sort(-abs(ylim)) else yyrange
            par(xaxs="i", yaxs="i")
            ylab <- if ("ylab" %in% names(list(...)))
                list(...)$ylab
            else { if (which.ytype==1) resizable.label("p") else "Depth [ m ]" }
            if (is.null(at)) {
                plot(xxrange, yyrange,
                     xaxs="i", yaxs="i",
                     xlim=xlim,
                     ylim=ylim,
                     col="white",
                     xlab=if (which.xtype==1) "Distance [ km ]" else "Along-track Distance [km]",
                     ylab=ylab,
                     axes=FALSE)
                axis(4, labels=FALSE)
                ytics <- axis(2, labels=FALSE)
                axis(2, at=ytics, labels=-ytics)
            } else {
                plot(xxrange, yyrange,
                     xaxs="i", yaxs="i",
##                     ylim=rev(yyrange),
                     xlim=xlim, ylim=ylim,
                     col="white",
                     xlab="", ylab=ylab, axes=FALSE)
                axis(1, at=at, labels=labels)
                axis(2)
                axis(4, labels=FALSE)
                box()
            }
            ## Bottom trace
            usr <- par("usr")
            graph.bottom <- usr[3]
            water.depth <- NULL

            for (i in 1:num.stations) {
                zz[i,] <- rev(x$data$station[[station.indices[i]]]$data[[variable]])
                if (grid) points(rep(xx[i], length(yy)), yy, col="gray", pch=20, cex=1/3)
                temp <- x$data$station[[station.indices[i]]]$data$temperature
                len <- length(temp)
                if (is.finite(x$data$station[[station.indices[i]]]$metadata$water.depth)) {
                    wd <- x$data$station[[station.indices[i]]]$metadata$water.depth
                } else {
                    wd <- NA
                    if (is.na(temp[len])) {
                        ##cat("bottom temperature is missing\n")
                        ##print(data.frame(p=x$data$station[[station.indices[[i]]]]$data$pressure, temp=temp))
                        wdi <- len - which(!is.na(rev(temp)))[1] + 1
                        ##cat("BOTTOM T:");print(temp[wdi])
                        ##cat("BOTTOM p:");print(x$data$station[[station.indices[i]]]$data$pressure[wdi])
                        wd <- x$data$station[[station.indices[i]]]$data$pressure[wdi]
                    }
                }
                in.land <- which(is.na(x$data$station[[station.indices[i]]]$data$temperature[-3])) # skip first 3 points
                ##cat("check==\n")
                ##print(x$data$station[[station.indices[i]]]$data$temperature)
                ##stop()
                ##cat("in.land=");print(in.land)
                if (!is.na(wd)) {
                    water.depth <- c(water.depth, wd)
                } else {
                    water.depth <- c(water.depth, max(x$data$station[[station.indices[i]]]$data$pressure, na.rm=TRUE))
                }
            }
            if (!grid)
                Axis(side=3, at=xx, labels=FALSE, tcl=-1/3, lwd=0.5) # station locations
            bottom.x <- c(xx[1], xx, xx[length(xx)])
            bottom.y <- c(graph.bottom, -water.depth, graph.bottom)
            ##cat("bottom.x: (length", length(bottom.x),")");print(bottom.x)
            ##cat("bottom.y: (length", length(bottom.y),")");print(bottom.y)

            dots <- list(...) # adjust plot parameter labcex, unless user did

            ##par(xaxs="i", yaxs="i")

            if (!is.null(contour.levels) && !is.null(contour.labels)) {
                oce.debug(debug, "user-supplied contour levels: ", contour.levels, "\n")
                if (!("labcex" %in% dots$labcex)) {
                    contour(x=xx, y=yy, z=zz, axes=FALSE, labcex=0.8,
                            levels=contour.levels,
                            labels=contour.labels,
                            add=TRUE,
                            xaxs="i", yaxs="i",
                            ...)
                } else {
                    contour(x=xx, y=yy, z=zz, axes=FALSE,
                            add=TRUE,
                            xaxs="i", yaxs="i",
                            ...)
                }
            } else {
                oce.debug(debug, "automatically-calculated contour levels\n")
                if (is.null(dots$labcex)) {
                    contour(x=xx, y=yy, z=zz, axes=FALSE, labcex=0.8,
                            add=TRUE,
                            xaxs="i", yaxs="i",
                            ...)
                } else {
                    contour(x=xx, y=yy, z=zz, axes=FALSE,
                            add=TRUE,
                            xaxs="i", yaxs="i",
                            ...)
                }
            }
#            if (length(bottom.x) == length(bottom.y))
#                polygon(bottom.x, bottom.y, col="gray")
            box()
            axis(1)
            legend(legend.loc, title, bg="white", x.intersp=0, y.intersp=0.5,cex=1)
        }
        oce.debug(debug, "\b} # plot.subsection()\n")
    }                                   # plot.subsection

    if (!inherits(x, "section")) stop("method is only for section objects")
    opar <- par(no.readonly = TRUE)
    if (length(which) > 1) on.exit(par(opar))

    if (any(!which %in% 1:4)) stop("which must be between 1 and 4")

    which.xtype <- pmatch(xtype, c("distance", "track"), nomatch=0)
    which.ytype <- pmatch(ytype, c("pressure", "depth"), nomatch=0)

    if (missing(station.indices)) {
        num.stations <- length(x$data$station)
        station.indices <- 1:num.stations
    } else {
        num.stations <- length(station.indices)
    }
    if (num.stations < 2) stop("cannot plot a section containing fewer than 2 stations")
    num.depths <- length(x$data$station[[station.indices[1]]]$data$pressure)

    ## Check that pressures coincide
    p1 <- x$data$station[[station.indices[1]]]$data$pressure
    for (ix in 2:num.stations) {
        if (any(p1 != x$data$station[[station.indices[ix]]]$data$pressure)) {
            stop("This section has stations with different pressure levels.\n  Please use e.g.\n\tsection.gridded <- section.grid(section)\n  to create a uniform grid, and then you'll be able to plot the section.")
        }
    }

    zz <- matrix(nrow=num.stations, ncol=num.depths)
    xx <- array(NA, num.stations)
    yy <- array(NA, num.depths)
    if (is.null(at)) {
        lat0 <- x$data$station[[station.indices[1]]]$metadata$latitude
        lon0 <- x$data$station[[station.indices[1]]]$metadata$longitude
        for (ix in 1:num.stations) {
            j <- station.indices[ix]
            if (which.xtype == 1) {
                xx[ix] <- geod.dist(lat0,
                                    lon0,
                                    x$data$station[[j]]$metadata$latitude,
                                    x$data$station[[j]]$metadata$longitude)
            } else if (which.xtype == 2) {
                if (ix == 1)
                    xx[ix] <- 0
                else
                    xx[ix] <- xx[ix-1] +
                        geod.dist(x$data$station[[station.indices[ix-1]]]$metadata$latitude,
                                  x$data$station[[station.indices[ix-1]]]$metadata$longitude,
                                  x$data$station[[j]]$metadata$latitude,
                                  x$data$station[[j]]$metadata$longitude)
            } else {
                stop("unknown xtype")
            }
        }
    } else {
        xx <- at
    }

    if (which.ytype == 1) yy <- rev(-x$data$station[[station.indices[1]]]$data$pressure)
    else if (which.ytype == 2) yy <- rev(-sw.depth(x$data$station[[station.indices[1]]]$data$pressure))
    else stop("unknown ytype")

    lw <- length(which)

    par(mgp=mgp, mar=mar)

    if (lw > 1) {
        if (lw > 2)
            layout(matrix(1:4, nrow=2, byrow=TRUE))
        else
            layout(matrix(1:2, nrow=2, byrow=TRUE))
    }
    adorn.length <- length(adorn)
    if (adorn.length == 1) {
        adorn <- rep(adorn, lw)
        adorn.length <- lw
    }
    for (w in 1:length(which)) {
        if (!missing(contour.levels)) {
            if (which[w] == 1)
                plot.subsection("temperature", "T", nlevels=contour.levels, xlim=xlim, ylim=ylim, debug=debug-1, ...)
            if (which[w] == 2)
                plot.subsection("salinity",    "S", ylab="", nlevels=contour.levels, xlim=xlim, ylim=ylim, debug=debug-1, ...)
            if (which[w] == 3)
                plot.subsection("sigma.theta",  expression(sigma[theta]), nlevels=contour.levels, xlim=xlim, ylim=ylim, debug=debug-1, ...)
        } else {
            if (which[w] == 1)
                plot.subsection("temperature", "T", xlim=xlim, ylim=ylim, debug=debug-1, ...)
            if (which[w] == 2)
                plot.subsection("salinity",    "S", ylab="", xlim=xlim, ylim=ylim, debug=debug-1, ...)
            if (which[w] == 3)
                plot.subsection("sigma.theta", expression(sigma[theta]), xlim=xlim, ylim=ylim, debug=debug-1, ...)
        }
        if (which[w] == 4)
            plot.subsection("map", indicate.stations=FALSE, debug=debug-1, ...)
        if (w <= adorn.length) {
            t <- try(eval(adorn[w]), silent=TRUE)
            if (class(t) == "try-error") warning("cannot evaluate adorn[", w, "]\n")
        }
    }
    oce.debug(debug, "\b} # plot.section()\n")
}

read.section <- function(file, section.id="", debug=getOption("oce.debug"), log.action)
{
    if (is.character(file)) {
        filename <- file
        file <- file(file, "r")
        on.exit(close(file))
    }
    if (!inherits(file, "connection")) {
        stop("argument `file' must be a character string or connection")
    }
    if (!isOpen(file)) {
        filename <- "(connection)"
        open(file, "r")
        on.exit(close(file))
    }
                                        # Skip header
    lines <- readLines(file)
    if ("BOTTLE" != substr(lines[1], 1, 6))
        stop("only type \"BOTTLE\" understood, but got header line\n", lines[1],"\n")
    if (nchar(section.id) < 1) section.id <- substr(lines[1], 8, nchar(lines[1]))
    n <- length(lines)
    header <- lines[1]
    for (l in (2:n)) {
        oce.debug(debug, lines[l], "\n")
        if ("#" != substr(lines[l], 1, 1)) {
            header <- c(header, lines[l])
            break
        }
    }
    header.length <- l + 1
    ccc <- textConnection(lines[header.length - 1])
    var.names <- scan(ccc, sep=",", what="", quiet=TRUE)
    close(ccc)
    ccc <- textConnection(lines[header.length])
    var.units <- scan(ccc, sep=",", what="", quiet=TRUE)
    close(ccc)
    if (length(var.units) != length(var.names)) stop("length mismatch in variable lists")
    header <- lines[1:header.length]
    nd <- n - header.length - 1
    nv <- length(var.names)
    data <- array(dim=c(nd, nv - 2))
    stn.section.id <- vector("character", nd)
    stn.id <- vector("character", nd)
    for (l in ((header.length + 1):(n-1))) { # last line is END_DATA
        contents <- strsplit(lines[l], split=",")[[1]]
        stn.section.id[l - header.length] <- sub(" *","", contents[2])
        stn.id[l - header.length] <- sub("^ *","", contents[3])
        data[l - header.length,] <- contents[3:nv]
        ## FIXME: maybe should just scan this thing; it might work better anyway
    }
    p <- as.numeric(data[,which(var.names=="CTDPRS") - 2])
    t <- as.numeric(data[,which(var.names=="CTDTMP") - 2])
    S <- as.numeric(data[,which(var.names=="CTDSAL") - 2])
    water.depth  <- as.numeric(data[,which(var.names=="DEPTH") - 2])
    latitude  <- as.numeric(data[,which(var.names=="LATITUDE") - 2])
    longitude <- as.numeric(data[,which(var.names=="LONGITUDE") - 2])
    station.id <- data[,which(var.names=="STNNBR") - 2]
    station.id <- sub(" *$","",sub("^ *","",station.id)) #remove blanks
    station.list <- unique(station.id)
    num.stations <- length(station.list)
    station <- vector("list", num.stations)
    stn <- vector("character", num.stations)
    lon <- vector("numeric", num.stations)
    lat <- vector("numeric", num.stations)
    for (i in 1:num.stations) {
        oce.debug(debug, "procession station ", i, "\n")
        select <- which(station.id == station.list[i])
        stn[i] <- sub("^ *", "", station.id[select[1]])
        lat[i] <- latitude[select[1]]
        lon[i] <- longitude[select[1]]
        select.depths <- select[(S[select] >= 0) & (t[select] >= -2) & (p[select] >= 0)]
        this.station <- as.ctd(S[select.depths], t[select.depths], p[select.depths],
                               latitude=lat[i],
                               longitude=lon[i],
                               cruise=stn.section.id[select[1]],
                               station=stn[i],
                               water.depth=water.depth[select[1]])
        oce.debug(debug, "station at ", lat[i], "N and ", lon[i], "W\n")
        station[[i]] <- this.station
    }
    data <- list(station=station)
    metadata <- list(header=header,section.id=section.id,station.id=stn,latitude=lat,longitude=lon)
    if (missing(log.action))
        log.action <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processing.log.item(log.action)
    res <- list(data=data, metadata=metadata, processing.log=log.item)
    class(res) <- c("section", "oce")
    res
}

section.grid <- function(section, p, method=c("approx","boxcar","lm"),
                         debug=getOption("oce.debug"), ...)
{
    oce.debug(debug, "\bsection.grid(section, p, method=\"", method, "\", ...) {\n", sep="")
    method <- match.arg(method)
    n <- length(section$data$station)
    oce.debug(debug, "have", n, "stations in this section\n")
    dp.list <- NULL
    if (missing(p)) {
        oce.debug(debug, "argument 'p' not given\n")
        p.max <- 0
        for (i in 1:n) {
            p <- section$data$station[[i]]$data$pressure
            dp.list <- c(dp.list, mean(diff(p)))
            p.max <- max(c(p.max, p))
        }
        dp <- mean(dp.list, na.rm=TRUE) / 1.5 # make it a little smaller
        pt <- pretty(c(0, p.max), n=min(200, floor(p.max / dp)))
        oce.debug(debug, "p.max=", p.max, "; dp=", dp, "\n")
        oce.debug(debug, "pt=", pt, "\n")
    } else {
        if (length(p) == 1) {
            if (p=="levitus") {
                pt <- c(0,   10,   20,   30,   50,   75,  100,  125,  150,  200,
                        250,  300,  400,  500,  600,  700,  800,  900, 1000, 1100,
                        1200, 1300, 1400, 1500, 1750, 2000, 2500, 3000, 3500, 4000,
                        4500, 5000, 5500)
            } else { # FIXME should insist numeric
                p.max <- 0
                for (i in 1:n) {
                    p <- section$data$station[[i]]$data$pressure
                    p.max <- max(c(p.max, p))
                }
                pt <- seq(0, p.max, p)
            }
        } else {
            pt <- p
        }
    }
    ## BUG should handle all variables (but how to interpolate on a flag?)
    res <- section
    for (i in 1:n) {
        ##cat("BEFORE:");print(res$data$station[[i]]$data$temperature[1:6])
        res$data$station[[i]] <- ctd.decimate(section$data$station[[i]], p=pt, method=method, debug=debug-1, ...)
        ##cat("AFTER: ");print(res$data$station[[i]]$data$temperature[1:6])
        ##cat("\n")
    }
    res$processing.log <- processing.log.add(res$processing.log,
                                             paste(deparse(match.call()), sep="", collapse=""))
    oce.debug(debug, "\b\b} # section.grid\n")
    res
}
## bugs: should ensure that every station has identical pressures
## FIXME: should have smoothing in the vertical also ... and is spline what I want??
section.smooth <- function(section, ...)
{
    if (!inherits(section, "section")) stop("method is only for section objects")
    nstn <- length(section$data$station)
    nprs <- length(section$data$station[[1]]$data$pressure)
    supplied.df <- "df" %in% names(list(...))
    if (!supplied.df) df <- nstn / 5
    res <- section
    x <- geod.dist(section)
    temperature.mat <- array(dim=c(nprs, nstn))
    salinity.mat <- array(dim=c(nprs, nstn))
    sigma.theta.mat <- array(dim=c(nprs, nstn))
    for (s in 1:nstn) {
        temperature.mat[,s] <- section$data$station[[s]]$data$temperature
        salinity.mat[,s] <- section$data$station[[s]]$data$salinity
        sigma.theta.mat[,s] <- section$data$station[[s]]$data$sigma.theta
    }
    for (p in 1:nprs) {
        ok <- !is.na(temperature.mat[p,])
        if (sum(ok) > 4) {              # can only fit spline if have 4 or more values
            if (supplied.df) {
                temperature.mat[p,ok] <- predict(smooth.spline(x[ok], temperature.mat[p,ok], ...))$y
                salinity.mat[p,ok] <- predict(smooth.spline(x[ok], salinity.mat[p,ok], ...))$y
                sigma.theta.mat[p,ok] <- predict(smooth.spline(x[ok], sigma.theta.mat[p,ok], ...))$y
            } else {
                temperature.mat[p,ok] <- predict(smooth.spline(x[ok], temperature.mat[p,ok], df=df, ...))$y
                salinity.mat[p,ok] <- predict(smooth.spline(x[ok], salinity.mat[p,ok], df=df, ...))$y
                sigma.theta.mat[p,ok] <- predict(smooth.spline(x[ok], sigma.theta.mat[p,ok], df=df, ...))$y
            }
        }
    }
    for (s in 1:nstn) {
        res$data$station[[s]]$data$temperature <- temperature.mat[,s]
        res$data$station[[s]]$data$salinity <- salinity.mat[,s]
        res$data$station[[s]]$data$sigma.theta <- sigma.theta.mat[,s]
    }
    class(res) <- c("section", "oce")
    res$processing.log <- processing.log.add(res$processing.log,
                                             paste(deparse(match.call()), sep="", collapse=""))
    res
}

summary.section <- function(object, ...)
{
    if (!inherits(object, "section")) stop("method is only for section objects")
    num.stations <- length(object$data$station)
    stn.sum <- matrix(nrow=num.stations, ncol=5)
    res <- list(section.id=object$metadata$section.id,
                num.stations=num.stations,
                stn.sum=stn.sum, processing.log="?")
    lon1 <- object$data$station[[1]]$metadata$longitude
    lat1 <- object$data$station[[1]]$metadata$latitude
    for (i in 1:num.stations) {
        stn <- object$data$station[[i]]
        stn.sum[i, 1] <- stn$metadata$longitude
        stn.sum[i, 2] <- stn$metadata$latitude
        stn.sum[i, 3] <- length(stn$data$pressure)
        if (is.finite(stn$metadata$water.depth)) {
            stn.sum[i, 4] <- stn$metadata$water.depth
        } else {
            temp <- stn$data$temperature
            wdi <- length(temp) - which(!is.na(rev(temp)))[1] + 1
            stn.sum[i, 4] <- stn$data$pressure[wdi]
        }
        stn.sum[i, 5] <- geod.dist(lat1, lon1, stn$metadata$latitude, stn$metadata$longitude)
    }
    colnames(stn.sum) <- c("Long.", "Lat.", "Levels", "Depth", "Distance")
    rownames(stn.sum) <- object$metadata$station.id
    res$stn.sum <- stn.sum
    res$processing.log <- processing.log.summary(object)
    class(res) <- "summary.section"
    res
}

print.summary.section <- function(x, digits=max(6, getOption("digits") - 1), ...)
{
    num.stations <- x$num.stations
    cat("\nSection \"", x$section.id, "\" ",sep="")
    if (num.stations > 0) {
        cat("contains ", num.stations, "stations (first column is station ID):\n")
        print(x$stn.sum, digits=digits)
    } else {
        cat("contains no stations.\n")
    }
    cat("Processing Log:\n", ...)
    cat(x$processing.log, ...)
    invisible(x)
}

