as.sealevel <- function(eta,
                        t,
                        header=NULL,
                        station.number=NA,
                        station.version=NA,
                        station.name=NULL,
                        region=NA,
                        year=NA,
                        latitude=NA,
                        longitude=NA,
                        GMT.offset=NA,
                        decimation.method=NA,
                        reference.offset=NA,
                        reference.code=NA,
                        deltat)
{
    if (missing(eta)) stop("must supply sealevel height, eta, in metres")
    n <- length(eta)
    if (missing(t)) {              # construct hourly from time "zero"
        start <- as.POSIXct("0000-01-01 00:00:00", tz="GMT")
        t <- as.POSIXct(start + seq(0, n - 1, 1) * 3600, tz="GMT")
        if (is.na(GMT.offset)) GMT.offset <- 0 # FIXME: do I want to do this?
    } else {
        t <- as.POSIXct(t, tz="GMT") # FIXME: should this be GMT?
    }
    data <- data.frame(t=t, eta=eta)
    if (missing(deltat))
        deltat <- difftime(t[2], t[1], units="hours")
    if (is.na(deltat) | deltat <= 0)
        deltat <- 1
    metadata <- list(header=header,
                     year=year,
                     station.number=station.number,
                     station.version=station.version,
                     station.name=station.name,
                     region=region,
                     latitude=latitude,
                     longitude=longitude,
                     GMT.offset=GMT.offset,
                     decimation.method=decimation.method,
                     reference.offset=reference.offset,
                     reference.code=reference.code,
                     units=units,
                     n=length(t),
                     deltat=deltat)
    log.item <- processing.log.item(paste(deparse(match.call()), sep="", collapse=""))
    rval <- list(data=data, metadata=metadata, processing.log=log.item)
    class(rval) <- c("sealevel", "oce")
    rval
}

plot.sealevel <- function(x, focus.time=NULL, close.screens=TRUE, ...)
{
    ## tidal constituents (in cpd):
    ## http://www.soest.hawaii.edu/oceanography/dluther/HOME/Tables/Kaw.htm
    title.plot <- function(x) {
        mtext(paste(format(range(x$data$t)), collapse=" to "), side=3, cex=2/3, adj=0)
        title <- paste("Station ",
                       x$metadata$station.number, " (",
                       x$metadata$station.name,   ") ",
                       x$metadata$region,         "",
                       " ", latlon.format(x$metadata$latitude, x$metadata$longitude),
                       if (!is.na(x$metadata$year)) paste(" (", x$metadata$year, ")") else "",
                       sep="")
        mtext(side=3, title, adj=1, cex=2/3)
    }

    if (!"mgp" %in% names(list(...))) par(mgp = getOption("oce.mgp"))
    mgp <- par("mgp")
    par(mar=c(mgp[1],mgp[1]+2.5,mgp[2],mgp[2]+0.25))

    ##cat("mgp=",paste(par("mgp"), collapse=" "), "\n")
    ##cat("mar=",paste(par("mar"), collapse=" "), "\n")

    if (!is.null(focus.time)) {
        focus.time <- as.POSIXct(focus.time)
        focus <- (focus.time[1] <= x$data$t) & (x$data$t <= focus.time[2])
        eta.m <- x$data$eta
        MSL <- mean(eta.m, na.rm=TRUE)
        eta <- (eta.m[focus] - MSL)
        plot(as.POSIXct(x$data$t)[focus], eta, type='l',ylab="Sealevel Anomaly [m]")
        abline(h=0,col="darkgreen")
        mtext(side=4,text=sprintf("%.2f m", MSL),at=0,col="darkgreen")
        title.plot(x)
    } else {
        oldpar <- par(no.readonly = TRUE)
        eg.days <- 28
        stop <- 24 * eg.days
        eta.m <- x$data$eta
        MSL <- mean(eta.m, na.rm=TRUE)
        tmp <- (pretty(max(eta.m-MSL,na.rm=TRUE)-min(eta.m-MSL,na.rm=TRUE))/2)[2]
        ylim <- c(-tmp,tmp)
                                        # Whole timeseries
        n <- length(x$data$eta) # do not trust value in metadata
        from <- as.POSIXlt(x$data$t[1])
        from$mday <- 1
        from$hour <- from$min <- from$sec <- 0
        to <- as.POSIXlt(x$data$t[n])
        to$mday <- 28
        to$hour <- to$min <- to$sec <- 0
        at.t <- seq(from=from, to=to, by="month")
        num.NA <- sum(is.na(x$data$eta))
        oce.close.screen(all.screens=TRUE)
        if (num.NA) {
            warning("time series contains ", num.NA, " missing data, so no spectra will be drawn")
            oce.split.screen(c(2, 1), erase=TRUE)
##            par(mar=c(4,5,3,1)+0.1)
        } else {
            oce.split.screen(c(4, 1), erase=TRUE)
##            par(mar=c(2,5,2,1)+0.1)
        }

        oce.screen(1)
        plot(as.POSIXlt(x$data$t)[1:n], eta.m-MSL,
             xlab="",ylab=expression(paste(eta-eta[0], "  [m]")), type='l',ylim=ylim,
             lwd=0.5, axes=FALSE)
        mtext(paste(format(range(x$data$t)), collapse=" to "), side=3, cex=2/3, adj=0)
        axis(1, at.t, format(at.t, "%b %d"), cex=0.7)  # small font to get all 12 month names
        yax <- axis(2)
        box()
        abline(h=yax, col="darkgray", lty="dotted")
        abline(v=at.t, col="darkgray", lty="dotted")
        abline(h=0,col="darkgreen")
        mtext(side=4,text=sprintf("%.2f m",MSL),col="darkgreen")
        title.plot(x)

        oce.screen(2)
##        if (num.NA)
##            par(mar=c(3,5,0,1)+0.1)
##        else
##            par(mar=c(2,5,1,1)+0.1)
        from <- as.POSIXlt(x$data$t[1])
        from$hour <- from$min <- from$sec <- 0
        to <- from + 28 * 86400 # 28 days
        at.week <- seq(from=from, to=to, by="week")
        at.day  <- seq(from=from, to=to, by="day")
        tmp <- (pretty(max(eta.m[1:stop]-MSL,na.rm=TRUE)-min(eta.m[1:stop]-MSL,na.rm=TRUE))/2)[2]
        ylim <- c(-tmp,tmp)
        plot(x$data$t[1:stop], eta.m[1:stop] - MSL,
             xlab="",ylab=expression(paste(eta-eta[0], "  [m]")), type='l',ylim=ylim, axes=FALSE)
        axis(1, at.week, labels=format(at.week, "%b %d"))
        yax <- axis(2)
        abline(h=yax, col="lightgray", lty="dotted")
        box()
        abline(v=at.week, col="darkgray", lty="dotted")
        abline(v=at.day, col="lightgray", lty="dotted")
        abline(h=0,col="darkgreen")
        mtext(side=4,text=sprintf("%.2f m",MSL),col="darkgreen")

        ## Draw spectra, if series has no NA, so that spectrum is easy to construct
        if (!num.NA) {
            oce.screen(3)
            Eta <- ts(eta.m,start=1,deltat=x$metadata$deltat)
            s <- spectrum(Eta-mean(Eta),spans=c(5,3),plot=FALSE,log="y",demean=TRUE,detrend=TRUE)
            ##par(mar=c(2,5,1,1)+0.1)
            plot(s$freq,s$spec,xlim=c(0,0.1),
                 xlab="",ylab=expression(paste(Gamma^2, "   [",m^2/cph,"]")),
                 type='l',log="y")
            grid()
            draw.constituent <- function(frequency=0.0805114007,label="M2",col="darkred",side=1)
            {
                abline(v=frequency, col=col)
                mtext(label, side=side, at=frequency, col=col,cex=0.8)
            }
            draw.constituents <- function()
            {
                draw.constituent(0.0387306544, "O1", side=1)
                ##draw.constituent(0.0416666721, "S1", side=3)
                draw.constituent(0.0417807462, "K1", side=3)
                draw.constituent(0.0789992488, "N2", side=1)
                draw.constituent(0.0805114007, "M2", side=3)
                draw.constituent(0.0833333333, "S2", side=1)
            }
            draw.constituents()
            n <- length(x$data$eta)
            n.cum.spec <- length(s$spec)
            cum.spec <- sqrt(cumsum(s$spec) / n.cum.spec)
            e <- eta.m - mean(eta.m)
            oce.screen(4)
            ##par(mar=c(4,5,1,1)+0.1)
            plot(s$freq,cum.spec,
                 xlab="Frequency [ cph ]",
                 ylab=expression(paste(integral(Gamma,0,f)," df [m]")),
                 type='l',xlim=c(0,0.1))
            grid()
            draw.constituents()
        }
##        par(oldpar)
    }
    if (!is.null(focus.time) && close.screens) oce.close.screen(all.screens=TRUE)
}


read.sealevel <- function(file, tz=getOption("oce.tz"), log.action, debug=FALSE)
{
    ## Read sea-level data in format described at ftp://ilikai.soest.hawaii.edu/rqds/hourly.fmt
    filename <- file
    if (is.character(file)) {
        file <- file(file, "r")
        on.exit(close(file))
    }
    if (!inherits(file, "connection")) {
        stop("argument `file' must be a character string or connection")
    }
    if (!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }
    first.line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
    header <- first.line
    pushBack(first.line, file)
    station.number <- NA
    station.version <- NA
    station.name <- NULL
    region <- NULL
    year <- NA
    latitude <- NA
    longitude <- NA
    GMT.offset <- NA
    decimation.method <- NA
    reference.offset <- NA
    reference.code <- NA
    if (substr(first.line, 1, 12) == "Station_Name") { # type 2
        if(debug) cat("File is of format 1 (e.g. as in MEDS archives)\n")
                                        # Station_Name,HALIFAX
                                        # Station_Number,490
                                        # Latitude_Decimal_Degrees,44.666667
                                        # Longitude_Decimal_Degrees,63.583333
                                        # Datum,CD
                                        # Time_Zone,AST
                                        # SLEV=Observed Water Level
                                        # Obs_date,SLEV
                                        # 01/01/2001 12:00 AM,1.82,
        header.length <- 8
        header <- readLines(file, n = header.length)
        station.name   <- strsplit(header[1], ",")[[1]][2]
        station.number <- as.numeric(strsplit(header[2], ",")[[1]][2])
        latitude       <- as.numeric(strsplit(header[3], ",")[[1]][2])
        longitude      <- as.numeric(strsplit(header[4], ",")[[1]][2])
                                        # get GMT offset
        tz             <- strsplit(header[6], ",")[[1]][2]
        GMT.offset     <- GMT.offset.from.tz(tz)
        x <- read.csv(file, skip=header.length, header=FALSE)
        eta <- as.numeric(x$V2)
        ## t <- as.POSIXct(strptime(as.character(x$V1), "%d/%m/%Y %I:%M %p"), tz=tz) # fails, for some reason
        t <- as.POSIXct(strptime(as.character(x$V1), "%d/%m/%Y %I:%M %p"), tz="UTC")
        t <- t + 3600 * GMT.offset
    } else { # type 1
        if(debug) cat("File is of type 2 (e.g. as in the Hawaii archives)\n")
        d <- readLines(file)
        n <- length(d)
        header <- d[1]
        station.number    <- substr(header,  1,  3)
        station.version   <- substr(header,  4,  4)
        station.name      <- substr(header,  6, 23)
        station.name      <- sub("[ ]*$","",station.name)
        region            <- substr(header, 25, 43)
        region            <- sub("[ ]*$","",region)
        year              <- substr(header, 45, 48)
        latitude.str      <- substr(header, 50, 55) #degrees,minutes,tenths,hemisphere
        latitude <- as.numeric(substr(latitude.str,   1, 2)) + (as.numeric(substr(latitude.str,  3, 5)))/600
        if (tolower(substr(latitude.str,  6, 6)) == "s") latitude <- -latitude
        longitude.str     <- substr(header, 57, 63) #degrees,minutes,tenths,hemisphere
        longitude <- as.numeric(substr(longitude.str, 1, 3)) + (as.numeric(substr(longitude.str, 4, 6)))/600
        if (tolower(substr(longitude.str, 7, 7)) == "w") longitude <- -longitude
        GMT.offset        <- substr(header, 65, 68) #hours,tenths (East is +ve)
        if (debug) cat("GMT.offset:", GMT.offset, "\n")
        decimation.method <- substr(header, 70, 70) #1=filtered 2=average 3=spot readings 4=other
        reference.offset  <- substr(header, 72, 76) # add to values
        reference.code    <- substr(header, 77, 77) # add to values
        units             <- substr(header, 79, 80)
        if (tolower(units) != "mm") stop("require units to be 'mm' or 'MM', not '", units, "'")
        eta <- array(NA, 12*(n-1))
        first.twelve.hours  <- 3600 * (0:11)
        second.twelve.hours <- 3600 * (12:23)
        twelve <- seq(1, 12, 1)
        for (i in 2:n) {
            sp <- strsplit(d[i],"[ ]+")[[1]]
            target.index <- 12 * (i-2) + twelve
            eta[target.index] <- as.numeric(sp[4:15])
            day.portion <- as.numeric(substr(sp[3], 9, 9))
            if (i == 2) {
                if (missing(tz))
                    start.day <- as.POSIXct(strptime(paste(substr(sp[3],1,8),"00:00:00"), "%Y%m%d"))
                else
                    start.day <- as.POSIXct(strptime(paste(substr(sp[3],1,8),"00:00:00"), "%Y%m%d"), tz=tz)
            } else {
                if (day.portion == 1) {
                    if (last.day.portion != 2)
                        stop("non-alternating day portions on data line ", i)
                } else if (day.portion == 2) {
                    if (last.day.portion != 1)
                        stop("non-alternating day portions on data line ", i)
                } else {
                    stop("day portion is ", day.portion, " but must be 1 or 2, on data line", i)
                }
            }
            last.day.portion <- day.portion
        }
        if (missing(tz))
            t <- as.POSIXct(start.day + 3600 * (seq(0, 12*(n-1)-1)))
        else
            t <- as.POSIXct(start.day + 3600 * (seq(0, 12*(n-1)-1)), tz=tz)
        eta[eta==9999] <- NA
        if (tolower(units) == "mm") {
            eta <- eta / 1000
        } else {
            stop("require units to be MM")
        }
    }
    num.missing <- sum(is.na(eta))
    if (debug) {
        cat("t summary:");print(summary(t))
        cat("eta summary:");print(summary(eta))
    }
    if (num.missing > 0) warning("there are ", num.missing, " missing points in this timeseries, at indices ", paste(which(is.na(eta)), ""))
    data <- data.frame(t=t, eta=eta)
    metadata <- list(
                     header=header,
                     year=year,
                     station.number=station.number,
                     station.version=station.version,
                     station.name=station.name,
                     region=region,
                     latitude=latitude,
                     longitude=longitude,
                     GMT.offset=GMT.offset,
                     decimation.method=decimation.method,
                     reference.offset=reference.offset,
                     reference.code=reference.code,
                     units=NA,
                     n=length(t),
                     deltat=as.numeric(difftime(t[2], t[1], units="hours")))

    if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processing.log.item(log.action)
    rval <- list(data=data, metadata=metadata, processing.log=log.item)
    class(rval) <- c("sealevel", "oce")
    rval
}

summary.sealevel <- function(object, ...)
{
    if (!inherits(object, "sealevel")) stop("method is only for sealevel objects")
    fives <- matrix(nrow=1, ncol=5)
    res <- list(number=object$metadata$station.number,
                version=if (is.null(object$metadata$version)) "?" else object$metadata$version,
                name=object$metadata$station.name,
                region=if (is.null(object$metadata$region)) "?" else object$metadata$region,
                latitude=object$metadata$latitude,
                longitude=object$metadata$longitude,
                number=object$metadata$n,
                nonmissing=sum(!is.na(object$data$eta)),
                deltat=object$metadata$deltat,
                start.time=min(object$data$t, na.rm=TRUE),
                end.time=max(object$data$t, na.rm=TRUE),
                gmt.offset=if (is.na(object$metadata$GMT.offset)) "?" else object$metadata$GMT.offset,
                fives=fives,
                processing.log=processing.log.summary(object))
    fives[1,] <- fivenum(object$data$eta, na.rm=TRUE)
    rownames(fives) <- "Sea level"
    colnames(fives) <- c("Min.", "1st Qu.", "Median", "3rd Qu.", "Max.")
    res$fives <- fives
    class(res) <- "summary.sealevel"
    res
}

print.summary.sealevel <- function(x, digits=max(6, getOption("digits") - 1), ...)
{
    cat("\nStation\n")
    cat("  number:              ", x$number, "\n")
    cat("  version:             ", x$version, "\n")
    cat("  name:                ", x$name, "\n")
    cat("  region:              ", x$region, "\n")
    cat("  location:            ", latlon.format(x$latitude, x$longitude, digits=digits), "\n")
    cat("Data\n")
    cat(paste("  number observations: ", x$number, "\n"))
    cat(paste("     \"   non-missing:  ",x$nonmissing, "\n"))
    cat(paste("  sampling delta-t:    ", x$deltat, "hour\n"))
    cat(paste("  series start time:   ", x$start.time, "\n"))
    cat(paste("     \"     end time:   ",x$end.time, "\n"))
    cat(paste("  GMT offset:          ", x$gmt.offset, "\n"))
    cat("Statistics:\n")
    print(x$fives, digits=digits)
    print(x$processing.log)
    cat("\n")
    invisible(x)
}
