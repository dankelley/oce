as.sealevel <- function(elevation,
                        time,
                        header=NULL,
                        stationNumber=NA,
                        stationVersion=NA,
                        stationName=NULL,
                        region=NULL,
                        year=NA,
                        latitude=NA,
                        longitude=NA,
                        GMTOffset=NA,
                        decimationMethod=NA,
                        referenceOffset=NA,
                        referenceCode=NA,
                        deltat)
{
    if (missing(elevation))
        stop("must supply sealevel height, elevation, in metres")
    n <- length(elevation)
    if (missing(time)) {              # construct hourly from time "zero"
        start <- as.POSIXct("0000-01-01 00:00:00", tz="UTC")
        time <- as.POSIXct(start + seq(0, n - 1, 1) * 3600, tz="UTC")
        if (is.na(GMTOffset))
            GMTOffset <- 0 # FIXME: do I want to do this?
    } else {
        time <- as.POSIXct(time, tz="UTC")
    }
    data <- data.frame(time=time, elevation=elevation)
    if (missing(deltat))
        deltat <- as.numeric(difftime(time[2], time[1], units="hours"))
    if (is.na(deltat) | deltat <= 0)
        deltat <- 1
    metadata <- list(header=header,
                     year=year,
                     stationNumber=stationNumber,
                     stationVersion=stationVersion,
                     stationName=stationName,
                     region=region,
                     latitude=latitude,
                     longitude=longitude,
                     GMTOffset=GMTOffset,
                     decimationMethod=decimationMethod,
                     referenceOffset=referenceOffset,
                     referenceCode=referenceCode,
                     units=units,
                     n=length(t),
                     deltat=deltat)
    logItem <- historyItem(paste(deparse(match.call()), sep="", collapse=""))
    rval <- list(data=data, metadata=metadata, history=logItem)
    class(rval) <- c("sealevel", "oce")
    rval
}

plot.sealevel <- function(x, which=1:4,
                          adorn=NULL,
                          drawTimeRange=getOption("oceDrawTimeRange"),
                          mgp=getOption("oceMgp"),
                          mar=c(mgp[1],mgp[1]+1,1,1+par("cex")),
                          marginsAsImage=FALSE,
                          debug=getOption("oceDebug"),
                          ...)
{
    oceDebug(debug, "\bplot.sealevel(..., mar=c(", paste(mar, collapse=", "), "), ...) {\n",sep="")
    dots <- list(...)
    titlePlot <- function(x)
    {
        title <- ""
        if (!is.null(x$metadata$stationNumber) || !is.null(x$metadata$stationName) || !is.null(x$metadata$region))
            title <- paste(title, "Station ",
                           if (!is.na(x$metadata$stationNumber)) x$metadata$stationNumber else "",
                           " ",
                           if (!is.null(x$metadata$stationName)) x$metadata$stationName else "",
                           " ",
                           if (!is.null(x$metadata$region)) x$metadata$region else "",
                           sep="")
        if (!is.na(x$metadata$latitude) && !is.na(x$metadata$longitude))
            title <- paste(title, latlonFormat(x$metadata$latitude, x$metadata$longitude), sep="")
        if (nchar(title) > 0)
            mtext(side=3, title, adj=1, cex=2/3)
    }
    drawConstituent <- function(frequency=0.0805114007,label="M2",col="darkred",side=1)
    {
        abline(v=frequency, col=col)
        mtext(label, side=side, at=frequency, col=col, cex=3/4*par("cex"))
    }
    drawConstituents <- function()
    {
        drawConstituent(0.0387306544, "O1", side=1)
        ##draw.constituent(0.0416666721, "S1", side=3)
        drawConstituent(0.0417807462, "K1", side=3)
        drawConstituent(0.0789992488, "N2", side=1)
        drawConstituent(0.0805114007, "M2", side=3)
        drawConstituent(0.0833333333, "S2", side=1)
    }

    if (!inherits(x, "sealevel"))
        stop("method is only for sealevel objects")
    opar <- par(no.readonly = TRUE)
    par(mgp=mgp, mar=mar)
    lw <- length(which)
    if (marginsAsImage) {
        scale <- 0.7
        w <- (1.5 + par("mgp")[2]) * par("csi") * scale * 2.54 + 0.5
        lay <- layout(matrix(1:(2*lw), nrow=lw, byrow=TRUE), widths=rep(c(1, lcm(w)), lw))
    } else {
        if (lw > 1)
            lay <- layout(cbind(1:lw))
    }
    if (lw > 1) on.exit(par(opar))

    ## tidal constituents (in cpd):
    ## http://www.soest.hawaii.edu/oceanography/dluther/HOME/Tables/Kaw.htm
    adornLength <- length(adorn)
    if (adornLength == 1) {
        adorn <- rep(adorn, 4)
        adornLength <- 4
    }
    num.NA <- sum(is.na(x$data$elevation))

    par(mgp=mgp)
    par(mar=c(mgp[1],mgp[1]+2.5,mgp[2]+0.5,mgp[2]+1))

    MSL <- mean(x$data$elevation, na.rm=TRUE)
    if ("xlim" %in% names(dots)) {
        xtmp <- subset(x$data$elevation, dots$xlim[1] <= x$data$time & x$data$time <= dots$xlim[2])
        tmp <- max(abs(range(xtmp-MSL,na.rm=TRUE)))
    } else {
        tmp <- max(abs(range(x$data$elevation-MSL,na.rm=TRUE)))
    }
    ylim <- c(-tmp,tmp)
    oceDebug(debug, "ylim=", ylim, "\n")
    n <- length(x$data$elevation) # do not trust value in metadata
    for (w in 1:length(which)) {
        if (which[w] == 1) {
            plot(x$data$time, x$data$elevation-MSL,
                 xlab="",ylab="Elevation [m]", type='l', ylim=ylim, xaxs="i",
                 lwd=0.5, axes=FALSE, ...)
            tics <- oce.axis.POSIXct(1, x$data$time, drawTimeRange=drawTimeRange, cex.axis=1, debug=debug-1)
            box()
            titlePlot(x)
            yax <- axis(2)
            abline(h=yax, col="darkgray", lty="dotted")
            abline(v=tics, col="darkgray", lty="dotted")
            abline(h=0,col="darkgreen")
            mtext(side=4,text=sprintf("%.2f m",MSL),col="darkgreen", cex=2/3)
        } else if (which[w] == 2) {     # sample days
            from <- trunc(x$data$time[1], "day")
            to <- from + 28 * 86400 # 28 days
            xx <- subset(x, from <= time & time <= to)
            atWeek <- seq(from=from, to=to, by="week")
            atDay  <- seq(from=from, to=to, by="day")
            tmp <- (pretty(max(xx$data$elevation-MSL,na.rm=TRUE) -
                           min(xx$data$elevation-MSL,na.rm=TRUE))/2)[2]
            ylim <- c(-tmp,tmp)
            plot(xx$data$time, xx$data$elevation - MSL,
                 xlab="",ylab="Elevation [m]", type='l',ylim=ylim, xaxs="i",
                 axes=FALSE)
            oce.axis.POSIXct(1, xx$data$time, drawTimeRange=drawTimeRange, cex.axis=1, debug=debug-1)
            yax <- axis(2)
            abline(h=yax, col="lightgray", lty="dotted")
            box()
            abline(v=atWeek, col="darkgray", lty="dotted")
            abline(v=atDay, col="lightgray", lty="dotted")
            abline(h=0,col="darkgreen")
            mtext(side=4,text=sprintf("%.2f m",MSL),col="darkgreen", cex=2/3)
        } else if (which[w] == 3) {
            if (num.NA == 0) {
                Elevation <- ts(x$data$elevation, start=1, deltat=x$metadata$deltat)
                #s <- spectrum(Elevation-mean(Elevation),spans=c(5,3),plot=FALSE,log="y",demean=TRUE,detrend=TRUE)
                s <- spectrum(Elevation-mean(Elevation),plot=FALSE,log="y",demean=TRUE,detrend=TRUE)
                par(mar=c(mgp[1]+1.25,mgp[1]+2.5,mgp[2]+0.25,mgp[2]+0.25))
                plot(s$freq,s$spec,xlim=c(0,0.1),
                     xlab="",ylab=expression(paste(Gamma^2, "   [",m^2/cph,"]")),
                     type='l',log="y")
                grid()
                drawConstituents()
            } else {
                warning("cannot draw sealevel spectum, because the series contains missing values")
            }
        } else if (which[w] == 4) {
            if (num.NA == 0) {
                n <- length(x$data$elevation)
                nCumSpec <- length(s$spec)
                cumSpec <- sqrt(cumsum(s$spec) / nCumSpec)
                e <- x$data$elevation - mean(x$data$elevation)
                par(mar=c(mgp[1]+1.25,mgp[1]+2.5,mgp[2]+0.25,mgp[2]+0.25))
                plot(s$freq, cumSpec,
                     xlab="Frequency [ cph ]",
                     ylab=expression(paste(integral(Gamma,0,f)," df [m]")),
                     type='l',xlim=c(0,0.1))
                if (adornLength > 3) {
                    t <- try(eval(adorn[4]), silent=TRUE)
                    if (class(t) == "try-error") warning("cannot evaluate adorn[", 4, "]\n")
                }
                grid()
                drawConstituents()
            } else {
                warning("cannot draw sealevel spectum, because the series contains missing values")
            }
        } else {
            stop("unrecognized value of which", which[w])
        }
        if (marginsAsImage)  {
            ## blank plot, to get axis length same as for images
            omar <- par("mar")
            par(mar=c(mar[1], 1/4, mgp[2]+1/2, mgp[2]+1))
            plot(1:2, 1:2, type='n', axes=FALSE, xlab="", ylab="")
            par(mar=omar)
        }
        if (adornLength > 1) {
            t <- try(eval(adorn[w]), silent=TRUE)
            if (class(t) == "try-error")
                warning("cannot evaluate adorn[", w, "]\n")
        }
    }
    oceDebug(debug, "\b\b} # plot.sealevel()\n")
    invisible()
}


read.sealevel <- function(file, tz=getOption("oceTz"), history, debug=getOption("oceDebug"))
{
    ## Read sea-level data in format described at ftp://ilikai.soest.hawaii.edu/rqds/hourly.fmt
    filename <- fullFilename(file)
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
    firstLine <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
    header <- firstLine
    pushBack(firstLine, file)
    stationNumber <- NA
    stationVersion <- NA
    stationName <- NULL
    region <- NULL
    year <- NA
    latitude <- NA
    longitude <- NA
    GMTOffset <- NA
    decimationMethod <- NA
    referenceOffset <- NA
    referenceCode <- NA
    if (substr(firstLine, 1, 12) == "Station_Name") { # type 2
        oceDebug(debug, "File is of format 1 (e.g. as in MEDS archives)\n")
        ## Station_Name,HALIFAX
        ## Station_Number,490
        ## Latitude_Decimal_Degrees,44.666667
        ## Longitude_Decimal_Degrees,63.583333
        ## Datum,CD
        ## Time_Zone,AST
        ## SLEV=Observed Water Level
        ## Obs_date,SLEV
        ## 01/01/2001 12:00 AM,1.82,
        headerLength <- 8
        header <- readLines(file, n = headerLength)
        if (debug > 0) {
            print(header)
        }
        stationName   <- strsplit(header[1], ",")[[1]][2]
        stationNumber <- as.numeric(strsplit(header[2], ",")[[1]][2])
        latitude      <- as.numeric(strsplit(header[3], ",")[[1]][2])
        longitude     <- as.numeric(strsplit(header[4], ",")[[1]][2])
        tz            <- strsplit(header[6], ",")[[1]][2] # needed for get GMT offset
        GMTOffset     <- GMTOffsetFromTz(tz)
        x <- read.csv(file, header=FALSE, stringsAsFactors=FALSE, skip=headerLength)
        if (length(grep("[0-9]{4}/", x$V1[1])) > 0) {
            oceDebug(debug, "Date format is year/month/day hour:min with hour in range 1:24\n")
            time <- strptime(as.character(x$V1), "%Y/%m/%d %H:%M", "UTC") + 3600 * GMTOffset
        } else {
            oceDebug(debug, "Date format is day/month/year hour:min AMPM with hour in range 1:12 and AMPM indicating whether day or night\n")
            time <- strptime(as.character(x$V1), "%d/%m/%Y %I:%M %p", "UTC") + 3600 * GMTOffset
        }
        elevation <- as.numeric(x$V2)
        oceDebug(debug, "tz=", tz, "so GMTOffset=", GMTOffset,"\n",
                  "first pass has time string:", as.character(x$V1)[1], "\n",
                  "first pass has time start:", format(time[1]), " ", attr(time[1], "tzone"), "\n")
        year <- as.POSIXlt(time[1])$year + 1900
    } else { # type 1
        if(debug) cat("File is of type 2 (e.g. as in the Hawaii archives)\n")
        d <- readLines(file)
        n <- length(d)
        header <- d[1]
        stationNumber    <- substr(header,  1,  3)
        stationVersion   <- substr(header,  4,  4)
        stationName      <- substr(header,  6, 23)
        stationName      <- sub("[ ]*$","",stationName)
        region           <- substr(header, 25, 43)
        region           <- sub("[ ]*$","",region)
        year             <- substr(header, 45, 48)
        latitudeStr      <- substr(header, 50, 55) #degrees,minutes,tenths,hemisphere
        latitude <- as.numeric(substr(latitudeStr,   1, 2)) + (as.numeric(substr(latitudeStr,  3, 5)))/600
        if (tolower(substr(latitudeStr,  6, 6)) == "s") latitude <- -latitude
        longitudeStr     <- substr(header, 57, 63) #degrees,minutes,tenths,hemisphere
        longitude <- as.numeric(substr(longitudeStr, 1, 3)) + (as.numeric(substr(longitudeStr, 4, 6)))/600
        if (tolower(substr(longitudeStr, 7, 7)) == "w") longitude <- -longitude
        GMTOffset        <- substr(header, 65, 68) #hours,tenths (East is +ve)
        oceDebug(debug, "GMTOffset=", GMTOffset, "\n")
        decimationMethod <- substr(header, 70, 70) #1=filtered 2=average 3=spot readings 4=other
        referenceOffset  <- substr(header, 72, 76) # add to values
        referenceCode    <- substr(header, 77, 77) # add to values
        units            <- substr(header, 79, 80)
        oceDebug(debug, "units=", units, "\n")
        if (tolower(units) != "mm")
            stop("require units to be 'mm' or 'MM', not '", units, "'")
        elevation <- array(NA, 12*(n-1))
        first.twelve.hours  <- 3600 * (0:11)
        second.twelve.hours <- 3600 * (12:23)
        twelve <- seq(1, 12, 1)
        for (i in 2:n) {
            sp <- strsplit(d[i],"[ ]+")[[1]]
            target.index <- 12 * (i-2) + twelve
            elevation[target.index] <- as.numeric(sp[4:15])
            day.portion <- as.numeric(substr(sp[3], 9, 9))
            if (i == 2) {
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
        time <- as.POSIXct(start.day + 3600 * (seq(0, 12*(n-1)-1)), tz=tz)
        elevation[elevation==9999] <- NA
        if (tolower(units) == "mm") {
            elevation <- elevation / 1000
        } else {
            stop("require units to be MM")
        }
    }
    num.missing <- sum(is.na(elevation))
    if (num.missing > 0) warning("there are ", num.missing, " missing points in this timeseries, at indices ", paste(which(is.na(elevation)), ""))
    data <- data.frame(time=time, elevation=elevation)
    metadata <- list(header=header,
                     year=year,
                     stationNumber=stationNumber,
                     stationVersion=stationVersion,
                     stationName=stationName,
                     region=region,
                     latitude=latitude,
                     longitude=longitude,
                     GMTOffset=GMTOffset,
                     decimationMethod=decimationMethod,
                     referenceOffset=referenceOffset,
                     referenceCode=referenceCode,
                     units=NA,
                     n=length(time),
                     deltat=as.numeric(difftime(time[2], time[1], units="hours")))
    if (missing(history))
        history <- paste(deparse(match.call()), sep="", collapse="")
    rval <- list(data=data, metadata=metadata, history=historyItem(history))
    class(rval) <- c("sealevel", "oce")
    rval
}

summary.sealevel <- function(object, ...)
{
    if (!inherits(object, "sealevel"))
        stop("method is only for sealevel objects")
    fives <- matrix(nrow=1, ncol=5)
    res <- list(number=object$metadata$stationNumber,
                version=if (is.null(object$metadata$version)) "?" else object$metadata$version,
                name=object$metadata$stationName,
                region=if (is.null(object$metadata$region)) "?" else object$metadata$region,
                latitude=object$metadata$latitude,
                longitude=object$metadata$longitude,
                number=object$metadata$n,
                nonmissing=sum(!is.na(object$data$elevation)),
                deltat=object$metadata$deltat,
                year=object$metadata$year,
                startTime=min(object$data$time, na.rm=TRUE),
                endTime=max(object$data$time, na.rm=TRUE),
                gmtOffset=if (is.na(object$metadata$GMTOffset)) "?" else object$metadata$GMTOffset,
                fives=fives,
                history=object$history)
    fives[1,] <- fivenum(object$data$elevation, na.rm=TRUE)
    rownames(fives) <- "Sea level"
    colnames(fives) <- c("Min.", "1st Qu.", "Median", "3rd Qu.", "Max.")
    res$fives <- fives
    class(res) <- "summary.sealevel"
    res
}

print.summary.sealevel <- function(x, digits=max(6, getOption("digits") - 1), ...)
{
    cat("Sealevel Summary\n----------------\n\n", ...)
    cat("* number:              ", x$number, "\n", ...)
    cat("* version:             ", x$version, "\n", ...)
    cat("* name:                ", x$name, "\n", ...)
    cat("* region:              ", x$region, "\n", ...)
    cat("* location:            ", latlonFormat(x$latitude, x$longitude, digits=digits), "\n", ...)
    cat("* year:                ", x$year, "\n", ...)
    cat(paste("* number observations: ", x$number, "\n"), ...)
    cat(paste("*    \"   non-missing:  ",x$nonmissing, "\n"), ...)
    cat(paste("* sampling delta-t:    ", x$deltat, "hour\n"), ...)
    cat(paste("* series startTime:   ", x$startTime, "\n"), ...)
    cat(paste("*    \"     endTime:   ",x$endTime, "\n"), ...)
    cat(paste("* GMT offset:          ", if (is.null(x$GMTOffset)) "unknown" else x$GMTOffset, "\n", sep=""), ...)
    cat("* Statistics::\n", ...)
    cat(showFives(x, indent='     '), ...)
    print(summary(x$history))
    invisible(x)
}
