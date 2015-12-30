setMethod(f="initialize",
          signature="sealevel",
          definition=function(.Object, elevation, time) {
              if (!missing(elevation))
                  .Object@data$elevation <- elevation
              if (!missing(time))
                  .Object@data$time <- time 
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'sealevel' object"
              return(.Object)
          })


setMethod(f="summary",
          signature="sealevel",
          definition=function(object, ...) {
              cat("Sealevel Summary\n----------------\n\n")
              showMetadataItem(object, "stationNumber",  "number:              ")
              showMetadataItem(object, "version", "version:             ")
              showMetadataItem(object, "stationName",    "name:                ")
              showMetadataItem(object, "region",  "region:              ")
              showMetadataItem(object, "deltat",  "sampling delta-t:    ")
              cat("* Location:           ",       latlonFormat(object@metadata$latitude,
                                                               object@metadata$longitude,
                                                               digits=5), "\n")
              showMetadataItem(object, "year",    "year:                ")
              ndata <- length(object@data$elevation)
              cat("* number of observations:  ", ndata, "\n")
              cat("*    \"      non-missing:   ", sum(!is.na(object@data$elevation)), "\n")
              callNextMethod()
          })

setMethod(f="subset",
          signature="sealevel",
          definition=function(x, subset, ...) {
              res <- new("sealevel")
              res@metadata <- x@metadata
              res@processingLog <- x@processingLog
              for (i in seq_along(x@data)) {
                  r <- eval(substitute(subset), x@data, parent.frame(2))
                  r <- r & !is.na(r)
                  res@data[[i]] <- x@data[[i]][r]
              }
              names(res@data) <- names(x@data)
              subsetString <- paste(deparse(substitute(subset)), collapse=" ")
              res@processingLog <- processingLogAppend(res@processingLog, paste("subset.sealevel(x, subset=", subsetString, ")", sep=""))
              res
          })
 

setMethod(f="[[",
          signature(x="sealevel", i="ANY", j="ANY"),
          definition=function(x, i, j, drop) { # FIXME: use j for e.g. times
              if (i %in% names(x@metadata)) return(x@metadata[[i]])
              else if (i %in% names(x@data)) return(x@data[[i]])
              else return(as(x, "oce")[[i]])
          })

setMethod(f="[[<-",
          signature="sealevel",
          definition=function(x, i, j, value) { # FIXME: use j for e.g. times
              if (i %in% names(x@metadata)) x@metadata[[i]] <- value
              else if (i %in% names(x@data)) x@data[[i]] <- value
              else stop("there is no item named \"", i, "\" in this sealevel object")
              validObject(x)
              invisible(x)
          })

setValidity("sealevel",
            function(object) {
                ndata <- length(object@data)
                lengths <- vector("numeric", ndata)
                for (i in 1:ndata)
                    lengths[i] <- length(object@data[[i]])
                if (var(lengths) != 0) {
                    cat("lengths of data elements are unequal\n")
                    return(FALSE)
                } else
                    return(TRUE)
            })


as.sealevel <- function(elevation,
                        time,
                        header=NULL,
                        stationNumber=NA,
                        stationVersion=NA,
                        stationName=NULL,
                        region=NULL,
                        year=NA,
                        longitude=NA, latitude=NA,
                        GMTOffset=NA,
                        decimationMethod=NA,
                        referenceOffset=NA,
                        referenceCode=NA,
                        deltat)
{
    if (missing(elevation))
        stop("must supply sealevel height, elevation, in metres")
    res <- new('sealevel')
    n <- length(elevation)
    if (missing(time)) {              # construct hourly from time "zero"
        start <- as.POSIXct("0000-01-01 00:00:00", tz="UTC")
        time <- as.POSIXct(start + seq(0, n - 1, 1) * 3600, tz="UTC")
        if (is.na(GMTOffset))
            GMTOffset <- 0 # FIXME: do I want to do this?
    } else {
        time <- as.POSIXct(time, tz="UTC")
    }
    if (missing(deltat))
        deltat <- as.numeric(difftime(time[2], time[1], units="hours"))
    if (is.na(deltat) | deltat <= 0)
        deltat <- 1
    res@metadata$filename <- ""
    res@metadata$header <- header
    res@metadata$year <- year
    res@metadata$stationNumber <- stationNumber
    res@metadata$stationVersion <- stationVersion
    res@metadata$stationName <- stationName
    res@metadata$region <- region
    res@metadata$latitude <- latitude
    res@metadata$longitude <- longitude
    res@metadata$GMTOffset <- GMTOffset
    res@metadata$decimationMethod <- decimationMethod
    res@metadata$referenceOffset <- referenceOffset
    res@metadata$referenceCode <- referenceCode
    res@metadata$units <- list()
    res@metadata$n <- length(t)
    res@metadata$deltat <- deltat
    res@data$elevation <- elevation
    res@data$time <- time
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()),sep="",collapse=""))
    res
}

setMethod(f="plot",
          signature=signature("sealevel"),
          definition=function(x, which=1:3,
                              adorn=NULL,
                              drawTimeRange=getOption("oceDrawTimeRange"),
                              mgp=getOption("oceMgp"),
                              mar=c(mgp[1]+0.5, mgp[1]+1.5, mgp[2]+1, mgp[2]+3/4),
                              marginsAsImage=FALSE,
                              debug=getOption("oceDebug"),
                              ...)
          {
              oceDebug(debug, "plot.sealevel(..., mar=c(", paste(mar, collapse=", "), "), ...) {\n",sep="", unindent=1)
              dots <- list(...)
              titlePlot <- function(x)
              {
                  title <- ""
                  if (!is.null(x@metadata$stationNumber) || !is.null(x@metadata$stationName) || !is.null(x@metadata$region))
                      title <- paste(title, gettext("Station ", domain="R-oce"),
                                     if (!is.na(x@metadata$stationNumber)) x@metadata$stationNumber else "",
                                     " ",
                                     if (!is.null(x@metadata$stationName)) x@metadata$stationName else "",
                                     " ",
                                     if (!is.null(x@metadata$region)) x@metadata$region else "",
                                     sep="")
                  if (!is.na(x@metadata$latitude) && !is.na(x@metadata$longitude))
                      title <- paste(title, latlonFormat(x@metadata$latitude, x@metadata$longitude), sep="")
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
                  stop("method is only for objects of class '", "sealevel", "'")
              opar <- par(no.readonly = TRUE)
              par(mgp=mgp, mar=mar)
              lw <- length(which)
              if (marginsAsImage) {
                  scale <- 0.7
                  w <- (1.5 + par("mgp")[2]) * par("csi") * scale * 2.54 + 0.5
                  if (lw > 1)
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
              num.NA <- sum(is.na(x@data$elevation))

              par(mgp=mgp)
              ##par(mar=c(mgp[1],mgp[1]+2.5,mgp[2]+0.5,mgp[2]+1))
              par(mar=mar)
              MSL <- mean(x@data$elevation, na.rm=TRUE)
              if ("xlim" %in% names(dots)) {
                  xtmp <- subset(x@data$elevation, dots$xlim[1] <= x@data$time & x@data$time <= dots$xlim[2])
                  tmp <- max(abs(range(xtmp-MSL,na.rm=TRUE)))
              } else {
                  tmp <- max(abs(range(x@data$elevation-MSL,na.rm=TRUE)))
              }
              ylim <- c(-tmp,tmp)
              oceDebug(debug, "ylim=", ylim, "\n")
              n <- length(x@data$elevation) # do not trust value in metadata

              oceDebug(debug, "which:", which, "\n")
              which2 <- oce.pmatch(which, list(all=1, month=2, spectrum=3, cumulativespectrum=4))
              oceDebug(debug, "which2:", which2, "\n")

              for (w in 1:length(which2)) {
                  oceDebug(debug, "plotting for code which2[", w, "] = ", which2[w], "\n", sep="")
                  if (which2[w] == 1) {
                      plot(x@data$time, x@data$elevation-MSL,
                           xlab="",
                           ylab=resizableLabel("elevation"),
                           type='l', ylim=ylim, xaxs="i",
                           lwd=0.5, axes=FALSE, ...)
                      tics <- oce.axis.POSIXct(1, x@data$time, drawTimeRange=drawTimeRange, cex.axis=1, debug=debug-1)
                      box()
                      titlePlot(x)
                      yax <- axis(2)
                      abline(h=yax, col="darkgray", lty="dotted")
                      abline(v=tics, col="darkgray", lty="dotted")
                      abline(h=0,col="darkgreen")
                      mtext(side=4,text=sprintf("%.2f m",MSL),col="darkgreen", cex=2/3)
                  } else if (which2[w] == 2) {     # sample month 
                      from <- trunc(x@data$time[1], "day")
                      to <- from + 28 * 86400 # 28 days
                      look <- from <= x@data$time & x@data$time <= to
                      xx <- x
                      for(i in seq_along(x@data)) {
                          xx@data[[i]] <- x@data[[i]][look]
                      }
                      atWeek <- seq(from=from, to=to, by="week")
                      atDay  <- seq(from=from, to=to, by="day")
                      tmp <- (pretty(max(xx@data$elevation-MSL,na.rm=TRUE) -
                                     min(xx@data$elevation-MSL,na.rm=TRUE))/2)[2]
                      ylim <- c(-tmp,tmp)
                      plot(xx@data$time, xx@data$elevation - MSL,
                           xlab="",
                           ylab=resizableLabel("elevation"),
                           type='l',ylim=ylim, xaxs="i",
                           axes=FALSE)
                      oce.axis.POSIXct(1, xx@data$time, drawTimeRange=drawTimeRange, cex.axis=1, debug=debug-1)
                      yax <- axis(2)
                      abline(h=yax, col="lightgray", lty="dotted")
                      box()
                      abline(v=atWeek, col="darkgray", lty="dotted")
                      abline(v=atDay, col="lightgray", lty="dotted")
                      abline(h=0,col="darkgreen")
                      mtext(side=4,text=sprintf("%.2f m",MSL),col="darkgreen", cex=2/3)
                  } else if (which2[w] == 3) {
                      if (num.NA == 0) {
                          Elevation <- ts(x@data$elevation, start=1, deltat=x@metadata$deltat)
                          ##s <- spectrum(Elevation-mean(Elevation),spans=c(5,3),plot=FALSE,log="y",demean=TRUE,detrend=TRUE)
                          s <- spectrum(Elevation-mean(Elevation),plot=FALSE,log="y",demean=TRUE,detrend=TRUE)
                          par(mar=c(mgp[1]+1.25,mgp[1]+1.5,mgp[2]+0.25,mgp[2]+3/4))
                          xlim <- c(0, 0.1) # FIXME: should be able to set this
                          ylim <- range(subset(s$spec, xlim[1] <= s$freq & s$freq <= xlim[2]))
                          plot(s$freq,s$spec,xlim=xlim, ylim=ylim,
                               xlab=resizableLabel("frequency cph"),
                               ylab=resizableLabel("spectral density m2/cph"),
                               #[m^2/cph]",
                               type='l',log="y")
                          grid()
                          drawConstituents()
                      } else {
                          warning("cannot draw sealevel spectum, because the series contains missing values")
                      }
                  } else if (which2[w] == 4) {
                      if (num.NA == 0) {
                          n <- length(x@data$elevation)
                          Elevation <- ts(x@data$elevation, start=1, deltat=x@metadata$deltat)
                          s <- spectrum(Elevation-mean(Elevation),plot=FALSE,log="y",demean=TRUE,detrend=TRUE)
                          nCumSpec <- length(s$spec)
                          cumSpec <- sqrt(cumsum(s$spec) / nCumSpec)
                          ##e <- x@data$elevation - mean(x@data$elevation)
                          par(mar=c(mgp[1]+1.25,mgp[1]+2.5,mgp[2]+0.25,mgp[2]+0.25))
                          plot(s$freq, cumSpec,
                               xlab=resizableLabel("frequency cph"),
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
                      stop("unrecognized value of which: ", which[w])
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
              oceDebug(debug, "} # plot.sealevel()\n", unindent=1)
              invisible()
          })


read.sealevel <- function(file, tz=getOption("oceTz"), processingLog, debug=getOption("oceDebug"))
{
    if (!is.character(file))
        stop("'file' must be a character string")
    fileOrig <- file
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
    res <- new('sealevel')
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
        elevation <- array(NA_real_, 12*(n-1))
        ## first.twelve.hours  <- 3600 * (0:11)
        ## second.twelve.hours <- 3600 * (12:23)
        twelve <- seq(1, 12, 1)
        last.day.portion <- -1 # ignored; prevents undefined warning in code analysis
        for (i in 2:n) {
            sp <- strsplit(d[i],"[ ]+")[[1]]
            target.index <- 12 * (i-2) + twelve
            elevation[target.index] <- as.numeric(sp[4:15])
            day.portion <- as.numeric(substr(sp[3], 9, 9))
            if (i == 2) {
                start.day <- as.POSIXct(strptime(paste(substr(sp[3],1,8),"00:00:00"), "%Y%m%d"), tz=tz)
            } else {
                if (day.portion == 1) {
                    if (i > 2 && last.day.portion != 2)
                        stop("non-alternating day portions on data line ", i)
                } else if (day.portion == 2) {
                    if (i > 2 && last.day.portion != 1)
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
    res@metadata$filename <- filename
    res@metadata$header <- header
    res@metadata$year <- year
    res@metadata$stationNumber <- stationNumber
    res@metadata$stationVersion <- stationVersion
    res@metadata$stationName <- stationName
    res@metadata$region <- region
    res@metadata$latitude <- latitude
    res@metadata$longitude <- longitude
    res@metadata$GMTOffset <- GMTOffset
    res@metadata$decimationMethod <- decimationMethod
    res@metadata$referenceOffset <- referenceOffset
    res@metadata$referenceCode <- referenceCode
    res@metadata$units <- list()
    res@metadata$n <- length(time)
    res@metadata$deltat <- as.numeric(difftime(time[2], time[1], units <- "hours"))
    if (missing(processingLog))
        processingLog <- paste('read.sealevel(file="', file, '", tz="', tz, sep="", collapse="")
    res@data$elevation <- elevation
    res@data$time <- time
    res@processingLog <- processingLogAppend(res@processingLog,
                                              paste('read.sealevel(file="', fileOrig, '", tz="', tz, '")', sep="", collapse=""))
    res
}


