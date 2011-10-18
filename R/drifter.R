## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

summary.drifter <- function(object, ...)
{
    if (!inherits(object, "drifter"))
        stop("method is only for drifter objects")
    ndata <- length(object$data)
    threes <- matrix(nrow=ndata-1, ncol=3) # skipping time
    res <- list(time.range=range(object$data$time, na.rm=TRUE),
                threes=threes,
                processingLog=object$processingLog)
    for (i in 2:ndata)
        threes[i-1,] <- threenum(object$data[[i]])
    colnames(threes) <- c("Min.", "Mean", "Max.")
    rownames(threes) <- names(object$data)[-1]
    res$threes <- threes
    res$filename <- object$metadata$filename
    res$id <- object$metadata$id
    class(res) <- "summary.drifter"
    res
}

print.summary.drifter <- function(x, digits=max(6, getOption("digits") - 1), ...)
{
    cat("Drifter Summary\n---------------\n\n")
    cat("* source:     \"", x$filename, "\"\n", sep="")
    cat("* id:         \"", x$id, "\"\n", sep="")
    cat("* time range:", format(x$time.range[1], format="%Y-%m-%d %H:%M:%S %Z"),
        "to", format(x$time.range[2], format="%Y-%m-%d %H:%M:%S %Z"), "\n")
    cat("\n",...)
    cat("* Statistics::\n\n", ...)
    cat(showThrees(x, indent='     '), ...)
    print(summary(x$processingLog))
}

read.drifter <- function(file, debug=getOption("oceDebug"), processingLog, ...)
{
    if (missing(processingLog)) processingLog <- paste(deparse(match.call()), sep="", collapse="")
    library(ncdf)
    ofile <- file
    filename <- ""
    if (is.character(file)) {
        filename <- fullFilename(file)
        file <- ncdf::open.ncdf(file)
        on.exit(ncdf::close.ncdf(file))
    } else {
        if (!inherits(file, "connection"))
            stop("argument `file' must be a character string or connection")
        if (!isOpen(file)) {
            d <- ncdf::open.ncdf(file)
            on.exit(ncdf::close.ncdf(file))
        }
    }
    id <- ncdf::get.var.ncdf(file, "PLATFORM_NUMBER")
    id <- gsub(" *$", "", id)
    id <- gsub("^ *", "", id)
    t0s <- ncdf::get.var.ncdf(file, "REFERENCE_DATE_TIME")
    t0 <- strptime(t0s, "%Y%m%d%M%H%S", tz="UTC")
    julianDayTime <- ncdf::get.var.ncdf(file, "JULD")
    time <- t0 + julianDayTime * 86400
    longitude <- ncdf::get.var.ncdf(file, "LONGITUDE")
    longitudeNA <- ncdf::att.get.ncdf(file, "LONGITUDE","_FillValue")$value
    longitude[longitude == longitudeNA] <- NA
    latitude <- ncdf::get.var.ncdf(file, "LATITUDE")
    latitudeNA <- ncdf::att.get.ncdf(file, "LATITUDE","_FillValue")$value
    latitude[latitude == latitudeNA] <- NA
    salinity <- ncdf::get.var.ncdf(file, "PSAL")
    salinityNA <- ncdf::att.get.ncdf(file, "PSAL","_FillValue")$value
    salinity[salinity == salinityNA] <- NA
    temperature <- ncdf::get.var.ncdf(file, "TEMP")
    temperatureNA <- ncdf::att.get.ncdf(file, "TEMP","_FillValue")$value
    temperature[temperature == temperatureNA] <- NA
    pressure <- ncdf::get.var.ncdf(file, "PRES")
    pressureNA <- ncdf::att.get.ncdf(file, "PRES","_FillValue")$value
    pressure[pressure == pressureNA] <- NA
    metadata <- list(filename=filename, id=id)
    data <- list(time=time, longitude=longitude, latitude=latitude,
                 salinity=salinity, temperature=temperature, pressure=pressure)
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    hitem <- processingLogItem(processingLog)
    rval <- list(data=data, metadata=metadata, processingLog=hitem)
    class(rval) <- c("drifter", "oce")
    rval
}

as.drifter <- function(time, longitude, latitude,
                       salinity, temperature, pressure, 
                       id, filename="",
                       missingValue)
{
    if (class(time) == "data.frame") {
        df <- time 
        names <- names(df)
        time <- if ("time" %in% names) df$time else NULL
        salinity <- if ("salinity" %in% names) df$salinity else NULL
        temperature <- if ("temperature" %in% names) df$temperature else NULL
        pressure <- if ("pressure" %in% names) df$pressure else NULL
        longitude <- if ("longitude" %in% names) df$longitude else NULL
        latitude <- if ("latitude" %in% names) df$latitude else NULL
    } else {
        if (missing(time)) stop("must give time")
        if (missing(longitude)) stop("must give longitude")
        if (missing(latitude)) stop("must give latitude")
        if (missing(temperature)) stop("must give temperature")
        if (missing(salinity)) stop("must give salinity")
        if (missing(pressure)) stop("must give pressure")
    }
    data <- data.frame(time=time,
                       longitude=longitude,
                       latitude=latitude,
                       salinity=salinity,
                       temperature=temperature,
                       pressure=pressure)
    metadata <- list(filename=filename,
                     id=id)
    hitem <- processingLogItem(paste(deparse(match.call()), sep="", collapse=""))
    res <- list(data=data, metadata=metadata, processingLog=hitem)
    class(res) <- c("drifter", "oce")
    res
}

plot.drifter <- function (x, which = 1, level=1,
                          coastline,
                          cex=1,
                          pch=1,
                          type='p',
                          adorn=NULL,
                          mgp=getOption("oceMgp"),
                          mar=c(mgp[1]+1,mgp[1]+1,mgp[1]+1,mgp[1]+1),
                          debug=getOption("oceDebug"),
                          ...)
{
    if (!inherits(x, "drifter"))
        stop("method is only for drifter objects")
    oceDebug(debug, "\b\bplot.drifter() {\n")
    opar <- par(no.readonly = TRUE)
    lw <- length(which)
    if (lw > 1) on.exit(par(opar))
    if (length(type) < lw)
        type <- rep(type, lw) # FIXME: recycle more sensibly
    if (length(pch) < lw)
        pch <- rep(pch, lw) # FIXME: recycle more sensibly
    if (length(cex) < lw)
        cex <- rep(cex, lw) # FIXME: recycle more sensibly
    adorn.length <- length(adorn)
    if (adorn.length == 1) {
        adorn <- rep(adorn, lw)
        adorn.length <- lw
    }
    par(mgp=mgp, mar=mar)
    nw  <- length(which)
    if (nw > 1) {
        par(mfrow=c(nw, 1))
    }
    if (level == "all")
        level <- seq(1L, dim(x$data$temperature)[1])
    for (w in 1:nw) {
        if (which[w] == 1 || which[w] == "trajectory") {
            asp <- 1 / cos(mean(range(x$data$latitude, na.rm=TRUE)) * atan2(1,1) / 45)
            plot(x$data$longitude, x$data$latitude, asp=asp, 
                 type=type, cex=cex, pch=pch,
                 xlab="Longitude", ylab="Latitude", ...)
            if (!missing(coastline)) {
                polygon(coastline[["longitude"]], coastline[["latitude"]], col='lightgray')
                if (type == 'l')
                    lines(x$data$longitude, x$data$latitude)
                else
                    points(x$data$longitude, x$data$latitude, cex=cex, pch=pch)
            }
        } else if (which[w] == 2) {    # salinity timeseries
            if (0 != sum(!is.na(x$data$salinity))) {
                nlevels <- dim(x$data$salinity)[1]
                t <- if (length(level) > 1)
                    numberAsPOSIXct(t(matrix(rep(x$data$time, nlevels), byrow=FALSE, ncol=nlevels)))
                else
                    x$data$time
                oce.plot.ts(t, as.vector(x$data$salinity[level,]),
                            ylab=resizableLabel("S", "y"), type=type, ...)
            } else {
                warning("no non-missing salinity data")
            }
        } else if (which[w] == 3) {    # temperature timeseries
            if (0 != sum(!is.na(x$data$temperature))) {
                nlevels <- dim(x$data$temperature)[1]
                t <- if (length(level) > 1)
                    numberAsPOSIXct(t(matrix(rep(x$data$time, nlevels), byrow=FALSE, ncol=nlevels)))
                else
                    x$data$time
                oce.plot.ts(t, x$data$temperature[level,],
                            ylab=resizableLabel("T", "y"), type=type, ...)
            } else {
                warning("no non-missing temperature data")
            }
        } else if (which[w] == 4) {    # TS
            if (0 != sum(!is.na(x$data$temperature)) && 0 != sum(!is.na(x$data$salinity))) {
                plotTS(as.ctd(x$data$salinity[level,], x$data$temperature[level,], 0), ...)
            } else {
                warning("no non-missing salinity data")
            }
        } else if (which[w] == 5) {    # S profile
            ## FIXME: how to handle the noise; if as below, document it
            plot(x$data$salinity, x$data$pressure,
                 xlim=quantile(x$data$salinity, c(0.01, 0.99), na.rm=TRUE),
                 ylim=quantile(x$data$pressure, c(0.99, 0.01), na.rm=TRUE),
                 xlab=resizableLabel("S", "x"),
                 ylab=resizableLabel("p", "y"))
        } else if (which[w] == 6) {    # T profile
            ## FIXME: how to handle the noise; if as below, document it
            plot(x$data$temperature, x$data$pressure,
                 xlim=quantile(x$data$temperature, c(0.01, 0.99), na.rm=TRUE),
                 ylim=quantile(x$data$pressure, c(0.99, 0.01), na.rm=TRUE),
                 xlab=resizableLabel("T", "x"),
                 ylab=resizableLabel("p", "y"))
        } else {
            stop("invalid value of which (", which, ")")
        }
    }
    oceDebug(debug, "\b\b} # plot.drifter()\n")
    invisible()
}

