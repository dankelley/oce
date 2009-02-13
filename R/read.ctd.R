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
