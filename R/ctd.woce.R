## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Translate WOCE Data Names to Oce Data Names
#'
#' Translate WOCE-style names to \code{oce} names, using \code{\link{gsub}}
#' to match patterns. For example, the pattern \code{"CTDOXY.*"} is taken
#' to mean \code{oxygen}.
#'
#' @param names vector of strings holding WOCE-style names.
#'
#' @return vector of strings holding \code{oce}-style names.
#' @author Dan Kelley
#' @family things related to \code{ctd} data
#' @family functions that interpret variable names and units from headers
woceNames2oceNames <- function(names)
{
    ## FIXME: this almost certainly needs a lot more translations. The next comment lists some that
    ## FIXME: I've seen. But how are we to know, definitively? It would be great to find an official
    ## FIXME: list, partly because the present function should be documented, and that documentation
    ## FIXME: should list a source.
    ## SAMPNO,BTLNBR,BTLNBR_FLAG_W,DATE,TIME,LATITUDE,LONGITUDE,DEPTH,CTDPRS,CTDTMP,CTDSAL,CTDSAL_FLAG_W,SALNTY,SALNTY_FLAG_W,OXYGEN,OXYGEN_FLAG_W,SILCAT,SILCAT_FLAG_W,NITRIT,NITRIT_FLAG_W,NO2+NO3,NO2+NO3_FLAG_W,PHSPHT,PHSPHT_FLAG_W
    names <- gsub("_FLAG_W", "Flag", names)
    names <- gsub("CTDOXY", "oxygen", names)
    names <- gsub("CTDPRS", "pressure", names)
    names <- gsub("CTDSAL", "salinity", names)
    names <- gsub("CTDTMP", "temperature", names)
    names <- gsub("OXYGEN", "oxygen", names)
    names <- gsub("SALNTY", "salinityBottle", names)
    names <- gsub("SILCAT", "silicate", names)
    names <- gsub("NITRIT", "nitrite", names)
    names <- gsub("NO2+NO3", "nitrite+nitrate", names)
    names <- gsub("PHSPHT", "phosphate", names)
    names
}

#' Read a WOCE-type CTD file with First Word "CTD"
#' @template readCtdTemplate
#'
#' @details
#' \code{read.ctd.woce()} reads files stored in the exchange format used
#' by the World Ocean Circulation Experiment (WOCE), in which the first 4
#' characters are ``\code{CTD,}''. It also also in a rarer format with
#' the first 3 characters are \code{CTD}'' followed by a blank or the end
#' of the line.
#'
#' @references
#' The WOCE-exchange format is described at
#' \code{http://woce.nodc.noaa.gov/woce_v3/wocedata_1/whp/exchange/exchange_format_desc.htm},
#' and a sample file is at
#' \url{http://woce.nodc.noaa.gov/woce_v3/wocedata_1/whp/exchange/example_ct1.csv}
read.ctd.woce <- function(file, columns=NULL, station=NULL, missingValue, monitor=FALSE,
                          debug=getOption("oceDebug"), processingLog, ...)
{
    if (length(grep("\\*", file))) {
        oceDebug(debug, "read.ctd.woce(file=\"", file, "\") { # will read a series of files\n", unindent=1)
        files <- list.files(pattern=file)
        nfiles <- length(files)
        if (monitor)
            pb <- txtProgressBar(1, nfiles, style=3)
        res <- vector("list", nfiles)
        for (i in 1:nfiles) {
            res[[i]] <- read.ctd.woce(files[i], debug=debug-1)
            if (monitor)
                setTxtProgressBar(pb, i)
        }
        oceDebug(debug, "} # read.ctd.woce() {\n")
        return(res)
    }
    ## FIXME: should have an argument that selects CTDSAL or SALNTY
    oceDebug(debug, "read.ctd.woce(file=\"", file, "\", ..., debug=", debug, ", ...) {\n", sep="", unindent=1)
    if (is.character(file)) {
        filename <- fullFilename(file)
        file <- file(file, "r")
        on.exit(close(file))
    } else {
        filename <- ""
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }
    res <- new("ctd", pressureType="sea")
    ## Header
    scientist <- ship <- institute <- address <- NULL
    filename.orig <- NULL
    sampleInterval <- NaN
    systemUploadTime <- NULL
    latitude <- longitude <- NaN
    startTime <- NULL
    waterDepth <- NA
    date <- recovery <- NULL
    header <- c()
    ##col.names.inferred <- NULL
    ##conductivity.standard <- 4.2914
    ## http://www.nodc.noaa.gov/woce_V2/disk02/exchange/exchange_format_desc.htm
    ## First line
    line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
    oceDebug(debug, paste("examining header line '",line,"'\n", sep=""))
    header <- line
    waterDepthWarning <- FALSE

    ## Handle a format used in a 2003 survey of the Canada Basin
    if (substr(line, 1, 3) == "CTD" && substr(line, 4, 4) != ",")  {
        oceDebug(debug, "WOCE-like style used in a 2003 survey of the Arctic Canada Basin\n")
        ##CTD
        ##CRUISE NAME = LSSL 2003-21
        ##AREA = Arctic Ocean, Canada Basin
        ##SHIP = CCGS Louis S St.Laurent
        ##CASTNO = 1
        ##DATE = 11-Aug-2003
        ##LATITUDE (N)= 71.391
        ##LONGITUDE (W)= 134.001
        ##Pressure,Temperature,Salinity,Oxygen,Fluorescence,Transmission
        ##   DB   ,ITS-90 DEGC,   PSU  , ML/L ,     UG/L   ,      %
        ##         1,   -1.1999,   28.4279,      8.77,     0.026,    87.679
        lines <- readLines(file)
        oceDebug(debug, "file has", length(lines), "lines\n")
        headerEnd <- grep("[ ]*DB[ ]*,", lines)
        if (is.na(headerEnd))
            stop("cannot decode the header in this CTD file")
        header <- lines[1:headerEnd]
        oceDebug(debug, "headerEnd:", headerEnd, "\n")
        dataNamesOriginal <- as.list(gsub(" *$", "", strsplit(header[headerEnd-1], ",")[[1]]))
        names <- tolower(dataNamesOriginal)
        names(dataNamesOriginal) <- names
        unitsOriginal <- gsub(" *$", "", gsub("^ *", "", strsplit(header[headerEnd],",")[[1]]))
        ## FIXME: decode to real units
        units <- list()
        for (i in seq_along(names)) {
            ##message("'", unitsOriginal[i], "'")
            if (unitsOriginal[i] == "DB")
                units[[names[i]]] <- list(unit=expression(db), scale="")
            else if (unitsOriginal[i] == "ITS-90 DEGC")
                units[[names[i]]] <- list(unit=expression(degree*C), scale="ITS-90")
            else if (unitsOriginal[i] == "IPTS-68 DEGC")
                units[[names[i]]] <- list(unit=expression(degree*C), scale="IPTS-68")
            else if (unitsOriginal[i] == "PSU")
                units[[names[i]]] <- list(unit=expression(), scale="PSS-78")
            else if (unitsOriginal[i] == "ML/L")
                units[[names[i]]] <- list(unit=expression(ml/l), scale="")
            else if (unitsOriginal[i] == "UG/L")
                units[[names[i]]] <- list(unit=expression(mu*g/l), scale="")
            else if (unitsOriginal[i] == "%")
                units[[names[i]]] <- list(unit=expression(percent), scale="")
            else
                units[[names[i]]] <- list(unit=expression(), scale="")
        }
        for (i in seq_along(header)) {
            if (length(grep("CRUISE", header[i], ignore.case=TRUE))) {
                cruise<- sub("CRUISE[ ]*NAME[ ]*=[ ]*", "", header[i], ignore.case=TRUE)
                cruise <- sub("[ ]*$", "", cruise)
            } else if (length(grep("SHIP", header[i], ignore.case=TRUE))) {
                ship <- header[i]
                ship <- sub("^[ ]*SHIP[ ]*=[ ]*", "", ship, ignore.case=TRUE)
                ship <- sub(" *$", "", ship)
            } else if (length(grep("CASTNO", header[i], ignore.case=TRUE))) {
                station <- sub("[ ]*$", "", sub("CASTNO[ ]*=[ ]*", "", header[i]))
            } else if (length(grep("LATITUDE", header[i]))) {
                latitude <- as.numeric(sub("LATITUDE.*=[ ]*", "", header[i]))
                if (length(grep(".*S.*", header[i], ignore.case=TRUE)))
                    latitude <- -latitude
            } else if (length(grep("LONGITUDE", header[i]))) {
                longitude <- as.numeric(sub("LONGITUDE.*=[ ]*", "", header[i]))
                if (length(grep(".*W.*", header[i], ignore.case=TRUE)))
                    longitude <- -longitude
            } else if (length(grep("DATE", header[i]))) {
                date <- decodeTime(sub("[ ]*$", "", sub("[ ]*DATE[ ]*=[ ]*", "", header[i])), "%d-%b-%Y") # e.g. 01-Jul-2013 Canada Day
            }
        }
        dataLines <- lines[seq.int(headerEnd+1, length(lines)-1)]
        data <- as.list(read.table(textConnection(dataLines), header=FALSE, sep=",", col.names=names))
        res@metadata$header <- header
        res@metadata$filename <- filename # provided to this routine
        res@metadata$filename.orig <- filename.orig # from instrument
        res@metadata$systemUploadTime <- systemUploadTime
        res@metadata$units <- units
        ## res@metadata$units <- list(temperature=list(unit=expression(degree*C), scale="ITS-90"),
        ##                            salinity=list(unit=expression(), scale="PSS-78"),
        ##                            conductivity=list(unit=expression(), scale=""))
        res@metadata$dataNamesOriginal <- as.list(dataNamesOriginal)
        names(res@metadata$dataNamesOriginal) <- names
        res@metadata$pressureType <- "sea"
        res@metadata$ship <- ship
        res@metadata$scientist <- scientist
        res@metadata$institute <- institute
        res@metadata$address <- address
        res@metadata$cruise <- NULL
        res@metadata$station <- station
        res@metadata$deploymentType <- "unknown"
        res@metadata$date <- date
        res@metadata$startTime <- startTime
        res@metadata$latitude <- latitude
        res@metadata$longitude <- longitude
        res@metadata$recovery <- recovery
        res@metadata$waterDepth <- max(abs(data$pressure), na.rm=TRUE) # not in header
        res@metadata$sampleInterval <- sampleInterval
        ##res@metadata$names <- names
        ##res@metadata$labels <- labels
        res@metadata$src <- filename
    } else {                           # CTD, 20000718WHPOSIOSCD
        tmp <- sub("(.*), ", "", line)
        date <- substr(tmp, 1, 8)
        ##cat("DATE '", date, "'\n", sep="")
        diw <- substr(tmp, 9, nchar(tmp)) # really, divisionINSTITUTEwho
        institute <- diw # BUG: really, it is division, institute, who, strung together
        ## Kludge: recognize some institutes
        if (0 < regexpr("SIO", diw))
            institute <- "SIO"
        gotHeader <- FALSE
        while (TRUE) {
            line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE) # slow, for perhaps 20 lines of header
            oceDebug(debug, paste("examining header line '",line,"'\n"))
            if ((0 < (r<-regexpr("FILE_NAME", line)))) {
                ##  #CTDFILE_NAME:     KB51D003.WCT
                oceDebug(debug, "infer filename from:", line, "\n")
                filename.orig <- sub("^.*NAME:[ ]*", "", line)
                oceDebug(debug, " trim to '", filename.orig, "'\n", sep='')
                filename.orig <- sub("[ ]*$", "", filename.orig)
                oceDebug(debug, " trim to '", filename.orig, "'\n", sep='')
            }
            header <- c(header, line)
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
            if (!(0 < (r<-regexpr("^[ ]*#", line)[1]))) { # first non-hash line
                ## NUMBER_HEADERS = 10
                nh <- as.numeric(sub("(.*)NUMBER_HEADERS = ", "", ignore.case=TRUE, line))
                if (is.finite(nh)) {
                    for (i in 2:nh) {
                        line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
                        header <- c(header, line)
                        oceDebug(debug, line, "\n")
                        if ((0 < (r<-regexpr("LATITUDE",  line))))
                            latitude  <- as.numeric(sub("[a-zA-Z =]*","", line))
                        else if ((0 < (r<-regexpr("LONGITUDE", line))))
                            longitude <- as.numeric(sub("(.*) =","", line))
                        else if ((0 < (r<-regexpr("DATE", line))))
                            date <- decodeTime(sub(" *$", "", sub("[ ]*DATE[ ]*=[ ]*", "", line)), "%Y%m%d") # e.g. 20130701 Canada Day
                        else if ((0 < (r<-regexpr(pattern="DEPTH", text=line, ignore.case=TRUE))))
                            waterDepth <- as.numeric(sub("[a-zA-Z =:]*","", line))
                        else if ((0 < (r<-regexpr(pattern="Profondeur", text=line, ignore.case=TRUE))))
                            waterDepth <- as.numeric(sub("[a-zA-Z =]*","", line))
                        else if ((0 < (r<-regexpr(pattern="STNNBR", text=line, ignore.case=TRUE))))
                            station <- as.numeric(sub("[a-zA-Z =]*","", line))
                        else if ((0 < (r<-regexpr(pattern="Station", text=line, ignore.case=TRUE))))
                            station <- as.numeric(sub("[a-zA-Z =]*","", line))
                        else if ((0 < (r<-regexpr(pattern="Mission", text=line, ignore.case=TRUE))))
                            scientist <- sub("[ ]*$", "", sub(".*:", "", line))
                    }
                    break
                } else {
                    gotHeader <- TRUE
                    break
                }
            }
        }
        if (!gotHeader) {
            while (TRUE) {                    # catch any remaining "#" lines
                line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
                if (!(0 < (r<-regexpr("^#", line))))
                    break
                header <- c(header, line)
            }
        }
        ## 2 more header lines, one giving quantities, the next units, e.g.
        ## EXPOCODE,SECT_ID,STNNBR,CASTNO,SAMPNO,BTLNBR,BTLNBR_FLAG_W,DATE,TIME,LATITUDE,LONGITUDE,DEPTH,CTDPRS,CTDTMP,CTDSAL,CTDSAL_FLAG_W,SALNTY,SALNTY_FLAG_W,OXYGEN,OXYGEN_FLAG_W,SILCAT,SILCAT_FLAG_W,NITRIT,NITRIT_FLAG_W,NO2+NO3,NO2+NO3_FLAG_W,PHSPHT,PHSPHT_FLAG_W
        ## ,,,,,,,,,,,,DBAR,IPTS-68,PSS-78,,PSS-78,,UMOL/KG,,UMOL/KG,,UMOL/KG,,UMOL/KG,,UMOL/KG,
        varNames <- strsplit(line, split=",")[[1]]
        oceDebug(debug, "varNames: ", paste(varNames, sep=" "), "\n")
        oceDebug(debug, "oce names: ", paste(woceNames2oceNames(varNames), sep=" "), "\n")
        varNames <- gsub("^ *", "", gsub(" *$", "", varNames)) # trim whitespace

        ## catch some typos that have occured in files processed by oce
        oceDebug(debug, paste("before trying to correct typos, varNames=c(\"", paste(varNames, collapse="\", \""), "\")\n", sep=""))
        varNames <- gsub("FLAW", "FLAG", varNames) # Meteor39/4 cruise in Lab Sea had CTDSAL_FLAW_W for all 248 stations
        oceDebug(debug, paste("after trying to correct typos, varNames=c(\"", paste(varNames, collapse="\", \""), "\")\n", sep=""))
        line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE) # skip the units line
        varUnits <- strsplit(line, split=",")[[1]]
        pcol <- pmatch("CTDPRS", varNames)
        if (is.na(pcol)) {
            pcol <- pmatch("DB", varNames)
            if (is.na(pcol))
                stop("cannot find pressure column in list c(\"", paste(varNames, '","'), "\"); need 'DB' or 'CTDPRS'")
        }
        Scol <- pmatch("CTDSAL", varNames)
        if (is.na(Scol)) {
            Scol <- pmatch("SALNTY", varNames)
            if (is.na(Scol))
                stop("cannot find salinity column in list c(\"", paste(varNames, '","'), "\"); need 'CTDSAL' or 'SALNTY'")
        }
        ## FIXME: use these flags ... they are ignored at present.
        Sflagcol <- pmatch("CTDSAL_FLAG_W", varNames)
        if (is.na(Sflagcol)) {
            Sflagcol <- pmatch("SALNTY_FLAG_W", varNames)
            if (is.na(Sflagcol))
                stop("cannot find salinity-flag column in list c(\"", paste(varNames, '","'), "\"); need 'CTDSAL_FLAG_W' or 'SALNTY_FLAG_W'")
        }
        Tcol <- pmatch("CTDTMP", varNames)
        if (is.na(Tcol))
            stop("cannot find temperature column in list", paste(varNames,","))
        Ocol <- pmatch("CTDOXY", varNames)
        oceDebug(debug, "pcol=", pcol, "Scol=", Scol, "Tcol=", Tcol, "Ocol=", Ocol, "\n")
        line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
        varUnits <- strsplit(line, split=",")[[1]]
        lines <- readLines(file)
        ## nlines <- length(lines)
        ## pressure <- vector("numeric", nlines)
        ## temperature <- vector("numeric", nlines)
        ## salinity <- vector("numeric", nlines)
        ## oxygen <- vector("numeric", nlines)
        ## b <- 0
        oceDebug(debug, "pcol:", pcol, ", Scol:", Scol, ", Tcol:", Tcol, ", Ocol:", Ocol, "\n")
        ##m <- matrix(NA, nrow=nlines, ncol=length(varNames))
        ending <- grep("END_DATA", lines)
        if (length(ending) == 1)
            lines <- lines[-ending]
        varNamesOce <- woceNames2oceNames(varNames)
        ##print(data.frame(varNames, varNamesOce))
        nonflags <- grep("Flag$",varNamesOce, invert=TRUE)
        flags <- grep("Flag$",varNamesOce)
        dataAndFlags <- read.csv(text=lines, header=FALSE, col.names=woceNames2oceNames(varNames))
        data <- as.list(dataAndFlags[, nonflags])
        flags <- as.list(dataAndFlags[, flags])
        names(flags) <- gsub("Flag", "", names(flags))
        names <- names(data)
        ##labels <- titleCase(names)
        if (is.na(waterDepth)) {
            waterDepth <- max(abs(data$pressure), na.rm=TRUE)
            waterDepthWarning <- TRUE
        }
        ## catch e.g. -999 sometimes used for water depth's missing value
        if (is.finite(waterDepth) && waterDepth <= 0)
            waterDepth <- NA
        res@metadata$header <- header
        res@metadata$filename <- filename # provided to this routine
        res@metadata$filename.orig <- filename.orig # from instrument
        res@metadata$units <- list(temperature=list(unit=expression(degree*C), scale="ITS-90"),
                                   conductivity=list(unit=expression(), scale=""))
        res@metadata$flags <- flags
        res@metadata$pressureType <- "sea"
        res@metadata$systemUploadTime <- systemUploadTime
        res@metadata$ship <- ship
        res@metadata$scientist <- scientist
        res@metadata$institute <- institute
        res@metadata$address <- address
        res@metadata$cruise <- NULL
        res@metadata$station <- station
        res@metadata$deploymentType <- "unknown"
        res@metadata$date <- date
        res@metadata$startTime <- startTime
        res@metadata$latitude <- latitude
        res@metadata$longitude <- longitude
        res@metadata$recovery <- recovery
        res@metadata$waterDepth <- waterDepth
        res@metadata$sampleInterval <- sampleInterval
        ##res@metadata$names <- names
        ##res@metadata$labels <- labels
        res@metadata$src <- filename
    }
    res@data <- data
    ## replace any missingValue with NA
    if (!missing(missingValue) && !is.null(missingValue)) {
        for (item in names(data)) {
            data[[item]] <- ifelse(data[[item]]==missingValue, NA, data[[item]])
        }
    }
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    res@processingLog <- processingLogAppend(res@processingLog, processingLog)
    if (waterDepthWarning)
        res@processingLog <- processingLogAppend(res@processingLog, "inferred water depth from maximum pressure")
    oceDebug(debug, "} # read.ctd.woce()\n" , unindent=1) # FIXME: use S4 for ctd / woce
    res@metadata$units$pressure <- list(unit=expression(dbar), scale="")
    res
}

#' Read a WOCE-type CTD file with First Word "EXPOCODE"
#' @template readCtdTemplate
#'
#' @details
#' \code{read.ctd.woce.other()} reads files stored in the exchange format used
#' by the World Ocean Circulation Experiment (WOCE), in which the first
#' word in the file is \code{EXPOCODE}.
read.ctd.woce.other <- function(file, columns=NULL, station=NULL, missingValue, monitor=FALSE,
                                debug=getOption("oceDebug"), processingLog, ...)
{
    ##EXPOCODE 06MT18/1      WHP-ID A1E    DATE 090591
    ##STNNBR    558 CASTNO   1 NO.RECORDS=   83
    ##INSTRUMENT NO. NB3 SAMPLING RATE  31.25 HZ
    ##  CTDPRS  CTDTMP  CTDSAL  CTDOXY  NUMBER  QUALT1
    ##    DBAR  ITS-90  PSS-78 UMOL/KG    OBS.       *
    ## ******* ******* ******* *******               *
    ##     4.0  6.7068 34.7032   327.8      -9    2222
    ##     6.0  6.7059 34.7035   328.1      -9    2222
    ##     8.0  6.6928 34.7041   328.8      -9    2222
    examineHeaderLines <- 10
    header <- readLines(file, n=examineHeaderLines)
    station <- ""
    startTime <- NULL
    for (i in 1: examineHeaderLines) {
        if (1 == length(grep("STNNBR.*", header[i]))) {
            station <- gsub(" .*", "", gsub("STNNBR[ ]*", "", header[i]))
        } else if (1 == length(grep(".*DATE.*", header[i]))) {
            date <- gsub(" .*", "", gsub(".*DATE[ ]*", "", header[i]))
            month <- as.numeric(substr(date, 1, 2))
            day <- as.numeric(substr(date, 3, 4))
            year <- 1900 + as.numeric(substr(date, 5, 6))
            startTime <- ISOdatetime(year,month,day,0,0,0, tz="UTC")
        }
    }
    data <- read.table(file, skip=6, header=FALSE)
    pressure <- data$V1
    temperature <- data$V2
    salinity <- data$V3
    oxygen <- data$V4
    ## replace any missingValue with NA
    if (!missing(missingValue)) {
        salinity[salinity == missingValue] <- NA
        temperature[temperature == missingValue] <- NA
        pressure[pressure == missingValue] <- NA
        oxygen[oxygen == missingValue] <- NA
    }
    res <- as.ctd(salinity, temperature, pressure, station=station, startTime=startTime)
    res <- oceSetData(res, name="oxygen", value=oxygen,
                      units=expression(unit=expression(), scale=""))
    res@metadata$dataNamesOriginal <- list(pressure="CTDPRS", temperature="CTDTMP", 
                                           salinity="CTDSAL", oxygen="CTDOXY")
    res
}


