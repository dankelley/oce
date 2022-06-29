# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Translate Oce Data Names to WHP Data Names
#'
#' Translate oce-style names to WOCE names, using [gsub()]
#' to match patterns. For example, the pattern `"oxygen"`
#' is taken to mean `"CTDOXY"`.
#'
#' @param names vector of strings holding oce-style names.
#'
#' @return vector of strings holding WHP-style names.
#'
#' @author Dan Kelley
#'
#' @references
#' Several online sources list WHP names. An example is
#' `https://cchdo.github.io/hdo-assets/documentation/manuals/pdf/90_1/chap4.pdf`
#'
#' @family things related to ctd data
#' @family functions that interpret variable names and units from headers
oceNames2whpNames <- function(names)
{
    ## see woceNames2oceNames, and update both at once when new items added
    ## SAMPNO,BTLNBR,BTLNBR_FLAG_W,DATE,TIME,LATITUDE,LONGITUDE,DEPTH,CTDPRS,CTDTMP,CTDSAL,CTDSAL_FLAG_W,SALNTY,SALNTY_FLAG_W,OXYGEN,OXYGEN_FLAG_W,SILCAT,SILCAT_FLAG_W,NITRIT,NITRIT_FLAG_W,NO2+NO3,NO2+NO3_FLAG_W,PHSPHT,PHSPHT_FLAG_W
    names <- gsub("Flag", "_FLAG_W", names)
    names <- gsub("totalAlkalinity", "ALKALI", names)
    names <- gsub("CFC12", "CFC-12", names)
    names <- gsub("depth", "DEPTH", names)
    names <- gsub("flag", "FLAG", names)
    names <- gsub("oxygen", "CTDOXY", names)
    names <- gsub("pressure", "CTDPRS", names)
    names <- gsub("salinity", "CTDSAL", names)
    names <- gsub("scan", "SCAN", names)
    names <- gsub("temperature", "CTDTMP", names)
    names <- gsub("fluorescence", "FLUOR", names)
    names <- gsub("ammonium", "NH4", names)
    names <- gsub("phosphate", "PHSPHT", names)
    names <- gsub("pHTotal", "PH_TOT", names)
    names <- gsub("pHTemperature","PH_TMP", names) # what is this??
    names <- gsub("nitrate", "NITRAT", names)
    names <- gsub("nitrite", "NITRIT", names)
    names <- gsub("number", "NUMBER", names)
    names <- gsub("nitrite+nitrate", "NO2+NO3", names)
    names <- gsub("oxygen", "OXYGEN", names)
    names <- gsub("quality", "QUALT", names) # flags, really, but we are not capturing that info yet
    names <- gsub("salinityBottle","SALNTY", names)
    names <- gsub("SF6", "SF6", names)
    names <- gsub("silicate", "SILCAT", names)
    names <- gsub("totalCarbon", "TCARBN", names)
    names <- gsub("transmission", "TRANS", names)
    names
}

#' Translate oce unit to WHP unit
#'
#' Translate oce units to WHP-style strings,
#' to match patterns.
#'
#' @param units vector of expressions for units in oce notation.
#'
#' @param scales vector of strings for scales in oce notation.
#'
#' @return vector of strings holding WOCE-style names.
#'
#' @author Dan Kelley
#'
#' @references
#' Several online sources list WOCE names. An example is
#' `https://cchdo.github.io/hdo-assets/documentation/manuals/pdf/90_1/chap4.pdf`
#'
#' @family things related to ctd data
#' @family functions that interpret variable names and units from headers
oceUnits2whpUnits <- function(units, scales)
{
    if (length(units) != length(scales))
        stop("lengths of units and scales must match")
    rval <- NULL
    for (i in seq_along(units)) {
        ##message("i=", i, ", units=\"", units[i], "\", scales=\"", scales[i], "\"", sep="")
        if (is.na(units[i])) {
            ##message("NA ... scales[", i, "]=\"", scales[i], "\"", sep="")
            if (scales[i] == "PSS-78")
                rval[i] <- "PSS-78" # FIXME: check for other salinity units
            else
                rval[i] <- ""
        } else if (units[i] == "") { # might be salinity
            rval[i] <- if (scales[i] != "") toupper(scales[i]) else "PSAL"
        } else if (units[i] == "dbar") {
            rval[i] <- "DBAR"
        } else if (units[i] == "degree * C") {
            rval[i] <- if (scales[i] != "") toupper(scales[i]) else "DEG C"
        } else if (units[i] == "fmol/kg") {
            rval[i] <- "FMOL/KG"
        } else if (units[i] == "m") {
            rval[i] <- "M"
        } else if (units[i] == "s") {
            rval[i] <- "S"
        } else {
            rval[i] <- i
        }
    }
    rval

    #units <- gsub("^dbar$", "DBAR", units)
    ### units <- gsub("^degree \\* C$", "DEG C", units)
    #units <- gsub("^degree \\* C$", "ITS-90", units)
    #units <- gsub("^m$", "M", units)
    #units <- gsub("^s$", "S", units)
    ###units <- gsub("^$", "PSS-78", units) # problem: lots of things have no units.
    #print(units)
    #return(units)

    ### message("woceUnit2oceUnit(\"", woceUnit, "\")", sep="")
    #if (unit == "FMOL/KG")
    #    return(list(unit=expression(fmol/kg), scale=""))
    #if (unit == "ITS-90" || unit == "ITS-90 DEGC")
    #    return(list(unit=expression(degree*C), scale="ITS-90"))
    #if (unit == "IPTS-68" || unit == "ITS-68" || unit == "ITS-68 DEGC")
    #    return(list(unit=expression(degree*C), scale="IPTS-68"))
    #if (unit == "ML/L")
    #    return(list(unit=expression(ml/l), scale=""))
    #if (unit == "PMOL/KG")
    #    return(list(unit=expression(pmol/kg), scale=""))
    #if (unit == "PSU" || unit == "PSS-78")
    #    return(list(unit=expression(), scale="PSS-78"))
    #if (unit == "UG/L")
    #    return(list(unit=expression(mu*g/l), scale=""))
    #if (unit == "UMOL/KG")
    #    return(list(unit=expression(mu*mol/kg), scale=""))
    #if (unit == "%")
    #    return(list(unit=expression(percent), scale=""))
    #return(list(unit=expression(), scale=""))
}


#' Translate WOCE Data Names to Oce Data Names
#'
#' Translate WOCE-style names to `oce` names, using [gsub()]
#' to match patterns. For example, the pattern `"CTDOXY.*"` is taken
#' to mean `oxygen`.
#'
#' @param names vector of strings holding WOCE-style names.
#'
#' @return vector of strings holding `oce`-style names.
#'
#' @author Dan Kelley
#'
#' @references
#' Several online sources list WOCE names. An example is
#' `https://cchdo.github.io/hdo-assets/documentation/manuals/pdf/90_1/chap4.pdf`
#'
#' @family things related to ctd data
#' @family functions that interpret variable names and units from headers
woceNames2oceNames <- function(names)
{
    ## see woceNames2oceNames, and update both at once when new items added
    ##
    ## FIXME: this almost certainly needs a lot more translations. The next comment lists some that
    ## FIXME: I've seen. But how are we to know, definitively? It would be great to find an official
    ## FIXME: list, partly because the present function should be documented, and that documentation
    ## FIXME: should list a source.
    ## SAMPNO,BTLNBR,BTLNBR_FLAG_W,DATE,TIME,LATITUDE,LONGITUDE,DEPTH,CTDPRS,CTDTMP,CTDSAL,CTDSAL_FLAG_W,SALNTY,SALNTY_FLAG_W,OXYGEN,OXYGEN_FLAG_W,SILCAT,SILCAT_FLAG_W,NITRIT,NITRIT_FLAG_W,NO2+NO3,NO2+NO3_FLAG_W,PHSPHT,PHSPHT_FLAG_W
    names <- gsub("_FLAG_W", "Flag", names)
    names <- gsub("_FLAG_I", "Flag", names)
    names <- gsub("ALKALI", "totalAlkalinity", names)
    names <- gsub("CFC-12", "CFC12", names)
    names <- gsub("CTDOXY", "oxygen", names)
    names <- gsub("CTDPRS", "pressure", names)
    names <- gsub("CTDSAL", "salinity", names)
    names <- gsub("CTDTMP", "temperature", names)
    names <- gsub("FLUOR", "fluorescence", names)
    names <- gsub("NH4", "ammonium", names)
    names <- gsub("PHSPHT", "phosphate", names)
    names <- gsub("PH_TOT", "pHTotal", names)
    names <- gsub("PH_TMP", "pHTemperature", names) # what is this??
    names <- gsub("NITRAT", "nitrate", names)
    names <- gsub("NITRIT", "nitrite", names)
    names <- gsub("NUMBER", "number", names)
    names <- gsub("NO2+NO3", "nitrite+nitrate", names)
    names <- gsub("OXYGEN", "oxygen", names)
    names <- gsub("QUALT", "quality", names) # flags, really, but we are not capturing that info yet
    names <- gsub("SALNTY", "salinityBottle", names)
    names <- gsub("SF6", "SF6", names)
    names <- gsub("SILCAT", "silicate", names)
    names <- gsub("TCARBN", "totalCarbon", names)
    names <- gsub("TRANS", "transmission", names)
    names
}

#' Translate WOCE units to oce units
#'
#' Translate WOCE-style units to `oce` units.
#'
#' @param woceUnit string holding a WOCE unit
#'
#' @return expression in oce unit form
#'
#' @author Dan Kelley
#'
#' @family things related to ctd data
#' @family functions that interpret variable names and units from headers
woceUnit2oceUnit <- function(woceUnit)
{
    ## message("woceUnit2oceUnit(\"", woceUnit, "\")", sep="")
    if (woceUnit == "DB" || woceUnit == "DBAR")
        return(list(unit=expression(dbar), scale=""))
    if (woceUnit == "DEG C")
        return(list(unit=expression(degree*C), scale="")) # unknown scale
    if (woceUnit == "FMOL/KG")
        return(list(unit=expression(fmol/kg), scale=""))
    if (woceUnit == "ITS-90" || woceUnit == "ITS-90 DEGC")
        return(list(unit=expression(degree*C), scale="ITS-90"))
    if (woceUnit == "IPTS-68" || woceUnit == "ITS-68" || woceUnit == "ITS-68 DEGC")
        return(list(unit=expression(degree*C), scale="IPTS-68"))
    if (woceUnit == "ML/L")
        return(list(unit=expression(ml/l), scale=""))
    if (woceUnit == "PMOL/KG")
        return(list(unit=expression(pmol/kg), scale=""))
    if (woceUnit == "PSU" || woceUnit == "PSS-78")
        return(list(unit=expression(), scale="PSS-78"))
    if (woceUnit == "UG/L")
        return(list(unit=expression(mu*g/l), scale=""))
    if (woceUnit == "UMOL/KG")
        return(list(unit=expression(mu*mol/kg), scale=""))
    if (woceUnit == "%")
        return(list(unit=expression(percent), scale=""))
    return(list(unit=expression(), scale=""))
}


#' Read a WOCE-exchange CTD File
#'
#' This reads WOCE exchange files that start with the string `"CTD"`.
#' There are two variants: one in which the first 4 characters are
#' `"CTD,"` and the other in which the first 3 characters are
#' again `"CTD"` but no other non-whitespace characters occur on
#' the line.
#'
#' @template readCtdTemplate
#'
#' @template encodingTemplate
#'
#' @references
#' The WOCE-exchange format was once described at
#' `http://woce.nodc.noaa.gov/woce_v3/wocedata_1/whp/exchange/exchange_format_desc.htm`
#' although that link is no longer working as of December 2020.
#'
#' @family functions that read ctd data
#'
#' @author Dan Kelley
read.ctd.woce <- function(file,
    columns=NULL,
    station=NULL,
    missingValue,
    deploymentType="unknown",
    monitor=FALSE,
    encoding="latin1",
    debug=getOption("oceDebug"),
    processingLog,
    ...)
{
    if (missing(file))
        stop("must supply 'file'")
    if (is.character(file)) {
        if (!file.exists(file))
            stop("cannot find file '", file, "'")
        if (0L == file.info(file)$size)
            stop("empty file '", file, "'")
    }
    if (is.character(file) && length(grep("\\*", file))) {
        oceDebug(debug, "read.ctd.woce(file=\"", file, "\") { # will read a series of files\n", unindent=1)
        files <- list.files(pattern=file)
        nfiles <- length(files)
        if (monitor)
            pb <- txtProgressBar(1, nfiles, style=3)
        res <- vector("list", nfiles)
        for (i in 1:nfiles) {
            res[[i]] <- read.ctd.woce(files[i], deploymentType=deploymentType, debug=debug-1)
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
        file <- file(file, "r", encoding=encoding)
        on.exit(close(file))
    } else {
        filename <- ""
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        open(file, "r", encoding=encoding)
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
    date <- recoveryTime <- NULL
    header <- c()
    ##col.names.inferred <- NULL
    ##conductivity.standard <- 4.2914
    ## http://www.nodc.noaa.gov/woce_V2/disk02/exchange/exchange_format_desc.htm
    ## First line
    line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
    oceDebug(debug, paste("examining header line '", line, "'\n", sep=""))
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
        lines <- readLines(file, encoding=encoding)
        oceDebug(debug, "file has", length(lines), "lines\n")
        headerEnd <- grep("[ ]*DB[ ]*,", lines)
        if (is.na(headerEnd))
            stop("cannot decode the header in this CTD file")
        header <- lines[1:headerEnd]
        oceDebug(debug, "headerEnd:", headerEnd, "\n")
        dataNamesOriginal <- as.list(gsub(" *$", "", strsplit(header[headerEnd-1], ",")[[1]]))
        names <- tolower(dataNamesOriginal)
        names(dataNamesOriginal) <- names
        unitsOriginal <- gsub(" *$", "", gsub("^ *", "", strsplit(header[headerEnd], ",")[[1]]))
        ## FIXME: decode to real units
        units <- list()
        for (i in seq_along(names)) {
            units[[names[i]]] <- woceUnit2oceUnit(unitsOriginal[i])
        }
        for (i in seq_along(header)) {
            oceDebug(debug, " header[", i, "]=\"", header[i], "\"\n", sep="")
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
        res@metadata$date <- date
        res@metadata$startTime <- startTime
        res@metadata$recoveryTime <- recoveryTime
        res@metadata$latitude <- latitude
        res@metadata$longitude <- longitude
        res@metadata$deploymentType <- deploymentType
        res@metadata$waterDepth <- max(abs(data$pressure), na.rm=TRUE) # not in header
        res@metadata$sampleInterval <- sampleInterval
        ##res@metadata$names <- names
        ##res@metadata$labels <- labels
        res@metadata$src <- filename
    } else {
        oceDebug(debug, "handling woce-exchange style, in which first line starts 'CTD,'\n")
        ## CTD,20000718WHPOSIOSCD
        tmp <- sub("(.*), ", "", line)
        date <- substr(tmp, 1, 8)
        ##cat("DATE '", date, "'\n", sep="")
        ## 20170424 diw <- substr(tmp, 9, nchar(tmp)) # really, divisionINSTITUTEwho
        ## 20170424 institute <- diw # BUG: really, it is division, institute, who, strung together
        ## 20170424 ## Kludge: recognize some institutes
        ## 20170424 if (0 < regexpr("SIO", diw))
        ## 20170424     institute <- "SIO"
        gotHeader <- FALSE
        gotDate <- FALSE
        gotTime <- FALSE
        startTime <- NULL
        while (TRUE) {
            line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE) # slow, for perhaps 20 lines of header
            oceDebug(debug, paste("examining header line '", line, "'\n"))
            if (0 < (r<-regexpr("FILE_NAME", line))) {
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
            if (!(0 < (r<-regexpr("^[ ]*#", line)[1]))) {
                ## first non-hash line
                ## NUMBER_HEADERS = 10
                nh <- as.numeric(sub("(.*)NUMBER_HEADERS = ", "", ignore.case=TRUE, line))
                if (is.finite(nh)) {
                    for (i in 2:nh) {
                        line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
                        ##message("line: ", line)
                        header <- c(header, line)
                        oceDebug(debug, line, "\n")
                        if (0 < (r<-regexpr("LATITUDE",  line)))
                            latitude  <- as.numeric(sub("[a-zA-Z =]*", "", line))
                        else if (0 < (r<-regexpr("LONGITUDE", line)))
                            longitude <- as.numeric(sub("(.*) =", "", line))
                        else if (0 < (r<-regexpr("DATE", line))) {
                            ## FIXME: check to see whether woce-exchange always gives dates as 8 digits
                            date <- sub(" *$", "", sub("[ ]*DATE[ ]*=[ ]*", "", line))
                            gotDate <- TRUE
                            ##message("got date=", date)
                        } else if (0 < (r<-regexpr(pattern="TIME", text=line, ignore.case=TRUE))) {
                            time <- sub("[a-zA-Z =]*", "", line)
                            gotTime <- TRUE
                            ##message("got time=", time)
                        } else if (0 < (r<-regexpr(pattern="DEPTH", text=line, ignore.case=TRUE)))
                            waterDepth <- as.numeric(sub("[a-zA-Z =:]*", "", line))
                        else if (0 < (r<-regexpr(pattern="Profondeur", text=line, ignore.case=TRUE)))
                            waterDepth <- as.numeric(sub("[a-zA-Z =]*", "", line))
                        else if (0 < (r<-regexpr(pattern="STNNBR", text=line, ignore.case=TRUE)))
                            station <- as.numeric(sub("[a-zA-Z =]*", "", line))
                        else if (0 < (r<-regexpr(pattern="Station", text=line, ignore.case=TRUE)))
                            station <- as.numeric(sub("[a-zA-Z =]*", "", line))
                        else if (0 < (r<-regexpr(pattern="Mission", text=line, ignore.case=TRUE)))
                            scientist <- sub("[ ]*$", "", sub(".*:", "", line))
                    }
                    break
                } else {
                    gotHeader <- TRUE
                    break
                }
            }
        }
        if (gotDate && gotTime) {
            ##message("gotDate && gotTime")
            if (nchar(time) == 3)
                time <- paste("0", time, sep="")
            startTime <- as.POSIXct(paste(date, time), format="%Y%m%d %H%M", tz="UTC")
        }
        if (!gotHeader) {
            while (TRUE) {
                ## catch any remaining "#" lines
                line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
                if (!(0 < (r<-regexpr("^#", line))))
                    break
                header <- c(header, line)
            }
        }
        ## nolint start (long lines)
        ## 2 more header lines, one giving quantities, the next units, e.g.
        ## EXPOCODE,SECT_ID,STNNBR,CASTNO,SAMPNO,BTLNBR,BTLNBR_FLAG_W,DATE,TIME,LATITUDE,LONGITUDE,DEPTH,CTDPRS,CTDTMP,CTDSAL,CTDSAL_FLAG_W,SALNTY,SALNTY_FLAG_W,OXYGEN,OXYGEN_FLAG_W,SILCAT,SILCAT_FLAG_W,NITRIT,NITRIT_FLAG_W,NO2+NO3,NO2+NO3_FLAG_W,PHSPHT,PHSPHT_FLAG_W
        ## ,,,,,,,,,,,,DBAR,IPTS-68,PSS-78,,PSS-78,,UMOL/KG,,UMOL/KG,,UMOL/KG,,UMOL/KG,,UMOL/KG,
        ## nolint end (long lines)
        dataNamesOriginal <- as.list(trimws(strsplit(line, ",")[[1]]))
        oceDebug(debug, "dataNamesOriginal: ", paste(dataNamesOriginal, sep=" "), "\n")
        dataNamesOriginalCorrected <- dataNamesOriginal

        ## catch some typos that have occured in files processed by oce
        oceDebug(debug, paste("before correcting typos, dataNamesOriginal        =c(\"",
                              paste(dataNamesOriginal, collapse="\", \""), "\")\n", sep=""))
        ## Meteor39/4 cruise in Lab Sea had CTDSAL_FLAW_W for all 248 stations
        dataNamesOriginalCorrected <- gsub("FLAW", "FLAG", dataNamesOriginalCorrected)
        oceDebug(debug, paste("after correcting typos, dataNamesOriginalCorrected=c(\"",
                              paste(dataNamesOriginalCorrected, collapse="\", \""), "\")\n", sep=""))

        names <- woceNames2oceNames(dataNamesOriginalCorrected)
        names(dataNamesOriginal) <- names
        oceDebug(debug, "names: ", paste(names, sep=" "), "\n")
        ##> varNames <- strsplit(line, split=",")[[1]]
        ##> oceDebug(debug, "varNames: ", paste(varNames, sep=" "), "\n")
        ##> oceDebug(debug, "oce names: ", paste(woceNames2oceNames(varNames), sep=" "), "\n")
        ##> varNames <- gsub("^ *", "", gsub(" *$", "", varNames)) # trim whitespace

        ## Units. Note that if this line ends in ",", then we need to tack
        ## on a final empty string to the vector named unitsOriginal.
        line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
        unitsOriginal<- strsplit(line, split=",")[[1]] # nolint (variable not used)
        if (substr(line, nchar(line), nchar(line)) == ",")
            unitsOriginal <- c(unitsOriginal, "")
        units <- list()
        for (i in seq_along(names)) {
            oceDebug(debug, "names[", i, "]='", names[i], "', unitsOriginal[", i, "]='", unitsOriginal[i], "'\n", sep="")
            units[[names[i]]] <- woceUnit2oceUnit(unitsOriginal[i])
        }

        ##20161218 pcol <- pmatch("CTDPRS", varNames)
        ##20161218 if (is.na(pcol)) {
        ##20161218     pcol <- pmatch("DB", varNames)
        ##20161218     if (is.na(pcol))
        ##20161218         stop("cannot find pressure column in list c(\"", paste(varNames, '","'), "\"); need 'DB' or 'CTDPRS'")
        ##20161218 }
        ##20161218 Scol <- pmatch("CTDSAL", varNames)
        ##20161218 if (is.na(Scol)) {
        ##20161218     Scol <- pmatch("SALNTY", varNames)
        ##20161218     if (is.na(Scol))
        ##20161218         stop("cannot find salinity column in list c(\"", paste(varNames, '","'), "\"); need 'CTDSAL' or 'SALNTY'")
        ##20161218 }
        ##20161218 ## FIXME: use these flags ... they are ignored at present.
        ##20161218 Sflagcol <- pmatch("CTDSAL_FLAG", varNames)
        ##20161218 if (is.na(Sflagcol)) {
        ##20161218     Sflagcol <- pmatch("SALNTY_FLAG", varNames)
        ##20161218     if (is.na(Sflagcol))
        ##20161218         stop("cannot find salinity-flag column in list c(\"", paste(varNames, '","'), "\"); need 'CTDSAL_FLAG...' or 'SALNTY_FLAG...'")
        ##20161218 }
        ##20161218 Tcol <- pmatch("CTDTMP", varNames)
        ##20161218 if (is.na(Tcol))
        ##20161218     stop("cannot find temperature column in list", paste(varNames, ","))
        ##20161218 Ocol <- pmatch("CTDOXY", varNames)
        ##20161218 oceDebug(debug, "pcol=", pcol, "Scol=", Scol, "Tcol=", Tcol, "Ocol=", Ocol, "\n")
        ##20161218 line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
        ##20161218 varUnits <- strsplit(line, split=",")[[1]] # nolint (variable not used)

        ## Read the data into a buffer, since there will likely be
        ## a trailer line at the end, and read.table() cannot handle that.
        #> owarn <- options('warn')$warn
        #> options(warn=-1)
        lines <- readLines(file)# , warn=FALSE)
        #> options(warn=owarn)
        nlines <- length(lines)
        if (length(grep("^END", lines[nlines])))
            lines <- lines[-nlines]
        dataAndFlags <- read.csv(text=lines, header=FALSE, col.names=names, sep=",")

        ## nlines <- length(lines)
        ## pressure <- vector("numeric", nlines)
        ## temperature <- vector("numeric", nlines)
        ## salinity <- vector("numeric", nlines)
        ## oxygen <- vector("numeric", nlines)
        ## b <- 0
        ##20161218 oceDebug(debug, "pcol:", pcol, ", Scol:", Scol, ", Tcol:", Tcol, ", Ocol:", Ocol, "\n")
        ##20161218 ##m <- matrix(NA, nrow=nlines, ncol=length(varNames))
        ##20161218 ending <- grep("END_DATA", lines)
        ##20161218 if (length(ending) == 1)
        ##20161218     lines <- lines[-ending]
        ##20161218 varNamesOce <- woceNames2oceNames(varNames)
        ##print(data.frame(varNames, varNamesOce))
        nonflags <- grep("Flag$", names, invert=TRUE)
        flags <- grep("Flag$", names)
        dataAndFlags <- read.csv(text=lines, header=FALSE, col.names=woceNames2oceNames(names))
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
        res@metadata$date <- date
        res@metadata$startTime <- startTime
        res@metadata$recoveryTime <- recoveryTime
        res@metadata$latitude <- latitude
        res@metadata$longitude <- longitude
        res@metadata$deploymentType <- deploymentType
        res@metadata$waterDepth <- waterDepth
        res@metadata$sampleInterval <- sampleInterval
        ##res@metadata$names <- names
        ##res@metadata$labels <- labels
        res@metadata$src <- filename
        ## trim units (there can be flag units in the list)
        units <- units[names(units) %in% names]
    }
    ## replace any missingValue with NA. If missingValue is not supplied, look
    ## for crazy S values, and if none are found, look for crazy T values.
    if (missing(missingValue)) {
        if ("salinity" %in% names(data) && "temperature" %in% names(data)) {
            Smin <- min(data[["salinity"]], na.rm=TRUE)
            Tmin <- min(data[["temperature"]], na.rm=TRUE)
            mv <- NULL
            if (!is.na(Smin) && Smin < -8)
                mv <- c(mv, Smin)
            if (!is.na(Tmin) && Tmin < -8)
                mv <- c(mv, Tmin)
            if (length(mv) == 1) {
                missingValue <- mv
                msg <- paste("missingValue inferred as ", missingValue, " from S or T minimum", sep="")
                warning(msg)
                res@processingLog <- processingLogAppend(res@processingLog, msg)
            } else if (length(mv) == 2) {
                if (mv[1] == mv[2]) {
                    missingValue <- mv[1]
                    msg <- paste("missingValue inferred as ", missingValue, " from S and T minima", sep="")
                    warning(msg)
                    res@processingLog <- processingLogAppend(res@processingLog, msg)
                }
            }
        }
    }
    if (!missing(missingValue) && !is.null(missingValue)) {
        for (item in names(data)) {
            data[[item]] <- ifelse(data[[item]]==missingValue, NA, data[[item]])
        }
    }
    res@data <- data
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    res@processingLog <- processingLogAppend(res@processingLog, processingLog)
    if (waterDepthWarning)
        res@processingLog <- processingLogAppend(res@processingLog, "inferred water depth from maximum pressure")
    oceDebug(debug, "} # read.ctd.woce()\n", unindent=1) # FIXME: use S4 for ctd / woce
    # res@metadata$units$pressure <- list(unit=expression(dbar), scale="")
    res@metadata$units <- units
    res@metadata$dataNamesOriginal <- dataNamesOriginal
    res
}

#' Read a WOCE-exchange EXPOCODE File
#'
#' This reads WOCE exchange files that start with the string `"EXPOCODE"`.
#'
#' @template readCtdTemplate
#'
#' @template encodingTemplate
#'
#' @family functions that read ctd data
#'
#' @author Dan Kelley
read.ctd.woce.other <- function(file,
    columns=NULL,
    station=NULL,
    missingValue,
    deploymentType="unknown",
    monitor=FALSE,
    encoding="latin1",
    debug=getOption("oceDebug"),
    processingLog,
    ...)
{
    if (missing(file))
        stop("must supply 'file'")
    if (is.character(file)) {
        if (!file.exists(file))
            stop("cannot find file '", file, "'")
        if (0L == file.info(file)$size)
            stop("empty file '", file, "'")
    }
    ##EXPOCODE 06MT18/1      WHP-ID A1E    DATE 090591
    ##STNNBR    558 CASTNO   1 NO.RECORDS=   83
    ##INSTRUMENT NO. NB3 SAMPLING RATE  31.25 HZ
    ##  CTDPRS  CTDTMP  CTDSAL  CTDOXY  NUMBER  QUALT1
    ##    DBAR  ITS-90  PSS-78 UMOL/KG    OBS.       *
    ## ******* ******* ******* *******               *
    ##     4.0  6.7068 34.7032   327.8      -9    2222
    ##     6.0  6.7059 34.7035   328.1      -9    2222
    ##     8.0  6.6928 34.7041   328.8      -9    2222
    res <- new("ctd")
    examineHeaderLines <- 10
    header <- readLines(file, n=examineHeaderLines, encoding=encoding)
    station <- ""
    for (i in 1: examineHeaderLines) {
        if (1 == length(grep("STNNBR.*", header[i]))) {
            res@metadata$station <- gsub(" .*", "", gsub("STNNBR[ ]*", "", header[i]))
        } else if (1 == length(grep(".*DATE.*", header[i]))) {
            date <- gsub(" .*", "", gsub(".*DATE[ ]*", "", header[i]))
            month <- as.numeric(substr(date, 1, 2))
            day <- as.numeric(substr(date, 3, 4))
            year <- 1900 + as.numeric(substr(date, 5, 6))
            res@metadata$startTime <- ISOdatetime(year, month, day, 0, 0, 0, tz="UTC")
        }
    }
    namesLine <- grep("CTDPRS", header)
    if (length(namesLine)) {
        dataNamesOriginal <- header[namesLine]
        dataNamesOriginal <- gsub("(.*)[1-9]$", "\\1", dataNamesOriginal) # line ends in a digit
        dataNamesOriginal <- gsub("^ *", "", dataNamesOriginal) # line starts with blanks
        dataNamesOriginal <- gsub("[ ]+", " ", dataNamesOriginal) # remove multiple spaces
        dataNamesOriginal <- strsplit(dataNamesOriginal, " ")[[1]]
        names <- woceNames2oceNames(dataNamesOriginal)
        unitsLine <- grep("DBAR", header)
        units <- header[unitsLine]
        units <- gsub("(.*)[1-9]$", "\\1", units) # line ends in a digit
        units <- gsub("^ *", "", units) # line starts with blanks
        units <- gsub("[ ]+", " ", units) # remove multiple spaces
        units <- strsplit(units, " ")[[1]]
        skip <- max(grep("^ *[*a-zA-Z]", header))
        data <- read.table(file, skip=skip, header=FALSE, col.names=names)
    } else {
        stop("cannot decode data header, since no line therein contains the string \"CTDPRS\"\n")
    }
    ## replace any missingValue with NA
    if (!missing(missingValue)) {
        for (name in names(data)) {
            data[[name]][missingValue == data[[name]]] <- NA
        }
    }
    res@data <- data
    res@metadata$dataNamesOriginal <- dataNamesOriginal
    if (length(names) == length(units)) {
        n <- length(names)
        ## example
        ##  "DBAR"    "ITS-90"  "PSS-7"   "UMOL/KG" "V"       "V"       "OBS."    "*"
        for (i in 1:n) {
            if (units[i] == "DBAR") {
                res@metadata$units[[names[i]]] <- list(unit=expression(dbar), scale="")
            } else if (units[i] == "ITS-90") {
                res@metadata$units[[names[i]]] <- list(unit=expression(degree*C), scale="ITS-90")
            } else if (units[i] == "PSS-78") {
                res@metadata$units[[names[i]]] <- list(unit=expression(), scale="PSS-78")
            } else if (units[i] == "UMOL/KG") {
                res@metadata$units[[names[i]]] <- list(unit=expression(mu*mol/kg), scale="")
            } else if (units[i] == "V") {
                res@metadata$units[[names[i]]] <- list(unit=expression(V), scale="")
            }
        }
    } else {
        warning("problem assigning units\n")
    }
    res@metadata$deploymentType <- deploymentType
    res
}
