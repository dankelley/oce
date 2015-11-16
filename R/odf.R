## [1] Anthony W. Isenor and David Kellow, 2011. ODF Format Specification Version 2.0. (A .doc file downloaded from a now-forgotten URL by Dan Kelley, in June 2011.)
##
## [2] An older document is: http://slgo.ca/app-sgdo/en/pdf/docs_reference/Format_ODF.pdf

setMethod(f="initialize",
          signature="odf",
          definition=function(.Object,time,filename="") {
              ## Assign to some columns so they exist if needed later (even if they are NULL)
              .Object@data$time <- if (missing(time)) NULL else time
              .Object@metadata$filename <- filename
              .Object@metadata$deploymentType <- "HUHunknown" # see ctd
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'odf' object"
              return(.Object)
          })

setMethod(f="subset",
          signature="odf",
          definition=function(x, subset, ...) {
              subsetString <- paste(deparse(substitute(subset)), collapse=" ")
              rval <- x
              dots <- list(...)
              debug <- if (length(dots) && ("debug" %in% names(dots))) dots$debug else getOption("oceDebug")
              if (missing(subset))
                  stop("must give 'subset'")

              if (missing(subset))
                  stop("must specify a 'subset'")
              keep <- eval(substitute(subset), x@data, parent.frame(2)) # used for $ts and $ma, but $tsSlow gets another
              rval <- x
              for (name in names(x@data)) {
                  rval@data[[name]] <- x@data[[name]][keep]
              }
              rval@processingLog <- processingLogAppend(rval@processingLog, paste("subset(x, subset=", subsetString, ")", sep=""))
              rval
          })


setMethod(f="plot",
          signature=signature("odf"),
          definition=function(x) {
              names <- names(x@data)
              n <- length(names)
              par(mfrow=c(n-1, 1))
              for (i in 1:n) {
                   if (names[i] != "time") {
                       oce.plot.ts(x[["time"]], x[[names[i]]],
                                   ylab=names[i], mar=c(2, 3, 0.5, 1), drawTimeRange=FALSE)
                   }
              }
          })

setMethod(f="summary",
          signature="odf",
          definition=function(object, ...) {
              cat("ODF Summary\n-----------\n\n")
              showMetadataItem(object, "type",                     "Instrument:          ")
              showMetadataItem(object, "model",                    "Instrument model:    ")
              showMetadataItem(object, "serialNumber",             "Instr. serial no.:   ")
              showMetadataItem(object, "serialNumberTemperature",  "Temp. serial no.:    ")
              showMetadataItem(object, "serialNumberConductivity", "Cond. serial no.:    ")
              showMetadataItem(object, "filename",                 "File source:         ")
              showMetadataItem(object, "hexfilename",              "Orig. hex file:      ")
              showMetadataItem(object, "institute",                "Institute:           ")
              showMetadataItem(object, "scientist",                "Chief scientist:     ")
              showMetadataItem(object, "date",                     "Date:                ", isdate=TRUE)
              showMetadataItem(object, "startTime",                "Start time:          ", isdate=TRUE)
              showMetadataItem(object, "systemUploadTime",         "System upload time:  ", isdate=TRUE)
              showMetadataItem(object, "cruise",                   "Cruise:              ")
              showMetadataItem(object, "ship",                     "Vessel:              ")
              showMetadataItem(object, "station",                  "Station:             ")
              cat("* Location:           ",       latlonFormat(object@metadata$latitude,
                                                               object@metadata$longitude,
                                                               digits=5), "\n")
              showMetadataItem(object, "waterDepth",               "Water depth:         ")
              showMetadataItem(object, "levels",                   "Number of levels:    ")
              names <- names(object@data)
              ndata <- length(names)
              isTime <- names == "time"
              if (any(isTime))
                  cat("* Time ranges from", format(object@data$time[1]), "to", format(tail(object@data$time, 1)), "\n")
              threes <- matrix(nrow=sum(!isTime), ncol=3)
              ii <- 1
              for (i in 1:ndata) {
                  if (isTime[i])
                      next
                  threes[ii,] <- threenum(object@data[[i]])
                  ii <- ii + 1
              }
              rownames(threes) <- paste("   ", names[!isTime])
              colnames(threes) <- c("Min.", "Mean", "Max.")
              cat("* Statistics of data::\n")
              print(threes, indent='  ')
              processingLogShow(object)
          })


 
findInHeader <- function(key, lines)
{
    i <- grep(key, lines)
    if (length(i) < 1)
        ""
    else
        gsub("\\s*$", "", gsub("^\\s*", "", gsub("'","", gsub(",","",strsplit(lines[i[1]], "=")[[1]][2]))))
}

#' Translate from ODF names to oce names
#'
#' @details
#' The following table gives the regular expressions that define recognized
#' ODF names, along with the translated names as used in oce objects. Note
#' that if an item is repeated, then the second one has a \code{2} appended
#' at the end, etc.  Note that quality-control columns (with names starting with
#' \code{"QQQQ"}) are not handled with regular expressions. Instead, if such
#' a flag is found in the i-th column, then a name is constructed by taking
#' the name of the (i-1)-th column and appending \code{"Flag"}.
#' \tabular{lll}{
#'     \strong{Regexp} \tab \strong{Result}           \tab \strong{Notes}                                             \cr
#'     \code{ALTB_*.*} \tab \code{altimeter}          \tab                                                            \cr
#'     \code{BATH_*.*} \tab \code{barometricDepth}    \tab Barometric depth (of sensor? of water column?)             \cr
#'     \code{BEAM_*.*} \tab \code{a}                  \tab Used in \code{adp} objects                                 \cr
#'     \code{CNTR_*.*} \tab \code{scan}               \tab Used in \code{ctd} objects                                 \cr
#'     \code{CRAT_*.*} \tab \code{conductivity}       \tab Conductivity ratio                                         \cr
#'     \code{COND_*.*} \tab \code{conductivity_Spm}   \tab Conductivity in S/m                                        \cr
#'     \code{DEPH_*.*} \tab \code{pressure}           \tab Sensor depth below sea level                               \cr
#'     \code{DOXY_*.*} \tab \code{oxygen_by_volume}   \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{ERRV_*.*} \tab \code{error}              \tab Used in \code{adp} objects                                 \cr
#'     \code{EWCT_*.*} \tab \code{u}                  \tab Used in \code{adp} and \code{cm} objects                   \cr
#'     \code{FFFF_*.*} \tab \code{flag_archaic}       \tab Old flag name, replaced by \code{QCFF}                     \cr
#'     \code{FLOR_*.*} \tab \code{fluorometer}        \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{FWETLABS} \tab \code{fwetlabs}           \tab Used in ??                                                 \cr
#'     \code{LATD_*.*} \tab \code{latitude}           \tab                                                            \cr
#'     \code{LOND_*.*} \tab \code{longitude}          \tab                                                            \cr
#'     \code{NSCT_*.*} \tab \code{v}                  \tab Used in \code{adp} objects                                 \cr
#'     \code{OCUR_*.*} \tab \code{oxygen_by_mole}     \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{OTMP_*.*} \tab \code{oxygen_temperature} \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{OXYV_*.*} \tab \code{oxygen_voltage}     \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{PHPH_*.*} \tab \code{pH}                 \tab                                                            \cr
#'     \code{POTM_*.*} \tab \code{theta}              \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{PRES_*.*} \tab \code{pressure}           \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{PSAL_*.*} \tab \code{salinity}           \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{PSAR_*.*} \tab \code{par}                \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{QCFF_*.*} \tab \code{flag}               \tab Overall flag                                               \cr
#'     \code{SIGP_*.*} \tab \code{sigmaTheta}         \tab Used mainly in \code{ctd} objecs                           \cr
#'     \code{SIGT_*.*} \tab \code{sigmat}             \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{SYTM_*.*} \tab \code{time}               \tab Used in many objects                                       \cr
#'     \code{TE90_*.*} \tab \code{temperature}        \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{TEMP_*.*} \tab \code{temperature}        \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{UNKN_*.*} \tab \code{-}                  \tab The result is context-dependent                            \cr
#'     \code{VCSP_*.*} \tab \code{w}                  \tab Used in \code{adp} objects                                 \cr
#' }
#' Any code not shown in the list is transferred to the oce object without renaming, apart from 
#' the adjustment of suffix numbers. The following code have been seen in data files from
#' the Bedford Institute of Oceanography: \code{ALTB}, \code{PHPH} and \code{QCFF}.
#'
#' @param names Data names in ODF format.
#' @param PARAMETER_HEADER optional list containing information on the data variables
#' @return A vector of strings.

ODFNames2oceNames <- function(names, PARAMETER_HEADER=NULL)
{
    n <- length(names)
    ## Capture names for UNKN_* items, and key on them.  Possibly this should be done to
    ## get all the names, but then we just transfer the problem of decoding keys
    ## to decoding strings, and that might yield problems with encoding, etc.
    if (!is.null(PARAMETER_HEADER)) {
        for (i in 1:n) {
            if (length(grep("^UNKN_.*", PARAMETER_HEADER[[i]]$CODE))) {
                uname <- PARAMETER_HEADER[[i]]$NAME
                ## message("i:", i, ", name:\"", uname)
                names[i] <- if (length(grep("Percent Good Pings", uname ))) "g" else uname
            }
        }
    }
    ## Infer standardized names for columns, partly based on documentation (e.g. PSAL for salinity), but
    ## mainly from reverse engineering of some files from BIO and DFO.  The reverse engineering
    ## really is a kludge, and if things break (e.g. if data won't plot because of missing temperatures,
    ## or whatever), this is a place to look.
    names <- gsub("ALTB", "altimeter", names)
    names <- gsub("BATH", "waterDepth", names) # FIXME: is this water column depth or sensor depth?
    names <- gsub("BEAM", "a", names)  # FIXME: is this sensible?
    names <- gsub("CNTR", "scan", names)
    names <- gsub("CRAT", "conductivity", names)
    names <- gsub("COND", "conductivity_Spm", names)
    names <- gsub("DEPH", "depth", names)
    names <- gsub("DOXY", "oxygen_by_volume", names)
    names <- gsub("ERRV", "error", names)
    names <- gsub("EWCT", "u", names)
    names <- gsub("FFFF", "flag_archaic", names)
    names <- gsub("FLOR", "fluorometer", names)
    names <- gsub("FWETLABS", "fwetlabs", names) # FIXME: what is this?
    names <- gsub("LATD", "latitude", names)
    names <- gsub("LOND", "longitude", names)
    names <- gsub("NSCT", "v", names)
    names <- gsub("OCUR", "oxygen_by_mole", names)
    names <- gsub("OTMP", "oxygen_temperature", names)
    names <- gsub("OXYV", "oxygen_voltage", names)
    names <- gsub("PHPH", "pH", names)
    names <- gsub("POTM", "theta", names) # in a moored ctd file examined 2014-05-15
    names <- gsub("PRES", "pressure", names)
    names <- gsub("PSAL", "salinity", names)
    names <- gsub("PSAR", "par", names)
    names <- gsub("QCFF", "flag", names) # overall flag
    names <- gsub("SIGP", "sigmaTheta", names)
    names <- gsub("SIGT", "sigmat", names) # in a moored ctd file examined 2014-05-15
    names <- gsub("SYTM", "time", names) # in a moored ctd file examined 2014-05-15
    names <- gsub("TE90", "temperature", names)
    names <- gsub("TEMP", "temperature", names)
    names <- gsub("VCSP", "w", names)
    ## Step 3: recognize something from moving-vessel CTDs
    ## Step 4: some meanings inferred (guessed, really) from file CTD_HUD2014030_163_1_DN.ODF
    ## Finally, fix up suffixes.
    names <- gsub("_01", "", names)
    names <- gsub("_02", "2", names)
    names <- gsub("_03", "3", names)
    names <- gsub("_04", "4", names)
    for (i in 2:length(names)) {
        if (substr(names[i], 1, 4)=="QQQQ")
            names[i] <- paste(names[i-1], "Flag", sep="")
    }
    names
}

ODF2oce <- function(ODF, coerce=TRUE, debug=getOption("oceDebug"))
{
    ## Stage 1. insert metadata (with odfHeader holding entire ODF header info)
    ## FIXME: add other types, starting with ADCP perhaps
    isCTD <- FALSE
    isMCTD <- FALSE
    if (coerce) {
        if ("CTD" == ODF$EVENT_HEADER$DATA_TYPE) { 
            isCTD <- TRUE
            rval <- new("ctd")
        } else if ("MCTD" == ODF$EVENT_HEADER$DATA_TYPE) { 
            isMCTD <- TRUE
            rval <- new("ctd")
            rval@metadata$deploymentType <- "moored"
        } else {
            rval <- new("odf") # FIXME: other types
        }
    } else {
        rval <- new("odf")
    }
    ## Save the whole header as read by BIO routine read_ODF()
    rval@metadata <- list(odfHeader=list(ODF_HEADER=ODF$ODF_HEADER,
                                         CRUISE_HEADER=ODF$CRUISE_HEADER,
                                         EVENT_HEADER=ODF$EVENT_HEADER,
                                         METEO_HEADER=ODF$METEO_HEADER,
                                         INSTRUMENT_HEADER=ODF$INSTRUMENT_HEADER,
                                         QUALITY_HEADER=ODF$QUALITY_HEADER,
                                         GENERAL_CAL_HEADER=ODF$GENERAL_CAL_HEADER,
                                         POLYNOMIAL_CAL_HEADER=ODF$POLYNOMIAL_CAL_HEADER,
                                         COMPASS_CAL_HEADER=ODF$COMPASS_CAL_HEADER,
                                         HISTORY_HEADER=ODF$HISTORY_HEADER,
                                         PARAMETER_HEADER=ODF$PARAMETER_HEADER,
                                         RECORD_HEADER=ODF$RECORD_HEADER,
                                         INPUT_FILE=ODF$INPUT_FILE)) 
    ## Define some standard items that are used in plotting and summaries
    if (isCTD) {
        rval@metadata$type <- rval@metadata$odfHeader$INSTRUMENT_HEADER$INST_TYPE
        rval@metadata$model <- rval@metadata$odfHeader$INSTRUMENT_HEADER$INST_MODEL
        rval@metadata$serialNumber <- rval@metadata$odfHeader$INSTRUMENT_HEADER$SERIAL_NUMBER
    }
    rval@metadata$startTime <- strptime(rval@metadata$odfHeader$EVENT_HEADER$START_DATE_TIME,
                                        "%d-%B-%Y %H:%M:%S", tz="UTC")
    rval@metadata$filename <- rval@metadata$odfHeader$ODF_HEADER$FILE_SPECIFICATION
    rval@metadata$serialNumber <- rval@metadata$odfHeader$INSTRUMENT_HEADER$SERIAL_NUMBER
    rval@metadata$ship <- rval@metadata$odfHeader$CRUISE_HEADER$PLATFORM
    rval@metadata$cruise <- rval@metadata$odfHeader$CRUISE_HEADER$CRUISE_NUMBER
    rval@metadata$station <- rval@metadata$odfHeader$EVENT_HEADER$EVENT_NUMBER # FIXME: is this right?
    rval@metadata$scientist <- rval@metadata$odfHeader$CRUISE_HEADER$CHIEF_SCIENTIST
    rval@metadata$latitude <- as.numeric(rval@metadata$odfHeader$EVENT_HEADER$INITIAL_LATITUDE)
    rval@metadata$longitude <- as.numeric(rval@metadata$odfHeader$EVENT_HEADER$INITIAL_LONGITUDE)

    ## Stage 2. insert data (renamed to Oce convention)
    xnames <- names(ODF$DATA)
    rval@data <- as.list(ODF$DATA)
    ## table relating ODF names to Oce names ... guessing on FFF and SIGP, and no idea on CRAT
    ## FIXME: be sure to record unit as conductivityRatio.
    rvalNames <- ODFNames2oceNames(xnames, PARAMETER_HEADER=ODF$PARAMETER_HEADER)
    names(rval@data) <- rvalNames
    ## Obey missing values ... only for numerical things (which might be everything, for all I know)
    nd <- length(rvalNames)
    for (i in 1:nd) {
        if (is.numeric(rval@data[[i]])) {
            NAvalue <- as.numeric(ODF$PARAMETER_HEADER[[i]]$NULL_VALUE)
            ## message("NAvalue: ", NAvalue)
            rval@data[[i]][rval@data[[i]] == NAvalue] <- NA
        }
    }
    ## Stage 3. rename QQQQ_* columns as flags on the previous column
    names <- names(rval@data)
    for (i in seq_along(names)) {
        if (substr(names[i], 1, 4) == "QQQQ") {
            if (i > 1) {
                names[i] <- paste(names[i-1], "Flag", sep="")
            }
        }
    }
    if ("flag_archaic" %in% names && !("flag" %in% names))
        names <- gsub("flag_archaic", "flag", names)
    names(rval@data) <- names
    rval
}


#' Read an ODF file, producing an oce object
#'
#' @details
#' ODF (Ocean Data Format) is a 
#' format developed at the Bedford Institute of Oceanography and also used
#' at other Canadian Department of Fisheries and Oceans (DFO) facilities.
#' It can hold various types of time-series data, which includes a variety
#' of instrument types. Thus, \code{read.odf} 
#' is used by \code{read.ctd.odf} for CTD data, etc. As of mid-2015,
#' \code{read.odf} is still in development, with features being added as  a 
#' project with DFO makes available more files.
#'
#' Note that some elements of the metadata are particular to ODF objects,
#' e.g. \code{depthMin}, \code{depthMax} and \code{sounding}, which
#' are inferred from ODF items named \code{MIN_DEPTH}, \code{MAX_DEPTH}
#' and \code{SOUNDING}, respectively. In addition, the more common metadata
#' item \code{waterDepth}, which is used in \code{ctd} objects to refer to
#' the total water depth, is here identical to \code{sounding}.
#'
#' @param file the file containing the data.
#' @param debug a debugging flag, 0 for none, 1 for some debugging
#' @return an object of class \code{oce}. It is up to a calling function to determine what to do with this object.
#' @references Anthony W. Isenor and David Kellow, 2011. ODF Format Specification Version 2.0. (A .doc file downloaded from a now-forgotten URL by Dan Kelley, in June 2011.)
read.odf <- function(file, debug=getOption("oceDebug"))
{
    FILE<-file
    oceDebug(debug, "read.odf(\"", file, "\", ...) {\n", unindent=1, sep="")
    if (debug>=100) t0 <- Sys.time()
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
    if (debug>=100) oceDebug(debug, sprintf("%.2fs: before reading lines to get header\n", Sys.time()-t0))
    lines <- readLines(file, 1000, encoding="UTF-8")
    if (debug>=100) oceDebug(debug, sprintf("%.2fs: after reading 1000 lines to get header\n", Sys.time()-t0))
    pushBack(lines, file) # we used to read.table(text=lines, ...) but it is VERY slow
    if (debug>=100) oceDebug(debug, sprintf("%.2fs: after pushing-back those lines\n", Sys.time()-t0))
    dataStart <- grep("-- DATA --", lines)
    if (!length(dataStart))
        stop("cannot locate a line containing '-- DATA --'")
    parameterStart <- grep("PARAMETER_HEADER", lines)
    if (!length(parameterStart))
        stop("cannot locate any lines containing 'PARAMETER_HEADER'")
    namesWithin <- parameterStart[1]:dataStart[1]
    ## extract column codes in a step-by-step way, to make it easier to adjust if the format changes

    ## The mess below hides warnings on non-numeric missing-value codes.
    options <- options('warn')
    options(warn=-1) 
    t <- try({nullValue <- as.numeric(findInHeader("NULL_VALUE", lines)[1])},
        silent=TRUE)
    if (class(t) == "try-error") {
        nullValue <- findInHeader("NULL_VALUE", lines)[1]
    }
    options(warn=options$warn)

    names <- lines[grep("^\\s*CODE\\s*=", lines)]
    names <- gsub("\\s*$", "", gsub("^\\s*", "", names)) # trim start/end whitespace
    names <- gsub(",", "", names) # trim commas
    names <- gsub("'", "", names) # trim single quotes
    names <- gsub(",\\s*$", "", gsub("^\\s*","", names)) # "  CODE=PRES_01," -> "CODE=PRES_01"
    names <- gsub("^CODE\\s*=\\s*", "", names) # "CODE=PRES_01" -> "PRES_01"
    names <- gsub("\\s*$", "", gsub("^\\s*", "", names)) # trim remnant start/end spaces
    scientist <- findInHeader("CHIEF_SCIENTIST", lines)
    ship <- findInHeader("PLATFORM", lines) # maybe should rename, e.g. for helicopter
    institute <- findInHeader("ORGANIZATION", lines) # maybe should rename, e.g. for helicopter
    latitude <- as.numeric(findInHeader("INITIAL_LATITUDE", lines))
    longitude <- as.numeric(findInHeader("INITIAL_LONGITUDE", lines))
    cruise <- findInHeader("CRUISE_NAME", lines)
    countryInstituteCode <- findInHeader("COUNTRY_INSTITUTE_CODE", lines)
    cruiseNumber <- findInHeader("CRUISE_NUMBER", lines)
    DATA_TYPE <- findInHeader("DATA_TYPE", lines)
    deploymentType <- if ("CTD" == DATA_TYPE) "profile" else if ("MCTD" == DATA_TYPE) "moored" else "unknown"
    date <- strptime(findInHeader("START_DATE", lines), "%b %d/%y")
    startTime <- strptime(tolower(findInHeader("START_DATE_TIME", lines)), "%d-%b-%Y %H:%M:%S", tz="UTC")
    endTime <- strptime(tolower(findInHeader("END_DATE_TIME", lines)), "%d-%b-%Y %H:%M:%S", tz="UTC")
    depthMin <- as.numeric(findInHeader("MIN_DEPTH", lines))
    depthMax <- as.numeric(findInHeader("MAX_DEPTH", lines))
    sounding <- as.numeric(findInHeader("SOUNDING", lines))
    waterDepth <- as.numeric(findInHeader("SOUNDING", lines))
    if (is.null(waterDepth))
        waterDepth <- NA
    station <- findInHeader("EVENT_NUMBER", lines)

    ## water depth could be missing or e.g. -999
    waterDepthWarning <- FALSE
    if (is.na(waterDepth)) {
        waterDepth <- max(abs(data$pressure), na.rm=TRUE)
        waterDepthWarning <- TRUE
    }
    if (!is.na(waterDepth) && waterDepth < 0)
        waterDepth <- NA

    type <- findInHeader("INST_TYPE", lines)
    if (length(grep("sea", type, ignore.case=TRUE)))
        type <- "SBE"
    serialNumber <- findInHeader("SERIAL_NUMBER", lines)
    model <- findInHeader("MODEL", lines)

    metadata <- list(header=NULL, # FIXME
                     type=type,        # only odt
                     model=model,      # only odt
                     serialNumber=serialNumber,
                     ship=ship,
                     scientist=scientist,
                     institute=institute,
                     address=NULL,
                     cruise=cruise,
                     station=station,
                     countryInstituteCode=countryInstituteCode, # ODF only
                     cruiseNumber=cruiseNumber, # ODF only
                     deploymentType=deploymentType, # used by CTD also
                     date=startTime,
                     startTime=startTime,
                     latitude=latitude,
                     longitude=longitude,
                     recovery=NULL,
                     waterDepth=waterDepth, # this is not the sensor depth
                     depthMin=depthMin, depthMax=depthMax, sounding=sounding, # specific to ODF
                     sampleInterval=NA,
                     filename=filename)
    if (debug>=100) oceDebug(debug, sprintf("%.2fs: after determining metadata\n", Sys.time()-t0))
    ##> ## fix issue 768
    ##> lines <- lines[grep('%[0-9.]*f', lines,invert=TRUE)]
    data <- read.table(file, skip=dataStart, stringsAsFactors=FALSE)
    ## data <- fread(FILE, skip=dataStart, header=FALSE)
    if (debug>=100) oceDebug(debug, sprintf("%.2fs: after reading data table\n", Sys.time()-t0))
    if (length(data) != length(names))
        stop("mismatch between length of data names (", length(names), ") and number of columns in data matrix (", length(data), ")")
    oceDebug(debug, "Initially, column names are:", paste(names, collapse="|"), "\n")
    names <- ODFNames2oceNames(names, PARAMETER_HEADER=NULL)
    oceDebug(debug, "Finally, column names are:", paste(names, collapse="|"), "\n")
    names(data) <- names
    if (!is.na(nullValue)) {
        data[data==nullValue] <- NA
    }
    if (debug>=100) oceDebug(debug, sprintf("%.2fs: after converting ODF names to oce names\n", Sys.time()-t0))
    if ("time" %in% names)
        data$time <- as.POSIXct(strptime(as.character(data$time), format="%d-%b-%Y %H:%M:%S", tz="UTC"))
    if (debug>=100) oceDebug(debug, sprintf("%.2fs: after converting ODF time to R time\n", Sys.time()-t0))
    metadata$names <- names
    metadata$labels <- names
    res <- new("odf")
    res@data <- as.list(data)
    res@metadata <- metadata
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # read.odf()\n")
    res
}

