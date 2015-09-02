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
#' at the end, etc.
#' \tabular{lll}{
#'     \strong{Regexp} \tab \strong{Result}           \tab \strong{Notes}                                             \cr
#'     \code{BEAM_*.*} \tab \code{a}                  \tab Used in \code{adp} objects                                 \cr
#'     \code{CNTR_*.*} \tab \code{scan}               \tab Used in \code{ctd} objects                                 \cr
#'     \code{CRAT_*.*} \tab \code{conductivity}       \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{DEPH_*.*} \tab \code{pressure}           \tab Used in \code{ctd} (may just be pressure)                  \cr
#'     \code{DOXY_*.*} \tab \code{oxygen_by_volume}   \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{ERRV_*.*} \tab \code{error}              \tab Used in \code{adp} objects                                 \cr
#'     \code{EWCT_*.*} \tab \code{u}                  \tab Used in \code{adp} and \code{cm} objects                   \cr
#'     \code{FFFF_*.*} \tab \code{flag}               \tab Used in many objects                                       \cr
#'     \code{FLOR_*.*} \tab \code{fluorometer}        \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{FWETLABS} \tab \code{fwetlabs}           \tab Used in ??                                                 \cr
#'     \code{NSCT_*.*} \tab \code{v}                  \tab Used in \code{adp} objects                                 \cr
#'     \code{OCUR_*.*} \tab \code{oxygen_by_mole}     \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{OTMP_*.*} \tab \code{oxygen_temperature} \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{POTM_*.*} \tab \code{theta}              \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{PRES_*.*} \tab \code{pressure}           \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{PSAL_*.*} \tab \code{salinity}           \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{PSAR_*.*} \tab \code{par}                \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{SIGP_*.*} \tab \code{sigmaTheta}         \tab Used mainly in \code{ctd} objecs                           \cr
#'     \code{SIGT_*.*} \tab \code{sigmat}             \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{SYTM_*.*} \tab \code{time}               \tab Used in many objects                                       \cr
#'     \code{TE90_*.*} \tab \code{temperature}        \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{TEMP_*.*} \tab \code{temperature}        \tab Used mainly in \code{ctd} objects (FIXME: check if IPTS68) \cr
#'     \code{UNKN_*.*} \tab \code{g}                  \tab Used in ??                                                 \cr
#'     \code{VCSP_*.*} \tab \code{w}                  \tab Used in \code{adp} objects                                 \cr
#' }
#'
#' @param names Data names in ODF format.
#' @param PARAMETER_HEADER optional list containing information on the data variables
#' @return A vector of string strings.

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
    ## or whatever), this is a place to look.  That's why the debugging flag displays a before-and-after
    ## view of names.
    names[grep("CNTR_*.*", names)[1]] <- "scan"
    names[grep("CRAT_*.*", names)[1]] <- "conductivity" # ratio
    ## For COND, a sample file states: UNITS='mmHo'; is that a typo for mho/m=(1/ohm)/m=Seimens/m?
    names[grep("COND_*.*", names)[1]] <- "conductivity_Spm"
    names[grep("OCUR_*.*", names)[1]] <- "oxygen_by_mole"
    names[grep("OTMP_*.*", names)[1]] <- "oxygen_temperature"
    names[grep("PSAL_*.*", names)[1]] <- "salinity"
    names[grep("PSAR_*.*", names)[1]] <- "par"
    names[grep("DOXY_*.*", names)[1]] <- "oxygen_by_volume"
    names[grep("TEMP_*.*", names)[1]] <- "temperature"
    names[grep("TE90_*.*", names)[1]] <- "temperature"
    names[grep("PRES_*.*", names)[1]] <- "pressure"
    names[grep("DEPH_*.*", names)[1]] <- "depth"
    names[grep("SIGP_*.*", names)[1]] <- "sigmaTheta"
    names[grep("FLOR_*.*", names)[1]] <- "fluorometer"
    names[grep("FFFF_*.*", names)[1]] <- "flag"
    names[grep("SYTM_*.*", names)[1]] <- "time" # in a moored ctd file examined 2014-05-15
    names[grep("SIGT_*.*", names)[1]] <- "sigmat" # in a moored ctd file examined 2014-05-15
    names[grep("POTM_*.*", names)[1]] <- "theta" # in a moored ctd file examined 2014-05-15
    ## Below are some codes that seem to be useful for ADCP, inferred from a file from BIO
    ## [1] "EWCT_01" "NSCT_01" "VCSP_01" "ERRV_01" "BEAM_01" "UNKN_01" "time"   
    names[grep("EWCT_*.*", names)[1]] <- "u"
    names[grep("NSCT_*.*", names)[1]] <- "v"
    names[grep("VCSP_*.*", names)[1]] <- "w"
    names[grep("ERRV_*.*", names)[1]] <- "error"
    ## next is  NAME='Average Echo Intensity (AGC)'
    names[grep("BEAM_*.*", names)[1]] <- "a" # FIXME: is this sensible?
    ## names[grep("UNKN_*.*", names)[1]] <- "g" # percent good
    ## Step 3: recognize something from moving-vessel CTDs
    names[which(names=="FWETLABS")[1]] <- "fwetlabs" # FIXME: what is this?
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
    rval@metadata$latitude <- rval@metadata$odfHeader$EVENT_HEADER$INITIAL_LATITUDE
    rval@metadata$longitude <- rval@metadata$odfHeader$EVENT_HEADER$INITIAL_LONGITUDE

    ## Stage 2. insert data (renamed to Oce convention)
    xnames <- names(ODF$DATA)
    rval@data <- as.list(ODF$DATA)
    ## table relating ODF names to Oce names ... guessing on FFF and SIGP, and no idea on CRAT
    ## FIXME: be sure to record unit as conductivityRatio.
    rvalNames <- ODFNames2oceNames(xnames, PARAMETER_HEADER=ODF$PARAMETER_HEADER)
    names(rval@data) <- rvalNames
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
    oceDebug(debug, "read.odf() {\n", unindent=1)
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
    lines <- readLines(file, encoding="UTF-8")
    dataStart <- grep("-- DATA --", lines)
    if (!length(dataStart))
        stop("cannot locate a line containing '-- DATA --'")
    parameterStart <- grep("PARAMETER_HEADER", lines)
    if (!length(parameterStart))
        stop("cannot locate any lines containing 'PARAMETER_HEADER'")
    namesWithin <- parameterStart[1]:dataStart[1]
    ## extract column codes in a step-by-step way, to make it easier to adjust if the format changes
    nullValue <- as.numeric(findInHeader("NULL_VALUE", lines)[1]) # FIXME: should do this for columns separately
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
    data <- read.table(text=lines, skip=dataStart)
    if (length(data) != length(names))
        stop("mismatch between length of data names (", length(names), ") and number of columns in data matrix (", length(data), ")")
    if (debug) cat("Initially, column names are:", paste(names, collapse="|"), "\n\n")
    names <- ODFNames2oceNames(names, PARAMETER_HEADER=ODF$PARAMETER_HEADER)
    if (debug) cat("Finally, column names are:", paste(names, collapse="|"), "\n\n")
    names(data) <- names
    if (!is.na(nullValue)) {
        data[data==nullValue] <- NA
    }
    if ("time" %in% names)
        data$time <- strptime(as.character(data$time), format="%d-%b-%Y %H:%M:%S", tz="UTC")
    metadata$names <- names
    metadata$labels <- names
    res <- new("odf")
    res@data <- as.list(data)
    res@metadata <- metadata
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # read.odf()\n")
    res
}

