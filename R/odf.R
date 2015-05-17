## [1] Anthony W. Isenor and David Kellow, 2011. ODF Format Specification Version 2.0. (A .doc file downloaded from a now-forgotten URL by Dan Kelley, in June 2011.)
##
## [2] An older document is: http://slgo.ca/app-sgdo/en/pdf/docs_reference/Format_ODF.pdf

setMethod(f="initialize",
          signature="odf",
          definition=function(.Object,time,filename="") {
              ## Assign to some columns so they exist if needed later (even if they are NULL)
              .Object@data$time <- if (missing(time)) NULL else time
              .Object@metadata$filename <- filename
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'odf' object"
              return(.Object)
          })

setMethod(f="plot",
          signature=signature("odf"),
          definition=function(x) {
              names <- names(x@data)
              n <- length(names) - 1
              par(mfrow=c(n, 1))
              for (i in 1:n) {
                   if (names[i] != "time") {
                       oce.plot.ts(x[["time"]], x[[names[i]]], ylab=names[i])
                   }
              }
          })

setMethod(f="summary",
          signature="odf",
          definition=function(object, ...) {
              cat("CTD Summary\n-----------\n\n")
              showMetadataItem(object, "type", "Instrument: ")
              showMetadataItem(object, "model", "Instrument model:  ")
              showMetadataItem(object, "serialNumber", "Instrument serial number:  ")
              showMetadataItem(object, "serialNumberTemperature", "Temperature serial number:  ")
              showMetadataItem(object, "serialNumberConductivity", "Conductivity serial number:  ")
              showMetadataItem(object, "filename", "File source:         ")
              showMetadataItem(object, "hexfilename", "Original file source (hex):  ")
              showMetadataItem(object, "institute", "Institute:      ")
              showMetadataItem(object, "scientist", "Chief scientist:      ")
              showMetadataItem(object, "date", "Date:      ", isdate=TRUE)
              showMetadataItem(object, "startTime", "Start time:          ", isdate=TRUE)
              showMetadataItem(object, "systemUploadTime", "System upload time:  ", isdate=TRUE)
              showMetadataItem(object, "cruise",  "Cruise:              ")
              showMetadataItem(object, "ship",    "Vessel:              ")
              showMetadataItem(object, "station", "Station:             ")
              cat("* Location:           ",       latlonFormat(object@metadata$latitude,
                                                               object@metadata$longitude,
                                                               digits=5), "\n")
              showMetadataItem(object, "waterDepth", "Water depth: ")
              showMetadataItem(object, "levels", "Number of levels: ")
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

#' Read an ODF file
#'
#' @details
#' ODF (Ocean Data Format) is a 
#' format developed at the Bedford Institute of Oceanography and also used
#' at other Department of Fisheries and Oceans (Canada) facilities.
#' It can hold various types of time-series data, which includes a variety
#' of instrument types. Thus, \code{read.odf()} 
#' is used by \code{read.ctd.odf} for CTD data, etc.
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
    date <- strptime(findInHeader("START_DATE", lines), "%b %d/%y")
    startTime <- strptime(tolower(findInHeader("START_DATE_TIME", lines)), "%d-%b-%Y %H:%M:%S", tz="UTC")
    endTime <- strptime(tolower(findInHeader("END_DATE_TIME", lines)), "%d-%b-%Y %H:%M:%S", tz="UTC")
    waterDepth <- as.numeric(findInHeader("SOUNDING", lines))
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
                     date=startTime,
                     startTime=startTime,
                     latitude=latitude,
                     longitude=longitude,
                     recovery=NULL,
                     waterDepth=waterDepth,
                     sampleInterval=NA,
                     filename=filename)
    data <- read.table(text=lines, skip=dataStart)
    if (length(data) != length(names))
        stop("mismatch between length of data names (", length(names), ") and number of columns in data matrix (", length(data), ")")
    if (debug) cat("Initially, column names are:", paste(names, collapse="|"), "\n\n")
    ## Infer standardized names for columsn, partly based on documentation (e.g. PSAL for salinity), but
    ## mainly from reverse engineering of some files from BIO and DFO.  The reverse engineering
    ## really is a kludge, and if things break (e.g. if data won't plot because of missing temperatures,
    ## or whatever), this is a place to look.  That's why the debugging flag displays a before-and-after
    ## view of names.
    ## Step 1: trim numbers at end (which occur for BIO files)
    ## Step 2: recognize some official names
    names[grep("CNTR_*.*", names)[1]] <- "scan"
    names[grep("CRAT_*.*", names)[1]] <- "conductivity"
    names[grep("OCUR_*.*", names)[1]] <- "oxygen_by_mole"
    names[grep("OTMP_*.*", names)[1]] <- "oxygen_temperature"
    names[grep("PSAL_*.*", names)[1]] <- "salinity"
    names[grep("PSAR_*.*", names)[1]] <- "par"
    names[grep("DOXY_*.*", names)[1]] <- "oxygen_by_volume"
    names[grep("TEMP_*.*", names)[1]] <- "temperature"
    names[grep("TE90_*.*", names)[1]] <- "temperature"
    names[grep("PRES_*.*", names)[1]] <- "pressure"
    names[grep("DEPH_*.*", names)[1]] <- "pressure" # FIXME possibly this actually *is* depth, but I doubt it
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
    names[grep("UNKN_*.*", names)[1]] <- "g" # percent good

    ## Step 3: recognize something from moving-vessel CTDs
    names[which(names=="FWETLABS")[1]] <- "fwetlabs" # FIXME: what is this?
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
    res@data <- data
    res@metadata <- metadata
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # read.odf()\n")
    res
}

