#' Read an ODV-type CTD File
#'
#' @template readCtdTemplate
#'
#' @template encodingTemplate
#'
#' @author Dan Kelley
#'
#' @details
#' [read.ctd.odv()] attempts to read files stored in ODV format,
#' used by some European data providers. This works only crudely, as of
#' 2020-05-17. In particular, the translation from ODV parameter names to oce
#' names is *very* limited.  For example, only one of the dozens of possibilities
#' for variants of phosphate is handled at the moment, and that is because
#' this was the variant supplied in a test file sent to the author on
#' 2020-05-16.  It is unlikely that full support of ODV files will
#' become available in [read.ctd.odv()], given the lack of a comprehensive source
#' listing ODV variable names and their meanings, and low user
#' interest.
#'
#' @references
#' 1. `https://www.bodc.ac.uk/resources/delivery_formats/odv_format/` describes
#' the `ODV` format.
#' 2. `https://vocab.nerc.ac.uk/collection/P07/current/` may be
#' worth consulting for variable names.
read.ctd.odv <- function(file,
    columns=NULL,
    station=NULL,
    missingValue,
    deploymentType,
    encoding="latin1",
    monitor=FALSE,
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
    oceDebug(debug, "read.ctd.odv() {\n", unindent=1)
    filename <- ""
    if (is.character(file)) {
        filename <- fullFilename(file)
        file <- file(file, "r", encoding=encoding)
        on.exit(close(file))
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }
    res <- new("ctd")
    lines <- readLines(file)
    nlines <- length(lines)
    dataStart <- grep("^//.*$", lines, invert=TRUE)[1]
    if (!dataStart)
	stop("cannot locate the end of the header in this ODV file")
    if (nlines - dataStart < 1)
	stop("file has column header, but nothing else")
    if (nlines - dataStart < 2)
	stop("file has two rows of column header, but nothing else")
    oceDebug(debug, "data (column names) start at line", dataStart, "\n")
    res@metadata$filename <- filename
    res@metadata$header <- lines[seq(1, dataStart-1)]
    colNames <- strsplit(lines[dataStart], "\t")[[1]]
    oceDebug(debug, "original colNames=c(\"", paste(colNames, collapse="\", \""), "\")\n", sep="")
    ## Below is a sample header line from https://github.com/dankelley/oce/issues/1696
    ## Cruise	Station	Type	yyyy-mm-ddThh:mm:ss.sss	Longitude [degrees_east]	Latitude [degrees_north]	LOCAL_CDI_ID	EDMO_code	Bot. Depth [m]	DEPHPR01 [m]	QV:SEADATANET	PRESPR01 [decibar=10000 pascals]	QV:SEADATANET	OXYSSC01 [%]	QV:SEADATANET	PHOSAADZ [millimole/m3]	QV:SEADATANET	ODSDM017 [millimole/m3]	QV:SEADATANET
    data <- read.delim(text=lines[seq(dataStart, nlines)], sep="\t")
    dataNamesOriginal <- as.list(colNames)
    names(dataNamesOriginal) <- colNames
    units <- list()
    flags <- list()
    ## Try to decode some columns.  Here, an assumption is being
    ## made that each of these columns is being followed
    ## by a flag column.
    ##
    ## Some test files:
    ##
    ## sample01.txt:
    ##
    ## Cruise	Station	Type	yyyy-mm-ddThh:mm:ss.sss	Longitude [degrees_east]	Latitude [degrees_north]	LOCAL_CDI_ID	EDMO_code	Bot. Depth [m]	DEPHPR01 [m]	QV:SEADATANET	PRESPR01 [decibar=10000 pascals]	QV:SEADATANET	OXYSSC01 [%]	QV:SEADATANET	PHOSAADZ [millimole/m3]	QV:SEADATANET	ODSDM017 [millimole/m3]	QV:SEADATANET
    ##
    ## sample02.txt:
    ##
    ## Cruise	Station	Type	YYYY-MM-DDThh:mm:ss.sss	Longitude [degrees_east]	Latitude [degrees_north]	LOCAL_CDI_ID	EDMO_code	Bot. Depth [m]	DEPH [m]	QV:SEADATANET	PRES [decibar=10000 pascals]	QV:SEADATANET	TEMP [Celsius degree]	QV:SEADATANET	PSAL [P.S.U.]	QV:SEADATANET	DOX2 [micromole/kg]	QV:SEADATANET
    flagColumnsToDelete <- NULL
    ## "Bot. Depth [m]"
    i  <- grep("Bot. Depth \\[m\\]", colNames)
    if (1 == length(i)) {
	colNames[i] <- "bottleDepth"
	units$bottleDepth <- list(unit=expression(m), scale="")
        dataNamesOriginal$bottleDepth <- "Bot. Depth [m]"
    }
    ## "DEPH [m]"
    i  <- grep("DEPH \\[m\\]", colNames)
    if (1 == length(i)) {
	colNames[i] <- "depth"
	units$depth <- list(unit=expression(m), scale="")
	flags$depth <- data[, i+1]
	flagColumnsToDelete <- c(flagColumnsToDelete, i+1)
        dataNamesOriginal$depth <- "DEPH [m]"
    }
    i  <- grep("DEPHPR01 \\[m\\]", colNames)
    if (1 == length(i)) {
	colNames[i] <- "depthFromPressure"
	units$depthFromPressure <- list(unit=expression(m), scale="")
	flags$depthFromPressure <- data[, i+1]
	flagColumnsToDelete <- c(flagColumnsToDelete, i+1)
        dataNamesOriginal$depthFromPressure <- "DEPH [m]"
    }
    ## "dissO2_in-situ-sensor [mg/l]"
    i <- grep("dissO2_in-situ-sensor \\[mg/l\\]", colNames)
    if (1 == length(i)) {
	colNames[i] <- "oxygen"
	units$oxygen <- list(unit=expression(mg/l), scale="")
	flags$oxygen <- data[, i+1]
	flagColumnsToDelete <- c(flagColumnsToDelete, i+1)
        dataNamesOriginal$oxygen <- "dissO2_in-situ-sensor [mg/l]"
    }
    i  <- grep("DOX2 \\[micromole/kg\\]", colNames)
    if (1 == length(i)) {
	colNames[i] <- "oxygen"
	units$oxygen <- list(unit=expression(mu*mol/kg), scale="")
	flags$oxygen <- data[, i+1]
	flagColumnsToDelete <- c(flagColumnsToDelete, i+1)
        dataNamesOriginal$oxygen <- "DOX2 [micromole/kg]"
    }
    ## "chl-a_fluorometry [ug/l]"
    i  <- grep("chl-a_fluorometry \\[ug/l\\]", colNames)
    if (1 == length(i)) {
	colNames[i] <- "fluorometry"
	units$fluorometry <- list(unit=expression(mu*mol/l), scale="")
	flags$fluorometry <- data[, i+1]
	flagColumnsToDelete <- c(flagColumnsToDelete, i+1)
        dataNamesOriginal$fluorometry <- "chl-a_fluorometry [ug/l]"
    }
    ## "NH4 [umol/l]"
    i  <- grep("NH4 \\[umol/l\\]", colNames)
    if (1 == length(i)) {
	colNames[i] <- "ammonium"
	units$ammonium <- list(unit=expression(mu*mol/l), scale="")
	flags$ammonium <- data[, i+1]
	flagColumnsToDelete <- c(flagColumnsToDelete, i+1)
        dataNamesOriginal$ammonium <- "NH4 [umol/l]"
    }
    ## "Latitude [degrees_north]"
    i <- grep("Latitude \\[degrees_north\\]", colNames)
    if (1 == length(i)) {
	colNames[i] <- "latitude"
	units$latitude <- list(unit=expression(degree*N), scale="")
        dataNamesOriginal$latitude <- "Latitude [degrees_north]"
    }
    ## "Longitude [degrees_east]"
    i <- grep("Longitude \\[degrees_east\\]", colNames)
    if (1 == length(i)) {
	colNames[i] <- "longitude"
	units$longitude <- list(unit=expression(degree*E), scale="")
        dataNamesOriginal$longitude <- "Longitude [degrees_east]"
    }
    ## https://www.bodc.ac.uk/resources/vocabularies/vocabulary_search/P01/
    i <- grep("OXYSSC01 \\[%\\]", colNames)
    if (1 == length(i)) {
	colNames[i] <- "oxygen"
	units$oxygen <- list(unit=expression(percent), scale="")
	flags$oxygen <- data[, i+1]
	flagColumnsToDelete <- c(flagColumnsToDelete, i+1)
        dataNamesOriginal$oxygen <- "OXYSSC01 [%]"
    }
    i  <- grep("PRES \\[decibar=10000 pascals\\]", colNames)
    if (1 == length(i)) {
	colNames[i] <- "pressure"
	units$pressure <- list(unit=expression(dbar), scale="")
	flags$pressure <- data[, i+1]
	flagColumnsToDelete <- c(flagColumnsToDelete, i+1)
        dataNamesOriginal$pressure <- "PRES [decibar=10000 pascals]"
    }
    i  <- grep("PRESPR01 \\[decibar=10000 pascals\\]", colNames)
    if (1 == length(i)) {
	colNames[i] <- "pressure"
	units$pressure <- list(unit=expression(dbar), scale="")
	flags$pressure <- data[, i+1]
	flagColumnsToDelete <- c(flagColumnsToDelete, i+1)
        dataNamesOriginal$pressure <- "PRES [decibar=10000 pascals]"
    }
    ## http://seadatanet.maris2.nl/v_bodc_vocab_v2/vocab_relations.asp?lib=P08
    i <- grep("PHOSAADZ \\[millimole/m3\\]", colNames)
    if (1 == length(i)) {
	colNames[i] <- "phosphate"
	units$phosphate <- list(unit=expression(mmol/m^3), scale="")
	flags$phosphate <- data[, i+1]
	flagColumnsToDelete <- c(flagColumnsToDelete, i+1)
        dataNamesOriginal$phosphate <- "PHOSAADZ [millimole/m3]"
    }
    i <- grep("PSAL \\[P\\.S\\.U\\.]", colNames)
    if (1 == length(i)) {
	colNames[i] <- "salinity"
	units$salinity <- list(unit=expression(), scale="")
	flags$salinity <- data[, i+1]
	flagColumnsToDelete <- c(flagColumnsToDelete, i+1)
        dataNamesOriginal$salinity <- "PSAL [P.S.U.]"
    }
    ## I could not find this in my searches on seadatant.  Maybe it's a form of
    ## dissolved oxygen, but it is crazy to just make gusses.
    i <- grep("ODSDM017 \\[millimole/m3\\]", colNames)
    if (1 == length(i)) {
	colNames[i] <- "nitrogen"
	units$nitrogen <- list(unit=expression(mmol/m^3), scale="")
	flags$nitrogen <- data[, i+1]
	flagColumnsToDelete <- c(flagColumnsToDelete, i+1)
	## warning("code ODSDM017 is not understood\n")
        dataNamesOriginal$nitrogen <- "ODSDM017 [millimole/m3]"
    }
    ## "Sal_conductivity-cell [TU]"
    i <- grep("Sal_conductivity-cell \\[TU\\]", colNames)
    if (1 == length(i)) {
	colNames[i] <- "salinity"
	units$salinity <- list(unit=expression(), scale="")
	flags$salinity <- data[, i+1]
	flagColumnsToDelete <- c(flagColumnsToDelete, i+1)
        dataNamesOriginal$salinity <- "Sal_conductivity-cell [TU]"
    }
    ## "SiO4_Filt_ColAA [umol/l]"
    i <- grep("SiO4_Filt_ColAA \\[umol/l\\]", colNames)
    if (1 == length(i)) {
	colNames[i] <- "silicate"
	units$silicate <- list(unit=expression(mu*mol/l), scale="")
	flags$silicate <- data[, i+1]
	flagColumnsToDelete <- c(flagColumnsToDelete, i+1)
        dataNamesOriginal$silicate <- "SiO4_Filt_ColAA [umol/l]"
    }
    ## "TEMP [Celsius degree]"
    i  <- grep("TEMP \\[Celsius degree\\]", colNames)
    if (1 == length(i)) {
	colNames[i] <- "temperature"
	units$temperature <- list(unit=expression(degree*C), scale="")
	flags$temperature <- data[, i+1]
	flagColumnsToDelete <- c(flagColumnsToDelete, i+1)
        dataNamesOriginal$temperature <- "TEMP [Celsius degree]"
    }
    ## "Temp_insitu-thermistor [degC]"
    i  <- grep("Temp_insitu-thermistor \\[degC\\]", colNames)
    if (1 == length(i)) {
	colNames[i] <- "temperature"
	units$temperature <- list(unit=expression(degree*C), scale="")
	flags$temperature <- data[, i+1]
	flagColumnsToDelete <- c(flagColumnsToDelete, i+1)
        dataNamesOriginal$temperature <- "Temp_insitu-thermistor [degC]"
    }
    ## "time_ISO8601 [YYYY-MM-DDThh:mm]"
    i  <- grep("time_ISO8601 \\[YYYY-MM-DDThh:mm\\]", colNames)
    if (1 == length(i)) {
	colNames[i] <- "time"
	units$date <- list(unit=expression(), scale="")
        dataNamesOriginal$date <- "time_ISO8601 [YYYY-MM-DDThh:mm]"
    }

    ## "YYYY-MM-DDThh:mm:ss.sss" 
    i  <- grep("YYYY\\-MM\\-DDThh:mm:ss.sss", colNames)
    if (1 == length(i)) {
	colNames[i] <- "time"
	units$date <- list(unit=expression(), scale="")
        dataNamesOriginal$date <- "YYYY-MM-DDThh:mm:ss.sss"
    }
    ## "yyyy-mm-ddThh:mm:ss.sss" 
    i  <- grep("yyyy\\-mm\\-ddThh:mm:ss.sss", colNames)
    if (1 == length(i)) {
	colNames[i] <- "time"
	units$date <- list(unit=expression(), scale="")
        dataNamesOriginal$date <- "yyyy-mm-ddThh:mm:ss.sss"
    }
    oceDebug(debug, "post-processed colNames=c(\"", paste(colNames, collapse="\", \""), "\")\n", sep="")
    names(data) <- colNames
    ## Delete the flag columns from 'data', since we have stored them in the metadata
    if (length(flagColumnsToDelete)) {
	data <- data[, -flagColumnsToDelete]
    }
    res@metadata$dataNamesOriginal <- dataNamesOriginal
    res@metadata$units <- units
    res@metadata$flags <- flags
    ## Move some things from data to metadata, if there is only one value and if it
    ## makes sense.  (Basically, in R terms, make it untidy.)
    for (thing in c("Station", "Cruise", "Type", "longitude", "latitude")) {
        if (thing %in% names(data)) {
            THING <- data[[thing]]
            if (is.numeric(THING)) {
                ok <- is.finite(THING)
                if (any(ok)) {
                    THING <- THING[ok]
                    if (all(THING == THING[1])) {
                        res@metadata[tolower(thing)] <- THING[1]
                        data[thing] <- NULL
                    }
                }
            } else if (is.character(THING)) {
                ok <- nchar(THING) > 0
                if (any(ok)) {
                    THING <- THING[ok]
                    if (all(THING == THING[1])) {
                        res@metadata[tolower(thing)] <- THING[1]
                        data[thing] <- NULL
                    }
                }
             }
        }
    }
    res@metadata$type <- ""
    res@data <- as.list(data)
    ## Fix dates
    for (i in grep("^time[\\.0-9]*$", names(data))) {
        D <- gsub(" UTC", "", res@data[[i]])
        res@data[[i]] <- numberAsPOSIXct(unlist(lapply(D, function(t) if (nchar(t) > 0) as.POSIXct(t, tz="UTC") else NA)))
    }
    res@processingLog <- processingLogAppend(res, paste("read.ctd.odv(file=\"", filename, "\", ...)", sep=""))
    oceDebug(debug, "} # read.ctd.odv\n", unindent=1)
    res
}

