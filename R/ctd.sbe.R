#' Decode an SBE variable name from a header line
#'
#' This function is used by \code{\link{read.ctd.sbe}} to infer data names
#' and units from the coding used by Teledyne/Seabird (SBE) \code{.cnv}
#' files.  Lacking access to documentation on the SBE format,
#' the present function is based on inspection of a suite of CNV files
#' available to the \code{oce} developers.
#'
#' A few sample header lines to be decoded are as follows.
#'\preformatted{
#' # name 3 = t090C: Temperature [ITS-90, deg C]
#' # name 4 = t190C: Temperature, 2 [ITS-90, deg C]
#'}
#' Examination of several CNV files suggests that it is best to
#' try to infer the name from the characters between the "\code{=}"
#' and "\code{:}" characters, because the material after the colon
#' seems to vary more between sample files.
#'
#' The table given below indicates the translation patterns used.
#' The \code{.cnv} convention for multiple sensors is to include digits in the
#' name, and these are indicated with \code{N} in the leftmost column of
#' the table below. This maps to \code{M} in the second column in the following
#' way: \code{M} is omitted if \code{N} is zero, or \code{M} equals \code{N}
#" otherwise.
#'
#' \tabular{llll}{
#'   \strong{Key}       \tab \strong{Result}                   \tab \strong{Unit, scale} \tab \strong{Notes} \cr
#'   \code{altM}        \tab \code{altimeter}                  \tab m                    \tab                \cr
#'   \code{cNms/cm}     \tab \code{conductivityM}              \tab ms/cm                \tab                \cr
#'   \code{CStarAtN}    \tab \code{beamAttenuation}            \tab 1/m                  \tab                \cr
#'   \code{CStarTrN}    \tab \code{beamTransmission}           \tab percent              \tab                \cr
#'   \code{depS}        \tab \code{depth}                      \tab m                    \tab                \cr
#'   \code{depSM}       \tab \code{depth}                      \tab m                    \tab                \cr
#'   \code{dz/dtM}      \tab \code{descentRate}                \tab m/s                  \tab                \cr
#'   \code{flag}        \tab \code{flag}                       \tab -                    \tab                \cr
#'   \code{flC}         \tab \code{fluorescenceChelsea}        \tab ug/l                 \tab                \cr
#'   \code{flsP}        \tab \code{fluorescence}               \tab -                    \tab                \cr
#'   \code{latitude}    \tab \code{latitude}                   \tab degN                 \tab                \cr
#'   \code{longitude}   \tab \code{longitude}                  \tab degE                 \tab                \cr
#'   \code{nbin}        \tab \code{nbin}                       \tab -                    \tab                \cr
#'   \code{potempN90C}  \tab \code{thetaM}                     \tab degC, ITS-90         \tab                \cr
#'   \code{pr}          \tab \code{pressure}                   \tab dbar                 \tab                \cr
#'   \code{prDM}        \tab \code{pressure}                   \tab dbar                 \tab 1              \cr
#'   \code{ptempC}      \tab \code{pressureTemperature}        \tab degC, ITS-90         \tab 2              \cr
#'   \code{potempN90C}  \tab \code{thetaM}                     \tab degC, ITS-90         \tab 2              \cr
#'   \code{pumps}       \tab \code{pumpStatus}                 \tab                      \tab                \cr
#'   \code{salNN}       \tab \code{salinityM}                  \tab unitless, PSS-78     \tab 3              \cr
#'   \code{sbeoxNML/L}  \tab \code{oxygenConcentrationVolumeM} \tab ml/l                 \tab                \cr
#'   \code{sbeoxNMm/Kg} \tab \code{oxygenConcentrationMoleM}   \tab ml/l                 \tab                \cr
#'   \code{sbeoxNPs}    \tab \code{oxygenSaturationM}          \tab percent              \tab                \cr
#'   \code{sbeoxNV}     \tab \code{oxygenVoltageM}             \tab ml/l                 \tab                \cr
#'   \code{scan}        \tab \code{scan}                       \tab -                    \tab                \cr
#'   \code{sigma-theta} \tab \code{sigmaTheta}                 \tab kg/m^3               \tab 4              \cr
#'   \code{spar}        \tab \code{spar}                       \tab -                    \tab                \cr
#'   \code{svCM}        \tab \code{soundSpeed}                 \tab m/s                  \tab                \cr
#'   \code{tN68}        \tab \code{temperatureM}               \tab degC, IPTS-68        \tab                \cr 
#'   \code{tN90c}       \tab \code{temperatureM}               \tab degC, ITS-90         \tab                \cr
#'   \code{upolyN}      \tab \code{upolyM}                     \tab -                    \tab                \cr 
#'   \code{vN}          \tab \code{voltageM}                   \tab V                    \tab                \cr 
#'   \code{wetCDOM}     \tab \code{fluorescence}               \tab mg/m^3               \tab                \cr 
#' }
#' Notes:
#' \itemize{
#' \item{1. what should be done if a file has both \code{pr} and \code{prDM}?}
#' \item{2: assume ITS-90 temperature scale, since sample \code{.cnv} file headers do not specify it.}
#' \item{3: some files have PSU for this. Should we handle that? And are there other S scales to consider?}
#' \item{4: 'theta' may appear in different ways with different encoding configurations, set up
#' within R or in the operating system.}
#' }
#'
#' @param h The header line.
#' @return a list containing \code{name} (the oce name), \code{nameOriginal} (the SBE name) and \code{unit}.
#' @author Dan Kelley
#' @family things related to \code{ctd} data
cnvName2oceName <- function(h)
{
    if (length(h) != 1)
        stop("oceNameFromSBE() expects just 1 line of header")
    ## An example, for which the grep is designed, is below.
    ## '# name 4 = t190C: Temperature, 2 [ITS-90, deg C]'
    if (1 != length(grep("^# name [0-9][0-9]* = .*:.*$", h, ignore.case=TRUE)))
        stop("header line does not contain a variable name")
    ## message("h: '", h, "'")
    name <- gsub("^# name [0-9][0-9]* = (.*):.*$", "\\1", h, ignore.case=TRUE)
    nameOriginal <- name
    ## message("name: '", name, "'")
    if (1 == length(grep("altM", name, ignore.case=TRUE))) {
        name <- "altimeter"
        unit <- list(unit=expression(m), scale="")
    } else if (1 == length(grep("c[0-9]mS/cm", name, ignore.case=TRUE))) {
        number <- gsub("mS/cm$", "", gsub("^c", "", name, ignore.case=TRUE), ignore.case=TRUE)
        name <- paste("conductivity", number, sep="")
        unit <- list(unit=expression(mS/cm), scale="")
    } else if (1 == length(grep("CStarTr[0-9]", name, ignore.case=TRUE))) {
        number <- gsub("^CStarTr", "", name, ignore.case=TRUE)
        name <- paste("beamTransmission", number, sep="")
        unit <- list(unit=expression(percent), scale="")
    } else if (1 == length(grep("CStarAt[0-9]", name, ignore.case=TRUE))) {
        number <- gsub("^CStarAt", "", name, ignore.case=TRUE)
        name <- paste("beamAttenuation", number, sep="")
        unit <- list(unit=expression(1/m), scale="")
    } else if (1 == length(grep("depS[M]*", name, ignore.case=TRUE))) {
        name <- "depth"
        unit <- list(unit=expression(m), scale="")
    } else if (1 == length(grep("dz/dt[M]*", name, ignore.case=TRUE))) {
        name <- "descentRate"
        unit <- list(unit=expression(m/s), scale="")
    } else if (1 == length(grep("flag", name, ignore.case=TRUE))) {
        name <- "flag"
        unit <- list(unit=expression(), scale="")
    } else if (1 == length(grep("flC", name, ignore.case=TRUE))) {
        name <- "fluorescenceChelsea"
        unit <- list(unit=expression(mu*g/l), scale="")
    } else if (1 == length(grep("flsP", name, ignore.case=TRUE))) {
        name <- "fluorescence"
        unit <- list(unit=expression(), scale="")
    } else if (1 == length(grep("latitude", name, ignore.case=TRUE))) {
        name <- "latitude"
        unit <- list(unit=expression(degree*N), scale="")
    } else if (1 == length(grep("longitude", name, ignore.case=TRUE))) {
        name <- "longitude"
        unit <- list(unit=expression(degree*E), scale="")
    } else if (name == "nbin") {
        name <- "nbin"
        unit <- list(unit=expression(), scale="")
    } else if (name == "par") {
        name <- "par"
        unit <- list(unit=expression(), scale="")
    } else if (length(grep("pr[m]*", name, ignore.case=TRUE))) {
        name <- "pressure"
        unit <- list(unit=expression(dbar), scale="")
    } else if (1 == length(grep("ptempC", name, ignore.case=TRUE))) {
        name <- "pressureTemperature" # temperature at the pressure sensor
        unit <- list(unit=expression(degree*C), scale="ITS-90") # FIXME: guess on scale
    } else if (1 == length(grep("potemp[0-9]*90C", name, ignore.case=TRUE))) {
        number <- gsub("90C$", "", gsub("^potemp", "", name))
        name <- paste("theta", number, sep="")
        unit <- list(unit=expression(degree*C), scale="ITS-90") # FIXME: guess on scale
    } else if (1 == length(grep("pumps", name, ignore.case=TRUE))) {
        name <- "pumpStatus"
        unit <- list(unit=expression(), scale="")
    } else if (1 == length(grep("sal[0-9]{2}", name, ignore.case=TRUE))) {
        number <- gsub(".$", "", gsub("^sal", "", name))
        name <- paste("salinity", number, sep="")
        unit <- list(unit=expression(), scale="PSS-78") # FIXME: guess on scale
    } else if (1 == length(grep("sbeox[0-9]ML/L", name, ignore.case=TRUE))) {
        number <- gsub("ML/L$", "", gsub("^sbeox", "", name, ignore.case=TRUE), ignore.case=TRUE)
        name <- paste("oxygenConcentrationVolume", number, sep="")
        unit <- list(unit=expression(ml/l), scale="")
    } else if (1 == length(grep("sbeox[0-9]Mm/Kg", name, ignore.case=TRUE))) {
        number <- gsub("Mm/Kg$", "", gsub("^sbeox", "", name, ignore.case=TRUE), ignore.case=TRUE)
        name <- paste("oxygenConcentrationMole", number, sep="")
        unit <- list(unit=expression(mu*mol/kg), scale="")
    } else if (1 == length(grep("sbeox[0-9]PS", name, ignore.case=TRUE))) {
        number <- gsub("PS$", "", gsub("^sbeox", "", name, ignore.case=TRUE), ignore.case=TRUE)
        name <- paste("oxygenSaturation", number, sep="")
        unit <- list(unit=expression(percent), scale="")
    } else if (1 == length(grep("sbeox[0-9]V", name, ignore.case=TRUE))) {
        number <- gsub("V$", "", gsub("^sbeox", "", name, ignore.case=TRUE), ignore.case=TRUE)
        name <- paste("oxygenVoltage", number, sep="")
        unit <- list(unit=expression(V), scale="")
    } else if (1 == length(grep("scan", name, ignore.case=TRUE))) {
        name <- "scan"
        unit <- list(unit=expression(), scale="")
    } else if (1 == length(grep("sigma-.*[0-9]*", name, ignore.case=TRUE))) {
        number <- substr(gsub(".*(\\d+).*", "\\1", name, ignore.case=TRUE), 1, 1)
        name <- paste("sigmaTheta", number, sep="")
        unit <- list(unit=expression(kg/m^3), scale="")
    } else if (1 == length(grep("spar", name, ignore.case=TRUE))) {
        name <- "spar"
        unit <- list(unit=expression(), scale="")
    } else if (1 == length(grep("svCM", name, ignore.case=TRUE))) {
        name <- "soundSpeed"
        unit <- list(unit=expression(m/s), scale="")
    } else if (1 == length(grep("t[0-9]68", name, ignore.case=TRUE))) {
        number <- gsub("68$", "", gsub("^t", "", name))
        name <- paste("temperature", number, sep="")
        unit <- list(unit=expression(degree*C), scale="IPTS-68")
    } else if (1 == length(grep("t[0-9]90c", name, ignore.case=TRUE))) {
        number <- gsub("90c$", "", gsub("^t", "", name, ignore.case=TRUE), ignore.case=TRUE)
        name <- paste("temperature", number, sep="")
        unit <- list(unit=expression(degree*C), scale="ITS-90")
    } else if (1 == length(grep("timeS", name, ignore.case=TRUE))) {
        name <- "time"
        unit <- list(unit=expression(s), scale="")
    } else if (1 == length(grep("upoly[0-9]+", name, ignore.case=TRUE))) {
        name <- paste("upoly", gsub("upoly", "", name), sep="")
        unit <- list(unit=expression(), scale="")
    } else if (1 == length(grep("v[0-9]+", name, ignore.case=TRUE))) {
        name <- paste("v", gsub("v", "", name), sep="")
        unit <- list(unit=expression(V), scale="")
    } else if (1 == length(grep("wetCDOM", name, ignore.case=TRUE))) {
        name <- "fluorescence"
        unit <- list(unit=expression(mg/m^3), scale="")
    } else {
        warning("unrecognized name '", name, "', so not setting any unit")
        unit <- list(unit=expression(), scale="")
    }
    ## Finally, drop any zero suffices, so that e.g. salinity0 becomes
    ## salinity.
    if (1 == length(grep("0$", name)))
        name <- substr(name, 1, nchar(name)-1)
    ## message(" name: '", name, "', nameOriginal: '", nameOriginal, '"')
    list(name=name, nameOriginal=nameOriginal, unit=unit)
}


#' Read an Seabird \code{ctd} file.
#'
#' Read a Teledyne/Seabird CTD file, i.e. a file with name ending in \code{.cnv}.
#' @template readCtdTemplate
#'
#' @details
#' \code{read.ctd.sbe} reads files stored in Seabird \code{.cnv} format.
#' Note that these files can contain multiple sensors for a given field. For example,
#' the file might contain a column named \code{t090C} for one 
#' temperature sensor and \code{t190C} for a second. The first will be denoted
#' \code{temperature} in the \code{data} slot of the return value, and the second
#' will be denoted \code{temperature1}. This means that the first sensor
#' will be used in any future processing that accesses \code{temperature}. This
#' is for convenience of processing, and it does not pose a limitation, because the
#' data from the second sensor are also available as e.g. \code{x[["temperature1"]]},
#' where \code{x} is the name of the returned value.  For the details of the 
#' mapping from \code{.cnv} names to \code{ctd} names, see \code{\link{cnvName2oceName}}.
#'
#' The original variable names as stored in \code{file} are stored within the \code{data}
#' slot as attribute named \code{namesOriginal}, and so can be retrieved with e.g.
#' \code{attr(x@data, "namesOriginal")}, if \code{x} is the object returned by
#' this function. This feature can be helpful in detailed work.  See the Appendix
#' VI of [2], noting that the \code{namesOriginal} values are taken from
#' the "Short Name" column of the table spanning pages 161 through 172.
#'
#' @references
#' 1. The Sea-Bird SBE 19plus profiler is described at
#' \url{http://www.seabird.com/products/spec_sheets/19plusdata.htm}.  Some more
#' information is given in the Sea-Bird data-processing manaual
#' \url{http://www.seabird.com/old-manuals/Software_Manuals/SBE_Data_Processing/SBEDataProcessing_7.20g.pdf}.
#'
#' 2. A SBE data processing manual is at \url{http://www.seabird.com/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.seabird.com/sites/default/files/documents/SBEDataProcessing_7.25.0.pdf&nid=1320}.
read.ctd.sbe <- function(file, columns=NULL, station=NULL, missing.value,
                         monitor=FALSE, debug=getOption("oceDebug"), processingLog, ...)
{
    if (!is.null(columns)) {
        warning("'columns' argument is defunct, and will be removed in the next release of oce\n")
        ## columnsNames <- names(columns)
        ## if (!("temperature" %in% columnsNames)) stop("'columns' must contain 'temperature'")
        ## if (!("pressure" %in% columnsNames)) stop("'columns' must contain 'pressure'")
        ## if (!("salinity" %in% columnsNames)) stop("'columns' must contain 'salinity'")
        ## if (3 > length(columns)) stop("'columns' must contain three or more elements")
    }

    if (length(grep("\\*", file, ignore.case=TRUE))) {
        oceDebug(debug, "read.ctd.sbe(file=\"", file, "\") { # will read a series of files\n", unindent=1)
        files <- list.files(pattern=file)
        nfiles <- length(files)
        if (monitor)
            pb <- txtProgressBar(1, nfiles, style=3)
        res <- vector("list", nfiles)
        for (i in 1:nfiles) {
            res[[i]] <- read.ctd.sbe(files[i], debug=debug-1)
            if (monitor)
                setTxtProgressBar(pb, i)
        }
        oceDebug(debug, "} # read.ctd.sbe() {\n")
        return(res)
    }
    oceDebug(debug, "read.ctd.sbe(file=\"", file, "\") {\n", unindent=1)

    ## Read Seabird data file.  Note on headers: '*' is machine-generated,
    ## '**' is a user header, and '#' is a post-processing header.
    filename <- ""
    if (is.character(file)) {
        filename <- fullFilename(file)
        file <- file(file, "r")
        on.exit(close(file))
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }
    res <- new("ctd", pressureType="sea")
    ## Header
    scientist <- ship <- institute <- address <- cruise <- hexfilename <- ""
    sampleInterval <- NA
    systemUploadTime <- NULL
    latitude <- longitude <- NA
    startTime <- NULL
    waterDepth <- NA
    date <- recovery <- NA
    header <- c()
    ##conductivity.standard <- 4.2914
    found.header.latitude <- found.header.longitude <- FALSE
    serialNumber <- serialNumberConductivity <- serialNumberTemperature <- ""
    ## units$conductivity <- list(unit=expression(), scale="") # guess; other types are "mS/cm" and "S/m"
    ## units$temperature <- list(unit=expression(degree*C), scale="ITS-90") # guess; other option is IPTS-68
    pressureType = "sea"               # guess; other option is "absolute"

    lines <- readLines(file, encoding="UTF-8")

    ## Get names and units of columns in the SBE data file
    nameLines  <- grep("^# name [0-9][0-9]* = .*:.*$", lines, ignore.case=TRUE)
    units <- list()
    col.names.inferred <- col.names.original <- NULL
    for (iline in nameLines) {
        nu <- cnvName2oceName(lines[iline])
        col.names.inferred <- c(col.names.inferred, nu$name)
        col.names.original <- c(col.names.original, nu$nameOriginal)
        units[[nu$name]] <- nu$unit
    }
    found.scan <- "scan" %in% col.names.inferred
    found.temperature <- "temperature" %in% col.names.inferred
    found.pressure <- "pressure" %in% col.names.inferred
    found.salinity <- "salinity" %in% col.names.inferred
    found.time <- "time" %in% col.names.inferred
    found.depth <- "depth" %in% col.names.inferred
    found.conductivity <- "conductivity" %in% col.names.inferred
    found.conductivity.ratio <- "conductivity.ratio" %in% col.names.inferred
    ## FIXME: should we insist on having salinity, temperature, and pressure?

    for (iline in seq_along(lines)) {
        line <- lines[iline]
        #line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
        oceDebug(debug>1, paste("Examining header line '",line,"'\n", sep=""))
        header <- c(header, line)
        ##if (length(grep("\*END\*", line))) #BUG# why is this regexp no good (new with R-2.1.0)
        aline <- iconv(line, from="UTF-8", to="ASCII", sub="?")
        if (length(grep("END", aline, perl=TRUE, useBytes=TRUE))) {
            ## Sometimes SBE files have a header line after the *END* line.
            iline <- iline + 1
            if (length(grep("[a-cf-zA-CF-Z]", lines[iline])))
                iline <- iline + 1
            break
        }
        lline <- tolower(aline)
        ##> BUG: discovery of column names is brittle to format changes
        ##> if (0 < (r <- regexpr("# name ", lline))) {
        ##>     oceDebug(debug, "lline: '",lline,"'\n",sep="")
        ##>     tokens <- strsplit(line, split=" ", useBytes=TRUE)
        ##>     name <- tokens[[1]][6]
        ##>     name <- gsub(",", "", name)
        ##>     oceDebug(debug, "  name: '",name,"'\n",sep="")
        ##>     if (0 < regexpr("scan", lline)) {
        ##>         name <- "scan"
        ##>         found.scan <- TRUE
        ##>     } else if (0 < regexpr("pressure", lline)) {
        ##>         if (0 > regexpr("deg c", lline)) {
        ##>             ## ignore "# name 5 = ptempC: Pressure Temperature [deg C]"
        ##>             name <- "pressure"
        ##>             found.pressure <- TRUE
        ##>         }
        ##>     } else if (0 < regexpr("time", lline)) {
        ##>         name <- "time"
        ##>         found.time <- TRUE
        ##>     } else if (0 < regexpr("salinity", lline)) {
        ##>         name <- "salinity"
        ##>         found.salinity <- TRUE
        ##>     } else if (0 < regexpr("temperature", lline)) {
        ##>         ## ignore "# name 5 = ptempC: Pressure Temperature [deg C]"
        ##>         if (0 > regexpr("pressure", lline) && 0 > regexpr("potential", lline)) {
        ##>             name <- "temperature"
        ##>             found.temperature <- TRUE
        ##>             unit <- gsub(":.*","",gsub(".*=[ ]*","", line))
        ##>             if (length(grep("68", unit)))
        ##>                 units$temperature <- list(unit=expression(degree*C), scale="IPTS-68")
        ##>             else if (length(grep("90", unit)))
        ##>                 units$temperature <- list(unit=expression(degree*C), scale="ITS-90")
        ##>             oceDebug(debug, "temperatureUnit: ", as.character(units$temperature$unit$expression), "(inferred from '", unit, "')\n", sep="")
        ##>         }
        ##>     } else if (0 < regexpr("conductivity", lline)) {
        ##>         if (0 < regexpr("ratio", lline)) {
        ##>             found.conductivity.ratio <- TRUE;
        ##>             name <- "conductivityratio"
        ##>             units$conductivity <-list(unit=expression(), scale="")
        ##>         } else {
        ##>             found.conductivity <- TRUE;
        ##>             name <- "conductivity"
        ##>             unit <- gsub(":.*","",gsub(".*=[ ]*","", line))
        ##>             if (length(grep("S/m", unit)))
        ##>                 units$conductivity <- list(unit="S/m", scale="")
        ##>             else if (length(grep("mS/cm", unit)))
        ##>                 units$conductivity <- list(unit="mS/cm", scale="")
        ##>         }
        ##>     } else if (0 < regexpr("depth", lline) || 0 < regexpr("depSM", lline)) {
        ##>         name <- "depth"
        ##>         found.depth <- TRUE
        ##>     } else if (0 < regexpr("fluorometer", lline)) {
        ##>         name <- "fluorometer"
        ##>     ## Used to have oxygen.temperature and oxygen.current here (why??)
        ##>     } else if (0 < regexpr("oxygen", lline)) {
        ##>         name <- "oxygen"
        ##>     } else if (0 < regexpr("flag", lline)) {
        ##>         name <- "flag"
        ##>     } else if (0 < regexpr("sigma-theta", lline)) {
        ##>         name <- "sigmaTheta"
        ##>         ##foundSigmaTheta <- TRUE
        ##>     } else if (0 < regexpr("sigma-t", lline)) {
        ##>         name <- "sigmat"
        ##>         ##foundSigmaT <- TRUE
        ##>     } else {
        ##>         oceDebug(debug>1, "unrecognized item\n")
        ##>     }
        ##>     col.names.inferred <- c(col.names.inferred, name)
        ##> }
        ##> oceDebug(debug>1, "col.names.inferred=c(\"", paste(col.names.inferred, collapse='","'), "\")", sep="")

        if (0 < regexpr(".*seacat profiler.*", lline))
            serialNumber <- gsub("[ ].*$","",gsub(".*sn[ ]*","",lline))
        if (length(grep("^\\* Temperature SN", lline, ignore.case=TRUE)))
            serialNumberTemperature <- gsub("^.*=\\s", "", lline)
        if (length(grep("^\\* Conductivity SN", lline, ignore.case=TRUE)))
            serialNumberConductivity <- gsub("^.*=\\s", "", lline)
        if (0 < (r<-regexpr("date:", lline))) {
            d <- sub("(.*)date:([ ])*", "", lline)
            date <- decodeTime(d, "%Y%m%d") # e.g. 20130701 Canada Day
        }
        ##* NMEA UTC (Time) = Jul 28 2011  04:17:53
        ##* system upload time = jan 26 2010 13:02:57
        if (length(grep("^\\* .*time.*=.*$", lline))) {
            if (0 == length(grep("real-time sample interval", lline))) {
                d <- sub(".*=", "", lline)
                d <- sub("^ *", "", d)
                d <- sub(" *$", "", d)
                date <- decodeTime(d)
            }
        }
        if (0 < (r<-regexpr("filename", lline)))
            hexfilename <- sub("(.*)FileName =([ ])*", "", ignore.case=TRUE, lline)
        if (0 < (r<-regexpr("system upload time", lline))) {
            d <- sub("([^=]*)[ ]*=[ ]*", "", ignore.case=TRUE, lline)
            systemUploadTime <- decodeTime(d)
            oceDebug(debug, " systemUploadTime ", format(systemUploadTime), " inferred from substring '", d, "'\n", sep="")
        }
        ## Styles:
        ## * NMEA Latitude = 47 54.760 N
        ## ** Latitude:      47 53.27 N
        if (!found.header.latitude && (0 < (r<-regexpr("latitude*[0-8]*", lline, ignore.case=TRUE)))) {
            latitude <- parseLatLon(lline, debug=debug-1)
            found.header.latitude <- TRUE
        }
        if (!found.header.longitude && (0 < (r<-regexpr("longitude*[0-8]*", lline, ignore.case=TRUE)))) {
            longitude <- parseLatLon(lline, debug=debug-1)
            found.header.longitude <- TRUE
        }
        if (0 < (r<-regexpr("start_time =", lline))) {
            d <- sub("#[ ]*start_time[ ]*=[ ]*", "", lline)
            startTime <- decodeTime(d)
            oceDebug(debug, " startTime ", format(startTime), "' inferred from substring '", d, "'\n", sep="")
        }
        if (0 < (r<-regexpr("ship:", lline))) {
            ship <- sub("(.*)ship:([ \t])*", "", ignore.case=TRUE, line) # note: using full string
            ship <- sub("[ \t]*$", "", ship)
        }
        if (0 < (r<-regexpr("scientist:", lline)))
            scientist <- sub("(.*)scientist:([ ])*", "", ignore.case=TRUE, line) # full string
        if (0 < (r<-regexpr("chef", lline)))
            scientist <- sub("(.*):([ ])*", "", ignore.case=TRUE, line) # full string
        if (0 < (r<-regexpr("institute:", lline)))
            institute <- sub("(.*)institute:([ ])*", "", ignore.case=TRUE, line) # full string
        if (0 < (r<-regexpr("address:", lline)))
            address <- sub("(.*)address:([ ])*", "", ignore.case=TRUE, line) # full string
        if (0 < (r<-regexpr("cruise:", lline))) {
            cruise <- sub("(.*)cruise:([ ])*", "", ignore.case=TRUE, line) # full string
            cruise <- sub("[ ]*$", "", ignore.case=TRUE, cruise) # full string
        }
        if (is.null(station)) {
            if (0 < (r<-regexpr("station:", lline)))
                station <- sub("[ ]*$", "", sub("(.*)station:([ ])*", "", ignore.case=TRUE, line)) # full string
        }
        if (0 < (r<-regexpr("recovery:", lline)))
            recovery <- sub("(.*)recovery:([ ])*", "", lline)
        if (0 < (r<-regexpr("depth", lline))) { # "** Depth (m): 3447 "
            look <- sub("[a-z:()]*", "", lline, ignore.case=TRUE)
            look <- gsub("^[*a-zA-Z\\(\\) :]*", "", lline, ignore.case=TRUE)
            look <- gsub("[ ]*", "", look, ignore.case=TRUE)
            oceDebug(debug, " trying to get water depth from '", lline, "', reduced to '", look, "'\n", sep="")
            if (!length(grep('[a-zA-Z]', look))) {
                waterDepth<- as.numeric(look)
                oceDebug(debug, "got waterDepth: ", waterDepth, "\n")
            }
        }
        if (0 < (r<-regexpr("water depth:", lline))
            || 0 < (r<-regexpr(pattern="profondeur", text=lline))) {
            ## Examples from files in use by author:
            ##** Profondeur: 76
            ##** Water Depth:   40 m
            look <- sub("[ ]*$", "", sub(".*:[ ]*", "", lline))
            linesplit <- strsplit(look," ")[[1]]
            nitems <- length(linesplit)
            if (nitems == 1) {
                waterDepth <- as.numeric(linesplit[1])
            } else if (nitems == 2) {
                unit <- linesplit[2]
                if (unit == "m") {
                    waterDepth <- as.numeric(linesplit[1])
                } else if (unit == "km") {
                    waterDepth <- 1000 * as.numeric(linesplit[1])
                } else {
                    warning("ignoring unit on water depth '", look, "'")
                }
            } else {
                stop("cannot interpret water depth from '", lline, "'")
            }
        }
        if (0 < (r<-regexpr("^. sample rate =", lline))) {
            ## * sample rate = 1 scan every 5.0 seconds
            rtmp <- lline;
            rtmp <- sub("(.*) sample rate = ", "", rtmp)
            rtmp <- sub("scan every ", "", rtmp)
            rtmp <- strsplit(rtmp, " ")
            ##      if (length(rtmp[[1]]) != 3)
            ##        warning("cannot parse sample-rate string in `",line,"'")
            sampleInterval <- as.double(rtmp[[1]][2]) / as.double(rtmp[[1]][1])
            if (rtmp[[1]][3] == "seconds") {
                ;
            } else {
                if (rtmp[[1]][3] == "minutes") {
                    sampleInterval <- sampleInterval / 60;
                } else {
                    if (rtmp[[1]][3] == "hours") {
                        sampleInterval <- sampleInterval / 3600;
                    } else {
                        warning("cannot understand `",rtmp[[1]][2],"' as a unit of time for sampleInterval")
                    }
                }
            }
        }
    }
    oceDebug(debug, "Finished reading header\n")
    if (debug > 0) {
        if (is.nan(sampleInterval))
            warning("'* sample rate =' not found in header")
        if (is.nan(latitude))
            warning("'** Latitude:' not found in header")
        if (is.nan(longitude))
            warning("'** Longitude:' not found in header")
        if (is.null(date))
            warning("'** Date:' not found in header")
        if (is.null(recovery))
            warning("'** Recovery' not found in header")
    }
    ## Require p,S,T data at least
    if (!found.temperature)
        stop("cannot find 'temperature' in this file")
    if (!found.pressure && !found.depth)
        stop("no column named 'pressure', 'depth' or 'depSM'")

    res@metadata$header <- header
    res@metadata$type <- "SBE"
    res@metadata$hexfilename <- hexfilename # from instrument
    res@metadata$serialNumber <- serialNumber
    res@metadata$serialNumberTemperature <- serialNumberTemperature
    res@metadata$serialNumberConductivity <- serialNumberConductivity
    res@metadata$pressureType <- pressureType
    res@metadata$units <- units
    res@metadata$systemUploadTime <- systemUploadTime
    res@metadata$ship <- ship
    res@metadata$scientist <- scientist
    res@metadata$institute <- institute
    res@metadata$address <- address
    res@metadata$cruise <- cruise
    res@metadata$station <- station
    res@metadata$deploymentType <- "unknown"
    res@metadata$date <- date
    res@metadata$startTime <- startTime
    res@metadata$latitude <- latitude
    res@metadata$longitude <- longitude
    res@metadata$recovery <- recovery
    res@metadata$waterDepth <- waterDepth # if NA, will update later
    res@metadata$sampleInterval <- sampleInterval
    res@metadata$names <- col.names.inferred
    res@metadata$labels <- col.names.inferred
    res@metadata$filename <- filename
    ## Read the data as a table.
    ## FIXME: should we match to standardized names?
    ##col.names.forced <- c("scan","pressure","temperature","conductivity","descent","salinity","sigmaThetaUnused","depth","flag")

    ##> ## Handle similar names by tacking numbers on the end, e.g. the first column that
    ##> ## is automatically inferred to hold temperature is called "temperature", while the
    ##> ## next one is called "temperature2", and a third would be called "temperature3".
    ##> ## col.names.inferred <- tolower(col.names.inferred)
    ##> for (uname in unique(col.names.inferred)) {
    ##>     w <- which(uname == col.names.inferred)
    ##>     lw <- length(w)
    ##>     ##message("uname:", uname, ", lw: ", lw)
    ##>     if (1 != lw) {
    ##>         col.names.inferred[w[-1]] <- paste(uname, seq.int(2, lw), sep="")
    ##>     }
    ##> }
    pushBack(lines, file)
    if (is.null(columns)) {
        oceDebug(debug, "About to read these names: c(\"", paste(col.names.inferred, collapse='","'),"\")\n", sep="")
        data <- as.list(read.table(file, skip=iline-1, header=FALSE))
        if (length(data) != length(col.names.inferred))
            stop("Number of columns in .cnv data file does not equal number of named variables")
        names(data) <- col.names.inferred
        ## data <- as.list(read.table(text=lines[seq.int(iline, length(lines))],
        ##                            header=FALSE, col.names=col.names.inferred))
        ndata <- length(data[[1]])
        if (0 < ndata) {
            haveData <- TRUE
            names <- names(data)
            ##labels <- names
            if (!found.scan) {
                data$scan <- 1:ndata
                names <- names(data)
                col.names.inferred <- c(col.names.inferred, "scan")
                col.names.original <- c(col.names.original, "scan")
            }
        } else {
            haveData <- FALSE
            warning("no data in CTD file \"", filename, "\"\n")
            data <- list(scan=NULL, salinity=NULL, temperature=NULL, pressure=NULL)
        }
        attributes(data) <- list(names=col.names.inferred, namesOriginal=col.names.original)
        ## FIXME: should the above attributes() call be later, also? (see next block)
    } else {
        dataAll <- read.table(file, skip=iline-1, header=FALSE, col.names=col.names.inferred)
        ## dataAll <- read.table(text=lines[seq.int(iline, length(lines))],
        ##                       header=FALSE, col.names=col.names.inferred)
        if ("scan" %in% names(columns)) {
            data <- dataAll[, as.numeric(columns)]
            names(data) <- names(columns)
        } else {
            data <- cbind(seq.int(1, dim(dataAll)[1]), dataAll[, as.numeric(columns)])
            names(data) <- c("scan", names(columns))
        }
        data <- as.list(data)
        ndata <- length(data[[1]])
        haveData <- ndata > 0
    }
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    ##hitem <- processingLogItem(processingLog)
    res@data <- data
    ## Add standard things, if missing
    if (haveData) {
        if (!found.salinity) {
            if (found.conductivity.ratio) {
                warning("cannot find 'salinity' in this file; calculating from T, conductivity ratio, and p")
                C <- data$conductivityratio
                cmax <- max(C, na.rm=TRUE)
                if (cmax > 5) {
                    warning("max(conductivity) > 5, so dividing by 42.914 before computing S. However, the original data are left in the object.")
                    C <- C / 42.914
                } else if (cmax > 1) {
                    warning("max(conductivity) between 1 and 5, so dividing by 4.2914 before computing S. However, the original data are left in the object.")
                    C <- C / 4.2914
                }
                S <- swSCTp(C, data$temperature, data$pressure)
            } else if (found.conductivity) {
                warning("cannot find 'salinity' in this file; calculating from T, conductivity, and p")
                C <- data$conductivity
                cmax <- max(C, na.rm=TRUE)
                if (cmax > 5) {
                    warning("max(conductivity) > 5, so dividing by 42.914 before computing S. However, the original data are left in the object.")
                    C <- C / 42.914
                } else if (cmax > 1) {
                    warning("max(conductivity) between 1 and 5, so dividing by 4.2914 before computing S. However, the original data are left in the object.")
                    C <- C / 4.2914
                }
                S <- swSCTp(C, data$temperature, data$pressure)
            } else {
                stop("cannot find salinity in this file, nor conductivity or conductivity ratio")
            }
            res <- ctdAddColumn(res, S, name="salinity", label="Salinity",
                                unit=c(unit=expression(), scale="PSS-78"), debug=debug-1)
        }
        if (found.depth && !found.pressure) { # BUG: this is a poor, nonrobust approximation of pressure
            g <- if (found.header.latitude) gravity(latitude) else 9.8
            rho0 <- 1000 + swSigmaTheta(median(res@data$salinity), median(res@data$temperature), 0)
            res <- ctdAddColumn(res, res@data$depth * g * rho0 / 1e4, name="pressure", label="Pressure",
                                unit=list(unit=expression("dbar"), scale=""), debug=debug-1)
            warning("created a pressure column from the depth column\n")
        }
        ## res <- ctdAddColumn(res, swSigmaTheta(res@data$salinity, res@data$temperature, res@data$pressure),
        ##                 name="sigmaTheta", label="Sigma Theta", unit=list(unit=expression(kg/m^3), scale=""),
        ##                 debug=debug-1)
    }
    ## waterDepthWarning <- FALSE
    ## if (is.na(res@metadata$waterDepth)) {
    ##     res@metadata$waterDepth <- max(abs(res@data$pressure), na.rm=TRUE)
    ##     waterDepthWarning <- TRUE
    ## }
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    ## update to temperature IPTS-90, if have an older version
    if (2 == length(res@metadata$unit$temperature) &&
        "IPTS-68" == as.character(res@metadata$units$temperature$scale)) {
        res@data$temperature68 <- res@data$temperature
        res@metadata$units$temperature68 <- list(unit=expression(degree*C), scale="IPTS-68")
        res@data$temperature <- T90fromT68(res@data$temperature68)
        res@metadata$units$temperature <- list(unit=expression(degree*C), scale="ITS-90")
        warning("converted temperature from IPTS-68 to ITS-90")
        res@processingLog <- processingLogAppend(res@processingLog, "converted temperature from IPTS-68 to ITS-90")
    }
    if (!("salinity" %in% names(res@metadata$units))) res@metadata$units$salinity <- list(unit=expression(), scale="PSS-78")
    if (!("pressure" %in% names(res@metadata$units))) res@metadata$units$pressure <- list(unit=expression(dbar), scale="")
    if (!("depth" %in% names(res@metadata$units))) res@metadata$units$depth <- list(unit=expression(m), scale="")
    oceDebug(debug, "} # read.ctd.sbe()\n")
    ## if (waterDepthWarning)
    ##     res@processingLog <- processingLogAppend(res@processingLog, "inferred water depth from maximum pressure")
    res
}

