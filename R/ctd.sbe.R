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
#' The table given below indicates the translation patterns used. These are 
#' taken from [1]. The \code{.cnv} convention for multiple sensors is to
#' include optional extra digits in the name, and these are indicated
#' with \code{_} in the table; their decoding is done with \code{\link{grep}}.
#'
#' It is important to note that this table is by no means complete, since there
#' are a great many SBE names listed in their document [1], plus names 
#' not listed there but present in data files
#' supplied by prominent archiving agencies. If an SBE name is not recogized,
#' then the oce name is set to that SBE name. This can cause problems in
#' some other processing steps (e.g. if \code{\link{swRho}} or a similar
#' function is called with an \code{oce} object as first argument), and so
#' users are well-advised to rename the items as appropriate. The first
#' step in doing this is to pass the object to \code{summary()}, to discover
#' the SBE names in question. Then consult the SBE documentation to find
#' what the data represent. Finally, either manipulate the names in the object
#' data slot directly or, usually better, use 
#' \code{\link{renameData}} to rename the elements.
#'
#' \tabular{llll}{
#'   \strong{Key}       \tab \strong{Result}                     \tab \strong{Unit, scale} \tab \strong{Notes} \cr
#'   \code{alt_}        \tab \code{altimeter}                    \tab m                    \tab   \cr
#'   \code{c_ms/cm}     \tab \code{conductivity}                 \tab ms/cm                \tab   \cr
#'   \code{CStarAt_}    \tab \code{beamAttenuation}              \tab 1/m                  \tab   \cr
#'   \code{CStarTr_}    \tab \code{beamTransmission}             \tab percent              \tab   \cr
#'   \code{depS}        \tab \code{depth}                        \tab m                    \tab   \cr
#'   \code{depSM}       \tab \code{depth}                        \tab m                    \tab   \cr
#'   \code{dz/dtM}      \tab \code{descentRate}                  \tab m/s                  \tab   \cr
#'   \code{flag}        \tab \code{flag}                         \tab -                    \tab   \cr
#'   \code{flC}         \tab \code{fluorescence}                 \tab ug/l, Chelsea        \tab   \cr
#'   \code{flC_}        \tab \code{fluorescence}                 \tab ug/l, Chelsea        \tab   \cr
#'   \code{flEC-AFLM}   \tab \code{fluorescence}                 \tab mg/m^3               \tab   \cr
#'   \code{flsP}        \tab \code{fluorescence}                 \tab -                    \tab   \cr
#'   \code{latitude}    \tab \code{latitude}                     \tab degN                 \tab   \cr
#'   \code{longitude}   \tab \code{longitude}                    \tab degE                 \tab   \cr
#'   \code{nbin}        \tab \code{nbin}                         \tab -                    \tab   \cr
#'   \code{oxsatML/L}   \tab \code{oxygen}                       \tab ml/l, Weiss          \tab   \cr
#'   \code{oxsatMg/L}   \tab \code{oxygen}                       \tab mg/l, Weiss          \tab   \cr
#'   \code{oxsatMm/Kg}  \tab \code{oxygen}                       \tab umol/kg, Weiss       \tab   \cr
#'   \code{oxsolML/L}   \tab \code{oxygen}                       \tab ml/l, Garcia-Gordon  \tab   \cr
#'   \code{oxsolMg/L}   \tab \code{oxygen}                       \tab mg/l, Garcia-Gordon  \tab   \cr
#'   \code{oxsolMm/Kg}  \tab \code{oxygen}                       \tab umol/kg, Garcia-Gordon \tab   \cr
#'   \code{potemp_68C}  \tab \code{thetaM}                       \tab degC, IPTS-68        \tab   \cr
#'   \code{potemp_90C}  \tab \code{thetaM}                       \tab degC, ITS-90         \tab   \cr
#'   \code{pr}          \tab \code{pressure}                     \tab dbar                 \tab   \cr
#'   \code{prDM}        \tab \code{pressure}                     \tab dbar                 \tab 1 \cr
#'   \code{ptempC}      \tab \code{pressureTemperature}          \tab degC, ITS-90         \tab 2 \cr
#'   \code{pumps}       \tab \code{pumpStatus}                   \tab                      \tab   \cr
#'   \code{sal__}       \tab \code{salinityM}                    \tab unitless, PSS-78     \tab 3 \cr
#'   \code{sbeox_ML/L}  \tab \code{oxygen}                       \tab ml/l                 \tab   \cr
#'   \code{sbeox_Mm/Kg} \tab \code{oxygen}                       \tab ml/l                 \tab   \cr
#'   \code{sbeox_Ps}    \tab \code{oxygen}                       \tab percent              \tab   \cr
#'   \code{sbeox_V}     \tab \code{oxygenRaw}                    \tab V                    \tab   \cr
#'   \code{scan}        \tab \code{scan}                         \tab -                    \tab   \cr
#'   \code{sigma-theta} \tab \code{sigmaTheta}                   \tab kg/m^3               \tab 4 \cr
#'   \code{spar}        \tab \code{spar}                         \tab -                    \tab   \cr
#'   \code{svCM}        \tab \code{soundSpeed}                   \tab m/s                  \tab   \cr
#'   \code{t_68}        \tab \code{temperature}                  \tab degC, IPTS-68        \tab   \cr 
#'   \code{t_90}        \tab \code{temperature}                  \tab degC, ITS-90         \tab   \cr 
#'   \code{t_68C}       \tab \code{temperature}                  \tab degC, ITS-90         \tab   \cr 
#'   \code{t_90C}       \tab \code{temperature}                  \tab degC, ITS-90         \tab   \cr
#'   \code{t090Cm}      \tab \code{temperature}                  \tab degC, ITS-90         \tab   \cr
#'   \code{t4990C}      \tab \code{temperature}                  \tab degC, ITS-90         \tab   \cr
#'   \code{tnc90C}      \tab \code{temperature}                  \tab degC, ITS-90         \tab   \cr
#'   \code{tv290C}      \tab \code{temperature}                  \tab degC, ITS-90         \tab   \cr
#'   \code{t068C}       \tab \code{temperature}                  \tab degC, IPTS-68        \tab   \cr
#'   \code{t4968C}      \tab \code{temperature}                  \tab degC, IPTS-68        \tab   \cr
#'   \code{tnc68C}      \tab \code{temperature}                  \tab degC, IPTS-68        \tab   \cr
#'   \code{tv268C}      \tab \code{temperature}                  \tab degC, IPTS-68        \tab   \cr
#'   \code{t190C}       \tab \code{temperature}                  \tab degC, ITS-90         \tab   \cr
#'   \code{tnc290C}     \tab \code{temperature}                  \tab degC, ITS-90         \tab   \cr
#'   \code{t168C}       \tab \code{temperature}                  \tab degC, IPTS-68        \tab   \cr
#'   \code{tnc268C}     \tab \code{temperature}                  \tab degC, IPTS-68        \tab   \cr
#'   \code{t3890C}      \tab \code{temperature}                  \tab degC, ITS-90         \tab   \cr
#'   \code{t38_90C}     \tab \code{temperature}                  \tab degC, ITS-90         \tab   \cr
#'   \code{t3868C}      \tab \code{temperature}                  \tab degC, IPTS-68        \tab   \cr
#'   \code{t38_38C}     \tab \code{temperature}                  \tab degC, IPTS-68        \tab   \cr
#'   \code{upoly_}      \tab \code{upoly}                        \tab -                    \tab                \cr 
#'   \code{v_}          \tab \code{voltage}                      \tab V                    \tab                \cr 
#'   \code{wetCDOM}     \tab \code{fluorescence}                 \tab mg/m^3               \tab                \cr 
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
#' @template debugTemplate
#' @return a list containing \code{name} (the oce name), \code{nameOriginal} (the SBE name) and \code{unit}.
#' @author Dan Kelley
#' @references
#' 1. A SBE data processing manual is at \url{http://www.seabird.com/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.seabird.com/sites/default/files/documents/SBEDataProcessing_7.25.0.pdf&nid=1320}.
#'
#' @family things related to \code{ctd} data
cnvName2oceName <- function(h, debug=getOption("oceDebug"))
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
    if (1 == length(grep("altM", name, ignore.case=TRUE))) {
        name <- "altimeter"
        unit <- list(unit=expression(m), scale="")
    } else if (1 == length(grep("c[0-9]mS/cm", name, ignore.case=TRUE))) {
        name <- "conductivity"
        unit <- list(unit=expression(mS/cm), scale="")
    } else if (1 == length(grep("CStarTr[0-9]", name, ignore.case=TRUE))) {
        name <- "beamTransmission"
        unit <- list(unit=expression(percent), scale="")
    } else if (1 == length(grep("CStarAt[0-9]", name, ignore.case=TRUE))) {
        name <- "beamAttenuation"
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
    } else if (1 == length(grep("flC[1]*", name, ignore.case=TRUE))) {
        name <- "fluorescence"
        unit <- list(unit=expression(mu*g/l), scale="Chelsea")
    } else if (1 == length(grep("^flECO-AFL", name, ignore.case=TRUE))) {
        name <- "fluorescence"
        unit <- list(unit=expression(mg/m^3), scale="")
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
    } else if (name == "oxsatML/L") {
        name <- "oxygen"
        unit <- list(unit=expression(ml/l), scale="Weiss")
    } else if (name == "oxsatMg/L") {
        name <- "oxygen"
        unit <- list(unit=expression(mg/l), scale="Weiss")
    } else if (name == "oxsatMm/Kg") {
        name <- "oxygen"
        unit <- list(unit=expression(mu*mol/kg), scale="Weiss")
    } else if (name == "oxsolML/L") {
        name <- "oxygen"
        unit <- list(unit=expression(ml/l), scale="Garcia-Gordon")
    } else if (name == "oxsolMg/L") {
        name <- "oxygen"
        unit <- list(unit=expression(mg/l), scale="Garcia-Gordon")
    } else if (name == "oxsolMm/Kg") {
        name <- "oxygen"
        unit <- list(unit=expression(umol/kg), scale="Garcia-Gordon")
    } else if (name == "par") {
        name <- "par"
        unit <- list(unit=expression(), scale="")
    } else if (length(grep("pr[m]*", name, ignore.case=TRUE))) {
        name <- "pressure"
        unit <- list(unit=expression(dbar), scale="")
    } else if (1 == length(grep("ptempC", name, ignore.case=TRUE))) {
        name <- "pressureTemperature" # temperature at the pressure sensor
        unit <- list(unit=expression(degree*C), scale="ITS-90") # FIXME: guess on scale
    } else if (1 == length(grep("potemp[0-9]*68C", name, ignore.case=TRUE))) {
        name <- "theta68"
        unit <- list(unit=expression(degree*C), scale="ITS-68") # FIXME: guess on scale
    } else if (1 == length(grep("potemp[0-9]*90C", name, ignore.case=TRUE))) {
        name <- "theta"
        unit <- list(unit=expression(degree*C), scale="ITS-90") # FIXME: guess on scale
    } else if (1 == length(grep("pumps", name, ignore.case=TRUE))) {
        name <- "pumpStatus"
        unit <- list(unit=expression(), scale="")
    } else if (1 == length(grep("sal[0-9]{2}", name, ignore.case=TRUE))) {
        name <- "salinity"
        unit <- list(unit=expression(), scale="PSS-78") # FIXME: guess on scale
    } else if (1 == length(grep("sbeox[0-9]ML/L", name, ignore.case=TRUE))) {
        name <- "oxygen"
        unit <- list(unit=expression(ml/l), scale="SBE43")
    } else if (1 == length(grep("sbeox[0-9]Mm/Kg", name, ignore.case=TRUE))) {
        name <- "oxygen"
        unit <- list(unit=expression(mu*mol/kg), scale="SBE43")
    } else if (1 == length(grep("sbeox[0-9]PS", name, ignore.case=TRUE))) {
        name <- "oxygen"
        unit <- list(unit=expression(percent), scale="SBE43")
    } else if (1 == length(grep("sbeox[0-9]V", name, ignore.case=TRUE))) {
        name <- "oxygenRaw"
        unit <- list(unit=expression(V), scale="SBE43")
    } else if (1 == length(grep("scan", name, ignore.case=TRUE))) {
        name <- "scan"
        unit <- list(unit=expression(), scale="")
    } else if (1 == length(grep("sigma-.*[0-9]*", name, ignore.case=TRUE))) {
        name <- "sigmaTheta"
        unit <- list(unit=expression(kg/m^3), scale="")
    } else if (1 == length(grep("spar", name, ignore.case=TRUE))) {
        name <- "spar"
        unit <- list(unit=expression(), scale="")
    } else if (1 == length(grep("svCM", name, ignore.case=TRUE))) {
        name <- "soundSpeed"
        unit <- list(unit=expression(m/s), scale="")
    } else if (1 == length(grep("^t[0-9]68C?$", name, ignore.case=TRUE))) {
        name <- "temperature"
        unit <- list(unit=expression(degree*C), scale="IPTS-68")
    } else if (1 == length(grep("^t[0-9]90C?$", name, ignore.case=TRUE))) {
        name <- "temperature"
        unit <- list(unit=expression(degree*C), scale="ITS-90")
    } else if (name %in% c("t068C", "t4968C", "tnc68C", "tv268C",
                           "t168C", "tnc268C",
                           "t3868C", "t38_68C")) { # [1] p169-170
        name <- "temperature"
        unit <- list(unit=expression(degree*C), scale="IPTS-68")
    } else if (name %in% c("t090C",
                           "t090Cm", "t4990C", "tnc90C", "tv290C",
                           "t190C", "tnc290C",
                           "t3890C", "t38_90C")) { # [1] p169-170
        name <- "temperature"
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
        warning("unrecognized SBE name '", name, "'; consider using renameData() to set a new name")
        unit <- list(unit=expression(), scale="")
    }
    if (debug > 0)
        message("cnvName2oceName(): '", nameOriginal, "' -> '", name, "' (", unit$scale, ")")
    list(name=name, nameOriginal=nameOriginal, unit=unit)
}


#' Read an Seabird CTD File
#' @template readCtdTemplate
#'
#' @details
#' This function reads files stored in Seabird \code{.cnv} format.
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
#' The original data names as stored in \code{file} are stored within the \code{metadata}
#' slot as \code{dataNamesOriginal}, and are displayed with \code{summary} alongside the
#' numerical summary. See the Appendix VI of [2] for the meanings of these
#' names (in the "Short Name" column of the table spanning pages 161 through 172).
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
    colUnits <- list()
    colNamesInferred <- NULL
    dataNamesOriginal <- NULL
    for (iline in seq_along(nameLines)) {
        nu <- cnvName2oceName(lines[nameLines[iline]], debug=debug-1)
        colNamesInferred <- c(colNamesInferred, nu$name)
        dataNamesOriginal <- c(dataNamesOriginal, nu$nameOriginal)
        colUnits[[iline]] <- nu$unit
        ## message("nu$name: ", nu$name, "; scale: ", colUnits[[nu$name]]$unit$scale)
    }
    ## Handle duplicated names
    for (i in seq_along(colNamesInferred)) {
        w <- which(colNamesInferred == colNamesInferred[i])
        if (1 < length(w)) {
            ##print(w)
            w <- w[-1]
            ##message("duplicated: ", colNamesInferred[i])
            ##message("w: ", paste(w, collapse=" "))
            ##message(paste(colNamesInferred, collapse=" "))
            colNamesInferred[w] <- paste(colNamesInferred[i], "_", 1+seq.int(1,length(w)), sep="")
            ##message(paste(colNamesInferred, collapse=" "))
        }
    }
    res@metadata$dataNamesOriginal <- dataNamesOriginal



    found.scan <- "scan" %in% colNamesInferred
    found.temperature <- "temperature" %in% colNamesInferred
    found.pressure <- "pressure" %in% colNamesInferred
    found.salinity <- "salinity" %in% colNamesInferred
    found.time <- "time" %in% colNamesInferred
    found.depth <- "depth" %in% colNamesInferred
    found.conductivity <- "conductivity" %in% colNamesInferred
    found.conductivity.ratio <- "conductivity.ratio" %in% colNamesInferred
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
    if (!("temperature" %in% colNamesInferred))
        stop("no 'temperature' column; hint: use summary() to find SBE names, then set 'columns' argument")
    ## if (!("pressure" %in% colNamesInferred) && !("depth" %in% colNamesInferred))
    ##     stop("no 'pressure' column; hint: use summary() to find SBE names, then set 'columns' argument")

    res@metadata$header <- header
    res@metadata$type <- "SBE"
    res@metadata$hexfilename <- hexfilename # from instrument
    res@metadata$serialNumber <- serialNumber
    res@metadata$serialNumberTemperature <- serialNumberTemperature
    res@metadata$serialNumberConductivity <- serialNumberConductivity
    res@metadata$pressureType <- pressureType
    res@metadata$units <- colUnits
    names(res@metadata$units) <- colNamesInferred
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
    res@metadata$names <- colNamesInferred
    res@metadata$labels <- colNamesInferred
    res@metadata$filename <- filename
    ## Read the data as a table.
    pushBack(lines, file)
    if (is.null(columns)) {
        oceDebug(debug, "About to read these names: c(\"", paste(colNamesInferred, collapse='","'),"\")\n", sep="")
        data <- as.list(read.table(file, skip=iline-1, header=FALSE))
        if (length(data) != length(colNamesInferred))
            stop("Number of columns in .cnv data file does not equal number of named variables")
        names(data) <- colNamesInferred
        ndata <- length(data[[1]])
        if (0 < ndata) {
            haveData <- TRUE
            names <- names(data)
            ##labels <- names
            ## if (!found.scan) {
            ##     data$scan <- 1:ndata
            ##     names <- names(data)
            ##     colNamesInferred <- c(colNamesInferred, "scan")
            ##     colNamesOriginal <- c(colNamesOriginal, "scan")
            ## }
        } else {
            haveData <- FALSE
            warning("no data in CTD file \"", filename, "\"\n")
            data <- list(scan=NULL, salinity=NULL, temperature=NULL, pressure=NULL)
        }
    } else {
        warning("CAUTION: read.ctd.sbe() is not handling 'columns' argument properly (please post a GitHub issue)\n")
        dataAll <- read.table(file, skip=iline-1, header=FALSE, col.names=colNamesInferred)
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
            ## FIXME: move this to the very end, where we add 'scan' if that's not found.
            res <- ctdAddColumn(res, S, name="salinity", label="Salinity",
                                unit=c(unit=expression(), scale="PSS-78"), debug=debug-1)
            ## colNamesOriginal <- c(colNamesOriginal, "NA")
        }
        if (found.depth && !found.pressure) { # BUG: this is a poor, nonrobust approximation of pressure
            g <- if (found.header.latitude) gravity(latitude) else 9.8
            rho0 <- 1000 + swSigmaTheta(median(res@data$salinity), median(res@data$temperature), 0)
            res <- ctdAddColumn(res, res@data$depth * g * rho0 / 1e4, name="pressure", label="Pressure",
                                unit=list(unit=expression("dbar"), scale=""), debug=debug-1)
            ## colNamesOriginal <- c(colNamesOriginal, "NA")
            warning("created a pressure column from the depth column\n")
        }
    }
    ##res@metadata$dataNamesOriginal <- colNamesOriginal
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))

    ## update to temperature IPTS-90, if have an older version
    if (!("temperature" %in% names(res@data)) && ("temperature68" %in% names(res@data))) {
        res <- ctdAddColumn(res, T90fromT68(res@data$temperature68),
                            name="temperature", label="Temperature",
                            unit=c(unit=expression(degree*C), scale="ITS-90"), debug=debug-1)
        warning("converted temperature from IPTS-68 to ITS-90")
        res@processingLog <- processingLogAppend(res@processingLog, "converted temperature from IPTS-68 to ITS-90")
    }
    if (!("scan" %in% names(res@data))) {
        res <- ctdAddColumn(res, 1:length(res@data[[1]]), label="scan",
                            unit=c(unit=expression(), scale=""), debug=debug-1)
    }
    ## FIXME(20160429): do we need/want next 3 lines?
    if (!("salinity" %in% names(res@metadata$units))) res@metadata$units$salinity <- list(unit=expression(), scale="PSS-78")
    if (!("pressure" %in% names(res@metadata$units))) res@metadata$units$pressure <- list(unit=expression(dbar), scale="")
    if (!("depth" %in% names(res@metadata$units))) res@metadata$units$depth <- list(unit=expression(m), scale="")
    oceDebug(debug, "} # read.ctd.sbe()\n")
    res
}

