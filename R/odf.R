#' @title Class to Store ODF data
#'
#' @description
#' Class for data stored in a format used at Canadian Department of Fisheries
#' and Oceans laboratories. This is somewhat unusual amongst \code{oce}
#' classes, in that it does not map to a particular instrument, but rather to a
#' storage type; in that sense, it is similar to the \code{bremen-class}.
#'
#' @section Methods:
#'
#' Consider an ODF object named \code{odf}.
#'
#' \emph{Accessing metadata.}
#'
#' Metadata (contained in the S4 slot named \code{metadata}) may be retrieved
#' or set by name, \code{odf[["longitude"]] <- odf[["longitude"]] + 1} corrects
#' a one-degree error.
#'
#' \emph{Accessing measured data.}
#'
#' Column data may be accessed by name, e.g. \code{odf[["salinity"]]},
#' \code{odf[["temperature"]]}, \code{odf[["pressure"]]}, etc.  It is up to the
#' user to realize what is in the object.
#'
#' \emph{Assigning values.}
#'
#' Items stored in the object may be altered with e.g.  \code{odf[["salinity"]]
#' <- rep(35,10)}.
#'
#' \emph{Overview of contents.}
#'
#' The \code{show} method (e.g.  \code{show(odf)}) displays information about
#' the object.
#' @author Dan Kelley
#' @family things related to \code{odf} data
#' @family classes provided by \code{oce}
setClass("odf", contains="oce")

## [1] Anthony W. Isenor and David Kellow, 2011. ODF Format Specification Version 2.0. (A .doc file downloaded from a now-forgotten URL by Dan Kelley, in June 2011.)
##
## [2] An older document is: http://slgo.ca/app-sgdo/en/pdf/docs_reference/Format_ODF.pdf

setMethod(f="initialize",
          signature="odf",
          definition=function(.Object, time, filename="") {
              ## Assign to some columns so they exist if needed later (even if they are NULL)
              .Object@data$time <- if (missing(time)) NULL else time
              .Object@metadata$filename <- filename
              .Object@metadata$deploymentType <- "" # see ctd
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'odf' object"
              return(.Object)
          })

#' @title Extract Something From an ODF Object
#' @param x an odf object, i.e. one inheriting from \code{\link{odf-class}}.
#' @template sub_subTemplate
#' @family things related to \code{odf} data
setMethod(f="[[",
          signature(x="odf", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              callNextMethod()         # [[
          })

#' @title Replace Parts of an ODF Object
#' @param x an \code{odf} object, i.e. inheriting from \code{\link{odf-class}}
#' @template sub_subsetTemplate
#' @family things related to \code{odf} data
setMethod(f="[[<-",
          signature(x="odf", i="ANY", j="ANY"),
          definition=function(x, i, j, ..., value) {
              callNextMethod(x=x, i=i, j=j, ...=..., value=value) # [[<-
          })

#' @title Subset an ODF object
#'
#' @description
#' This function is somewhat analogous to \code{\link{subset.data.frame}}.
#'
#' @param x an \code{odf} object.
#' @param subset a condition to be applied to the \code{data} portion of
#' \code{x}.  See \sQuote{Details}.
#' @param \dots ignored.
#' @return A new \code{odf} object.
#' @author Dan Kelley
#' @family things related to \code{odf} data
#' @family functions that subset \code{oce} objects
setMethod(f="subset",
          signature="odf",
          definition=function(x, subset, ...) {
              subsetString <- paste(deparse(substitute(subset)), collapse=" ")
              res <- x
              ##dots <- list(...)
              if (missing(subset))
                  stop("must give 'subset'")

              if (missing(subset))
                  stop("must specify a 'subset'")
              keep <- eval(substitute(subset), x@data, parent.frame(2)) # used for $ts and $ma, but $tsSlow gets another
              res <- x
              for (name in names(x@data)) {
                  res@data[[name]] <- x@data[[name]][keep]
              }
              res@processingLog <- processingLogAppend(res@processingLog, paste("subset(x, subset=", subsetString, ")", sep=""))
              res
          })


#' @title Plot an ODF Object
#'
#' @description
#' Plot data contained within an ODF object,
#' using \code{\link{oce.plot.ts}} to create panels of time-series plots for all
#' the columns contained in the \code{odf} object. If the object's \code{data}
#' slot does not contain \code{time}, then \code{\link{pairs}} is used to plot
#' all the elements in the slot. These actions are both crude and there are
#' no arguments to control the behaviour, but this function is really just a stop-gap
#' measure, since in practical work \code{odf} objects are usually cast to other types,
#' and those types tend to have more useful plots.
#'
#' @param x A \code{odf} object, e.g. one inheriting from \code{\link{odf-class}}.
#' @author Dan Kelley
#' @family functions that plot \code{oce} data
#' @family things related to \code{odf} data
setMethod(f="plot",
          signature=signature("odf"),
          definition=function(x) {
              data <- x@data
              dataNames <- names(data)
              n <- length(dataNames)
              time <- data$time
              if (!is.null(time)) {
                  par(mfrow=c(n-1, 1))
                  for (i in 1:n) {
                      if (dataNames[i] != "time") {
                          oce.plot.ts(time, data[[dataNames[i]]],
                                      ylab=dataNames[i], mar=c(2, 3, 0.5, 1), drawTimeRange=FALSE)
                      }
                  }
              } else {
                  flags <- grepl("^.*[fF]lag$", dataNames)
                  pairs(data.frame(data)[,!flags], labels=dataNames[!flags])
              }
          })


#' @title Summarize an ODF Object
#'
#' @description
#' Pertinent summary information is presented, including the station name,
#' sampling location, data ranges, etc.
#'
#' @param object an object of class \code{"odf"}, usually, a result of a call
#' to \code{\link{read.odf}} or \code{\link{read.oce}}.
#' @param \dots further arguments passed to or from other methods.
#' @return A matrix containing statistics of the elements of the \code{data}
#' slot.
#' @author Dan Kelley
#' @family things related to \code{odf} data
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
              callNextMethod()         # [[
          })



## find first match in header
findInHeader <- function(key, lines, returnOnlyFirst=TRUE) # local function
{
    i <- grep(key, lines)
    rval <- ""
    rval <- list()
    for (j in seq_along(i)) {
        ## ----------
        ## RISKY CODE: only look at first match
        ## ----------
        ## isolate the RHS of the eqquality
        tmp <- gsub("\\s*$", "", gsub("^\\s*", "", gsub("'", "", gsub(",", "", strsplit(lines[i[j]], "=")[[1]][2]))))
        ## convert e.g. D+00 to e+00
        if (length(grep("[-A-CF-Za-cf-z ]", tmp))) {
            rval[[j]] <- tmp
        } else {
            tmp <- gsub("(.*)D([-+])([0-9]{2})", "\\1e\\2\\3", tmp)
            rval[[j]] <- as.numeric(tmp)
        }
    }
    if (0 < length(rval)) {
        if (returnOnlyFirst) {
            rval[[1]]
        } else {
            rval
        }
    } else {
        NULL
    }
}

#' @title Translate from ODF Names to Oce Names
#'
#' @description
#' Translate data names in the ODF convention to similar names in the Oce convention.
#'
#' @details
#' The following table gives the regular expressions that define recognized
#' ODF names, along with the translated names as used in oce objects.
#' If an item is repeated, then the second one has a \code{2} appended
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
#'     \code{COND_*.*} \tab \code{conductivity}       \tab Conductivity in mS/cm or S/m (unit detected)               \cr
#'     \code{CNDC_*.*} \tab \code{conductivity}       \tab Conductivity in mS/cm or S/m (unit detected)               \cr
#'     \code{DEPH_*.*} \tab \code{pressure}           \tab Sensor depth below sea level                               \cr
#'     \code{DOXY_*.*} \tab \code{oxygen}             \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{ERRV_*.*} \tab \code{error}              \tab Used in \code{adp} objects                                 \cr
#'     \code{EWCT_*.*} \tab \code{u}                  \tab Used in \code{adp} and \code{cm} objects                   \cr
#'     \code{FFFF_*.*} \tab \code{flagArchaic}        \tab Old flag name, replaced by \code{QCFF}                     \cr
#'     \code{FLOR_*.*} \tab \code{fluorometer}        \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{FWETLABS} \tab \code{fwetlabs}           \tab Used in ??                                                 \cr
#'     \code{HCDM}     \tab \code{directionMagnetic}  \tab                                                            \cr
#'     \code{HCDT}     \tab \code{directionTrue}      \tab                                                            \cr
#'     \code{HCSP}     \tab \code{speedHorizontal}    \tab                                                            \cr
#'     \code{LATD_*.*} \tab \code{latitude}           \tab                                                            \cr
#'     \code{LOND_*.*} \tab \code{longitude}          \tab                                                            \cr
#'     \code{NSCT_*.*} \tab \code{v}                  \tab Used in \code{adp} objects                                 \cr
#'     \code{OCUR_*.*} \tab \code{oxygenCurrent}      \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{OSAT_*.*} \tab \code{oxygenSaturation}   \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{OTMP_*.*} \tab \code{oxygenTemperature}  \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{OXYV_*.*} \tab \code{oxygenVoltage}      \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{PHPH_*.*} \tab \code{pH}                 \tab                                                            \cr
#'     \code{POTM_*.*} \tab \code{theta}              \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{PRES_*.*} \tab \code{pressure}           \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{PSAL_*.*} \tab \code{salinity}           \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{PSAR_*.*} \tab \code{par}                \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{QCFF_*.*} \tab \code{flag}               \tab Overall flag                                               \cr
#'     \code{SIGP_*.*} \tab \code{sigmaTheta}         \tab Used mainly in \code{ctd} objecs                           \cr
#'     \code{SIGT_*.*} \tab \code{sigmat}             \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{SNCN_*.*} \tab \code{scanCounter}        \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{SYTM_*.*} \tab \code{time}               \tab Used in many objects                                       \cr
#'     \code{TE90_*.*} \tab \code{temperature}        \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{TEMP_*.*} \tab \code{temperature}        \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{TOTP_*.*} \tab \code{pressureAbsolute}   \tab Used mainly in \code{ctd} objects                          \cr
#'     \code{UNKN_*.*} \tab \code{-}                  \tab The result is context-dependent                            \cr
#'     \code{VCSP_*.*} \tab \code{w}                  \tab Used in \code{adp} objects                                 \cr
#' }
#' Any code not shown in the list is transferred to the oce object without renaming, apart from
#' the adjustment of suffix numbers. The following code have been seen in data files from
#' the Bedford Institute of Oceanography: \code{ALTB}, \code{PHPH} and \code{QCFF}.
#'
#' @section A note on unit conventions:
#' Some older ODF files contain non-standard units for conductivity,
#' including \code{mho/m}, \code{mmho/cm}, and \code{mmHo}. As the
#' units for conductivity are important for derived quantities
#' (e.g. salinity), such units are converted to standard units
#' (e.g. \code{S/m} and \code{mS/cm}), with a warning.
#'
#' @section Consistency warning:
#' There are disagreements on variable names. For example, the ``DFO
#' Common Data Dictionary'' [1]
#' has unit millmole/m^3 for NODC and MEDS, but it has unit mL/L for BIO and IML.
#'
#' @param ODFnames Vector of strings holding ODF names.
#' @param ODFunits Vector of strings holding ODF units.
#' @param columns Optional list containing name correspondances, as described for
#' \code{\link{read.ctd.odf}}.
#' @param PARAMETER_HEADER Optional list containing information on the data variables.
#' @template debugTemplate
#' @return A vector of strings.
#' @author Dan Kelley
#' @family functions that interpret variable names and units from headers
#'
#' @references
#' 1. The Department of Fisheries and Oceans Common Data Dictionary may be
#' available at \code{http://www.isdm.gc.ca/isdm-gdsi/diction/code_search-eng.asp?code=DOXY})
#' although that link seems to be unreliable.
#' @family things related to \code{odf} data
ODFNames2oceNames <- function(ODFnames, ODFunits=NULL,
                              columns=NULL, PARAMETER_HEADER=NULL, debug=getOption("oceDebug"))
{
    oceDebug(debug, "ODFNames2oceNames() {\n", unindent=1, sep="")
    n <- length(ODFnames)
    if (n != length(ODFunits)) {
        if (debug>0) message("ODFnames: ", paste(ODFnames, collapse=" "))
        if (debug>0) message("ODFunits: ", paste(ODFunits, collapse=" "))
        if (0 == length(ODFunits)) {
            ## Handle the case of missing UNITS
            ODFunits <- rep("", n)
        } else {
            warning("length of ODFnames and ODFunits should agree but they are ", n, " and ", length(ODFunits), ". Padding with empty units" )
            ODFunits <- c(ODFunits, rep("", n-length(ODFunits)))
        }
    }
    names <- ODFnames
    ## message("names: ", paste(names, collapse="|"))
    ## Capture names for UNKN_* items, and key on them.  Possibly this should be done to
    ## get all the names, but then we just transfer the problem of decoding keys
    ## to decoding strings, and that might yield problems with encoding, etc.
    if (!is.null(PARAMETER_HEADER)) {
        if (length(grep("^UNKN_.*", PARAMETER_HEADER[[i]]$CODE))) {
            uname <- PARAMETER_HEADER[[i]]$NAME
            ## message("i:", i, ", name:\"", uname)
            name <- if (length(grep("Percent Good Pings", uname ))) "g" else uname
        }
    }
    ## If 'name' is mentioned in columns, then use columns and ignore the lookup table.
    if (!is.null(columns)) {
        ##message("name:", name)
        ## d<-read.ctd("~/src/oce/create_data/ctd/ctd.cnv",columns=list(salinity=list(name="sal00",unit=list(expression(), scale="PSS-78monkey"))))
        cnames <- names(columns)
        for (i in seq_along(cnames)) {
            if (name[i] == columns[[i]]$name) {
                ##message("HIT; name=", cnames[i])
                ##message("HIT; unit$scale=", columns[[i]]$unit$scale)
                name[i] <- names
            }
        }
        ## do something with units too; check this block generally for new spelling
        warning("FIXME(Kelley): code 'columns' support into ODFNames2oceNames")
    }


    ## Infer standardized names for columns, partly based on documentation (e.g. PSAL for salinity), but
    ## mainly from reverse engineering of some files from BIO and DFO.  The reverse engineering
    ## really is a kludge, and if things break (e.g. if data won't plot because of missing temperatures,
    ## or whatever), this is a place to look.
    oceDebug(debug, "STAGE 1 names: ", paste(names, collapse=" "), "\n")
    names <- gsub("ALTB", "altimeter", names)
    names <- gsub("BATH", "waterDepth", names) # FIXME: is this water column depth or sensor depth?
    names <- gsub("BEAM", "a", names)  # FIXME: is this sensible?
    names <- gsub("CNTR", "scan", names)
    names <- gsub("CRAT", "conductivity", names)
    names <- gsub("COND", "conductivity", names)
    names <- gsub("CNDC", "conductivity", names)
    names <- gsub("DEPH", "depth", names)
    names <- gsub("DOXY", "oxygen", names)
    names <- gsub("ERRV", "error", names)
    names <- gsub("EWCT", "u", names)
    names <- gsub("FFFF", "overallFlag", names)
    names <- gsub("FLOR", "fluorometer", names)
    names <- gsub("FWETLABS", "fwetlabs", names) # FIXME: what is this?
    names <- gsub("HCSP", "speedHorizontal", names)
    names <- gsub("HCDM", "directionMagnetic", names)
    names <- gsub("HCDT", "directionTrue", names)
    names <- gsub("LATD", "latitude", names)
    names <- gsub("LOND", "longitude", names)
    names <- gsub("NSCT", "v", names)
    names <- gsub("OCUR", "oxygenCurrent", names)
    names <- gsub("OSAT", "oxygenSaturation", names)
    names <- gsub("OTMP", "oxygenTemperature", names)
    names <- gsub("OXYV", "oxygenVoltage", names)
    names <- gsub("PHPH", "pH", names)
    names <- gsub("POTM", "theta", names) # in a moored ctd file examined 2014-05-15
    names <- gsub("PRES", "pressure", names)
    names <- gsub("PSAL", "salinity", names)
    names <- gsub("PSAR", "par", names)
    names <- gsub("QCFF", "QCFlag", names)
    names <- gsub("SIGP", "sigmaTheta", names)
    names <- gsub("SIGT", "sigmat", names) # in a moored ctd file examined 2014-05-15
    names <- gsub("SNCN", "scanCounter", names)
    names <- gsub("SYTM", "time", names) # in a moored ctd file examined 2014-05-15
    names <- gsub("TE90", "temperature", names)
    names <- gsub("TEMP", "temperature", names)
    names <- gsub("TOTP", "pressureAbsolute", names)
    names <- gsub("UNKN", "unknown", names)
    names <- gsub("VCSP", "w", names)
    ## Step 3: recognize something from moving-vessel CTDs
    ## Step 4: some meanings inferred (guessed, really) from file CTD_HUD2014030_163_1_DN.ODF
    ## Finally, fix up suffixes.
    ##message("names (line 324): ", paste(names, collapse="|"))
    names <- gsub("_[0-9][0-9]", "", names)
    oceDebug(debug, "STAGE 2 names: ", paste(names, collapse=" "), "\n")
    if (n > 1) {
        for (i in 2:n) {
            ##message("names[", i, "] = '", names[i], "'")
            if (1 == length(grep("^QQQQ", names[i])))
                names[i] <- paste(names[i-1], "Flag", sep="")
        }
    }
    oceDebug(debug, "STAGE 3 names: ", paste(names, collapse=" "), "\n")
    names <- unduplicateNames(names)
    oceDebug(debug, "STAGE 4 names: ", paste(names, collapse=" "), "\n")
    ## Now deal with units
    units <- list()
    for (i in seq_along(names)) {
        ## NOTE: this was originally coded with ==, but as errors in ODF
        ## formatting have been found, I've moved to grep() instead; for
        ## example, the sigma-theta case is done that way, because the
        ## original code expected kg/m^3 but then (issue 1051) I ran
        ## across an ODF file that wrote density as Kg/m^3.
        oceDebug(1+debug, paste("ODFnames[",i,"]='",ODFnames[i],"', names[",i,"]='", names[i], "', ODFunits[", i, "]='", ODFunits[i], "'\n", sep=""))
        units[[names[i]]] <- if (ODFunits[i] == "code") {
            list(unit=expression(), scale="")
        } else if (ODFunits[i] == "counts") {
            list(unit=expression(), scale="")
        } else if (ODFunits[i] == "db") {
            list(unit=expression(dbar), scale="")
        } else if (ODFunits[i] == "decibars") {
            list(unit=expression(dbar), scale="")
        } else if (1 == length(grep("^deg(ree)?(s)?$", ODFunits[i], ignore.case=TRUE))) {
            list(unit=expression(degree), scale="")
        } else if (1 == length(grep("^I[P]?TS-68, deg C$", ODFunits[i], ignore.case=TRUE))) {
            ## handles both the correct IPTS and the incorrect ITS.
            list(unit=expression(degree*C), scale="IPTS-68")
        } else if (ODFunits[i] == "degrees C") {
            ## guess on scale
            list(unit=expression(degree*C), scale="ITS-90")
        } else if (ODFunits[i] == "ITS-90, deg C") {
            list(unit=expression(degree*C), scale="ITS-90")
        } else if (1 == length(grep("^m$", ODFunits[i], ignore.case=TRUE))) {
            list(unit=expression(m), scale="")
        } else if (1 == length(grep("^metres$", ODFunits[i], ignore.case=TRUE))) {
            list(unit=expression(m), scale="")
        } else if (ODFunits[i] == "mg/m^3") {
            list(unit=expression(mg/m^3), scale="")
        } else if (ODFunits[i] == "mg/m**3") {
            list(unit=expression(mg/m^3), scale="")
        } else if (ODFunits[i] == "ml/l") {
            list(unit=expression(ml/l), scale="")
        } else if (1 == length(grep("^\\s*m/s\\s*$", ODFunits[i], ignore.case=TRUE))) {
            list(unit=expression(m/s), scale="")
        } else if (1 == length(grep("^\\s*m/sec\\s*$", ODFunits[i], ignore.case=TRUE))) {
            list(unit=expression(m/s), scale="")
        } else if (1 == length(grep("^\\s*m\\^-1/sr\\s*$", ODFunits[i], ignore.case=TRUE))) {
            list(unit=expression(1/m/sr), scale="")
        } else if (1 == length(grep("^\\s*mho[s]{0,1}/m\\s*$", ODFunits[i], ignore.case=TRUE))) {
            warning('Changed unit mho/m to S/m for conductivity')
            list(unit=expression(S/m), scale="")
        #} else if (1 == length(grep("^\\s*micro[ ]?mols/m2/s\\s*$", ODFunits[i], ignore.case=TRUE))) {
        #    list(unit=expression(mu*mol/m^2/s), scale="")
        } else if (1 == length(grep("^\\s*mmho[s]?/cm\\s*$", ODFunits[i], ignore.case=TRUE))) {
            warning('Changed unit mmho/cm to mS/cm for conductivity')
            list(unit=expression(mS/cm), scale="")
        } else if (ODFunits[i] == "mmHo") {
            warning('Changed unit mmHo to S/m for conductivity')
            list(unit=expression(S/m), scale="")
        ##} else if (ODFunits[i] == "[(]*none[)]$") {
        } else if (1 == length(grep("^[(]*none[)]*$", ODFunits[i], ignore.case=TRUE))) {
            list(unit=expression(), scale="")
        ##} else if (ODFunits[i] == "PSU") {
        } else if (1 == length(grep("^psu$", ODFunits[i], ignore.case=TRUE))) {
            list(unit=expression(), scale="PSS-78")
        } else if (1 == length(grep("^\\s*kg/m\\^3$", ODFunits[i], ignore.case=TRUE))) {
            list(unit=expression(kg/m^3), scale="")
        } else if (1 == length(grep("^\\s*kg/m\\*\\*3\\s*$", ODFunits[i], ignore.case=TRUE))) {
            list(unit=expression(kg/m^3), scale="")
        } else if (1 == length(grep("^\\s*ma\\s*$", ODFunits[i], ignore.case=TRUE))) {
            list(unit=expression(ma), scale="")
        } else if (1 == length(grep("^\\s*micro[ ]?mol[e]?s/m2/s(ec)?\\s*$", ODFunits[i], ignore.case=TRUE))) {
            list(unit=expression(mu*mol/m^2/s), scale="")
        } else if (1 == length(grep("^sigma-theta,\\s*kg/m\\^3$", ODFunits[i], ignore.case=TRUE))) {
            list(unit=expression(kg/m^3), scale="")
        } else if (1 == length(grep("^seconds$", ODFunits[i], ignore.case=TRUE))) {
            list(unit=expression(s), scale="")
        } else if (ODFunits[i] == "S/m") {
            list(unit=expression(S/m), scale="")
        } else if (ODFunits[i] == "ratio") {
            list(unit=expression(ratio), scale="")
        } else if (1 == length(grep("^uA$", ODFunits[i], ignore.case=TRUE))) {
            list(unit=expression(mu*a), scale="")
        } else if (1 == length(grep("^ueinsteins/s/m\\*\\*2$", ODFunits[i], ignore.case=TRUE))) {
            list(unit=expression(mu*einstein/s/m^2), scale="")
        } else if (1 == length(grep("^ug/l$", ODFunits[i], ignore.case=TRUE))) {
            list(unit=expression(mu*g/l), scale="")
        } else if (1 == length(grep("^UTC$", ODFunits[i], ignore.case=TRUE))) {
            list(unit=expression(), scale="")
        } else if (ODFunits[i] == "V") {
            list(unit=expression(V), scale="")
        } else if (1 == length(grep("^%$", ODFunits[i], ignore.case=TRUE))) {
            list(unit=expression("%"), scale="")
        } else if (nchar(ODFunits[i]) == 0) {
            list(unit=expression(), scale="")
        } else {
            warning("unable to interpret ODFunits[", i, "]='", ODFunits[i], "', for item named '", names[i], "'", sep="")
            list(unit=parse(text=ODFunits[i]), scale=ODFunits[i])
        }
    }
    ## Catch some problems I've seen in data
    directionVariables <- which(names == "directionMagnetic" | names == "directionTrue")
    for (directionVariable in directionVariables) {
        ## message("directionVariable=",directionVariable)
        unit <- units[[directionVariable]]$unit
        if (is.null(unit)) {
            warning("no unit found for '",
                    names[[directionVariable]], "'; this will not affect calculations, though")
            ## units[[directionVariable]]$unit <- expression(degree)
        } else if ("degree" != as.character(unit)) {
            warning("odd unit, '", as.character(unit), "', for '",
                    names[directionVariable], "'; this will not affect calculations, though")
            ## units[[directionVariable]]$unit <- expression(degree)
        }
    }
    oceDebug(debug, "} # ODFNames2oceNames()\n", unindent=1, sep="")
    list(names=names, units=units)
}


#' @title Create ODF object from the output of \code{ODF::read_ODF()}
#'
#' @description
#' As of August 11, 2015, \code{ODF::read_ODF} returns a list with 9 elements,
#' one named \code{DATA}, which is a \code{\link{data.frame}} containing the
#' columnar data, the others being headers of various sorts.  The present
#' function constructs an oce object from such data, facilitating processing
#' and plotting with the general oce functions.
#' This involves storing the 8 headers verbatim in the
#' \code{odfHeaders} in the \code{metadata} slot, and also
#' copying some of the header
#' information into more standard names (e.g.  \code{metadata@@longitude} is a
#' copy of \code{metadata@@odfHeader$EVENT_HEADER$INITIAL_LATITUDE}).  As for
#' the \code{DATA}, they are stored in the \code{data} slot, after renaming
#' from ODF to oce convention using \code{\link{ODFNames2oceNames}}.
#'
#' @param ODF A list as returned by \code{read_ODF} in the \code{ODF} package
#' @param coerce A logical value indicating whether to coerce the return value
#' to an appropriate object type, if possible.
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, or to 2 to get more.
#' @return An oce object, possibly coerced to a subtype.
#'
#' @section Caution: This function may change as the \code{ODF} package
#' changes.  Since \code{ODF} has not been released yet, this should not affect
#' any users except those involved in the development of \code{oce} and
#' \code{ODF}.
#' @author Dan Kelley
#' @family things related to \code{odf} data
ODF2oce <- function(ODF, coerce=TRUE, debug=getOption("oceDebug"))
{
    message("DAN")
    ## Stage 1. insert metadata (with odfHeader holding entire ODF header info)
    ## FIXME: add other types, starting with ADCP perhaps
    isCTD <- FALSE
    isMCTD <- FALSE                    # nolint (variable not used)
    if (coerce) {
        if ("CTD" == ODF$EVENT_HEADER$DATA_TYPE) {
            isCTD <- TRUE
            res <- new("ctd")
        } else if ("MCTD" == ODF$EVENT_HEADER$DATA_TYPE) {
            isMCTD <- TRUE             # nolint (variable not used)
            res <- new("ctd")
            res@metadata$deploymentType <- "moored"
        } else {
            res <- new("odf") # FIXME: other types
        }
    } else {
        res <- new("odf")
    }
    ## Save the whole header as read by BIO routine read_ODF()

    res@metadata$odfHeader <- list(ODF_HEADER=ODF$ODF_HEADER,
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
                                    INPUT_FILE=ODF$INPUT_FILE)

    ## Define some standard items that are used in plotting and summaries
    if (isCTD) {
        res@metadata$type <- res@metadata$odfHeader$INSTRUMENT_HEADER$INST_TYPE
        res@metadata$model <- res@metadata$odfHeader$INSTRUMENT_HEADER$INST_MODEL
        res@metadata$serialNumber <- res@metadata$odfHeader$INSTRUMENT_HEADER$SERIAL_NUMBER
    }
    res@metadata$startTime <- strptime(res@metadata$odfHeader$EVENT_HEADER$START_DATE_TIME,
                                       "%d-%B-%Y %H:%M:%S", tz="UTC")
    res@metadata$filename <- res@metadata$odfHeader$ODF_HEADER$FILE_SPECIFICATION
    res@metadata$serialNumber <- res@metadata$odfHeader$INSTRUMENT_HEADER$SERIAL_NUMBER
    res@metadata$ship <- res@metadata$odfHeader$CRUISE_HEADER$PLATFORM
    res@metadata$cruise <- res@metadata$odfHeader$CRUISE_HEADER$CRUISE_NUMBER
    res@metadata$station <- res@metadata$odfHeader$EVENT_HEADER$EVENT_NUMBER # FIXME: is this right?
    res@metadata$scientist <- res@metadata$odfHeader$CRUISE_HEADER$CHIEF_SCIENTIST
    res@metadata$latitude <- as.numeric(res@metadata$odfHeader$EVENT_HEADER$INITIAL_LATITUDE)
    res@metadata$longitude <- as.numeric(res@metadata$odfHeader$EVENT_HEADER$INITIAL_LONGITUDE)

    ## Stage 2. insert data (renamed to Oce convention)
    xnames <- names(ODF$DATA)
    res@data <- as.list(ODF$DATA)
    resNames <- ODFNames2oceNames(xnames, columns=NULL, PARAMETER_HEADER=ODF$PARAMETER_HEADER, debug=debug-1)
    names(res@data) <- resNames
    ## Obey missing values ... only for numerical things (which might be everything, for all I know)
    nd <- length(resNames)
    for (i in 1:nd) {
        if (is.numeric(res@data[[i]])) {
            ##message("NULL_VALUE='", ODF$PARAMETER_HEADER[[i]]$NULL_VALUE, "'")
            NAvalue <- as.numeric(gsub("D", "e", ODF$PARAMETER_HEADER[[i]]$NULL_VALUE))
            ##message("NAvalue=", NAvalue)
            ## message("NAvalue: ", NAvalue)
            res@data[[i]][res@data[[i]] == NAvalue] <- NA
        }
    }
    ## Stage 3. rename QQQQ_* columns as flags on the previous column
    names <- names(res@data)
    for (i in seq_along(names)) {
        if (substr(names[i], 1, 4) == "QQQQ") {
            if (i > 1) {
                names[i] <- paste(names[i-1], "Flag", sep="")
            }
        }
    }
    ## use old (FFFF) flag if there is no modern (QCFF) flag
    ##if ("overall2Flag" %in% names && !("flag" %in% names))
    ##    names <- gsub("flagArchaic", "flag", names)
    names(res@data) <- names
    res
}


#' @title Read an ODF file
#'
#' @description
#' ODF (Ocean Data Format) is a
#' format developed at the Bedford Institute of Oceanography and also used
#' at other Canadian Department of Fisheries and Oceans (DFO) facilities.
#' It can hold various types of time-series data, which includes a variety
#' of instrument types. Thus, \code{read.odf}
#' is used by \code{read.ctd.odf} for CTD data, etc. As of mid-2015,
#' \code{read.odf} is still in development, with features being added as  a
#' project with DFO makes available more files.
#'
#' @details
#' Note that some elements of the metadata are particular to ODF objects,
#' e.g. \code{depthMin}, \code{depthMax} and \code{sounding}, which
#' are inferred from ODF items named \code{MIN_DEPTH}, \code{MAX_DEPTH}
#' and \code{SOUNDING}, respectively. In addition, the more common metadata
#' item \code{waterDepth}, which is used in \code{ctd} objects to refer to
#' the total water depth, is set to \code{sounding} if that is finite,
#' or to \code{maxDepth} otherwise.
#'
#' The function \code{\link{ODFNames2oceNames}} is used to translate
#' data names from the ODF file to standard \code{oce} names, and
#' handles conversion for a few non-standard units. The documentation
#' of \code{\link{ODFNames2oceNames}} should be consulted for more
#' details.
#'
#' @examples
#' library(oce)
#' # Read a CTD cast made on the Scotian Shelf. Note that the file's metadata
#' # states that conductivity is in S/m, but it is really conductivity ratio,
#' # so we must alter the unit before converting to a CTD object. Note that
#' # read.odf() on this data file produces a warning suggesting that the user
#' # repair the unit, using the method outlined here.
#' odf <- read.odf(system.file("extdata", "CTD_BCD2014666_008_1_DN.ODF", package="oce"))
#' odf[["conductivityUnit"]] <- list(unit=expression(), scale="")
#' #
#' # Figure 1. make a CTD, and plot (with span to show NS)
#' plot(as.ctd(odf), span=500)
#' # Figure 2. highlight bad data on TS diagram
#' plotTS(odf, type='o') # use a line to show loops
#' bad <- odf[["QCFlag"]]!=0
#' points(odf[['salinity']][bad],odf[['temperature']][bad],col='red',pch=20)
#'
#' @param file the file containing the data.
#' @param columns An optional \code{\link{list}} that can be used to convert unrecognized
#' data names to resultant variable names.  For example,
#' \code{columns=list(salinity=list(name="salt", unit=list(unit=expression(), scale="PSS-78"))}
#' states that a short-name of \code{"salt"} represents salinity, and that the unit is
#' as indicated. This is passed to \code{\link{cnvName2oceName}} or \code{\link{ODFNames2oceNames}},
#' as appropriate, and takes precendence over the lookup table in that function.
#' @template debugTemplate
#'
#' @return An object of class \code{oce}. It is up to a calling function to determine what to do with this object.
#'
#' @section Caution:
#' ODF files do not store information on the temperature or salinity scale, and \code{read.odf}
#' assumes them to be ITS-90 and PSS-78, respectively. These scales will not be correct for old
#' data files. Note that the temperature scale can be converted from old scales
#' using \code{\link{T90fromT68}} and \code{\link{T90fromT48}}, although the change will be in
#' a fraction of a millidegree, which probably exceeds reasonable confidence in old data.
#'
#' @seealso \code{\link{ODF2oce}} will be an alternative to this, once (or perhaps if) a \code{ODF}
#' package is released by the Canadian Department of Fisheries and Oceans.
#'
#' @references Anthony W. Isenor and David Kellow, 2011. ODF Format Specification
#' Version 2.0. (This is a .doc file downloaded from a now-forgotten URL by Dan Kelley,
#' in June 2011.)
#'
#' @family things related to \code{odf} data
read.odf <- function(file, columns=NULL, debug=getOption("oceDebug"))
{
    oceDebug(debug, "read.odf(\"", file, "\", ...) {\n", unindent=1, sep="")
    if (is.character(file)) {
        if (nchar(file) == 0)
            stop("'file' cannot be an empty string")
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
    lines <- readLines(file, 1000, encoding="UTF-8")
    pushBack(lines, file) # we used to read.table(text=lines, ...) but it is VERY slow
    dataStart <- grep("-- DATA --", lines)
    if (!length(dataStart)) {
        lines <- readLines(file, encoding="UTF-8")
        dataStart <- grep("-- DATA --", lines)
        if (!length(dataStart)) {
            stop("cannot locate a line containing '-- DATA --'")
        }
        pushBack(lines, file)
    }
    nlines <- length(lines)

    ## Learn about each parameter from its own header block
    linePARAMETER_HEADER <- grep("^\\s*PARAMETER_HEADER,\\s*$", lines)
    nPARAMETER_HEADER <- length(linePARAMETER_HEADER)
    if (nPARAMETER_HEADER < 1)
        stop("cannot locate any lines containing 'PARAMETER_HEADER'")
    ## namesWithin <- parameterStart[1]:dataStart[1]
    ## extract column codes in a step-by-step way, to make it easier to adjust if the format changes

    if (TRUE) { # FIXME: delete this later, after recoding to get individualized NA codes
        ## The mess below hides warnings on non-numeric missing-value codes.
        options <- options('warn')
        options(warn=-1)
        nullValue <- NA
        t <- try({nullValue <- as.numeric(findInHeader("NULL_VALUE", lines)[1])},
            silent=TRUE)
        if (class(t) == "try-error") {
            nullValue <- findInHeader("NULL_VALUE", lines)[1]
        }
        options(warn=options$warn)
    }
    ODForiginalNames <- NULL
    ODFnames <- NULL
    ODFunits <- NULL
    for (l in linePARAMETER_HEADER) {
        ## message("\nl=", l)
        lstart <- l + 1
        ## Isolate this block. Note that there seem to be two ways to end blocks.
        lend <- 0
        for (ll in seq.int(lstart, min(lstart + 100, nlines))) {
            if (length(grep("^\\s*(PARAMETER_HEADER|RECORD_HEADER)", lines[ll]))) {
                lend <- ll
                break
            }
        }
        if (lend < 0)
            stop("cannot find the end of a PARAMETER_HEADER block starting at line ", lstart-1)
        ## CODE (which is mandatory)
        iCODE <- grep("^\\s*(WMO_)?CODE\\s*=\\s*'?", lines[lstart:lend])
        if (length(iCODE) == 0)
            stop("cannot locate a CODE line in a PARAMETER_HEADER block starting at line ", lstart-1)
        if (length(iCODE) > 1)
            stop("cannot handle two CODE lines in a PARAMETER_HEADER block starting at line ", lstart-1)
        ## message("lines[", lstart+iCODE-1, "] is \"", lines[lstart+iCODE-1], "\"")
        CODE <- gsub("^\\s*(WMO_)?CODE\\s*=\\s*'?([^',]*)'?,?\\s*$", "\\2", lines[lstart+iCODE-1])
        ## message("    CODE = \"", CODE, "\"")
        if (length(grep("QQQQ", CODE))) {
            iNAME <- grep("^\\s*NAME\\s*=\\s*'", lines[lstart:lend])
            if (length(iNAME) == 1) {
                NAME <- paste(gsub("^.*:\\s*'?(.*)([_0-9]*)'?.*$", "\\1", lines[lstart+iNAME-1]), "Flag", sep="")
            } else {
                stop("cannot link flag to variable name in a PARAMETER_HEADER block starting at line ", lstart-1)
            }
        } else {
            NAME <- CODE
        }
        ## message("    NAME = \"", NAME, "\"")
        ## UNIT (which are optional)
        iUNITS <- grep("^\\s*UNITS\\s*=\\s*'?", lines[lstart:lend])
        if (length(iUNITS) == 0) {
            UNITS <- ""
        } else {
            ##message("lines[", lstart+iUNITS-1, "] is \"", lines[lstart+iUNITS-1], "\"")
            UNITS <- gsub("^\\s*UNITS\\s*=\\s*'?(.*)',?\\s*$", "\\1", lines[lstart+iUNITS[1]-1])
        }
        ##message("    UNITS = \"", UNITS, "\"")
        ODFnames <- c(ODFnames, NAME)
        ODFunits <- c(ODFunits, UNITS)
        ODForiginalNames <- c(ODForiginalNames, CODE)


        ##> for (ll in seq.int(l+1, min(l+100, nlines))) {
        ##>     ## message("; ll=", ll)
        ##>     ##> if (length(grep(",\\s*$", lines[ll], invert=TRUE))) break
        ##>     ## It is not clear how we can know when a block ends. Some files follow
        ##>     ## a pattern that the last line of the block does not have a trailing
        ##>     ## comma, but I coded for that (##> above) and it fails on other files,
        ##>     ## so I am going to break when I see some patterns that come up in file
        ##>     ## that are in my possession.
        ##>     ## FIXME: find the official syntax of PARAMETER_HEADER blocks.
        ##>     ## /Library/Frameworks/R.framework/Versions/3.4/Resources/library/oce/extdata/CTD_BCD2014666_008_1_DN.ODF
        ##>     ##if (length(grep("^\\s*NAME\\s*=\\s*'", lines[ll]))) {
        ##>     message("FIXME: detect flags by finding QQQQ?")
        ##>     if (length(grep("^\\s*CODE\\s*=\\s*'", lines[ll]))) {
        ##>         message("CODE at ll = ", ll, "; line is: <", lines[ll], ">")
        ##>         ## Trim start/end material for both data and flag cases.
        ##>         tmp <- gsub("^\\s*CODE\\s*=\\s*'(.*)\\s*',\\s*$", "\\1", lines[ll])
        ##>         message("tmp \"", tmp, "\"")
        ##>         if (length(grep("Quality Flag for Parameter:", tmp, ignore.case=TRUE))) {
        ##>             ## "  NAME= 'Quality Flag for Parameter: SIGP_01',"
        ##>             ##thisName <- paste(gsub("'.*$", "", gsub("^.*:\\s*", "", lines[ll])), "Flag", sep="")
        ##>             thisName <- gsub("^.*:\\s*", "", tmp)
        ##>             message(" flag thisName \"", thisName, "\"")
        ##>             ODForiginalNames <- c(ODForiginalNames, paste("...", thisName, sep=""))
        ##>             thisNameShortened <- gsub("_[0-9]*", "", thisName)
        ##>             thisFlag <- paste(thisNameShortened, "Flag", sep="")
        ##>             message("    flag '", thisFlag, "'")
        ##>             ODFnames <- c(ODFnames, thisFlag)
        #>         } else if (length(grep("Quality Flag:", tmp, ignore.case=TRUE))) {
        ##>             ## "  NAME= 'Quality flag: QCFF',"
        ##>             thisName <- gsub("^.*:\\s*", "", tmp)
        ##>             message(" flag thisName \"", thisName, "\"")
        ##>             ODForiginalNames <- c(ODForiginalNames, paste("...", thisName, sep=""))
        ##>             thisNameShortened <- gsub("_[0-9]*", "", thisName)
        ##>             thisFlag <- "QCFlag"
        ##>             message("    flag '", thisFlag, "'")
        ##>             ODFnames <- c(ODFnames, thisFlag)
        ##>         } else {
        ##>             ## "  NAME= 'SIGP_01',"
        ##>             ##thisName <- gsub("^\\s*NAME\\s*=\\s*'(.*)\\s*'.*$","\\1", lines[ll])
        ##>             thisName <- tmp
        ##>             ODForiginalNames <- c(ODForiginalNames, thisName)
        ##>             thisNameShortened <- gsub("_[0-9]*", "", thisName)
        ##>             message("    name '", thisName, "' (from CODE)")
        ##>             ##ODFnames <- c(ODFnames, gsub("\\s*',\\s*$", "", gsub("^\\s*NAME\\s*=\\s*'", "", lines[ll])))
        ##>             ODFnames <- c(ODFnames, thisNameShortened)
        ##>         }
        ##>     }
        ##>     if (length(grep("^\\s*UNITS\\s*=\\s*'", lines[ll]))) {
        ##>         message("UNIT at ll = ", ll, "; line is: <", lines[ll], ">")
        ##>         thisUnit <- gsub("\\s*',\\s*$", "", gsub("^\\s*UNITS\\s*=\\s*'", "", lines[ll]))
        ##>         message("    unit '", thisUnit, "'")
        ##>         ODFunits <- c(ODFunits, thisUnit)
        ##>     }
        ##>     ##message("> ", lines[ll])
        ##> }
    }
    ## print(data.frame(ODFnames, ODFunits, ODForiginalNames))
    ##> ODFunits <- lines[grep("^\\s*UNITS\\s*=", lines)]
    ##> ODFunits <- gsub("^[^']*'(.*)'.*$", "\\1", ODFunits) # e.g.  "  UNITS= 'none',"
    ##> ODFunits <- trimws(ODFunits)
    options(warn=options$warn)
    oceDebug(debug, "nullValue=", nullValue, "; it's class is ", class(nullValue), "\n")

    ODFunits <- lines[grep("^\\s*UNITS\\s*=", lines)]
    ODFunits <- gsub("^[^']*'(.*)'.*$", "\\1", ODFunits) # e.g.  "  UNITS= 'none',"
    ODFunits <- trimws(ODFunits)
    ##message("below is ODFunits...")
    ##print(ODFunits)

    ##> ODFnames <- lines[grep("^\\s*CODE\\s*=", lines)]
    ##> ODFnames <- gsub("^.*CODE=", "", ODFnames)
    ##> ODFnames <- gsub(",", "", ODFnames)
    ##> ODFnames <- gsub("^[^']*'(.*)'.*$", "\\1", ODFnames) # e.g. "  CODE= 'CNTR_01',"

    ##> if (length(ODFnames) < 1) {
    ##>     ODFnames <- lines[grep("^\\s*WMO_CODE\\s*=", lines)]
    ##>     ODFnames <- gsub("^.*WMO_CODE=", "", ODFnames)
    ##>     ODFnames <- gsub(",", "", ODFnames)
    ##>     ODFnames <- gsub("^[^']*'(.*)'.*$", "\\1", ODFnames) # e.g. "  CODE= 'CNTR_01',"
    ##> }
    ##message("below is ODFnames...")
    ##print(ODFnames)

    ##> oceDebug(debug, "ODFnames: ", paste(ODFnames, collapse=" "), "\n")
    ##> ODFnames <- gsub("_1$", "", ODFnames)
    ##> oceDebug(debug, "ODFnames: ", paste(ODFnames, collapse=" "), "\n")

    namesUnits <- ODFNames2oceNames(ODFnames, ODFunits, PARAMETER_HEADER=NULL, columns=columns, debug=debug-1)
    ##names <- ODFName2oceName(ODFnames, PARAMETER_HEADER=NULL, columns=columns, debug=debug-1)
    oceDebug(debug, "oce names:", paste(namesUnits$names, collapse=" "), "\n")
    scientist <- findInHeader("CHIEF_SCIENTIST", lines)
    ship <- findInHeader("PLATFORM", lines) # maybe should rename, e.g. for helicopter
    institute <- findInHeader("ORGANIZATION", lines) # maybe should rename, e.g. for helicopter
    station <- findInHeader("EVENT_NUMBER", lines)
    latitude <- as.numeric(findInHeader("INITIAL_LATITUDE", lines))
    longitude <- as.numeric(findInHeader("INITIAL_LONGITUDE", lines))
    cruise <- findInHeader("CRUISE_NAME", lines)
    countryInstituteCode <- findInHeader("COUNTRY_INSTITUTE_CODE", lines)
    cruiseNumber <- findInHeader("CRUISE_NUMBER", lines)
    DATA_TYPE <- trimws(findInHeader("DATA_TYPE", lines))
    deploymentType <- if ("CTD" == DATA_TYPE) "profile" else if ("MCTD" == DATA_TYPE) "moored" else "unknown"
    ## date <- strptime(findInHeader("START_DATE", lines), "%b %d/%y")

    ## if any changes here, update ctd.R @ ODF_CTD_LINK {
    startTime <- as.POSIXct(strptime(tolower(findInHeader("START_DATE_TIME", lines)), "%d-%b-%Y %H:%M:%S", tz="UTC"))
    eventNumber <- findInHeader("EVENT_NUMBER", lines) # synchronize with ctd.R at ODFMETADATA tag
    eventQualifier <- findInHeader("EVENT_QUALIFIER", lines)# synchronize with ctd.R at ODFMETADATA tag
    ## } ODF_CTD_LINK

    ## endTime <- strptime(tolower(findInHeader("END_DATE_TIME", lines)), "%d-%b-%Y %H:%M:%S", tz="UTC")

    ## FIXME: The next block tries to infer a single numeric NA value, if
    ## FIXME: possible; otherwise it returns the first value.  Perhaps we should be
    ## FIXME: keeping all these values and using them for individual columns, but (a)
    ## FIXME: non-numeric values seem to be restricted to times, and times seem never
    ## FIXME: to equal NULL_VALUE and (b) all files that I've seen have just a single
    ## FIXME: numerical NULL_VALUE and (c) what should we do if there are elements in
    ## FIXME: the header, which are not in columns?
    NAvalue <- unlist(findInHeader("NULL_VALUE", lines, FALSE))
    ##> message("NAvalue=", paste(NAvalue, collapse=" "))
    NAvalue <- gsub("D([+-])+", "e\\1", NAvalue)
    ##> message("NAvalue=", paste(NAvalue, collapse=" "))
    NAvalue <- NAvalue[!grepl("[a-df-zA-DFZ]+", NAvalue)] # remove e.g. times
    ##> message("NAvalue=", paste(NAvalue, collapse=" "))
    if (length(NAvalue) > 1) {
        NAvalue <- gsub("D", "e", NAvalue) # R does not like e.g. "-.99D+02"
        NAvalue <- try({as.numeric(unlist(NAvalue))}, silent=TRUE)
        isNumeric <- is.numeric(NAvalue)
        if (any(!isNumeric)) {
            warning("ignoring non-numeric NULL_VALUE (", NAvalue, ")")
        }
        if (any(isNumeric)) {
            tmp <- NAvalue[isNumeric]
            tmp <- tmp[is.finite(tmp)]
            ##print(tmp)
            ltmp <- length(unique(tmp))
            if (ltmp == 0) {
                NAvalue <- NA
            } else if (1 < ltmp) {
                warning("using first of ", length(unique(tmp)), " unique numeric NULL_VALUEs")
                NAvalue <- tmp[[1]]
            }
        } else {
            NAvalue <- NAvalue[[1]]
        }
    }
    oceDebug(debug, "NAvalue=", NAvalue, "; it's class is ", class(NAvalue), "\n")

    depthMin <- as.numeric(findInHeader("MIN_DEPTH", lines))
    depthMax <- as.numeric(findInHeader("MAX_DEPTH", lines))
    sounding <- as.numeric(findInHeader("SOUNDING", lines))
    ## Compute waterDepth from "SOUNDING" by preference, or from "MAX_DEPTH" if necessary
    waterDepth <- NA
    if (length(sounding)) {
        waterDepth <- sounding[1]
    } else {
        if (length(depthMax))
            waterDepth <- depthMax[1]
    }
    type <- findInHeader("INST_TYPE", lines)
    if (length(grep("sea", type, ignore.case=TRUE)))
        type <- "SBE"
    serialNumber <- findInHeader("SERIAL_NUMBER", lines)
    model <- findInHeader("MODEL", lines)
    res <- new("odf")
    res@metadata$header <- NULL

    ## catch erroneous units on CRAT, which should be in a ratio, and hence have no units.
    ## This is necessary for the sample file inst/extdata/CTD_BCD2014666_008_1_DN.ODF
    if (length(grep("CRAT", ODFnames))) {
        which <- grep("CRAT", ODFnames)
        for (w in which) {
            ustring <- as.character(namesUnits$units[[w]]$unit)
            if (length(ustring) && ustring != "" && ustring != "ratio")
                warning("\"", ODFnames[w], "\" should be unitless, i.e. \"\", but the file has \"", ustring, "\" so that is retained in the object metadata; see ?read.odf for an example of rectifying this unit error.")
        }
    }

    res@metadata$units <- namesUnits$units
    ## res@metadata$dataNamesOriginal <- ODFnames
    ##> res@metadata$dataNamesOriginal <- as.list(ODFnames)
    res@metadata$dataNamesOriginal <- as.list(ODForiginalNames)
    names(res@metadata$dataNamesOriginal) <- namesUnits$names
    res@metadata$type <- type
    res@metadata$model <- model
    res@metadata$serialNumber <- serialNumber
    res@metadata$eventNumber <- eventNumber
    res@metadata$eventQualifier <- eventQualifier
    res@metadata$ship <- ship
    res@metadata$scientist <- scientist
    res@metadata$institute <- institute
    res@metadata$address <- NULL
    res@metadata$cruise <- cruise
    res@metadata$station <- station
    res@metadata$countryInstituteCode <- countryInstituteCode
    res@metadata$cruiseNumber <- cruiseNumber
    res@metadata$deploymentType <- deploymentType
    res@metadata$date <- startTime
    res@metadata$startTime <- startTime
    res@metadata$latitude <- latitude
    res@metadata$longitude <- longitude
    res@metadata$recovery <- NULL
    res@metadata$waterDepth <- waterDepth
    res@metadata$depthMin <- depthMin
    res@metadata$depthMax <- depthMax
    res@metadata$sounding <- sounding
    res@metadata$sampleInterval <- NA
    res@metadata$filename <- filename
    ##> ## fix issue 768
    ##> lines <- lines[grep('%[0-9.]*f', lines,invert=TRUE)]
    ## issue1226 data <- read.table(file, skip=dataStart, stringsAsFactors=FALSE)
    data <- scan(file, what="character", skip=dataStart, quiet=TRUE)
    data <- matrix(data, ncol=length(namesUnits$names), byrow=TRUE)
    data <- as.data.frame(data, stringsAsFactors=FALSE)
    ## some files have text string (e.g. dates)
    colIsChar <- as.logical(lapply(data[1,], function(l) length(grep("[a-zA-Z]", l))))
    for (j in 1:dim(data)[2]) {
        if (!colIsChar[j]) {
            ##message("colIsChar[", j, "]=", colIsChar[j], " so making col ", j, " be numeric. First value=", data[1,j])
            data[[j]] <- as.numeric(data[[j]])
        } else {
            data[[j]] <- as.character(data[[j]])
            ##message("colIsChar[", j, "]=", colIsChar[j], " so leaving col ", j, " alone. First value=", data[1,j])
        }
    }
    if (length(data) != length(namesUnits$names))
        stop("mismatch between length of data names (", length(namesUnits$names), ") and number of columns in data matrix (", length(data), ")")
    names(data) <- namesUnits$names
    if (length(NAvalue) > 0 && !is.na(NAvalue)) {
        data[data==NAvalue[1]] <- NA
    }
    if ("time" %in% namesUnits$names)
        data$time <- as.POSIXct(strptime(as.character(data$time), format="%d-%b-%Y %H:%M:%S", tz="UTC"))
    ##res@metadata$names <- namesUnits$names
    ##res@metadata$labels <- namesUnits$names
    res@data <- as.list(data)

    ## Return to water depth issue. In a BIO file, I found that the missing-value code was
    ## -99, but that a SOUNDING was given as -99.9, so this is an extra check.
    if (is.na(waterDepth) || waterDepth < 0) {
        res@metadata$waterDepth <- max(abs(res@data$pressure), na.rm=TRUE)
        warning("estimating waterDepth from maximum pressure")
    }

    ## Move flags into metadata (could have done it above).
    dnames <- names(res@data)
    iflags <- grep("Flag", dnames)
    if (length(iflags)) {
        for (iflag in iflags) {
            res@metadata$flags[[gsub("Flag", "", dnames[iflag])]] <- res@data[[iflag]]
            res@metadata$dataNamesOriginal[[iflag]] <- ""
        }
        ## remove flags from data, and then remove their orig names
        res@data[iflags] <- NULL
        res@metadata$dataNamesOriginal <- res@metadata$dataNamesOrigina[res@metadata$dataNamesOriginal!=""]
        ##res@metadata$dataNamesOriginal[[iflags]] <- NULL
    }
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # read.odf()\n")
    res
}
