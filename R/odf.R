# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4


#' Class to Store ODF Data
#'
#' This class is for data stored in a format used at Canadian
#' Department of Fisheries and Oceans laboratories. It is somewhat
#' similar to the [bremen-class], in the sense
#' that it does not apply just to a particular instrument.
#'
#' @templateVar class odf
#'
#' @templateVar dataExample {}
#'
#' @templateVar metadataExample {}
#'
#' @template slot_summary
#'
#' @template slot_put
#'
#' @template slot_get
#'
#' @references
#'
#' 1. Anthony W. Isenor and David Kellow, 2011. \emph{ODF Format Specification
#' Version 2.0}. (This is a .doc file obtained in June 2011 by Dan Kelley,
#' which no longer seems to be made available at any DFO website.)
#'
#' 2. (Unknown authors), October 2014. \emph{ODF Format Description (MLI)},
#' \url{https://ogsl.ca/wp-content/uploads/ODF_format_desc_en_0.pdf},
#' (Link verified on June 4, 2020.)
#'
#' 3. A sample ODF file in the MLI format is available at
#' \url{https://ogsl.ca/wp-content/uploads/ODF_file_example_en_0.pdf}.
#' (Link verified on June 4, 2020.)
#'
#' @author Dan Kelley
#' @family things related to odf data
#' @family classes provided by oce
setClass("odf", contains="oce")

setMethod(f="initialize",
          signature="odf",
          definition=function(.Object, time, filename="", ...) {
              .Object <- callNextMethod(.Object, ...)
              ## Assign to some columns so they exist if needed later (even if they are NULL)
              .Object@data$time <- if (missing(time)) NULL else time
              .Object@metadata$filename <- filename
              .Object@metadata$deploymentType <- "" # see ctd
              .Object@processingLog$time <- presentTime()
              .Object@processingLog$value <- "create 'odf' object"
              return(.Object)
          })

#' Extract Something From an ODF Object
#'
#' @param x an [odf-class] object.
#'
#' @template sub_subTemplate
#'
#' @family things related to odf data
setMethod(f="[[",
          signature(x="odf", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              callNextMethod()         # [[
          })

#' @title Replace Parts of an ODF Object
#'
#' @param x an [odf-class] object.
#'
#' @template sub_subsetTemplate
#'
#' @family things related to odf data
setMethod(f="[[<-",
          signature(x="odf", i="ANY", j="ANY"),
          definition=function(x, i, j, ..., value) {
              callNextMethod(x=x, i=i, j=j, ...=..., value=value) # [[<-
          })

#' Subset an ODF object
#'
#' This function is somewhat analogous to [subset.data.frame()].
#'
#' It seems likely that users will first convert the odf object into
#' another class (e.g. ctd) and use the subset method of that class;
#' note that some of those methods interpret the \dots argument.
#'
#' @param x an [odf-class] object.
#'
#' @param subset a condition to be applied to the `data` portion of
#' `x`.  See \sQuote{Details}.
#'
#' @param \dots ignored.
#'
#' @return An [odf-class] object.
#'
#' @author Dan Kelley
#'
#' @family things related to odf data
#' @family functions that subset oce objects
setMethod(f="subset",
          signature="odf",
          definition=function(x, subset, ...) {
              subsetString <- paste(deparse(substitute(expr=subset, env=environment())), collapse=" ")
              res <- x
              ##dots <- list(...)
              if (missing(subset))
                  stop("must give 'subset'")

              if (missing(subset))
                  stop("must specify a 'subset'")
              keep <- eval(substitute(expr=subset, env=environment()), envir=x@data, enclos=parent.frame(2)) # used for $ts and $ma, but $tsSlow gets another
              res <- x
              for (i in seq_along(x@data)) {
                  res@data[[i]] <- x@data[[i]][keep]
              }
              for (i in seq_along(x@metadata$flags)) {
                  res@metadata$flags[[i]] <- x@metadata$flags[[i]][keep]
              }
              res@processingLog <- processingLogAppend(res@processingLog, paste("subset(x, subset=", subsetString, ")", sep=""))
              res
          })


#' Plot an odf Object
#'
#' Plot data contained within an ODF object,
#' using [oce.plot.ts()] to create panels of time-series plots for
#' all the columns contained in the `odf` object (or just those that
#' contain at least one finite value, if `blanks` is `FALSE`).
#' If the object's `data` slot does not contain `time`, then
#' [pairs()] is used to plot all the elements in the `data`
#' slot that contain at least one finite value.
#' These actions are both
#' crude and there are no arguments to control the behaviour, but this
#' function is really just a stop-gap measure, since in practical work
#' [odf-class] objects are usually cast to other types, and those types
#' tend to have more useful plots.
#'
#' @param x an [odf-class] object.
#'
#' @param blanks A logical value that indicates whether to include dummy
#' plots for data items that lack any finite values.
#'
#' @template debugTemplate
#'
#' @author Dan Kelley
#'
#' @family functions that plot oce data
#' @family things related to odf data
#'
#' @aliases plot.odf
setMethod(f="plot",
          signature=signature("odf"),
          definition=function(x, blanks=TRUE,
                              debug=getOption("oceDebug")) {
              oceDebug(debug, "plot,odf-method(..., blanks=", blanks, "...) {\n", sep="", unindent=1)
              data <- x@data
              dataNames <- names(data)
              ## At the start, n is the number of non-time variables, but
              ## later on we might switch it to equal nok, which is the
              ## number of non-time variables that contain finite data.
              if (!("time" %in% dataNames)) {
                  finite <- unlist(lapply(data, function(col) any(is.finite(col))))
                  pairs(data.frame(data)[, finite], labels=dataNames[finite])
              } else {
                  ## Define n as the number of non-time data items and nok as the
                  ## number of such columns that contain at least 1 finite value.
                  n <- length(dataNames) - 1
                  if (blanks) {
                      nok <- n
                  } else {
                      nok <- 0
                      for (i in 1:n) {
                          if (dataNames[i] != "time" && any(is.finite(data[[i]])))
                              nok <- nok + 1
                      }
                  }
                  time <- data$time
                  if (!is.null(time)) {
                      if (!blanks)
                          n <- nok
                      if (n > 5) {
                          ## make a roughly square grid
                          N <- as.integer(0.5 + sqrt(n - 1))
                          M <- as.integer(n / N)
                          ## may need to add 1, but use a loop in case my logic is mixed up
                          ## if that would
                          while (N * M < n)
                              M <- M + 1
                          par(mfrow=c(N, M))
                          oceDebug(debug, "N=", N, ", M=", M, ", prod=", N*M, ", n=", n, "\n", sep="")
                      } else {
                          par(mfrow=c(n, 1))
                      }
                      for (i in seq_along(dataNames)) {
                          if (dataNames[i] != "time") {
                              y <- data[[dataNames[i]]]
                              yok <- any(is.finite(y))
                              if (blanks || yok)
                                  oce.plot.ts(time, y, ylab=dataNames[i], mar=c(2, 3, 0.5, 1), drawTimeRange=FALSE)
                              if (!yok)
                                  warning(paste("In plot,odf-method() : '", dataNames[i], "' has no finite data", sep=""), call.=FALSE)
                          }
                      }
                  }
              }
              oceDebug(debug, "} # plot,odf-method\n", sep="", unindent=1)
          })


#' Summarize an ODF Object
#'
#' Pertinent summary information is presented, including the station name,
#' sampling location, data ranges, etc.
#'
#' @param object an [odf-class] object.
#'
#' @param \dots further arguments passed to or from other methods.
#'
#' @return A matrix containing statistics of the elements of the `data`
#' slot.
#'
#' @author Dan Kelley
#'
#' @family things related to odf data
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
              invisible(callNextMethod()) # summary
          })



## find first match in header
findInHeader <- function(key, lines, returnOnlyFirst=TRUE, numeric=FALSE, prefix=TRUE) # local function
{
    if (prefix)
        key <- paste("^[ ]*", key, sep="")
    i <- grep(key, lines)
    rval <- ""
    rval <- list()
    for (j in seq_along(i)) {
        ##. cat("j=", j, ", i=", i, "\n")
        ## ----------
        ## RISKY CODE: only look at first match
        ## ----------
        ## isolate the RHS of the eqquality
        tmp <- gsub("\\s*$", "", gsub("^\\s*", "", gsub("'", "", gsub(",", "", strsplit(lines[i[j]], "=")[[1]][2]))))
        ## convert e.g. D+00 to e+00
        if (length(grep("[-A-CF-Za-cf-z ]", tmp))) {
            ##. cat("case A. tmp '", tmp, "'\n", sep="")
            rval[[j]] <- tmp
        } else {
            ##. cat("case B. tmp '", tmp, "'\n", sep="")
            tmp <- gsub("(.*)D([-+])([0-9]{2})", "\\1e\\2\\3", tmp)
            number <- 0 == length(grep("[-+.0-9eEdD ]*", tmp))
            ##. cat("number=", number, "\n")
            rval[[j]] <- if (number && numeric) as.numeric(tmp) else tmp
        }
        ##.message("j=", j, " end")
    }
    ##.message("A")
    ##.print(rval)
    ##? if (numeric)
    ##?     rval <- as.numeric(rval)
    ##.message("B")
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

#' Translate from ODF Names to Oce Names
#'
#' Translate data names in the ODF convention to similar names in the Oce convention.
#'
#' The following table gives the recognized ODF code names for variables,
#' along with the translated names as used in oce objects. Note that the
#' code names are appended with strings such as `"_01"`, `"_02"`, etc,
#' for repeats. The converted name for an `"_01"` item is as shown below,
#' and for e.g. `"_02"` a suffix 2 is added to the oce name, etc.
#' Note that quality-control items (with names starting with `"QQQQ"`)
#' are given matching names.
#' \tabular{lll}{
#' **ODF name** \tab **Oce name**      \tab **Notes**                                    \cr
#' `ACO2` \tab `CO2Atmosphere`         \tab                                              \cr
#' `ALTB` \tab `altimeter`             \tab                                              \cr
#' `ALKW` \tab `alkalinity`            \tab                                              \cr
#' `AMON` \tab `ammonium`              \tab                                              \cr
#' `ATMP` \tab `pressureAtmosphere`    \tab                                              \cr
#' `ATTU` \tab `attenuation`           \tab                                              \cr
#' `AUTH` \tab `authority`             \tab                                              \cr
#' `BATH` \tab `barometricDepth`       \tab                                              \cr
#' `BEAM` \tab `a`                     \tab                                              \cr
#' `BNO7` \tab `bestNODC7Number`       \tab that is an "oh" letter, not a zero           \cr
#' `CNTR` \tab `scan`                  \tab                                              \cr
#' `CPHL` \tab `chlorophyll`           \tab                                              \cr
#' `CRAT` \tab `conductivity`          \tab Conductivity ratio (may have spurious unit)  \cr
#' `CMNT` \tab `comment`               \tab                                              \cr
#' `CNDC` \tab `conductivity`          \tab Conductivity in mS/cm or S/m                 \cr
#' `COND` \tab `conductivity`          \tab Conductivity in mS/cm or S/m                 \cr
#' `DCHG` \tab `discharge`             \tab                                              \cr
#' `DENS` \tab `density`               \tab                                              \cr
#' `DEPH` \tab `pressure`              \tab                                              \cr
#' `DOXY` \tab `oxygen`                \tab                                              \cr
#' `DPDT` \tab `dpdt`                  \tab                                              \cr
#' `DPWT` \tab `dryWeight`             \tab                                              \cr
#' `DRYT` \tab `temperatureDryBulb`    \tab                                              \cr
#' `ERRV` \tab `error`                 \tab                                              \cr
#' `EWCT` \tab `u`                     \tab                                              \cr
#' `FFFF` \tab `overall(FFFF)`         \tab Archaic overall flag, replaced by `QCFF`     \cr
#' `FLOR` \tab `fluorometer`           \tab                                              \cr
#' `FWETLABS` \tab `fwetlabs`          \tab                                              \cr
#' `GEOP` \tab `geopotential`          \tab                                              \cr
#' `GSPD` \tab `windSpeedGust`         \tab                                              \cr
#' `HCDM` \tab `directionMagnetic`     \tab                                              \cr
#' `HCDT` \tab `directionTrue`         \tab                                              \cr
#' `HCSP` \tab `speedHorizontal`       \tab                                              \cr
#' `HEAD` \tab `heading`               \tab                                              \cr
#' `IDEN` \tab `sampleNumber`          \tab                                              \cr
#' `LABT` \tab `temperatureLaboratory` \tab                                              \cr
#' `LATD` \tab `latitude`              \tab                                              \cr
#' `LHIS` \tab `lifeHistory`           \tab                                              \cr
#' `LOND` \tab `longitude`             \tab                                              \cr
#' `LPHT` \tab `pHLaboratory`          \tab                                              \cr
#' `NSCT` \tab `v`                     \tab                                              \cr
#' `NUM_` \tab `scansPerAverage`       \tab                                              \cr
#' `MNSV` \tab `retentionFilterSize`   \tab                                              \cr
#' `MNSZ` \tab `organismSizeMinimum`   \tab                                              \cr
#' `MODF` \tab `additionalTaxonomicInformation` \tab                                     \cr
#' `MXSZ` \tab `organismSizeMaximum`   \tab                                              \cr
#' `NONE` \tab `noWMOcode`             \tab                                              \cr
#' `NTRI` \tab `nitrite`               \tab                                              \cr
#' `NTRZ` \tab `nitrite+nitrate`       \tab                                              \cr
#' `OCUR` \tab `oxygenCurrent`         \tab                                              \cr
#' `OSAT` \tab `oxygenSaturation`      \tab                                              \cr
#' `OTMP` \tab `oxygenTemperature`     \tab                                              \cr
#' `OXYV` \tab `oxygenVoltage`         \tab                                              \cr
#' `OXV_` \tab `oxygenVoltageRaw`      \tab                                              \cr
#' `PCO2` \tab `CO2`                   \tab                                              \cr
#' `PHA_` \tab `phaeopigment`          \tab                                              \cr
#' `PHOS` \tab `phosphate`             \tab                                              \cr
#' `PHPH` \tab `pH`                    \tab                                              \cr
#' `PHT_` \tab `pHTotal`               \tab                                              \cr
#' `PHY_` \tab `phytoplanktonCount`    \tab                                              \cr
#' `POTM` \tab `theta`                 \tab                                              \cr
#' `PRES` \tab `pressure`              \tab                                              \cr
#' `PSAL` \tab `salinity`              \tab                                              \cr
#' `PSAR` \tab `par`                   \tab                                              \cr
#' `PTCH` \tab `pitch`                 \tab                                              \cr
#' `QCFF` \tab `overall(QCFF)`         \tab Overall flag (see also archaic FFFF)         \cr
#' `REFR` \tab `reference`             \tab                                              \cr
#' `RELH` \tab `humidityRelative`      \tab                                              \cr
#' `ROLL` \tab `roll`                  \tab                                              \cr
#' `SDEV` \tab `pressureStdDev`        \tab                                              \cr
#' `SECC` \tab `SecchiDepth`           \tab                                              \cr
#' `SEX_` \tab `sex`                   \tab                                              \cr
#' `SIGP` \tab `sigmaTheta`            \tab                                              \cr
#' `SIGT` \tab `sigmat`                \tab                                              \cr
#' `SLCA` \tab `silicate`              \tab                                              \cr
#' `SNCN` \tab `scanCounter`           \tab                                              \cr
#' `SPAR` \tab `SPAR`                  \tab                                              \cr
#' `SPFR` \tab `sampleFraction`        \tab                                              \cr
#' `SPVA` \tab `specificVolumeAnomaly` \tab                                              \cr
#' `SYTM` \tab `time`                  \tab                                              \cr
#' `TAXN` \tab `taxonomicName`         \tab                                              \cr
#' `TE90` \tab `temperature`           \tab                                              \cr
#' `TEMP` \tab `temperature`           \tab                                              \cr
#' `TICW` \tab `totalInorganicCarbon`  \tab                                              \cr
#' `TILT` \tab `tilt`                  \tab                                              \cr
#' `TOTP` \tab `pressureAbsolute`      \tab                                              \cr
#' `TRB_` \tab `turbidity`             \tab                                              \cr
#' `TRBH` \tab `trophicDescriptor`     \tab                                              \cr
#' `TSN_` \tab `taxonomicSerialNumber` \tab                                              \cr
#' `UNKN` \tab `-`                     \tab                                              \cr
#' `VAIS` \tab `BVFrequency`           \tab                                              \cr
#' `VCSP` \tab `w`                     \tab                                              \cr
#' `VMXL` \tab `waveHeightMaximum`     \tab                                              \cr
#' `VRMS` \tab `waveHeightMean`        \tab                                              \cr
#' `VTCA` \tab `wavePeriod`            \tab                                              \cr
#' `WDIR` \tab `windDirection`         \tab                                              \cr
#' `WSPD` \tab `windSpeed`             \tab                                              \cr
#' `WTWT` \tab `wetWeight`             \tab                                              \cr
#' `ZOO_` \tab `zooplanktonCount`      \tab                                              \cr
#' }
#' Any code not shown in the list is transferred to the oce object without renaming, apart from
#' the adjustment of suffix numbers. The following code have been seen in data files from
#' the Bedford Institute of Oceanography: `ALTB`, `PHPH` and `QCFF`.
#'
#' @section A note on unit conventions:
#' Some older ODF files contain non-standard units for conductivity,
#' including `mho/m`, `mmho/cm`, and `mmHo`. As the
#' units for conductivity are important for derived quantities
#' (e.g. salinity), such units are converted to standard units
#' (e.g. `S/m` and `mS/cm`).  (This was once done with a warning,
#' but on 2020-02-07 the warning was removed, since it did not
#' indicate a problem with the file or the data scanning; rather,
#' it was a simple matter of nudging towards uniformity in a way
#' that ought to confuse no users, akin to converting `m**3` to
#' `m^3`, which is also done here without warning.)
#'
#' @section Consistency warning:
#' There are disagreements on variable names. For example, the ``DFO
#' Common Data Dictionary'' (reference 1)
#' has unit millimole/m^3 for NODC and MEDS, but it has unit mL/L for BIO and IML.
#'
#' @param ODFnames Vector of strings holding ODF names.
#'
#' @param ODFunits Vector of strings holding ODF units.
#'
#' @param columns Optional list containing name correspondances, as described for
#' [read.ctd.odf()].
#'
#' @param PARAMETER_HEADER Optional list containing information on the data variables.
#' @template debugTemplate
#'
#' @return A vector of strings.
#'
#' @author Dan Kelley
#'
#' @family functions that interpret variable names and units from headers
#'
#' @references
#'
#' For sources that describe the ODF format, see the documentation
#' for the [odf-class].
#'
#' @family things related to odf data
ODFNames2oceNames <- function(ODFnames, ODFunits=NULL,
                              columns=NULL, PARAMETER_HEADER=NULL, debug=getOption("oceDebug"))
{
    oceDebug(debug, "ODFNames2oceNames() {\n", unindent=1, sep="")
    n <- length(ODFnames)
    if (n != length(ODFunits)) {
        if (debug>0) cat("ODFnames: '", paste(ODFnames, collapse="' '"), "'\n", sep="")
        if (debug>0) cat("ODFunits: '", paste(ODFunits, collapse="' '"), "'\n", sep="")
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
    names <- gsub("ACO2", "CO2Atmosphere", names)
    names <- gsub("ALTB", "altimeter", names)
    names <- gsub("ALKW", "alkalinity", names)
    names <- gsub("AMON", "ammonium", names)
    names <- gsub("ATMP", "pressureAtmosphere", names)
    names <- gsub("ATTU", "attenuation", names)
    names <- gsub("AUTH", "authority", names)
    names <- gsub("BATH", "waterDepth", names) # FIXME: is this water column depth or sensor depth?
    names <- gsub("BEAM", "a", names)  # FIXME: is this sensible?
    names <- gsub("BNO7", "bestNODC7Number", names)
    names <- gsub("CNTR", "scan", names)
    names <- gsub("CPHL", "chlorophyll", names)
    names <- gsub("CRAT", "conductivity", names)
    names <- gsub("CMNT", "comment", names)
    names <- gsub("CNDC", "conductivity", names)
    names <- gsub("COND", "conductivity", names)
    names <- gsub("DCHG", "discharge", names)
    names <- gsub("DENS", "density", names)
    names <- gsub("DEPH", "depth", names)
    names <- gsub("DOXY", "oxygen", names)
    names <- gsub("DPDT", "dpdt", names)
    names <- gsub("DRWT", "dryWeight", names)
    names <- gsub("DRYT", "temperatureDryBulb", names)
    names <- gsub("ERRV", "error", names)
    names <- gsub("EWCT", "u", names)
    names <- gsub("FFFF", "overall(FFFF)Flag", names)
    names <- gsub("FLOR", "fluorometer", names)
    names <- gsub("FWETLABS", "fwetlabs", names) # FIXME: what is this?
    names <- gsub("GEOP", "geopotential", names)
    names <- gsub("GSPD", "windSpeedGust", names)
    names <- gsub("HCSP", "speedHorizontal", names)
    names <- gsub("HCDM", "directionMagnetic", names)
    names <- gsub("HCDT", "directionTrue", names)
    names <- gsub("HEAD", "heading", names)
    names <- gsub("IDEN", "sampleNumber", names)
    names <- gsub("LABT", "temperatureLaboratory", names)
    names <- gsub("LATD", "latitude", names)
    names <- gsub("LHIS", "lifeHistory", names)
    names <- gsub("LOND", "longitude", names)
    names <- gsub("LPHT", "pHLaboratory", names)
    names <- gsub("MNSV", "retentionFilterSize", names)
    names <- gsub("MNSZ", "organismSizeMinimum", names)
    names <- gsub("MODF", "additionalTaxonomicInformation", names)
    names <- gsub("MXSV", "largestSieveUsed", names)
    names <- gsub("MXSZ", "organismSizeMaximum", names)
    names <- gsub("NONE", "noWMOcode", names)
    names <- gsub("NTRI", "nitrite", names)
    names <- gsub("NTRZ", "nitrite+nitrate", names)
    names <- gsub("NSCT", "v", names)
    names <- gsub("NUM_", "scansPerAverage", names)
    names <- gsub("OCUR", "oxygenCurrent", names)
    names <- gsub("OSAT", "oxygenSaturation", names)
    names <- gsub("OTMP", "oxygenTemperature", names)
    names <- gsub("OXYV", "oxygenVoltage", names)
    names <- gsub("OXV_", "oxygenVoltageRaw", names)
    names <- gsub("PCO2", "CO2", names)
    names <- gsub("PHA_", "phaeopigment", names)
    names <- gsub("PHOS", "phosphate", names)
    names <- gsub("PHPH", "pH", names)
    names <- gsub("PHT_", "pHTotal", names)
    names <- gsub("PHY_", "phytoplanktonCount", names)
    names <- gsub("POTM", "theta", names) # in a moored ctd file examined 2014-05-15
    names <- gsub("PRES", "pressure", names)
    names <- gsub("PSAL", "salinity", names)
    names <- gsub("PSAR", "par", names)
    names <- gsub("PTCH", "pitch", names)
    names <- gsub("QCFF", "overall(QCFF)Flag", names)
    names <- gsub("REFR", "reference", names)
    names <- gsub("RELH", "humdidityRelative", names)
    names <- gsub("ROLL", "roll", names)
    names <- gsub("SDEV", "pressureStdDev", names)
    names <- gsub("SECC", "SecchiDepth", names)
    names <- gsub("SEX_", "sex", names)
    names <- gsub("SIGP", "sigmaTheta", names)
    names <- gsub("SIGT", "sigmat", names) # in a moored ctd file examined 2014-05-15
    names <- gsub("SLCA", "silicate", names)
    names <- gsub("SNCN", "scanCounter", names)
    names <- gsub("SPAR", "SPAR", names)
    names <- gsub("SPFR", "sampleFraction", names)
    names <- gsub("SPVA", "specificVolumeAnomaly", names)
    names <- gsub("SYTM", "time", names) # in a moored ctd file examined 2014-05-15
    names <- gsub("TAXN", "taxonomicName", names)
    names <- gsub("TE90", "temperature", names)
    names <- gsub("TEMP", "temperature", names)
    names <- gsub("TICW", "totalInorganicCarbon", names)
    names <- gsub("TILT", "tilt", names)
    names <- gsub("TOTP", "pressureAbsolute", names)
    names <- gsub("TRB_", "turbidity", names)
    names <- gsub("TRPH", "trophicDescriptor", names)
    names <- gsub("TSN_", "taxonomicSerialNumber", names)
    names <- gsub("UNKN", "unknown", names)
    names <- gsub("VAIS", "BVFrequency", names)
    names <- gsub("VCSP", "w", names)
    names <- gsub("VMXL", "waveHeightMaximum", names)
    names <- gsub("VRMS", "waveHeightMean", names)
    names <- gsub("VTCA", "wavePeriod", names)
    names <- gsub("WDIR", "windDirection", names)
    names <- gsub("WSPD", "windSpeed", names)
    names <- gsub("WTWT", "wetWeight", names)
    names <- gsub("ZOO_", "zooplanktonCount", names)
    ## Fix up suffixes.
    names <- gsub("_[0-9][0-9]", "", names)
    oceDebug(debug, "STAGE 2 names: ", paste(names, collapse=" "), "\n")
    oceDebug(debug, "STAGE 3 names: ", paste(names, collapse=" "), "\n")
    names <- unduplicateNames(names)
    oceDebug(debug, "STAGE 4 names: ", paste(names, collapse=" "), "\n")
    ## Handle units
    units <- list()
    oceDebug(debug, "STAGE 5 units: ", paste(units, collapse=" "), "\n")
    ODFunits <- gsub("^/", "1/",ODFunits)
    oceDebug(debug, "STAGE 6 units: ", paste(units, collapse=" "), " (after changing '/*' to '1/*')\n")
    for (i in seq_along(names)) {
        ## NOTE: this was originally coded with ==, but as errors in ODF
        ## formatting have been found, I've moved to grep() instead; for
        ## example, the sigma-theta case is done that way, because the
        ## original code expected kg/m^3 but then (issue 1051) I ran
        ## across an ODF file that wrote density as Kg/m^3.
        oceDebug(debug, paste("ODFnames[",i,"]='",ODFnames[i],"', names[",i,"]='", names[i], "', ODFunits[", i, "]='", ODFunits[i], "'\n", sep=""))
        thisUnit <- trimws(ODFunits[i])
        units[[names[i]]] <- if (thisUnit == "10**3cells/L") {
            list(unit=expression(10^3*cells/l), scale="")
        } else if (thisUnit == "code") {
            list(unit=expression(), scale="")
        } else if (thisUnit == "counts") {
            list(unit=expression(), scale="")
        } else if (thisUnit == "db") {
            list(unit=expression(dbar), scale="")
        } else if (thisUnit == "decibars") {
            list(unit=expression(dbar), scale="")
        } else if (grepl("^deg(ree)?(s)?$", thisUnit, ignore.case=TRUE)) {
            list(unit=expression(degree), scale="")
        } else if (thisUnit == "degrees C" || thisUnit == "Degrees C") {
            list(unit=expression(degree*C), scale="ITS-90") # guess on scale
        } else if (thisUnit == "IPTS-68, deg C" || thisUnit == "ITS-68, deg C") {
            ## handles both the correct IPTS and the incorrect ITS.
            list(unit=expression(degree*C), scale="IPTS-68")
        } else if (thisUnit == "FTU") {
            list(unit=expression(FTU), scale="")
        } else if (thisUnit == "g") {
            list(unit=expression(g), scale="")
        } else if (thisUnit == "GMT") {
            list(unit=expression(), scale="")
        } else if (thisUnit == "hPa") {
            list(unit=expression(hPa), scale="")
        } else if (thisUnit == "ITS-90, deg C4") {
            list(unit=expression(degree*C), scale="ITS-90")
        } else if (thisUnit == "hertz" || thisUnit == "Hertz") {
            list(unit=expression(Hz), scale="")
        } else if (thisUnit == "kg/m^3" || thisUnit == "kg/m**3") {
            list(unit=expression(kg/m^3), scale="")
        } else if (thisUnit == "m") {
            list(unit=expression(m), scale="")
        } else if (thisUnit == "m**3/s") {
            list(unit=expression(m^3/s), scale="")
        } else if (thisUnit == "metres") {
            list(unit=expression(m), scale="")
        } else if (thisUnit == "m**3/kg") {
            list(unit=expression(m^3/kg), scale="")
        } else if (thisUnit == "mg/m^3") {
            list(unit=expression(mg/m^3), scale="")
        } else if (thisUnit == "mg/m**3") {
            list(unit=expression(mg/m^3), scale="")
        } else if (thisUnit == "ml/l") {
            list(unit=expression(ml/l), scale="")
        } else if (thisUnit == "m/s" || thisUnit == "M/s") {
            list(unit=expression(m/s), scale="")
        } else if (thisUnit == "m/sec") { #1 == length(grep("^\\s*m/sec\\s*$", thisUnit, ignore.case=TRUE))) {
            list(unit=expression(m/s), scale="")
        } else if (thisUnit == "m^-1/sr") { #1 == length(grep("^\\s*m\\^-1/sr\\s*$", thisUnit, ignore.case=TRUE))) {
            list(unit=expression(1/m/sr), scale="")
        } else if (grepl("^\\s*mho[s]{0,1}/m\\s*$", thisUnit, ignore.case=TRUE)) {
            ##20200207 warning('Changed unit mho/m to S/m for conductivity')
            list(unit=expression(S/m), scale="")
        #} else if (1 == length(grep("^\\s*micro[ ]?mols/m2/s\\s*$", thisUnit, ignore.case=TRUE))) {
        #    list(unit=expression(mu*mol/m^2/s), scale="")
        } else if (grepl("^\\s*mmho[s]?/cm\\s*$", thisUnit, ignore.case=TRUE)) {
            ##20200207 warning('Changed unit mmho/cm to mS/cm for conductivity')
            list(unit=expression(mS/cm), scale="")
        } else if (thisUnit == "mmHo") {
            ##20200207 warning('Changed unit mmHo to S/m for conductivity')
            list(unit=expression(S/m), scale="")
        ##} else if (thisUnit == "[(]*none[)]$") {
        } else if (grepl("^[(]*none[)]*$", thisUnit, ignore.case=TRUE)) {
            list(unit=expression(), scale="")
        } else if (thisUnit == "NBS scale") {
            list(unit=expression(), scale="NBS scale")
        } else if (thisUnit == "NTU") {
            list(unit=expression(NTU), scale="")
        } else if (thisUnit == "ppm" || thisUnit == "PPM") {
            list(unit=expression(ppm), scale="")
        } else if (thisUnit == "psu" || thisUnit == "PSU") {
            list(unit=expression(), scale="PSS-78")
        } else if (thisUnit == "ma") { # grepl("^\\s*ma\\s*$", thisUnit, ignore.case=TRUE)) {
            list(unit=expression(ma), scale="")
        } else if (thisUnit == "metres/sec") {
            list(unit=expression(m/s), scale="")
        } else if (thisUnit == "microns") {
            list(unit=expression(mu*m), scale="")
        } else if (grepl("^\\s*micro[ ]?mol[e]?s/m(\\*){0,2}2/s(ec)?\\s*$", thisUnit, ignore.case=TRUE)) {
            list(unit=expression(mu*mol/m^2/s), scale="")
        } else if (thisUnit == "ratio") {
            list(unit=expression(ratio), scale="")
        } else if (grepl("^\\s*sigma-theta,\\s*kg/m\\^3\\s*$", thisUnit, ignore.case=TRUE)) {
            list(unit=expression(kg/m^3), scale="")
        } else if (thisUnit == "s" || thisUnit == "seconds") {
            list(unit=expression(s), scale="")
        } else if (thisUnit == "S/m") {
            list(unit=expression(S/m), scale="")
        } else if (thisUnit == "Total scale") {
            list(unit=expression(), scale="")
        } else if (thisUnit == "True degrees") {
            list(unit=expression(), scale="")
        } else if (thisUnit == "uA") {
            list(unit=expression(mu*a), scale="")
        } else if (thisUnit == "ueinsteins/s/m**2") { # grepl("^\\s*ueinsteins/s/m\\*\\*2\\s*$", thisUnit, ignore.case=TRUE)) {
            list(unit=expression(mu*einstein/s/m^2), scale="")
        } else if (thisUnit == "ug/l") { # grepl("^\\s*ug/l\\s*$", thisUnit, ignore.case=TRUE)) {
            list(unit=expression(mu*g/l), scale="")
        } else if (grepl("^\\s*mmol/m\\*\\*3\\s*$", thisUnit, ignore.case=TRUE)) {
            list(unit=expression(m*mol/m^3), scale="")
        } else if (thisUnit == "umol/kg") { # grepl("^\\s*umol/kg\\s*$", thisUnit, ignore.case=TRUE)) {
            list(unit=expression(m*mol/kg), scale="")
        } else if (thisUnit == "umol/m**3") { # grepl("^\\s*umol/m\\*\\*3\\s*$", thisUnit, ignore.case=TRUE)) {
            list(unit=expression(mu*mol/m^3), scale="")
        } else if (thisUnit == "umol/m**2/s") { # grepl("^\\s*umol/m\\*\\*2/s\\s*$", thisUnit, ignore.case=TRUE)) {
            list(unit=expression(mu*mol/m^2/s), scale="")
        } else if (thisUnit == "umol photons/m2/s") { # grepl("^\\s*umol[ ]*photons/m2/s\\s*$", thisUnit, ignore.case=TRUE)) {
            list(unit=expression(mu*mol/m^2/s), scale="")
        } else if (thisUnit == "UTC") {
            list(unit=expression(), scale="")
        } else if (thisUnit == "V") {
            list(unit=expression(V), scale="")
        } else if (thisUnit == "1/cm") {
            list(unit=expression(1/cm), scale="")
        } else if (thisUnit == "1/m") {
            list(unit=expression(1/m), scale="")
        } else if (thisUnit == "%") {
            list(unit=expression("%"), scale="")
        } else if (thisUnit == "volts") {
            list(unit=expression(V), scale="")
        } else if (nchar(thisUnit) == 0) {
            list(unit=expression(), scale="")
        } else {
            # print(names)
            warning("unable to interpret ODFunits[", i, "]='", thisUnit, "', for item code-named '", names[i], "', so making an educated guess using parse() or, as a last-ditch effort, simply copying the string", sep="")
            uu <- try(parse(text=thisUnit), silent=TRUE)
            if (class(uu) == "try-error")
                uu <- thisUnit
            list(unit=uu, scale="")
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


#' Create ODF object from the output of `ODF::read_ODF()`.
#'
#' As of August 11, 2015, `ODF::read_ODF` returns a list with 9 elements,
#' one named `DATA`, which is a [data.frame()] containing the
#' columnar data, the others being headers of various sorts.  The present
#' function constructs an oce object from such data, facilitating processing
#' and plotting with the general oce functions.
#' This involves storing the 8 headers verbatim in the
#' `odfHeaders` in the `metadata` slot, and also
#' copying some of the header
#' information into more standard names (e.g.  `metadata@@longitude` is a
#' copy of `metadata@@odfHeader$EVENT_HEADER$INITIAL_LATITUDE`).  As for
#' the `DATA`, they are stored in the `data` slot, after renaming
#' from ODF to oce convention using [ODFNames2oceNames()].
#'
#' @param ODF A list as returned by `read_ODF` in the `ODF` package
#'
#' @param coerce A logical value indicating whether to coerce the return value
#' to an appropriate object type, if possible.
#'
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, or to 2 to get more.
#'
#' @return An oce object, possibly coerced to a subtype.
#'
#' @section Caution: This function may change as the `ODF` package
#' changes.  Since `ODF` has not been released yet, this should not affect
#' any users except those involved in the development of `oce` and
#' `ODF`.
#'
#' @author Dan Kelley
#'
#' @family things related to odf data
ODF2oce <- function(ODF, coerce=TRUE, debug=getOption("oceDebug"))
{
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
    names(res@data) <- names
    res
}

#' Create a list of ODF header metadata
#'
#' @param header Vector of character strings, holding the header
#'
#' @return A list holding the metadata, with item names matching
#' those in the ODF header, except that duplicates are transformed
#' through the use of [unduplicateNames()].
#'
#' @family things related to odf data
ODFListFromHeader <- function(header)
{
    ## remove trailing blanks
    header <- gsub("[ ]*$", "", header)
    A <- grep("^[A-Z].*,$", header)
    h <- vector("list", length=length(A))
    names(h) <- gsub(",.*$", "", header[A])
    names(h) <- unduplicateNames(names(h), 2)
    starts <- A + 1
    ends <- c(A[-1], length(header) + 1) - 1
    for (i in seq_along(starts)) {
        ##msg(header[A[i]], "\n")
        nitems <- 1 + ends[i] - starts[i]
        ##msg("  nitems: ", nitems)
        h[[i]] <- vector("list", length=nitems)
        itemsNames <- NULL
        itemsI <- 1
        for (ii in starts[i]:ends[i]) {
            ##msg("line <", header[ii], ">")
            name <- gsub("^[ ]*([^=]*)[ ]*=.*$", "\\1", header[ii])
            ##msg("    name  <", name, ">")
            value <- gsub("^[ ]*([^=]*)[ ]*=[ ]*", "", header[ii])
            ##msg("    value <", value, "> (original)")
            ## Trim trailing comma (which seems to occur for all but last item in a list)
            if ("," == substr(value, nchar(value), nchar(value)))
                value <- substr(value, 1, nchar(value)-1)
            ##msg("    value <", value, ">  (after trailing-comma removal)")
            ## Trim leading single-quote, and its matching trailing single-quote; warn
            ## if former is present but latter is missing.
            if ("'" == substr(value, 1, 1)) {
                value <- substr(value, 2, nchar(value))
                if ("'" == substr(value, nchar(value), nchar(value))) {
                    value <- substr(value, 1, nchar(value)-1)
                } else {
                    warning("malformed string in ODF header line <", header[ii], ">\n", sep="")
                }
            }
            ##msg("    value <", value, ">    (after '' removal)")
            itemsNames <- c(itemsNames, name)
            h[[i]][[itemsI]] <- value
            itemsI <- itemsI + 1
        }
        names(h[[i]]) <- itemsNames
        names(h[[i]]) <- unduplicateNames(names(h[[i]]), 2)
    }
    h
}


#' Read an ODF file
#'
#' ODF (Ocean Data Format) is a
#' format developed at the Bedford Institute of Oceanography and also used
#' at other Canadian Department of Fisheries and Oceans (DFO) facilities
#' (see references 1 and 2).
#' It can hold various types of time-series data, which includes a variety
#' of instrument types. Thus, [read.odf()]
#' is used by `read.ctd.odf` for CTD data, etc. As of mid-2018,
#' [read.odf()] is still in development, with features being added as a
#' project with DFO makes available more files.
#'
#' Note that some elements of the metadata are particular to ODF objects,
#' e.g. `depthMin`, `depthMax` and `sounding`, which
#' are inferred from ODF items named `MIN_DEPTH`, `MAX_DEPTH`
#' and `SOUNDING`, respectively. In addition, the more common metadata
#' item `waterDepth`, which is used in `ctd` objects to refer to
#' the total water depth, is set to `sounding` if that is finite,
#' or to `maxDepth` otherwise.
#'
#' The function [ODFNames2oceNames()] is used to translate
#' data names from the ODF file to standard `oce` names, and
#' handles conversion for a few non-standard units. The documentation
#' of [ODFNames2oceNames()] should be consulted for more
#' details.
#'
#'
#' @section Metadata conventions:
#'
#' Some metadata items may be specific to certain instruments, and
#' certain research groups. It can be important for analysts to be aware of
#' the conventions used in datasets that are under study.
#' For example, as of June 2018, `adp`
#' objects created at the Bedford Institute of Oceanography may
#' have a metadata item named `depthOffBottom` (called
#' `DEPTH_OFF_BOTTOM` in ODF files), which is not typically
#' present in `ctd` files. This item illustrates the renaming
#' convention, from the CAMEL_CASE used in ODF files to the snakeCase
#' used in oce. Bearing this conversion in mind, users should not
#' find it difficult to understand the meaning of items that [read.odf()]
#' stores within the `metadata` slot. Users should bear in mind
#' that the whole ODF header is saved as a list by
#' calling the function with `header="list"`, after which
#' e.g. [str]`(rval[["header"]])` or [View]`(rval[["header"]])`
#' can be used to isolate any information of interest (but bear in mind
#' that suffices are used to disambiguate sibling items of identical
#' name in the ODF header).
#'
#' @examples
#' library(oce)
#' #
#' # 1. Read a CTD cast made on the Scotian Shelf. Note that the file's metadata
#' # states that conductivity is in S/m, but it is really conductivity ratio,
#' # so we must alter the unit before converting to a CTD object. Note that
#' # read.odf() on this data file produces a warning suggesting that the user
#' # repair the unit, using the method outlined here.
#' odf <- read.odf(system.file("extdata", "CTD_BCD2014666_008_1_DN.ODF.gz", package="oce"))
#' ctd <- as.ctd(odf) ## so we can e.g. extract potential temperature
#' ctd[["conductivityUnit"]] <- list(unit=expression(), scale="")
#' #
#' # 2. Make a CTD, and plot (with span to show NS)
#' plot(ctd, span=500)
#' #
#' # 3. Highlight bad data on TS diagram. (Note that the eos
#' # is specified, because we will extract practical-salinity and
#' # UNESCO-defined potential temperatures for the added points.)
#' plotTS(ctd, type="o", eos="unesco") # use a line to show loops
#' bad <- ctd[["QCFlag"]]!=0
#' points(ctd[['salinity']][bad],ctd[['theta']][bad],col='red',pch=20)
#'
#' @param file the file containing the data.
#'
#' @param columns An optional [list] that can be used to convert unrecognized
#' data names to resultant variable names.  For example,
#' `columns=list(salinity=list(name="salt", unit=list(unit=expression(), scale="PSS-78"))`
#' states that a short-name of `"salt"` represents salinity, and that the unit is
#' as indicated. This is passed to [cnvName2oceName()] or [ODFNames2oceNames()],
#' as appropriate, and takes precedence over the lookup table in that function.
#'
#' @param header An indication of whether, or how, to store the entire
#' ODF file header in the `metadata` slot of the returned object.
#' There are three choices for the `header` argument.
#' (1) If it is `NULL`, then the ODF header is not stored in
#' the `metadata` slot (although some of its contents are).
#' (2) If it is `"character"`, the header is stored within
#' the `metadata` as a vector named `header`, comprising
#' a character string for each line of the header within the ODF file.
#' (3) If it is `"list"`, then the `metadata` slot of the
#' returned object will contain a `list` named `header` that
#' has lists as its entries. (The sub-lists are in the form of
#' key-value pairs.) The naming of list entries is patterned on
#' that in the ODF header, except that [unduplicateNames()]
#' is used to transform repeated names by adding numerical suffices.
#' Note: on June 6, 2019, the default value of `header` was
#' changed from `NULL` to `"list"`; in addition, the resultant
#' list was made to contain every single item in the ODF header, with
#' [unduplicateNames()] being used to append integers to
#' distinguish between repeated names in the ODF format.
#'
#' @param exclude either a character value holding a regular
#' expression that is used with [grep()] to remove lines from the
#' header before processing, or `NULL` (the default), meaning
#' not to exclude any such lines.  The purpose of this argument
#' is to solve problems with some files, which can have
#' thousands of lines that indicate details that are may be of
#' little value in processing.  For example, some files have thousands
#' of lines that would be excluded by using
#' `exclude="PROCESS='Nulled the .* value"` in the function call.
#'
#' @template debugTemplate
#'
#' @return An [oce-class] object.
#'
#' @section Handling of temperature scales:
#' `read.odf()` stores temperature data directly as read from the file, which
#' might mean the IPTS68 scale.  These values should not be used to calculate
#' other seawater quantities, because formulae are generally based in ITS90
#' temperatures. To avoid problems, the accessor function converts to the modern
#' scale, e.g. `x[["temperature"]]` yields temperature in the ITS90
#' scale, whether temperatures in the original file were reported on that scale
#' or the older IPTS68 scale.
#'
#' @seealso [ODF2oce()] will be an alternative to this, once (or perhaps if) a `ODF`
#' package is released by the Canadian Department of Fisheries and Oceans.
#'
#' @references
#'
#' For sources that describe the ODF format, see the documentation
#' for the [odf-class].
#'
#' @family things related to odf data
read.odf <- function(file, columns=NULL, header="list", exclude=NULL, debug=getOption("oceDebug"))
{
    if (missing(file))
        stop("must supply 'file', a character value holding the name of an ODF file")
    if (length(file) > 1)
        stop("can only handle one file at a time (the length of 'file' is ", length(file), ", not 1)")
    if (is.character(file) && 0 == file.info(file)$size)
        stop("the file named '", file, "' is empty, and so cannot be read")
    debug <- as.integer(min(max(debug, 0), 3))
    oceDebug(debug, "read.odf(\"", file, "\", exclude=",
             if (is.null(exclude)) "NULL" else paste0("'", exclude, "'"), ", ...) {\n", unindent=1, sep="", style="bold")
    if (!is.null(header)) {
        if (!is.character(header))
            stop("the header argument must be NULL, \"character\", or \"list\"")
        if (!(header %in% c("character", "list")))
            stop("the header argument must be NULL, \"character\" or \"list\"")
    }
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
    ## Read the full file.   (In a previous version, we only read the first 1000 lines
    ## at the start, and later read the whole thing if we didn't find the DATA line.)
    lines <- readLines(file, encoding="latin1") # issue 1430 re encoding
    ## Trim excluded lines.
    if (!is.null(exclude)) {
        oldLength <- length(lines)
        lines <- lines[grep(exclude, lines, invert=TRUE)]
        oceDebug(debug, "the 'exclude' argument reduced the file line count from", oldLength, "to", length(lines), "lines\n")
    }
    ## Locate the header/data separator
    dataStart <- grep("^[ ]*-- DATA --[ ]*$", lines) # issue 1430 re leading/trailing spaces
    if (!length(dataStart))
        stop("ODF files must contain a line with \"-- DATA --\"")
    res <- new("odf")

    nlines <- length(lines)
    ## Make a list holding all the information in the header. Note that this is entirely
    ## separate from e.g. inference of longitude and latitude from a header.
    h <- gsub(",[ ]*$", "", lines[seq(1L, dataStart - 1)]) # note trimming trailing comma and maybe whitespace
    ## Handle the case where there is a blank at the start of each line. (We only check
    ## the first line, actually.) I have no idea whether this start-with-blank is part of the
    ## ODF format, but I *can* say that quite a few of the ODF files on my computer have
    ## this property.
    if (length(grep("^ ", h)))
        h <- gsub("^ ", "", h)

    categoryIndex <- grep("^[a-zA-Z]", h)
    categoryNames <- h[categoryIndex]
    headerlist <- list()
    if (length(categoryIndex > 0))
        headerlist <- vector("list", length(categoryIndex))
    oceDebug(debug > 2, "headerlist will have", length(headerlist), "items\n")
    names(headerlist) <- categoryNames
    indexCategory <- 0
    ## demo of what I will try, as a way to avoid this 2:10000 loop:
    ## ## Set up counter
    ## lhsc <- list()
    ## ## Handle an item
    ## lhs <- gsub("^[ ]*([^=]*)=(.*)$","\\1", h[i])
    ## if (!(lhs %in% names(lhsc))) lhsc[[lhs]] <- 1 else lhsc[[lhs]] <- 1+lhsc[[lhs]]
    ## lhs <- paste0(lhs, lhsc[[lhs]])
    lhsc <- list() # set up a list for counts of lhs patterns, used in renaming
    for (i in seq_along(h)) {
        if (length(grep("^[a-zA-Z]", h[i]))) {
            indexCategory <- indexCategory + 1
            headerlist[[indexCategory]] <- list()
            ##> message("* '", h[i], "' is indexCategory ", indexCategory)
            lhsUsed <- NULL
        } else {
            if (0 == indexCategory) {
                warning("cannot parse ODF header, a header line started with a space, but no previous line started with non-space")
                header <- NULL
                break
            }
            ## Use regexp to find lhs and rhs. This is better than using strsplit on '=' because some
            ## rhs have '=' in them.
            ##> oceDebug(debug > 2, "h[", i, "]='", h[i], "'\n", sep="")
            lhs <- gsub("^[ ]*([^=]*)=(.*)$","\\1", h[i])
            if (!(lhs %in% names(lhsc))) {
                lhsc[[lhs]] <- 1
            } else {
                lhsc[[lhs]] <- 1 + lhsc[[lhs]]
            }
            ##SLOW oceDebug(debug > 2, "lhs='", lhs, "'", "\n", sep="")
            lhs <- paste0(lhs, "_", lhsc[[lhs]])
            ##SLOW oceDebug(debug > 2, "lhs='", lhs, "' after renaming it to make it distinct\n", sep="")
            ##OLD ##> oceDebug(debug > 2, "  lhs='", lhs, "' (before renaming to remove duplicates)\n", sep="")
            ##OLD ok <- TRUE
            ##OLD if (lhs %in% lhsUsed) {
            ##OLD     ok <- FALSE
            ##OLD     ## This is slow, of O(N^2), since with N data, we will get to O(N) in the next loop,
            ##OLD     ## and the enclosing loop will also operatre O(N) times.
            ##OLD     for (trial in 2:10000) {
            ##OLD         if (!(paste(lhs, trial, sep="") %in% lhsUsed)) {
            ##OLD             lhs <- paste(lhs, trial, sep="")
            ##OLD             ok <- TRUE
            ##OLD             break
            ##OLD         }
            ##OLD     }
            ##OLD }
            ##OLD if (!ok)
            ##OLD     stop("cannot have more than 10000 items of the same name in ODF metadata; rerun with debug=5 to diagnose")
            rhs <- gsub("^[^=]*=[ ]*(.*)[,]*$","\\1", h[i])
            oceDebug(debug > 2, "  rhs='", rhs, "'\n", sep="")
            headerlist[[indexCategory]][[lhs]] <- rhs
            lhsUsed <- c(lhsUsed, lhs)
        }
    }
    if (length(headerlist)) {
        names(headerlist) <- unduplicateNames(names(headerlist))
    }
    res@metadata$header <- headerlist

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
        t <- try({nullValue <- as.numeric(gsub("D\\+", "e+", findInHeader("NULL_VALUE", lines))[1])},
            silent=TRUE)
        if (class(t) == "try-error") {
            nullValue <- findInHeader("NULL_VALUE", lines)[1]
        }
        options(warn=options$warn)
    }
    ODForiginalNames <- NULL
    ODFnames <- NULL
    ODFunits <- NULL
    flagTranslationTable <- list()
    NAME2CODE <- list()
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
        # CODE (taken to be mandatory)
        iCODE <- grep("^\\s*(WMO_)?CODE\\s*=\\s*'?", lines[lstart:lend])
        if (length(iCODE) == 0)
            stop("cannot locate a CODE line in PARAMETER_HEADER block starting at line ", lstart-1)
        if (length(iCODE) > 1)
            stop("cannot handle more than one CODE line in PARAMETER_HEADER block starting at line ", lstart-1)
        #oceDebug(debug, "iCODE=", iCODE, "\n")
        CODE <- gsub("^\\s*(WMO_)?CODE\\s*=\\s*'?([^',]*)'?,?\\s*$", "\\2", lines[lstart+iCODE-1])
        oceDebug(debug, "CODE \"", CODE, "\" in PARAMETER_HEADER starting at line ", lstart-1, "\n", sep="")
        # NAME (take to be mandatory), e.g. next lines (start with 2 spaces, end with comma)
        # "  NAME= 'Sea Temperature (IPTS-68)',"
        # "  NAME= 'CNTR_01',"
        iNAME <- grep("^\\s*?NAME\\s*=\\s*'?", lines[lstart:lend])
        if (length(iNAME) == 0)
            stop("cannot locate a NAME line in PARAMETER_HEADER block starting at line ", lstart-1)
        if (length(iNAME) > 1)
            stop("cannot handle more than one NAME line in PARAMETER_HEADER block starting at line ", lstart-1)
        #oceDebug(debug, "iNAME=", iNAME, "\n")
        NAME <- gsub("^\\s*NAME\\s*=\\s*'?([^']*)'?,?\\s*$", "\\1", lines[lstart+iNAME-1])
        oceDebug(debug, "NAME \"", NAME, "\" in PARAMETER_HEADER starting at line ", lstart-1, "\n", sep="")

        NAME2CODE[NAME] <- CODE

        if (grepl("QQQQ", CODE)) {
            iNAME <- grep("^\\s*NAME\\s*=\\s*'", lines[lstart:lend])
            if (length(iNAME) == 1) {
                ## Sample input lines (two leading spaces):
                ##   NAME= 'Quality Flag for Parameter: TEMP_01',
                ##   NAME= 'Quality flag: Sea Temperature (IPTS-68)',
                ## NAME <- paste(gsub("^.*:\\s*'?(.*)([_0-9]*)'?.*$", "\\1", lines[lstart+iNAME-1]), "Flag", sep="")
                NAME <- paste(gsub(".*:[ ]*([A-Z0-9_]*).*", "\\1", lines[lstart+iNAME-1]), "Flag", sep="")
                NAME <- paste(gsub(".*:[ ]*([^']*).*$", "\\1", lines[lstart+iNAME-1]), "Flag", sep="")
                #?IML? NAME <- gsub("^.*:[ ]*(.*)',[ ]*$","\\1", lines[lstart+iNAME-1])
                oceDebug(debug, "   \"", lines[lstart+iNAME-1], "\" -> flag name \"", NAME, "\"\n", sep="")
                flagTranslationTable[CODE] <- NAME
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
    ##> oceDebug(debug, "nullValue=", nullValue, "; it's class is ", class(nullValue), "\n")

    ##OLD ODFunits <- lines[grep("^\\s*UNITS\\s*=", lines)]
    ##OLD ODFunits <- gsub("^[^']*'(.*)'.*$", "\\1", ODFunits) # e.g.  "  UNITS= 'none',"
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
    if (debug > 0) {
        oceDebug(debug, "next is flagTranslationTable:\n")
        print(flagTranslationTable)
        oceDebug(debug, "next is NAME2CODE:\n")
        print(NAME2CODE)
    }

    namesUnits <- ODFNames2oceNames(ODFnames, ODFunits, PARAMETER_HEADER=NULL, columns=columns, debug=debug-1)
    ## check for missing units, and warn if pressure and/or temperature lack units
    w <- which(namesUnits[[1]]=="pressure")
    if (length(w)) {
        if (!length(namesUnits[[2]]["pressure"][[1]]$unit))
            warning("source file does not indicate a unit for pressure (and perhaps for other items)\n")
    }

    ##names <- ODFName2oceName(ODFnames, PARAMETER_HEADER=NULL, columns=columns, debug=debug-1)
    oceDebug(debug, "oce names: c(\"", paste(namesUnits$names, collapse="\",\""), ")\n", sep="")

    res@metadata$depthOffBottom <- findInHeader("DEPTH_OFF_BOTTOM", lines, returnOnlyFirst=TRUE, numeric=TRUE)
    res@metadata$initialLatitude <- findInHeader("INITIAL_LATITUDE", lines, returnOnlyFirst=TRUE, numeric=TRUE)
    res@metadata$initialLongitude <- findInHeader("INITIAL_LONGITUDE", lines, returnOnlyFirst=TRUE, numeric=TRUE)
    res@metadata$endLatitude <- findInHeader("END_LATITUDE", lines, returnOnlyFirst=TRUE, numeric=TRUE)
    res@metadata$endLongitude <- findInHeader("END_LONGITUDE", lines, returnOnlyFirst=TRUE, numeric=TRUE)
    res@metadata$samplingInterval <- findInHeader("SAMPLING_INTERVAL", lines, returnOnlyFirst=TRUE, numeric=TRUE)

    res@metadata$scientist <- findInHeader("CHIEF_SCIENTIST", lines)
    res@metadata$ship <- findInHeader("PLATFORM", lines) # maybe should rename, e.g. for helicopter
    res@metadata$institute <- findInHeader("ORGANIZATION", lines) # maybe should rename, e.g. for helicopter
    res@metadata$station <- findInHeader("EVENT_NUMBER", lines)
    res@metadata$latitude <- as.numeric(res@metadata$initialLatitude)
    res@metadata$longitude <- as.numeric(res@metadata$initialLongitude)
    res@metadata$cruise <- findInHeader("CRUISE_NAME", lines)
    res@metadata$countryInstituteCode <- findInHeader("COUNTRY_INSTITUTE_CODE", lines)
    res@metadata$cruiseNumber <- findInHeader("CRUISE_NUMBER", lines)
    DATA_TYPE <- trimws(findInHeader("DATA_TYPE", lines))
    res@metadata$deploymentType <- if ("CTD" == DATA_TYPE) "profile" else if ("MCTD" == DATA_TYPE) "moored" else "unknown"
    ## date <- strptime(findInHeader("START_DATE", lines), "%b %d/%y")

    ## if any changes here, update ctd.R @ ODF_CTD_LINK {
    res@metadata$startTime <- as.POSIXct(strptime(tolower(findInHeader("START_DATE_TIME", lines)), "%d-%b-%Y %H:%M:%S", tz="UTC"))
    res@metadata$date <- res@metadata$time
    res@metadata$eventNumber <- findInHeader("EVENT_NUMBER", lines) # synchronize with ctd.R at ODFMETADATA tag
    res@metadata$eventQualifier <- findInHeader("EVENT_QUALIFIER", lines)# synchronize with ctd.R at ODFMETADATA tag
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
    oceDebug(debug, "NAvalue (step 1): ", paste(deparse(NAvalue),collapse=""), "\n", sep="")
    ##> message("NAvalue=", paste(NAvalue, collapse=" "))
    NAvalue <- gsub("D([+-])+", "e\\1", NAvalue)
    oceDebug(debug, "NAvalue (step 2): ", paste(deparse(NAvalue),collapse=""), "\n", sep="")
    ##> message("NAvalue=", paste(NAvalue, collapse=" "))
    #? NAvalue <- NAvalue[!grepl("[a-df-zA-DFZ]+", NAvalue)] # remove e.g. times
    NAvalue[NAvalue == "NA"] <- NA
    oceDebug(debug, "NAvalue (step 3): ", paste(deparse(NAvalue),collapse=""), "\n", sep="")
    ##> message("NAvalue=", paste(NAvalue, collapse=" "))
    if (length(NAvalue) > 1) {
        NAvalue <- gsub("D", "e", NAvalue) # R does not like e.g. "-.99D+02"
        options <- options('warn')
        options(warn=-1)
        NAvalue <- try({as.numeric(unlist(NAvalue))}, silent=TRUE)
        NAvalueList <- NAvalue
        names(NAvalueList) <- namesUnits$names
        options(warn=options$warn)
        ##> isNumeric <- is.numeric(NAvalue)
        ##> if (any(!isNumeric)) {
        ##>     warning("ignoring non-numeric NULL_VALUE (", NAvalue, ")")
        ##> }
        ##> if (any(isNumeric)) {
        ##>     tmp <- NAvalue[isNumeric]
        ##>     if (any(!is.finite(tmp)))
        ##>         tmp <- tmp[is.finite(tmp)]
        ##>     tmp <- unique(tmp)
        ##>     ltmp <- length(tmp)
        ##>     if (ltmp == 0) {
        ##>         NAvalue <- NA
        ##>     } else if (1 == ltmp) {
        ##>         NAvalue <- tmp
        ##>     } else if (1 < ltmp) {
        ##>         warning("using first of ", ltmp, " unique NULL_VALUEs")
        ##>         tmp <- tmp[is.finite(tmp)]
        ##>         NAvalue <- tmp[[1]]
        ##>     }
        ##> } else {
        ##>     NAvalue <- NAvalue[[1]]
        ##> }
    }
    oceDebug(debug, "NAvalue (step 4): ", paste(deparse(NAvalue),collapse=""), "\n", sep="")
    ##oceDebug(debug, "NAvalue=", NAvalue, "; it's class is ", class(NAvalue), "\n")

    res@metadata$depthMin <- as.numeric(findInHeader("MIN_DEPTH", lines))
    res@metadata$depthMax <- as.numeric(findInHeader("MAX_DEPTH", lines))
    res@metadata$sounding <- as.numeric(findInHeader("SOUNDING", lines))
    ## Compute waterDepth from "SOUNDING" by preference, or from "MAX_DEPTH" if necessary
    res@metadata$waterDepth <- NA
    if (length(res@metadata$sounding)) {
        res@metadata$waterDepth <- res@metadata$sounding[1]
    } else {
        if (length(res@metadata$depthMax))
            res@metadata$waterDepth <- res@metadata$depthMax[1]
    }
    res@metadata$type <- findInHeader("INST_TYPE", lines)
    ##if (length(grep("sea", res@metadata$type, ignore.case=TRUE)))
    ##    res@metadata$type <- "SBE"
    res@metadata$serialNumber <- findInHeader("SERIAL_NUMBER", lines)
    res@metadata$model <- findInHeader("MODEL", lines)
    if (is.null(header)) {
        res@metadata$header <- NULL
    } else if (header == "character") {
        res@metadata$header <- lines[seq(1L, dataStart-1L)]
    } else if (header == "list") {
        res@metadata$header <- headerlist # ODFListFromHeader(lines[seq(1L, dataStart-1L)])
    } else {
        stop("problem decoding header argument; please report an error")
    }

    ## catch erroneous units on CRAT, which should be in a ratio, and hence have no units.
    ## This is necessary for the sample file inst/extdata/CTD_BCD2014666_008_1_DN.ODF.gz
    if (length(grep("CRAT", ODFnames))) {
        which <- grep("CRAT", ODFnames)
        for (w in which) {
            ustring <- as.character(namesUnits$units[[w]]$unit)
            if (length(ustring) && ustring != "" && ustring != "ratio")
                warning("\"", ODFnames[w], "\" should be unitless, but the file states the unit as \"", ustring, "\" so that is retained in the object metadata. This will likely cause problems.  See ?read.odf for an example of rectifying this unit error.")
        }
    }

    res@metadata$units <- namesUnits$units
    ## res@metadata$dataNamesOriginal <- ODFnames
    ##> res@metadata$dataNamesOriginal <- as.list(ODFnames)
    res@metadata$dataNamesOriginal <- as.list(ODForiginalNames)
    names(res@metadata$dataNamesOriginal) <- namesUnits$names
    ##res@metadata$type <- type
    ##res@metadata$model <- model
    ##res@metadata$serialNumber <- serialNumber
    ##res@metadata$eventNumber <- eventNumber
    ##res@metadata$eventQualifier <- eventQualifier
    ##res@metadata$ship <- ship
    ##res@metadata$scientist <- scientist
    ##res@metadata$institute <- institute
    res@metadata$address <- NULL
    ##res@metadata$cruise <- cruise
    ##res@metadata$station <- station
    ##res@metadata$countryInstituteCode <- countryInstituteCode
    ##res@metadata$cruiseNumber <- cruiseNumber
    ##res@metadata$deploymentType <- deploymentType
    ##res@metadata$date <- startTime
    ##res@metadata$startTime <- startTime
    ##res@metadata$latitude <- latitude
    ##res@metadata$longitude <- longitude
    res@metadata$recovery <- NULL
    ##res@metadata$waterDepth <- waterDepth
    ##res@metadata$depthMin <- depthMin
    ##res@metadata$depthMax <- depthMax
    ##res@metadata$sounding <- sounding
    res@metadata$sampleInterval <- NA
    res@metadata$filename <- filename
    ##> ## fix issue 768
    ##> lines <- lines[grep('%[0-9.]*f', lines,invert=TRUE)]
    ## issue1226 data <- read.table(file, skip=dataStart, stringsAsFactors=FALSE)
    data <- scan(text=lines, what="character", skip=dataStart, quiet=TRUE)
    data <- matrix(data, ncol=length(namesUnits$names), byrow=TRUE)
    data <- as.data.frame(data, stringsAsFactors=FALSE)
    ## some files have text string (e.g. dates, species lengths given as strings)
    colIsChar <- as.logical(lapply(seq_len(dim(data)[2]),
                                   function(j) any(grep("[ a-zA-Z\\(\\)]", data[,j]))))
    for (j in 1:dim(data)[2]) {
        if (!colIsChar[j]) {
            oceDebug(debug, "setting data[[,", j, "]] to numeric mode\n", sep="")
            data[[j]] <- as.numeric(data[[j]])
        } else {
            oceDebug(debug, "setting data[[,", j, "]] to character mode\n", sep="")
            data[[j]] <- as.character(data[[j]])
        }
    }
    if (length(data) != length(namesUnits$names))
        stop("mismatch between length of data names (", length(namesUnits$names), ") and number of columns in data matrix (", length(data), ")")
    names(data) <- namesUnits$names
    if (length(NAvalueList)) {
        for (name in names(data)) {
            if (is.finite(NAvalueList[[name]])) {
                bad <- data[[name]] == NAvalueList[[name]]
                data[[name]][bad] <- NA
                if (sum(bad) > 0)
                    oceDebug(debug, "set ", sum(bad), " values in '", name, "' to NA, because they matched the NULL_VALUE (", NAvalueList[[name]], ")\n", sep="")
            }
        }
    }
    ##. if (length(NAvalue) > 0 && !is.na(NAvalue)) {
    ##.     data[data==NAvalue[1]] <- NA
    ##. }
    if ("time" %in% namesUnits$names)
        data$time <- as.POSIXct(strptime(as.character(data$time), format="%d-%b-%Y %H:%M:%S", tz="UTC"))
    ##res@metadata$names <- namesUnits$names
    ##res@metadata$labels <- namesUnits$names
    res@data <- as.list(data)
    # Removed for issue 1810
    #(REMOVED) ## Return to water depth issue. In a BIO file, I found that the missing-value code was
    #(REMOVED) ## -99, but that a SOUNDING was given as -99.9, so this is an extra check.
    #(REMOVED) if (is.na(res@metadata$waterDepth) || res@metadata$waterDepth < 0) {
    #(REMOVED)     if ('pressure' %in% names(res@data)) {
    #(REMOVED)         res@metadata$waterDepth <- max(abs(res@data$pressure), na.rm=TRUE)
    #(REMOVED)         warning("estimating waterDepth from maximum pressure")
    #(REMOVED)     }
    #(REMOVED) }
    ## Move flags into metadata.
    dnames <- names(res@data)
    iflags <- grep("Flag$", dnames)
    oceDebug(debug, "About to move flags from @data to @metadata\n")
    oceDebug(debug, "iflags=", paste(iflags, collapse=" "), "\n")
    oceDebug(debug, "names(@data) = c(\"", paste(names(res@data), collapse="\", \""), "\")\n", sep="")
    oceDebug(debug, "names(@data)[iflags] = c(\"", paste(names(res@data)[iflags], collapse="\", \""), "\")\n", sep="")
    flagNamesForMetadata <- NULL
    if (length(iflags)) {
        if (debug > 0) {
            oceDebug(debug, "Following is str(res@data) before transferring 'Flag' columns to metadata\n")
            cat(str(res@data, 1))
        }
        # Step 1: determine names to be used in metadata
        for (iflag in iflags) {
            fname <- gsub("Flag$", "", dnames[iflag])
            oceDebug(debug, "\"", dnames[iflag], "\" -> \"", fname, "\"\n", sep="")
            # Handle low-level and high-level names differently.  At least with
            # test files available to me in April 2021, it seems that the BIO
            # dialectd of ODF uses low-level names, so we have e.g. PSALFlag,
            # but the IML variant uses high-level names, so we have e.g.
            # "Practical SalinityFlag". We use ODFNames2oceNames to determine
            # which case it is. Admittedly, this is a guessing game, though,
            # and I hear that there is a west-coast dialect as well. Sigh.
            fnameBase <- ODFNames2oceNames(NAME2CODE[[fname]])$names # "" if not <known>Flag
            oceDebug(debug, "fname=\"", fname, "\"; fnameBase=\"", fnameBase, "\"\n", sep="")
            flagNamesForMetadata <- c(flagNamesForMetadata,
                                      if (length(fnameBase)) fnameBase else fname)
        }
        # Step 2: unduplicate names (e.g. if "salinity" is repeated, second becomes "salinity2")
        oceDebug(debug, "preliminary  flagNamesForMetadata=c(\"", paste(flagNamesForMetadata, collapse="\", \""), "\")\n", sep="")
        flagNamesForMetadata <- unduplicateNames(flagNamesForMetadata)
        oceDebug(debug, "unduplicated flagNamesForMetadata=c(\"", paste(flagNamesForMetadata, collapse="\", \""), "\")\n", sep="")
        # Step 3: copy "Flag" columns from data to metadata
        for (I in seq_along(iflags)) {
            oceDebug(debug, "move @data[\"", dnames[iflags[I]], "\"] to @metadata$flags[\"", flagNamesForMetadata[I], "\"]\n", sep="")
            res@metadata$flags[[flagNamesForMetadata[I]]] <- res@data[[iflags[I]]]
        }
        if (debug > 1) {
            oceDebug(debug, "before Step 4: res@metadata$dataNamesOriginal:\n")
            print(res@metadata$dataNamesOriginal)
        }
        # Step 4: remove Flag entries from @metadata$dataNamesOriginal
        remove <- grep("Flag$", names(res@metadata$dataNamesOriginal))
        if (length(remove))
            res@metadata$dataNamesOriginal[remove] <- NULL
        if (debug > 1) {
            oceDebug(debug, "after Step 4: res@metadata$dataNamesOriginal:\n")
            print(res@metadata$dataNamesOriginal)
        }
        # Step 5: remove "Flag" entries from @data
        res@data[iflags] <- NULL
        if (debug > 0) {
            oceDebug(debug, "after Step 5: str(res@data):\n")
            cat(str(res@data, 1))
        }
    }
    if (exists("DATA_TYPE") && DATA_TYPE == "CTD")
        res@metadata$pressureType <- "sea"
    res@processingLog <- processingLogAppend(res@processingLog,
                                             paste("read.odf(\"", filename, "\", ",
                                                   "columns=c(\"", paste(columns, collapse="\", \""), "\"), ",
                                                   "debug=", debug, ")", sep=""))
    oceDebug(debug, "} # read.odf()\n", sep="", style="bold", unindent=1)
    res
}
