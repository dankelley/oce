# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4:foldmethod=marker

#' Class to Store Argo Data
#'
#' This class stores data from Argo floats.
#'
#' An `argo` object may be read with [read.argo()] or
#' created with [as.argo()].  Argo data can be gridded to constant
#' pressures with [argoGrid()] or subsetted with
#' [subset,argo-method()].  Plots can be made with
#' [plot,argo-method()], while [summary,argo-method()]
#' produces statistical summaries and `show` produces overviews.
#'
#' @templateVar class argo
#'
#' @templateVar dataExample The key items stored in this slot include  equal-length vectors `time`, `longitude`, `latitude` and equal-dimension matrices `pressure`, `salinity`, and `temperature`.
#'
#' @templateVar metadataExample Examples that are of common interest include `id`, a vector of ID codes for the profiles, and `dataMode`, a vector of strings indicating whether the profile is in archived mode (`"A"`), realtime mode (`"R"`), or delayed mode (`"D"`).
#'
#' @template slot_summary
#'
#' @template slot_put
#'
#' @template slot_get
#'
#' @author Dan Kelley and Clark Richards
#'
#' @family classes provided by oce
#' @family things related to argo data
setClass("argo", contains="oce")

#' ARGO float dataset
#'
#' This holds data from ARGO 6900388 in the North Atlantic.
#'
#' Below is the official citation (note that this DOI has web links for
#' downloads):
#' Argo (2017). Argo float data and metadata from Global Data Assembly Centre
#' (Argo GDAC) - Snapshot of Argo GDAC of July, 8st 2017. SEANOE.
#' \doi{10.17882/42182#50865}
#'
#' @name argo
#' @docType data
#'
#' @examples
#' library(oce)
#' data(argo)
#' summary(argo)
#' data(coastlineWorld)
#' plot(argo, which="trajectory")
#'
#' @source The netcdf file used by [read.argo()] to create this [argo-class]
#' object was downloaded using FTP to
#' \code{ftp.ifremer.fr/ifremer/argo/dac/bodc/6900388/6900388_prof.nc}
#' on 2020 June 24.
#'
#' @family datasets provided with oce
#' @family things related to argo data
NULL


#' Extract Something From an Argo Object
#'
#' @param x an [argo-class] object.
#'
#' @templateVar class argo
#'
#' @section Details of the Specialized Method:
#'
#' Note that [argo-class] data may contain both unadjusted data and adjusted
#' data.  By default, this extraction function refers to the former, but a
#' preference for the latter may be set with [preferAdjusted()], the
#' documentation of which explains (fairly complex) details.
#'
#' The results from `argo[[i]]` or `argo[[i,j]]` depend on the
#' nature of `i` and (if provided) `j`. The details are as follows.
#'
#' * If `i` is `"?"`, then the return value is a list
#' containing four items, each of which is a character vector
#' holding the names of things that can be accessed with `[[`.
#' The `data` and `metadata` items hold the names of
#' entries in the object's data and metadata
#' slots, respectively. The `dataDerived`
#' and `metadataDerived` items hold the names of things
#' that can be inferred from the object's contents, e.g.
#' `"SA"` is named in `dataDerived`, indicating that
#' `argo[["SA"]]` is permitted (to compute Absolute Salinity).
#'
#' * If `i` is `"profile"` and `j` is an integer vector,
#' then an argo object is returned, as specified by `j`. For example,
#' `argo[["profile", 2:5]]` is equivalent to
#' `subset(argo, profile %in% 2:5)`.
#'
#' * If `i` is `"CT"`, then
#' Conservative Temperature is returned, as computed with
#' [`gsw::gsw_CT_from_t`]`(SA,t,p)`, where
#' first `SA` is computed as explained
#' in the next item, `t` is in-situ temperature,
#' and `p` is pressure.
#'
#' * If `i` is `"N2"`, then the square of buoyancy is returned,
#' as computed with [swN2()].
#'
#' * If `i` is `"SA"`, then
#' Absolute Salinity is returned, as computed with
#' [gsw::gsw_SA_from_SP()].
#'
#' * If `i` is `"sigmaTheta"`, then
#' potential density anomaly (referenced to zero
#' pressure) is computed, with [swSigmaTheta()], where the
#' equation of state is taken to be
#' [`getOption`]`("oceEOS",default="gsw")`.
#'
#' * If `i` is `"sigma0"`, `"sigma1"`, `"sigma2"`, `"sigma3"` or `"sigma4"`,
#' then the associated function in the \CRANpkg{gsw} package.
#' For example, `"sigma0"` uses [gsw::gsw_sigma0()], which returns
#' potential density anomaly referenced to 0 dbar,
#' according to the gsw equation of state.
#'
#' * If `i` is `"theta"`, then
#' potential temperature (referenced to zero
#' pressure) is computed, with [swTheta()], where the
#' equation of state is taken to be
#' [`getOption`]`("oceEOS",default="gsw")`.
#'
#' * If `i` is `"depth"`, then
#' a matrix of depths is returned.
#'
#' * If `i` is `"id"` or `"ID"`, then the `id` element within
#' the `metadata` slot is returned.
#'
#' * If `i` is in the `data` slot of `x`,
#' then it is returned, otherwise if it is in the `metadata` slot,
#' then that is returned, otherwise `NULL` is returned.
#'
#' @template sub_subTemplate
#'
#' @examples
#' data(argo)
#' # 1. show that dataset has 223 profiles, each with 56 levels
#' dim(argo[['temperature']])
#'
#' # 2. show importance of focussing on data flagged 'good'
#' fivenum(argo[["salinity"]],na.rm=TRUE)
#' fivenum(argo[["salinity"]][argo[["salinityFlag"]]==1],na.rm=TRUE)
#'
#' @author Dan Kelley
#'
#' @family things related to argo data
setMethod(f="[[",
          signature(x="argo", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              res <- NULL
              dots <- list(...)
              debug <- if ("debug" %in% names(dots)) dots$debug else 0
              oceDebug(debug, "[[,argo-method(\"", i, "\") {\n", sep="", style="bold", unindent=1)
              metadataDerived <- c("ID", "cycle", "*Flag", "*Unit")
              dataDerived <- c("profile", "CT", "N2", "SA", "sigmaTheta",
                  "theta",
                  "z", "depth",
                  paste("Absolute", "Salinity"),
                  paste("Conservative", "Temperature"),
                  paste("sigma", 0:4, sep=""),
                  "spice",
                  paste("spiciness", 0:2, sep=""))
              if (i == "?")
                  return(list(metadata=sort(names(x@metadata)),
                          metadataDerived=sort(metadataDerived),
                          data=sort(names(x@data)),
                          dataDerived=sort(dataDerived)))
              if (i == "profile") {
                  ## This assignment to profile is merely to prevent a NOTE from
                  ## the syntax checker. It is needed because of issues with non-standard
                  ## evaluation in subset() calls. This is a problem that many
                  ## package authors have encountered; see e.g.
                  ## https://stackoverflow.com/questions/23475309/in-r-is-it-possible-to-suppress-note-no-visible-binding-for-global-variable?noredirect=1&lq=1
                  ## https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
                  profile <- NULL # does *not* affect the subset() call that follows
                  if (missing(j))
                      stop("must provide an integer vector to access, e.g. argo[[\"profile\", 1:3]]")
                  return(subset(x, profile %in% j))
              }
              namesData <- names(x@data)
              ## handle some computed items
              if (i %in% c("CT", paste("conservative", "temperature"), "N2",
                      "SA", paste("Absolute", "Salinity"),
                      "sigmaTheta",
                      "theta",
                      #paste("sigma", 0:4, sep=""),
                      "spice")
                      #paste("spiciness", 0:2, sep="")
                      ) {
                  salinity <- x[["salinity", debug=debug-1]]
                  pressure <- x[["pressure", debug=debug-1]]
                  temperature <- x[["temperature", debug=debug-1]]
                  dim <- dim(salinity)
                  ## Do not need longitude and latitude if eos="unesco", but retain for code clarity
                  longitude <- rep(x@data$longitude, each=dim[1])
                  latitude <- rep(x@data$latitude, each=dim[1])
                  if (i %in% c("CT", "Conservative Temperature")) {
                      res <- gsw_CT_from_t(x[["SA"]], temperature, pressure)
                  } else if (i == "N2") {
                      ##nprofile <- dim[2]
                      res <- array(NA_real_,  dim=dim)
                      for (i in seq_len(dim[2])) {
                          ##message("i=",i, ", nprofile=", nprofile)
                          ##if (i == 14) browser()
                          if (sum(!is.na(pressure[,i])) > 2) {
                              ctd <- as.ctd(salinity=salinity[,i],
                                  temperature=temperature[,i],
                                  pressure=pressure[,i],
                                  longitude=x@data$longitude[i],
                                  latitude=x@data$latitude[i])
                              res[,i] <- swN2(ctd)
                          } else {
                              res[,i] <- rep(NA, length(salinity[,i]))
                          }
                      }
                  } else if (i %in% c("SA", "Absolute Salinity")) {
                      res <- gsw_SA_from_SP(salinity, pressure, longitude=longitude, latitude=latitude)
                  } else if (i %in% paste("sigma", 0:4, sep="")) {
                      SA <- gsw_SA_from_SP(salinity, pressure, longitude=longitude, latitude=latitude)
                      CT <- gsw_CT_from_t(SA, temperature, pressure)
                      res <- switch(i, "sigma0"=gsw_sigma0(SA, CT),
                          "sigma1"=gsw_sigma1(SA, CT),
                          "sigma2"=gsw_sigma2(SA, CT),
                          "sigma3"=gsw_sigma3(SA, CT),
                          "sigma4"=gsw_sigma4(SA, CT))
                  } else if (i %in% "spice") {
                      if (missing(j)) {
                          res <- swSpice(x)
                      } else {
                          if (!j %in% c("gsw", "unesco"))
                              stop("\"", j, "\" not allowed; use either \"gsw\" or \"unesco\"")
                          res <- swSpice(x, eos=j)
                      }
                  } else if (i == "sigmaTheta") {
                      res <- swSigmaTheta(salinity, temperature=temperature, pressure=pressure,
                          referencePressure=0, longitude=longitude, latitude=latitude,
                          eos=getOption("oceEOS", default="gsw"))
                  } else if (i == "theta") {
                      res <- swTheta(salinity, temperature=temperature, pressure=pressure,
                          referencePressure=0, longitude=longitude, latitude=latitude,
                          eos=getOption("oceEOS", default="gsw"))
                  } else {
                      stop("argo[[ coding error: unknown item '", i, "'")
                  }
                  dim(res) <- dim
              } else if (i == "z") {
                  # See note at "depth", below.
                  if (is.matrix(x@data$pressure)) {
                      n <- dim(x@data$pressure)[1]
                      latitude <- matrix(rep(x@data$latitude, each=n),
                                         nrow=n, byrow=TRUE)
                      res <- -swDepth(x@data$pressure, latitude)
                  } else {
                      res <- -swDepth(x@data$pressure, x@data$latitude)
                  }
              } else if (i == "depth") {
                  ## This accessor added for issue 1333. Note that the
                  ## fix for that issue was sometimes calling with
                  ## vector-form argo object. I don't know how that vector
                  ## form is arising, but it is likely an index without
                  ## a drop=FALSE condition ... if I find it, I'll fix it,
                  ## but the following works fine, so I don't really care too
                  ## much.
                  if (is.matrix(x@data$pressure)) {
                      n <- dim(x@data$pressure)[1]
                      latitude <- matrix(rep(x@data$latitude, each=n),
                          nrow=n, byrow=TRUE)
                      res <- swDepth(x@data$pressure, latitude)
                      ##. print("matrix ... lat and then pres... and the depth...")
                      ##. print(latitude[1:3, 1:3])
                      ##. print(x@data$pressure[1:3, 1:3])
                      ##. print(res[1:3, 1:3])
                  } else {
                      res <- swDepth(x@data$pressure, x@data$latitude)
                  }
              } else if (i == "ID" || i == "id") {
                  res <- x@metadata$id
              } else if (i == "cycleNumber" || i == "cycle") {
                  res <- x@metadata$cycle
              } else if (i == "latitude") {
                  res <- x@data$latitude
              } else if (i == "longitude") {
                  res <- x@data$longitude
              } else if (i %in% c(namesData, paste0(namesData, "Flag"), paste0(namesData, "Unit"))) {
                  ## Select adjusted or unadusted variants of things stored in the data slot, or
                  ## their cousins stored in metadata$units and metadata$flags.
                  wantFlag <- grepl("Flag", i)
                  wantUnit <- grepl("Unit", i)
                  which <- x@metadata$adjustedWhich
                  fallback <- x@metadata$adjustedFallback
                  iBase <- gsub("Unit$", "", gsub("Flag$", "", i)) # base name for i
                  iBaseAdjusted <- paste0(iBase, "Adjusted")
                  oceDebug(debug, "i='", i,
                           "', iBase='",iBase,"', iBaseAdjusted='", iBaseAdjusted,
                           "', wantFlag=", wantFlag, ", wantUnit=", wantUnit, "\n", sep="")
                  unadjusted <- if (wantUnit) {
                      x@metadata$units[[iBase]]
                  } else if (wantFlag) {
                      x@metadata$flags[[iBase]]
                  } else {
                      x@data[[iBase]]
                  }
                  adjusted <- if (wantUnit) {
                      x@metadata$units[[iBaseAdjusted]]
                  } else if (wantFlag) {
                      x@metadata$flags[[iBaseAdjusted]]
                  } else {
                      x@data[[iBaseAdjusted]]
                  }
                  if (!is.null(which) && !is.null(fallback)) {
                      ## The handling of adjusted/unadjusted preference is carried out in cases;
                      ## the debugging statements explain the logic flow here in the code, and
                      ## also expose it to the user (in hopes that users may notice if there are
                      ## errors with respect to the documented behaviour).
                      if (which == "all" || i %in% which) {
                          if (is.null(adjusted)) {
                              oceDebug(debug, "Case 1: returning unadjusted item, since the adjusted item does not exist.\n")
                              res <- unadjusted
                          } else {
                              if (any(is.finite(adjusted))) {
                                  oceDebug(debug, "Case 2: returning adjusted data.\n")
                                  res <- adjusted
                              } else {
                                  if (fallback) {
                                      oceDebug(debug, "Case 3: returning unadjusted data, since all adjusted values are NA and metadata$adjustedFallback=", fallback, "\n")
                                      res <- unadjusted
                                  } else {
                                      oceDebug(debug, "Case 4: returning adjusted data, even though all are are NA, because metadata$adjustedFallback=", fallback, "\n", sep="")
                                      res <- adjusted
                                  }
                              }
                          }
                      } else {
                          oceDebug(debug, "Case 5: returning unadjusted data, because \"", i, "\" is not in metadata$adjustedWhich.\n", sep="")
                          res <- unadjusted
                      }
                  } else {
                      oceDebug(debug, "Case 6: returning unadjusted data, since metadata slot does not contain adjustedWhich and adjustedFallback\n")
                      res <- unadjusted
                  }
              } else {
                  #message("[[,argo-method calling next method")
                  res <- callNextMethod()         # [[ defined in R/AllClass.R
              }
              oceDebug(debug, "} # [[,argo-method\n", sep="", style="bold", unindent=1)
              res
          })

#' Replace Parts of an Argo Object
#'
#' @param x an [argo-class] object.
#'
#' @template sub_subsetTemplate
#'
#' @family things related to argo data
setMethod(f="[[<-",
          signature(x="argo", i="ANY", j="ANY"),
          definition=function(x, i, j, ..., value) {
              callNextMethod(x=x, i=i, j=j, ...=..., value=value) # [[<-
          })

setMethod(f="initialize",
          signature="argo",
          definition=function(.Object, time, id, longitude, latitude, salinity, temperature, pressure, filename, dataMode, ...) {
              .Object <- callNextMethod(.Object, ...)
              if (!missing(time)) .Object@data$time <- time
              if (!missing(id)) .Object@metadata$id <- id
              if (!missing(longitude)) .Object@data$longitude <- longitude
              if (!missing(latitude)) .Object@data$latitude <- latitude
              if (!missing(salinity)) .Object@data$salinity <- salinity
              if (!missing(temperature)) .Object@data$temperature <-temperature
              if (!missing(pressure)) .Object@data$pressure <- pressure
              .Object@metadata$filename <- if (missing(filename)) "" else filename
              .Object@metadata$dataMode <- if (missing(dataMode)) "" else dataMode
              .Object@processingLog$time <- presentTime()
              .Object@processingLog$value <- "create 'argo' object"
              .Object <- initializeFlagScheme(.Object, "argo")
              return(.Object)
          })

## a local function -- no need to pollute namespace with it
maybeLC <- function(s, lower)
    if (lower) tolower(s) else s

## a local function -- no need to pollute namespace with it
getData <- function(file, name, quiet=FALSE)
{
    res <- NA
    ## wrap in capture.output to silence what seems like direct printing to stdout()
    ## or stderr() by ncvar_get().
    capture.output(
                   {
                       res <- try(ncdf4::ncvar_get(file, name), silent=TRUE)
                   })
    if (inherits(res, "try-error")) {
        if (!quiet)
            warning(file$filename, " has no variable named '", name, "'\n", sep='')
        res <- NULL
    }
    if (is.array(res) && 1 == length(dim(res))) res <- matrix(res) else res
}

#' Convert Argo Data Name to Oce Name
#'
#' This function is used internally by [read.argo()] to convert Argo-convention
#' data names to oce-convention names. Users should not call this directly, since
#' its return value may be changed at any moment (e.g. to include units as well
#' as names).
#'
#'
#' The inference of names was done
#' by inspection of some data files, based on reference 1. It should be noted,
#' however, that the data files examined contain some names that are not
#' documented in reference 1, and others that are listed only in its changelog,
#' with no actual definitions being given. For example, the files had six distinct
#' variable names that seem to relate to phase in the oxygen sensor, but
#' these are not translated by the present function because these
#' variable names are not defined in reference 1, or not defined uniquely
#' in reference 2.
#'
#' The names are converted with
#' [gsub()], using the `ignore.case` argument of the present
#' function.
#' The procedure
#' is to first handle the items listed in the following table, with string
#' searches anchored to the start of the string. After that,
#' the qualifiers
#' `_ADJUSTED`, `_ERROR` and `_QC`,
#' are translated to `Adjusted`, `Error`, and `QC`, respectively.
#'
#' \tabular{ll}{
#' **Argo name** \tab **oce name**\cr
#' `BBP` \tab `bbp`\cr
#' `BETA_BACKSCATTERING` \tab `betaBackscattering`\cr
#' `BPHASE_OXY` \tab `bphaseOxygen`\cr
#' `CDOM` \tab `CDOM`\cr
#' `CNDC` \tab `conductivity`\cr
#' `CHLA` \tab `chlorophyllA`\cr
#' `CP` \tab `beamAttenuation`\cr
#' `CYCLE_NUMBER` \tab `cycleNumber` (both this and `cycle` are handled by the [[ operator)\cr
#' `DATA_CENTRE` \tab `dataCentre`\cr
#' `DATA_MODE` \tab `dataMode`\cr
#' `DATA_STATE_INDICATOR` \tab `dataStateIndicator`\cr
#' `DC_REFERENCE` \tab `DCReference`\cr
#' `DIRECTION` \tab `direction`\cr
#' `DOWN_IRRADIANCE` \tab `downwellingIrradiance`\cr
#' `DOWNWELLING_PAR` \tab `downwellingPAR`\cr
#' `FIRMWARE_VERSION` \tab `firmwareVersion`\cr
#' `FIT_ERROR_NITRATE` \tab `fitErrorNitrate`\cr
#' `FLUORESCENCE_CDOM` \tab `fluorescenceCDOM`\cr
#' `FLUORESCENCE_CHLA` \tab `fluorescenceChlorophyllA`\cr
#' `INST_REFERENCE` \tab `instReference`\cr
#' `JULD` \tab `juld` (and used to compute `time`)\cr
#' `JULD_QC_LOCATION` \tab `juldQCLocation`\cr
#' `LATITUDE` \tab `latitude`\cr
#' `LONGITUDE` \tab `longitude`\cr
#' `MOLAR_DOXY` \tab `oxygenUncompensated`\cr
#' `PH_IN_SITU_FREE` \tab `pHFree`\cr
#' `PH_IN_SITU_TOTAL` \tab `pH`\cr
#' `PI_NAME` \tab `PIName`\cr
#' `PLATFORM_NUMBER` \tab `id`\cr
#' `POSITION_ACCURACY` \tab `positionAccuracy`\cr
#' `POSITIONING_SYSTEM` \tab `positioningSystem`\cr
#' `PROFILE` \tab `profile`\cr
#' `PROJECT_NAME` \tab `projectName`\cr
#' `RAW_DOWNWELLING_IRRADIANCE` \tab `rawDownwellingIrradiance`\cr
#' `RAW_DOWNWELLING_PAR` \tab `rawDownwellingPAR`\cr
#' `RAW_UPWELLING_RADIANCE` \tab `rawUpwellingRadiance`\cr
#' `STATION_PARAMETERS` \tab `stationParameters`\cr
#' `TEMP` \tab `temperature`\cr
#' `TEMP_CPU_CHLA` \tab `temperatureCPUChlorophyllA`\cr
#' `TEMP_DOXY` \tab `temperatureOxygen`\cr
#' `TEMP_NITRATE` \tab `temperatureNitrate`\cr
#' `TEMP_PH` \tab `temperaturePH`\cr
#' `TEMP_SPECTROPHOTOMETER_NITRATE` \tab `temperatureSpectrophotometerNitrate`\cr
#' `TILT` \tab `tilt`\cr
#' `TURBIDITY` \tab `turbidity`\cr
#' `UP_RADIANCE` \tab `upwellingRadiance`\cr
#' `UV_INTENSITY` \tab `UVIntensity`\cr
#' `UV_INTENSITY_DARK_NITRATE` \tab `UVIntensityDarkNitrate`\cr
#' `UV_INTENSITY_NITRATE` \tab `UVIntensityNitrate`\cr
#' `VRS_PH` \tab `VRSpH`\cr
#' `WMO_INST_TYPE` \tab `WMOInstType`\cr
#'}
#'
#' @param names vector of character strings containing names in the Argo convention.
#'
#' @param ignore.case a logical value passed to [gsub()], indicating whether to
#' ignore the case of input strings. The default is set to `TRUE` because some data
#' files use lower-case names, despite the fact that the Argo documentation specifies
#' upper-case.
#'
#' @return A character vector of the same length as `names`, but with
#' replacements having been made for all known quantities.
#'
#' @references
#' 1. Argo User's Manual Version 3.3, Nov 89th, 2019, available at
#' `https://archimer.ifremer.fr/doc/00187/29825/` online.
#'
#' 2. Argo list of parameters in an excel spreadsheet, available at
#' `http://www.argodatamgt.org/content/download/27444/187206/file/argo-parameters-list-core-and-b.xlsx`
#'
#' @family things related to argo data
argoNames2oceNames <- function(names, ignore.case=TRUE)
{
    ## do NOT change the order below, because we are working with partial strings.
    names <- gsub("^BBP([0-9_]*)", "BBP\\1", names, ignore.case=ignore.case)
    names <- gsub("^BETA_BACKSCATTERING([0-9_]*)", "betaBackscattering\\1", names, ignore.case=ignore.case)
    names <- gsub("^BPHASE_DOXY", "bphaseOxygen", names, ignore.case=ignore.case)
    names <- gsub("^CHLA", "chlorophyllA", names, ignore.case=ignore.case)
    names <- gsub("^CDOM", "CDOM", names, ignore.case=ignore.case)
    names <- gsub("^CNDC([0-9_]*)", "conductivity\\1", names, ignore.case=ignore.case)
    names <- gsub("^CP([0-9_]*)", "beamAttenuation\\1", names, ignore.case=ignore.case)
    names <- gsub("^CYCLE_NUMBER", "cycleNumber", names, ignore.case=ignore.case)
    names <- gsub("^DOWN_IRRADIANCE", "downwellingIrradiance", names, ignore.case=ignore.case)
    names <- gsub("^DOWNWELLING_PAR", "downwellingPAR", names, ignore.case=ignore.case)
    names <- gsub("^FIT_ERROR_NITRATE", "fitErrorNitrate", names, ignore.case=ignore.case) # put before CHLA
    names <- gsub("^FLUORESCENCE_CDOM", "fluorescenceCDOM", names, ignore.case=ignore.case) # put before CHLA
    names <- gsub("^FLUORESCENCE_CHLA", "fluorescenceChlorophyllA", names, ignore.case=ignore.case) # put before CHLA
    names <- gsub("^MOLAR_DOXY", "oxygenUncompensated", names, ignore.case=ignore.case)
    names <- gsub("^PH_IN_SITU_FREE", "pHFree", names, ignore.case=ignore.case)
    names <- gsub("^PH_IN_SITU_TOTAL", "pH", names, ignore.case=ignore.case)
    names <- gsub("^TEMP_DOXY", "temperatureOxygen", names, ignore.case=ignore.case)
    names <- gsub("^TEMP_NITRATE", "temperatureNitrate", names, ignore.case=ignore.case)
    names <- gsub("^TEMP_PH", "temperaturePH", names, ignore.case=ignore.case)
    names <- gsub("^TEMP_SPECTROPHOTOMETER_NITRATE", "temperatureSpectrophotometerNitrate", names, ignore.case=ignore.case)
    names <- gsub("^TEMP_CPU_CHLA", "temperatureCPUChlA", names, ignore.case=ignore.case)
    names <- gsub("^TEMP_VOLTAGE_DOXY", "temperatureVoltageOxygen", names, ignore.case=ignore.case)
    names <- gsub("^TEMP_", "temperature_", names, ignore.case=ignore.case)
    names <- gsub("^POSITION_ACCURACY", "positionAccuracy", names, ignore.case=ignore.case)
    names <- gsub("^NITRATE", "nitrate", names, ignore.case=ignore.case)
    names <- gsub("^DOXY", "oxygen", names, ignore.case=ignore.case)
    names <- gsub("^PRES", "pressure", names, ignore.case=ignore.case)
    names <- gsub("^PSAL", "salinity", names, ignore.case=ignore.case)
    names <- gsub("^RAW_DOWNWELLING_IRRADIANCE", "rawDownwellingIrradiance", names, ignore.case=ignore.case)
    names <- gsub("^RAW_DOWNWELLING_PAR", "rawDownwellingPAR", names, ignore.case=ignore.case)
    names <- gsub("^RAW_UPWELLING_RADIANCE", "rawUpwellingRadiance", names, ignore.case=ignore.case)
    names <- gsub("^TEMP([0-9_]*)$", "temperature\\1", names, ignore.case=ignore.case)
    names <- gsub("^TILT([0-9_]*)$", "tilt\\1", names, ignore.case=ignore.case)
    names <- gsub("^TRANSMITTANCE_PARTICLE_BEAM_ATTENUATION([0-9_]*)$",
                  "transmittanceParticleBeamAttenuation\\1", names, ignore.case=ignore.case)
    names <- gsub("^TURBIDITY([0-9_]*)$", "turbidity\\1", names, ignore.case=ignore.case)
    names <- gsub("^UP_RADIANCE", "upwellingRadiance", names, ignore.case=ignore.case)
    names <- gsub("^UV_INTENSITY_DARK_NITRATE", "UVIntensityDarkNitrate", names, ignore.case=ignore.case)
    names <- gsub("^UV_INTENSITY_NITRATE", "UVIntensityNitrate", names, ignore.case=ignore.case)
    names <- gsub("^UV_INTENSITY", "UVIntensity", names, ignore.case=ignore.case)
    names <- gsub("^VRS_PH", "VRSpH", names, ignore.case=ignore.case)
    names <- gsub("_ADJUSTED", "Adjusted", names, ignore.case=ignore.case)
    names <- gsub("_QC", "QC", names, ignore.case=ignore.case)
    names <- gsub("_ERROR", "Error", names, ignore.case=ignore.case)
    names
}

#' Subset an Argo Object
#'
#' Subset an argo object, either by selecting just the "adjusted" data
#' or by subsetting by pressure or other variables.
#'
#' @details
#' If `subset` is the string `"adjusted"`, then `subset`
#' replaces the station variables with their adjusted counterparts. In
#' the argo notation, e.g. `PSAL` is replaced with `PSAL_ADJUSTED`;
#' in the present notation, this means that `salinity` in the `data`
#' slot is replaced with `salinityAdjusted`, and the latter is deleted.
#' Similar replacements are also done with the flags stored in the `metadata`
#' slot.
#'
#' If `subset` is an expression, then the action is somewhat similar
#' to other `subset` functions, but with the restriction that
#' only one independent variable may be
#' used in in any call to the function, so that
#' repeated calls will be necessary to subset based on more than one
#' independent variable.  Subsetting may be done by anything
#' stored in the data, e.g. `time`,
#' `latitude`, `longitude`, `profile`, `dataMode`,
#' or `pressure` or by `profile` (a made-up variable),
#' `id` (from the `metadata` slot) or `ID` (a synonym for `id`).
#' Note that subsetting by `pressure`
#' preserves matrix shape, by setting discarded values to `NA`, as opposed
#' to dropping data (as is the case with `time`, for example).
#'
#' @param x an [argo-class] object.
#'
#' @param subset An expression indicating how to subset `x`.
#'
#' @param ... optional arguments, of which only the first is examined. The
#' only possibility is `within`, a polygon enclosing data to be
#' retained. This must be either a list or data frame, containing items
#' named either `x` and `y` or `longitude` and
#' `latitude`; see Example 4.  If `within` is given,
#' then `subset` is ignored.
#'
#' @return An [argo-class] object.
#'
#' @examples
#' library(oce)
#' data(argo)
#'
#' # Example 1: subset by time, longitude, and pressure
#' par(mfrow=c(2,2))
#' plot(argo)
#' plot(subset(argo, time > mean(time)))
#' plot(subset(argo, longitude > mean(longitude)))
#' plot(subset(argoGrid(argo), pressure > 500 & pressure < 1000), which=5)
#'
#' # Example 2: restrict attention to delayed-mode profiles.
#'\dontrun{
#' par(mfrow=c(1, 1))
#' plot(subset(argo, dataMode == "D"))
#'}
#'
#' # Example 3: contrast adjusted and unadjusted data
#'\dontrun{
#' par(mfrow=c(1, 2))
#' plotTS(argo)
#' plotTS(subset(argo, "adjusted"))
#'}
#'
#' # Example 4. Subset by a polygon determined with locator()
#'\dontrun{
#' par(mfrow=c(1, 2))
#' plot(argo, which="map")
#' ## Can get a boundary with e.g. locator(4)
#' boundary <- list(x=c(-65, -40, -40, -65), y=c(65, 65, 45, 45))
#' argoSubset <- subset(argo, within=boundary)
#' plot(argoSubset, which="map")
#'}
#
#' @author Dan Kelley
#'
#' @family things related to argo data
#' @family functions that subset oce objects
#' @aliases subset.argo
setMethod(f="subset",
          signature="argo",
          definition=function(x, subset, ...) {
              subsetString <- paste(deparse(substitute(expr=subset, env=environment())), collapse=" ")
              res <- x
              dots <- list(...)
              dotsNames <- names(dots)
              withinGiven <- length(dots) && ("within" %in% dotsNames)
              debug <- if (length(dots) && ("debug" %in% names(dots))) dots$debug else getOption("oceDebug")
              oceDebug(debug, "subset,argo-method() {\n", sep="", unindent=1, style="bold")
              if (withinGiven) {
                  oceDebug(debug, "subsetting with 'within' method\n")
                  ## {{{ OLD 'sp::point.in.polygon' method
                  polygon <- dots$within
                  if (!is.data.frame(polygon) && !is.list(polygon))
                      stop("'within' must be a data frame or a polygon")
                  polygonNames <- names(polygon)
                  lonp <- if ("x" %in% polygonNames) {
                      polygon$x
                  } else if ("longitude" %in% polygonNames) {
                      polygon$longitude
                  } else {
                      stop("'within' must contain either 'x' or 'longitude'")
                  }
                  latp <- if ("y" %in% polygonNames) {
                      polygon$y
                  } else if ("latitude" %in% polygonNames) {
                      polygon$latitude
                  } else {
                      stop("'within' must contain either 'y' or 'latitude'")
                  }
                  lon <- x[["longitude", "byStation"]]
                  lat <- x[["latitude", "byStation"]]
                  if (requireNamespace("sp", quietly=TRUE)) {
                      keep <- 1==sp::point.in.polygon(lon, lat, lonp, latp)
                  } else {
                      stop("cannot use 'within' because the 'sp' package is not installed")
                  }
                  ## }}}
                  ## {{{ NEW 'sf' method
                  ## Compare with 'sf' results
                  polyNew <- sf::st_polygon(list(outer=cbind(c(lonp, lonp[1]), c(latp, latp[1]))))
                  pointsNew <- sf::st_multipoint(cbind(lon, lat))
                  insideNew <- sf::st_intersection(pointsNew, polyNew)
                  keepNew <- matrix(pointsNew %in% insideNew, ncol=2)[,1]
                  if (!all.equal(keepNew, keep)) {
                      warning("subset,argo-method disagreement between old 'sp' method and new 'sf' method\n")
                  } else {
                      oceDebug(debug, "subset,argo-method: old 'sp' method and new 'sf' method gave identical results\n");
                  }
                  ## }}}
                  ## Metadata
                  for (name in names(x@metadata)) {
                      oceDebug(debug, "subsetting metadata item named '", name, "'.\n", sep="")
                      ## Pass some things through directly.
                      ## 20200831 if (name %in% c("units", "flags", "filename", "flagScheme", "dataNamesOriginal")) {
                      if (name %in% c("units", "filename", "flagScheme", "dataNamesOriginal"))
                          next
                      item <- x@metadata[[name]]
                      ## Handle things that are encoded as characters in a string, namely 'direction', 'juldQC', 'positionQC',
                      ## and some other 'QC` things that are found by grepping.
                      if (name == "direction" || grepl("QC$", name)) {
                          oceDebug(debug, "  \"", name, "\" is a special string ('direction' or '*QC'), being subsetted by character number\n", sep="")
                          res@metadata[[name]] <- paste(strsplit(item,"")[[1]][keep],collapse="")
                      } else if (is.list(item)) {
                          oceDebug(debug, "  \"", name, "\" is a list, with each element being subsetted\n", sep="")
                          for (l in seq_along(item)) {
                              res@metadata[[name]][[l]] <- item[[l]][, keep, drop=FALSE]
                          }
                      } else if (is.vector(name)) {
                          res@metadata[[name]] <- item[keep]
                      } else if (is.matrix(name)) {
                          res@metadata[[name]] <- item[, keep, drop=FALSE]
                      } else if (is.array(name)) {
                          oceDebug(debug, "name=", name, " has dim ", paste(dim(res@metadata[[name]]), collapse=" "), "\n")
                          if (length(dim(res@metadata[[name]])) <= 3) {
                              res@metadata[[name]] <- item[, , keep, drop=FALSE]
                          } else {
                              warning("not subsetting \"", name, "\" in metadata, because it is an array of rank > 3")
                          }
                      } else {
                          stop("cannot subset metadata item named '", name, "' because it is not a length-one string, a vector, or a matrix")
                      }
                  }
                  ## Data
                  for (name in names(x@data)) {
                      oceDebug(debug, "subsetting data item named '", name, "'\n", sep="")
                      item <- x@data[[name]]
                      if ("time" == name) {
                          ##.message("'", name, "' is time (not a vector)")
                          res@data$time <- item[keep]
                          ##.message(" ... ok")
                      } else if (is.vector(item)) {
                          ##.message("'", name, "' is vector")
                          res@data[[name]] <- item[keep]
                          ##.message(" ... ok")
                      } else if (is.matrix(item)) {
                          ##.message("'", name, "' is matrix")
                          res@data[[name]] <- item[, keep]
                          ##.message(" ... ok")
                      } else {
                          stop("argo object has data item '", name, "' that is neither a vector nor a matrix, so we cannot subset it")
                      }
                  }
                  res@processingLog <- processingLogAppend(res@processingLog,
                                                           paste("subset(x, within) kept ", sum(keep), " of ",
                                                                 length(keep), " stations", sep=""))
              } else {
                  if (is.character(substitute(expr=subset, env=environment()))) {
                  if (subset != "adjusted")
                      stop("if subset is a string, it must be \"adjusted\"")
                  dataNames <- names(x@data)
                  ## Seek 'Adjusted' names
                  adjustedIndices <- grep(".*Adjusted$", dataNames)
                  for (i in adjustedIndices) {
                      adjusted <- dataNames[i]
                      base <- gsub("Adjusted$", "", adjusted)
                      adjustedError <- paste(adjusted, "Error", sep="")
                      ##> message("    base:          ", base)
                      ##> message("    adjusted:      ", adjusted)
                      ##> message("    adjustedError: ", adjustedError)
                      res@data[[base]] <- res@data[[adjusted]]
                      res@data[[adjusted]] <- NULL
                      res@data[[adjustedError]] <- NULL
                  }
                  flagNames <- names(x@metadata$flags)
                  adjustedIndices <- grep("Adjusted", flagNames)
                  ##> message("FLAGS")
                  ##> message("flagNames...");print(flagNames)
                  ##> message("adjustedIndices");print(adjustedIndices)
                  for (i in adjustedIndices) {
                      adjusted <- flagNames[i]
                      base <- gsub("Adjusted", "", adjusted)
                      adjustedError <- paste(adjusted, "Error", sep="")
                      ##> message("    base:          ", base)
                      ##> message("    adjusted:      ", adjusted)
                      ##> message("    adjustedError: ", adjustedError)
                      res@metadata$flags[[base]] <- res@metadata$flags[[adjusted]]
                      res@metadata$flags[[adjusted]] <- NULL
                      res@metadata$flags[[adjustedError]] <- NULL
                  }
                  res@processingLog <- processingLogAppend(res@processingLog,
                                                           paste("subset.argo(x, subset=\"",
                                                                 as.character(subset), "\")", sep=""))
              } else {
                  subsetString <- paste(deparse(substitute(expr=subset, env=environment())), collapse=" ")
                  res <- x
                  if (length(grep("time", subsetString)) ||
                      length(grep("longitude", subsetString)) || length(grep("latitude", subsetString))) {
                      keep <- eval(expr=substitute(expr=subset, env=environment()), envir=x@data, enclos=parent.frame(2))
                  } else if (length(grep("id", subsetString, ignore.case=TRUE))) {
                      ## add id into the data, then do as usual
                      tmp <- x@data
                      tmp$id <- x@metadata$id
                      keep <- eval(expr=substitute(expr=subset, env=environment()), envir=tmp, enclos=parent.frame(2))
                      rm(tmp)
                  } else if (length(grep("profile", subsetString))) {
                      ## add profile into the data, then do as usual
                      tmp <- x@data
                      tmp$profile <- seq_along(x@data$time)
                      keep <- eval(expr=substitute(expr=subset, env=environment()), envir=tmp, enclos=parent.frame(2))
                      rm(tmp)
                  } else if (length(grep("pressure", subsetString))) {
                      ## issue1628 ## check that it is a "gridded" argo
                      ## issue1628 gridded <- ifelse(all(apply(x@data$pressure, 1, diff) == 0, na.rm=TRUE), TRUE, FALSE)
                      ## issue1628 if (gridded) {
                      ## issue1628     x@data$pressure <- x@data$pressure[, 1] ## FIXME: have to convert pressure to vector
                      ## issue1628     keep <- eval(substitute(subset), x@data, parent.frame(2))
                      ## issue1628     x@data$pressure <- res@data$pressure ## FIXME: convert back to original for subsetting below
                      ## issue1628 } else {
                      ## issue1628     stop("cannot subset ungridded argo by pressure -- use argoGrid() first", call.=FALSE)
                      ## issue1628 }
                      keep <- eval(expr=substitute(expr=subset, env=environment()), envir=x@data, enclos=parent.frame(2))
                  } else if (length(grep("dataMode", subsetString))) {
                      keep <- eval(expr=substitute(expr=subset, env=environment()), envir=x@metadata, enclos=parent.frame(2))
                  } else {
                      stop("can only subset by time, longitude, latitude, pressure, dataMode, and not by combinations", call.=FALSE)
                  }
                  if (length(grep("pressure", subsetString))) {
                      ## Now do the subset. Note that we preserve matrix dimensions, by setting
                      ## discarded values to NA.
                      fieldname <- names(x@data)
                      for (field in fieldname) {
                          if (field != 'time' & field != 'longitude' & field != 'latitude') { # DEBUG: see issue 1327
                              ifield <- which(field == fieldname)
                              ##debug message("ifield=", ifield, ", field=", field,
                              ##debug        "\n\tlength(keep)=", length(keep),
                              ##debug        "\n\tsum(keep)=", sum(keep))
                              if (is.matrix(res@data[[ifield]])) {
                                  res@data[[ifield]][!keep] <- NA
                              } else {
                                  res@data[[ifield]][!keep] <- NA
                              }
                          }
                      }
                      fieldname <- names(x@metadata$flags)
                      for (field in fieldname) {
                          ifield <- which(field == fieldname)
                          res@metadata$flags[[ifield]][!keep] <- NA
                      }
                      ## res@data$salinity <- x@data$salinity[keep, ]
                      ## res@data$temperature <- x@data$temperature[keep, ]
                      ## res@data$pressure <- x@data$pressure[keep, ]
                      res@processingLog <- processingLogAppend(res@processingLog, paste("subset.argo(x, subset=", subsetString, ")", sep=""))
                  } else {
                      res@data$time <- x@data$time[keep]
                      res@data$longitude <- x@data$longitude[keep]
                      res@data$latitude <- x@data$latitude[keep]
                      res@data$profile <- x@data$profile[keep]
                      res@metadata$dataMode <- x@metadata$dataMode[keep]
                      fieldname <- names(x@data)
                      for (field in fieldname) {
                          if (field != 'time' && field != 'longitude' && field != 'latitude' && field != 'profile') {
                              ifield <- which(field == fieldname)
                              res@data[[ifield]] <- if (is.matrix(x@data[[ifield]]))
                                  x@data[[ifield]][, keep, drop=FALSE] else x@data[[ifield]][keep]
                          }
                      }
                      fieldname <- names(x@metadata$flags)
                      for (field in fieldname) {
                          ifield <- which(field == fieldname)
                          res@metadata$flags[[ifield]] <- res@metadata$flags[[ifield]][, keep]
                      }
                                        #if (sum(keep) < 1) warning("In subset.argo() :\n  removed all profiles", call.=FALSE)
                      ## res@data$salinity <- x@data$salinity[, keep]
                      ## res@data$temperature <- x@data$temperature[, keep]
                      ## res@data$pressure <- x@data$pressure[, keep]
                  }
                  res@processingLog <- processingLogAppend(res@processingLog, paste("subset.argo(x, subset=", subsetString, ")", sep=""))
                  }
              }
              oceDebug(debug, "} # subset,argo-method\n", sep="", unindent=1, style="bold")
              res
          })


#' Summarize an Argo Object
#'
#' @description Summarizes some of the data in an `argo` object.
#'
#' @details Pertinent summary information is presented.
#' @param object}{an object of class `"argo"`, usually, a result of a
#'     call to [read.argo()].
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A matrix containing statistics of the elements of the `data` slot.
#' @examples
#' library(oce)
#' data(argo)
#' summary(argo)
#'
#' @author Dan Kelley
#' @family things related to argo data
#' @aliases summary.argo
setMethod(f="summary",
          signature="argo",
          definition=function(object, ...) {
              cat("Argo Summary\n------------\n\n")
              showMetadataItem(object, "filename",                  "Source:              ", quote=TRUE)
              nid <- length(unique(object@metadata$id))
              if (1 == nid)
                   cat("* id:                  \"", object@metadata$id[1], "\"\n", sep="")
              else cat("* id list:             \"", object@metadata$id[1], "\", \"", object@metadata$id[2], "\", ...\n", sep="")
              if ("featureType" %in% names(object@metadata))
                  cat("* feature type:        \"", object@metadata$featureType, "\"\n", sep="")
              nD <- sum(object@metadata$dataMode == "D")
              nA <- sum(object@metadata$dataMode == "A")
              nR <- sum(object@metadata$dataMode == "R")
              cat("* Profiles:            ", nD, " delayed; ", nA, " adjusted; ", nR, " realtime", "\n", sep="")
              invisible(callNextMethod()) # summary
          })

ncdfFixMatrix <- function(x)
{
    if (length(dim(x)) == 1)
        x <- as.vector(x)
    x
}

#' Grid Argo float data
#'
#' Grid an Argo float, by interpolating to fixed pressure levels.
#' The gridding is done with [approx()].  If there is
#' sufficient user demand, other methods may be added, by analogy to
#' [sectionGrid()].
#'
#' @template flagDeletionTemplate
#'
#' @param argo A `argo` object to be gridded.
#'
#' @param p Optional indication of the pressure levels to which interpolation
#' should be done.  If this is not supplied, the pressure levels will be
#' calculated based on the existing values, using medians. If `p="levitus"`,
#' then pressures will be set to be those of the Levitus atlas, given by
#' [standardDepths()], trimmed to the maximum pressure in `argo`.
#' If `p` is a single numerical value, it is taken as the number of
#' subdivisions to use in a call to [seq()] that has range from 0 to the
#' maximum pressure in `argo`.  Finally, if a vector numerical values is
#' provided, then it is used as is.
#'
#' @param debug A flag that turns on debugging.  Higher values provide deeper
#' debugging.
#'
#' @param ... Optional arguments to [approx()], which is used to do the
#' gridding.
#'
#' @return x an [argo-class] object.
#'
#' @examples
#' library(oce)
#' data(argo)
#' g <- argoGrid(argo, p=seq(0, 100, 1))
#' par(mfrow=c(2,1))
#' t <- g[["time"]]
#' z <- -g[["pressure"]][,1]
#' ## Set zlim because of spurious temperatures.
#' imagep(t, z, t(g[['temperature']]), ylim=c(-100,0), zlim=c(0,20))
#' imagep(t, z, t(g[['salinity']]), ylim=c(-100,0))
#'
#' @family things related to argo data
#' @author Dan Kelley and Clark Richards
argoGrid <- function(argo, p, debug=getOption("oceDebug"), ...)
{
    oceDebug(debug, "argoGrid() {\n", sep="", unindent=1)
    warningMessages <- NULL
    dim <- dim(argo@data$pressure)
    ## ndepth <- dim[1]
    nprofile <- dim[2]
    ## FIXME: modify sal, temp, and pre.  In the end, pre constant along first index
    res <- argo
    res[["flags"]] <- NULL
    warningMessages <- c(warningMessages,
                         "Data flags are omitted from the gridded argo object. Use handleFlags() first to remove bad data.")
    pressure <- argo[["pressure"]]
    if (missing(p)) {
        pt <- apply(pressure, 1, median, na.rm=TRUE)
    } else if (length(p) == 1 && p == "levitus") {
        pt <- standardDepths()
        pt <- pt[pt < max(pressure, na.rm=TRUE)]
    } else if (is.numeric(p)) {
        if (length(p) == 1) {
            if (p < 1)
                stop("'p' must exceed 1")
            pt <- seq(0, max(pressure, na.rm=TRUE), length.out=p)
        } else {
            pt <- p
        }
    } else {
        stop("value of 'p' must be numeric, or \"levitus\"")
    }
    ##message("pt=c(", paste(round(pt), collapse=","), ")")
    npt <- length(pt)
    res@data$pressure <- matrix(NA, ncol=nprofile, nrow=npt)
    for (field in names(res@data)) {
        if (!(field %in% c('time', 'longitude', 'latitude'))) {
            res@data[[field]] <- matrix(NA, ncol=nprofile, nrow=npt)
            for (profile in 1:nprofile) {
                ndata <- sum(!is.na(argo@data[[field]][, profile]))
                if (ndata > 2 && sum(is.finite(diff(pressure[, profile])))
                    && 0 < max(abs(diff(pressure[, profile])), na.rm=TRUE)) {
                    res@data[[field]][, profile] <- approx(pressure[, profile], argo@data[[field]][, profile], pt, ...)$y
                } else {
                    res@data[[field]][, profile] <- rep(NA, npt)
                }
                res@data$pressure[, profile] <- pt
            }
        }
    }
    res@processingLog <- processingLogAppend(res@processingLog, paste("Grid to regular pressures with: ", deparse(match.call()), sep="", collapse=""))
    for (w in warningMessages)
        res@processingLog <- processingLogAppend(res@processingLog, w)
    res
}

argoDecodeFlags <- function(f) # local function
{
    res <- unlist(lapply(seq_along(f), function(i) strsplit(f[i], split="")))
    dim(res) <- c(length(res)/length(f), length(f))
    mode(res) <- "numeric"
    res
}



#' Read an Argo Data File
#'
#' `read.argo` is used to read an Argo file, producing an [argo-class] object.
#' The file must be in the ARGO-style NetCDF format described
#' in the Argo documentation (see references 2 and 3).
#'
#' @details
#'
#' See the Argo documentation (see references 2 and 3) for some details on what files contain.
#' Many items listed in section 2.2.3 of reference 3 are read from the
#' file and stored in the `metadata` slot, with the exception of
#' `longitude` and `latitude`, which are stored in the
#' `data` slot, alongside hydrographic information.
#'
#' The names of several data parameters stored within the netCDF file
#' are altered to fit the oce context. For example, `PRES` becomes `pressure`,
#' matching the name of this variable in other oce data types.
#' The original names are reported by `summary,argo-method`, and
#' data may be extracted with `[[,argo-method` using those names, so
#' the renaming should not be too inconvenient to Argo experts who
#' are new to oce.
#'
#' Several of the netCDF global attributes are also renamed before
#' placement in the `metadata` slot of the return value.  These include
#' `conventions`, `featureType`, `history`, `institution`,
#' `nParameters`, `nProfiles`,  `references`, `source`, `title`,
#' and `userManualVersion`.
#' These names are derived from those in the netcdf
#' file, and mainly follow the pattern explained in the
#' \dQuote{Variable renaming convention} section.
#'
#' For profile data (as indicated by the NetCDF global attribute
#' named `"featureType"` being equal to `"trajectoryProfile"`),
#' the NetCDF item named `"STATION_PARAMETERS"` controls
#' whether variables in the source file will be stored in the
#' `metadata` or `data` slot of the returned object.
#' If `STATION_PARAMETERS` is not present, as is the case for
#' trajectory files (which are detected by `featureType` being
#' `"trajectory"`), some guesses are made as to what goes in
#' `data` and `metadata` slots.
#'
#' Each data item can have variants, as
#' described in Sections 2.3.4 of reference 3.
#' For example, if `"PRES"` is found in `STATION_PARAMETERS`,
#' then `PRES` (pressure) data are sought in the file, along with
#' `PRES_QC`, `PRES_ADJUSTED`, `PRES_ADJUSTED_QC`, and
#' `PRES_ERROR`. The same pattern works for other profile data. The variables
#' are stored with names created as explained in the
#' \dQuote{Variable renaming convention} section below. Note that
#' flags, which are stored variables ending in `"_QC"` in the netcdf
#' file, are stored in the `flags` item within the `metadata` slot
#' of the returned object; thus, for example,
#' `PRES_QC` is stored as `pressure` in `flags`.
#'
#' @section Variable renaming convention:
#' Argo netcdf files employ a `"SNAKE_CASE"` naming scheme (sometimes
#' using lower case) that is inconsistent with the `"camelCase"` scheme
#' used in oce. Since argo objects are just a small part of oce, a decision
#' was made to rename argo items. For example, `"CYCLE_NUMBER"` in the netcdf file
#' becomes `"cycleNumber"` in the oce object returned by `read.argo`.
#' (Note that `[[,argo-method` also accepts `"cycle"` for this item.)
#' The conversion for objects in the `data` slot often also involves
#' expanding on argo abbreviations, e.g. `"PSAL"` becomes `"salinity"`.
#' The renaming work is carried out with [argoNames2oceNames()] for
#' handles both name expansion for several dozen special cases,
#' and with [snakeToCamel()] with the `specialCase` argument
#' set to `"QC"`. While this results in variable names that should make
#' sense in the general oce context (where, for example, salinity is expected
#' to be stored in a variable named `"salinity"`), it may be confusing
#' to argo experts who are just starting to use oce.  Such people might
#' find it helpful to use e.g. `sort(names(x[["metadata"]]))` to get a list
#' of all items in the `metadata` slot (or similar with `"data"`), since working
#' in reverse may be easier than simply guessing at what names oce has chosen.
#' (Note that prior to 2020 June 24, some metadata items were stored in
#' `"SNAKE_CASE"`.)
#'
#' @param file A character string giving the name of the file to load.
#'
#' @template encodingIgnoredTemplate
#'
#' @param debug A flag that turns on debugging.  Set to 1 to get a moderate amount
#' of debugging information, or to 2 to get more.
#'
#' @param processingLog If provided, the action item to be stored in the log.
#' (Typically only provided for internal calls; the default that it provides is
#' better for normal calls by a user.)
#'
#' @param ... additional arguments, passed to called routines.
#'
#' @return
#' An [argo-class] object.
#'
#' @examples
#'\dontrun{
#' ## Example 1: read from a local file
#' library(oce)
#' d <- read.argo("/data/OAR/6900388_prof.nc")
#' summary(d)
#' plot(d)
#'
#' ## Example 2: construct URL for download (brittle)
#' id <- "6900388"
#' url <- "https://www.usgodae.org/ftp/outgoing/argo"
#' if (!length(list.files(pattern="argo_index.txt")))
#'     download.file(paste(url, "ar_index_global_meta.txt", sep="/"), "argo_index.txt")
#' index <- readLines("argo_index.txt")
#' line <- grep(id, index)
#' if (0 == length(line)) stop("id ", id, " not found")
#' if (1 < length(line)) stop("id ", id, " found multiple times")
#' dac <- strsplit(index[line], "/")[[1]][1]
#' profile <- paste(id, "_prof.nc", sep="")
#' float <- paste(url, "dac", dac, id, profile, sep="/")
#' download.file(float, profile)
#' argo <- read.argo(profile)
#' summary(argo)
#'}
#'
#'
#' @seealso
#' The documentation for the [argo-class] class explains the structure of argo
#' objects, and also outlines the other functions dealing with them.
#'
#' @references
#' 1. `https://argo.ucsd.edu`
#'
#' 2. Argo User's Manual Version 3.2, Dec 29th, 2015, available at
#' `https://archimer.ifremer.fr/doc/00187/29825/` online.
#'
#' 3. User's Manual (ar-um-02-01) 13 July 2010, available at
#' `http://www.argodatamgt.org/content/download/4729/34634/file/argo-dm-user-manual-version-2.3.pdf`,
#' which is the main document describing argo data.
#'
#' @section Data sources:
#' Argo data are made available at several websites. A bit of detective
#' work can be required to track down the data.
#'
#' Some servers provide data for floats that surfaced in a given ocean
#' on a given day, the anonymous FTP server
#' \code{usgodae.org/pub/outgoing/argo/geo/} being an example.
#'
#' Other servers provide data on a per-float basis. A complicating
#' factor is that these data tend to be categorized by "dac" (data
#' archiving centre), which makes it difficult to find a particular
#' float. For example,
#' \code{https://www.usgodae.org/ftp/outgoing/argo/} is the top level of
#' a such a repository. If the ID of a float is known but not the
#' "dac", then a first step is to download the text file
#' \code{https://www.usgodae.org/ftp/outgoing/argo/ar_index_global_meta.txt}
#' and search for the ID. The first few lines of that file are header,
#' and after that the format is simple, with columns separated by slash
#' (`/`). The dac is in the first such column and the float ID in the
#' second. A simple search will reveal the dac.
#' For example `data(argo)` is based on float 6900388, and the line
#' containing that token is
#' `bodc/6900388/6900388_meta.nc,846,BO,20120225005617`, from
#' which the dac is seen to be the British Oceanographic Data Centre
#' (`bodc`). Armed with that information, visit
#' \code{https://www.usgodae.org/ftp/outgoing/argo/dac/bodc/6900388}
#' and see a directory called `profiles` that contains a NetCDF
#' file for each profile the float made. These can be read with
#' `read.argo`. It is also possible, and probably more common,
#' to read a NetCDF file containing all the profiles together and for
#' that purpose the file
#' \code{https://www.usgodae.org/ftp/outgoing/argo/dac/bodc/6900388/6900388_prof.nc}
#' should be downloaded and provided as the `file` argument to
#' `read.argo`.  This can be automated as in Example 2,
#' although readers are cautioned that URL structures tend to change
#' over time.
#'
#' Similar steps can be followed on other servers.
#'
#' @author Dan Kelley
#' @family things related to argo data
read.argo <- function(file,
    encoding=NA,
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
    debug <- max(0, min(2, floor(as.numeric(debug))))
    if (!requireNamespace("ncdf4", quietly=TRUE))
        stop('must install.packages("ncdf4") to read argo data')
    if (missing(processingLog)) processingLog <- paste(deparse(match.call()), sep="", collapse="")
    ## ofile <- file
    filename <- ""
    ## NOTE: need to name ncdf4 package because otherwise R checks give warnings.
    if (is.character(file)) {
        filename <- fullFilename(file)
        file <- ncdf4::nc_open(file)
        on.exit(ncdf4::nc_close(file))
    } else {
        if (!inherits(file, "connection"))
            stop("argument `file' must be a character string or connection")
        if (!isOpen(file)) {
            file <- ncdf4::nc_open(file)
            on.exit(ncdf4::nc_close(file))
        }
    }
    oceDebug(debug, "read.argo(file=\"", filename, "\", ...) {\n", sep="", unindent=1, style="bold")
    varNames <- names(file$var)

    ## 'lc' will be TRUE if the data names are in lower case
    lc <- "data_type" %in% varNames
    oceDebug(debug, "File convention inferred to be ", if (lc) "lower-case" else "upper-case", ".\n", sep="")
    res <- new("argo")
    ## columnNames <- gsub(" *$", "", gsub("^ *", "", unique(as.vector(ncvar_get(f, maybeLC("STATION_PARAMETERS", lc))))))
    ## QCNames <- paste(columnNames, "_QC",  sep="")

    ## global attributes (see https://github.com/dankelley/oce/issues/1528)
    getGlobalAttribute <- function(file, attname)
    {
        a <- ncdf4::ncatt_get(nc=file, varid=0, attname=attname)
        res <- if (a$hasatt) a$value else NULL
        ## message("'", attname, "' value='", res, "'")
        res
    }
    res@metadata$title <- getGlobalAttribute(file, "title")
    res@metadata$institution <- getGlobalAttribute(file, "institution")
    res@metadata$source <- getGlobalAttribute(file, "source")
    res@metadata$history <- getGlobalAttribute(file, "history")
    res@metadata$references <- getGlobalAttribute(file, "references")
    res@metadata$userManualVersion <- getGlobalAttribute(file, "user_manual_version")
    res@metadata$conventions <- getGlobalAttribute(file, "Conventions")
    res@metadata$featureType <- getGlobalAttribute(file, "featureType")

    varNamesOmit <- function(v, o)
    {
        where <- which(tolower(o) == tolower(v))
        if (length(where))
            v <- v[-where[1]]
        v
    }

    oceDebug(debug-1, "At processing step  1, the ", length(varNames), " varnames are: c(\"", paste(sort(varNames), collapse="\",\""), "\")\n", sep="")
    res@metadata$id <- if (maybeLC("PLATFORM_NUMBER", lc) %in% varNames)
        as.vector(trimString(ncdf4::ncvar_get(file, maybeLC("PLATFORM_NUMBER", lc)))) else NULL
    varNames <- varNamesOmit(varNames, "PLATFORM_NUMBER")

    ### if (FALSE) {
    ###     ## DEK 2019-04-08 note:
    ###     ## What is the FLOAT_SERIAL_NUMBER? For the sample file in this package,
    ###     ## the first 210 values are "APEX-SBE 1882", and after that we have 211
    ###     ## to 223 equal to "1882".  Since I do not know what it means, or
    ###     ## whether to prefer one of these names to another, I propose we
    ###     ## just don't bother trying to decode this item into a full-fledged
    ###     ## (camelCase namd) metadata item, but rather just let it gt stored
    ###     ## as a plain-copy (SNAKE_CASE named) metadata item.
    ###     res@metadata$floatSerialNumber <- if (maybeLC("FLOAT_SERIAL_NO", lc) %in% varNames)
    ###         as.vector(trimString(ncdf4::ncvar_get(file, maybeLC("FLOAT_SERIAL_NO", lc)))) else NULL
    ###     varNames <- varNamesOmit(varNames, "FLOAT_SERIAL_NO")
    ###     oceDebug(debug, "varNames=", paste(varNames, collapse=","), "\n")
    ### }

    res@metadata$projectName <- if (maybeLC("PROJECT_NAME", lc) %in% varNames)
        as.vector(trimString(ncdf4::ncvar_get(file, maybeLC("PROJECT_NAME", lc)))) else NULL
    varNames <- varNamesOmit(varNames, "PROJECT_NAME")
    oceDebug(debug-1, "Extracting PROJECT_NAME\n")
    oceDebug(debug-1, "At processing step  2, the ", length(varNames), " varnames are: c(\"", paste(sort(varNames), collapse="\",\""), "\")\n", sep="")
    res@metadata$PIName <- if (maybeLC("PI_NAME", lc) %in% varNames)
        as.vector(trimString(ncdf4::ncvar_get(file, maybeLC("PI_NAME", lc)))) else NULL
    varNames <- varNamesOmit(varNames, "PI_NAME")
    oceDebug(debug-1, "Extracting PI_NAME\n")
    oceDebug(debug-1, "At processing step  3, the ", length(varNames), " varnames are: c(\"", paste(sort(varNames), collapse="\",\""), "\")\n", sep="")
    res@metadata$stationParameters <- NULL
    res@metadata$nParameters <- file$dim$N_PARAM$len
    res@metadata$nProfiles <- file$dim$N_PROF$len
    if (maybeLC("STATION_PARAMETERS", lc) %in% varNames) {
        res@metadata$stationParameters <- trimString(ncdf4::ncvar_get(file, maybeLC("STATION_PARAMETERS", lc)))
        if (is.null(res@metadata$stationParameters))
            warning("This file has nothing listed in its STATION_PARAMETERS item, so pressure, salinity, temperature, etc. are being stored in the metadata slot instead of the data slot. This will cause problems in further processing.")
    } else {
        ## print(sort(names(file$var)))
        if (res@metadata$featureType != "trajectory")
            warning("This 'profile'-type file lacks a STATION_PARAMETERS item, so guesses were on whether to store items in the 'metadata' or 'data' slot. This may lead cause problems.")
        for (want in c("PSAL", "TEMP", "PRES")) {
            if (want %in% toupper(varNames)) {
                res@metadata$stationParameters <- c(res@metadata$stationParameters, want)
                oceDebug(debug, "Will try to extract \"", want, "\" as a special case, because STATION_PARAMETERS is missing.\n", sep="")
            }
        }
    }
    varNames <- varNamesOmit(varNames, "STATION_PARAMETERS")
    oceDebug(debug-1, "Extracting STATION_PARAMETERS (if it exists)\n")
    oceDebug(debug-1, "At processing step  4, the ", length(varNames), " varnames are: c(\"", paste(sort(varNames), collapse="\",\""), "\")\n", sep="")

    oceDebug(debug-1, "Extracting CYCLE_NUMBER\n")
    res@metadata$cycleNumber <- if (maybeLC("CYCLE_NUMBER", lc) %in% varNames)
        as.vector(ncdf4::ncvar_get(file, maybeLC("CYCLE_NUMBER", lc))) else NULL
    varNames <- varNamesOmit(varNames, "CYCLE_NUMBER")

    oceDebug(debug-1, "Extracting CYCLE_NUMBER_INDEX\n")
    res@metadata$cycleNumberIndex <- if (maybeLC("CYCLE_NUMBER_INDEX", lc) %in% varNames)
        as.vector(ncdf4::ncvar_get(file, maybeLC("CYCLE_NUMBER_INDEX", lc))) else NULL
    varNames <- varNamesOmit(varNames, "CYCLE_NUMBER_INDEX")

    oceDebug(debug-1, "Extracting CYCLE_NUMBER_ADJUSTED\n")
    res@metadata$cycleNumberAdjusted <- if (maybeLC("CYCLE_NUMBER_ADJUSTED", lc) %in% varNames)
        as.vector(ncdf4::ncvar_get(file, maybeLC("CYCLE_NUMBER_ADJUSTED", lc))) else NULL
    varNames <- varNamesOmit(varNames, "CYCLE_NUMBER_ADJUSTED")

    oceDebug(debug-1, "Extracting CYCLE_NUMBER_ADJUSTED_INDEX\n")
    res@metadata$cycleNumberAdjustedIndex <- if (maybeLC("CYCLE_NUMBER_ADJUSTED_INDEX", lc) %in% varNames)
        as.vector(ncdf4::ncvar_get(file, maybeLC("CYCLE_NUMBER_ADJUSTED_INDEX", lc))) else NULL
    varNames <- varNamesOmit(varNames, "CYCLE_NUMBER_ADJUSTED_INDEX")

    oceDebug(debug-1, "At processing step  5, the ", length(varNames), " varnames are: c(\"", paste(sort(varNames), collapse="\",\""), "\")\n", sep="")
    res@metadata$direction <- if (maybeLC("DIRECTION", lc) %in% varNames)
        as.vector(ncdf4::ncvar_get(file, maybeLC("DIRECTION", lc))) else NULL
    varNames <- varNamesOmit(varNames, "DIRECTION")
    oceDebug(debug-1, "Extracting DIRECTION\n")
    oceDebug(debug-1, "At processing step  6, the ", length(varNames), " varnames are: c(\"", paste(sort(varNames), collapse="\",\""), "\")\n", sep="")
    res@metadata$dataCentre <- if (maybeLC("DATA_CENTRE", lc) %in% varNames)
        as.vector(ncdf4::ncvar_get(file, maybeLC("DATA_CENTRE", lc))) else NULL
    varNames <- varNamesOmit(varNames, "DATA_CENTRE")
    oceDebug(debug-1, "Extracting DATA_CENTRE\n")
    oceDebug(debug-1, "At processing step  7, the ", length(varNames), " varnames are: c(\"", paste(sort(varNames), collapse="\",\""), "\")\n", sep="")
    res@metadata$DCReference <- if (maybeLC("DC_REFERENCE", lc) %in% varNames)
        as.vector(trimString(ncdf4::ncvar_get(file, maybeLC("DC_REFERENCE", lc)))) else NULL
    varNames <- varNamesOmit(varNames, "DC_REFERENCE")
    oceDebug(debug-1, "Extracting DC_REFERENCE\n")
    oceDebug(debug-1, "At processing step  8, the ", length(varNames), " varnames are: c(\"", paste(sort(varNames), collapse="\",\""), "\")\n", sep="")
    res@metadata$dataStateIndicator <- if (maybeLC("DATA_STATE_INDICATOR", lc) %in% varNames)
        as.vector(trimws(ncdf4::ncvar_get(file, maybeLC("DATA_STATE_INDICATOR", lc)))) else NULL
    varNames <- varNamesOmit(varNames, "DATA_STATE_INDICATOR")
    oceDebug(debug-1, "Extracting DATA_STATE_INDICATOR\n")
    oceDebug(debug-1, "At processing step  9, the ", length(varNames), " varnames are: c(\"", paste(sort(varNames), collapse="\",\""), "\")\n", sep="")
    res@metadata$dataMode <- if (maybeLC("DATA_MODE", lc) %in% varNames)
        strsplit(ncdf4::ncvar_get(file, maybeLC("DATA_MODE", lc)), "")[[1]] else NULL
    varNames <- varNamesOmit(varNames, "DATA_MODE")
    oceDebug(debug-1, "Extracting DATA_MODE\n")
    oceDebug(debug-1, "At processing step 10, the ", length(varNames), " varnames are: c(\"", paste(sort(varNames), collapse="\",\""), "\")\n", sep="")
    res@metadata$instReference <- if (maybeLC("INST_REFERENCE", lc) %in% varNames)
        as.vector(trimString(ncdf4::ncvar_get(file, maybeLC("INST_REFERENCE", lc)))) else NULL
    varNames <- varNamesOmit(varNames, "INST_REFERENCE")
    oceDebug(debug-1, "Extracting INST_REFERENCE\n")
    oceDebug(debug-1, "At processing step 11, the ", length(varNames), " varnames are: c(\"", paste(sort(varNames), collapse="\",\""), "\")\n", sep="")
    res@metadata$firmwareVersion <- if (maybeLC("FIRMWARE_VERSION", lc) %in% varNames)
        as.vector(trimString(ncdf4::ncvar_get(file, maybeLC("FIRMWARE_VERSION", lc)))) else NULL
    varNames <- varNamesOmit(varNames, "FIRMWARE_VERSION")
    oceDebug(debug-1, "Extracting FIRMWARE_REFERENCE\n")
    oceDebug(debug-1, "At processing step 12, the ", length(varNames), " varnames are: c(\"", paste(sort(varNames), collapse="\",\""), "\")\n", sep="")
    res@metadata$WMOInstType <- if (maybeLC("WMO_INST_TYPE", lc) %in% varNames)
        as.vector(trimString(ncdf4::ncvar_get(file, maybeLC("WMO_INST_TYPE", lc)))) else NULL
    varNames <- varNamesOmit(varNames, "WMO_INST_TYPE")
    oceDebug(debug-1, "Extracting WMO_INST_TYPE\n")
    oceDebug(debug-1, "At processing step 13, the ", length(varNames), " varnames are: c(\"", paste(sort(varNames), collapse="\",\""), "\")\n", sep="")
    res@metadata$juld <- if (maybeLC("JULD", lc) %in% varNames)
        as.vector(ncdf4::ncvar_get(file, maybeLC("JULD", lc))) else NULL
    varNames <- varNamesOmit(varNames, "JULD")
    oceDebug(debug-1, "Extracting JULD\n")
    oceDebug(debug-1, "At processing step 14, the ", length(varNames), " varnames are: c(\"", paste(sort(varNames), collapse="\",\""), "\")\n", sep="")
    ## set up 'time' also
    t0s <- as.vector(ncdf4::ncvar_get(file, maybeLC("REFERENCE_DATE_TIME", lc)))
    varNames <- varNamesOmit(varNames, "REFERENCE_DATE_TIME")
    oceDebug(debug-1, "Extracting REFERENCE_DATE_TIME\n")
    oceDebug(debug-1, "At processing step 15, the ", length(varNames), " varnames are: c(\"", paste(sort(varNames), collapse="\",\""), "\")\n", sep="")
    t0 <- strptime(t0s, "%Y%m%d%M%H%S", tz="UTC")
    ##julianDayTime <- as.vector(ncdf4::ncvar_get(file, maybeLC("JULD", lc)))
    res@data$time <- t0 + res@metadata$juld * 86400
    rm(list=c("t0s", "t0")) # no longer needed

    res@metadata$juldQC <- if (maybeLC("JULD_QC", lc) %in% varNames)
        as.vector(ncdf4::ncvar_get(file, maybeLC("JULD_QC", lc))) else NULL
    varNames <- varNamesOmit(varNames, "JULD_QC")
    oceDebug(debug-1, "Extracting JULD_QC\n")
    oceDebug(debug-1, "At processing step 16, the ", length(varNames), " varnames are: c(\"", paste(sort(varNames), collapse="\",\""), "\")\n", sep="")
    res@metadata$juldLocation <- if (maybeLC("JULD_LOCATION", lc) %in% varNames)
        as.vector(ncdf4::ncvar_get(file, maybeLC("JULD_LOCATION", lc))) else NULL
    varNames <- varNamesOmit(varNames, "JULD_LOCATION")
    oceDebug(debug-1, "Extracting JULD_LOCATION\n")
    oceDebug(debug-1, "At processing step 17, the ", length(varNames), " varnames are: c(\"", paste(sort(varNames), collapse="\",\""), "\")\n", sep="")

    ## Now for the data.
    res@metadata$dataNamesOriginal <- list() # NB. will store upper-case names

    if (maybeLC("LATITUDE", lc) %in% varNames) {
        res@data$latitude <- as.vector(ncdf4::ncvar_get(file, maybeLC("LATITUDE", lc)))
        res@metadata$dataNamesOriginal$latitude <- "LATITUDE"
        varNames <- varNamesOmit(varNames, "LATITUDE")
        latitudeNA <- ncdf4::ncatt_get(file, maybeLC("LATITUDE", lc), "_FillValue")$value
        res@data$latitude[res@data$latitude == latitudeNA] <- NA
        rm(list="latitudeNA") # no longer needed
        res@metadata$units$latitude <-
            if (1 == length(grep("north", ncdf4::ncatt_get(file, maybeLC("LATITUDE", lc), "units")$value, ignore.case=TRUE)))
                list(unit=expression(degree*N), scale="") else list(unit=expression(degree*S), scale="")
    }
    if (maybeLC("LONGITUDE", lc) %in% varNames) {
        res@data$longitude <- as.vector(ncdf4::ncvar_get(file, maybeLC("LONGITUDE", lc)))
        res@metadata$dataNamesOriginal$longitude <- "LONGITUDE"
        varNames <- varNamesOmit(varNames, "LONGITUDE")
        longitudeNA <- ncdf4::ncatt_get(file, maybeLC("LONGITUDE", lc), "_FillValue")$value
        res@data$longitude[res@data$longitude == longitudeNA] <- NA
        rm(list="longitudeNA") # no longer needed
        res@metadata$units$longitude <-
            if (1 == length(grep("east", ncdf4::ncatt_get(file, maybeLC("LONGITUDE", lc), "units")$value, ignore.case=TRUE)))
                list(unit=expression(degree*E), scale="") else list(unit=expression(degree*W), scale="")
    }

    res@metadata$positionQC <- if (maybeLC("POSITION_QC", lc) %in% varNames)
        as.vector(ncdf4::ncvar_get(file, maybeLC("POSITION_QC", lc))) else NULL
    varNames <- varNamesOmit(varNames, "POSITION_QC")
    oceDebug(debug-1, "POSITION_QC\n")
    oceDebug(debug-1, "varNames=", paste(varNames, collapse=","), "\n")
    res@metadata$positioningSystem <- if (maybeLC("POSITIONING_SYSTEM", lc) %in% varNames)
        as.vector(trimString(ncdf4::ncvar_get(file, maybeLC("POSITIONING_SYSTEM", lc)))) else NULL
    varNames <- varNamesOmit(varNames, "POSITIONING_SYSTEM")
    oceDebug(debug-1, "POSITIONING_SYSTEM\n")
    oceDebug(debug-1, "varNames=", paste(varNames, collapse=","), "\n")
    stationParameters <- unique(as.vector(res@metadata$stationParameters)) # will be PRES, TEMP etc

    ## Handle units before getting the data. (We must do it here, because when
    ## we read the data, we remove entries from varNames ... that is how
    ## we know what is left over at the end, to be shoved into metadata.)
    ## FIXME: it would be nice to automate this more, to handle cases not handled
    ## in the hardwired code below.

    ## Oxygen: DOXY DOXY_ADJUSTED DOXY_ADJUSTED_ERROR
    if (maybeLC("DOXY", lc) %in% varNames) {
        attTMP <- ncdf4::ncatt_get(file, maybeLC("DOXY", lc), "units")
        if (attTMP$hasatt) {
            if (attTMP$value == "micromole/kg") {
                res@metadata$units$oxygen <- list(unit=expression(mu*mol/kg),scale="")
            } else {
                warning("skipping oxygen unit '", attTMP$value, "' because only understood unit is 'micromole/kg'", sep="")
            }
        }
    }
    if (maybeLC("DOXY_ADJUSTED", lc) %in% varNames) {
        ## print(ncdf4::ncatt_get(file, maybeLC("DOXY", lc), "long_name"))
        attTMP <- ncdf4::ncatt_get(file, maybeLC("DOXY_ADJUSTED", lc), "units")
        if (attTMP$hasatt) {
            if (attTMP$value == "micromole/kg") {
                res@metadata$units$oxygenAdjusted <- list(unit=expression(mu*mol/kg),scale="")
            } else {
                warning("skipping oxygenAdjusted unit '", attTMP$value, "' because only understood unit is 'micromole/kg'", sep="")
            }
        }
    }
    if (maybeLC("DOXY_ADJUSTED_ERROR", lc) %in% varNames) {
        ## print(ncdf4::ncatt_get(file, maybeLC("DOXY", lc), "long_name"))
        attTMP <- ncdf4::ncatt_get(file, maybeLC("DOXY_ADJUSTED_ERROR", lc), "units")
        if (attTMP$hasatt) {
            if (attTMP$value == "micromole/kg") {
                res@metadata$units$oxygenAdjustedError <- list(unit=expression(mu*mol/kg),scale="")
            } else {
                warning("skipping oxygenAdjustedError unit '", attTMP$value, "' because only understood unit is 'micromole/kg'", sep="")
            }
        }
    }

    ## Temperature: TEMP, TEMP_ADJUSTED, TEMP_ADJUSTED_ERROR
    if (maybeLC("TEMP", lc) %in% varNames) {
        if (1 == length(grep("ITS-90", ncdf4::ncatt_get(file, maybeLC("TEMP", lc), "long_name")$value, ignore.case=TRUE)))
            res@metadata$units$temperature <- list(unit=expression(degree *C), scale="ITS-90")
        else res@metadata$units$temperature <- list(unit=expression(degree *C), scale="ITS-90")
    }
    if (maybeLC("TEMP_ADJUSTED", lc) %in% varNames) {
        if (1 == length(grep("ITS-90", ncdf4::ncatt_get(file, maybeLC("TEMP_ADJUSTED", lc), "long_name")$value, ignore.case=TRUE)))
            res@metadata$units$temperatureAdjusted <- list(unit=expression(degree *C), scale="ITS-90")
        else res@metadata$units$temperatureAdjusted <- list(unit=expression(degree *C), scale="ITS-90")
    }
    if (maybeLC("TEMP_ADJUSTED_ERROR", lc) %in% varNames) {
        if (1 == length(grep("ITS-90", ncdf4::ncatt_get(file, maybeLC("TEMP_ADJUSTED_ERROR", lc), "long_name")$value, ignore.case=TRUE)))
            res@metadata$units$temperatureAdjustedError <- list(unit=expression(degree *C), scale="ITS-90")
        else res@metadata$units$temperatureAdjustedError <- list(unit=expression(degree *C), scale="ITS-90")
    }

    ## salinity: PSAL, PSAL_ADJUSTED, PSAL_ADJUSTED_ERROR
    if (maybeLC("PSAL", lc) %in% varNames) {
        if (1 == length(grep("PRACTICAL", ncdf4::ncatt_get(file, maybeLC("PSAL", lc), "long_name")$value, ignore.case=TRUE)))
            res@metadata$units$salinity <- list(unit=expression(), scale="PSS-78")
        else
            res@metadata$units$salinity <- list(unit=expression(), scale="PSS-78")
    }
    if (maybeLC("PSAL_ADJUSTED", lc) %in% varNames) {
        if (1 == length(grep("PRACTICAL", ncdf4::ncatt_get(file, maybeLC("PSAL_ADJUSTED", lc), "long_name")$value, ignore.case=TRUE)))
            res@metadata$units$salinityAdjusted <- list(unit=expression(), scale="PSS-78")
        else
            res@metadata$units$salinityAdjusted <- list(unit=expression(), scale="PSS-78")
    }
    if (maybeLC("PSAL_ADJUSTED_ERROR", lc) %in% varNames) {
        if (1 == length(grep(maybeLC("PRACTICAL", lc), ncdf4::ncatt_get(file, maybeLC("PSAL_ADJUSTED_ERROR", lc), "long_name")$value, ignore.case=TRUE)))
            res@metadata$units$salinityAdjustedError <- list(unit=expression(), scale="PSS-78")
        else
            res@metadata$units$salinityAdjustedError <- list(unit=expression(), scale="PSS-78")
    }

    ## pressure: PRES, PRES_ADJUSTED, PRES_ADJUSTED_ERROR
    if (maybeLC("PRES", lc) %in% varNames) {
        if (1 == length(grep("decibar", ncdf4::ncatt_get(file, maybeLC("PRES", lc), "units")$value, ignore.case=TRUE)))
            res@metadata$units$pressure <- list(unit=expression(dbar), scale="")
        else
            res@metadata$units$pressure <- list(unit=expression(dbar), scale="")
    }
    if (maybeLC("PRES_ADJUSTED", lc) %in% varNames) {
        if (1 == length(grep("decibar", ncdf4::ncatt_get(file, maybeLC("PRES_ADJUSTED", lc), "units")$value, ignore.case=TRUE)))
            res@metadata$units$pressureAdjusted <- list(unit=expression(dbar), scale="")
        else
            res@metadata$units$pressureAdjusted <- list(unit=expression(dbar), scale="")
    }
    if (maybeLC("PRES_ADJUSTED_ERROR", lc) %in% varNames) {
        if (1 == length(grep("decibar", ncdf4::ncatt_get(file, maybeLC("PRES_ADJUSTED_ERROR", lc), "units")$value, ignore.case=TRUE)))
            res@metadata$units$pressureAdjustedError <- list(unit=expression(dbar), scale="")
        else
            res@metadata$units$pressureAdjustedError<- list(unit=expression(dbar), scale="")
    }

    ## Fix up names of flags. This became required with changes made to argoNames2oceNames() in Dec 17-18, 2016. Arguably, I
    ## should find out why the change occurred, but fixing the names now is just as easy, and might be clearer to the reader.
    names(res@metadata$flags) <- gsub("QC$", "", names(res@metadata$flags))

    ## Now, work through the columnar data. Note how we remove entries from varNames again,
    ## after having interrupted that practice whilst finding units and flags.
    oceDebug(debug, "About to process stationParameters: c(\"",
            paste(stationParameters, collapse="\",\""), "\")\n", sep="")
    for (item in stationParameters) {
        if (!nchar(item)) ## some files have unnamed variables, so we skip them
            next
        n <- item
        d <- getData(file, maybeLC(n, lc))
        varNames <- varNamesOmit(varNames, n)
        if (!is.null(d)) {
            oceDebug(debug, "Storing \"", n, "\" as \"", argoNames2oceNames(n), "\" in the data slot.\n", sep="")
            res@data[[argoNames2oceNames(n)]] <- d
            res@metadata$dataNamesOriginal[[argoNames2oceNames(n)]] <- n
        } else {
            oceDebug(debug, "Set item = \"", n, "\" in data slot to NULL, since the data file contains no data for this.\n", sep="")
            res@data[[argoNames2oceNames(n)]] <- NULL
        }
        oceDebug(debug-1, "Remaining ", length(varNames), "are: =", paste(varNames, collapse=" "), "\n")

        n <- paste(item, maybeLC("_QC", lc), sep="")
        oceDebug(debug-2, "about to try to get '", n, "' from netcdf file\n", sep="")
        ##if (n == "PRES_QC") browser()
        d <- getData(file, maybeLC(n, lc), quiet=TRUE)
        oceDebug(debug-2, "... got it\n", sep="")
        varNames <- varNamesOmit(varNames, n)
        oceDebug(debug-1, n, "\n")
        oceDebug(debug-1, "B varNames=", paste(sort(varNames), collapse=","), "\n")
        if (!is.null(d)) res@metadata$flags[[argoNames2oceNames(n)]] <- argoDecodeFlags(d)
        n <- paste(item, maybeLC("_ADJUSTED", lc), sep="")
        if (n %in% varNames) {
            d <- getData(file, maybeLC(n, lc))
            varNames <- varNamesOmit(varNames, n)
            oceDebug(debug-1, n, "\n")
            oceDebug(debug-1, "C varNames=", paste(sort(varNames), collapse=","), "\n")
            if (!is.null(d)) {
                res@data[[argoNames2oceNames(n)]] <- d
                res@metadata$dataNamesOriginal[[argoNames2oceNames(n)]] <- n
            } else {
                res@data[[argoNames2oceNames(n)]] <- NULL
            }
        }
        n <- paste(item, maybeLC("_ADJUSTED_QC", lc), sep="")
        if (n %in% varNames) {
            d <- getData(file, maybeLC(n, lc))
            varNames <- varNamesOmit(varNames, n)
            oceDebug(debug-1, n, "\n")
            oceDebug(debug-1, "D varNames=", paste(sort(varNames), collapse=","), "\n")
            if (!is.null(d)) res@metadata$flags[[argoNames2oceNames(n)]] <- argoDecodeFlags(d)
        }
        n <- paste(item, maybeLC("_ADJUSTED_ERROR", lc), sep="")
        if (n %in% varNames) {
            d <- getData(file, maybeLC(n, lc))
            varNames <- varNamesOmit(varNames, n)
            oceDebug(debug-1, n, "\n")
            oceDebug(debug-1, "E varNames=", paste(sort(varNames), collapse=","), "\n")
            if (!is.null(d)) {
                res@data[[argoNames2oceNames(n)]] <- d
                res@metadata$dataNamesOriginal[[argoNames2oceNames(n)]] <- n
            } else {
                res@data[[argoNames2oceNames(n)]] <- NULL
            }
        }
    }
    oceDebug(debug, "After processing stationParameters, flag names are: c(\"",
             paste(names(res@metadata$flags), collapse="\",\""), "\").\n", sep="")
    if (length(res@metadata$flags))
        names(res@metadata$flags) <- gsub("QC$", "", names(res@metadata$flags))
    oceDebug(debug, "After trimming QC, flag names are: c(\"",
             paste(names(res@metadata$flags), collapse="\",\""), "\")\n", sep="")
    res@metadata$filename <- filename

    #cat("@L1391 varnames: ", paste(sort(varNames), sep=" "), "\n")

    ## Now, insert any unprocessed items from varNames into metadata. We
    ## need to check for access failures because we get the error
    ##     > Error in R_nc4_get_vara_text: NetCDF: Index exceeds dimension bound
    ##     > Var: HISTORY_PARAMETER  Ndims: 3   Start: 0,0,0 Count: 0,223,16
    ## for "HISTORY_INSTITUTION", "HISTORY_STEP", "HISTORY_SOFTWARE",
    ## "HISTORY_SOFTWARE_RELEASE", "HISTORY_REFERENCE",
    ## "HISTORY_DATE", "HISTORY_ACTION", "HISTORY_PARAMETER",
    ## "HISTORY_START_PRES", "HISTORY_STOP_PRES",
    ## "HISTORY_PREVIOUS_VALUE", "HISTORY_QCTEST"
    for (name in varNames) {
        ocename <- snakeToCamel(name, specialCases=c("QC"))
        oceDebug(debug, "Inserting \"", name, "\" as \"", ocename, "\" in the metadata slot.\n", sep="")
        value <- NA
        o <- capture.output(
                            {
                                value <- try(ncdf4::ncvar_get(file, name), silent=TRUE)
                            }
        )
        if (inherits(value, "try-error")) {
            ## see https://github.com/dankelley/oce/issues/1522 for a discussion of the fact
            ## that the file used for data(argo) has zero-length HISTORY_* items.
            if (length(grep("Index exceeds dimension", o))) {
                ## FIXME: this code is brittle, being dependent on the layout of
                ## FIXME: the output from nc_open(), which might be subject to change.
                ## FIXME: The code worked on 2019-04-13.
                oceDebug(debug, "ncdf4::ncvar_get() error diagnosis ... name=", name, " len=",file$var[[name]]$dim[[file$var[[name]]$ndim]]$len, " (if this is 0, will not save '", name, "' to metadata)\n")
                if (0 != file$var[[name]]$dim[[file$var[[name]]$ndim]]$len) {
                    oceDebug(debug, "ncvar_get() failed for \"", name, "\" (Index exceeds dimension), so it isn't stored in metadata\n")
                }
            } else {
                oceDebug(debug, "ncvar_get() failed for \"", name, "\", so it isn't stored in metadata\n")
            }
        } else{
            ## Make a vector, if it is a single-column matrix
            if (1 == length(dim(value)))
                value <- as.vector(value)
            ## Trim leading/trailing whitespace, if it is a string
            if (is.character(value))
                value <- trimString(value)
            res@metadata[[ocename]] <- value
        }
    }
    ## Record a log item
    res@processingLog <- processingLogAppend(res@processingLog, paste("read.argo(file=\"", filename, "\")", sep=""))
    oceDebug(debug, "} # read.argo()\n", sep="", unindent=1, style="bold")
    res
}

#' Coerce Data Into an Argo Dataset
#'
#' Coerce a dataset into an argo dataset. This is not the right way to
#' read official argo datasets, which are provided in NetCDF format and may
#' be read with [read.argo()].
#'
#' @param time a vector of POSIXct times.
#' @param longitude a vector of longitudes.
#' @param latitude a vector of latitudes.
#' @param salinity a vector of salinities.
#' @param temperature a vector of temperatures.
#' @param pressure a vector of pressures.
#' @param units an optional list containing units. If `NULL`, the default,
#' then `"degree east"` is used for `longitude`,
#' `"degree north"` for `latitude`,
#' `""` for `salinity`,
#' `"ITS-90"` for `temperature`, and
#' `"dbar"` for `pressure`.
#' @param id an identifier for the argo float, typically a number, but stored within
#' the object in a character form. (For example, the dataset retrieved with `data(argo)`
#' has an `id` of `"6900388"`.)
#' @param filename a source filename, which defaults to an empty string.
#' @param missingValue an optional missing value, indicating data values that should be
#' taken as `NA`.
#'
#' @return
#' An [argo-class] object.
#'
#' @seealso
#' The documentation for the [argo-class] class explains the structure of argo
#' objects, and also outlines the other functions dealing with them.
#'
#' @author Dan Kelley
#' @family things related to argo data
as.argo <- function(time, longitude, latitude,
                       salinity, temperature, pressure,
                       units=NULL,
                       id, filename="",
                       missingValue)
{
    if (inherits(class, "data.frame")) {
        df <- time
        names <- names(df)
        time <- if ("time" %in% names) df$time else NULL
        salinity <- if ("salinity" %in% names) df$salinity else NULL
        temperature <- if ("temperature" %in% names) df$temperature else NULL
        pressure <- if ("pressure" %in% names) df$pressure else NULL
        longitude <- if ("longitude" %in% names) df$longitude else NULL
        latitude <- if ("latitude" %in% names) df$latitude else NULL
        id <- if ("id" %in% names) df$id else NULL
    } else {
        if (missing(time)) stop("must give time")
        if (missing(longitude)) stop("must give longitude")
        if (missing(latitude)) stop("must give latitude")
        if (missing(temperature)) stop("must give temperature")
        if (missing(salinity)) stop("must give salinity")
        if (missing(pressure)) stop("must give pressure")
        if (missing(id)) stop("must give id")
    }
    res <- new("argo", time=time, id=id,
               longitude=longitude, latitude=latitude, salinity=salinity,
               temperature=temperature, pressure=pressure, filename=filename)
    res@metadata$units <- if (!is.null(units)) units else
        list(longitude=list(expression(degree*E), scale=""),
             latitude=list(expression(degree*N), scale=""),
             salinity=list(unit=expression(), scale="PSS-78"), # assuming a particular scale
             temperature=list(unit=expression(degree*C), scale="ITS-90"), # assuming a particular scale
             pressure=list(unit=expression(dbar), scale="")) # assuming a particular unit
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}


#' Plot an argo Object
#'
#' Plot a summary diagram for argo data.
#'
#' @param x an [argo-class] object.
#'
#' @param which list of desired plot types, one of the following. Note
#' that [oce.pmatch()] is used to try to complete partial
#' character matches, and that an error will occur if the match is
#' not complete (e.g. `"salinity"` matches to both
#' `"salinity ts"` and `"salinity profile"`.).
#'
#' * `which=1`, `which="trajectory"` or `which="map"` gives a
#' plot of the argo trajectory, with the coastline, if one is provided.
#'
#' * `which=2` or `"salinity ts"` gives a time series of
#' salinity at the indicated level(s)
#'
#' * `which=3` or `"temperature ts"` gives a time series
#' of temperature at the indicated level(s)
#'
#' * `which=4` or `"TS"` gives a TS diagram at the
#' indicated level(s)
#'
#' * `which=5` or `"salinity profile"` gives a salinity
#' profile of all the data (with S and p trimmed to the 1 and 99
#' percentiles)
#'
#' * `which=6` or `"temperature profile"` gives a
#' temperature profile (with T and p trimmed to the 1 and 99
#' percentiles)
#
#' @param level depth pseudo-level to plot, for `which=2` and higher.  May be an
#' integer, in which case it refers to an index of depth (1 being the top)
#' or it may be the string "all" which means to plot all data.
#'
#' @param coastline character string giving the coastline to be used in an Argo-location
#' map, or `"best"` to pick the one with highest resolution, or
#' `"none"` to avoid drawing the coastline.
#'
#' @param cex size of plotting symbols to be used if `type='p'`.
#'
#' @param pch type of plotting symbols to be used if `type='p'`.
#'
#' @param type plot type, either `"l"` or `"p"`.
#'
#' @param col optional list of colors for plotting.
#'
#' @param fill Either a logical, indicating whether to fill the land with
#' light-gray, or a color name.  Owing to problems with some projections, the
#' default is not to fill.
#'
#' @param mgp 3-element numerical vector to use for `par(mgp)`, and also for
#' `par(mar)`, computed from this.  The default is tighter than the R
#' default, in order to use more space for the data and less for the axes.
#'
#' @param projection indication of the projection to be used
#' in trajectory maps. If this is `NULL`, no projection is used, although
#' the plot aspect ratio will be set to yield zero shape distortion at the
#' mean float latitude.  If `projection="automatic"`, then one
#' of two projections is used: stereopolar (i.e. `"+proj=stere +lon_0=X"`
#' where `X` is the mean longitude), or Mercator (i.e. `"+proj=merc"`)
#' otherwise.  Otherwise, `projection` must be a character string specifying
#' a projection in the notation used by [oceProject()] and [mapPlot()].
#'
#' @param mar value to be used with `par('mar')`.
#'
#' @param tformat optional argument passed to [oce.plot.ts()], for plot
#' types that call that function.  (See [strptime()] for the format
#' used.)
#'
#' @param debug debugging flag.
#'
#' @param ... optional arguments passed to plotting functions.
#'
#' @return None.
#'
#' @examples
#' library(oce)
#' data(argo)
#' tc <- cut(argo[["time"]], "year")
#' # Example 1: plot map, which reveals float trajectory.
#' plot(argo, pch=as.integer(tc))
#' year <- substr(levels(tc), 1, 4)
#' data(topoWorld)
#' contour(topoWorld[['longitude']], topoWorld[['latitude']],
#'         topoWorld[['z']], add=TRUE)
#' legend("bottomleft", pch=seq_along(year), legend=year, bg="white", cex=3/4)
#'
#' # Example 2: plot map, TS, T(z) and S(z). Note the use
#' # of handleFlags(), to skip over questionable data.
#' plot(handleFlags(argo), which=c(1, 4, 6, 5))
#'
#' @author Dan Kelley
#'
#' @family things related to argo data
#' @family functions that plot oce data
#' @aliases plot.argo
setMethod(f="plot",
    signature=signature("argo"),
    definition=function (x, which=1, level,
        coastline=c("best", "coastlineWorld", "coastlineWorldMedium",
            "coastlineWorldFine", "none"),
        cex=1, pch=1, type='p', col=1, fill=FALSE,
        projection=NULL,
        mgp=getOption("oceMgp"), mar=c(mgp[1]+1.5, mgp[1]+1.5, 1.5, 1.5),
        tformat,
        debug=getOption("oceDebug"),
        ...)
    {
        debug <- min(3L, max(0L, as.integer(debug)))
        if (!inherits(x, "argo"))
            stop("method is only for objects of class '", "argo", "'")
        oceDebug(debug, "plot.argo(x, which=c(", paste(which, collapse=","), "),",
            argShow(mgp),
            argShow(mar),
            argShow(cex),
            " ...) {\n", sep="", unindent=1, style="bold")
        coastline <- match.arg(coastline)
        nw  <- length(which)
        if (nw > 1)
            par(mfcol=c(1, nw))
        par(mgp=mgp, mar=mar)
        if (missing(level) || level == "all")
            level <- seq(1L, dim(x@data$temperature)[1])
        longitude <- x[["longitude"]]
        latitude <- x[["latitude"]]
        dim <- dim(x@data$salinity)
        if (length(longitude) < prod(dim)) {
            # Copy across depths. This is inside a conditional because possibly
            # argo[["longitude"]] should mimic section[["longitude"]], in doing
            # the lengthing by itself unless the second argument is "byStation"
            # (issue 1273 ... under consideration 2017jul12)
            longitude <- rep(x[["longitude"]], each=dim[1])
            latitude <- rep(x[["latitude"]], each=dim[1])
        }
        ctd <- as.ctd(x@data$salinity, x@data$temperature, x@data$pressure,
            longitude=longitude, latitude=latitude,
            units=list(temperature=list(unit=expression(degree*C), scale="ITS-90"),
                conductivity=list(list=expression(), scale=""))) # guess on units
        whichOrig <- which
        which <- oce.pmatch(which,
            list("trajectory"=1,
                "map"=1,
                "salinity ts"=2,
                "temperature ts"=3,
                "TS"=4,
                "salinity profile"=5,
                "temperature profile"=6))
        #if (any(is.na(which)))
        #    stop("In plot,argo-method() :\n  unrecognized value(s) of which: ", paste(whichOrig[is.na(which)], collapse=", "), call.=FALSE)
        lw <- length(which)
        par(mgp=mgp)
        if (lw == 2) {
            par(mfcol=c(2, 1))
        } else if (lw == 3) {
            par(mfcol=c(3, 1))
        } else if (lw == 4) {
            par(mfrow=c(2, 2))
        } else if (lw != 1) {
            nnn <- floor(sqrt(lw))
            par(mfcol=c(nnn, ceiling(lw/nnn)))
            rm(nnn)
        }
        for (w in 1:nw) {
            oceDebug(debug, "handling which[", w, "]=\"", whichOrig[w], "\"\n", sep="")
            if (is.na(which[w])) {
                oceDebug(debug, "not a special case, so passing 'which' to plot,ctd-method\n")
                plot(ctd, which=whichOrig[w], debug=debug-1, ...)
            } else if (which[w] == 1) {
                oceDebug(debug, "note: par(\"mfrow\") = ", paste(par("mfrow"), collapse=","), "\n")
                ## map
                ## FIXME: coastline selection should be DRY
                haveCoastline <- FALSE
                haveOcedata <- requireNamespace("ocedata", quietly=TRUE)
                lonr <- range(x[["longitude"]], na.rm=TRUE)
                latr <- range(x[["latitude"]], na.rm=TRUE)
                if (coastline == "best") {
                    if (haveOcedata) {
                        bestcoastline <- coastlineBest(lonRange=lonr, latRange=latr)
                        oceDebug(debug, " 'best' coastline is: \"", bestcoastline, '\"\n', sep="")
                        if (bestcoastline == "coastlineWorld") {
                            data(list=bestcoastline, package="oce", envir=environment())
                        } else {
                            data(list=bestcoastline, package="ocedata", envir=environment())
                        }
                        coastline <- get(bestcoastline)
                    } else {
                        bestcoastline <- coastlineBest(lonRange=lonr, latRange=latr)
                        oceDebug(debug, " using \"coastlineWorld\" because ocedata package not installed\n")
                        data("coastlineWorld", package="oce", envir=environment())
                        coastline <- get("coastlineWorld")
                    }
                    haveCoastline <- TRUE
                } else {
                    if (coastline != "none") {
                        if (coastline == "coastlineWorld") {
                            data("coastlineWorld", package="oce", envir=environment())
                            coastline <- get("coastlineWorld")
                        } else if (haveOcedata && coastline == "coastlineWorldFine") {
                            data("coastlineWorldFine", package="ocedata", envir=environment())
                            coastline <- get("coastlineWorldFine")
                        } else if (haveOcedata && coastline == "coastlineWorldMedium") {
                            data("coastlineWorldMedium", package="ocedata", envir=environment())
                            coastline <- get("coastlineWorldMedium")
                        }  else {
                            stop("there is no built-in coastline file of name \"", coastline, "\"")
                        }
                        haveCoastline <- TRUE
                    }
                }
                ## if (!is.character(coastline)) stop("coastline must be a character string")

                if (!is.null(projection)) {
                    oceDebug(debug, "drawing an argo map with a projection\n")
                    meanlat <- mean(x[['latitude']], na.rm=TRUE)
                    meanlon <- mean(x[['longitude']], na.rm=TRUE)
                    ## id <- pmatch(projection, "automatic")
                    if (!is.na(pmatch(projection, "automatic"))) {
                        projection <- if (meanlat > 70)
                            paste("+proj=stere +lon_0=", meanlon, sep="") else "+proj=merc"
                        oceDebug(debug, "using", projection, "projection (chosen automatically)\n")
                    } else {
                        oceDebug(debug, "using", projection, "projection (specified)\n")
                    }
                    mapPlot(x[["longitude"]], x[["latitude"]],
                        projection=projection,
                        type='p', cex=cex, pch=pch,
                        col=col,
                        debug=debug-1)
                    if (is.logical(fill) && fill) {
                        mapPolygon(coastline[['longitude']], coastline[['latitude']], col='lightgray')
                    } else {
                        if (is.character(fill)) {
                            mapPolygon(coastline[['longitude']], coastline[['latitude']], col=fill)
                        } else {
                            mapPolygon(coastline[['longitude']], coastline[['latitude']])
                        }
                    }
                } else {
                    oceDebug(debug, "drawing an argo map without a projection\n")
                    asp <- 1 / cos(mean(range(x@data$latitude, na.rm=TRUE)) * atan2(1, 1) / 45)
                    plot(x@data$longitude, x@data$latitude, asp=asp,
                        type=type, cex=cex, pch=pch,
                        col=col,
                        xlab=resizableLabel("longitude"), ylab=resizableLabel("latitude"), ...)

                    if (haveCoastline) {
                        if (!is.null(coastline@metadata$fillable) && coastline@metadata$fillable) {
                            polygon(coastline[["longitude"]], coastline[["latitude"]], col="lightgray", lwd=3/4)
                            polygon(coastline[["longitude"]]+360, coastline[["latitude"]], col="lightgray", lwd=3/4)
                        } else {
                            lines(coastline[["longitude"]], coastline[["latitude"]], col="darkgray")
                            lines(coastline[["longitude"]]+360, coastline[["latitude"]], col="darkgray")
                        }
                    }
                    if (!missing(coastline)) {
                        polygon(coastline[["longitude"]], coastline[["latitude"]], col='lightgray')
                        if (type[w] == 'l')
                            lines(x@data$longitude, x@data$latitude)
                        else
                            points(x@data$longitude, x@data$latitude, cex=cex, pch=pch, col=col)
                    }
                }
                par(mar=mar)
            } else if (which[w] == 2) {
                ## salinity timeseries
                if (0 != sum(!is.na(x@data$salinity))) {
                    nlevels <- dim(x@data$salinity)[1]
                    t <- if (length(level) > 1)
                        numberAsPOSIXct(t(matrix(rep(x@data$time, nlevels), byrow=FALSE, ncol=nlevels)))
                    else
                        x@data$time
                    oce.plot.ts(t, as.vector(x@data$salinity[level, ]),
                        ylab=resizableLabel("S", "y"),
                        cex=cex, pch=pch, col=col, type=type,
                        tformat=tformat)
                } else {
                    warning("no non-missing salinity data")
                }
            } else if (which[w] == 3) {
                ## temperature timeseries
                if (0 != sum(!is.na(x@data$temperature))) {
                    nlevels <- dim(x@data$temperature)[1]
                    t <- if (length(level) > 1)
                        numberAsPOSIXct(t(matrix(rep(x@data$time, nlevels), byrow=FALSE, ncol=nlevels)))
                    else
                        x@data$time
                    oce.plot.ts(t, x@data$temperature[level, ],
                        ylab=resizableLabel("T", "y"),
                        cex=cex, pch=pch, col=col, type=type,
                        tformat=tformat)
                } else {
                    warning("no non-missing temperature data")
                }
            } else if (which[w] == 4) {
                ## TS
                if (0 != sum(!is.na(x@data$temperature)) && 0 != sum(!is.na(x@data$salinity))) {
                    plotTS(ctd,
                        cex=cex, pch=pch, col=col, type=type,
                        debug=debug-1)
                } else {
                    warning("no non-missing salinity data")
                }
            } else if (which[w] == 5) {
                ## S profile
                ## FIXME: how to handle the noise; if as below, document it
                plotProfile(ctd, xtype="salinity",
                    Slim=quantile(x@data$salinity, c(0.01, 0.99), na.rm=TRUE),
                    ylim=quantile(x@data$pressure, c(0.99, 0.01), na.rm=TRUE),
                    cex=cex, pch=pch, col=col, type=type)
            } else if (which[w] == 6) {
                ## T profile
                ## FIXME: how to handle the noise; if as below, document it
                plotProfile(ctd, xtype="temperature",
                    Tlim=quantile(x@data$temperature, c(0.01, 0.99), na.rm=TRUE),
                    ylim=quantile(x@data$pressure, c(0.99, 0.01), na.rm=TRUE),
                    cex=cex, pch=pch, col=col, type=type)
            } else {
                stop("Unknown value of which=", which[w], "\n", call.=FALSE)
            }
        }
        oceDebug(debug, "} # plot.argo()\n", unindent=1, style="bold")
        invisible(NULL)
    })

## DEVELOPERS: please pattern functions and documentation on the 'ctd' code, for uniformity.
## DEVELOPERS: You will need to change the docs, and the 3 spots in the code
## DEVELOPERS: marked '# DEVELOPER 1:', etc.
#' @title Handle Flags in ARGO Objects
#'
#' @param object an [argo-class] object.
#'
#' @template handleFlagsTemplate
#'
#' @references
#' 1. Wong, Annie, Robert Keeley, Thierry Carval, and Argo Data Management Team.
#' "Argo Quality Control Manual for CTD and Trajectory Data," January 1, 2020.
#' `https://archimer.ifremer.fr/doc/00228/33951/`.
#'
#' @examples
#' library(oce)
#' data(argo)
#' argoNew <- handleFlags(argo)
#' # Demonstrate replacement, looking at the second profile
#' f <- argo[["salinityFlag"]][,2]
#' df <- data.frame(flag=f, orig=argo[["salinity"]][,2], new=argoNew[["salinity"]][,2])
#' df[11:15,] # notice line 13
#'
#' @author Dan Kelley
#'
#' @family things related to argo data
#' @aliases handleFlags.argo
setMethod("handleFlags", signature=c(object="argo", flags="ANY", actions="ANY", where="ANY", debug="ANY"),
          definition=function(object, flags=NULL, actions=NULL, where=NULL, debug=getOption("oceDebug")) {
              ## DEVELOPER 1: alter the next comment to explain your setup
              if (is.null(flags)) {
                  flags <- defaultFlags(object)
                  if (is.null(flags))
                      stop("must supply 'flags', or use initializeFlagScheme() on the argo object first")
              }
              if (is.null(actions)) {
                  actions <- list("NA") # DEVELOPER 3: alter this line to suit a new data class
                  names(actions) <- names(flags)
              }
              if (any(names(actions)!=names(flags)))
                  stop("names of flags and actions must match")
              handleFlagsInternal(object=object, flags=flags, actions=actions, where=where, debug=debug)
          })


#' Set Preference for Adjusted Values
#'
#' [argo-class] data can contain "adjusted" forms of data items,
#' which may be more trustworthy than the original
#' data, and `preferAdjusted` lets the user express a preference
#' for such adjusted data.  This means that using
#' \code{\link{[[,argo-method}} on the results returned by `preferAdjusted`
#' will (if possible) return adjusted data, and also use those adjusted
#' data in computations of derived quantities such as Absolute Salinity.
#' The preference applies also to units and to data-quality flags,
#' both of which can be returned by \code{\link{[[,argo-method}}, as
#' discussed in \dQuote{Details}.
#'
#' `preferAdjusted()` merely sets two items in the `metadata` slot of the
#' returned [argo-class] object. The real action is carried out by
#' \code{\link{[[,argo-method}} but, for convenience, the details are explained here.
#'
#' Consider salinity, for example.
#' If `which` equals `"all"`, or if it is a character
#' vector containing `"salinity"`, then using
#' \code{\link{[[,argo-method}} on the returned object
#' will yield the adjusted forms of the salinity data,
#' its associated flags, or its units.  Thus, in the salinity
#' case,
#' * `argo[["salinity"]]` will attempt to return `argo@data$salinityAdjusted`
#' instead of returning `argo@data$salinity`, although if the adjusted values
#' are all `NA` then, depending on the value of `fallback`, the
#' unadjusted values may be returned; similarly
#' * `argo[["salinityFlags"]]` will attempt to return
#' `argo@metadata$flags$salinityAdjusted`
#' instead of `argo@metadata$flags$salinity`, and
#' * `argo[["salinityUnits"]]` will attempt to return
#' `argo@metadata$units$salinityAdjusted`
#' instead of `argo@metadata$units$salinity`.
#'
#' The default value, `which="all"`, indicates that this
#' preference for adjusted values will apply to all the
#' elements of the `data` slot of the returned object, along
#' with associated flags and units. This can be handy for quick
#' work, but analysts may also choose to restrict their use of
#' adjusted values to a subset of variables, based on their own
#' decisions about data quality or accuracy.
#'
#' The default value `fallback=TRUE` indicates that later calls to
#' \code{\link{[[,argo-method}} should return unadjusted values for any
#' data items that have `NA` for all the adjusted values.  This
#' condition is rare for core variables (salinity, temperature and
#' pressure) but is annoyingly common for biogeochemical variables; see
#' e.g. Section 2.2.5 of Reference 1 for a discussion of
#' the conditions under which Argo netcdf files contain
#' adjusted values. Setting `fallback=FALSE` means that adjusted
#' values (if they exist) will always be returned, even if they
#' are a useless collection of `NA` values.
#'
#' Error fields, such as `salinityAdjustedError`, are returned
#' as-is by \code{\link{[[,argo-method}}, regardless of whether
#' the object was created by `preferAdjusted`.
#'
#' It should be noted that, regardless of whether `preferAdjusted`
#' has been used, the analyst can always access either unadjusted
#' or adjusted data directly, using the original variable names stored
#' in the source netcdf file.  For example, `argo[["PSAL"]]`
#' yields unadjusted salinity values, and
#' `argo[["PSAL_ADJUSTED"]]` yields adjusted values (if they exist, or
#' `NULL` if they do not).
#' Similarly, adjusted value can always be obtained by using a form
#' like `argo[["salinityAdjusted"]]`.
#'
#' @param argo An [argo-class] object.
#'
#' @param which A character vector naming the items for which
#' (depending also on the value of `fallback`) adjusted values
#' are to be sought by future calls to \code{\link{[[,argo-method}}.
#' The short names are used, e.g. `which="oxygen"` means that
#' adjusted oxygen is to be returned in future calls
#' such as `argo[["oxygen"]]`.  The default,
#' `"all"`, means to  use adjusted values for any item in `argo`
#' that has adjusted values.
#'
#' @param fallback A logical value indicating whether to fall back
#' to unadjusted values for any data field in which the
#' adjusted values are all `NA`.  The default value, `TRUE`,
#' avoids a problem with biogeochemical fields, where adjustment
#' of any one field may lead to insertion of "adjusted" values for
#' other fields that consist of nothing more than `NA`s.
#'
#' @return An [argo-class] object its `metadata` slot altered
#' (in its `adjustedWhich` and `adjustedFallback` elements)
#' as a signal for how \code{\link{[[,argo-method}} should
#' function on the object.
#'
#' @examples
#' library(oce)
#' data(argo)
#' argoAdjusted <- preferAdjusted(argo)
#' all.equal(argo[["salinityAdjusted"]], argoAdjusted[["salinity"]])
#' all.equal(argo[["salinityFlagsAdjusted"]], argoAdjusted[["salinityFlags"]])
#' all.equal(argo[["salinityUnitsAdjusted"]], argoAdjusted[["salinityUnits"]])
#'
#' @references
#' 1. Argo Data Management Team. "Argo User's Manual V3.3." Ifremer,
#' November 28, 2019.
#' \doi{10.13155/29825}
#'
#' @author Dan Kelley, based on discussions with Jaimie Harbin (with
#' respect to the \code{\link{[[,argo-method}} interface) and Clark Richards
#' (with respect to storing the preference in the `metadata` slot).
preferAdjusted <- function(argo, which="all", fallback=TRUE)
{
    if (!inherits(argo, "argo"))
        stop("'argo' must be an oce 'argo' object")
    if (!is.logical(fallback))
        stop("fallback must be a logical value")
    argo@metadata$adjustedFallback <- fallback
    argo@metadata$adjustedWhich <- which
    argo
}

#' Convert time to Argo Julian Day (juld)
#'
#' This converts a POSIXct time into an Argo julian day, with the convention
#' that juld=0 at 1950-01-01.
#'
#' @param t A POSIXct time or a string that can be converted to a POSIXct time
#'
#' @examples
#' timeToArgoJuld("2020-07-01")
#'
#' @author Jaimie Harbin and Dan Kelley
timeToArgoJuld <- function(t)
    oce::julianDay(as.POSIXct(t, tz='UTC')) - oce::julianDay(as.POSIXct("1950-01-01", tz="UTC"))

#' Convert Argo Julian Day (juld) to time
#'
#' @param jday A numerical value indicating the julian day in the Argo convention,
#' with day=0 at 1950-01-01.
#'
#' @examples
#' argoJuldToTime(25749)
#'
#' @author Jaimie Harbin and Dan Kelley
argoJuldToTime <- function(jday)
    as.POSIXct("1950-01-01", tz="UTC") + jday*86400

