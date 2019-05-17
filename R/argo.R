# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Class to Store Argo Data
#'
#' This class stores data from Argo floats.
#'
#' An \code{argo} object may be read with \code{\link{read.argo}} or
#' created with \code{\link{as.argo}}.  Argo data can be gridded to constant
#' pressures with \code{\link{argoGrid}} or subsetted with
#' \code{\link{subset,argo-method}}.  Plots can be made with
#' \code{\link{plot,argo-method}}, while \code{\link{summary,argo-method}}
#' produces statistical summaries and \code{show} produces overviews.
#'
#' See \url{http://www.argo.ucsd.edu/Gridded_fields.html} for some
#' argo-related datasets that may be useful in a wider context.
#'
#' @templateVar class argo
#'
#' @templateVar dataExample The key items stored in this slot include  equal-length vectors \code{time}, \code{longitude}, \code{latitude} and equal-dimension matrices \code{pressure}, \code{salinity}, and \code{temperature}.
#'
#' @templateVar metadataExample Examples that are of common interest include \code{id}, a vector of ID codes for the profiles, and \code{dataMode}, a vector of strings indicating whether the profile is in archived mode (\code{"A"}), realtime mode (\code{"R"}), or delayed mode (\code{"D"}).
#'
#' @template slot_summary
#'
#' @template slot_put
#'
#' @template slot_get
#'
#' @author Dan Kelley and Clark Richards
#'
#' @family classes provided by \code{oce}
#' @family things related to \code{argo} data
setClass("argo", contains="oce")

#' ARGO float dataset
#'
#' This holds data from ARGO 6900388 in the North Atlantic.
#'
#' To quote Argo's website: "These data were collected and made freely
#' available by the International Argo Program and the national programs
#' that contribute to it.  (http//www.argo.ucsd.edu,
#' http://argo.jcommops.org).  The Argo Program is part of the
#' Global Ocean Observing System."
#'
#' Below is the official citation (note that this DOI has web links for
#' downloads):
#' Argo (2017). Argo float data and metadata from Global Data Assembly Centre
#' (Argo GDAC) - Snapshot of Argo GDAC of July, 8st 2017. SEANOE.
#' \url{http://doi.org/10.17882/42182#50865}
#'
#' @name argo
#' @docType data
#'
#' @examples
#'\donttest{
#' library(oce)
#' data(argo)
#' summary(argo)
#' data(coastlineWorld)
#' plot(argo, which="trajectory")
#'}
#'
#' @source This file was downloaded using the unix command
#'\preformatted{
#' ftp ftp://ftp.ifremer.fr/ifremer/argo/dac/bodc/6900388/6900388_prof.nc
#'} issued on 2017 July 7.
#'
#' @family datasets provided with \code{oce}
#' @family things related to \code{argo} data
NULL


#' @title Extract Something From an Argo Object
#' @param x An \code{argo} object, i.e. one inheriting from \code{\link{argo-class}}.
#'
#' @templateVar class argo
#'
#' @section Details of the specialized \code{argo} method:
#'
#' There are several possibilities, depending on the nature of \code{i}.
#' Note that all of these calculations are done with
#' \code{salinityAdjusted}, if that is present, or with \code{salinity}
#' otherwise, and similar for temperature and pressure.
#'
#'\itemize{
#'
#' \item If \code{i} is \code{"profile"} and \code{j} is an integer vector,
#' then an argo object is returned, as specified by \code{j}. For example,
#' \code{argo[["profile", 2:5]]} is equivalent to
#' \code{subset(argo, profile \%in\% 2:5)}.
#'
#' \item If \code{i} is \code{"CT"}, then
#' Conservative Temperature is returned, as computed with
#' \code{\link[gsw]{gsw_CT_from_t}(SA, t, p)}, where
#' first \code{SA} is computed as explained
#' in the next item, \code{t} is in-situ temperature,
#' and \code{p} is pressure.
#'
#' \item If \code{i} is \code{"N2"}, then
#' the square of buoyancy is returned, as computed with
#' \code{\link{swN2}}.
#'
#' \item If \code{i} is \code{"SA"}, then
#' Absolute Salinity is returned, as computed with
#' \code{\link[gsw]{gsw_SA_from_SP}}.
#'
#' \item If \code{i} is \code{"sigmaTheta"}, then
#' potential density anomaly (referenced to zero
#' pressure) is computed, with \code{\link{swSigmaTheta}}, where the
#' equation of state is taken to be
#' \code{\link{getOption}("oceEOS", default="gsw")}.
#'
#' \item If \code{i} is \code{"theta"}, then
#' potential temperature (referenced to zero
#' pressure) is computed, with \code{\link{swTheta}}, where the
#' equation of state is taken to be
#' \code{\link{getOption}("oceEOS", default="gsw")}.
#'
#' \item If \code{i} is \code{"depth"}, then
#' a matrix of depths is returned.
#'
#' \item If \code{i} is in the \code{data} slot of \code{x},
#' then it is returned, otherwise if it is in the \code{metadata} slot,
#' then that is returned, otherwise \code{NULL} is returned.
#'
#'}
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
#' @family things related to \code{argo} data
#' @author Dan Kelley
setMethod(f="[[",
          signature(x="argo", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              res <- NULL
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
              if (i %in% c("CT", "N2", "SA", "sigmaTheta", "theta")) {
                  ## FIXME: should we prefer e.g. salinityAdjusted or salinity?
                  names <- names(x@data)
                  salinity <- x@data[[if ("salinityAdjusted" %in% names) "salinityAdjusted" else "salinity"]]
                  pressure <- x@data[[if ("pressureAdjusted" %in% names) "pressureAdjusted" else "pressure"]]
                  temperature <- x@data[[if ("temperatureAdjusted" %in% names) "temperatureAdjusted" else "temperature"]]
                  dim <- dim(salinity)
                  ## Do not need longitude and latitude if eos="unesco", but retain for code clarity
                  longitude <- rep(x@data$longitude, each=dim[1])
                  latitude <- rep(x@data$latitude, each=dim[1])
                  if (i == "CT") {
                      res <- gsw_CT_from_t(x[["SA"]], temperature, pressure)
                  } else if (i == "N2") {
                      nprofile <- dim[2]
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
                  } else if (i == "SA") {
                      res <- gsw_SA_from_SP(salinity, pressure, longitude=longitude, latitude=latitude)
                  } else if (i == "sigmaTheta") {
                      res <- swSigmaTheta(salinity, temperature=temperature, pressure=pressure,
                                          referencePressure=0, longitude=longitude, latitude=latitude,
                                          eos=getOption("oceEOS", default="gsw"))
                  } else if (i == "theta") {
                      res <- swTheta(salinity, temperature=temperature, pressure=pressure,
                                     referencePressure=0, longitude=longitude, latitude=latitude,
                                     eos=getOption("oceEOS", default="gsw"))
                  } else {
                      stop("coding error: unknown item '", i, "'")
                  }
                  dim(res) <- dim
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
              } else if (i == "latitude") {
                  res <- x@data$latitude
              } else if (i == "longitude") {
                  res <- x@data$longitude
              } else {
                  res <- callNextMethod()         # [[
              }
              res
          })

#' @title Replace Parts of an Argo Object
#' @param x An \code{argo} object, i.e. inheriting from \code{\link{argo-class}}
#' @template sub_subsetTemplate
#' @family things related to \code{argo} data
setMethod(f="[[<-",
          signature(x="argo", i="ANY", j="ANY"),
          definition=function(x, i, j, ..., value) {
              callNextMethod(x=x, i=i, j=j, ...=..., value=value) # [[<-
          })

setMethod(f="initialize",
          signature="argo",
          definition=function(.Object, time, id, longitude, latitude, salinity, temperature, pressure, filename, dataMode) {
              if (!missing(time)) .Object@data$time <- time
              if (!missing(id)) .Object@metadata$id <- id
              if (!missing(longitude)) .Object@data$longitude <- longitude
              if (!missing(latitude)) .Object@data$latitude <- latitude
              if (!missing(salinity)) .Object@data$salinity <- salinity
              if (!missing(temperature)) .Object@data$temperature <-temperature
              if (!missing(pressure)) .Object@data$pressure <- pressure
              .Object@metadata$filename <- if (missing(filename)) "" else filename
              .Object@metadata$dataMode <- if (missing(dataMode)) "" else dataMode
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'argo' object"
              return(.Object)
          })

maybeLC <- function(s, lower)
    if (lower) tolower(s) else s

getData <- function(file, name) # a local function -- no need to pollute namesapce with it
{
    capture.output(res <- try(ncdf4::ncvar_get(file, name), silent=TRUE))
    if (inherits(res, "try-error")) {
        warning(file$filename, " has no variable named '", name, "'\n", sep='')
        res <- NULL
    }
    if (is.array(res) && 1 == length(dim(res))) res <- matrix(res) else res
}

#' Convert Argo Data Name to Oce Name
#'
#' This function is used internally by \code{\link{read.argo}} to convert Argo-convention
#' data names to oce-convention names. Users should not call this directly, since
#' its return value may be changed at any moment (e.g. to include units as well
#' as names).
#'
#'
#' The inference of names was done
#' by inspection of some data files, using [1] as a reference. It should be noted,
#' however, that the data files examined contain some names that are not
#' undocumented in [1], and others that are listed only in its changelog,
#' with no actual definitions being given. For example, the files had six distinct
#' variable names that seem to relate to phase in the oxygen sensor, but
#' these are not translated by the present function because these
#' variable names are not defined in [1], or not defined uniquely
#' in [2].
#'
#' The names are converted with
#' \code{\link{gsub}}, using the \code{ignore.case} argument of the present
#' function.
#' The procedure
#' is to first handle the items listed in the following table, with string
#' searches anchored to the start of the string. After that,
#' the qualifiers
#' \code{_ADJUSTED}, \code{_ERROR} and \code{_QC},
#' are translated to \code{Adjusted}, \code{Error}, and \code{QC}, respectively.
#' \tabular{ll}{
#' \strong{Argo name} \tab \strong{oce name}\cr
#' \code{BBP} \tab \code{bbp}\cr
#' \code{BETA_BACKSCATTERING} \tab \code{betaBackscattering}\cr
#' \code{BPHASE_OXY} \tab \code{bphaseOxygen}\cr
#' \code{CDOM} \tab \code{CDOM}\cr
#' \code{CNDC} \tab \code{conductivity}\cr
#' \code{CHLA} \tab \code{chlorophyllA}\cr
#' \code{CP} \tab \code{beamAttenuation}\cr
#' \code{CYCLE_NUMBER} \tab \code{cycleNumber}\cr
#' \code{DATA_CENTRE} \tab \code{dataCentre}\cr
#' \code{DATA_MODE} \tab \code{dataMode}\cr
#' \code{DATA_STATE_INDICATOR} \tab \code{dataStateIndicator}\cr
#' \code{DC_REFERENCE} \tab \code{DCReference}\cr
#' \code{DIRECTION} \tab \code{direction}\cr
#' \code{DOWN_IRRADIANCE} \tab \code{downwellingIrradiance}\cr
#' \code{DOWNWELLING_PAR} \tab \code{downwellingPAR}\cr
#' \code{FIRMWARE_VERSION} \tab \code{firmwareVersion}\cr
#' \code{FIT_ERROR_NITRATE} \tab \code{fitErrorNitrate}\cr
#' \code{FLUORESCENCE_CDOM} \tab \code{fluorescenceCDOM}\cr
#' \code{FLUORESCENCE_CHLA} \tab \code{fluorescenceChlorophyllA}\cr
#' \code{INST_REFERENCE} \tab \code{instReference}\cr
#' \code{JULD} \tab \code{juld} (and used to compute \code{time})\cr
#' \code{JULD_QC_LOCATION} \tab \code{juldQCLocation}\cr
#' \code{LATITUDE} \tab \code{latitude}\cr
#' \code{LONGITUDE} \tab \code{longitude}\cr
#' \code{MOLAR_DOXY} \tab \code{oxygenUncompensated}\cr
#' \code{PH_IN_SITU_FREE} \tab \code{pHFree}\cr
#' \code{PH_IN_SITU_TOTAL} \tab \code{pH}\cr
#' \code{PI_NAME} \tab \code{PIName}\cr
#' \code{PLATFORM_NUMBER} \tab \code{id}\cr
#' \code{POSITION_ACCURACY} \tab \code{positionAccuracy}\cr
#' \code{POSITIONING_SYSTEM} \tab \code{positioningSystem}\cr
#' \code{PROFILE} \tab \code{profile}\cr
#' \code{PROJECT_NAME} \tab \code{projectName}\cr
#' \code{RAW_DOWNWELLING_IRRADIANCE} \tab \code{rawDownwellingIrradiance}\cr
#' \code{RAW_DOWNWELLING_PAR} \tab \code{rawDownwellingPAR}\cr
#' \code{RAW_UPWELLING_RADIANCE} \tab \code{rawUpwellingRadiance}\cr
#' \code{STATION_PARAMETERS} \tab \code{stationParameters}\cr
#' \code{TEMP} \tab \code{temperature}\cr
#' \code{TEMP_CPU_CHLA} \tab \code{temperatureCPUChlorophyllA}\cr
#' \code{TEMP_DOXY} \tab \code{temperatureOxygen}\cr
#' \code{TEMP_NITRATE} \tab \code{temperatureNitrate}\cr
#' \code{TEMP_PH} \tab \code{temperaturePH}\cr
#' \code{TEMP_SPECTROPHOTOMETER_NITRATE} \tab \code{temperatureSpectrophotometerNitrate}\cr
#' \code{TILT} \tab \code{tilt}\cr
#' \code{TURBIDITY} \tab \code{turbidity}\cr
#' \code{UP_RADIANCE} \tab \code{upwellingRadiance}\cr
#' \code{UV_INTENSITY} \tab \code{UVIntensity}\cr
#' \code{UV_INTENSITY_DARK_NITRATE} \tab \code{UVIntensityDarkNitrate}\cr
#' \code{UV_INTENSITY_NITRATE} \tab \code{UVIntensityNitrate}\cr
#' \code{VRS_PH} \tab \code{VRSpH}\cr
#' \code{WMO_INST_TYPE} \tab \code{WMOInstType}\cr
#'}
#'
#' @param names vector of character strings containing names in the Argo convention.
#' @param ignore.case a logical value passed to \code{\link{gsub}}, indicating whether to
#' ignore the case of input strings. The default is set to \code{TRUE} because some data
#' files use lower-case names, despite the fact that the Argo documentation specifies
#' upper-case.
#' @return A character vector of the same length as \code{names}, but with
#' replacements having been made for all known quantities.
#'
#' @references
#' 1. Argo User's Manual Version 3.2, Dec 29th, 2015, available at
#' \url{http://archimer.ifremer.fr/doc/00187/29825/40575.pdf}
#' (but note that this is a draft; newer versions may have
#' replaced this by now).
#'
#' 2. Argo list of parameters in an excel spreadsheet, available at
#' \url{http://www.argodatamgt.org/content/download/27444/187206/file/argo-parameters-list-core-and-b.xlsx}
#'
#' @family things related to \code{argo} data
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
#' If \code{subset} is the string \code{"adjusted"}, then \code{subset}
#' replaces the station variables with their adjusted counterparts. In
#' the argo notation, e.g. \code{PSAL} is replaced with \code{PSAL_ADJUSTED};
#' in the present notation, this means that \code{salinity} in the \code{data}
#' slot is replaced with \code{salinityAdjusted}, and the latter is deleted.
#' Similar replacements are also done with the flags stored in the \code{metadata}
#' slot.
#'
#' If \code{subset} is an expression, then the action is somewhat similar
#' to other \code{subset} functions, but with the restriction that
#' only one independent variable may be
#' used in in any call to the function, so that
#' repeated calls will be necessary to subset based on more than one
#' independent variable.  Subsetting may be done by anything
#' stored in the data, e.g. \code{time},
#' \code{latitude}, \code{longitude}, \code{profile}, \code{dataMode},
#' or \code{pressure} or by \code{profile} (a made-up variable)
#' or \code{id} (from the \code{metadata} slot).
#'
#' @param x An \code{argo} object, i.e. one inheriting from \code{\link{argo-class}}.
#'
#' @param subset An expression indicating how to subset \code{x}.
#'
#' @param ... optional arguments, of which only the first is examined. The
#' only possibility is \code{within}, a polygon enclosing data to be
#' retained. This must be either a list or data frame, containing items
#' named either \code{x} and \code{y} or \code{longitude} and
#' \code{latitude}; see Example 4.  If \code{within} is given,
#' then \code{subset} is ignored.
#'
#' @return An argo object.
#'
#' @examples
#' library(oce)
#' data(argo)
#'
#' # Example 1: buset by time, longitude, and pressure
#' par(mfrow=c(2,2))
#' plot(argo)
#' plot(subset(argo, time > mean(time)))
#' plot(subset(argo, longitude > mean(longitude)))
#' plot(subset(argoGrid(argo), pressure > 500 & pressure < 1000), which=5)
#'
#' # Example 2: restrict attention to delayed-mode profiles.
#' par(mfrow=c(1, 1))
#' plot(subset(argo, dataMode == "D"))
#'
#' # Example 3: contrast corrected and uncorrected data
#' par(mfrow=c(1,2))
#' plotTS(argo)
#' plotTS(subset(argo, "adjusted"))
#'
#' # Example 4. Subset by a polygon determined with locator()
#'\dontrun{
#' par(mfrow=c(2, 1))
#' plot(argo, which="map")
#' bdy <- locator(4) # Click the mouse on 4 boundary points
#' argoSubset <- subset(argo, within=bdy)
#' plot(argoSubset, which="map")
#'}
#'
#' @author Dan Kelley
#'
#' @family things related to \code{argo} data
#' @family functions that subset \code{oce} objects
setMethod(f="subset",
          signature="argo",
          definition=function(x, subset, ...) {
              subsetString <- paste(deparse(substitute(subset)), collapse=" ")
              res <- x
              dots <- list(...)
              dotsNames <- names(dots)
              withinGiven <- length(dots) && ("within" %in% dotsNames)
              debug <- getOption("oceDebug")
              if (length(dots) && ("debug" %in% names(dots)))
                  debug <- dots$debug
              if (withinGiven) {
                  oceDebug(debug, "subsetting with 'within' method")
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
                      stop("cannot use 'within' becaue the 'sp' package is not installed")
                  }
                  ## Metadata
                  for (name in names(x@metadata)) {
                      oceDebug(debug, "subsetting metadata item named '", name, "'\n", sep="")
                      ## Pass oce-generated things through directly.
                      if (name %in% c("units", "flags", "filename", "flagScheme", "dataNamesOriginal")) {
                          ##.message("  ... special case, so passed directly")
                          next
                      }
                      item <- x@metadata[[name]]
                      ## Handle things that are encoded as characters in a string,
                      ## namely 'direction', 'juldQc', and 'positionQc'.
                      if (is.character(item) && length(item) == 1) {
                          ##.message("'", name, "' is character-encoded")
                          res@metadata[[name]] <- paste(strsplit(item,"")[[1]][keep],collapse="")
                          ##.message(" ... ok")
                      } else if (is.vector(name)) {
                          ##.message("'", name, "' is a vector")
                          res@metadata[[name]] <- item[keep]
                          ##.message(" ... ok")
                      } else if (is.matrix(name)) {
                          ##.message("'", name, "' is a matrix")
                          res@metadata[[name]] <- item[, keep]
                          ##.message(" ... ok")
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
                  if (is.character(substitute(subset))) {
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
                  subsetString <- paste(deparse(substitute(subset)), collapse=" ")
                  res <- x
                  if (length(grep("time", subsetString)) ||
                      length(grep("longitude", subsetString)) || length(grep("latitude", subsetString))) {
                      keep <- eval(substitute(subset), x@data, parent.frame(2))
                  } else if (length(grep("id", subsetString))) {
                      ## add id into the data, then do as usual
                      tmp <- x@data
                      tmp$id <- x@metadata$id
                      keep <- eval(substitute(subset), tmp, parent.frame(2))
                      rm(tmp)
                  } else if (length(grep("profile", subsetString))) {
                      ## add profile into the data, then do as usual
                      tmp <- x@data
                      tmp$profile <- seq_along(x@data$time)
                      keep <- eval(substitute(subset), tmp, parent.frame(2))
                      rm(tmp)
                  } else if (length(grep("pressure", subsetString))) {
                      ## check that it is a "gridded" argo
                      gridded <- ifelse(all(apply(x@data$pressure, 1, diff) == 0, na.rm=TRUE), TRUE, FALSE)
                      if (gridded) {
                          x@data$pressure <- x@data$pressure[, 1] ## FIXME: have to convert pressure to vector
                          keep <- eval(substitute(subset), x@data, parent.frame(2))
                          x@data$pressure <- res@data$pressure ## FIXME: convert back to original for subsetting below
                      } else {
                          stop("cannot subset ungridded argo by pressure -- use argoGrid() first", call.=FALSE)
                      }
                  } else if (length(grep("dataMode", subsetString))) {
                      keep <- eval(substitute(subset), x@metadata, parent.frame(2))
                  } else {
                      stop("can only subset by time, longitude, latitude, pressure, dataMode, and not by combinations", call.=FALSE)
                  }
                  ## Now do the subset
                  if (length(grep("pressure", subsetString))) {
                      fieldname <- names(x@data)
                      for (field in fieldname) {
                          if (field != 'time' & field != 'longitude' & field != 'latitude') { # DEBUG: see issue 1327
                              ifield <- which(field == fieldname)
                              ##debug message("ifield=", ifield, ", field=", field,
                              ##debug        "\n\tlength(keep)=", length(keep),
                              ##debug        "\n\tsum(keep)=", sum(keep))
                              if (is.matrix(res@data[[ifield]])) {
                                  ##debug message("\tdim(x@data[[ifield]])=", paste(dim(x@data[[ifield]]), collapse=","))
                                  res@data[[ifield]] <- res@data[[ifield]][keep,]
                                  ##debugmessage("\tdim(res@data[[ifield]])=", paste(dim(res@data[[ifield]]), collapse=","))
                              } else {
                                  ##debug message("\tlength(x@data[[ifield]])=", length(x@data[[ifield]]))
                                  res@data[[ifield]] <- res@data[[ifield]][keep]
                                  ##debug message("\tlength(res@data[[ifield]])=", length(res@data[[ifield]]))
                              }
                          }
                      }
                      fieldname <- names(x@metadata$flags)
                      for (field in fieldname) {
                          ifield <- which(field == fieldname)
                          res@metadata$flags[[ifield]] <- res@metadata$flags[[ifield]][keep, ]
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
                                  x@data[[ifield]][, keep] else x@data[[ifield]][keep]
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
              res
          })


#' Summarize an Argo Object
#'
#' @description Summarizes some of the data in an \code{argo} object.
#'
#' @details Pertinent summary information is presented.
#' @param object}{an object of class \code{"argo"}, usually, a result of a
#'     call to \code{\link{read.argo}}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A matrix containing statistics of the elements of the \code{data} slot.
#' @examples
#' library(oce)
#' data(argo)
#' summary(argo)
#'
#' @author Dan Kelley
#' @family things related to \code{argo} data
setMethod(f="summary",
          signature="argo",
          definition=function(object, ...) {
              cat("Argo Summary\n------------\n\n")
              showMetadataItem(object, "filename",                  "Source:              ", quote=TRUE)
              nid <- length(unique(object@metadata$id))
              if (1 == nid)
                   cat("* id:                  \"", object@metadata$id[1], "\"\n", sep="")
              else cat("* id list:             \"", object@metadata$id[1], "\", \"", object@metadata$id[2], "\", ...\n", sep="")
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
#' The gridding is done with \code{\link{approx}}.  If there is
#' sufficient user demand, other methods may be added, by analogy to
#' \code{\link{sectionGrid}}.
#'
#' @template flagDeletionTemplate
#'
#' @param argo A \code{argo} object to be gridded.
#'
#' @param p Optional indication of the pressure levels to which interpolation
#' should be done.  If this is not supplied, the pressure levels will be
#' calculated based on the existing values, using medians. If \code{p="levitus"},
#' then pressures will be set to be those of the Levitus atlas, given by
#' \code{\link{standardDepths}}, trimmed to the maximum pressure in \code{argo}.
#' If \code{p} is a single numerical value, it is taken as the number of
#' subdivisions to use in a call to \code{\link{seq}} that has range from 0 to the
#' maximum pressure in \code{argo}.  Finally, if a vector numerical values is
#' provided, then it is used as is.
#'
#' @param debug A flag that turns on debugging.  Higher values provide deeper
#' debugging.
#'
#' @param ... Optional arguments to \code{\link{approx}}, which is used to do the
#' gridding.
#'
#' @return An object of \code{\link{argo-class}} that contains a pressure matrix
#' with constant values along the first index.
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
#' @family things related to \code{argo} data
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
#' \code{read.argo} is used to read an Argo file, producing an object of type
#' \code{argo}. The file must be in the ARGO-style NetCDF format described at
#' in the Argo documentation [2,3].
#'
#' @details
#'
#' Items are inferred from the data file in a straightforward way, using
#' \code{\link[ncdf4]{ncvar_get}}, converting from one-column matrices
#' to vectors, and trimming leading and trailing blank space in character
#' values, using \code{\link{trimString}}.
#'
#' Items are renamed from the argo ('snake case') forms to oce ('camel
#' case') forms with \code{\link{argoNames2oceNames}}. For example,
#' \code{FIRMWARE_VERSION} in the data file is renamed as
#' \code{firmwareVersion} in the return value.
#' Note that some files use upper-case for items, while other files
#' use lower-case for the same things; \code{read.argo} attempts
#' to ignore this variation.
#'
#' See the Argo documentation [2,3] for some details on what files contain.
#' Many items listed in section 2.2.3 of [3] are read from the
#' file and stored in the \code{metadata} slot, with the exception of
#' \code{longitude} and \code{latitude}, which are stored in the
#' \code{data} slot, alongside hydrographic information.
#'
#' The following global attributes stored within the netcdf file are stored in the
#' \code{metadata} slot: \code{title}, \code{institution}, \code{source},
#' \code{history}, \code{references}, \code{userManualVersion}, \code{conventions},
#' and \code{featureType}. These names are identical to those in the netcdf
#' file, except that \code{userManualVersion} is named
#' \code{user_manual_version} in the file, and \code{conventions} is
#' named \code{Conventions} in the file.
#'
#' It is assumed that the profile data are as listed in the NetCDF variable
#' called \code{STATION_PARAMETERS}. Each item can have variants, as
#' described in Sections 2.3.4 of [3].
#' For example, if \code{"PRES"} is found in \code{STATION_PARAMETERS},
#' then \code{PRES} (pressure) data are sought in the file, along with
#' \code{PRES_QC}, \code{PRES_ADJUSTED}, \code{PRES_ADJUSTED_QC}, and
#' \code{PRES_ERROR}. The same pattern works for other profile data. The variables
#' are stored with different names within the resultant \code{\link{argo-class}}
#' object, to match with \code{oce} conventions. Thus, \code{PRES} gets renamed
#' \code{pressure}, while \code{PRES_ADJUSTED} gets renamed \code{pressureAdjusted},
#' and \code{PRES_ERROR} gets renamed \code{pressureError}; all of these are
#' stored in the \code{data} slot. Meanwhile, the quality-control flags
#' \code{PRES_QC} and \code{PRES_ADJUSTED_QC} are stored as \code{pressure}
#' and \code{pressureAdjusted} in the \code{metadata$flags} slot.
#'
#' Once a predefined series of items are inferred and stored in either the
#' \code{metadata} or \code{data} slot, \code{read.argo} then reads all
#' non-empty variables in the file, storing them in the \code{metadata}
#' slot, using with the original ('snake case') name that is used in
#' the data file. (Note that the sample dataset accessed with \code{data(argo)}
#' lacks metadata items with names starting with \code{HISTORY_}, because
#' those are zero-length in the source file.)
#'
#' @param file a character string giving the name of the file to load.
#'
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate amount
#' of debugging information, or to 2 to get more.
#'
#' @param processingLog if provided, the action item to be stored in the log.
#' (Typically only provided for internal calls; the default that it provides is
#' better for normal calls by a user.)
#'
#' @param ... additional arguments, passed to called routines.
#'
#' @return
#' An object of \code{\link{argo-class}}.
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
#' url <- "http://www.usgodae.org/ftp/outgoing/argo"
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
#' The documentation for \code{\link{argo-class}} explains the structure of argo
#' objects, and also outlines the other functions dealing with them.
#'
#' @references
#' 1. \url{http://www.argo.ucsd.edu/}
#'
#' 2. Argo User's Manual Version 3.2, Dec 29th, 2015, available at
#' \url{https://archimer.ifremer.fr/doc/00187/29825/40575.pdf}
#' (but note that this is a draft; newer versions may have
#' replaced this by now).
#'
#' 3. User's Manual (ar-um-02-01) 13 July 2010, available at
#' \url{http://www.argodatamgt.org/content/download/4729/34634/file/argo-dm-user-manual-version-2.3.pdf},
#' which is the main document describing argo data.
#'
#' @section Data sources:
#' Argo data are made available at several websites. A bit of detective
#' work can be required to track down the data.
#'
#' Some servers provide  data for floats that surfaced in a given ocean
#' on a given day, the anonymous FTP server
#' \url{ftp://usgodae.org/pub/outgoing/argo/geo/} being an example.
#'
#' Other servers provide data on a per-float basis. A complicating
#' factor is that these data tend to be categorized by "dac" (data
#' archiving centre), which makes it difficult to find a particular
#' float. For example,
#' \url{http://www.usgodae.org/ftp/outgoing/argo/} is the top level of
#' a such a repository. If the ID of a float is known but not the
#' "dac", then a first step is to download the text file
#' \url{http://www.usgodae.org/ftp/outgoing/argo/ar_index_global_meta.txt}
#' and search for the ID. The first few lines of that file are header,
#' and after that the format is simple, with columns separated by slash
#' (\code{/}). The dac is in the first such column and the float ID in the
#' second. A simple search will reveal the dac.
#' For example \code{data(argo)} is based on float 6900388, and the line
#' containing that token is
#' \code{bodc/6900388/6900388_meta.nc,846,BO,20120225005617}, from
#' which the dac is seen to be the British Oceanographic Data Centre
#' (\code{bodc}). Armed with that information, visit
#' \url{http://www.usgodae.org/ftp/outgoing/argo/dac/bodc/6900388}
#' and see a directory called `profiles` that contains a NetCDF
#' file for each profile the float made. These can be read with
#' \code{read.argo}. It is also possible, and probably more common,
#' to read a NetCDF file containing all the profiles together and for
#' that purpose the file
#' \url{http://www.usgodae.org/ftp/outgoing/argo/dac/bodc/6900388/6900388_prof.nc}
#' should be downloaded and provided as the \code{file} argument to
#' \code{read.argo}.  This can be automated as in Example 2,
#' although readers are cautioned that URL structures tend to change
#' over time.
#'
#' Similar steps can be followed on other servers.
#'
#' @author Dan Kelley
#' @family things related to \code{argo} data
read.argo <- function(file, debug=getOption("oceDebug"), processingLog, ...)
{
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
    oceDebug(debug, "read.argo(file=\"", filename, "\", ...) {\n", sep="", unindent=1)
    varNames <- names(file$var)
    lc <- "data_type" %in% varNames
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
        where <- which(o == v)
        if (length(where))
            v <- v[-where[1]]
        v
    }

    oceDebug(debug-1, "varNames=", paste(varNames, collapse=","), "\n", sep="")
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
    oceDebug(debug-1, "varNames=", paste(varNames, collapse=","), "\n")
    res@metadata$PIName <- if (maybeLC("PI_NAME", lc) %in% varNames)
        as.vector(trimString(ncdf4::ncvar_get(file, maybeLC("PI_NAME", lc)))) else NULL
    varNames <- varNamesOmit(varNames, "PI_NAME")
    oceDebug(debug-1, "varNames=", paste(varNames, collapse=","), "\n")
    res@metadata$stationParameters <- if (maybeLC("STATION_PARAMETERS", lc) %in% varNames)
        trimString(ncdf4::ncvar_get(file, maybeLC("STATION_PARAMETERS", lc))) else NULL
    varNames <- varNamesOmit(varNames, "STATION_PARAMETERS")
    oceDebug(debug-1, "STATION_PARAMETERS\n")
    oceDebug(debug-1, "varNames=", paste(varNames, collapse=","), "\n")
    res@metadata$cycleNumber <- if (maybeLC("CYCLE_NUMBER", lc) %in% varNames)
        as.vector(ncdf4::ncvar_get(file, maybeLC("CYCLE_NUMBER", lc))) else NULL
    varNames <- varNamesOmit(varNames, "CYCLE_NUMBER")
    oceDebug(debug-1, "CYCLE_NUMBER\n")
    oceDebug(debug-1, "varNames=", paste(varNames, collapse=","), "\n")
    res@metadata$direction <- if (maybeLC("DIRECTION", lc) %in% varNames)
        as.vector(ncdf4::ncvar_get(file, maybeLC("DIRECTION", lc))) else NULL
    varNames <- varNamesOmit(varNames, "DIRECTION")
    oceDebug(debug-1, "DIRECTION\n")
    oceDebug(debug-1, "varNames=", paste(varNames, collapse=","), "\n")
    res@metadata$dataCentre <- if (maybeLC("DATA_CENTRE", lc) %in% varNames)
        as.vector(ncdf4::ncvar_get(file, maybeLC("DATA_CENTRE", lc))) else NULL
    varNames <- varNamesOmit(varNames, "DATA_CENTRE")
    oceDebug(debug-1, "DATA_CENTRE\n")
    oceDebug(debug-1, "varNames=", paste(varNames, collapse=","), "\n")
    res@metadata$DCReference <- if (maybeLC("DC_REFERENCE", lc) %in% varNames)
        as.vector(trimString(ncdf4::ncvar_get(file, maybeLC("DC_REFERENCE", lc)))) else NULL
    varNames <- varNamesOmit(varNames, "DC_REFERENCE")
    oceDebug(debug-1, "DC_REFERENCE\n")
    oceDebug(debug-1, "varNames=", paste(varNames, collapse=","), "\n")
    res@metadata$dataStateIndicator <- if (maybeLC("DATA_STATE_INDICATOR", lc) %in% varNames)
        as.vector(trimString(ncdf4::ncvar_get(file, maybeLC("DATA_STATE_INDICATOR", lc)))) else NULL
    varNames <- varNamesOmit(varNames, "DATA_STATE_INDICATOR")
    oceDebug(debug-1, "DATA_STATE_INDICATOR\n")
    oceDebug(debug-1, "varNames=", paste(varNames, collapse=","), "\n")
    res@metadata$dataMode <- if (maybeLC("DATA_MODE", lc) %in% varNames)
        strsplit(ncdf4::ncvar_get(file, maybeLC("DATA_MODE", lc)), "")[[1]] else NULL
    varNames <- varNamesOmit(varNames, "DATA_MODE")
    oceDebug(debug-1, "DATA_MODE\n")
    oceDebug(debug-1, "varNames=", paste(varNames, collapse=","), "\n")
    res@metadata$instReference <- if (maybeLC("INST_REFERENCE", lc) %in% varNames)
        as.vector(trimString(ncdf4::ncvar_get(file, maybeLC("INST_REFERENCE", lc)))) else NULL
    varNames <- varNamesOmit(varNames, "INST_REFERENCE")
    oceDebug(debug-1, "INST_REFERENCE\n")
    oceDebug(debug-1, "varNames=", paste(varNames, collapse=","), "\n")
    res@metadata$firmwareVersion <- if (maybeLC("FIRMWARE_VERSION", lc) %in% varNames)
        as.vector(trimString(ncdf4::ncvar_get(file, maybeLC("FIRMWARE_VERSION", lc)))) else NULL
    varNames <- varNamesOmit(varNames, "FIRMWARE_VERSION")
    oceDebug(debug-1, "FIRMWARE_REFERENCE\n")
    oceDebug(debug-1, "varNames=", paste(varNames, collapse=","), "\n")
    res@metadata$WMOInstType <- if (maybeLC("WMO_INST_TYPE", lc) %in% varNames)
        as.vector(trimString(ncdf4::ncvar_get(file, maybeLC("WMO_INST_TYPE", lc)))) else NULL
    varNames <- varNamesOmit(varNames, "WMO_INST_TYPE")
    oceDebug(debug-1, "WMO_INST_TYPE\n")
    oceDebug(debug-1, "varNames=", paste(varNames, collapse=","), "\n")
    res@metadata$juld <- if (maybeLC("JULD", lc) %in% varNames)
        as.vector(ncdf4::ncvar_get(file, maybeLC("JULD", lc))) else NULL
    varNames <- varNamesOmit(varNames, "JULD")
    oceDebug(debug-1, "JULD\n")
    oceDebug(debug-1, "varNames=", paste(varNames, collapse=","), "\n")
    ## set up 'time' also
    t0s <- as.vector(ncdf4::ncvar_get(file, maybeLC("REFERENCE_DATE_TIME", lc)))
    varNames <- varNamesOmit(varNames, "REFERENCE_DATE_TIME")
    oceDebug(debug-1, "REFERENCE_DATE_TIME\n")
    oceDebug(debug-1, "varNames=", paste(varNames, collapse=","), "\n")
    t0 <- strptime(t0s, "%Y%m%d%M%H%S", tz="UTC")
    ##julianDayTime <- as.vector(ncdf4::ncvar_get(file, maybeLC("JULD", lc)))
    res@data$time <- t0 + res@metadata$juld * 86400
    rm(list=c("t0s", "t0")) # no longer needed

    res@metadata$juldQc <- if (maybeLC("JULD_QC", lc) %in% varNames)
        as.vector(ncdf4::ncvar_get(file, maybeLC("JULD_QC", lc))) else NULL
    varNames <- varNamesOmit(varNames, "JULD_QC")
    oceDebug(debug-1, "JULD_QC\n")
    oceDebug(debug-1, "varNames=", paste(varNames, collapse=","), "\n")
    res@metadata$juldLocation <- if (maybeLC("JULD_LOCATION", lc) %in% varNames)
        as.vector(ncdf4::ncvar_get(file, maybeLC("JULD_LOCATION", lc))) else NULL
    varNames <- varNamesOmit(varNames, "JULD_LOCATION")
    oceDebug(debug-1, "JULD_LOCATION\n")
    oceDebug(debug-1, "varNames=", paste(varNames, collapse=","), "\n")

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

    res@metadata$positionQc <- if (maybeLC("POSITION_QC", lc) %in% varNames)
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
    if (maybeLC("TEMP", lc) %in% varNames) {
        ## leave some code in case we get a newer scale
        if (1 == length(grep("ITS-90", ncdf4::ncatt_get(file, maybeLC("TEMP", lc), "long_name")$value, ignore.case=TRUE)))
            res@metadata$units$temperature <- list(unit=expression(degree *C), scale="ITS-90")
        else res@metadata$units$temperature <- list(unit=expression(degree *C), scale="ITS-90")
    }
    if (maybeLC("TEMP_ADJUSTED", lc) %in% varNames) {
        ## leave some code in case we get a newer scale
        if (1 == length(grep("ITS-90", ncdf4::ncatt_get(file, maybeLC("TEMP_ADJUSTED", lc), "long_name")$value, ignore.case=TRUE)))
            res@metadata$units$temperatureAdjusted <- list(unit=expression(degree *C), scale="ITS-90")
        else res@metadata$units$temperatureAdjusted <- list(unit=expression(degree *C), scale="ITS-90")
    }
    if (maybeLC("TEMP_ADJUSTED_ERROR", lc) %in% varNames) {
        ## leave some code in case we get a newer scale
        if (1 == length(grep("ITS-90", ncdf4::ncatt_get(file, maybeLC("TEMP_ADJUSTED_ERROR", lc), "long_name")$value, ignore.case=TRUE)))
            res@metadata$units$temperatureAdjustedError <- list(unit=expression(degree *C), scale="ITS-90")
        else res@metadata$units$temperatureAdjustedError <- list(unit=expression(degree *C), scale="ITS-90")
    }
    if (maybeLC("PSAL", lc) %in% varNames) {
        ## leave some code in case we get a newer scale
        if (1 == length(grep("PRACTICAL", ncdf4::ncatt_get(file, maybeLC("PSAL", lc), "long_name")$value, ignore.case=TRUE)))
            res@metadata$units$salinity <- list(unit=expression(), scale="PSS-78")
        else
            res@metadata$units$salinity <- list(unit=expression(), scale="PSS-78")
    }
    if (maybeLC("PSAL_ADJUSTED", lc) %in% varNames) {
        ## leave some code in case we get a newer scale
        if (1 == length(grep("PRACTICAL", ncdf4::ncatt_get(file, maybeLC("PSAL_ADJUSTED", lc), "long_name")$value, ignore.case=TRUE)))
            res@metadata$units$salinityAdjusted <- list(unit=expression(), scale="PSS-78")
        else
            res@metadata$units$salinityAdjusted <- list(unit=expression(), scale="PSS-78")
    }
    if (maybeLC("PSAL_ADJUSTED_ERROR", lc) %in% varNames) {
        ## leave some code in case we get a newer scale
        if (1 == length(grep(maybeLC("PRACTICAL", lc), ncdf4::ncatt_get(file, maybeLC("PSAL_ADJUSTED_ERROR", lc), "long_name")$value, ignore.case=TRUE)))
            res@metadata$units$salinityAdjustedError <- list(unit=expression(), scale="PSS-78")
        else
            res@metadata$units$salinityAdjustedError <- list(unit=expression(), scale="PSS-78")
    }
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
    oceDebug(debug-1, "About to process stationParameters ...\n")
    for (item in stationParameters) {
        if (!nchar(item)) ## some files have unnamed variables, so we skip them
            next
        n <- item
        d <- getData(file, maybeLC(n, lc))
        varNames <- varNamesOmit(varNames, n)
        oceDebug(debug-1, n, "\n")
        oceDebug(debug-1, "A varNames=", paste(varNames, collapse=","), "\n")
        if (!is.null(d)) {
            res@data[[argoNames2oceNames(n)]] <- d
            res@metadata$dataNamesOriginal[[argoNames2oceNames(n)]] <- n
        } else {
            res@data[[argoNames2oceNames(n)]] <- NULL
        }

        n <- paste(item, maybeLC("_QC", lc), sep="")
        d <- getData(file, maybeLC(n, lc))
        varNames <- varNamesOmit(varNames, n)
        oceDebug(debug-1, n, "\n")
        oceDebug(debug-1, "B varNames=", paste(varNames, collapse=","), "\n")
        if (!is.null(d)) res@metadata$flags[[argoNames2oceNames(n)]] <- argoDecodeFlags(d)
        n <- paste(item, maybeLC("_ADJUSTED", lc), sep="")
        if (n %in% varNames) {
            d <- getData(file, maybeLC(n, lc))
            varNames <- varNamesOmit(varNames, n)
            oceDebug(debug-1, n, "\n")
            oceDebug(debug-1, "C varNames=", paste(varNames, collapse=","), "\n")
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
            oceDebug(debug-1, "D varNames=", paste(varNames, collapse=","), "\n")
            if (!is.null(d)) res@metadata$flags[[argoNames2oceNames(n)]] <- argoDecodeFlags(d)
        }
        n <- paste(item, maybeLC("_ADJUSTED_ERROR", lc), sep="")
        if (n %in% varNames) {
            d <- getData(file, maybeLC(n, lc))
            varNames <- varNamesOmit(varNames, n)
            oceDebug(debug-1, n, "\n")
            oceDebug(debug-1, "E varNames=", paste(varNames, collapse=","), "\n")
            if (!is.null(d)) {
                res@data[[argoNames2oceNames(n)]] <- d
                res@metadata$dataNamesOriginal[[argoNames2oceNames(n)]] <- n
            } else {
                res@data[[argoNames2oceNames(n)]] <- NULL
            }
        }
    }
    oceDebug(debug-1, "after processing stationParameters, flag names are: ",
             paste(names(res@metadata$flags), collapse=" "), "\n")
    if (length(res@metadata$flags))
        names(res@metadata$flags) <- gsub("QC$", "", names(res@metadata$flags))
    oceDebug(debug-1, "after trimming QC, flag names are: ",
             paste(names(res@metadata$flags), collapse=" "), "\n")
    res@metadata$filename <- filename

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
        oceDebug(debug, "about to try to insert \"", name, "\" into metadata\n", sep="")
        o <- capture.output(value <- try(ncdf4::ncvar_get(file, name), silent=TRUE))
        if (inherits(value, "try-error")) {
            ## see https://github.com/dankelley/oce/issues/1522 for a discussion of the fact
            ## that the file used for data(argo) has zero-length HISTORY_* items.
            if (length(grep("Index exceeds dimension", o))) {
                ## FIXME: this code is brittle, being dependent on the layout of
                ## FIXME: the output from nc_open(), which might be subject to change.
                ## FIXME: The code worked on 2019-04-13.
                oceDebug(debug, "ncdf4::ncvar_get() error diagnosis ... name=", name, " len=",file$var[[name]]$dim[[file$var[[name]]$ndim]]$len, " (if this is 0, will not save '", name, "' to metadata)\n")
                if (0 != file$var[[name]]$dim[[file$var[[name]]$ndim]]$len) {
                    warning("ncvar_get() failed for \"", name, "\" (Index exceeds dimension), so it isn't stored in metadata\n")
                }
            } else {
                warning("ncvar_get() failed for \"", name, "\", so it isn't stored in metadata\n")
            }
        } else{
            ## Make a vector, if it is a single-column matrix
            if (1 == length(dim(value)))
                value <- as.vector(value)
            ## Trim leading/trailing whitespace, if it is a string
            if (is.character(value))
                value <- trimString(value)
            res@metadata[[name]] <- value
        }
    }

    ## Record a log item
    res@processingLog <- if (is.character(file))
        processingLogAppend(res@processingLog, paste("read.argo(\"", file, "\")", sep=""))
    else processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res <- initializeFlagScheme(res, "argo")
    oceDebug(debug, "} # read.argo()\n", sep="", unindent=1)
    res
}

#' Coerce Data Into an Argo Dataset
#'
#' Coerce a dataset into an argo dataset. This is not the right way to
#' read official argo datasets, which are provided in NetCDF format and may
#' be read with \code{\link{read.argo}}.
#'
#' @param time vector of POSIXct times.
#' @param longitude vector of longitudes.
#' @param latitude vector of latitudes.
#' @param salinity vector of salinities.
#' @param temperature vector of temperatures.
#' @param pressure vector of pressures.
#' @param units optional list containing units. If \code{NULL}, the default,
#' then \code{"degree east"} is used for \code{longitude},
#' \code{"degree north"} for \code{latitude},
#' \code{""} for \code{salinity},
#' \code{"ITS-90"} for \code{temperature}, and
#' \code{"dbar"} for \code{pressure}.
#' @param id identifier.
#' @param filename source filename.
#' @param missingValue Optional missing value, indicating data values that should be
#' taken as \code{NA}.
#'
#' @return
#' An object of \code{\link{argo-class}}.
#'
#' @seealso
#' The documentation for \code{\link{argo-class}} explains the structure of argo
#' objects, and also outlines the other functions dealing with them.
#'
#' @author Dan Kelley
#' @family things related to \code{argo} data
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


#' Plot Argo Data
#'
#' Plot a summary diagram for argo data.
#'
#' @param x object inheriting from \code{\link{argo-class}}.
#'
#' @param which list of desired plot types, one of the following. Note
#' that \code{\link{oce.pmatch}} is used to try to complete partial
#' character matches, and that an error will occur if the match is
#' not complete (e.g. \code{"salinity"} matches to both
#' \code{"salinity ts"} and \code{"salinity profile"}.).
#' \itemize{
#'     \item \code{which=1}, \code{which="trajectory"} or \code{which="map"} gives a
#'     plot of the argo trajectory, with the coastline, if one is provided.
#'
#'     \item \code{which=2} or \code{"salinity ts"} gives a time series of
#'     salinity at the indicated level(s)
#'
#'     \item \code{which=3} or \code{"temperature ts"} gives a time series
#'     of temperature at the indicated level(s)
#'
#'     \item \code{which=4} or \code{"TS"} gives a TS diagram at the
#'     indicated level(s)
#'
#'     \item \code{which=5} or \code{"salinity profile"} gives a salinity
#'     profile of all the data (with S and p trimmed to the 1 and 99
#'     percentiles)
#'
#'     \item \code{which=6} or \code{"temperature profile"} gives a
#'     temperature profile (with T and p trimmed to the 1 and 99
#'     percentiles)
#' }
#
#' @param level depth pseudo-level to plot, for \code{which=2} and higher.  May be an
#' integer, in which case it refers to an index of depth (1 being the top)
#' or it may be the string "all" which means to plot all data.
#'
#' @param coastline character string giving the coastline to be used in an Argo-location
#' map, or \code{"best"} to pick the one with highest resolution, or
#' \code{"none"} to avoid drawing the coastline.
#'
#' @param cex size of plotting symbols to be used if \code{type='p'}.
#'
#' @param pch type of plotting symbols to be used if \code{type='p'}.
#'
#' @param type plot type, either \code{"l"} or \code{"p"}.
#'
#' @param col optional list of colors for plotting.
#'
#' @param fill Either a logical, indicating whether to fill the land with
#' light-gray, or a color name.  Owing to problems with some projections, the
#' default is not to fill.
#'
#' @param mgp 3-element numerical vector to use for \code{par(mgp)}, and also for
#' \code{par(mar)}, computed from this.  The default is tighter than the R
#' default, in order to use more space for the data and less for the axes.
#'
#' @param projection indication of the projection to be used
#' in trajectory maps. If this is \code{NULL}, no projection is used, although
#' the plot aspect ratio will be set to yield zero shape distortion at the
#' mean float latitude.  If \code{projection="automatic"}, then one
#' of two projections is used: stereopolar (i.e. \code{"+proj=stere +lon_0=X"}
#' where \code{X} is the mean longitude), or Mercator (i.e. \code{"+proj=merc"})
#' otherwise.  Otherwise, \code{projection} must be a character string specifying
#' a projection in the notation used by the \code{link[rgdal]{project}} function
#' in the \CRANpkg{rgdal} package; this will be familiar to many readers as
#' the PROJ.4 notation; see \code{\link{mapPlot}}.
#'
#' @param mar value to be used with \code{\link{par}}("mar").
#'
#' @param tformat optional argument passed to \code{\link{oce.plot.ts}}, for plot
#' types that call that function.  (See \code{\link{strptime}} for the format
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
#' plot(argo, pch=as.integer(tc))
#' year <- substr(levels(tc), 1, 4)
#' data(topoWorld)
#' contour(topoWorld[['longitude']], topoWorld[['latitude']],
#'         topoWorld[['z']], add=TRUE)
#' legend("bottomleft", pch=seq_along(year), legend=year, bg="white", cex=3/4)
#'
#' @references \url{http://www.argo.ucsd.edu/}
#'
#' @author Dan Kelley
#'
#' @family things related to \code{argo} data
#' @family functions that plot \code{oce} data
#' @aliases plot.argo
setMethod(f="plot",
          signature=signature("argo"),
          definition=function (x, which = 1, level,
                               coastline=c("best", "coastlineWorld", "coastlineWorldMedium",
                                           "coastlineWorldFine", "none"),
                               cex=1, pch=1, type='p', col, fill=FALSE,
                               projection=NULL,
                               mgp=getOption("oceMgp"), mar=c(mgp[1]+1.5, mgp[1]+1.5, 1.5, 1.5),
                               tformat,
                               debug=getOption("oceDebug"),
                               ...)
          {
              if (!inherits(x, "argo"))
                  stop("method is only for objects of class '", "argo", "'")
              if ("adorn" %in% names(list(...)))
                  warning("In plot,argo-method() : the 'adorn' argument was removed in November 2017", call.=FALSE)
              oceDebug(debug, "plot.argo(x, which=c(", paste(which, collapse=","), "),",
                      " mgp=c(", paste(mgp, collapse=","), "),",
                      " mar=c(", paste(mar, collapse=","), "),",
                      " ...) {\n", sep="", unindent=1)
              coastline <- match.arg(coastline)
              nw  <- length(which)
              if (nw > 1) {
                  par(mfcol=c(1, nw), mgp=mgp, mar=mar)
              } else {
                  par(mgp=mgp, mar=mar)
              }
              if (missing(level) || level == "all")
                  level <- seq(1L, dim(x@data$temperature)[1])
              longitude <- x[["longitude"]]
              latitude <- x[["latitude"]]
              dim <- dim(x@data$salinity)
              if (length(longitude) < prod(dim)) {
                  ## Copy across depths. This is inside a conditional because
                  ## possibly argo[["longitude"]] should mimic section[["longitude"]],
                  ## in doing the lengthing by itself unless the second argument is
                  ## "byStation" (issue 1273 ... under consideration 2017jul12)
                  longitude <- rep(x[["longitude"]], each=dim[1])
                  latitude <- rep(x[["latitude"]], each=dim[1])
              }
              ctd <- as.ctd(x@data$salinity, x@data$temperature, x@data$pressure,
                            longitude=longitude, latitude=latitude,
                            units=list(temperature=list(unit=expression(degree*C), scale="ITS-90"),
                                       conductivity=list(list=expression(), scale=""))) # guess on units
              which <- oce.pmatch(which,
                                  list("trajectory"=1,
                                       "map"=1,
                                       "salinity ts"=2,
                                       "temperature ts"=3,
                                       "TS"=4,
                                       "salinity profile"=5,
                                       "temperature profile"=6))
              if (is.na(which))
                  stop("In plot,argo-method() :\n  unrecognized value of which", call.=FALSE)
              for (w in 1:nw) {
                  if (which[w] == 1) {
                      oceDebug(debug, "which[", w, "] ==1, so plotting a map\n")
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
                                  col=if (missing(col)) "black" else col,
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
                          asp <- 1 / cos(mean(range(x@data$latitude, na.rm=TRUE)) * atan2(1, 1) / 45)
                          plot(x@data$longitude, x@data$latitude, asp=asp,
                               type=type, cex=cex, pch=pch,
                               col=if (missing(col)) "black" else col,
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
                                  points(x@data$longitude, x@data$latitude, cex=cex, pch=pch, col=if (!missing(col))col)
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
                                      ylab=resizableLabel("S", "y"), type=type,
                                      col=if (missing(col)) "black" else col,
                                      tformat=tformat, ...)
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
                                      ylab=resizableLabel("T", "y"), type=type,
                                      col=if (missing(col)) "black" else col,
                                      tformat=tformat, ...)
                      } else {
                          warning("no non-missing temperature data")
                      }
                  } else if (which[w] == 4) {
                      ## TS
                      if (0 != sum(!is.na(x@data$temperature)) && 0 != sum(!is.na(x@data$salinity))) {
                          plotTS(ctd, col=if (missing(col)) "black" else col, type=type, ...)
                     } else {
                          warning("no non-missing salinity data")
                      }
                  } else if (which[w] == 5) {
                      ## S profile
                      ## FIXME: how to handle the noise; if as below, document it
                      plotProfile(ctd, xtype="salinity",
                           Slim=quantile(x@data$salinity, c(0.01, 0.99), na.rm=TRUE),
                           ylim=quantile(x@data$pressure, c(0.99, 0.01), na.rm=TRUE),
                           col=if (missing(col)) "black" else col, type=type)
                  } else if (which[w] == 6) {
                      ## T profile
                      ## FIXME: how to handle the noise; if as below, document it
                      plotProfile(ctd, xtype="temperature",
                           Tlim=quantile(x@data$temperature, c(0.01, 0.99), na.rm=TRUE),
                           ylim=quantile(x@data$pressure, c(0.99, 0.01), na.rm=TRUE),
                           col=if (missing(col)) "black" else col, type=type)
                  } else {
                      stop("Unknown value of which=", which[w], "\n", call.=FALSE)
                  }
              }
              oceDebug(debug, "} # plot.argo()\n", unindent=1)
              invisible()
          })

## DEVELOPERS: please pattern functions and documentation on the 'ctd' code, for uniformity.
## DEVELOPERS: You will need to change the docs, and the 3 spots in the code
## DEVELOPERS: marked '# DEVELOPER 1:', etc.
#' @title Handle Flags in ARGO Objects
#' @param object An object of \code{\link{argo-class}}.
#' @template handleFlagsTemplate
#'
#' @references
#' 1. \url{http://www.argo.ucsd.edu/Argo_date_guide.html#dmodedata}
#'
#' @examples
#' library(oce)
#' data(argo)
#' # 1. Default: set to NA any data that is not flagged with
#' # code value 1 (meaning \code{"passed_all_tests"})
#' argoNew <- handleFlags(argo, flags=c(0, 2:9))
#' # Demonstrate replacement, looking at the second profile
#' f <- argo[["salinityFlag"]][,2] # first column with a flag=4 entry
#' df <- data.frame(flag=f, orig=argo[["salinity"]][,2], new=argoNew[["salinity"]][,2])
#' df[11:15,] # notice line 13
#'
#' # 2. A less restrictive case: focussing just on salinity,
#' # retain only data with flags 1 (meaning \code{"passed_all_tests"})
#' # and 2 (\code{"probably_good"}).
#' argoNew <- handleFlags(argo, flags=list(salinity=c(0, 3:9)))
#'
#' @author Dan Kelley
#'
#' @family things related to \code{argo} data
setMethod("handleFlags",
          c(object="argo", flags="ANY", actions="ANY", debug="ANY"),
          function(object, flags=NULL, actions=NULL, debug=getOption("oceDebug")) {
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
              handleFlagsInternal(object, flags, actions, debug)
          })
